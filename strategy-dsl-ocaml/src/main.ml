(** main.ml - Main entry point for Strategy DSL Engine *)

open Lwt.Infix (* For Lwt binds *)
open Strategy_config (* Need config *)
open Strategy_data (* Need DataManager *)
open Strategy_grpc (* Need gRPC Clients *)
open Strategy_engine (* Need Strategy Engine *)
open Strategy_health_check (* Need Health Check Server *)
open Logs (* For logging *)
open Lwt_unix (* For signal handling *)
open Grpc_lwt.Server (* For gRPC server *)
open Prometheus_app.Instr.Cohttp_lwt_unix (* For Prometheus HTTP server instrumentation *)
open Prometheus_app (* For Prometheus metrics endpoint *)

(* Global state for graceful shutdown *)
let shutdown_promise, resolve_shutdown = Lwt.wait ()

(* Signal handler *)
let handle_signal signal =
  info (fun m -> m "Received signal %d. Initiating shutdown." signal);
  Lwt.wakeup_later resolve_shutdown () (* Resolve the shutdown promise *)

let main () =
  (* Load configuration *)
  let config =
    try Strategy_config.load "config/settings.yml"
    with Strategy_config.ConfigError msg ->
      failwith (Printf.sprintf "Failed to load configuration: %s" msg)
  in
  (* Initialize logging based on config *)
  Strategy_config.initialize_logging config;

  info (fun m -> m "Strategy DSL Engine starting...");

  (* Create DataManager *)
  let data_manager = Strategy_data.create ~max_history_size:config.data.max_history_size in
  info (fun m -> m "DataManager created with max history size %d." config.data.max_history_size);

  (* Create gRPC Clients (channels) with retry *)
  let* grpc_clients = Strategy_grpc.create
    ~indicator_addr:config.upstream_grpc.indicator_engine_address
    ~prediction_addr:config.upstream_grpc.ml_engine_address
    ~ingestion_addr:config.upstream_grpc.data_ingestion_address
  in
  info (fun m -> m "gRPC Clients created.");

  (* Create Strategy Engine *)
  let strategy_engine = Strategy_engine.create data_manager grpc_clients config in
  info (fun m -> m "Strategy Engine created.");

  (* Load strategies (requires DataManager to get known names) *)
  Strategy_engine.load_strategies strategy_engine >>= fun () ->
  Strategy_engine.get_loaded_strategy_names strategy_engine >>= fun strategy_names ->
  info (fun m -> m "Loaded strategies: [%s]" (String.concat ", " strategy_names));


  (* Start gRPC client subscriptions *)
  let indicator_sub_task = Lwt.async (fun () -> Strategy_grpc.start_indicator_subscription grpc_clients data_manager); in (* Use Lwt.async to run in background *)
  let prediction_sub_task = Lwt.async (fun () -> Strategy_grpc.start_prediction_subscription grpc_clients data_manager); in (* Use Lwt.async *)
  let order_update_sub_task = Lwt.async (fun () -> Strategy_grpc.start_order_update_subscription grpc_clients data_manager); in (* Added order update subscription *)
  info (fun m -> m "gRPC client subscription tasks started.");

  (* Start Health Check / Prometheus server *)
  (* Use Prometheus_app to serve metrics and health check *)
  let metrics_and_health_server_task =
    let server_address = config.health_check.listen_address in
    let uri = Uri.of_string ("http://" ^ server_address) in
    let host = Uri.host uri |> Option.value ~default:"localhost" in
    let port = Uri.port uri |> Option.value ~default:8080 in
    let callback _conn req _body = (* Custom handler can call Strategy_health_check.request_handler *)
        let uri = Cohttp.Request.uri req in
        let path = Uri.path uri in
        if path = "/healthz" then
            Strategy_health_check.request_handler Cohttp_lwt_unix.Server.IO.read Cohttp_lwt_unix.Server.IO.write Cohttp_lwt_unix.Server.IO.f ~conn req ~src:req req data_manager strategy_engine grpc_clients
        else
            Prometheus_app.http_server_handler req
    in
    let server =
      try
        info (fun m -> m "Starting Health Check/Prometheus server on %s:%d" host port);
        Cohttp_lwt_unix.Server.create ~mode:(`TCP (`Port port)) (Cohttp_lwt_unix.Server.make ~callback ())
      with
      | e ->
          error (fun m -> m "Error starting Health Check/Prometheus server on %s:%d: %s" host port (Printexc.to_string e));
          Lwt.fail e (* Fail the Lwt task *)
    in
    Lwt.async (fun () -> server);
    info (fun m -> m "Health Check/Prometheus server task started.");
    Lwt.return_unit (* Return a dummy Lwt task *)
  in


  (* Start Strategy Engine gRPC server *)
  let strategy_engine_grpc_server_task =
    let listen_address = config.grpc.listen_address in
    let uri = Uri.of_string ("grpc://" ^ listen_address) in
    let host = Uri.host uri |> Option.value ~default:"0.0.0.0" in
    let port = Uri.port uri |> Option.value ~default:50053 in
    let service = Strategy_engine_grpc.StrategyEngineService.make_service_binding (fun _ctx -> {
        Strategy_engine_grpc.StrategyEngineService.reload_strategies = Strategy_engine.handle_reload_strategies strategy_engine;
        (* Implement other RPCs like get_loaded_strategies here *)
        (* get_loaded_strategies = handle_get_loaded_strategies engine; *)
      })
    in
    let server =
      try
        info (fun m -> m "Starting Strategy Engine gRPC server on %s:%d" host port);
        Grpc_lwt.Server.create ~port ~service ()
      with
      | e ->
          error (fun m -> m "Error starting Strategy Engine gRPC server on %s:%d: %s" host port (Printexc.to_string e));
          Lwt.fail e (* Fail the Lwt task *)
    in
    Lwt.async (fun () -> server);
     info (fun m -> m "Strategy Engine gRPC server task started.");
     Lwt.return_unit (* Return a dummy Lwt task *)
  in


  (* Start strategy execution loop *)
  let execution_loop_task = Lwt.async (fun () -> Strategy_engine.start_execution_loop strategy_engine); in (* Use Lwt.async *)
  info (fun m -> m "Strategy execution loop task started.");


  (* Register signal handlers for graceful shutdown *)
  let _ = Lwt_unix.on_signal Sys.sigint handle_signal in
  let _ = Lwt_unix.on_signal Sys.sigterm handle_signal in
  info (fun m -> m "Signal handlers registered. Waiting for shutdown signal...");


  (* Wait for the shutdown signal promise to resolve *)
  shutdown_promise >>= fun () ->
  info (fun m -> m "Shutdown signal received. Initiating graceful shutdown...");

  (* Initiate graceful shutdown of components *)
  Strategy_engine.stop_execution_loop strategy_engine >>= fun () ->
  (* Stopping Lwt.async tasks requires cancelling their promises.
     Our main tasks (execution loop, health check, client subscriptions)
     are structured to check the stop_requested flag or handle stream closure/errors gracefully.
     We can just wait for the main async tasks to finish after the stop flag is set. *)
  info (fun m -> m "Waiting for tasks to finish...");
  Lwt.all [
      Lwt.catch (fun () -> Lwt.cancel indicator_sub_task; indicator_sub_task >>= fun _ -> Lwt.return_unit) (fun _ -> Lwt.return_unit); (* Cancel and wait *)
      Lwt.catch (fun () -> Lwt.cancel prediction_sub_task; prediction_sub_task >>= fun _ -> Lwt.return_unit) (fun _ -> Lwt.return_unit); (* Cancel and wait *)
      Lwt.catch (fun () -> Lwt.cancel order_update_sub_task; order_update_sub_task >>= fun _ -> Lwt.return_unit) (fun _ -> Lwt.return_unit); (* Added cancel order update subscription *)
      (* Cancelling the health check/prometheus server task and grpc server task might be tricky.
         Cohttp server promise resolves on shutdown. Grpc_lwt server promise also resolves on shutdown.
         We don't have explicit handles to stop them here, rely on process exit or add stop handles if needed.
         For now, assume they will terminate when the main process exits after stopping the execution loop. *)
      (* Lwt.catch (fun () -> Lwt.cancel health_check_task; health_check_task >>= fun _ -> Lwt.return_unit) (fun _ -> Lwt.return_unit); *)
      (* Lwt.catch (fun () -> Lwt.cancel strategy_engine_grpc_server_task; strategy_engine_grpc_server_task >>= fun _ -> Lwt.return_unit) (fun _ -> Lwt.return_unit); *)
      Lwt.catch (fun () -> execution_loop_task >>= fun _ -> Lwt.return_unit) (fun _ -> Lwt.return_unit); (* Wait for execution loop to finish its current cycle *)
    ] >>= fun _ ->

  info (fun m -> m "Strategy DSL Engine shut down complete.");
  Lwt.return_unit (* Exit the main Lwt task *)

(* Run the main Lwt function *)
let () = Lwt_main.run (main ())