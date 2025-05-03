(** strategy_health_check.ml - HTTP Health Check Server *)

open Strategy_health_check_intf (* Include interface *)
open Lwt.Infix (* For Lwt binds *)
open Strategy_data (* Need DataManager for checks *)
open Strategy_engine (* Need Engine for checks *)
open Strategy_grpc (* Need gRPC clients for checks *)
open Logs (* For logging *)
open Cohttp_lwt_unix (* HTTP server *)
open Cohttp (* HTTP types *)
open Uri (* For parsing URI *)

(* Handler for incoming HTTP requests *)
let request_handler ~conn ~src body data_mgr engine grpc_clients =
  let uri = Request.uri src in
  let path = Uri.path uri in
  debug (fun m -> m "Health check request for path: %s" path);

  if path = "/healthz" then (
    (* Perform health checks *)
    let is_healthy = ref true in
    let messages = ref [] in

    (* Check DataManager status (e.g., is it tracking symbols?) - Lwt-safe *)
    let* tracked_symbols = Strategy_data.get_tracked_symbols data_mgr in
    if List.length tracked_symbols > 0 then
       messages := (Printf.sprintf "DataManager: OK (tracking %d symbols)" (List.length tracked_symbols)) :: !messages
    else (
       is_healthy := false;
       messages := "DataManager: WARNING (no symbols tracked)" :: !messages
    );

    (* Check Strategy Engine status (e.g., are strategies loaded/active?) - Lwt-safe *)
    let* loaded_strategies = Strategy_engine.get_loaded_strategy_names engine in
     if List.length loaded_strategies > 0 then
        messages := (Printf.sprintf "StrategyEngine: OK (loaded %d strategies)" (List.length loaded_strategies)) :: !messages
     else (
        is_healthy := false;
        messages := "StrategyEngine: WARNING (no strategies loaded)" :: !messages
     );

    (* Check gRPC client connectivity (requires tracking connection state in strategy_grpc) *)
    (* This check is complex; for simplicity, assume if channels are created, clients are "trying" *)
    messages := "gRPC Clients: Basic check (connectivity not verified)" :: !messages;


    let status, body_str =
      if !is_healthy then (
        info (fun m -> m "Health check status: OK");
        `OK, "OK\n" ^ (String.concat "\n" (List.rev !messages))
      ) else (
        warning (fun m -> m "Health check status: UNHEALTHY");
        `Service_unavailable, "Service Unavailable\n" ^ (String.concat "\n" (List.rev !messages)) (* 503 Service Unavailable *)
      )
    in

    let headers = Header.init_with "Content-Type" "text/plain" in
    Server.respond_string ~status:status ~headers:headers ~body:body_str ()

  ) else (
    Server.respond_string ~status:`Not_found ~body:"Not Found" () (* 404 for other paths *)
  )


(** Start the HTTP health check server. *)
let start_server ~listen_address data_mgr engine grpc_clients =
  let uri = Uri.of_string ("http://" ^ listen_address) in
  let host = Uri.host uri |> Option.value ~default:"localhost" in
  let port = Uri.port uri |> Option.value ~default:8080 in

  let callback = request_handler body data_mgr engine grpc_clients in
  let server =
    try
      info (fun m -> m "Starting Health Check server on %s:%d" host port);
      Server.create ~mode:(`TCP (`Port port)) (Server.make ~callback ())
    with
    | e ->
        error (fun m -> m "Error starting Health Check server on %s:%d: %s" host port (Printexc.to_string e));
        Lwt.fail e (* Fail the Lwt task *)
  in
  server (* Server.create returns a task that runs until the server is shut down *)