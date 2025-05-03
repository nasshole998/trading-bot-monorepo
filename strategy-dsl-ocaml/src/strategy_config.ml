(** strategy_config.ml - Configuration Loading for Strategy Engine *)

open Strategy_config_intf (* Include interface *)
open Yaml_ez (* For YAML parsing *)
open Logs (* For logging *)

exception ConfigError of string

(* Implement of_yaml for option type manually as deriving doesn't always handle it *)
let logging_config_of_yaml node =
  if is_null node then Result.Ok None
  else (
    let open Result.Infix in
    logging_config_of_yaml node >>= fun cfg ->
    Result.Ok (Some cfg)
  )


(* Implement of_yaml for the main config type, manually combining parsers *)
let t_of_yaml node =
  let open Result.Infix in
  Yaml_ez.from_yaml_exn node (field "grpc" grpc_config_of_yaml) >>= fun grpc ->
  Yaml_ez.from_yaml_exn node (field "upstream_grpc" upstream_grpc_config_of_yaml) >>= fun upstream_grpc ->
  Yaml_ez.from_yaml_exn node (field "strategy_engine" strategy_engine_config_of_yaml) >>= fun strategy_engine ->
  Yaml_ez.from_yaml_exn node (field "data" data_config_of_yaml) >>= fun data ->
  Yaml_ez.from_yaml_exn node (field "health_check" health_check_config_of_yaml) >>= fun health_check ->
  Yaml_ez.from_yaml_exn node (field_opt "logging" logging_config_of_yaml) >>= fun logging ->
  Result.Ok { grpc; upstream_grpc; strategy_engine; data; health_check; logging }


(** Load configuration from a YAML file. Returns config or raises ConfigError. *)
let load filepath =
  info (fun m -> m "Loading configuration from %s" filepath);
  let yaml =
    try Yaml_ez.load_file filepath
    with e -> raise (ConfigError (Printf.sprintf "Error reading YAML file %s: %s" filepath (Printexc.to_string e)))
  in
  match yaml with
  | `Ok node ->
      (match t_of_yaml node with
       | Result.Ok config ->
           info (fun m -> m "Configuration loaded successfully");
           config
       | Result.Error (`Msg msg) ->
           error (fun m -> m "Configuration parsing error: %s" msg);
           raise (ConfigError (Printf.sprintf "Configuration parsing error: %s" msg)))
  | `Error (`Msg msg) ->
      error (fun m -> m "Configuration loading error: %s" msg);
      raise (ConfigError (Printf.sprintf "Configuration loading error: %s" msg))

(* Helper to convert string log level to Logs.level. *)
let set_log_level_from_config = function
  | "Debug" -> Logs.Debug
  | "Info" -> Logs.Info
  | "Warning" -> Logs.Warning
  | "Error" -> Logs.Error
  | "App" -> Logs.App (* App is typically used for important application messages *)
  | s ->
      warning (fun m -> m "Unknown log level '%s' in config, defaulting to Info" s);
      Logs.Info (* Default to Info if string is unrecognized *)

(* Initialize logging based on configuration *)
let initialize_logging config =
  let level = match config.logging with
    | Some log_cfg -> set_log_level_from_config log_cfg.level
    | None -> Logs.Info (* Default level if no logging section *)
  in
  Logs.set_level (Some level);
  Logs.set_reporter (Logs_fmt.reporter ()); (* Use a simple formatter *)
  info (fun m -> m "Logging initialized with level %s" (Logs.level_to_string level |> Option.value ~default:"Unknown"));
  ()