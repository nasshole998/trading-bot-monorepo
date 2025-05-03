(** strategy_config.mli - Configuration Loading for Strategy Engine *)

open Result (* For Result type *)
open Yaml_ez (* For YAML parsing *)
open Logs (* For Logs.level *)

(** Type for the gRPC configuration. *)
type grpc_config = {
  listen_address: string;
} [@@deriving of_yaml]

(** Type for upstream gRPC addresses. *)
type upstream_grpc_config = {
  indicator_engine_address: string;
  ml_engine_address: string;
  data_ingestion_address: string;
} [@@deriving of_yaml]

(** Type for data management configuration. *)
type data_config = {
  max_history_size: int;
} [@@deriving of_yaml]

(** Type for strategy engine configuration. *)
type strategy_engine_config = {
  strategies_path: string; (* Path to directory containing strategy files *)
  active_strategies: string list; (* List of strategy file names to load *)
} [@@deriving of_yaml]

(** Type for health check configuration. *)
type health_check_config = {
  listen_address: string;
} [@@deriving of_yaml]

(** Type for logging configuration (optional). *)
type logging_config = {
  level: string; (* Log level string, e.g., "Info" *)
} [@@deriving of_yaml]

(** The main configuration type. *)
type t = {
  grpc: grpc_config;
  upstream_grpc: upstream_grpc_config;
  strategy_engine: strategy_engine_config;
  data: data_config;
  health_check: health_check_config;
  logging: logging_config option; (* Optional logging config *)
}

(** Exception raised during configuration errors. *)
exception ConfigError of string

(** Load configuration from a YAML file. Returns config or raises ConfigError. *)
val load : string -> t

(** Initialize logging based on configuration. *)
val initialize_logging : t -> unit

(** Helper to convert string log level to Logs.level. *)
val set_log_level_from_config : string -> Logs.level