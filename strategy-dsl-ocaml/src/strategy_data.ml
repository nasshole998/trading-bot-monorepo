(** strategy_data.ml - Data Management for Strategy Engine *)

open Strategy_data_intf (* Include interface *)
open Lwt.Infix (* For Lwt binds *)
open Lwt_mutex (* For mutexes *)
open Lwt_condition (* For condition variables *)
open Timestamps_proto.Google.Protobuf (* For Timestamp *)
open Indicator_data_pb (* For IndicatorValue *)
open Ml_prediction_pb (* For PredictionValue *)
open Market_data_pb (* For Trade, Quote, OrderBookUpdate *)
open Logs (* For logging *)
open Batteries.Deque (* Use Deque for history *)
open Batteries.List (* For Batteries.List.last *)


(** Internal structure to hold data for a single symbol *)
type symbol_data = {
  symbol: string;
  mutable recent_market_data: market_data_item Deque.t; (* Deque for history *)
  mutable recent_indicators: (string, indicator_data_item Deque.t) Hashtbl.t; (* Indicator name -> Deque *)
  mutable recent_predictions: (string, prediction_data_item Deque.t) Hashtbl.t; (* Prediction type -> Deque *)
  mutex: Lwt_mutex.t; (* Mutex for this symbol's data *)
}

(** The main data manager type *)
type t = {
  symbol_data_map: (string, symbol_data) Hashtbl.t; (* Symbol -> symbol_data *)
  map_mutex: Lwt_mutex.t; (* Mutex for the symbol_data_map *)
  max_history_size: int;
  new_data_condition: Lwt_condition.t; (* Condition variable to signal new data availability *)
  mutable new_data_symbols: string list; (* List of symbols with new data since last check *)
  new_data_mutex: Lwt_mutex.t; (* Mutex for new_data_symbols *)
  mutable known_indicators: string list; (* List of all indicator names seen *)
  mutable known_predictions: string list; (* List of all prediction types seen *)
  known_names_mutex: Lwt_mutex.t; (* Mutex for known names lists *)
}

(** Create a new data manager instance. *)
let create ~max_history_size =
  {
    symbol_data_map = Hashtbl.create 100; (* Initial size *)
    map_mutex = Lwt_mutex.create ();
    max_history_size = max_history_size;
    new_data_condition = Lwt_condition.create ();
    new_data_symbols = [];
    new_data_mutex = Lwt_mutex.create ();
    known_indicators = [];
    known_predictions = [];
    known_names_mutex = Lwt_mutex.create ();
  }

(* Helper to get or create symbol_data *)
let get_or_create_symbol_data mgr symbol =
  Lwt_mutex.with_lock mgr.map_mutex (fun () ->
    match Hashtbl.find_opt mgr.symbol_data_map symbol with
    | Some sd -> Lwt.return sd
    | None ->
        info (fun m -> m "Creating symbol data for '%s'" symbol);
        let sd = {
          symbol = symbol;
          recent_market_data = Deque.empty ();
          recent_indicators = Hashtbl.create 10;
          recent_predictions = Hashtbl.create 10;
          mutex = Lwt_mutex.create ();
        } in
        Hashtbl.add mgr.symbol_data_map symbol sd;
        Lwt.return sd
  )

(* Helper to add data to history deque, trimming if necessary *)
let add_to_deque deque item max_size =
  let new_deque = Deque.cons item deque in (* Add to front *)
  if Deque.size new_deque > max_size then
    Deque.init (max_size) (fun i -> Deque.nth new_deque i) (* Create a new deque with the N most recent items *)
  else
    new_deque

(* Helper to notify about new data for a symbol *)
let notify_new_data mgr symbol =
  Lwt_mutex.with_lock mgr.new_data_mutex (fun () ->
    if not (List.mem symbol mgr.new_data_symbols) then (
      mgr.new_data_symbols <- symbol :: mgr.new_data_symbols;
      Lwt_condition.signal mgr.new_data_condition; (* Signal condition variable *)
      Lwt.return_unit
    ) else (
      Lwt.return_unit (* Already flagged for new data *)
    )
  )


(* Add a market data event to the manager. *)
let add_market_data mgr event =
  let open Market_data_pb in
  let symbol =
    if event.MarketDataEvent.has_trade then event.trade.symbol
    else if event.has_quote then event.quote.symbol
    else if event.has_order_book_update then event.order_book_update.symbol
    else "" (* Unhandled type *)
  in
  if symbol = "" then (
    warning (fun m -> m "Received market data event with empty symbol");
    Lwt.return_unit
  ) else (
    let* sd = get_or_create_symbol_data mgr symbol in
    Lwt_mutex.with_lock sd.mutex (fun () ->
      let item =
        if event.has_trade then
          { timestamp = event.trade.timestamp; value = event.trade.price; source = "trade" }
        else if event.has_quote then
          (* Using midpoint price for quote *)
          let bid_price_opt = float_of_string_opt event.quote.bid_price in
          let ask_price_opt = float_of_string_opt event.quote.ask_price in
          match bid_price_opt, ask_price_opt with
          | Some bid, Some ask when bid > 0.0 && ask > 0.0 -> (* Ensure positive prices *)
              let midpoint = (bid +. ask) /. 2.0 in
              { timestamp = event.quote.timestamp; value = string_of_float midpoint; source = "quote" }
          | _ ->
              warning (fun m -> m "Could not parse or invalid quote prices for %s" symbol);
              { timestamp = event.quote.timestamp; value = ""; source = "quote" } (* Store empty value on parse error *)
        else if event.has_order_book_update then
           (* Using midpoint of best bid/ask for OB update *)
           if List.length event.order_book_update.bids > 0 && List.length event.order_book_update.asks > 0 then
            let best_bid_opt = float_of_string_opt (List.hd event.order_book_update.bids).price in
            let best_ask_opt = float_of_string_opt (List.hd event.order_book_update.asks).price in
            match best_bid_opt, best_ask_opt with
            | Some bid, Some ask when bid > 0.0 && ask > 0.0 -> (* Ensure positive prices *)
                let midpoint = (bid +. ask) /. 2.0 in
                { timestamp = event.order_book_update.timestamp; value = string_of_float midpoint; source = "order_book_update" }
            | _ ->
                warning (fun m -> m "Could not parse or invalid best bid/ask prices for %s OB update" symbol);
                 { timestamp = event.order_book_update.timestamp; value = ""; source = "order_book_update" }
           else
              { timestamp = event.order_book_update.timestamp; value = ""; source = "order_book_update" }
        else
          (* Should not happen based on initial check, but defensive *)
          { timestamp = Timestamp.create (); value = ""; source = "unknown" }
      in

      (* Only add if value is not empty (i.e., successfully parsed) *)
       if item.value <> "" then (
            sd.recent_market_data <- add_to_deque sd.recent_market_data item mgr.max_history_size;
            debug (fun m -> m "Added market data for %s. History size: %d" symbol (Deque.size sd.recent_market_data));
            notify_new_data mgr symbol (* Notify engine *)
        ) else (
            warning (fun m -> m "Skipping market data event for %s due to parsing error or invalid price" symbol);
            Lwt.return_unit
        )
    )
  )

(* Add an indicator value to the manager. *)
let add_indicator_value mgr value =
  let open Indicator_data_pb in
  let symbol = value.IndicatorValue.symbol in
  let indicator_name = value.indicator_name in
  let value_str = value.value in
  let timestamp = value.timestamp in

   if symbol = "" || indicator_name = "" || value_str = "" || not (Timestamp.is_valid timestamp) then (
        warning (fun m -> m "Received invalid indicator value (empty field or invalid timestamp)");
        Lwt.return_unit
    ) else (
      (* Track known indicator names *)
      let* () = Lwt_mutex.with_lock mgr.known_names_mutex (fun () ->
         if not (List.mem indicator_name mgr.known_indicators) then (
            mgr.known_indicators <- indicator_name :: mgr.known_indicators;
            info (fun m -> m "Discovered new indicator name: %s" indicator_name);
         );
         Lwt.return_unit
      ) in

      let* sd = get_or_create_symbol_data mgr symbol in
      Lwt_mutex.with_lock sd.mutex (fun () ->
        let item = { timestamp = timestamp; value = value_str; indicator_name = indicator_name } in
        let current_history = Hashtbl.find_opt sd.recent_indicators indicator_name |> Option.value ~default:(Deque.empty ()) in
        let new_history = add_to_deque current_history item mgr.max_history_size in
        Hashtbl.replace sd.recent_indicators indicator_name new_history;
        debug (fun m -> m "Added indicator %s for %s. History size: %d" indicator_name symbol (Deque.size new_history));
        notify_new_data mgr symbol (* Notify engine *)
      )
    )

(* Add a prediction value to the manager. *)
let add_prediction_value mgr value =
   let open Ml_prediction_pb in
   let symbol = value.PredictionValue.symbol in
   let prediction_type = value.prediction_type in
   let value_str = value.value in
   let timestamp = value.timestamp in

    if symbol = "" || prediction_type = "" || value_str = "" || not (Timestamp.is_valid timestamp) then (
         warning (fun m -> m "Received invalid prediction value (empty field or invalid timestamp)");
         Lwt.return_unit
     ) else (
       (* Track known prediction types *)
       let* () = Lwt_mutex.with_lock mgr.known_names_mutex (fun () ->
          if not (List.mem prediction_type mgr.known_predictions) then (
             mgr.known_predictions <- prediction_type :: mgr.known_predictions;
             info (fun m -> m "Discovered new prediction type: %s" prediction_type);
          );
          Lwt.return_unit
       ) in

       let* sd = get_or_create_symbol_data mgr symbol in
       Lwt_mutex.with_lock sd.mutex (fun () ->
         let item = { timestamp = timestamp; value = value_str; prediction_type = prediction_type } in
         let current_history = Hashtbl.find_opt sd.recent_predictions prediction_type |> Option.value ~default:(Deque.empty ()) in
         let new_history = add_to_deque current_history item mgr.max_history_size in
         Hashtbl.replace sd.recent_predictions prediction_type new_history;
         debug (fun m -> m "Added prediction %s for %s. History size: %d" prediction_type symbol (Deque.size new_history));
         notify_new_data mgr symbol (* Notify engine *)
       )
     )


(* Helper to get nth most recent item from a deque *)
let get_nth_recent deque n =
  if n < 0 || n >= Deque.size deque then None
  else Some (Deque.nth deque n)

(* Get the latest market data item for a symbol. Returns (timestamp, value, source) option Lwt.t. *)
let get_latest_market_data mgr symbol =
  let* sd_opt = Lwt_mutex.with_lock mgr.map_mutex (fun () -> Lwt.return (Hashtbl.find_opt mgr.symbol_data_map symbol)) in
  match sd_opt with
  | Some sd ->
      Lwt_mutex.with_lock sd.mutex (fun () ->
        Deque.get_front sd.recent_market_data |> Lwt.return_option (* get_front is O(1) *)
      ) >>= fun item_opt ->
      Lwt.return (Option.map (fun item -> (item.timestamp, item.value, item.source)) item_opt)
  | None -> Lwt.return None

(* Helper to get latest/previous from a specific deque in symbol_data *)
let get_latest_or_previous deque_opt n_ago =
   match deque_opt with
   | Some deque ->
       (match get_nth_recent deque n_ago with
        | Some item -> Lwt.return (Some (item.timestamp, item.value))
        | None -> Lwt.return None)
   | None -> Lwt.return None


(* Get the latest indicator value for a specific indicator on a symbol. Returns (timestamp, value, name) option Lwt.t. *)
let get_latest_indicator mgr symbol name =
   let* sd_opt = Lwt_mutex.with_lock mgr.map_mutex (fun () -> Lwt.return (Hashtbl.find_opt mgr.symbol_data_map symbol)) in
   match sd_opt with
   | Some sd ->
       Lwt_mutex.with_lock sd.mutex (fun () ->
          let deque_opt = Hashtbl.find_opt sd.recent_indicators name in
          get_latest_or_previous deque_opt 0 >>= fun item_opt ->
          Lwt.return (Option.map (fun (ts, value) -> (ts, value, name)) item_opt)
       )
   | None -> Lwt.return None

(* Get the previous indicator value for a specific indicator on a symbol (1 period ago). Returns (timestamp, value, name) option Lwt.t. *)
let get_previous_indicator mgr symbol name =
    let* sd_opt = Lwt_mutex.with_lock mgr.map_mutex (fun () -> Lwt.return (Hashtbl.find_opt mgr.symbol_data_map symbol)) in
   match sd_opt with
   | Some sd ->
       Lwt_mutex.with_lock sd.mutex (fun () ->
          let deque_opt = Hashtbl.find_opt sd.recent_indicators name in
          get_latest_or_previous deque_opt 1 >>= fun item_opt -> (* Get the 1st item (0 is latest, 1 is previous) *)
          Lwt.return (Option.map (fun (ts, value) -> (ts, value, name)) item_opt)
       )
   | None -> Lwt.return None


(* Get the latest prediction value for a specific type on a symbol. Returns (timestamp, value, type) option Lwt.t. *)
let get_latest_prediction mgr symbol ptype =
   let* sd_opt = Lwt_mutex.with_lock mgr.map_mutex (fun () -> Lwt.return (Hashtbl.find_opt mgr.symbol_data_map symbol)) in
   match sd_opt with
   | Some sd ->
       Lwt_mutex.with_lock sd.mutex (fun () ->
          let deque_opt = Hashtbl.find_opt sd.recent_predictions ptype in
          get_latest_or_previous deque_opt 0 >>= fun item_opt ->
          Lwt.return (Option.map (fun (ts, value) -> (ts, value, ptype)) item_opt)
       )
   | None -> Lwt.return None

(* Get the previous prediction value for a specific type on a symbol (1 period ago). Returns (timestamp, value, type) option Lwt.t. *)
let get_previous_prediction mgr symbol ptype =
    let* sd_opt = Lwt_mutex.with_lock mgr.map_mutex (fun () -> Lwt.return (Hashtbl.find_opt mgr.symbol_data_map symbol)) in
   match sd_opt with
   | Some sd ->
       Lwt_mutex.with_lock sd.mutex (fun () ->
          let deque_opt = Hashtbl.find_opt sd.recent_predictions ptype in
          get_latest_or_previous deque_opt 1 >>= fun item_opt -> (* Get the 1st item (0 is latest, 1 is previous) *)
          Lwt.return (Option.map (fun (ts, value) -> (ts, value, ptype)) item_opt)
       )
   | None -> Lwt.return None


(* Get a list of symbols currently tracked by the data manager. *)
let get_tracked_symbols mgr =
  Lwt_mutex.with_lock mgr.map_mutex (fun () ->
    Hashtbl.keys mgr.symbol_data_map |> List.of_seq |> Lwt.return
  )

(* Get a list of known indicator names seen by the data manager. *)
let get_known_indicator_names mgr =
  Lwt_mutex.with_lock mgr.known_names_mutex (fun () ->
    Lwt.return mgr.known_indicators
  )

(* Get a list of known prediction types seen by the data manager. *)
let get_known_prediction_names mgr =
   Lwt_mutex.with_lock mgr.known_names_mutex (fun () ->
    Lwt.return mgr.known_predictions
  )


(* Wait for new data notification and return symbols that have new data. *)
let wait_for_new_data mgr =
  Lwt_mutex.with_lock mgr.new_data_mutex (fun () ->
    (* Wait until new_data_symbols is not empty *)
    let rec wait_loop () =
      if mgr.new_data_symbols <> [] then (
        let symbols = mgr.new_data_symbols in
        mgr.new_data_symbols <- []; (* Clear the list *immediately* upon retrieval *)
        Lwt.return symbols
      ) else (
        Lwt_condition.wait mgr.new_data_condition >>= wait_loop (* Wait and loop *)
      )
    in
    wait_loop ()
  )