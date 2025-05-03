# Strategy DSL Examples

This directory contains example strategy files written in the Trading Strategy DSL.
These strategies are intended to be loaded and executed by the OCaml Strategy DSL Engine.

## `simple_ma_crossover.strat`

Implements a simplified Moving Average Crossover strategy.

**Logic:**

* Tracks a `position` state variable (0 for flat, 1 for long).
* On receiving data for `btc_usdt`, it retrieves the current and previous values of "SMA_20_BTCUSDT" and "SMA_50_BTCUSDT".
* It checks for a crossover condition:
    * **BUY:** If the current Fast SMA > current Slow SMA AND the previous Fast SMA <= previous Slow SMA, *and* the strategy is currently flat (`position == 0`), it triggers a BUY order and sets `position` to 1.
    * **SELL:** If the current Fast SMA < current Slow SMA AND the previous Fast SMA >= previous Slow SMA, *and* the strategy is currently long (`position == 1`), it triggers a SELL order and sets `position` to 0.
* Otherwise, it executes a HOLD action.

**Requirements:**

* The Indicator Engine must be providing indicators named "SMA_20_BTCUSDT" and "SMA_50_BTCUSDT".
* The Data Manager in the Strategy Engine must be configured with `max_history_size` of at least 2 to provide previous values.

**Simplifications:**

* Uses fixed order quantity.
* Only implements a simple long-only position (position can be 0 or 1). A full strategy would handle shorting (`position == -1`) and closing short positions.
* Assumes the strategy is always relevant to `btc_usdt`.

## `prediction_threshold.strat`

Implements a simple strategy based on an ML price prediction.

**Logic:**

* Tracks a `position` state variable (0 for flat, 1 for long).
* Defines threshold multipliers (`buy_threshold_multiplier`, `sell_threshold_multiplier`) as state variables (constants in this case, but could be dynamic).
* On receiving data for `btc_usdt`, it retrieves the current ML prediction value ("price_forecast_next_tick_BTCUSDT") and the current price ("current_price").
* It checks for buy/sell conditions based on thresholds:
    * **BUY:** If the `forecast` is greater than `current_price * buy_threshold_multiplier`, *and* the strategy is currently flat (`position == 0`), it triggers a BUY order and sets `position` to 1.
    * **SELL:** If the `forecast` is less than `current_price * sell_threshold_multiplier`, *and* the strategy is currently long (`position == 1`), it triggers a SELL order and sets `position` to 0.
* Otherwise, it executes a HOLD action.

**Requirements:**

* The ML Engine must be providing predictions named "price_forecast_next_tick_BTCUSDT".
* Market data must be processed such that "current_price" is available (e.g., exposed by the Indicator Engine or directly processed by Strategy Engine Data Manager).

**Simplifications:**

* Uses fixed order quantity.
* Only implements a simple long-only position.
* Assumes the strategy is always relevant to `btc_usdt`.
* Thresholds are fixed (could be dynamic based on volatility etc. in a more complex DSL).

These examples demonstrate the use of state variables, accessing current/previous data, and basic conditional logic and actions within the enhanced DSL.