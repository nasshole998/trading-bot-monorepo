# Strategy DSL Examples

This directory contains example strategy files written in the Trading Strategy DSL.
These strategies are intended to be loaded and executed by the OCaml Strategy DSL Engine.

## DSL Features Demonstrated:

* **State Variables:** Declared with `VAR` and updated with `SET`.
* **Accessing Data:** Using `Indicator("Name")` and `Prediction("Name")` for current values.
* **Accessing Previous Data:** Using `Indicator("Name").prev` and `Prediction("Name").prev` for the value one period ago.
* **Built-in Functions:** Using `ABS`, `MIN`, `MAX` (syntax `FUNC_NAME(args)`).
* **Conditional Logic:** `IF expr THEN { ... } ELSE { ... }`.
* **Actions:** `BUY expr`, `SELL expr`, `HOLD`, `Log(expr)`.
* **Expressions:** Arithmetic, comparison, logical operators.

**Note:** The `ON DataUpdate("symbol") { ... }` block is parsed but currently ignored by the interpreter. The strategy logic is executed for any symbol that receives new data, provided the strategy uses indicators/predictions relevant to that symbol (implicitly handled by checking data availability during execution).

## `simple_ma_crossover.strat`

Implements a simplified Moving Average Crossover strategy.

**Logic:**

* Tracks a `position` state variable (0 for flat, 1 for long).
* On receiving data for `btc_usdt`, it retrieves the current and previous values of "SMA_20_BTCUSDT" and "SMA_50_BTCUSDT".
* It checks for a crossover condition by comparing current and previous SMA values.
* It uses the `position` state variable to trigger BUY only when flat, and SELL only when long.
* Uses `ABS()` on the quantity expression as an example of a built-in function.
* Includes checks for `NaN` to handle potential missing data or evaluation errors gracefully.

**Requirements:**

* The Indicator Engine must be providing indicators named "SMA_20_BTCUSDT" and "SMA_50_BTCUSDT".
* The Data Manager in the Strategy Engine must be configured with `max_history_size` of at least 2 to provide previous values (`.prev`).

**Simplifications:**

* Uses fixed order quantity (though it's an expression, it evaluates to a constant here).
* Only implements a simple long-only position.
* Assumes the strategy is always relevant to `btc_usdt`.

## `prediction_threshold.strat`

Implements a simple strategy based on an ML price prediction.

**Logic:**

* Tracks a `position` state variable (0 for flat, 1 for long).
* Defines threshold multipliers (`buy_threshold_multiplier`, `sell_threshold_multiplier`) as state variables.
* On receiving data for `btc_usdt`, it retrieves the current ML prediction value ("price_forecast_next_tick_BTCUSDT") and the current price ("current_price").
* It checks for buy/sell conditions based on whether the forecast exceeds the current price multiplied by the respective thresholds.
* It uses the `position` state variable to trigger BUY only when flat, and SELL only when long.
* Uses `ABS()` on the quantity expression as an example of a built-in function.
* Includes checks for `NaN` to handle potential missing data or evaluation errors gracefully.

**Requirements:**

* The ML Engine must be providing predictions named "price_forecast_next_tick_BTCUSDT".
* Market data must be processed such that "current_price" is available (e.g., exposed by the Indicator Engine or directly processed by Strategy Engine Data Manager).

**Simplifications:**

* Uses fixed order quantity.
* Only implements a simple long-only position.
* Assumes the strategy is always relevant to `btc_usdt`.
* Thresholds are fixed.

These examples demonstrate the use of the enhanced DSL features for building more complex and stateful trading logic compared to the initial version.