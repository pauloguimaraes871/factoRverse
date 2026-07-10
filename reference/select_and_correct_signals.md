# Select and Correct Signal Positions

This function selects signals based on a given policy, adjusts their
positions according to the policy, and validates the data against
backtest returns.

## Usage

``` r
select_and_correct_signals(
  signals_m_df,
  chosen_signals_and_positions,
  signal_themes_m_df = NULL,
  backtest_returns_m_xts = NULL
)
```

## Arguments

- signals_m_df:

  A (meta) data frame with columns including "id", "tickers", "dates",
  and the selected signals.

- chosen_signals_and_positions:

  A named vector indicating signals and their corresponding positions
  (long or short). For example, chosen_signals_and_positions =
  c(book_yield = "long", vol_36m = "short").

- signal_themes_m_df:

  A (meta) data frame with "id", "tickers" ("signals"), and "dates"
  columns, including all signals in `signals_m_df`, and a "theme" column
  providing group membership for each signal.

- backtest_returns_m_xts:

  A xts containing historical backtested returns named according to
  signals in `signals_m_df`,

## Value

A named list with three components (the last two are `NULL` when their
inputs are not supplied):

- `selected_signals_corrected_positions_m_df`: The updated data frame
  from `signals_m_df` with corrected signal positions and adjusted
  column names.

- `selected_signal_themes_m_df`: The `signal_themes_m_df` subset to the
  corrected signals (`NULL` if not provided).

- `selected_backtest_returns_corrected_positions_m_xts`: The
  `backtest_returns_m_xts` subset to the columns matching the corrected
  signal positions (`NULL` if not provided).

## Details

The function performs the following operations:

- Extracts the chosen signals from `signals_m_df` and subsets the data
  frame to include only these signals.

- Checks for consistency between the length of `chosen_signals` and
  `signal_positions` and ensures that all chosen signals have
  corresponding positions.

- Adjusts the signal positions based on whether they are "short" by
  multiplying their values by -1.

- Updates column names in the data frame to reflect the corrected
  positions of the signals.

- Validates that all adjusted signals have corresponding columns in
  `backtest_returns_m_xts`.
