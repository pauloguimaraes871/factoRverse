# signal_universe_m_df-class

`signal_universe_m_df` stores performance summaries for a set of signals
(each row = one signal/strategy). It inverts the usual `meta_dataframe`
layout: instead of each row being a stock-date observation and columns
being signals/features, each row is a signal/strategy and columns are
performance metrics or attributes (e.g. IR, alpha, p_value, theme,
is_eligible). This object is the canonical input/output for functions
that summarize, rank and select signals after backtests (for example
[`summarize_performance()`](https://pauloguimaraes871.github.io/factoRverse/reference/summarize_performance.md),
selection heuristics, or meta-model training).

## Details

An S4 subclass of `meta_dataframe` that represents a universe of
backtested signals (strategies).

- The parent-class structural rules still apply: `data` must be a
  `data.frame` whose first three columns are `id`, `tickers`, `dates`
  (here `tickers` typically stores the signal identifier or strategy
  name, and `dates` represents the evaluation date for the performance
  metrics). The `id` column is expected to be
  `paste0(tickers, "-", dates)` so objects remain compatible with the
  rest of the package.

- Typical performance columns: `IR`, `alpha`, `alpha_se`, `p_value`,
  `turnover`, `theme`, `benchmark_return`, etc.

- Eligibility-related columns:

  - `pre_eligible_signals` — logical (or integer 0/1) indicating whether
    the signal passed initial, signal-level filters (data sufficiency,
    minimum history, etc.).

  - `is_eligible` — logical indicating final eligibility for selection
    in signal-selection pipelines (after rules, thresholds, and
    cross-signal constraints are applied).

  - `eligibility_reason` (optional) — character describing why a signal
    was excluded (if present).

- Use
  [`summarize_performance()`](https://pauloguimaraes871.github.io/factoRverse/reference/summarize_performance.md)
  (or the package helper that builds signal universes) to construct
  valid objects; low-level creation must still respect `meta_dataframe`
  validation (id/tickers/dates ordering, types, uniqueness).

## Slots

- `data`:

  A `data.frame` where each row is a backtested signal/strategy and
  columns are performance metrics, attributes and eligibility flags.

- `ss_backtest_workflow`:

  `ANY`: the signal-selection backtest workflow metadata attached to
  this universe (may be `NULL`).

- `workflow`:

  Inherited: history of steps that produced the signal-universe (e.g.
  backtest parameters, resampling choices).

- `signals`:

  Inherited: typically identifies which columns to treat as signal
  identifiers or the metrics of interest.

- `unique_dates`:

  Inherited: number of distinct dates.

- `unique_tickers`:

  Inherited: number of distinct signal identifiers.

- `n_obs`:

  Inherited: total number of rows (signals).

- `current_date`:

  Inherited: reference/current date for the object.

- `meta_dataframe_name`:

  Inherited: short identifier for the object.

## See also

[`meta_dataframe-class`](https://pauloguimaraes871.github.io/factoRverse/reference/meta_dataframe-class.md),
[`summarize_performance`](https://pauloguimaraes871.github.io/factoRverse/reference/summarize_performance.md),
[`run_sb_backtest`](https://pauloguimaraes871.github.io/factoRverse/reference/run_sb_backtest.md)

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(
  id = c("earnings_yield-2026-07-01", "free_cash_flow_yield-2026-07-01"),
  tickers = c("earnings_yield", "free_cash_flow_yield"), # here: signal names
  dates = as.Date(c("2026-07-01", "2026-07-01")),
  IR = c(1.2, 0.8),
  alpha = c(0.015, 0.008),
  p_value = c(0.02, 0.10),
  theme = c("Value", "Value"),
  pre_eligible_signals = c(TRUE, TRUE),
  is_eligible = c(TRUE, FALSE),
  stringsAsFactors = FALSE
)

su <- new(
  "signal_universe_m_df",
  data = df,
  workflow = list(step = "summarize_performance()"),
  signals = c("earnings_yield","free_cash_flow_yield"),
  unique_dates = length(unique(df$dates)),
  unique_tickers = length(unique(df$tickers)),
  n_obs = nrow(df),
  current_date = Sys.Date(),
  meta_dataframe_name = "signal_universe_example"
)
} # }
```
