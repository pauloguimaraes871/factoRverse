# oos_sb_outputs_m_df-class

`oos_sb_outputs_m_df` extends
[`meta_dataframe-class`](https://pauloguimaraes871.github.io/factoRverse/reference/meta_dataframe-class.md)
to represent out-of-sample predictions produced by the signal-blending
backtest pipeline
([`run_sb_backtest()`](https://pauloguimaraes871.github.io/factoRverse/reference/run_sb_backtest.md)).
It stores target values, model predictions and the associated errors
together with the usual workflow metadata.

## Details

Out-of-sample Signal-Blending backtest outputs container (S4).

This class is the canonical return type for out-of-sample evaluation
routines in the signal-blending workflow. Keeping `target`, `pred`, and
`error` aligned and validated enables downstream aggregation,
performance reporting and diagnostics to rely on consistent semantics.

## Slots

- `sb_backtest_workflow`:

  Any. A record (usually a list) describing the out-of-sample backtest
  workflow and parameters used to produce the outputs.

- `data`:

  A data.frame containing the observations (first three columns: id,
  tickers, dates).

- `workflow`:

  ANY. Record of preprocessing/backtest workflow that produced this
  object.

- `signals`:

  character. Names of columns in data that represent signals/metrics.

- `unique_dates`:

  numeric. Number of distinct dates in data.

- `unique_tickers`:

  numeric. Number of distinct tickers in data.

- `n_obs`:

  numeric. Total number of observations (rows) in data.

- `current_date`:

  Date. Reference/current date for the object.

- `meta_dataframe_name`:

  character. Short identifier for this meta_dataframe instance.

## Validity

Objects must satisfy parent-class checks plus:

- `data` must include the columns `id`, `tickers`, `dates`, `target`,
  `pred`, and `error`.

- For rows where `target` is not `NA`, `error` must equal
  `target - pred`.

- For rows where both `target` and `pred` are `NA`, `error` must be
  `NA`.

## See also

[`meta_dataframe-class`](https://pauloguimaraes871.github.io/factoRverse/reference/meta_dataframe-class.md),
[`run_sb_backtest`](https://pauloguimaraes871.github.io/factoRverse/reference/run_sb_backtest.md),
[`signal_universe_m_df-class`](https://pauloguimaraes871.github.io/factoRverse/reference/signal_universe_m_df-class.md)

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(
  id = c("A-2026-07-01","B-2026-07-01"),
  tickers = c("A","B"),
  dates = as.Date(c("2026-07-01","2026-07-01")),
  target = c(0.02, NA),
  pred = c(0.015, NA),
  error = c(0.005, NA),
  stringsAsFactors = FALSE
)
oos <- new(
  "oos_sb_outputs_m_df",
  data = df,
  workflow = list(step = "oos_evaluate"),
  signals = character(),
  sb_backtest_workflow = list(params = "example"),
  unique_dates = length(unique(df$dates)),
  unique_tickers = length(unique(df$tickers)),
  n_obs = nrow(df),
  current_date = Sys.Date(),
  meta_dataframe_name = "oos_example"
)
} # }
```
