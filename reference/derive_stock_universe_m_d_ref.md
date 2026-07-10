# Build Stock Universe with Expected Return Score

This function creates a stock universe data frame by merging ticker
information with either out-of-sample predictions or signal values. It
then transforms the score using a user-provided function
(`signal_transform`), which is typically used to winsorize, z-score,
and/or otherwise adjust the raw signal.

## Usage

``` r
derive_stock_universe_m_d_ref(
  signals_m_d_ref,
  oos_predictions_m_d_ref = NULL,
  chosen_score_metric_and_position = NULL,
  scaler_m_d_ref = NULL,
  chosen_scaler = NULL,
  scaler_shrinkage = 0,
  lower_quantile_winsorization,
  upper_quantile_winsorization
)
```

## Arguments

- signals_m_d_ref:

  A data frame containing at least the columns `id`, `tickers`, and one
  or more signal metrics.

- oos_predictions_m_d_ref:

  Optional. A data frame with out-of-sample predictions. It must contain
  columns `id` and `pred`. If not provided (i.e. `NULL`), the signal
  from `signals_m_d_ref` is used.

- chosen_score_metric_and_position:

  A named character vector indicating the chosen signal metric and its
  associated position. For example, `c("signal" = "long")` implies a
  long position (multiplier 1) while `c("signal" = "short")` implies a
  short position (multiplier -1).

- scaler_m_d_ref:

  Optional. A data frame containing scaling factors with columns `id`,
  `tickers`, `dates`, and one or more scaler metrics. If not provided
  (i.e. `NULL`), no scaling is applied.

- chosen_scaler:

  Optional. A string indicating the chosen scaler metric from
  `scaler_m_d_ref`. If not provided (i.e. `NULL`), no scaling is
  applied.

- scaler_shrinkage:

  Numeric between 0 and 1. If greater than 0, applies shrinkage to the
  scaler values towards 1 (i.e., no scaling). A value of 0 means no
  shrinkage, while a value of 1 means all scalers are set to 1. Default
  is 0.

- lower_quantile_winsorization:

  Numeric. Lower quantile value for winsorization in `signal_transform`.

- upper_quantile_winsorization:

  Numeric. Upper quantile value for winsorization in `signal_transform`.

## Value

A data frame with columns:

- `id`:

  A unique identifier combining the ticker and date.

- `tickers`:

  Ticker symbols.

- `dates`:

  The current date.

- `exp_ret_score_raw`:

  The transformed (winsorized/z-scored and sign-adjusted) score before
  any scaling.

- `scaler`:

  The winsorized and (optionally) shrunk scaling factor. Present only
  when `scaler_m_d_ref` and `chosen_scaler` are supplied.

- `exp_ret_score`:

  The final expected return score - `exp_ret_score_raw` multiplied by
  `scaler` when scaling, otherwise equal to `exp_ret_score_raw`. Always
  the last column.

## Examples

``` r
signals_m_d_ref <- data.frame(
  id = paste0(c("AAA", "BBB", "CCC"), "-2020-01-31"),
  tickers = c("AAA", "BBB", "CCC"),
  dates = as.Date("2020-01-31"),
  book_yield = c(0.08, 0.05, 0.12)
)
# Long tilt on a book-yield characteristic
derive_stock_universe_m_d_ref(
  signals_m_d_ref = signals_m_d_ref,
  chosen_score_metric_and_position = c(book_yield = "long"),
  lower_quantile_winsorization = 0.05,
  upper_quantile_winsorization = 0.95
)
#>               id tickers      dates exp_ret_score_raw exp_ret_score
#> 1 AAA-2020-01-31     AAA 2020-01-31         0.9133122     0.9133122
#> 2 BBB-2020-01-31     BBB 2020-01-31         0.5130420     0.5130420
#> 3 CCC-2020-01-31     CCC 2020-01-31         2.0440738     2.0440738
```
