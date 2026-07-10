# Update Portfolio Backtest The `update_port_backtest` function will take an existing `port_backtest_results` object and update it with new dates. This function is useful when you want to add new dates to an existing backtest without having to re-run the entire backtest.

Update Portfolio Backtest The `update_port_backtest` function will take
an existing `port_backtest_results` object and update it with new dates.
This function is useful when you want to add new dates to an existing
backtest without having to re-run the entire backtest.

## Usage

``` r
update_port_backtest(
  signals_m_df,
  fwd_return_m_df,
  liquidity_m_df,
  volatility_m_df,
  old_results,
  ...
)

# S4 method for class 'meta_dataframe,meta_dataframe,meta_dataframe,meta_dataframe,port_backtest_results'
update_port_backtest(
  signals_m_df,
  fwd_return_m_df,
  liquidity_m_df,
  volatility_m_df,
  old_results,
  updated_sb_backtest_results = NULL,
  scaler_m_df = NULL,
  target_port_m_df = NULL,
  stock_groups_m_df = NULL,
  benchmark_weights_m_df = NULL,
  daily_stock_returns_m_xts = NULL,
  daily_bench_returns_m_xts = NULL,
  benchmark_returns_m_xts = NULL,
  custom_stock_weights_m_df = NULL,
  custom_stock_metrics_m_df = NULL,
  user_defined_OR_rules_m_df = NULL,
  user_defined_AND_rules_m_df = NULL,
  verbose = TRUE,
  parallel = TRUE,
  .test_seed = NULL
)
```

## Arguments

- signals_m_df:

  A meta_dataframe containing the signal features. It must include at
  least the columns `id`, `tickers`, and `dates`.

- fwd_return_m_df:

  A meta_dataframe containing forward returns.

- liquidity_m_df:

  A meta_dataframe containing liquidity metrics.

- volatility_m_df:

  A meta_dataframe containing volatility metrics.

- old_results:

  An object of class `port_backtest_results` specifying the portfolio
  backtest results to be updated.

- ...:

  Additional arguments (if needed).

- updated_sb_backtest_results:

  An optional object of class `sb_backtest_results` or
  `sb_metabacktest_results` used to update the backtest using
  style-based results.

- scaler_m_df:

  An optional meta_dataframe containing scaling factors, if applicable.

- target_port_m_df:

  An optional meta_dataframe containing target portfolio weights for
  shrinkage, if applicable.

- stock_groups_m_df:

  A `meta_dataframe` containing stock group classifications, if
  applicable.

- benchmark_weights_m_df:

  A `meta_dataframe` with benchmark weights.

- daily_stock_returns_m_xts:

  An object of class `meta_xts` with daily stock returns, used in
  covariance estimation.

- daily_bench_returns_m_xts:

  An object of class `meta_xts` with daily benchmark returns, used in
  covariance estimation.

- benchmark_returns_m_xts:

  An object of class `meta_xts` containing benchmark returns over the
  rebalancing periods.

- custom_stock_weights_m_df:

  A `meta_dataframe` with user-defined stock weights for portfolio
  construction.

- custom_stock_metrics_m_df:

  A `meta_dataframe` with user-defined metrics to be used in portfolio
  rules.

- user_defined_OR_rules_m_df:

  A `meta_dataframe` specifying OR-based portfolio inclusion rules.

- user_defined_AND_rules_m_df:

  A `meta_dataframe` specifying AND-based portfolio inclusion rules.

- verbose:

  Logical; if `TRUE`, prints progress messages (default is `TRUE`).

- parallel:

  Logical; if `TRUE`, executes parts of the backtest in parallel
  (default is `TRUE`).

- .test_seed:

  Optional seed for reproducibility when testing.

## Value

An object of class `port_backtest_results` containing the portfolio
backtest results.

## Details

The update re-runs a one-month extension of the existing backtest: it
takes the backtest back to the last covered date (so the now-populated
`fwd_return_m_df` at that date can be used to roll the portfolio
forward), sets `initial_buffer_period` to the previous number of dates,
and verifies that the new input objects are the one-month continuation
of the old ones (matching object names and dates via
[`check_update_backtest_objects()`](https://pauloguimaraes871.github.io/factoRverse/reference/check_update_backtest_objects.md))
before binding the new outputs onto the old results with
[`consolidate_backtest_results()`](https://pauloguimaraes871.github.io/factoRverse/reference/consolidate_backtest_results.md).
If the original backtest was driven by an
`sb_backtest_results`/`sb_metabacktest_results` object, a matching
`updated_sb_backtest_results` (same `backtest_identifier`, one month
ahead) must be supplied here.

## Methods (by class)

- `update_port_backtest( signals_m_df = meta_dataframe, fwd_return_m_df = meta_dataframe, liquidity_m_df = meta_dataframe, volatility_m_df = meta_dataframe, old_results = port_backtest_results )`:
  Updates a portfolio backtest using based on a `port_backtest_results`
  object.

  This method extracts the parameters from the `results` object (of
  class `port_backtest_results`), modifies initial_buffer_period,
  performs the new backtest and then binds results
