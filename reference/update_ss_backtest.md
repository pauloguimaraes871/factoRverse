# Update Signal Selection Backtest The `update_ss_backtest` function will take an existing `port_backtest_results` object and update it with new dates. This function is useful when you want to add new dates to an existing backtest without having to re-run the entire backtest.

Update Signal Selection Backtest The `update_ss_backtest` function will
take an existing `port_backtest_results` object and update it with new
dates. This function is useful when you want to add new dates to an
existing backtest without having to re-run the entire backtest.

## Usage

``` r
update_ss_backtest(
  signals_m_df,
  updated_backtest_returns_m_xts,
  updated_port_backtest_cohort,
  benchmark_returns_m_xts,
  signal_themes_m_df,
  old_results,
  ...
)

# S4 method for class 'meta_dataframe,missing,port_backtest_cohort,meta_xts,meta_dataframe,ss_backtest_results'
update_ss_backtest(
  signals_m_df,
  updated_port_backtest_cohort,
  benchmark_returns_m_xts,
  signal_themes_m_df,
  old_results,
  priors_m_df = NULL,
  custom_signal_universe_metrics_m_df = NULL,
  verbose = TRUE,
  parallel = TRUE
)

# S4 method for class 'meta_dataframe,meta_xts,missing,meta_xts,meta_dataframe,ss_backtest_results'
update_ss_backtest(
  signals_m_df,
  updated_backtest_returns_m_xts,
  benchmark_returns_m_xts,
  signal_themes_m_df,
  old_results,
  priors_m_df = NULL,
  custom_signal_universe_metrics_m_df = NULL,
  verbose = TRUE,
  parallel = TRUE
)
```

## Arguments

- signals_m_df:

  A meta_dataframe containing the signal features. It must include at
  least the columns `id`, `tickers`, and `dates`.

- updated_backtest_returns_m_xts:

  An up-to-date xts containing historical backtested returns named
  according to signals in `signals_m_df`,

- updated_port_backtest_cohort:

  An up-to-date `port_backtest_cohort` object containing historical
  backtested returns named according to signals in `signals_m_df`,

- benchmark_returns_m_xts:

  A xts with benchmark returns, named accordingly.

- signal_themes_m_df:

  A (meta) data frame with "id", "tickers" ("signals"), and "dates"
  columns, including all signals in `signals_m_df`, and a "theme" column
  providing group membership for each signal.

- old_results:

  An object of class `ss_backtest_results` to be updated with new
  results.

- ...:

  Additional arguments (not used in this method).

- priors_m_df:

  A (meta) data frame with columns including "id", "ticker", "dates",
  "theme" (used for clustering in the Bayesian hierarchical model), and
  values for active_return, bench_return, alpha (mean and se), beta
  (mean and se), and sigma. Data should be exogenous, as it will be used
  to set priors for the hierarchical Bayesian model.

- custom_signal_universe_metrics_m_df:

  A `meta_dataframe` containing user-defined metrics for the signal
  universe, used for custom filtering or classification.

- verbose:

  A boolean indicating whether to print messages.

- parallel:

  A boolean indicating whether to run the backtest in parallel.

## Value

An object of class `ss_backtest_results` containing the portfolio
backtest results.

## Functions

- `update_ss_backtest( signals_m_df = meta_dataframe, updated_backtest_returns_m_xts = missing, updated_port_backtest_cohort = port_backtest_cohort, benchmark_returns_m_xts = meta_xts, signal_themes_m_df = meta_dataframe, old_results = ss_backtest_results )`:
  Updates a signal selection backtest based on a `ss_backtest_results`
  object and a `port_backtest_cohort`.

  This method extracts the parameters from the `results` object (of
  class `ss_backtest_results`), modifies initial_sample_size, performs
  the new backtest and then binds results to the old results.

- `update_ss_backtest( signals_m_df = meta_dataframe, updated_backtest_returns_m_xts = meta_xts, updated_port_backtest_cohort = missing, benchmark_returns_m_xts = meta_xts, signal_themes_m_df = meta_dataframe, old_results = ss_backtest_results )`:
  Updates a signal selection backtest based on a `ss_backtest_results`
  object and a `backtest_returns_m_xts`.

  This method extracts the parameters from the `results` object (of
  class `ss_backtest_results`), modifies initial_sample_size, performs
  the new backtest and then binds results to the old results.
