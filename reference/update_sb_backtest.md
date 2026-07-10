# Update Signal Blending Backtest

Updates an existing signal blending (SB) backtest by extending it one
month forward. This avoids re-running the entire backtest while
preserving prior results.

## Usage

``` r
update_sb_backtest(features_m_df, target_m_df, old_results, ...)

# S4 method for class 'meta_dataframe,meta_dataframe,sb_backtest_results'
update_sb_backtest(
  features_m_df,
  target_m_df,
  old_results,
  updated_ss_backtest_results = NULL,
  updated_port_backtest_cohort = NULL,
  updated_backtest_returns_m_xts = NULL,
  benchmark_returns_m_xts = NULL,
  signal_themes_m_df = NULL,
  target_port_m_df = NULL,
  custom_signal_weights_m_df = NULL,
  custom_signal_universe_metrics_m_df = NULL,
  verbose = TRUE,
  parallel = TRUE,
  .test_seed = NULL
)

# S4 method for class 'meta_dataframe,meta_dataframe,sb_metabacktest_results'
update_sb_backtest(
  features_m_df,
  target_m_df,
  old_results,
  updated_base_sb_backtest_results,
  updated_base_port_backtest_cohort = NULL,
  updated_base_backtest_returns_m_xts = NULL,
  base_benchmark_returns_m_xts = NULL,
  base_signal_themes_m_df = NULL,
  base_priors_m_df = NULL,
  base_custom_signal_weights_m_df = NULL,
  base_custom_signal_universe_metrics_m_df = NULL,
  updated_meta_port_backtest_cohort = NULL,
  updated_meta_backtest_returns_m_xts = NULL,
  meta_benchmark_returns_m_xts = NULL,
  meta_signal_themes_m_df = NULL,
  meta_priors_m_df = NULL,
  meta_custom_signal_weights_m_df = NULL,
  meta_custom_signal_universe_metrics_m_df = NULL,
  verbose = TRUE,
  parallel = TRUE,
  .test_seed = NULL
)
```

## Arguments

- features_m_df:

  A `meta_dataframe` containing engineered features for the next
  rebalancing date.

- target_m_df:

  A `meta_dataframe` containing target returns aligned with
  `features_m_df`.

- old_results:

  A `sb_backtest_results` or `sb_metabacktest_results` object
  representing previously run backtests.

- ...:

  Additional optional arguments, depending on the method.

- updated_ss_backtest_results:

  Optional `ss_backtest_results` used if signal selection was performed.

- updated_port_backtest_cohort:

  Optional `port_backtest_cohort` used to derive updated signal and
  benchmark returns.

- updated_backtest_returns_m_xts:

  An optional `meta_xts` containing new backtest returns.

- benchmark_returns_m_xts:

  Optional `meta_xts` of benchmark returns.

- signal_themes_m_df:

  Optional `meta_dataframe` with theme classification used in RP/MVO
  strategies.

- target_port_m_df:

  (Optional) Target portfolio weights for shrinkage.

- custom_signal_weights_m_df:

  Optional `meta_dataframe` with custom weights for signals.

- custom_signal_universe_metrics_m_df:

  Optional `meta_dataframe` with custom signal metrics.

- verbose:

  Logical. If `TRUE`, prints progress messages. Default is `TRUE`.

- parallel:

  Logical. If `TRUE`, runs the update in parallel. Default is `TRUE`.

- .test_seed:

  Optional numeric seed for reproducibility.

- updated_base_sb_backtest_results:

  A named list of updated `sb_backtest_results` for each base learner.

- updated_base_port_backtest_cohort:

  Optional `port_backtest_cohort` for the base learners.

- updated_base_backtest_returns_m_xts:

  Optional `meta_xts` of base learners' returns.

- base_benchmark_returns_m_xts:

  Optional `meta_xts` of benchmark returns for base learners.

- base_signal_themes_m_df:

  Optional `meta_dataframe` with base learner signal themes.

- base_priors_m_df:

  Optional `meta_dataframe` with prior beliefs for base learners.

- base_custom_signal_weights_m_df:

  Optional `meta_dataframe` with custom signal weights for base
  learners.

- base_custom_signal_universe_metrics_m_df:

  Optional `meta_dataframe` with custom metrics for base learners.

- updated_meta_port_backtest_cohort:

  Optional `port_backtest_cohort` for the meta learner.

- updated_meta_backtest_returns_m_xts:

  Optional `meta_xts` of returns for the meta learner.

- meta_benchmark_returns_m_xts:

  Optional `meta_xts` with benchmark returns for the meta learner.

- meta_signal_themes_m_df:

  Optional `meta_dataframe` with signal themes for the meta learner.

- meta_priors_m_df:

  Optional `meta_dataframe` with priors for the meta learner.

- meta_custom_signal_weights_m_df:

  Optional `meta_dataframe` with custom weights for the meta learner.

- meta_custom_signal_universe_metrics_m_df:

  Optional `meta_dataframe` with custom metrics for the meta learner.

## Value

An updated `sb_backtest_results` or `sb_metabacktest_results` object,
depending on method used.

## Methods (by class)

- `update_sb_backtest( features_m_df = meta_dataframe, target_m_df = meta_dataframe, old_results = sb_backtest_results )`:
  Updates a base SB backtest (`sb_backtest_results`) by validating new
  data consistency, extracting prior configuration, recalculating the
  necessary one-month update, and appending results.

- `update_sb_backtest( features_m_df = meta_dataframe, target_m_df = meta_dataframe, old_results = sb_metabacktest_results )`:
  Updates a meta-learning SB backtest (`sb_metabacktest_results`) by
  updating both the base and meta learner layers.
