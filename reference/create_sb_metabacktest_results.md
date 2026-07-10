# Create an sb_metabacktest_results Object

Constructs an `sb_metabacktest_results` object from a list of
`sb_backtest_results` objects for base learners a single
`sb_backtest_results` object for the meta learner. It computes
consolidated and time series evaluation metrics for machine learning
backtests.

## Usage

``` r
create_sb_metabacktest_results(
  meta_sb_backtest_results,
  base_sb_backtest_results_list,
  oos_predictions_m_df,
  sb_metabacktest_config,
  ...
)

# S4 method for class 'sb_backtest_results,list,meta_dataframe,sb_metabacktest_config'
create_sb_metabacktest_results(
  meta_sb_backtest_results,
  base_sb_backtest_results_list,
  oos_predictions_m_df,
  sb_metabacktest_config
)
```

## Arguments

- meta_sb_backtest_results:

  A `sb_backtest_results` object for the meta learner

- base_sb_backtest_results_list:

  A named list of `sb_backtest_results` objects for the base learners.

- oos_predictions_m_df:

  A `meta_dataframe` object containing out-of-sample predictions for the
  base learners.

- sb_metabacktest_config:

  An `sb_metabacktest_config` object containing the configuration for
  the meta-learner.

- ...:

  Additional arguments (not used).

## Value

An object of class `sb_metabacktest_results`.
