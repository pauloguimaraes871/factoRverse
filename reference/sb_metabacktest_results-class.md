# sb_metabacktest_results Class

An S4 class designed to store and manage a collection of
`sb_backtest_results` objects, along with consolidated and time series
evaluation metrics for machine learning models.

## Slots

- `sb_metabacktest_config`:

  An object of class `sb_metabacktest_config` containing the
  configuration for the meta backtest.

- `meta_sb_backtest_results`:

  An object of class `sb_backtest_results` containing the results of the
  meta-learner.

- `base_sb_backtest_results_list`:

  A list of `sb_backtest_results` objects for each base algorithm.

- `base_learners_oos_predictions_m_df`:

  A meta dataframe containing out-of-sample predictions, target and
  errors for each base algorithm.

- `combined_oos_testing_metrics`:

  A list containing data frames with consolidated out-of-sample testing
  evaluation metrics for each algorithm.

- `mean_validation_metrics`:

  A data frame containing the mean validation metrics for each
  algorithm.

- `time_series_oos_testing_metrics`:

  A list of data frames for each evaluation metric over time
  (out-of-sample testing).

- `time_series_validation_metrics`:

  A list of data frames for each evaluation metric over time
  (validation).

- `backtest_identifier`:

  A character string used to identify the backtest.

## Validity

When non-`NULL`, `sb_metabacktest_config` must be a
`sb_metabacktest_config` and `mean_validation_metrics` a `data.frame`;
every element of `base_sb_backtest_results_list` must be a
`sb_backtest_results` and every element of
`combined_oos_testing_metrics` a `data.frame`.
