# S4 Class for Time Series Walk-Forward Validation Results of Signal-Blending Models

This S4 class encapsulates the results and parameters from performing
walk-forward validation on time series data using signal-blending
algorithms. It includes information about the model, data, tuning
process, and performance metrics.

## Value

An S4 object of class `sb_backtest_results` containing all the specified
results and sb_backtest_workflow.

## Slots

- `oos_sb_outputs_m_df`:

  A meta dataframe containing out-of-sample predictions, target and
  errors, all indexed by testing dates.

- `sb_backtest_config`:

  An object of class `sb_backtest_config` containing the configuration
  parameters for the backtest.

- `oos_testing_eval_metrics_m_xts`:

  A meta_xts of evaluation metrics for the out-of-sample testing
  samples.

- `consolidated_eval_metrics`:

  A data frame containing the consolidated evaluation metrics for the
  out-of-sample testing samples.

- `final_sb_model`:

  The final (re)fitted signal blending model with best hyperparameters
  found after tuning. Possibly a object of sb_model S4 class.

- `final_gsm`:

  The final (re)fitted global surrogate model.

- `chosen_eval_metric_validation`:

  A list of data.frames with the chosen evaluation metric calculated for
  the hyperparameter grid.

- `best_hyperparameters_m_xts`:

  A meta_xts containing the best hyperparameters selected during tuning
  for each rebalancing period.

- `validation_eval_metrics_hyper_choice_m_xts`:

  A meta_xts with all evaluation metrics calculated for the set of best
  hyperparameters.

- `feature_importance_m_df`:

  A meta_dataframe containing the feature importance scores for each
  feature.

- `final_feature_importance_m_d_ref`:

  A meta_dataframe containing the final feature importance scores for
  the chosen model.

- `sb_backtest_workflow`:

  A list containing sb_backtest_workflow about the walk-forward
  validation process.

- `backtest_identifier`:

  A character string that identifies the backtest.

## Validity

When non-`NULL`: `sb_backtest_config` must be a `sb_backtest_config`;
`final_sb_model` a `sb_model`; `final_gsm` an `lm` or `rpart`;
`feature_importance_m_df`/`final_feature_importance_m_d_ref` a
`meta_dataframe`; the `*_m_xts` slots `meta_xts` objects.
(Model/importance slots may be empty in an update.)
