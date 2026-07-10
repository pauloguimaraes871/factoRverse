# Show Method for signal_universe_m_df Class

This method extends the parent `meta_dataframe` show method by
displaying additional elements from `ss_backtest_workflow` and
`sb_backtest_workflow`. It focuses on the key fields you specified:

- **ss_backtest_workflow**: active_returns, model_structure,
  market_factor_proxy, backtest_type, p_correction_method,
  theme_level_intercept (can be NULL), theme_level_slope (can be NULL),
  signals_object_name, signal_themes_object_name, priors_object_name,
  backtest_returns_object_name, rebalancing_months

- **sb_backtest_workflow**: sb_algorithm, custom_objective,
  backtest_type, keras_architecture_parameters, tuning_method,
  chosen_eval_metric, huber_delta, quantile_tau

- Note: `sb_backtest_workflow` can be `NULL`.

## Usage

``` r
# S4 method for class 'signal_universe_m_df'
show(object)
```

## Arguments

- object:

  An instance of the `signal_universe_m_df` class.

## Value

Returns the object invisibly.
