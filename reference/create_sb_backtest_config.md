# Create sb_backtest_config Object

Constructs an sb_backtest_config object.

## Usage

``` r
create_sb_backtest_config(
  sb_algorithm = "ols",
  target_fwd_name,
  tuning_strategy = NULL,
  training_sample_size,
  rebalancing_months,
  split_method = "expanding",
  chosen_signals_and_positions = NULL,
  custom_objective = NULL,
  keras_architecture_parameters = NULL,
  signal_port_parameters = NULL,
  quantile_tau = 0.5,
  huber_delta = 1,
  config_name = "not_identified"
)
```

## Arguments

- sb_algorithm:

  Character string specifying the signal-blending algorithm. One of
  'ols' (default), 'glmnet', 'rf', 'xgb', 'nn' (ML), or 'ew', 'sw',
  'rp', 'hrp', 'mvo', 'mmaf', 'custom_weights' (heuristic/portfolio).

- target_fwd_name:

  Name of the target variable in `target_m_df`.

- tuning_strategy:

  An object of class tuning_strategy, specifying the strategy for tuning
  hyperparameters.

- training_sample_size:

  Number of observations to include in each training sample.

- rebalancing_months:

  Months (numeric) when model should be rebalanced (refit).

- split_method:

  Character string indicating the data splitting method ('expanding' or
  'rolling').

- chosen_signals_and_positions:

  A named vector of chosen signals and their positions ('long'/'short').
  Defaults to NULL, which is coerced to 'all' (use every signal in
  `features_m_df`).

- custom_objective:

  Character string, or NULL to auto-set. For ML algorithms:
  'squared_error' (default), 'pseudo_huber_error' or 'absolute_error'
  (last two only for 'xgb'/'nn'). For heuristic portfolio algorithms
  ('sw', 'rp', 'hrp', 'mvo', 'mmaf'): a 'max\_'/'min\_' +
  heuristic-metric string (defaults to 'max_info_ratio' when NULL). See
  [`display_valid_custom_objectives()`](https://pauloguimaraes871.github.io/factoRverse/reference/display_valid_custom_objectives.md).

- keras_architecture_parameters:

  An object of class `keras_architecture_parameters` providing
  parameters specific to keras-based neural networks.

- signal_port_parameters:

  An object of class `signal_port_parameters`, specifying the parameters
  for constructing signal portfolios (portfolio-blending).

- quantile_tau:

  Numeric value indicating the tau parameter used for quantile
  regression, between 0 and 1.

- huber_delta:

  Numeric value greater than 0, specifying the delta parameter for Huber
  loss function.

- config_name:

  Name of the backtest configuration.

## Value

An sb_backtest_config object.

## See also

[`add_tuning_strategy()`](https://pauloguimaraes871.github.io/factoRverse/reference/add_tuning_strategy.md),
[`add_hyperparameter()`](https://pauloguimaraes871.github.io/factoRverse/reference/add_hyperparameter.md),
[`add_concentration_constraint_policy()`](https://pauloguimaraes871.github.io/factoRverse/reference/add_concentration_constraint_policy.md),
[`run_sb_backtest()`](https://pauloguimaraes871.github.io/factoRverse/reference/run_sb_backtest.md)
