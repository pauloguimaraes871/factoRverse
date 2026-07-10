# sb_backtest_config Class

The sb_backtest_config class is designed to define an end-to-end
signal-blending (heuristic or machine learning) experiment, including
the hyperparameter tuning strategy, algorithm parameters, and other
experiment-specific configurations.

## Slots

- `sb_algorithm`:

  Character string specifying the signal-blending algorithm. One of ew
  (Equal Weight), sw (Signal Weighting), rp (Risk Parity), hrp
  (Hierarchical Risk Parity), mvo (Mean-Variance Optimization), mmaf
  (Micro-Macro Allocation Framework), custom_weights, ols (Ordinary
  Least Squares), glmnet (Elastic Net), rf (Random Forest), xgb (eXtreme
  Gradient Boosting), or nn (Keras Neural Network).

- `target_fwd_name`:

  Name of the target variable in `target_m_df`.

- `tuning_strategy`:

  An object of class `tuning_strategy`, specifying the strategy for
  tuning hyperparameters.

- `chosen_signals_and_positions`:

  A character vector indicating which signals to include in the backtest
  and their positions (long and short).

- `split_method`:

  Character string indicating the data splitting method ('expanding' or
  'rolling').

- `training_sample_size`:

  Number of observations to include in each training sample.

- `rebalancing_months`:

  Months (numeric) when model should be rebalanced (refit).

- `custom_objective`:

  Character string specifying the objective. For ML algorithms: one of
  'squared_error', 'pseudo_huber_error', 'absolute_error' (a
  twice-differentiable loss; non-'squared_error' choices only apply to
  'xgb'/'nn'). For heuristic portfolio algorithms ('sw', 'rp', 'hrp',
  'mvo', 'mmaf'): a 'max\_'/'min\_' prefix + a heuristic performance
  metric present in `signal_universe_m_df` (see
  [`display_valid_custom_objectives()`](https://pauloguimaraes871.github.io/factoRverse/reference/display_valid_custom_objectives.md)).
  May be NULL.

- `keras_architecture_parameters`:

  An object of class `keras_architecture_parameters` or NULL, providing
  parameters specific to keras-based neural networks. It includes:

  - **units**: A numeric vector specifying the number of neurons in each
    layer.

  - **n_layers**: An integer indicating the total number of layers in
    the neural network.

  - **activation**: A character vector listing the activation functions
    for each layer (e.g., "relu", "sigmoid", "tanh").

  - **nn_optimizer**: A character string specifying the optimizer used
    for training the model (options: "Adam" or "RMSProp").

  - **batch_norm_option**: A logical vector indicating whether batch
    normalization should be applied after each respective layer (TRUE or
    FALSE).

- `signal_port_parameters`:

  An object of class `signal_port_parameters`, specifying the parameters
  for constructing signal portfolios (portfolio-blending).

- `quantile_tau`:

  A single numeric value indicating the tau parameter used for quantile
  regression, between 0 and 1.

- `huber_delta`:

  A single positive numeric value indicating the boundary that separates
  where the loss function turns from quadratic to linear.

- `config_name`:

  A character string to identify the configuration.

## Validity

- `sb_algorithm` must be one of the 12 supported algorithms.

- `chosen_signals_and_positions` must be `"all"` or a named vector of
  `"long"`/`"short"`.

- `tuning_strategy` is required for ML algorithms and forbidden for
  `ols`/heuristic algorithms; hyperparameter names must match the
  algorithm.

- `early_stop` only for `xgb`/`nn`; `keras_architecture_parameters` only
  for `nn`; `signal_port_parameters` only for `rp`/`hrp`/`mvo`/`mmaf`.

- `split_method` must be `"expanding"`; `rebalancing_months` in 1–12;
  `quantile_tau` in (0,1); `huber_delta` \> 0.
