# Perform validation checks on inputs for SB workflow

This function validates and checks various inputs required for a signal
blending workflow.

## Usage

``` r
check_inputs_sb_backtest(
  features_m_df,
  target_m_df,
  training_sample_size,
  target_fwd_name,
  validation_sample_size,
  rebalancing_months,
  split_method,
  signal_universe_m_df,
  backtest_returns_m_xts,
  benchmark_returns_m_xts,
  cov_matrix_benchmark,
  cov_matrix_sample_size,
  cov_estimation_method,
  active_returns,
  signal_themes_m_df,
  rp_method,
  exp_ret_score_tilt,
  exp_ret_score_tilt_eta,
  linkage,
  n_random_ports,
  random_ports_method,
  opt_objective,
  opt_method,
  target_port_m_df,
  ridge_pen,
  n_resamples,
  exp_ret_score_jitter,
  cov_eigval_jitter,
  mmaf_method,
  top_down_proxy_port_method,
  mmaf_group_col,
  micro_port_construction_method,
  macro_port_construction_method,
  macro_concentration_constraint_policy,
  macro_n_random_ports,
  macro_random_ports_method,
  macro_opt_objective,
  macro_opt_method,
  macro_ridge_pen,
  macro_n_resamples,
  macro_exp_ret_score_jitter,
  macro_cov_eigval_jitter,
  macro_rp_method,
  macro_exp_ret_score_tilt,
  macro_exp_ret_score_tilt_eta,
  macro_linkage,
  concentration_constraint_policy,
  custom_signal_weights_m_df,
  sb_algorithm,
  gsm_algorithm,
  custom_objective,
  chosen_eval_metric,
  huber_delta,
  quantile_tau,
  hyper_grid_domain_list,
  tuning_method,
  n_iter,
  k_iter,
  acq,
  init_points,
  early_stop,
  keras_architecture_parameters,
  parallel,
  verbose = TRUE,
  .test_seed
)
```

## Arguments

- features_m_df:

  A data frame or matrix containing features data.

- target_m_df:

  A data frame or matrix containing target variable data.

- training_sample_size:

  Numeric, size of the training sample.

- target_fwd_name:

  Character, name of the forward looking target.

- validation_sample_size:

  Numeric, size of the validation sample.

- rebalancing_months:

  Numeric, number of months for rebalancing.

- split_method:

  Character, method of data splitting (currently only "expanding" is
  supported).

- signal_universe_m_df:

  A (meta) data frame defining the signal universe, including
  per-signal/date eligibility flags (`is_eligible`) and heuristic
  performance metrics.

- backtest_returns_m_xts:

  A xts containing historical backtested returns named according to
  signals in `signals_m_df`,

- benchmark_returns_m_xts:

  A xts with benchmark returns, named accordingly.

- cov_matrix_benchmark:

  Character, the benchmark column in `benchmark_returns_m_xts` used to
  build the active covariance matrix (rp/mvo when
  `active_returns = TRUE`).

- cov_matrix_sample_size:

  Numeric, number of trailing periods used to estimate the covariance
  matrix (rp/hrp/mvo/mmaf).

- cov_estimation_method:

  Character, covariance estimation method (e.g. "sample") for
  rp/hrp/mvo/mmaf.

- active_returns:

  Logical, whether covariance/optimization operate on returns in excess
  of the benchmark.

- signal_themes_m_df:

  A (meta) data frame mapping each signal to a `theme` group; required
  for group-level concentration constraints and MMAF grouping.

- rp_method:

  A parameter specifying the risk parity method to be applied (when
  applicable).

- exp_ret_score_tilt:

  A logical value indicating whether to apply expected return score
  tilting in risk parity or mean-variance optimization (when
  applicable).

- exp_ret_score_tilt_eta:

  A numeric value specifying the exponent used for expected return score
  tilting (when applicable).

- linkage:

  Character, linkage method for hierarchical clustering in HRP.

- n_random_ports:

  A numeric value indicating the number of random portfolios to
  generate.

- random_ports_method:

  A parameter specifying the method for generating random portfolios.

- opt_objective:

  A parameter defining the optimization objective.

- opt_method:

  A parameter specifying the optimization method.

- target_port_m_df:

  A data frame containing target portfolio weights (when applicable).

- ridge_pen:

  A numeric value indicating the ridge penalty (when applicable).

- n_resamples:

  A numeric value specifying the number of resamples (when applicable).

- exp_ret_score_jitter:

  A numeric value indicating the jitter applied to expected return
  scores (when applicable).

- cov_eigval_jitter:

  A numeric value specifying the jitter applied to covariance
  eigenvalues (when applicable).

- mmaf_method:

  Character. Method for micro-macro allocation framework: "bottom_up" or
  "top_down".

- top_down_proxy_port_method:

  Character. Method to create top-down proxy portfolio proxy.

- mmaf_group_col:

  Character. Column name in `stock_groups_m_df` defining groups for
  MMAF.

- micro_port_construction_method:

  Character. Method for micro-level portfolio construction in MMAF.

- macro_port_construction_method:

  Character. Method for macro-level portfolio construction in MMAF.

- macro_concentration_constraint_policy:

  A `list` defining concentration constraints at the macro level.

- macro_n_random_ports:

  Integer. Number of random portfolios for macro-level optimization.

- macro_random_ports_method:

  Character. Method to sample random portfolios at the macro level.

- macro_opt_objective:

  Character. Optimization target for macro-level portfolio.

- macro_opt_method:

  Character. Optimization method for macro-level portfolio.

- macro_ridge_pen:

  Numeric. Ridge penalty for macro-level optimization.

- macro_n_resamples:

  Integer. Number of resamples for robust optimization at the macro
  level.

- macro_exp_ret_score_jitter:

  Numeric. Jitter to add to expected return scores during macro-level
  resampling.

- macro_cov_eigval_jitter:

  Numeric. Jitter to add to covariance matrix eigenvalues during
  macro-level resampling.

- macro_rp_method:

  Character. Risk parity allocation method for macro-level portfolio.

- macro_exp_ret_score_tilt:

  Logical. If TRUE, applies expected return score tilt in macro-level
  RP.

- macro_exp_ret_score_tilt_eta:

  Numeric. Exponent for expected return score tilt in macro-level RP.

- macro_linkage:

  Character. Linkage method for hierarchical clustering in macro-level
  HRP.

- concentration_constraint_policy:

  A `list`/policy object defining signal-level (and optionally
  group-level) active-weight caps.

- custom_signal_weights_m_df:

  A (meta) data frame of user-supplied signal weights; required when
  `sb_algorithm = "custom_weights"`.

- sb_algorithm:

  Character, choice of signal blending algorithm ("ols", "glmnet", "rf",
  "xgb", "nn", "ew", "sw", "rp", "hrp", "mvo", "mmaf",
  "custom_weights").

- gsm_algorithm:

  Character, global surrogate model algorithm used for interpretability
  ("ols" or "tree").

- custom_objective:

  Character, custom objective function for loss.

- chosen_eval_metric:

  Character, chosen evaluation metric ("rmse", "mae", "cp", "rss",
  "mphe", "mpe", "hr", "mape").

- huber_delta:

  Numeric, delta parameter for Huber loss (for "pseudo_huber_error"
  custom objective).

- quantile_tau:

  Numeric, tau parameter for quantile loss (for "quantile_error" custom
  objective).

- hyper_grid_domain_list:

  List, domain list of hyperparameters for tuning.

- tuning_method:

  Character, method of hyperparameter tuning ("random_search",
  "grid_search", "bayesian_opt").

- n_iter:

  number of iterations for tuning.

- k_iter:

  number of iterations for k-fold cross-validation.

- acq:

  Character, acquisition function for Bayesian optimization ("ucb",
  "ei", "poi").

- init_points:

  Numeric, number of initial points for Bayesian optimization.

- early_stop:

  Numeric, number of epochs for early stopping (for "xgb" and "nn"
  algorithms).

- keras_architecture_parameters:

  List, containing units (numeric), n_layers (numeric between 1 and 5),
  activation_function and nn_optimizer ("Adam" or "RMSProp")

- parallel:

  Logical, whether to use parallel computation.

- verbose:

  Logical, whether to print verbose output.

- .test_seed:

  Optional numeric seed used to make validation reproducible in tests.

## Value

NULL. This function is used for validation and does not return a value;
it stops on errors.

## Details

This function performs comprehensive validation checks on various inputs
required for a signal blending workflow. It validates data formats,
correctness of hyperparameters, consistency of dates, and other specific
requirements for different signal blending algorithms.

## References

For more information on signal blending algorithms and their usage,
refer to appropriate documentation.
