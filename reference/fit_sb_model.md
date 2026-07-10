# Fit Signal Blending (SB) Model

Fits a Signal Blending (SB) model based on the specified `sb_algorithm`,
preparing and training the model with the given data, hyperparameters,
and constraints. This function dispatches to various modeling workflows,
including OLS, GLMNET, Ranger (RF), XGBoost, Keras (NN), heuristic
portfolios (equal- and signal-weighted, custom weights), Risk Parity
(RP), Hierarchical Risk Parity (HRP), Mean-Variance Optimization (MVO),
or the Micro-Macro Allocation Framework (MMAF), depending on the input.

## Usage

``` r
fit_sb_model(
  sb_algorithm,
  target_fwd_name,
  selected_features_corrected_positions_m_refit,
  target_m_refit,
  selected_full_data_corrected_positions_m_refit_clean = NULL,
  custom_objective_translated,
  huber_delta,
  quantile_tau,
  early_stop,
  keras_architecture_parameters,
  optimal_hyper = NULL,
  chosen_eval_metric_translated,
  most_recent_signal_universe_m_d_ref,
  most_recent_custom_signal_weights_m_d_ref = NULL,
  selected_backtest_returns_corrected_positions_m_xts_upd_ref,
  cov_matrix_sample_size = 36,
  cov_estimation_method = "sample",
  active_returns = TRUE,
  selected_cov_matrix_benchmark_m_xts_upd_ref,
  groups_m_d_ref,
  bench_assets_backtest_returns_corrected_positions_m_xts_upd_ref = NULL,
  selected_benchmark = NULL,
  rp_method = "cyclical-spinu",
  exp_ret_score_tilt = NULL,
  exp_ret_score_tilt_eta = NULL,
  linkage = "single",
  n_random_ports = 2000,
  random_ports_method = "sample",
  opt_objective = "sharpe",
  opt_method = "random",
  ridge_pen = NULL,
  n_resamples = 0,
  exp_ret_score_jitter = 0,
  cov_eigval_jitter = 0,
  target_port_m_d_ref = NULL,
  concentration_constraint_policy = NULL,
  mmaf_method = "bottom_up",
  top_down_proxy_port_method,
  mmaf_group_col,
  micro_port_construction_method = NULL,
  macro_port_construction_method = NULL,
  macro_concentration_constraint_policy = NULL,
  macro_n_random_ports = 2000,
  macro_random_ports_method = "sample",
  macro_opt_objective = "sharpe",
  macro_opt_method = "random",
  macro_ridge_pen = NULL,
  macro_n_resamples = 0,
  macro_exp_ret_score_jitter = 0,
  macro_cov_eigval_jitter = 0,
  macro_rp_method = "cyclical-spinu",
  macro_exp_ret_score_tilt = NULL,
  macro_exp_ret_score_tilt_eta = NULL,
  macro_linkage = "single",
  upper_quantile_winsorization = 0.95,
  lower_quantile_winsorization = 0.05,
  verbose
)
```

## Arguments

- sb_algorithm:

  A `character` specifying the signal blending algorithm. Options
  include: `"ols"`, `"glmnet"`, `"rf"`, `"xgb"`, `"nn"`, `"ew"`, `"sw"`,
  `"rp"`, `"hrp"`, `"mvo"`, `"mmaf"`, `"custom_weights"`.

- target_fwd_name:

  A `character` indicating the target variable's name.

- selected_features_corrected_positions_m_refit:

  A matrix or dataframe containing the features for model refitting.

- target_m_refit:

  A vector containing the target variable for model refitting.

- selected_full_data_corrected_positions_m_refit_clean:

  A cleaned meta-dataframe for refitting the model.

- custom_objective_translated:

  A `character` specifying the custom objective function for
  optimization.

- huber_delta:

  A numeric value specifying the delta parameter for Huber loss (used in
  XGBoost and NN).

- quantile_tau:

  A numeric value specifying the quantile level (used in custom
  objectives).

- early_stop:

  A numeric value specifying the early stopping criteria (if
  applicable).

- keras_architecture_parameters:

  A list containing Keras neural network architecture specifications.

- optimal_hyper:

  A named list of optimal hyperparameters for the specified
  `sb_algorithm`.

- chosen_eval_metric_translated:

  A `character` specifying the evaluation metric for validation.

- most_recent_signal_universe_m_d_ref:

  A meta-dataframe representing the most recent signal universe.

- most_recent_custom_signal_weights_m_d_ref:

  A meta-dataframe containing custom signal weights.

- selected_backtest_returns_corrected_positions_m_xts_upd_ref:

  An `xts` object containing backtested returns for corrected positions.

- cov_matrix_sample_size:

  A numeric value specifying the sample size for covariance matrix
  estimation.

- cov_estimation_method:

  A `character` specifying the method for covariance estimation (e.g.,
  `"sample"`).

- active_returns:

  A logical value indicating whether to use active returns (default:
  `TRUE`).

- selected_cov_matrix_benchmark_m_xts_upd_ref:

  An `xts` object representing the selected market factor proxy.

- groups_m_d_ref:

  A meta-dataframe containing group information for the assets.

- bench_assets_backtest_returns_corrected_positions_m_xts_upd_ref:

  A 'xts' object containg returns data for selected_benchmark. Only used
  in heuristics methods

- selected_benchmark:

  A character vector indicating the selected benchmark tickers. Only
  used in heuristics methods

- rp_method:

  A `character` specifying the method for Risk Parity optimization.

- exp_ret_score_tilt:

  Character argument specififying whether tilt must be applied during of
  after risk-parity weights

- exp_ret_score_tilt_eta:

  Numeric. The intensity of the tilt effect when using
  `exp_ret_score_tilt`. Higher values increase the tilt effect.

- linkage:

  Character. Linkage method for hierarchical clustering in Risk Parity.
  Defaults to `"single"`.

- n_random_ports:

  A numeric value specifying the number of random portfolios to generate
  (for MVO).

- random_ports_method:

  A `character` specifying the method for generating random portfolios.

- opt_objective:

  A `character` specifying the optimization objective (e.g.,
  `"sharpe"`).

- opt_method:

  A `character` specifying the optimization method (e.g., `"random"`).

- ridge_pen:

  Numeric. Ridge penalty for MVO optimization to improve numerical
  stability. Defaults to `NULL`.

- n_resamples:

  Number of resamples for resampled MVO. Defaults to `0`.

- exp_ret_score_jitter:

  Numeric. Standard deviation of jitter added to expected return scores
  during resampling. Defaults to `0`.

- cov_eigval_jitter:

  Numeric. Standard deviation of jitter added to covariance matrix
  eigenvalues during resampling. Defaults to `0`.

- target_port_m_d_ref:

  Optional. A data frame containing columns for id, tickers, dates, and
  target portfolio weights.

- concentration_constraint_policy:

  A policy object defining concentration constraints.

- mmaf_method:

  Character. Method for Micro-Macro Allocation Framework. Options are
  `"bottom_up"` or `"top_down"`. Defaults to `"bottom_up"`.

- top_down_proxy_port_method:

  Character. Method for constructing the top-down proxy portfolio in
  MMAF. Options are `"ew"`, `"sw"`, `"rp"`, or `"mvo"`.

- mmaf_group_col:

  Character. Column name in `groups_m_d_ref` used to define groups for
  MMAF.

- micro_port_construction_method:

  Character micro method used to allocate within groups (e.g., `"ew"`,
  `"rp"`, `"hrp"`, `"mvo"`).

- macro_port_construction_method:

  Character macro method used to allocate across groups (e.g., `"ew"`,
  `"rp"`, `"hrp"`, `"mvo"`). For a strictly *neutral* top-down sector
  allocation, prefer `"ew"`, `"rp"` or `"hrp"` and keep
  `macro_exp_ret_score_tilt = NULL`.

- macro_concentration_constraint_policy:

  Optional list with group-level weight caps.

- macro_n_random_ports:

  Integer, number of random portfolios at macro level.

- macro_random_ports_method:

  Character, sampling method (macro).

- macro_opt_objective:

  Character, optimization objective at macro.

- macro_opt_method:

  Character, optimizer selector at macro.

- macro_ridge_pen:

  Numeric or `NULL`, ridge penalty for macro MVO.

- macro_n_resamples:

  Integer, number of resamples for macro MVO.

- macro_exp_ret_score_jitter:

  Numeric, jitter on sector ER (macro MVO).

- macro_cov_eigval_jitter:

  Numeric, jitter on macro covariance eigenvalues.

- macro_rp_method:

  Character, risk parity method at macro.

- macro_exp_ret_score_tilt:

  Optional numeric vector/column name for RP tilt (macro).

- macro_exp_ret_score_tilt_eta:

  Optional numeric, tilt intensity for macro RP.

- macro_linkage:

  Character, passed to macro hierarchical methods when applicable.

- upper_quantile_winsorization:

  A numeric value specifying the upper winsorization quantile.

- lower_quantile_winsorization:

  A numeric value specifying the lower winsorization quantile.

- verbose:

  A logical value indicating whether to enable verbose output during
  model training.

## Value

An S4 object of class `sb_model`, encapsulating the trained model,
algorithm, and associated metadata.
