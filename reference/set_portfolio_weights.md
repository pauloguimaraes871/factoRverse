# Set Portfolio Weights

This function assigns weights to a portfolio based on the specified
portfolio construction method. It supports various methods including
equal weighting, signal weighting, capitalization weighting,
capitalization scaling, risk parity, and mean-tracking error
optimization. The function also accommodates additional constraints and
policies related to liquidity, turnover, and benchmark weights.

## Usage

``` r
set_portfolio_weights(
  universe_m_d_ref,
  port_construction_method,
  liquidity_m_d_ref = NULL,
  cap_weighting_metric = NULL,
  groups_m_d_ref = NULL,
  covariance_matrix = NULL,
  eligible_returns_m_xts_upd_ref = NULL,
  selected_benchmark_m_xts_upd_ref = NULL,
  active_returns = if (is.null(selected_benchmark_m_xts_upd_ref)) FALSE else TRUE,
  cov_estimation_method = "sample",
  cov_matrix_sample_size = if (is.null(eligible_returns_m_xts_upd_ref)) NULL else
    nrow(eligible_returns_m_xts_upd_ref),
  liquidity_constraint_policy = NULL,
  turnover_constraint_policy = NULL,
  concentration_constraint_policy = NULL,
  n_random_ports = 2000,
  random_ports_method = "sample",
  opt_objective = "sharpe",
  opt_method = "random",
  ridge_pen = NULL,
  n_resamples = 0,
  exp_ret_score_jitter = 0,
  cov_eigval_jitter = 0,
  rp_method = "cyclical-spinu",
  exp_ret_score_tilt = NULL,
  exp_ret_score_tilt_eta = NULL,
  linkage = "single",
  custom_weights_m_d_ref = NULL,
  mmaf_method = "bottom_up",
  top_down_proxy_port_method,
  mmaf_group_col,
  micro_port_construction_method = NULL,
  macro_port_construction_method = NULL,
  macro_concentration_constraint_policy = NULL,
  macro_cap_weighting_metric = NULL,
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
  selected_benchmark = NULL,
  bench_assets_returns_m_xts_upd_ref = NULL,
  level = "port",
  lower_quantile_winsorization = 0.025,
  upper_quantile_winsorization = 0.975,
  parallel = FALSE,
  verbose = TRUE
)
```

## Arguments

- universe_m_d_ref:

  A data frame containing the current universe of signals, including
  their associated metrics. The structure of this data frame depends on
  the portfolio construction method used.

- port_construction_method:

  A character string indicating the method to use for portfolio
  construction. Supported methods are:

  `ew`

  :   Equal-Weighted Portfolio

  `sw`

  :   Signal-Weighted Portfolio

  `cw`

  :   Cap-Weighted Portfolio

  `cs`

  :   Cap-Scaled Portfolio

  `rp`

  :   Risk-Parity Portfolio

  `hrp`

  :   Hierarchical Risk-Parity Portfolio

  `mvo`

  :   Mean-Variance (resampled) Optimization

  `mmaf`

  :   Micro-Macro Allocation Framework Portfolio

  `custom_weights`

  :   User-supplied weights, via `custom_weights_m_d_ref`

- liquidity_m_d_ref:

  An optional data frame or matrix containing liquidity metrics for the
  signals, used for capitalization weighting and scaling. Defaults to
  `NULL`.

- cap_weighting_metric:

  An optional character string specifying the metric to use for
  capitalization weighting or scaling. Defaults to `NULL`.

- groups_m_d_ref:

  An optional data frame used for group constraints and covariance
  matrix estimation. Should include group information if used. Defaults
  to `NULL`.

- covariance_matrix:

  An optional covariance matrix for the eligible tickers. If `NULL`, the
  function will estimate the covariance matrix using the provided return
  data. Defaults to `NULL`.

- eligible_returns_m_xts_upd_ref:

  An optional `xts` object containing return data for the eligible
  tickers, used in covariance matrix estimation for Risk-Parity and MVO
  methods.

- selected_benchmark_m_xts_upd_ref:

  An optional `xts` object containing benchmark returns used to compute
  active returns (only if `active_returns = TRUE`).

- active_returns:

  Logical. If `TRUE`, covariance estimation will use active returns
  (asset returns minus benchmark). Defaults to `FALSE` if
  `selected_benchmark_m_xts_upd_ref` is `NULL`, otherwise `TRUE`.

- cov_estimation_method:

  An optional character string specifying the method for estimating the
  covariance matrix. Defaults to `NULL`.

- cov_matrix_sample_size:

  Integer. Number of time periods (rows in
  `eligible_returns_m_xts_upd_ref`) used to estimate the covariance
  matrix. If `NULL`, uses all available observations.

- liquidity_constraint_policy:

  An optional list specifying the policy for liquidity constraints.
  Defaults to `NULL`.

- turnover_constraint_policy:

  An optional list specifying the policy for turnover constraints.
  Defaults to `NULL`.

- concentration_constraint_policy:

  Optional list specifying concentration constraints for MVO
  optimization. Typically includes `max_abs_active_individual_weight` or
  other individual/sector limits.

- n_random_ports:

  An optional numeric value indicating the number of random portfolios
  to generate for optimization methods. Defaults to `NULL`.

- random_ports_method:

  An optional character string specifying the method for risk parity
  optimization. Defaults to `NULL`.

- opt_objective:

  Objective of mean-tracking error optimization. Defaults to `NULL`.

- opt_method:

  Character. Optimization method for MVO. Defaults to `"random"` and can
  include methods like `"grid"`, `"bayesian"`, or
  `"differential_evolution"`.

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

- rp_method:

  Character. Method to compute the Risk Parity portfolio. Defaults to
  `"cyclical-spinu"`.

- exp_ret_score_tilt:

  Character argument specififying whether tilt must be applied during of
  after risk-parity weights

- exp_ret_score_tilt_eta:

  Numeric. The intensity of the tilt effect when using
  `exp_ret_score_tilt`. Higher values increase the tilt effect.

- linkage:

  Character. Linkage method for hierarchical clustering in Risk Parity.
  Defaults to `"single"`.

- custom_weights_m_d_ref:

  A meta dataframe containing custom user-defined weights. Required when
  `port_construction_method = "custom_weights"`. Must contain columns
  `tickers`, `dates`, and `weights`.

- mmaf_method:

  Character. Method for Micro-Macro Allocation Framework. Options are
  `"bottom_up"` or `"top_down"`. Defaults to `"bottom_up"`.

- top_down_proxy_port_method:

  Character. Method for constructing the top-down proxy portfolio in
  MMAF. Options are `"ew"`, `"sw"`, `"cw"`, `"cs"`, `"rp"`, or `"mvo"`.

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

- macro_cap_weighting_metric:

  Optional character for cap-based macro weighting (if used).

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

- selected_benchmark:

  It controls whether a 'port' object for the benchmark should be
  created and added to portolio results.

- bench_assets_returns_m_xts_upd_ref:

  Optional. A 'xts' object containing returns for all stocks. Needed for
  computing benchmark and port stats.

- level:

  Character. Level of the portfolio object. Options are `"port"`,
  `"benchmark"`, or `"group"`. Defaults to `"port"`. Helps in assessing
  which type of portfolio is being created.

- lower_quantile_winsorization:

  An optional numeric value for lower quantile winsorization when
  handling cap weighting and scaling. Defaults to `NULL`.

- upper_quantile_winsorization:

  An optional numeric value for upper quantile winsorization when
  handling cap weighting and scaling. Defaults to `NULL`.

- parallel:

  Logical. If `TRUE`, enables parallel processing for computationally
  intensive tasks. Defaults to `FALSE`.

- verbose:

  Logical. If `TRUE`, prints progress messages.

## Value

A data frame or object (depending on the portfolio construction method)
with the updated portfolio weights assigned based on the specified
method and constraints.
