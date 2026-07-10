# Set Top-Down Micro Weights

Constructs a micro-level portfolio within a given group as part of a
hierarchical risk parity allocation. The function subsets the universe,
covariance matrix, and (optionally) liquidity data for the group, scales
constraints relative to group weights, and calls
[`set_portfolio_weights()`](https://pauloguimaraes871.github.io/factoRverse/reference/set_portfolio_weights.md)
to compute intra-group allocations.

## Usage

``` r
set_top_down_micro_weights(
  group_name,
  group_weights = NULL,
  micro_port_construction_method,
  group_members,
  universe_m_d_ref,
  liquidity_m_d_ref = NULL,
  eligible_returns_m_xts_upd_ref = NULL,
  selected_benchmark_m_xts_upd_ref = NULL,
  active_returns = if (is.null(selected_benchmark_m_xts_upd_ref)) FALSE else TRUE,
  cov_estimation_method = "sample",
  cov_matrix_sample_size = if (is.null(eligible_returns_m_xts_upd_ref)) NULL else
    nrow(eligible_returns_m_xts_upd_ref),
  groups_m_d_ref = NULL,
  selected_benchmark = NULL,
  bench_assets_returns_m_xts_upd_ref = NULL,
  concentration_constraint_policy = NULL,
  turnover_constraint_policy = NULL,
  liquidity_constraint_policy = NULL,
  cap_weighting_metric = NULL,
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
  lower_quantile_winsorization = 0.025,
  upper_quantile_winsorization = 0.975
)
```

## Arguments

- group_name:

  Character. Name of the group being processed.

- group_weights:

  Named numeric vector of group weights. Required if any constraint or
  ridge penalty is defined.

- micro_port_construction_method:

  Character. Method used for portfolio construction within the group
  (e.g., `"mvo"`, `"risk_parity"`).

- group_members:

  List mapping group names to member tickers.

- universe_m_d_ref:

  Data frame with stock-level reference data, including tickers and
  optional benchmark/target weights.

- liquidity_m_d_ref:

  (Optional) Data frame with liquidity metrics for the full universe.

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

- groups_m_d_ref:

  An optional data frame used for group constraints and covariance
  matrix estimation. Should include group information if used. Defaults
  to `NULL`.

- selected_benchmark:

  It controls whether a 'port' object for the benchmark should be
  created and added to portolio results.

- bench_assets_returns_m_xts_upd_ref:

  Optional. A 'xts' object containing returns for all stocks. Needed for
  computing benchmark and port stats.

- concentration_constraint_policy:

  (Optional) Policy object controlling max active weights at stock
  level.

- turnover_constraint_policy:

  (Optional) Policy object controlling stock or group-level turnover
  caps.

- liquidity_constraint_policy:

  (Optional) Policy object controlling stock or group-level liquidity
  caps.

- cap_weighting_metric:

  (Optional) Market cap or related metric for cap-weighted allocations.

- rp_method:

  (Optional) Risk parity method.

- exp_ret_score_tilt:

  (Optional) Expected return tilt applied under risk parity.

- exp_ret_score_tilt_eta:

  (Optional) Tilt intensity for expected return tilt.

- linkage:

  (Optional) Linkage method for hierarchical clustering in risk parity.

- n_random_ports:

  (Optional) Number of random portfolios used for resampling.

- random_ports_method:

  (Optional) Sampling method for random portfolios.

- opt_objective:

  (Optional) Optimization objective (e.g., `"min_var"`, `"sharpe"`).

- opt_method:

  (Optional) Numerical optimization routine.

- ridge_pen:

  (Optional) Ridge penalty term for regularized optimization.

- n_resamples:

  (Optional) Number of resamples used in MVO.

- exp_ret_score_jitter:

  (Optional) Jitter applied to expected return scores.

- cov_eigval_jitter:

  (Optional) Jitter applied to covariance eigenvalues.

- lower_quantile_winsorization:

  (Optional) Lower quantile cutoff.

- upper_quantile_winsorization:

  (Optional) Upper quantile cutoff.

## Value

A portfolio object returned by
[`set_portfolio_weights()`](https://pauloguimaraes871.github.io/factoRverse/reference/set_portfolio_weights.md)
with weights for tickers in the specified group.

## Details

- Scales benchmark, BOP, and target weights by the group allocation if
  constraints are active.

- Normalizes scaled weights to sum to 1 if needed.

- Scales concentration, turnover, and liquidity constraints relative to
  the group weight.

- Removes pre-existing weight columns if no constraints are active.

## See also

[`process_micro_portfolios()`](https://pauloguimaraes871.github.io/factoRverse/reference/process_micro_portfolios.md),
[`set_portfolio_weights()`](https://pauloguimaraes871.github.io/factoRverse/reference/set_portfolio_weights.md)
