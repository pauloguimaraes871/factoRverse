# Create port_backtest_config Object

Constructs a `port_backtest_config` object containing all necessary
parameters for backtesting stock-level portfolios.

## Usage

``` r
create_port_backtest_config(
  chosen_score_metric_and_position = NULL,
  eligibility_quantile_range = c(0.9, 1),
  min_eligible_assets_fallback = NULL,
  chosen_scaler = NULL,
  scaler_shrinkage = NULL,
  use_raw_for_eligibility = NULL,
  enable_group_representativeness = NULL,
  selected_benchmark = NULL,
  initial_buffer_period,
  rebalancing_months,
  cov_est_method = NULL,
  port_construction_method = "ew",
  mvo_parameters = NULL,
  rp_parameters = NULL,
  hrp_parameters = NULL,
  mmaf_parameters = NULL,
  main_liquidity_metric,
  liquidity_floor_cutoffs = NULL,
  liquidity_constraint_policy = NULL,
  turnover_constraint_policy = NULL,
  concentration_constraint_policy = NULL,
  transaction_costs_parameters = NULL,
  config_name = "not_identified"
)
```

## Arguments

- chosen_score_metric_and_position:

  An object (or named vector) specifying the expected return score
  metric and its associated position. Required if `sb_backtest_results`
  is not provided.

- eligibility_quantile_range:

  A numeric vector of length 2 (e.g., c(0.9, 1.0)) specifying the
  quantile range used to determine eligible assets.

- min_eligible_assets_fallback:

  A numeric value indicating the minimum number of eligible assets to
  include in the portfolio.

- chosen_scaler:

  An object of class `scaler` specifying the scaling variable to be
  applied to the scores.

- scaler_shrinkage:

  A numeric value between 0 and 1 indicating the shrinkage intensity for
  the scaler.

- use_raw_for_eligibility:

  A logical value indicating whether to use raw scores for determining
  eligibility.

- enable_group_representativeness:

  Logical; if TRUE, ensures at least one asset in all groups in
  groups_m_d_ref

- selected_benchmark:

  A character string indicating the benchmark to use for
  benchmark-relative backtests.

- initial_buffer_period:

  A numeric value indicating the number of initial dates to skip before
  starting the backtest.

- rebalancing_months:

  A numeric vector (e.g., c(3,6,9,12)) indicating the months when the
  portfolio should be rebalanced.

- cov_est_method:

  A `cov_est_method` object specifying the covariance estimation method
  and its parameters. If not provided, a default is created using method
  "sample" and sample size 252; `active_returns` is set to `TRUE` (with
  `cov_matrix_benchmark = selected_benchmark`) when a
  `selected_benchmark` is supplied, and `FALSE` otherwise.

- port_construction_method:

  A character string representing the portfolio construction method.
  Must be one of "ew" (equal-weight), "sw" (signal-weight), "cw"
  (cap-weight), "cs" (cap-scaled), "rp" (risk parity), "hrp"
  (hierarchical risk parity), "mvo" (mean-variance optimization), or
  "mmaf" (micro-macro allocation framework). "custom_weights" is not
  supported through this constructor.

- mvo_parameters:

  An object of class `mvo_parameters` for mean-variance optimization.
  Only required if `port_construction_method` is "mvo". If missing and
  port_construction_method is "mvo", a default is created.

- rp_parameters:

  An object of class `rp_parameters` for risk parity portfolios. Only
  required if `port_construction_method` is "rp". If missing and
  port_construction_method is "rp", a default is created.

- hrp_parameters:

  An object of class `hrp_parameters` for hierarchical risk parity
  portfolios. Only required if `port_construction_method` is "hrp". If
  missing and port_construction_method is "hrp", a default is created.

- mmaf_parameters:

  An object of class `mmaf_parameters` for micro-macro allocation
  framework portfolios. Only required if `port_construction_method` is
  "mmaf". If missing and port_construction_method is "mmaf", a default
  is created (and `enable_group_representativeness` defaults to `TRUE`).

- main_liquidity_metric:

  A character string indicating which liquidity metric (i.e. column in
  liquidity_m_df) to use.

- liquidity_floor_cutoffs:

  An object (e.g., a data frame) containing liquidity cutoff values.

- liquidity_constraint_policy:

  An object of class `liquidity_constraint_policy` (optional).

- turnover_constraint_policy:

  An object of class `turnover_constraint_policy` (optional).

- concentration_constraint_policy:

  An object of class `concentration_constraint_policy` (optional).

- transaction_costs_parameters:

  An object specifying transaction cost parameters (optional).

- config_name:

  A character string representing the name of the configuration.

## Value

An object of class `port_backtest_config`.

## Examples

``` r
# Minimal equal-weighted configuration driven by a single characteristic signal
# (a book-yield tilt), rebalanced semi-annually after a 12-period buffer.
config <- create_port_backtest_config(
  chosen_score_metric_and_position = c(book_yield = "long"),
  eligibility_quantile_range = c(0.8, 1.0),
  initial_buffer_period = 12,
  rebalancing_months = c(6, 12),
  main_liquidity_metric = "mean_volfin_3m",
  port_construction_method = "ew",
  config_name = "ew_book_yield"
)

# Benchmark-relative risk-parity configuration: supplying a selected_benchmark
# makes the default cov_est_method use active returns against that benchmark.
rp_config <- create_port_backtest_config(
  chosen_score_metric_and_position = c(book_yield = "long"),
  selected_benchmark = "ibov",
  initial_buffer_period = 12,
  rebalancing_months = 12,
  main_liquidity_metric = "mean_volfin_3m",
  port_construction_method = "rp",
  config_name = "rp_book_yield"
)
```
