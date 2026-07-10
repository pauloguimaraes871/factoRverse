# Check Inputs for Portfolio Backtest

Validates all inputs required to run a portfolio backtest, including
structure, consistency, and ranges.

## Usage

``` r
check_inputs_port_backtest(
  signals_m_df,
  oos_predictions_m_df,
  chosen_score_metric_and_position,
  rebalancing_months,
  initial_buffer_period,
  port_construction_method,
  eligibility_quantile_range,
  min_eligible_assets_fallback,
  selected_benchmark,
  chosen_scaler,
  scaler_m_df,
  scaler_shrinkage,
  use_raw_for_eligibility,
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
  cov_estimation_method,
  cov_matrix_sample_size,
  active_returns,
  cov_matrix_benchmark,
  daily_stock_returns_m_xts,
  daily_bench_returns_m_xts,
  benchmark_returns_m_xts,
  liquidity_constraint_policy,
  turnover_constraint_policy,
  concentration_constraint_policy,
  liquidity_m_df,
  liquidity_floor_cutoffs,
  main_liquidity_metric,
  enable_group_representativeness,
  stock_groups_m_df,
  benchmark_weights_m_df,
  volatility_m_df,
  fwd_return_m_df,
  transaction_costs_parameters,
  custom_stock_weights_m_df,
  custom_stock_metrics_m_df,
  user_defined_OR_rules_m_df,
  user_defined_AND_rules_m_df,
  lower_quantile_winsorization,
  upper_quantile_winsorization,
  verbose
)
```

## Arguments

- signals_m_df:

  A data frame containing signal data that must be coercible to a
  meta_dataframe. It must have at least the columns for IDs, tickers,
  and dates (columns 1:3) and subsequent columns should be numeric and
  free of NAs. Dates must be of class `Date`.

- oos_predictions_m_df:

  (Optional) A data frame with out-of-sample predictions that must be
  coercible to a meta_dataframe. It is expected to contain the columns:
  `"id"`, `"tickers"`, `"dates"`, and `"pred"`. The IDs in this data
  frame must correspond to the IDs in `signals_m_df` after the initial
  buffer period.

- chosen_score_metric_and_position:

  (Optional) A single-element list specifying the chosen score metric
  and position. Its name(s) must not include the substring `"low_"` and
  the chosen metric must be present as a column name in `signals_m_df`.

- rebalancing_months:

  A numeric value indicating the rebalancing frequency in months. Must
  be numeric and between 1 and 12.

- initial_buffer_period:

  A numeric value specifying the number of initial periods to exclude
  from the backtest.

- port_construction_method:

  A character string indicating the portfolio construction method.
  Allowed values are `"ew"`, `"sw"`, `"cw"`, `"cs"`, `"rp"`, `"hrp"`,
  `"mvo"`, `"mmaf"`, or `"custom_weights"`.

- eligibility_quantile_range:

  A numeric vector of length 2 specifying the eligibility quantile range
  used in portfolio construction.

- min_eligible_assets_fallback:

  A positive integer indicating the fallback minimum number of eligible
  assets (optional).

- selected_benchmark:

  A character string naming the benchmark to be used for active return
  calculation or constraints.

- chosen_scaler:

  Character. Column name in `scaler_m_df` defining the scaling variable.

- scaler_m_df:

  Optional `meta_dataframe` with scaling parameters.

- scaler_shrinkage:

  Numeric. Shrinkage factor for scaling.

- use_raw_for_eligibility:

  Logical. If TRUE, uses raw scores for eligibility filtering.

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

  Character. Linkage method for hierarchical clustering in HRP.

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

- cov_estimation_method:

  A character string specifying the covariance estimation method.
  Required when `port_construction_method` is `"rp"` or `"mvo"`.

- cov_matrix_sample_size:

  A numeric value indicating the sample size used for covariance matrix
  estimation. Required when `port_construction_method` is `"rp"` or
  `"mvo"`.

- active_returns:

  A logical value indicating whether active returns are used. If `TRUE`,
  `daily_bench_returns_m_xts` must be provided.

- cov_matrix_benchmark:

  A character string specifying the benchmark for covariance matrix
  estimation. The specified benchmark must be present as a column in
  `daily_bench_returns_m_xts`.

- daily_stock_returns_m_xts:

  An `xts` object containing daily stock returns. The dates (i.e., the
  index) must be of class `Date`, consecutive, and should cover all
  tickers present in `signals_m_df`.

- daily_bench_returns_m_xts:

  An `xts` object containing daily benchmark returns. The dates must
  match those in `daily_stock_returns_m_xts` and the data must not
  contain NA values.

- benchmark_returns_m_xts:

  An `xts` object with benchmark returns. Dates must be of class `Date`
  and arranged as sequential monthly dates without NAs.

- liquidity_constraint_policy:

  (Optional) An object representing the liquidity constraint policy. It
  is validated by `validate_liquidity_constraint_policy` and requires
  that the corresponding liquidity data is provided.

- turnover_constraint_policy:

  (Optional) An object representing the turnover constraint policy. It
  is validated by `validate_turnover_constraint_policy` and requires
  that liquidity information is available.

- concentration_constraint_policy:

  (Optional) An object representing the concentration constraint policy.
  It is validated by `validate_concentration_constraint_policy` and
  requires corresponding benchmark weights and stock group data.

- liquidity_m_df:

  A data frame containing liquidity information. It must be coercible to
  a meta_dataframe, with non-normalized numeric columns free of NAs, and
  must cover all stocks from `signals_m_df`.

- liquidity_floor_cutoffs:

  A data frame providing liquidity floor cutoff values. It is validated
  by `validate_liquidity_floor_cutoffs` and must include the
  `main_liquidity_metric`.

- main_liquidity_metric:

  A character string specifying the primary liquidity metric. It must be
  present in `liquidity_m_df` and include the substring `"mean_volfin"`.

- enable_group_representativeness:

  A logical value indicating whether to enable group representativeness
  constraints. If `TRUE`, `stock_groups_m_df` must be provided.

- stock_groups_m_df:

  (Optional) A data frame containing stock group data, coercible to a
  meta_dataframe. All stocks in `signals_m_df` should be represented,
  and group columns must be of type character.

- benchmark_weights_m_df:

  (Optional) A data frame with benchmark weights. It must be coercible
  to a meta_dataframe, have numeric columns with values between 0 and 1
  (summing to 1 per date), and cover all stocks in `signals_m_df`.

- volatility_m_df:

  A data frame containing volatility data, coercible to a
  meta_dataframe. Numeric columns must have no NAs, include a
  `"daily_vol"` column, and cover all stocks in `signals_m_df`.

- fwd_return_m_df:

  A data frame containing forward returns, coercible to a
  meta_dataframe. It must contain a column named `"fwd_return_1m"`; the
  data should be numeric (with NAs allowed only at the final dates) and
  must match the structure of `signals_m_df` (IDs, tickers, dates).

- transaction_costs_parameters:

  An object (or list) containing transaction cost parameters. It must
  have the names `"direct_transaction_cost"`, `"strategy_aum"`,
  `"alpha"`, and `"lambda"`, and is validated via
  `validate_transaction_costs_parameters`.

- custom_stock_weights_m_df:

  (Optional) A data frame containing custom stock universe weights that
  is coercible to a meta_dataframe.

- custom_stock_metrics_m_df:

  (Optional) A data frame containing custom stock metrics that is
  coercible to a meta_dataframe.

- user_defined_OR_rules_m_df:

  (Optional) A meta_dataframe with 5 columns defining OR-based
  eligibility filters. The 5th column must be binary (0 or 1).

- user_defined_AND_rules_m_df:

  (Optional) A meta_dataframe with 5 columns defining AND-based
  eligibility filters. The 5th column must be binary (0 or 1).

- lower_quantile_winsorization:

  A numeric value specifying the lower winsorization quantile.

- upper_quantile_winsorization:

  A numeric value specifying the upper winsorization quantile.

- verbose:

  Logical. Whether to print warnings and informational messages.

## Value

`NULL`. This function is used for its side effects; it stops execution
if any input validation fails.

## Details

This function performs comprehensive validation of multiple inputs for a
portfolio backtest.
