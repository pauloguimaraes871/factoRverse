# Create a Resampled MVO portfolio

Create a Resampled MVO portfolio

## Usage

``` r
create_resampled_mvo_portfolio(
  universe_m_d_ref,
  covariance_matrix,
  liquidity_constraint_policy = NULL,
  turnover_constraint_policy = NULL,
  concentration_constraint_policy = NULL,
  groups_m_d_ref = NULL,
  n_random_ports = 2000,
  random_ports_method = "sample",
  opt_objective = "sharpe",
  opt_method = "random",
  ridge_pen = NULL,
  n_resamples = 0,
  exp_ret_score_jitter = 0,
  cov_eigval_jitter = 0,
  parallel = FALSE,
  verbose = TRUE
)
```

## Arguments

- universe_m_d_ref:

  A dataframe with tickers, is_eligible and exp_ret_score columns

- covariance_matrix:

  The covariance matrix of all eligible stocks in universe_m_d_ref.

- liquidity_constraint_policy:

  Optional. A named list containing objects used to apply liquidity
  constraints. Possible elements of the list are:

  - `liquidity_floor_rule`: A character indicating the liquidity
    classification (e.g., micro_caps, small_caps) used to filter stocks.
    Stocks with less liquidity than specified in `liquidity_floor_rule`
    will be considered ineligible. In the case of the
    `generate_box_constraints` function, `liquidity_constraint_policy`
    can also contain:

  - `liquidity_cap_rule` lists: One or many lists used to create upper
    bounds for weights based on a liquidity classification. Each list
    must contain:

    - `liquidity_classification`: A character indicating the
      classification for the cap.

    - `liquidity_cap`: A numeric value indicating the cap (upper bound)
      for stocks with that liquidity classification. Many liquidity caps
      might be created, and in this case, each `liquidity_cap_rule` must
      be identified with a number (e.g., liquidity_cap_rule_1,
      liquidity_cap_rule_2, and so on).

- turnover_constraint_policy:

  A named list containing objects used to build buffer zones and apply
  turnover constraints.

  - Each element will constitute a `buffer_zone`, being a list with
    three elements:

    - `liquidity_classification` element: A liquidity classification
      (e.g., "micro_caps", "small_caps") for that buffer zone.

    - `top_assets_quantile_buffer`: A numeric value indicating a buffer
      value that relaxes `top_assets_quantile` for stocks with the
      specified liquidity classification.

    - `turnover_cap`: A numeric value specifying the turnover cap.
      Stocks that are less liquid than specified for a buffer zone and
      have a signal higher than the respective buffer quantile will be
      considered eligible, even if they do not meet the
      `liquidity_floor_rule`.

- concentration_constraint_policy:

  A named list containing up to four elements:

  - `benchmark`: A character vector describing the benchmark to be used
    to apply constraint. Must have a correspondence in
    `benchmark_weights_m_d_ref`

  - `max_abs_active_individual_weight`: The maximum absolute individual
    active weights.

  - `max_abs_active_group_weight`: The maximum absolute group active
    weight used for creating group constraints in
    `generate_group_constraints`. If a given group has no eligible
    stock, the one with the greatest signal will be automatically
    promoted. Note that, in the context of `generate_group_constraints`,
    a `benchmark_weights_m_d_ref` data frame must also be supplied.

- groups_m_d_ref:

  A data frame containing columns for id, tickers, dates, and group
  classification columns following a given classification method. All
  tickers in the current stock universe must have a unique
  correspondence in the data frame.

- n_random_ports:

  An integer indicating the number of random portfolios to be generated.

- random_ports_method:

  A character indicating the method to be used for generating random
  portfolios. Possible values are "random" or "grid".

- opt_objective:

  A character describing the objective to maximize in order to choose
  the best portfolio. One of "return (max return)", "risk (min risk)" or
  "sharpe (max sharpe-ratio)"

- opt_method:

  A character describing the optimization method to be used. One of
  "random" or "DEoptim".

- ridge_pen:

  Optional. A numeric value representing the ridge penalty to be applied
  when a target portfolio is provided. Higher values will increase the
  importance of being close to the target portfolio.

- n_resamples:

  An integer indicating the number of resamples to be performed. Default
  is 0 (no resampling).

- exp_ret_score_jitter:

  A numeric value indicating the standard deviation of the normal
  distribution used to jitter expected return scores. A value of 0 means
  no jittering. Default is 0.

- cov_eigval_jitter:

  A numeric value indicating the standard deviation of the log-normal
  distribution used to jitter the eigenvalues of the covariance matrix.

- parallel:

  A logical indicating whether to run resamples in parallel using the
  furrr package.

- verbose:

  A logical indicating whether to print messages during the execution of
  the function.
