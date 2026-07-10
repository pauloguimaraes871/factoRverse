# Classify the universe based on signals and other custom and user-defined rules.

The eligibility of a stock/signal portfolio depends on a series of
criteria, as explained in Details. Default behavior is to apply only the
**Only Top Assets Rule**, in which case assets are promoted based on
their signal being above a given quantile.

## Usage

``` r
classify_investment_universe(
  universe_m_d_ref,
  eligibility_quantile_range = NULL,
  min_eligible_assets_fallback = NULL,
  signal_significance_threshold = NULL,
  liquidity_floor_cutoffs = NULL,
  liquidity_m_d_ref = NULL,
  liquidity_constraint_policy = NULL,
  updated_port_weights_m_lstd_ref = NULL,
  turnover_constraint_policy = NULL,
  selected_benchmark = NULL,
  benchmark_weights_m_d_ref = NULL,
  groups_m_d_ref = NULL,
  concentration_constraint_policy = NULL,
  enable_group_representativeness = if (!is.null(groups_m_d_ref) &&
    !is.null(concentration_constraint_policy$max_abs_active_group_weight)) TRUE else
    FALSE,
  is_mmaf = FALSE,
  target_port_m_d_ref = NULL,
  ridge_pen = NULL,
  user_defined_AND_rules_m_d_ref = NULL,
  user_defined_OR_rules_m_d_ref = NULL,
  asset_object = "stocks",
  use_raw_for_eligibility = FALSE,
  verbose = TRUE
)
```

## Arguments

- universe_m_d_ref:

  A data frame of stocks or signals.

- eligibility_quantile_range:

  A numeric vector of length 2 indicating the range of quantiles to be
  used for filtering stocks.

- min_eligible_assets_fallback:

  A numeric value indicating the minimum number of eligible assets to be
  selected.

- signal_significance_threshold:

  A numeric value indicating the threshold for signal significance.

- liquidity_floor_cutoffs:

  Mandatory if `turnover_constraint_policy` and/or
  `liquidity_constraint_policy` are provided. A list of named vectors
  containing cutoff values to classify stocks according to liquidity.
  Each element must be named according to the 5 following liquidity
  classifications: ("micro_caps", "small_caps", "mid_caps", "large_caps"
  and "mega_caps) and the vector must provide named numeric values that
  indicate the minimum acceptable values (adjusted for inflation) for
  stocks to have that classification. Classification should be in
  ascending order (from least liquid to most liquid) for all metrics. If
  set in decimals, values will be interpreted as quantiles and
  classification will be set accordingly. Stocks with liquidity lower
  than micro_caps will receive nano_caps classification.

- liquidity_m_d_ref:

  A data frame containing columns for id, tickers, dates, and one or
  more market liquidity measures (e.g., inflation-adjusted mean
  financial volume). All tickers in the current universe must have a
  unique correspondence in this data frame.

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

- updated_port_weights_m_lstd_ref:

  A data frame containing columns for id, tickers, dates, and weights
  from the old portfolio (pre-rebalancing). All tickers in the current
  stock universe must have a unique correspondence in this data frame.

- turnover_constraint_policy:

  A named list containing objects used to build buffer zones and apply
  turnover constraints.

  - Each element will constitute a `buffer_zone`, being a list with
    three elements:

    - `liquidity_classification` element: A liquidity classification
      (e.g., "micro_caps", "small_caps") for that buffer zone.

    - `top_quantile_buffer`: A numeric value indicating a buffer value
      that relaxes `pre_eligible_assets` for stocks with the specified
      liquidity classification.

    - `turnover_cap`: A numeric value specifying the turnover cap.
      Stocks that are less liquid than specified for a buffer zone and
      have a signal higher than the respective buffer quantile will be
      considered eligible, even if they do not meet the
      `liquidity_floor_rule`.

- selected_benchmark:

  A character vector describing the strategy benchmark.

- benchmark_weights_m_d_ref:

  A data frame containing columns for id, tickers, dates, and current
  benchmark weights columns. All tickers in the current universe must
  have a unique correspondence in this data frame.

- groups_m_d_ref:

  A data frame containing columns for id, tickers, dates, and group
  classification columns following a given classification method. All
  tickers in the current universe must have a unique correspondence in
  the data frame.

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
    asset, the one with the greatest signal will be automatically
    promoted. Note that, in the context of `generate_group_constraints`,
    a `benchmark_weights_m_d_ref` data frame must also be supplied.

- enable_group_representativeness:

  Logical. If TRUE, ensures that at least one asset from each group
  classification is included in the eligible universe.

- is_mmaf:

  If TRUE, indicates that the concentration_constraint_policy refers to
  macro level and thus forces group eligibility.

- target_port_m_d_ref:

  Optional. A data frame containing columns for id, tickers, dates, and
  target portfolio weights.

- ridge_pen:

  Optional. A numeric value indicating the ridge penalty to be used when
  shrinking MVO weights towards the target portfolio.

- user_defined_AND_rules_m_d_ref:

  Optional. A named list of named data frames containing a column with
  tickers, columns with metrics to be passed to the final data frame,
  and a column that describes the filter with the same name as the list
  element. For example, to apply a filter with stocks that begin with A,
  `user_defined_AND_rules_m_d_ref` can contain a data frame element
  named "starts_with_A_rule". This data frame can contain a metric
  column (e.g., the name of the stock), and the descriptive filter
  column must be named "starts_with_A_rule". In this case, the
  "starts_with_A_rule" column should be an integer, and its values must
  be either 1L (stock passes rule) or 0L (stock fails to pass rule). The
  rule will be appended to the filter as a regular promotion rule,
  accumulating with other rules.

- user_defined_OR_rules_m_d_ref:

  Optional. A named list of named data frames containing a column with
  tickers, columns with metrics to be passed to the final data frame,
  and a column that describes the filter with the same name as the list
  element. All tickers in the current stock universe must have a unique
  correspondence in this data frame.

- asset_object:

  A character indicating whether the analysis is being applied to
  "stocks" or "signal_portfolios"

- use_raw_for_eligibility:

  Logical. If TRUE, uses raw scores for eligibility filtering.

- verbose:

  A logical indicating whether to print messages during the function
  execution.

## Details

The function provides additional custom rules and also accepts
user-defined rules.

### Eligibility Criteria

To be promoted as eligible, assets must meet one of the following
criteria:

1.  **Regular Eligibility**

    - **Only Top Assets Rule**: Asset must be in the top quantile as
      specified by `top_quantile`.

      - To ignore this behavior, set `top_quantile` to 0.

    - **Liquidity Floor Rule** (exclusive for stocks): must meet minimum
      liquidity requirements as defined by the liquidity floor rule.

2.  OR **Active Weights Constraint Policy Eligibility:**

    - **Maximum Absolute Individual Active Weight Rule** (exlusive for
      stocks): Benchmark weight must exceed the maximum absolute
      individual active weight threshold.

3.  OR **Turnover Policy Eligibility:** (exclusive for stocks)

    - Stock must be in one of the buffer zones. For this to happen:

      - Stock must be in the top quantile buffer
        (`signal >= top_quantile_buffer`).

      - Stock must be in the pre-rebalancing portfolio.

      - Stock must meet the liquidity classification of the buffer zone.

4.  OR **user_defined_OR_rules Eligibility** (currently only implemented
    for stocks)

5.  OR **Group Representativeness Eligibility:**

    - If there are no stocks or signal portfolios in one of the groups
      specified in `concentration_constraint_policy`, a representative
      will be included according to the best quantile.

6.  AND **user_defined_AND_rules**

### Dominance of Rules

- The **Active Weights Constraint Policy Eligibility** is dominant;
  assets meeting this rule will always be eligible.

- The **Turnover Policy Eligibility** takes precedence over the
  **Liquidity Floor Rule**; thus, a stock in the buffer zone will be
  included even if the liquidity floor rule suggests otherwise.

- Assets that meet **user_defined_OR_rules** will always be promoted.

- Assets that fail to meet **user_defined_AND_rules** will always be
  excluded.

### Signal Portfolios vs. Stocks

The function classifies both stock universes (`asset_object = "stocks"`)
and signal-portfolio universes (`asset_object = "signals"`). For stocks,
pre-eligibility is the **Only Top Assets** quantile rule on
`exp_ret_score`. For signals, pre-eligibility is instead driven by the
statistical significance of the CAPM alpha: if a `pd_alpha` column is
present, the Bayesian rule
`1 - pd_alpha <= signal_significance_threshold` is used; otherwise the
frequentist rule
`adjusted_p_value <= signal_significance_threshold & alpha > 0` is
applied (using the `alpha` column in the no-pooled case, or
`individual_alpha` in the partial-pooled case). This shared logic is why
`classify_investment_universe()` is reused inside
[`define_signal_eligibility()`](https://pauloguimaraes871.github.io/factoRverse/reference/define_signal_eligibility.md).
