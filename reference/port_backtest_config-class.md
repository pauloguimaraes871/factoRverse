# Class for Port Backtest Config

An S4 class specifying parameters for backtesting stock-level
portfolios.

## Slots

- `chosen_score_metric_and_position`:

  A character string representing the chosen score metric and position.

- `eligibility_quantile_range`:

  A numeric vector of length 2 representing the quantile range for stock
  selection.

- `min_eligible_assets_fallback`:

  A numeric value representing the minimum number of eligible assets.

- `chosen_scaler`:

  A character string representing the chosen scaler method.

- `scaler_shrinkage`:

  A numeric value representing the scaler shrinkage parameter.

- `use_raw_for_eligibility`:

  Logical; if TRUE, uses raw scores for eligibility instead of scaled
  scores.

- `enable_group_representativeness`:

  Logical; if TRUE, ensures at least one asset in all groups in
  groups_m_d_ref

- `selected_benchmark`:

  A character string representing the selected benchmark.

- `initial_buffer_period`:

  A numeric value representing the initial buffer period.

- `rebalancing_months`:

  A numeric value representing the number of months for rebalancing.

- `cov_est_method`:

  An object of class `cov_est_method` representing the covariance
  estimation method and relevant parameters. Current methods are:
  'sample', 'ewma', 'cc' (constant correlation), 'pca1', 'pca2',
  'shrink_id' (shrinkage to identity matrix), 'shrink_cc' (shrinkage to
  constant correlation). This is only relevant for the covariance-based
  methods 'rp', 'hrp', 'mvo' and 'mmaf'.

- `port_construction_method`:

  A character string representing the type of portfolio. Must be one of
  'ew', 'sw', 'cw', 'cs', 'rp', 'hrp', 'mvo' or 'mmaf' ('custom_weights'
  is not supported for this config). For signal portfolios, 'cw' and
  'cs' are not applicable. For signal portfolios, this is inferred based
  on sb_algorithm.

- `mvo_parameters`:

  An object of class `mvo_parameters` representing the parameters for
  mean-variance optimization. This is only relevant for 'mvo'.

- `rp_parameters`:

  An object of class `rp_parameters` representing the parameters for
  risk parity. This is only relevant for 'rp'.

- `hrp_parameters`:

  An object of class `hrp_parameters` representing the parameters for
  hierarchical risk parity. This is only relevant for 'hrp'.

- `mmaf_parameters`:

  An object of class `mmaf_parameters` representing the parameters for
  the MMAF method. This is only relevant for 'mmaf'.

- `main_liquidity_metric`:

  A character string indicating which of the variables in
  `liquidity_m_df` should be ultimately used.

- `liquidity_floor_cutoffs`:

  Mandatory if `turnover_constraint_policy` and/or
  `liquidity_constraint_policy` are provided. A data.frame containing a
  liquidity_classification column and liquidity metrics that define
  cutoff values to classify stocks according to liquidity. Each
  liquidity_classification must be named according to the 5 following
  liquidity classifications: ("micro_caps", "small_caps", "mid_caps",
  "large_caps" and "mega_caps) and numeric column indicate the minimum
  acceptable values (adjusted for inflation) for stocks to have that
  classification. Classification should be in ascending order (from
  least liquid to most liquid) for all metrics. If set in decimals,
  values will be interpreted as quantiles and classification will be set
  accordingly. Stocks with liquidity lower than micro_caps will receive
  nano_caps classification.

- `liquidity_constraint_policy`:

  The policy to handle liquidity constraints. It is only relevant for
  stocks. Possible elements are:

  - `liquidity_floor_rule`: A character indicating the liquidity
    classification (e.g., micro_caps, small_caps) used to filter stocks.
    Stocks with less liquidity than specified in `liquidity_floor_rule`
    will be considered ineligible. In the case of the
    `generate_box_constraints` function, `liquidity_constraint_policy`
    can also contain:

  - `liquidity_cap_rules`: A named vector with one or many elements used
    to create upper bounds for weights based on a liquidity
    classification. Each element's name and corresponding value
    represents, respectively:

    - `liquidity_classification`: The character classification for the
      cap.

    - `liquidity_cap`: A numeric value indicating the cap (upper bound)
      for stocks with that liquidity classification. Many liquidity caps
      might be created.

- `turnover_constraint_policy`:

  The policy to handle turnover constraints. It is only relevant for
  stocks. Its elements are used to build buffer zones and apply turnover
  constraints.

  - It should contain:

  - `quantile_range_buffer`: A numeric value indicating the increase of
    quantile eligibility (both sides) range to be used for the buffer
    zones.

  - `turnover_cap_rules`: A named vector with one or many elements used
    to create maximum absolute bounds for weights in relation to the old
    portfolio, based on a liquidity classification. Each element's name
    and corresponding value represents, respectively:

  - `liquidity_classification`: The character classification for the
    cap.

  - `turnover_cap`: A numeric value indicating the cap (lower and upper
    bounds) for stocks with that liquidity classification. Many turnover
    caps might be created. Stocks that are less liquid than specified
    for a buffer zone and have a signal higher than the respective
    buffer quantile will be considered eligible, even if they do not
    meet the `liquidity_floor_rule`.

- `concentration_constraint_policy`:

  The policy to handle concentration constraints. This is the only
  constraint that is applicable to either signal or stock portfolios. It
  contains up to to four elements:

  - `benchmark`: A character vector describing the benchmark to be used
    to apply constraint. For signal portfolios, possible options are
    theme_ss or theme_sb. For stock portfolios, there must be a
    correspondence in `benchmark_weights_m_df`

  - `max_abs_active_individual_weight`: The maximum absolute individual
    active weights.

  - `max_abs_active_group_weight`: The maximum absolute sector/theme
    active weight used for creating group constraints in
    `generate_sector_constraints`. If a given sector has no eligible
    stock, the one with the greatest signal will be automatically
    promoted. In case of signal portfolios, during ss_backtest, signals
    with highest alpha_t_values are promoted if
    enable_theme_representativeness is TRUE. Note that, in the context
    of stocks, a `benchmark_weights_m_d_ref` data frame must also be
    supplied.

- `transaction_costs_parameters`:

  An object of class `transaction_costs_parameters` containing the
  parameters for calculating direct and indirect costs.

- `config_name`:

  A character string representing the name of the configuration.
