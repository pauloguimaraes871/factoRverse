# Class for Port Meta Backtest Config

An S4 class specifying how meta weights are attributed across a set of
already-backtested portfolios. It wraps a single `port_backtest_config`
describing the meta allocation, together with the rules for reading the
base portfolios' characteristics out of a
[`port_backtest_cohort-class`](https://pauloguimaraes871.github.io/factoRverse/reference/port_backtest_cohort-class.md).
The base portfolios themselves are supplied later, as a cohort, to
[`run_port_backtest`](https://pauloguimaraes871.github.io/factoRverse/reference/run_port_backtest.md).

## Slots

- `meta_port_backtest_config`:

  A `port_backtest_config` describing the meta allocation.

- `type`:

  A character selecting the allocation path, either `"multi_port"` or
  `"risk_targeted"`. Under `"multi_port"` the base portfolios form a
  cross-section scored on a column of
  [`port_universe_m_df-class`](https://pauloguimaraes871.github.io/factoRverse/reference/port_universe_m_df-class.md);
  under `"risk_targeted"` there is no cross-section to rank and one
  risky sleeve is scaled against a residual sleeve. Defaults to
  `"multi_port"`.

- `risk_target_parameters`:

  A
  [`risk_target_parameters-class`](https://pauloguimaraes871.github.io/factoRverse/reference/risk_target_parameters-class.md)
  object on the `"risk_targeted"` path, holding the target level, the
  response exponent, the weight bounds and the residual sleeve. `NULL`
  on the `"multi_port"` path, where there is no sleeve to scale.
  Defaults to `NULL`.

- `return_basis`:

  A character, `"net"` or `"raw"`, selecting which return basis of the
  base portfolios' statistics feeds the meta universe.

- `cost_lookback`:

  `NULL` for an expanding cost average, or a single positive whole
  number of trailing cost observations.

- `stock_cov_matrix_sample_size`:

  Numeric, in **trading days**. The covariance window used by the
  stock-level run for its analytics. It is separate from the meta-level
  window in `meta_port_backtest_config@cov_est_method`, which counts
  **months** because the meta level allocates over portfolios whose
  returns are monthly. Reusing one number for both would mean 36 months
  at one level and 36 days at the other. Defaults to 252.

- `config_name`:

  A character string naming the configuration.

## Which slots act at which level

The wrapped `port_backtest_config` serves two levels at once, and it is
worth being explicit about which of its slots does what:

- meta level, allocating across base portfolios:

  `port_construction_method`, `chosen_score_metric_and_position` (the
  meta score, a column of
  [`port_universe_m_df-class`](https://pauloguimaraes871.github.io/factoRverse/reference/port_universe_m_df-class.md)),
  `eligibility_quantile_range`, `min_eligible_assets_fallback`, the
  scaler slots, `cov_est_method` and the `mvo_parameters` /
  `rp_parameters` / `hrp_parameters` blocks.

- stock level, running the resulting weights as a portfolio:

  `main_liquidity_metric` and `transaction_costs_parameters`, which
  price the trades the meta allocation implies once it is pushed through
  to individual stocks.

- both levels:

  `rebalancing_months` and `initial_buffer_period`, which set the single
  schedule on which meta weights are formed, and `selected_benchmark`.

## Covariance sample size is in months

At meta level the assets are portfolios and their return series are
monthly, so `cov_est_method@cov_matrix_sample_size` counts months rather
than trading days. The default carried by
[`create_port_backtest_config`](https://pauloguimaraes871.github.io/factoRverse/reference/create_port_backtest_config.md)
is 252, which is a daily figure; left unchanged it would ask for 21
years of monthly history. Validity warns when a covariance-based method
is combined with an implausibly long sample.

## See also

[`create_port_metabacktest_config`](https://pauloguimaraes871.github.io/factoRverse/reference/create_port_metabacktest_config.md),
[`port_universe_m_df-class`](https://pauloguimaraes871.github.io/factoRverse/reference/port_universe_m_df-class.md),
[`port_backtest_cohort-class`](https://pauloguimaraes871.github.io/factoRverse/reference/port_backtest_cohort-class.md)
