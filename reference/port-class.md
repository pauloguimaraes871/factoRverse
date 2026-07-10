# Portfolio classes for backtesting portfolios

These classes encapsulate various parameters and constraints used in
backtesting portfolios.

## Slots

- `universe_m_d_ref`:

  A `meta_dataframe` containing the universe of assets, with weights and
  per-asset metrics attached.

- `port_construction_method`:

  A `character` string specifying the method used to construct the
  portfolio. Must be one of
  `c("ew", "sw", "cw", "cs", "rp", "hrp", "mvo", "mmaf", "custom_weights")`.

- `eligible_assets`:

  A `character` vector of eligible assets for the portfolio.

- `exp_ret_score`:

  The eligible assets expected return scores. Required when
  `port_construction_method` is `"sw"`, `"cs"` or `"mvo"`, and also for
  `"rp"`, `"hrp"` or `"mmaf"` when an `exp_ret_score` column is present
  in `universe_m_d_ref` (e.g. a risk-parity tilt).

- `covariance_matrix`:

  The eligible assets covariance matrix of returns. Must have rownames
  identical to colnames and be symmetric.

- `correlation_matrix`:

  An object storing the correlation matrix of returns (optional if
  covariance is provided).

- `weights`:

  A `numeric` vector of portfolio weights for eligible assets.

- `rel_risk_contr`:

  An object representing relative risk contributions (must be provided
  if `covariance_matrix` is not `NULL`).

- `clusters`:

  An object for storing clusters of assets (used in HRP).

- `mvo_port_spec`:

  An object of class `portfolio.spec` (from the PortfolioAnalytics or
  similar package) used for Markowitz optimization.

- `ind_max_weights`:

  A numeric vector specifying maximum weight constraints per asset.

- `ind_min_weights`:

  A numeric vector specifying minimum weight constraints per asset.

- `random_port_weights`:

  An object for storing random portfolio weights (used in MVO).

- `groups`:

  An object for grouping assets (e.g., sectors).

- `group_col`:

  A `character` naming the group column of `groups` used for macro/group
  aggregation (used in MMAF and group-level analytics).

- `mmaf_method`:

  An object for storing the MMAF method (used in MMAF).

- `group_cov_matrix`:

  An object for storing the group covariance matrix (used in MMAF).

- `micro`:

  An object for storing the micro-level portfolio (used in MMAF).

- `macro`:

  An object for storing the macro-level portfolio (used in MMAF).

- `selected_benchmark_port`:

  An object for storing the selected benchmark portfolio.

- `port_stats`:

  A one-row `data.frame` of portfolio (and, when applicable, group and
  active/benchmark-relative) analytics for this portfolio.

- `port_name`:

  A `character` giving a unique name or label for the portfolio.

## port-class

The `port` class is a base S4 class specifying general parameters for
portfolio construction.

## signal_port-class

Inherits from `port`. Restricts `port_construction_method` to one of
`c("ew","sw","rp","mvo")`. Additionally, it introduces:

- `heuristic_sb_metric`:

  `ANY` object that must be non-`NULL` when `port_construction_method`
  is `"sw"` or `"mvo"`.

## stock_port-class

Inherits from `port` and introduces:

- `type`:

  `character` specifying the portfolio subtype. Must be one of
  "signal_blend", "single_signal" or "custom_weights".

- `main_liquidity_metric`:

  `ANY` object specifying a liquidity metric; must be non-`NULL` when
  `port_construction_method` is `"cw"` or `"cs"`.

## sb_stock_port-class

Inherits from `stock_port`. Intended for scenarios where `sb_algorithm`
can be one of `"cw"` or `"cs"`, with additional liquidity constraints
required.

## single_signal_stock_port-class

Inherits from `stock_port`. Specialized stock-level portfolio class with
`sb_algorithm` possibly being `"cw"` or `"cs"`, requiring the same
liquidity-related parameters as `sb_stock_port`.
