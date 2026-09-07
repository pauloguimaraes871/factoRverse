# S4 Class for Portfolio Meta Backtest Results

Holds the results of allocating across a cohort of already-backtested
portfolios: the meta allocation itself, the stock-level backtest it
implies, and the objects that connect the two.

## Slots

- `port_metabacktest_config`:

  The `port_metabacktest_config` used.

- `meta_port_backtest_results`:

  A `port_backtest_results` for the stock-level portfolio.

- `port_backtest_cohort`:

  The `port_backtest_cohort` allocated across.

- `port_universe_m_df`:

  The `port_universe_m_df` the meta weights were chosen from.

- `meta_port_weights_m_df`:

  A `weights_m_df` of meta weights per base portfolio per rebalance
  date.

- `projected_stock_weights_m_df`:

  A `weights_m_df` of the stock-level weights those meta weights imply,
  as handed to the stock-level backtest.

- `meta_port_stats_m_df`:

  A `meta_dataframe` of meta-level portfolio analytics per rebalance
  date.

- `final_meta_port`:

  The `port` object for the last meta rebalance date.

- `backtest_identifier`:

  A character identifying the backtest.

## The two levels

A meta backtest happens at two levels and this object keeps both,
because neither answers the other's questions:

- meta level:

  `meta_port_weights_m_df` and `meta_port_stats_m_df` record how much of
  the portfolio each base portfolio was given and what the allocation
  looked like as an allocation, including its expected-return score,
  risk and relative risk contributions across base portfolios.
  `final_meta_port` is the `port` object from the last meta rebalance.

- stock level:

  `meta_port_backtest_results` is an ordinary `port_backtest_results`
  for the portfolio those meta weights imply once pushed through to
  individual stocks. Its returns, costs and turnover are the real ones,
  netted across base portfolios that hold the same names, and it is what
  should be compared against a benchmark or against the base portfolios
  themselves.

Note that the stock-level object reports no expected-return statistics:
its weights are supplied rather than derived, so `exp_ret` and its
ratios are missing by construction. The expected-return view lives at
the meta level, in `meta_port_stats_m_df`.

## Reading meta_port_stats_m_df

Two of its columns are easy to misread.

- `exp_ret` is the weighted average of the meta score after
  [`signal_transform`](https://pauloguimaraes871.github.io/factoRverse/reference/signal_transform.md),
  so it is a dimensionless cross-sectional quantity and not a return in
  percent. `sharpe` inherits that, being `exp_ret / risk`.

- `risk`, and everything else derived from the covariance matrix, is
  measured on the base portfolios' own returns, not on active returns,
  and so is an absolute figure. This holds regardless of
  `cov_est_method@active_returns`, which
  [`create_port_backtest_config`](https://pauloguimaraes871.github.io/factoRverse/reference/create_port_backtest_config.md)
  turns on automatically whenever a benchmark is set: that setting
  reaches only the covariance used to *construct* weights under `rp`,
  `hrp` and `mvo`, because
  [`calculate_port_stats()`](https://pauloguimaraes871.github.io/factoRverse/reference/calculate_port_stats.md)
  re-estimates its own covariance with active returns switched off to
  avoid counting the benchmark twice.

The stock-level object in `meta_port_backtest_results` is
benchmark-relative in the ordinary way and needs no such caveat.

## See also

[`port_metabacktest_config-class`](https://pauloguimaraes871.github.io/factoRverse/reference/port_metabacktest_config-class.md),
[`port_universe_m_df-class`](https://pauloguimaraes871.github.io/factoRverse/reference/port_universe_m_df-class.md),
[`port_backtest_results-class`](https://pauloguimaraes871.github.io/factoRverse/reference/port_backtest_results-class.md)
