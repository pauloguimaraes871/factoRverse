# Create a port_metabacktest_results Object

Assembles the output of a meta-portfolio backtest, wrapping the
meta-level allocation and the stock-level backtest it implies into a
single object. Normally called by
[`run_port_backtest()`](https://pauloguimaraes871.github.io/factoRverse/reference/run_port_backtest.md)
rather than directly.

## Usage

``` r
create_port_metabacktest_results(
  port_metabacktest_config,
  meta_port_backtest_results,
  port_backtest_cohort,
  port_universe_m_df,
  meta_port_weights_m_df,
  projected_stock_weights_m_df,
  meta_port_stats_m_df,
  final_meta_port
)
```

## Arguments

- port_metabacktest_config:

  The `port_metabacktest_config` used.

- meta_port_backtest_results:

  A `port_backtest_results` for the stock-level portfolio.

- port_backtest_cohort:

  The `port_backtest_cohort` allocated across.

- port_universe_m_df:

  The `port_universe_m_df` the meta weights were chosen from.

- meta_port_weights_m_df:

  A `data.frame` of meta weights per base portfolio per rebalance date,
  coerced to a `weights_m_df`.

- projected_stock_weights_m_df:

  The `weights_m_df` handed to the stock-level backtest.

- meta_port_stats_m_df:

  A `data.frame` of meta-level analytics per rebalance date, coerced to
  a `meta_dataframe`.

- final_meta_port:

  The `port` object for the last meta rebalance date.

## Value

An object of class
[port_metabacktest_results](https://pauloguimaraes871.github.io/factoRverse/reference/port_metabacktest_results-class.md).

## See also

[`run_port_backtest()`](https://pauloguimaraes871.github.io/factoRverse/reference/run_port_backtest.md),
[port_metabacktest_results](https://pauloguimaraes871.github.io/factoRverse/reference/port_metabacktest_results-class.md)
