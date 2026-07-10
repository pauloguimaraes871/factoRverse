# S4 Class for Portfolio Backtest Cohort

This S4 class encapsulates the merged results of multiple portfolio
backtests. It contains the merged portfolio weights (as a
meta_dataframe), portfolio costs, portfolio returns, and portfolio
metrics (each as lists of meta_xts objects), as well as the common
backtest workflow parameters.

## Slots

- `cohort_name`:

  A character string representing the cohort name.

- `port_backtest_results_list`:

  A list of `port_backtest_results` objects, each representing a
  portfolio backtest.

- `port_weights_m_df`:

  A meta_dataframe containing merged portfolio weights.

- `port_costs_m_xts_list`:

  A list of meta_xts objects for portfolio costs (direct_cost,
  market_impact_cost, total_cost, turnover).

- `port_returns_m_xts_list`:

  A list of meta_xts objects for portfolio returns (raw_return,
  net_return, raw_active_return, net_active_return).

- `port_metrics_m_xts_list`:

  A list of meta_xts objects for portfolio metrics.

- `port_stats_m_xts_nested_list`:

  A nested list of meta_xts objects for portfolio statistics.

- `backtest_workflow_common`:

  A list containing the common backtest workflow parameters (used for
  compatibility).
