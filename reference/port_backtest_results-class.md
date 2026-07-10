# S4 Class for Portfolio Backtest Results

This S4 class encapsulates the results and parameters from running a
portfolio backtest based on signals derived from simple stock
characteristics or expected returns from machine learning model
predictions.

## Slots

- `port_backtest_config`:

  An object of class `port_backtest_config` containing the configuration
  parameters for the backtest.

- `port_weights_m_df`:

  A meta dataframe containing the portfolio weights across different
  dates.

- `transactions_log`:

  An object of class `transactions_log` containing the transaction for
  the portfolio.

- `port_costs_m_xts`:

  A meta xts object containing portfolio costs (e.g., direct cost,
  market impact cost, total cost, turnover) indexed by dates.

- `port_metrics_m_xts`:

  A meta xts object containing portfolio performance metrics (if
  provided) indexed by dates.

- `port_returns_m_xts`:

  A meta xts object containing portfolio returns (raw and net returns)
  indexed by dates.

- `final_stock_port`:

  An object of class `stock_port` representing the final stock
  portfolio.

- `port_construction_method`:

  A character string indicating the portfolio construction method used
  (e.g., "ew", "sw", "rp", "mvo", "custom_weights").

- `stock_universe_m_df`:

  A meta dataframe containing the stock universe derived from the
  backtest.

- `final_stock_universe_m_d_ref`:

  A meta dataframe containing the last stock universe.

- `port_stats_m_df`:

  A meta dataframe containing portfolio statistics across different
  dates.

- `final_port_stats_m_d_ref`:

  A meta dataframe containing the final portfolio statistics.

- `port_backtest_workflow`:

  A list detailing the portfolio backtest workflow, including
  parameters, rebalancing dates, and other metadata.

- `backtest_identifier`:

  A character string representing the backtest identifier.

- `update`:

  A logical indicating whether the backtest results are an update or an
  original backtest.
