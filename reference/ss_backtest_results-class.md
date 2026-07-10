# S4 Class for Signal Selection Backtest Results

This S4 class encapsulates the results and parameters from performing a
signal selection backtest. It includes information about eligible
signals, signal universes, Bayesian fits, and the backtest workflow.

## Value

An S4 object of class `ss_backtest_results`.

## Slots

- `ss_backtest_config`:

  An object of class `ss_backtest_config` containing the configuration
  for the backtest.

- `signal_universe_m_df`:

  A meta dataframe containing the signal universes at each rebalancing
  period.

- `final_signal_universe_m_d_ref`:

  A meta dataframe containing the last signal universe.

- `selected_market_factor_proxy_m_xts`:

  A meta_xts object containing the selected market factor proxy.

- `frequentist_results`:

  A list of frequentist model fit results

- `bayesian_results`:

  A list of Bayesian model fit results

- `p_correction_method`:

  A character string indicating the p-value correction method used.

- `ss_backtest_workflow`:

  A list describing the signal selection backtest workflow, including
  parameters and metadata.

- `backtest_identifier`:

  A character string representing the backtest identifier.

- `update`:

  A logical indicating whether the backtest results should be updated.
