# Create a Performance Metrics Data Frame

This function calculates a wide range of performance and risk metrics
for financial signals based on backtest returns and a benchmark. The
results are returned as a structured data frame.

## Usage

``` r
create_performance_m_df(
  selected_backtest_returns_corrected_positions_m_xts_upd_ref,
  selected_market_factor_proxy_m_xts_upd_ref,
  active_returns,
  verbose = TRUE
)
```

## Arguments

- selected_backtest_returns_corrected_positions_m_xts_upd_ref:

  An `xts` object containing backtested returns for the selected signals
  Each column represents a signal, and rows represent time periods.

- selected_market_factor_proxy_m_xts_upd_ref:

  A xts containing benchmark returns data.

- active_returns:

  If TRUE, calculate ative returns before applying performance
  functions.

- verbose:

  If TRUE, will print messages to the console.

## Value

A data frame containing performance metrics for the specified signals.
The metrics include:

- **ID Information**: `id`, `tickers`, `dates`.

- **Return Metrics**: Arithmetic mean return, geometric mean return,
  annualized return.

- **Risk Metrics**: Standard deviation, semi deviation, downside
  deviation, maximum drawdown, and more.

- **Performance Ratios**: Sharpe ratio, Sortino ratio, Calmar ratio,
  Omega ratio, Rachev ratio, and others.

- **Additional Metrics**: Average recovery, Hurst index, probabilistic
  Sharpe ratio, and minimum track record.
