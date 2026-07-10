# Show Port Backtest Results

Displays a detailed summary of the `port_backtest_results` object,
including the backtest identifier, configuration (config name,
construction method, chosen score/position, selected benchmark), date
information, stock-universe size, performance information
(portfolio-return means, plus custom portfolio-metric means when a
`port_metrics_m_xts` is available), and the final stock portfolio.

## Usage

``` r
# S4 method for class 'port_backtest_results'
show(object)
```

## Arguments

- object:

  An instance of the `port_backtest_results` class.

## Value

The object is returned invisibly.
