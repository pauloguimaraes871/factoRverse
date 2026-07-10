# Show Signal Selection Backtest Config

Prints the contents of an `ss_backtest_config` object: config name;
backtest parameters (initial sample size, rebalancing months, active
returns, split method, chosen signals and positions); and, if set, the
`alpha_test_strategy` (delegated to its own `show` method).

## Usage

``` r
# S4 method for class 'ss_backtest_config'
show(object)
```

## Arguments

- object:

  An `ss_backtest_config` object to be displayed.
