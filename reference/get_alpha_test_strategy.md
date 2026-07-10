# Get Alpha Test Strategy

Accessor function to retrieve the alpha test strategy from
`ss_backtest_config`, `ss_backtest_results` object.

## Usage

``` r
get_alpha_test_strategy(object)

# S4 method for class 'ss_backtest_config'
get_alpha_test_strategy(object)

# S4 method for class 'ss_backtest_results'
get_alpha_test_strategy(object)

# S4 method for class 'sb_backtest_config'
get_alpha_test_strategy(object)
```

## Arguments

- object:

  An ss_backtest_config object.

## Value

The `alpha_test_strategy` slot of the `ss_backtest_config` object.
