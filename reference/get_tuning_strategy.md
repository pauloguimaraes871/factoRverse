# Get Hyperparameter Tuning Strategy

Accessor function to retrieve the hyperparameter tuning strategy from an
sb_backtest_config object.

## Usage

``` r
get_tuning_strategy(object)

# S4 method for class 'sb_backtest_config'
get_tuning_strategy(object)

# S4 method for class 'sb_metabacktest_config'
get_tuning_strategy(object)

# S4 method for class 'sb_backtest_results'
get_tuning_strategy(object)
```

## Arguments

- object:

  An sb_backtest_config object.

## Value

The `tuning_strategy` slot of the `sb_backtest_config` object.
