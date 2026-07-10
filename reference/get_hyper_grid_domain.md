# Get Hyperparameter Grid Domain

Accessor function to retrieve the hyperparameter grid domain.

## Usage

``` r
get_hyper_grid_domain(object)

# S4 method for class 'sb_backtest_config'
get_hyper_grid_domain(object)

# S4 method for class 'tuning_strategy'
get_hyper_grid_domain(object)

# S4 method for class 'sb_metabacktest_config'
get_hyper_grid_domain(object)

# S4 method for class 'sb_backtest_results'
get_hyper_grid_domain(object)
```

## Arguments

- object:

  An sb_backtest_config or tuning_strategy object

## Value

The `hyper_grid_domain` object stored in the `tuning_strategy`.
