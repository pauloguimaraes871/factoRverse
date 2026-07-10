# Add a `hyper_grid_domain` Object

This function adds a `hyper_grid_domain` S4 class to a `tuning_strategy`
or a `sb_backtest_config`. It allows users to add a `hyper_grid_domain`
already built or extracted from other objects.

## Usage

``` r
add_hyper_grid_domain(object, hyper_grid_domain)

# S4 method for class 'tuning_strategy,hyper_grid_domain'
add_hyper_grid_domain(object, hyper_grid_domain)

# S4 method for class 'sb_backtest_config,hyper_grid_domain'
add_hyper_grid_domain(object, hyper_grid_domain)
```

## Arguments

- object:

  An object of class `sb_backtest_config`. Must already have a
  `tuning_strategy` attached (via
  [`add_tuning_strategy()`](https://pauloguimaraes871.github.io/factoRverse/reference/add_tuning_strategy.md))
  – this method writes into `object@tuning_strategy@hyper_grid_domain`
  and will error on a `NULL`/unset strategy.

- hyper_grid_domain:

  An object of class `hyper_grid_domain`.

## Value

The appropriate object with the added `hyper_grid_domain`.

## Functions

- `add_hyper_grid_domain( object = tuning_strategy, hyper_grid_domain = hyper_grid_domain )`:
  Add `hyper_grid_domain` to `tuning_strategy` object

- `add_hyper_grid_domain( object = sb_backtest_config, hyper_grid_domain = hyper_grid_domain )`:
  Add `hyper_grid_domain` to `sb_backtest_config` object
