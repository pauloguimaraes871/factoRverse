# Add slsaf_parameters to a backtest config

Either add an existing `slsaf_parameters` object to a
`port_backtest_config`, or create one dynamically from the same
arguments as
[`create_slsaf_parameters`](https://pauloguimaraes871.github.io/factoRverse/reference/create_slsaf_parameters.md).

## Usage

``` r
add_slsaf_parameters(object, slsaf_params, ...)

# S4 method for class 'port_backtest_config,slsaf_parameters'
add_slsaf_parameters(object, slsaf_params, ...)

# S4 method for class 'port_backtest_config,missing'
add_slsaf_parameters(object, slsaf_params, ...)
```

## Arguments

- object:

  An object of class `port_backtest_config`.

- slsaf_params:

  An object of class `slsaf_parameters`, or missing if a new one is to
  be created.

- ...:

  Additional arguments passed to
  [`create_slsaf_parameters`](https://pauloguimaraes871.github.io/factoRverse/reference/create_slsaf_parameters.md)
  when `slsaf_params` is missing.

## Value

An updated `port_backtest_config` with the `slsaf_parameters` added.

## Functions

- `add_slsaf_parameters( object = port_backtest_config, slsaf_params = slsaf_parameters )`:
  Add an existing `slsaf_parameters` object.

- `add_slsaf_parameters(object = port_backtest_config, slsaf_params = missing)`:
  Dynamically create an `slsaf_parameters` object and add it.
