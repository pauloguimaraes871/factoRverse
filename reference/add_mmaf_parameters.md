# Add mmaf_parameters to a backtest config

This function allows either directly adding a pre-existing
`mmaf_parameters` object or creating one dynamically by passing
additional arguments.

## Usage

``` r
add_mmaf_parameters(object, mmaf_params, ...)

# S4 method for class 'sb_backtest_config,mmaf_parameters'
add_mmaf_parameters(object, mmaf_params, ...)

# S4 method for class 'sb_backtest_config,missing'
add_mmaf_parameters(
  object,
  mmaf_params,
  mmaf_method = "bottom_up",
  top_down_proxy_port_method = if (mmaf_method == "top_down") "ew" else NULL,
  mmaf_group_col,
  micro_port_construction_method,
  macro_port_construction_method,
  ...
)

# S4 method for class 'port_backtest_config,mmaf_parameters'
add_mmaf_parameters(object, mmaf_params, ...)

# S4 method for class 'port_backtest_config,missing'
add_mmaf_parameters(
  object,
  mmaf_params,
  mmaf_method = "bottom_up",
  top_down_proxy_port_method = if (mmaf_method == "top_down") "ew" else NULL,
  mmaf_group_col,
  micro_port_construction_method,
  macro_port_construction_method,
  ...
)
```

## Arguments

- object:

  An object of class `sb_backtest_config` or `port_backtest_config`.

- mmaf_params:

  An object of class `mmaf_parameters`, or missing if a new one is to be
  created.

- ...:

  Additional arguments used to create a new `mmaf_parameters` object
  when `mmaf_params` is missing.

- mmaf_method:

  A character indicating the MMAF method to be used. Must be one of
  'top_down' or 'bottom_up'.

- top_down_proxy_port_method:

  A character indicating the proxy portfolio method for top-down MMAF.

- mmaf_group_col:

  A character string (length 1) with the grouping column name.

- micro_port_construction_method:

  A character indicating the micro portfolio construction method.

- macro_port_construction_method:

  A character indicating the macro portfolio construction method.

## Value

An updated object of class `sb_backtest_config` or
`port_backtest_config` with the `mmaf_parameters` added.

## Functions

- `add_mmaf_parameters(object = sb_backtest_config, mmaf_params = mmaf_parameters)`:
  Add an existing `mmaf_parameters` object to a `sb_backtest_config`
  object.

- `add_mmaf_parameters(object = sb_backtest_config, mmaf_params = missing)`:
  Dynamically create a `mmaf_parameters` object and add it to a
  `sb_backtest_config` object.

- `add_mmaf_parameters( object = port_backtest_config, mmaf_params = mmaf_parameters )`:
  Add an existing `mmaf_parameters` object to a `port_backtest_config`
  object.

- `add_mmaf_parameters(object = port_backtest_config, mmaf_params = missing)`:
  Dynamically create a `mmaf_parameters` object and add it to a
  `port_backtest_config` object.
