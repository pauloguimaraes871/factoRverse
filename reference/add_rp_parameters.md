# Add rp_parameters to a backtest config

This function allows either directly adding a pre-existing
`rp_parameters` object or creating one dynamically by passing additional
arguments.

## Usage

``` r
add_rp_parameters(object, rp_params, ...)

# S4 method for class 'sb_backtest_config,rp_parameters'
add_rp_parameters(object, rp_params, level = NULL, ...)

# S4 method for class 'sb_backtest_config,missing'
add_rp_parameters(
  object,
  rp_params,
  rp_method = "cyclical-spinu",
  exp_ret_score_tilt = "none",
  exp_ret_score_tilt_eta = NULL,
  level = NULL,
  ...
)

# S4 method for class 'port_backtest_config,rp_parameters'
add_rp_parameters(object, rp_params, level = NULL, ...)

# S4 method for class 'port_backtest_config,missing'
add_rp_parameters(
  object,
  rp_params,
  rp_method = "cyclical-spinu",
  exp_ret_score_tilt = "none",
  exp_ret_score_tilt_eta = NULL,
  level = NULL,
  ...
)
```

## Arguments

- object:

  An object of class `sb_backtest_config` or `port_backtest_config`.

- rp_params:

  An object of class `rp_parameters`, or missing if a new object is to
  be created.

- ...:

  Additional arguments used to create a new `rp_parameters` object when
  `rp_params` is missing. These arguments must include:

  - **rp_method**: A character indicating the method to compute the
    risk-parity solution.

- level:

  A character indicating the level to which the parameters should be
  applied when using 'mmaf' strategy.

- rp_method:

  A character indicating the method to compute the risk-parity solution.

- exp_ret_score_tilt:

  A character value indicating the tilt to apply to the expected return
  score.

- exp_ret_score_tilt_eta:

  A numeric indicating the tilt intensity to apply to the expected
  return score.

## Value

An updated object of class `sb_backtest_config` or
`port_backtest_config` with the `rp_parameters` added.

## Functions

- `add_rp_parameters(object = sb_backtest_config, rp_params = rp_parameters)`:
  Add an existing `rp_parameters` object to a `sb_backtest_config`
  object.

- `add_rp_parameters(object = sb_backtest_config, rp_params = missing)`:
  Dynamically create a `rp_parameters` object and add it to a
  `sb_backtest_config` object.

- `add_rp_parameters(object = port_backtest_config, rp_params = rp_parameters)`:
  Add an existing `rp_parameters` object to a `port_backtest_config`
  object.

- `add_rp_parameters(object = port_backtest_config, rp_params = missing)`:
  Dynamically create a `rp_parameters` object and add it to a
  `port_backtest_config` object.
