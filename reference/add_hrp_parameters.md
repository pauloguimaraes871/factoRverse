# Add hrp_parameters to a backtest config

This function allows either directly adding a pre-existing
`hrp_parameters` object or creating one dynamically by passing
additional arguments.

## Usage

``` r
add_hrp_parameters(object, hrp_params, ...)

# S4 method for class 'sb_backtest_config,hrp_parameters'
add_hrp_parameters(object, hrp_params, level = NULL, ...)

# S4 method for class 'sb_backtest_config,missing'
add_hrp_parameters(
  object,
  hrp_params,
  linkage = "single",
  exp_ret_score_tilt = "none",
  exp_ret_score_tilt_eta = NULL,
  level = NULL,
  ...
)

# S4 method for class 'port_backtest_config,hrp_parameters'
add_hrp_parameters(object, hrp_params, level = NULL, ...)

# S4 method for class 'port_backtest_config,missing'
add_hrp_parameters(
  object,
  hrp_params,
  linkage = "single",
  exp_ret_score_tilt = "none",
  exp_ret_score_tilt_eta = NULL,
  level = NULL,
  ...
)
```

## Arguments

- object:

  An object of class `sb_backtest_config` or `port_backtest_config`.

- hrp_params:

  An object of class `hrp_parameters`, or missing if a new object is to
  be created.

- ...:

  Additional arguments used to create a new `hrp_parameters` object when
  `hrp_params` is missing. These arguments must include:

  - **linkage**: A character indicating the linkage method to use for
    hierarchical clustering.

- level:

  A character indicating the level to which the parameters should be
  applied when using 'mmaf' strategy.

- linkage:

  Character indicating the linkage method to use for hierarchical
  clustering.

- exp_ret_score_tilt:

  A character value indicating the tilt to apply to the expected return
  score.

- exp_ret_score_tilt_eta:

  A numeric indicating the tilt intensity to apply to the expected
  return score.

## Value

An updated object of class `sb_backtest_config` or
`port_backtest_config` with the `hrp_parameters` added.

## Functions

- `add_hrp_parameters(object = sb_backtest_config, hrp_params = hrp_parameters)`:
  Add an existing `hrp_parameters` object to a `sb_backtest_config`
  object.

- `add_hrp_parameters(object = sb_backtest_config, hrp_params = missing)`:
  Dynamically create a `hrp_parameters` object and add it to a
  `sb_backtest_config` object.

- `add_hrp_parameters(object = port_backtest_config, hrp_params = hrp_parameters)`:
  Add an existing `hrp_parameters` object to a `port_backtest_config`
  object.

- `add_hrp_parameters(object = port_backtest_config, hrp_params = missing)`:
  Dynamically create a `hrp_parameters` object and add it to a
  `port_backtest_config` object.
