# Add risk_target_parameters to a meta backtest config

Either attaches an existing `risk_target_parameters` object or builds
one from the arguments given. Only meaningful when the configuration's
`type` is `"risk_targeted"`.

## Usage

``` r
add_risk_target_parameters(object, risk_target_params, ...)

# S4 method for class 'port_metabacktest_config,risk_target_parameters'
add_risk_target_parameters(object, risk_target_params, ...)

# S4 method for class 'port_metabacktest_config,missing'
add_risk_target_parameters(
  object,
  risk_target_params,
  residual_ticker,
  target,
  target_metric = c("tracking_error", "volatility"),
  p = 1,
  vol_source = c("ex_ante", "realized_rolling", "supplied"),
  vol_cov_est_method = NULL,
  vol_window = 6,
  exposure_method = c("none", "trend", "ts_adjusted", "as_is"),
  exposure_window = NULL,
  exposure_center = 1,
  exposure_sensitivity = NULL,
  exposure_bounds = c(0, 1),
  min_weight = 0,
  max_weight = 1,
  ...
)
```

## Arguments

- object:

  An object of class `port_metabacktest_config`.

- risk_target_params:

  An object of class `risk_target_parameters`, or missing to build one.

- ...:

  Additional arguments (not used).

- residual_ticker, target, target_metric, p:

  Passed to
  [`create_risk_target_parameters()`](https://pauloguimaraes871.github.io/factoRverse/reference/create_risk_target_parameters.md).

- vol_source, vol_cov_est_method, vol_window:

  Passed to
  [`create_risk_target_parameters()`](https://pauloguimaraes871.github.io/factoRverse/reference/create_risk_target_parameters.md).

- exposure_method, exposure_window, exposure_center,
  exposure_sensitivity, exposure_bounds:

  Passed to
  [`create_risk_target_parameters()`](https://pauloguimaraes871.github.io/factoRverse/reference/create_risk_target_parameters.md).

- min_weight, max_weight:

  Passed to
  [`create_risk_target_parameters()`](https://pauloguimaraes871.github.io/factoRverse/reference/create_risk_target_parameters.md).

## Value

The updated `port_metabacktest_config`.

## Functions

- `add_risk_target_parameters( object = port_metabacktest_config, risk_target_params = risk_target_parameters )`:
  Attach an existing `risk_target_parameters` object.

- `add_risk_target_parameters( object = port_metabacktest_config, risk_target_params = missing )`:
  Build a `risk_target_parameters` object and attach it.

## See also

[`create_risk_target_parameters()`](https://pauloguimaraes871.github.io/factoRverse/reference/create_risk_target_parameters.md),
[risk_target_parameters](https://pauloguimaraes871.github.io/factoRverse/reference/risk_target_parameters-class.md)
