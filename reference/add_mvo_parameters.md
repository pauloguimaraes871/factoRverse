# Add mvo_parameters to a backtest config

This function allows either directly adding a pre-existing
`mvo_parameters` object or creating one dynamically by passing
additional arguments.

## Usage

``` r
add_mvo_parameters(object, mvo_params, ...)

# S4 method for class 'sb_backtest_config,mvo_parameters'
add_mvo_parameters(object, mvo_params, level = NULL, ...)

# S4 method for class 'sb_backtest_config,missing'
add_mvo_parameters(
  object,
  mvo_params,
  opt_method = "random",
  random_ports_method = "sample",
  n_random_ports = 1000,
  opt_objective = "sharpe",
  ridge_pen = NULL,
  n_resamples = 0,
  exp_ret_score_jitter = 0,
  cov_eigval_jitter = 0,
  level = NULL,
  ...
)

# S4 method for class 'port_backtest_config,mvo_parameters'
add_mvo_parameters(object, mvo_params, level = NULL, ...)

# S4 method for class 'port_backtest_config,missing'
add_mvo_parameters(
  object,
  mvo_params,
  opt_method = "random",
  random_ports_method = "sample",
  n_random_ports = 1000,
  opt_objective = "sharpe",
  ridge_pen = NULL,
  n_resamples = 0,
  exp_ret_score_jitter = 0,
  cov_eigval_jitter = 0,
  level = NULL,
  ...
)
```

## Arguments

- object:

  An object of class `sb_backtest_config` or `port_backtest_config`.

- mvo_params:

  An object of class `mvo_parameters`, or missing if a new object is to
  be created.

- ...:

  Additional arguments used to create a new `mvo_parameters` object when
  `mvo_params` is missing. These arguments must include:

  - **opt_method**: A character indicating the optimization method. The
    only current available method is 'random'.

  - **random_ports_method**: A character string representing the method
    to generate random portfolios. Options are 'sample', 'simplex' or
    'grid'.

  - **n_random_ports**: Number of random portfolios to generate. Only
    needed when `opt_method` is 'random'.

  - **opt_objective**: A character indicating the optimization
    objective. Possible options are 'return', 'risk' or 'sharpe'.

- level:

  A character indicating the level to which the parameters should be
  applied when using 'mmaf' strategy.

- opt_method:

  A character indicating the optimization method.

- random_ports_method:

  A character string representing the method to generate random
  portfolios.

- n_random_ports:

  Number of random portfolios to generate.

- opt_objective:

  A character indicating the optimization objective.

- ridge_pen:

  A numeric value representing the ridge penalty to be used in the
  optimization.

- n_resamples:

  A numeric value indicating the number of bootstrap resamples to
  perform

- exp_ret_score_jitter:

  A numeric value indicating the jitter to be applied to the expected
  return scores

- cov_eigval_jitter:

  A numeric value indicating the jitter to be applied to the covariance
  matrix eigenvalues

## Value

An updated object of class `sb_backtest_config` or
`port_backtest_config` with the `mvo_parameters` added.

## Functions

- `add_mvo_parameters(object = sb_backtest_config, mvo_params = mvo_parameters)`:
  Add existing `mvo_parameters` object to a `sb_backtest_config` object.

- `add_mvo_parameters(object = sb_backtest_config, mvo_params = missing)`:
  Dynamically create a `mvo_parameters` object and add it to a
  `sb_backtest_config` object.

- `add_mvo_parameters(object = port_backtest_config, mvo_params = mvo_parameters)`:
  Add existing `mvo_parameters` object to a `port_backtest_config`
  object.

- `add_mvo_parameters(object = port_backtest_config, mvo_params = missing)`:
  Dynamically create a `mvo_parameters` object and add it to a
  `port_backtest_config` object.
