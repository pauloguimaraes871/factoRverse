# Add covariance estimation method to a backtest configuration

This function allows either directly adding a pre-existing
`cov_est_method` object or creating one dynamically by passing
additional arguments. When `cov_est_method` is not provided, a new one
will be created using the values for `cov_estimation_method`,
`cov_matrix_sample_size`, and `active_returns`, passed via the `...`
argument.

## Usage

``` r
add_cov_est_method(object, cov_est_method, ...)

# S4 method for class 'sb_backtest_config,cov_est_method'
add_cov_est_method(object, cov_est_method, ...)

# S4 method for class 'sb_backtest_config,missing'
add_cov_est_method(
  object,
  cov_est_method,
  cov_estimation_method = "sample",
  cov_matrix_sample_size = 36,
  active_returns = TRUE,
  cov_matrix_benchmark = NULL,
  ...
)

# S4 method for class 'port_backtest_config,cov_est_method'
add_cov_est_method(object, cov_est_method, ...)

# S4 method for class 'port_backtest_config,missing'
add_cov_est_method(
  object,
  cov_est_method,
  cov_estimation_method = "sample",
  cov_matrix_sample_size = 252,
  active_returns = TRUE,
  cov_matrix_benchmark = NULL,
  ...
)
```

## Arguments

- object:

  An object of class `port_backtest_config`.

- cov_est_method:

  An existing object of class `cov_est_method`.

- ...:

  Additional arguments.

- cov_estimation_method:

  A character string representing the covariance estimation method. Must
  be one of `"sample"`, `"ewma"`, `"cc"`, `"pca1"`, `"pca2"`,
  `"shrink_id"` or `"shrink_cc"`.

- cov_matrix_sample_size:

  Number of periods to subset return sample when estimating the
  covariance matrix. A high number provides

- active_returns:

  logical. If `TRUE`, the covariance matrix is estimated using active
  returns. If `FALSE`, raw returns are used.

- cov_matrix_benchmark:

  A character string representing the benchmark for covariance matrix
  estimation. This is used when `cov_estimation_method` is `"shrink_id"`
  or `"shrink_cc"`.

## Value

An updated object of class `sb_backtest_config` or
`port_backtest_config` with the `cov_est_method` added.

## Functions

- `add_cov_est_method( object = sb_backtest_config, cov_est_method = cov_est_method )`:
  Add existing `cov_est_method` object to a `sb_backtest_config` object.

  This method allows to add an already existing `cov_est_method` object
  to an `sb_backtest_config`.

- `add_cov_est_method(object = sb_backtest_config, cov_est_method = missing)`:
  Create a `cov_est_method` object to a `sb_backtest_config` object.

  This method allows to dynamically create a `cov_est_method` object and
  add to `sb_backtest_config`.

- `add_cov_est_method( object = port_backtest_config, cov_est_method = cov_est_method )`:
  Add existing `cov_est_method` object to a `port_backtest_config`
  object.

  This method allows to add an already existing `cov_est_method` object
  to an `port_backtest_config`.

- `add_cov_est_method(object = port_backtest_config, cov_est_method = missing)`:
  Create a `cov_est_method` object to a `port_backtest_config` object.

  This method allows to dynamically create a `cov_est_method` object and
  add to `port_backtest_config`.
