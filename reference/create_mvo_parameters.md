# Create MVO Parameters

Constructor function for creating an instance of `mvo_parameters`.

## Usage

``` r
create_mvo_parameters(
  opt_method = "random",
  random_ports_method = "sample",
  n_random_ports = 1000,
  opt_objective = "sharpe",
  ridge_pen = NULL,
  n_resamples = 0,
  exp_ret_score_jitter = 0,
  cov_eigval_jitter = 0
)
```

## Arguments

- opt_method:

  A character indicating the optimization method. The only current
  available method is 'random'. In this case, `n_random_ports` are
  generated under the constraints defined in the `mvo_parameters` object
  and the one that optimizes the `opt_objective` will be selected.

- random_ports_method:

  A character string representing the method that will be passed to
  [`PortfolioAnalytics::random_portfolios`](https://rdrr.io/pkg/PortfolioAnalytics/man/random_portfolios.html)
  to generate random portfolios. Options are 'sample', 'simplex' or
  'grid'.

- n_random_ports:

  Number of random portfolios to generate. Only needed when `opt_method`
  is 'random'.

- opt_objective:

  A character indicating the optimization objective. Possible options
  are 'return', 'risk' or 'sharpe'.

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

An S4 object of class `mvo_parameters`.
