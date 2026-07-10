# Define the `mvo_parameters` S4 Class

S4 class to represent a set of configurations for mean-variance
optimization.

## Value

An S4 object of class `port_backtest_config`.

## Slots

- `opt_method`:

  A character indicating the optimization method. The only current
  available method is 'random'. In this case, n_random_portfolios are
  generated under the constraints defined in the mvo_parameters object
  and the one that optimizes the opt_objective will be selected.

- `random_ports_method`:

  A character string representing the method that will be passed to
  PortfolioAnalytics::random_portfolios to generate random portfolios.
  Options are 'sample', 'simplex or 'grid'.

- `n_random_ports`:

  Number of random portfolios to generate. Only needed when opt_method
  is 'random'.

- `opt_objective`:

  A character indicating the optimization objective. Possible options
  are 'return', 'risk' or 'sharpe'.

- `ridge_pen`:

  A numeric value representing the ridge penalty to be applied to
  weights. If NULL, no ridge penalty is applied.

- `n_resamples`:

  Number of resamples to perform when estimating the optimal portfolio.
  If 0, no resampling is performed.

- `exp_ret_score_jitter`:

  A numeric value representing the standard deviation of the Gaussian
  noise to be added to the expected returns scores when estimating the
  optimal portfolio. This is used to introduce randomness in the
  optimization process and avoid overfitting to the expected returns.

- `cov_eigval_jitter`:

  A numeric value representing the standard deviation of the Gaussian
  noise to be added to the covariance matrix eigenvalues when estimating
  the optimal portfolio. This is used to introduce randomness in the
  optimization process and avoid overfitting to the covariance matrix.
