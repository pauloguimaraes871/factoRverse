# Internal helper: fit regression of signal returns on benchmark returns

Internal helper: fit regression of signal returns on benchmark returns

## Usage

``` r
bench_regression_fit(
  ret_values,
  bench_ret_values,
  mult_last_n = 0,
  mult_by = -1,
  na.rm = TRUE,
  include_intercept = TRUE
)
```

## Arguments

- ret_values:

  Numeric vector (signal returns).

- bench_ret_values:

  Numeric vector (benchmark returns).

- mult_last_n:

  Integer \>= 0. If \> 0, multiply the last n (in time order) of
  ret_values by -1 before fitting the regression. Default is 0.

- mult_by:

  Numeric scalar. The number to multiply the last n values by. Default
  is -1 (inversion).

- na.rm:

  Logical. If TRUE, drop NA positions from ret_values (and aligned
  bench).

- include_intercept:

  Logical. If TRUE, fit y ~ 1 + x, else fit y ~ x (no intercept).

## Value

A list with elements: alpha, beta, residuals, residual_sd, n_obs.
