# Calculate Residual Momentum Score

This function computes the residual momentum score by fitting a linear
regression model with `ret_values` as the dependent variable and
`bench_ret_values` as the independent variable. The score is defined as
the sum of the regression residuals divided by their standard deviation.

## Usage

``` r
res_mom(ret_values, bench_ret_values, na.rm = TRUE)
```

## Arguments

- ret_values:

  A numeric vector of returns for the stock.

- bench_ret_values:

  A numeric vector of benchmark returns corresponding to the same
  periods as `ret_values`.

- na.rm:

  A logical value indicating whether to remove NA values before
  computation (default is TRUE).

## Value

A numeric value representing the residual momentum score. If the
standard deviation of residuals is zero, `NA_real_` is returned.

## Examples

``` r
res_mom(c(0.05, 0.06, 0.07), c(0.03, 0.04, 0.05))
#> [1] 0.5
```
