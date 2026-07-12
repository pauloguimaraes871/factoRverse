# Calculate Idiosyncratic Volatility

This function computes the idiosyncratic volatility of a stock by
fitting a linear regression model with `ret_values` as the dependent
variable and `bench_ret_values` as the independent variable.
Idiosyncratic volatility is defined as the square root of the difference
between the variance of the stock returns and the portion explained by
the benchmark (i.e., `beta^2` times the variance of the benchmark
returns).

## Usage

``` r
idio_vol(ret_values, bench_ret_values, na.rm = TRUE)
```

## Arguments

- ret_values:

  A numeric vector of stock returns.

- bench_ret_values:

  A numeric vector of benchmark returns.

- na.rm:

  A logical value indicating whether to remove NA values before
  computation (default is TRUE).

## Value

A numeric value representing the idiosyncratic volatility. If the
computed variance is negative, `NA_real_` is returned.

## Examples

``` r
idio_vol(c(0.05, 0.06, 0.07), c(0.03, 0.04, 0.05))
#> [1] NA
```
