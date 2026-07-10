# Calculate Alpha Relative to a Benchmark

This function computes the *alpha* of a return series relative to a
benchmark, defined as the intercept of a linear regression of the form:

## Usage

``` r
alpha_bench(
  ret_values,
  bench_ret_values,
  mult_last_n = 0,
  mult_by = -1,
  na.rm = TRUE
)
```

## Arguments

- ret_values:

  A numeric vector of asset returns.

- bench_ret_values:

  A numeric vector of benchmark returns aligned in time with
  `ret_values`.

- mult_last_n:

  Integer \>= 0. If \> 0, multiply the last n (in time order) of
  `ret_values` by 'mult_by' before fitting the regression. Default is 0.

- mult_by:

  Numeric scalar. The number to multiply the last n values by. Default
  is -1 (inversion).

- na.rm:

  Logical. If `TRUE`, removes observations where `ret_values` is `NA`
  and drops the corresponding benchmark observations. Default is `TRUE`.

## Value

A numeric scalar representing the estimated regression intercept
(alpha). Returns `NA_real_` if the regression cannot be estimated (e.g.,
insufficient observations, degenerate design matrix).

## Details

\$\$ r_t = \alpha + \beta b_t + \varepsilon_t \$\$

where \\r_t\\ denotes the asset returns and \\b_t\\ denotes the
benchmark returns. The estimated intercept \\\alpha\\ measures the
average excess return of the asset that is not explained by exposure to
the benchmark.

The regression is estimated using ordinary least squares (OLS). Missing
values in `ret_values` may be removed depending on `na.rm`, while
missing or infinite values in `bench_ret_values` are not allowed and
result in an error.

- If all elements of `ret_values` are `NA`, `NA_real_` is returned.

- If any element of `bench_ret_values` is `NA` or infinite, an error is
  thrown.

- If fewer than two valid observations are available after NA handling,
  `NA_real_` is returned.

## See also

[`beta_bench`](https://pauloguimaraes871.github.io/factoRverse/reference/beta_bench.md),
[`res_mom`](https://pauloguimaraes871.github.io/factoRverse/reference/res_mom.md),
[`idio_vol`](https://pauloguimaraes871.github.io/factoRverse/reference/idio_vol.md)
