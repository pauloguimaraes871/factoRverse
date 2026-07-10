# Calculate Beta Relative to a Benchmark

This function computes the *beta* of a return series relative to a
benchmark, defined as the slope coefficient in the linear regression:

## Usage

``` r
beta_bench(ret_values, bench_ret_values, na.rm = TRUE)
```

## Arguments

- ret_values:

  A numeric vector of asset returns.

- bench_ret_values:

  A numeric vector of benchmark returns aligned in time with
  `ret_values`.

- na.rm:

  Logical. If `TRUE`, removes observations where `ret_values` is `NA`
  and drops the corresponding benchmark observations. Default is `TRUE`.

## Value

A numeric scalar representing the estimated regression slope (beta).
Returns `NA_real_` if the regression cannot be estimated.

## Details

\$\$ r_t = \alpha + \beta b_t + \varepsilon_t \$\$

The estimated \\\beta\\ measures the systematic exposure of the asset
returns to movements in the benchmark.

The regression is estimated via ordinary least squares (OLS). Missing
values in `ret_values` may be removed depending on `na.rm`, while
missing or infinite values in `bench_ret_values` are not permitted.

- A beta close to zero indicates low systematic exposure to the
  benchmark.

- A beta greater than one indicates amplified exposure to benchmark
  movements.

- If the benchmark variance is zero or the regression is rank-deficient,
  `NA_real_` is returned.

## See also

[`alpha_bench`](https://pauloguimaraes871.github.io/factoRverse/reference/alpha_bench.md),
[`idio_vol`](https://pauloguimaraes871.github.io/factoRverse/reference/idio_vol.md)
