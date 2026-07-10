# Calculate Correlation with a Benchmark

This function computes the Pearson correlation coefficient between a
return series and a benchmark return series over a given window.

## Usage

``` r
correlation_bench(ret_values, bench_ret_values, na.rm = TRUE)
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

A numeric scalar in the interval \\\[-1, 1\]\\ representing the Pearson
correlation coefficient. Returns `NA_real_` if the correlation cannot be
computed.

## Details

Unlike regression-based measures such as alpha or beta, correlation is a
symmetric statistic that captures the strength and direction of linear
co-movement between the two series, without attributing causality or
directional dependence.

- A value of `1` indicates perfect positive linear correlation.

- A value of `-1` indicates perfect negative linear correlation.

- A value of `0` indicates no linear correlation.

- Missing or infinite values in `bench_ret_values` result in an error.

## See also

[`alpha_bench`](https://pauloguimaraes871.github.io/factoRverse/reference/alpha_bench.md),
[`beta_bench`](https://pauloguimaraes871.github.io/factoRverse/reference/beta_bench.md)
