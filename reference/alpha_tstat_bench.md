# Compute the t-Statistic of Alpha Relative to a Benchmark

This function calculates the t-statistic associated with the intercept
(\\\alpha\\) in a linear regression of asset returns on benchmark
returns. The model estimated is:

## Usage

``` r
alpha_tstat_bench(
  ret_values,
  bench_ret_values,
  mult_last_n = 0,
  mult_by = -1,
  na.rm = TRUE
)
```

## Arguments

- ret_values:

  Numeric vector of asset returns.

- bench_ret_values:

  Numeric vector of benchmark returns aligned with `ret_values`.

- mult_last_n:

  Integer \>= 0. If \> 0, multiply the last n (in time order) of
  `ret_values` by 'mult_by' before fitting the regression. Default is 0.

- mult_by:

  Numeric scalar. The number to multiply the last n values by. Default
  is -1 (inversion).

- na.rm:

  Logical. If `TRUE`, removes rows with NA in `ret_values` and applies
  the same mask to the benchmark. Default is `TRUE`. Missing or infinite
  values in the benchmark are not allowed and produce an error.

## Value

A numeric scalar equal to the t-statistic of the regression intercept.
Returns `NA_real_` if:

- the regression is not estimable (e.g., insufficient observations),

- the benchmark has zero variance,

- the standard error of alpha is zero or undefined,

- the rolling window contains fewer than three usable observations.

## Details

\$\$ r_t = \alpha + \beta b_t + \varepsilon_t, \$\$

where \\r_t\\ denotes the asset returns and \\b_t\\ denotes the
benchmark returns over a given rolling window. The regression is
estimated using the internal helper
[`bench_regression_fit()`](https://pauloguimaraes871.github.io/factoRverse/reference/bench_regression_fit.md),
which is a lightweight wrapper around
[`lm.fit()`](https://rdrr.io/r/stats/lmfit.html) designed for
high-volume, rolling, and point-in-time computations.

The t-statistic is computed as:

\$\$ t\_{\alpha} = \frac{\alpha}{SE(\alpha)}, \$\$

where the standard error of the intercept is given by the OLS formula:

\$\$ SE(\alpha) = \sqrt{ \sigma^2 \left( \frac{1}{n} +
\frac{\bar{b}^2}{\sum (b_t - \bar{b})^2} \right) }. \$\$

This function is designed for use inside rolling computations and has no
side effects: all output is numeric and unnamed. It is robust to missing
values in the return series and implements strict point-in-time
validation. The OLS formulas are computed manually for performance
reasons and to ensure that the function behaves predictably under
minimal rolling sample sizes.

## See also

[`bench_regression_fit`](https://pauloguimaraes871.github.io/factoRverse/reference/bench_regression_fit.md),
[`alpha_bench`](https://pauloguimaraes871.github.io/factoRverse/reference/alpha_bench.md),
[`beta_bench`](https://pauloguimaraes871.github.io/factoRverse/reference/beta_bench.md),
[`correlation_bench`](https://pauloguimaraes871.github.io/factoRverse/reference/correlation_bench.md)
