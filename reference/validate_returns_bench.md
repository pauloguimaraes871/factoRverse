# Validator function for returns and benchmark returns

Validator function for returns and benchmark returns

## Usage

``` r
validate_returns_bench(ret_values, bench_ret_values)
```

## Arguments

- ret_values:

  Numeric vector (signal returns).

- bench_ret_values:

  Numeric vector (benchmark returns).

## Value

NA_real\_ if ret_values are all NAs or contain Inf/-Inf, else throws
error if bench_ret_values contain NA/Inf or lengths differ.
