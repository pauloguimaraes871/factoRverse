# Calculate Compound Annual Growth Rate (CAGR)

This function computes the Compound Annual Growth Rate (CAGR) given an
initial value, a final value, and the number of periods. It handles
various scenarios, including cases with negative values or missing data.

## Usage

``` r
cagr(begin, final, period)
```

## Arguments

- begin:

  A numeric value representing the initial value.

- final:

  A numeric value representing the final value.

- period:

  A numeric value representing the number of periods over which the
  growth is calculated.

## Value

A numeric value representing the CAGR. If either `begin` or `final` is
`NA`, or if the period is less than or equal to zero, appropriate errors
or `NA` are returned.

## Examples

``` r
cagr(100, 200, 5)
#> [1] 0.1486984
```
