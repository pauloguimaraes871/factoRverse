# Calculate Skewness of a Numeric Vector

This function computes the skewness (the third standardized moment) of a
numeric vector. It handles missing values according to the `na.rm`
parameter and returns `NA` if the standard deviation is zero.

## Usage

``` r
skew(values, na.rm = TRUE)
```

## Arguments

- values:

  A numeric vector whose skewness is to be calculated.

- na.rm:

  A logical value indicating whether NA values should be removed before
  computation (default is TRUE).

## Value

A numeric value representing the skewness of the input vector. If the
standard deviation is zero, `NA_real_` is returned.

## Examples

``` r
skew(c(1, 2, 3, 4, 5))
#> [1] 0
```
