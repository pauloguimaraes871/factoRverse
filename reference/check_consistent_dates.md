# Check for Consistent Dates in a Vector (Allowing NULLs)

Checks whether all non-NULL date values in a given vector or list are
equal.

## Usage

``` r
check_consistent_dates(current_dates)
```

## Arguments

- current_dates:

  A list or vector of dates. Can include `NULL` or `NA` values.

## Value

Invisibly returns `TRUE` if all non-NULL, non-NA dates are identical;
otherwise, throws an error listing the differing dates.
