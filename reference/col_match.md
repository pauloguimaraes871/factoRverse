# Robust column matcher tolerant to R name-mangling

Return the subset of `colnames(obj)` that correspond to `want`, treating
`"Stock C"` and `"Stock.C"` as equivalent. The result preserves the
order of `want` and drops non-matches. Column names in `obj` are **not**
modified.

## Usage

``` r
col_match(obj, want, ignore_case = FALSE)
```

## Arguments

- obj:

  A matrix-, data.frame-, or xts-like object with
  [`base::colnames()`](https://rdrr.io/r/base/colnames.html).

- want:

  Character vector of desired column labels (may be non-syntactic).

- ignore_case:

  Logical; if `TRUE`, matching is case-insensitive. Default `FALSE`.

## Value

Character vector of column names from `obj` that match `want`, in the
same order as `want`. Non-matches are omitted.

## Details

Matching is performed on `base::make.names(x, unique = FALSE)`. This
mirrors how R sanitizes names (spaces/dashes → dots; leading digits
prefixed with `X`). If multiple columns in `obj` normalize to the same
token, the first occurrence is selected (per
[`base::match()`](https://rdrr.io/r/base/match.html) semantics).

## See also

base::make.names, base::match
