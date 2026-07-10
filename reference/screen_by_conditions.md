# Screen (filter) a meta_dataframe by row-wise conditions

Filters the rows of a `meta_dataframe` using one or more logical
conditions, which are passed straight to
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html).
Object metadata is preserved and a screening step is appended to the
`workflow` log. Errors if the conditions filter out every row.

## Usage

``` r
screen_by_conditions(meta_dataframe, ...)
```

## Arguments

- meta_dataframe:

  A `meta_dataframe` object.

- ...:

  One or more logical conditions passed to
  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
  (e.g. `value > 15`, `tickers != "C"`).

## Value

A new `meta_dataframe` containing only the rows that satisfy the
conditions.

## Examples

``` r
if (FALSE) { # \dontrun{
# Keep liquid, non-financial names
screen_by_conditions(features_m_df, mean_volfin_3m > 1e6, sector != "Financials")
} # }
```
