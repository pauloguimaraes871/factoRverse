# Method for screening a meta_dataframe based on conditions

Method for screening a meta_dataframe based on conditions

## Usage

``` r
# S4 method for class 'meta_dataframe'
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

A new `meta_dataframe` object with screened data.
