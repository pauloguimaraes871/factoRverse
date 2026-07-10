# Method for selecting features from a meta_dataframe

Method for selecting features from a meta_dataframe

## Usage

``` r
# S4 method for class 'meta_dataframe'
screen_by_feature(meta_dataframe, ...)
```

## Arguments

- meta_dataframe:

  A `meta_dataframe` object.

- ...:

  One or more column names or tidyselect helpers (e.g.
  `dplyr::starts_with("mom")`) passed to
  [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)
  to choose feature columns.

## Value

A new `meta_dataframe` object with selected features (always including
id, tickers, dates).
