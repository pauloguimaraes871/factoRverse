# Screen (select) features from a meta_dataframe

Selects columns (features) from a `meta_dataframe`, always retaining the
`id`, `tickers` and `dates` identifier columns regardless of the
selection. This is a thin, workflow-aware wrapper around
[`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html):
the requested columns are chosen, the three identifier columns are
guaranteed to be present and ordered first, and a screening step is
appended to the object's `workflow` log.

## Usage

``` r
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

A new `meta_dataframe` with the selected features (plus `id`, `tickers`,
`dates`). Errors if the selection would leave only the identifier
columns.

## Examples

``` r
if (FALSE) { # \dontrun{
# Keep only momentum features (id/tickers/dates are always retained)
screen_by_feature(features_m_df, dplyr::starts_with("mom"))
} # }
```
