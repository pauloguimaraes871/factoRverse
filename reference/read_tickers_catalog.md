# Apply tickers_catalog Transformations

Applies ticker ID mapping and filtering based on a `tickers_catalog`
object. Can handle either a `raw_features_m_df` or a `returns_meta_xts`
object. This ensures consistency in ticker identifiers and filters out
data outside trading ranges or for untraded tickers.

## Usage

``` r
read_tickers_catalog(data, tickers_catalog, ...)

# S4 method for class 'raw_features_m_df,tickers_catalog'
read_tickers_catalog(data, tickers_catalog, verbose = TRUE)

# S4 method for class 'returns_meta_xts,tickers_catalog'
read_tickers_catalog(data, tickers_catalog, verbose = TRUE)
```

## Arguments

- data:

  An object of class `raw_features_m_df` or `returns_meta_xts`.

- tickers_catalog:

  A `tickers_catalog` object containing reference data including ticker
  status, trading range, and permanent IDs.

- ...:

  Additional arguments passed to methods. Supports
  `verbose = TRUE/FALSE`.

- verbose:

  A logical indicating whether to print progress messages. Defaults to
  `TRUE`.

## Value

An object of the same class as `data`, modified according to the catalog
rules.

## Details

The catalog's stable `perm_id` values replace the (possibly changing)
`tickers` labels, so a company keeps a single identity across ticker
renames. Each series is then restricted to its valid trading window:
observations before `tickers_first_quote` or after `tickers_last_quote`
(plus the catalog's `n_days_tolerance`) are set to NA or dropped, and
tickers classified as `untraded` are removed. This yields a clean,
survivorship-aware panel ready for the silver/gold preprocessing stages.

## See also

[`create_tickers_catalog`](https://pauloguimaraes871.github.io/factoRverse/reference/create_tickers_catalog.md),
[`update_tickers_catalog`](https://pauloguimaraes871.github.io/factoRverse/reference/update_tickers_catalog.md),
[`tickers_catalog-class`](https://pauloguimaraes871.github.io/factoRverse/reference/tickers_catalog-class.md)
