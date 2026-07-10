# Lookup method for tickers_catalog

Filters the catalog by provided tickers.

## Usage

``` r
lookup_catalog(
  tickers_catalog,
  tickers_to_lookup = NULL,
  perm_id_to_lookup = NULL
)

# S4 method for class 'tickers_catalog'
lookup_catalog(
  tickers_catalog,
  tickers_to_lookup = NULL,
  perm_id_to_lookup = NULL
)
```

## Arguments

- tickers_catalog:

  A tickers_catalog object.

- tickers_to_lookup:

  A character vector of tickers to filter.

- perm_id_to_lookup:

  A character vector of perm_ids to filter.

## Value

A filtered data.frame.
