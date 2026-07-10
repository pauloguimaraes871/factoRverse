# Print method for tickers_catalog

Displays key information about a `tickers_catalog` object: its source
meta_dataframe name and current reference date, the total number of
tickers, the untraded / delisted / old / listed classifications, the
delisting tolerance (`n_days_tolerance`), and the first few rows of the
`catalog` slot.

## Usage

``` r
# S4 method for class 'tickers_catalog'
show(object)
```

## Arguments

- object:

  An instance of the `tickers_catalog` class.

## Value

Called for its side effect of printing a summary of the catalog.
