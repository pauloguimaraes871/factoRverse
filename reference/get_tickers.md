# Accessor for Tickers

Retrieves the unique tickers from a `meta_dataframe` or `meta_xts`
object.

## Usage

``` r
get_tickers(object)

# S4 method for class 'meta_dataframe'
get_tickers(object)

# S4 method for class 'meta_xts'
get_tickers(object)
```

## Arguments

- object:

  An object of class `meta_dataframe` or `meta_xts`.

## Value

A character vector of unique tickers (column names in the case of
`meta_xts`).
