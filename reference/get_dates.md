# Accessor for Dates

Retrieves the unique dates from a `meta_dataframe` or `meta_xts` object.

## Usage

``` r
get_dates(object, ...)

# S4 method for class 'meta_dataframe'
get_dates(object)

# S4 method for class 'meta_xts'
get_dates(object)
```

## Arguments

- object:

  An object of class `meta_dataframe` or `meta_xts`.

- ...:

  Additional arguments (not used).

## Value

A sorted vector of dates.
