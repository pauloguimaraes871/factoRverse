# Accessor for Data Slot

Retrieves the `data` slot from a `meta_dataframe` or `meta_xts` object.

## Usage

``` r
get_data(object)

# S4 method for class 'meta_dataframe'
get_data(object)

# S4 method for class 'meta_xts'
get_data(object)
```

## Arguments

- object:

  An object of class `meta_dataframe` or `meta_xts`.

## Value

A `data.frame` (for `meta_dataframe`) or `xts` object (for `meta_xts`)
containing the data.
