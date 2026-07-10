# Coerce a meta_dataframe Object to Data Frame

This method extracts the `data` slot from a `meta_dataframe` object and
returns it as a standard `data.frame`.

## Usage

``` r
# S4 method for class 'meta_dataframe'
as.data.frame(x)
```

## Arguments

- x:

  An object of class `meta_dataframe`.

## Value

A `data.frame` containing the contents of the `data` slot.
