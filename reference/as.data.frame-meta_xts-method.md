# Coerce a meta_xts Object to Data Frame

This method extracts the `data` slot from a `meta_xts` object and
returns it as a standard `data.frame`.

## Usage

``` r
# S4 method for class 'meta_xts'
as.data.frame(x)
```

## Arguments

- x:

  An object of class `meta_xts`.

## Value

A `data.frame` containing the contents of the `data` slot.
