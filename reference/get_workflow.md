# Accessor for Workflow Slot

Retrieves the `workflow` slot from a `meta_dataframe` or `meta_xts`
object.

## Usage

``` r
get_workflow(object)

# S4 method for class 'meta_dataframe'
get_workflow(object)

# S4 method for class 'meta_xts'
get_workflow(object)
```

## Arguments

- object:

  An object of class `meta_dataframe` or `meta_xts`.

## Value

A named list representing the workflow steps applied to the object.
