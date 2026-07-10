# Count elements that satisfy a condition

The `count_if` function counts the number of elements in `values` that
satisfy the condition specified in `count_condition_fun`.

## Usage

``` r
count_if(values, count_condition_fun, na.rm = TRUE)
```

## Arguments

- values:

  A numeric vector of values to evaluate.

- count_condition_fun:

  A function that takes a numeric vector and returns a logical vector.
  The function should return `TRUE` for elements that should be counted.

- na.rm:

  Logical. If `TRUE`, ignores `NA` values when counting. Default is
  `TRUE`.

## Value

An integer representing the count of values satisfying the condition.
