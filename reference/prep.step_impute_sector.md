# Prepare Method for step_impute_sector

Computes sector-wise means or medians for selected columns, storing them
in the step object.

## Usage

``` r
# S3 method for class 'step_impute_sector'
prep(x, training, info = NULL, ...)
```

## Arguments

- x:

  A `step_impute_sector` object.

- training:

  A data frame of training data used to compute imputation values.

- info:

  A `term_info` object passed by `prep()`, not used here.

- ...:

  Additional arguments passed to methods (not used).

## Value

An updated `step_impute_sector` object with `trained = TRUE` and
imputation values stored.
