# Prepare Method for step_winsorize

Estimates lower and upper bounds for winsorization based on quantiles
for each selected column.

## Usage

``` r
# S3 method for class 'step_winsorize'
prep(x, training, info = NULL, ...)
```

## Arguments

- x:

  A `step_winsorize` object.

- training:

  A data frame of training data used to estimate winsorization
  thresholds.

- info:

  An optional `term_info` object (not used here).

- ...:

  Additional arguments passed to methods (currently not used).

## Value

An updated `step_winsorize` object with estimated bounds stored in
`winsor_limits` and `trained = TRUE`.
