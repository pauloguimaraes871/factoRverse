# Tidy Method for step_impute_sector

Generates a tidy summary of the `step_impute_sector` object, showing
which columns were imputed, the method used, and the imputed values (if
trained).

## Usage

``` r
# S3 method for class 'step_impute_sector'
tidy(x, ...)
```

## Arguments

- x:

  A `step_impute_sector` object.

- ...:

  Not used. Included for method compatibility.

## Value

A tibble with columns:

- `column`: The names of the variables imputed.

- `sector`: The sector used for group-wise imputation (only if not
  trained).

- `method`: Imputation method ("mean", "median", etc.).

- `imputed_value`: Imputed values for each sector-variable pair (if
  trained).

- `id`: ID of the step.
