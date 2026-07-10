# Validate Liquidity Floor Cutoffs

Internal function to validate a liquidity_floor_cutoffs data frame.

## Usage

``` r
validate_liquidity_floor_cutoffs(
  liquidity_floor_cutoffs,
  main_liquidity_metric = NULL
)
```

## Arguments

- liquidity_floor_cutoffs:

  A data.frame containing liquidity floor cutoffs.

- main_liquidity_metric:

  Optional character string specifying the column (other than
  `"liquidity_classification"`) that should be used to check ascending
  order.

## Value

Invisibly returns `TRUE` if the data frame passes all validations.

## Details

This function checks that the liquidity_floor_cutoffs data frame meets
the following requirements:

- It is a data.frame with at least two columns.

- The first column is named `"liquidity_classification"`.

- It has at most 5 rows.

- The values in the `liquidity_classification` column are among
  `"micro_caps"`, `"small_caps"`, `"mid_caps"`, `"large_caps"`, or
  `"mega_caps"` and contain no duplicates.

- All other columns are numeric and contain no missing values.

- If `main_liquidity_metric` is provided, then it must be one of the
  non-classification column names, the data frame must be arranged in
  ascending order by that metric, and the ranking (order) of the values
  across all liquidity metric columns must be identical.

- The liquidity metric values must not be normalized; that is, it is an
  error if all values in each liquidity metric column (all but the
  first) are between -1 and 1.
