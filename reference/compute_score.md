# Compute Score Based on Conditions

Computes a custom score by evaluating multiple user-defined conditions
on columns of a `meta_dataframe`. Each condition corresponds to a
variable and is applied row-wise. For each row, the number of satisfied
conditions is counted.

If the number of non-NA inputs for that row is below `min_non_na`, the
score is set to `NA`.

## Usage

``` r
compute_score(features_m_df, conditions, feature_name, ...)

# S4 method for class 'meta_dataframe,list,character'
compute_score(
  features_m_df,
  conditions,
  feature_name = "score",
  min_non_na = 0
)
```

## Arguments

- features_m_df:

  A `meta_dataframe` object.

- conditions:

  A named list of functions. Each name must correspond to a column in
  the data, and each function should return a logical vector indicating
  whether the condition is met.

- feature_name:

  A `character` string specifying the name of the new score column.

- ...:

  Additional arguments passed to the conditions.

- min_non_na:

  A numeric value indicating the minimum number of non-NA values
  required to compute the score for a given row. Defaults to `0`.

## Value

A `meta_dataframe` object with an additional column (named
`feature_name`) containing the number of conditions met for each row (or
`NA` if insufficient data).

## Details

This function is useful for constructing composite scores based on
multiple thresholds or logical criteria. Each condition is applied to
its corresponding column using
[`purrr::map2()`](https://purrr.tidyverse.org/reference/map2.html). The
total score per row reflects how many of the defined conditions are
satisfied.

## Examples

``` r
if (FALSE) { # \dontrun{
# Count how many quality criteria each stock meets
conditions <- list(roe_3m = function(x) x > 0.1, net_margin = function(x) x > 0.05)
features_m_df <- compute_score(features_m_df, conditions, feature_name = "quality_score")
} # }
```
