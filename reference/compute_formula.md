# Compute Formula-Based Signal Calculation

Applies a user-defined arithmetic formula to variables in a
`meta_dataframe` object. The function computes the formula for each row
(ticker-date pair), without any rolling or seasonal windowing (unlike
`compute_window`).

## Usage

``` r
compute_formula(features_m_df, formula, ignore_NA = NULL)

# S4 method for class 'meta_dataframe,formula'
compute_formula(features_m_df, formula, ignore_NA = NULL)
```

## Arguments

- features_m_df:

  A `meta_dataframe` object.

- formula:

  A `formula` specifying the transformation to apply. The formula should
  use column names from `features_m_df` and may include arithmetic
  operations such as `+`, `-`, `*`, `/`, and common functions like
  [`log()`](https://rdrr.io/r/base/Log.html),
  [`exp()`](https://rdrr.io/r/base/Log.html),
  [`sqrt()`](https://rdrr.io/r/base/MathFun.html). The left-hand side of
  the formula defines the new feature name. For example:
  `log_mktcap ~ log(market_cap)`, `pe_ratio ~ price / earnings`.

- ignore_NA:

  A `character` vector specifying which variables (among those in the
  formula) should be imputed in case of missing values. If the formula
  uses `+` or `-`, NAs are replaced with 0. If the formula uses `*` or
  `/`, NAs are replaced with 1.

## Value

A modified `meta_dataframe` object with a new column computed based on
the formula.

## Details

If `ignore_NA` is specified, the computation avoids propagation of
missing values by replacing them according to the arithmetic structure.
Mixing addition/subtraction with multiplication/division in the same
formula is not allowed when `ignore_NA` is used. The function appends
the result to the `meta_dataframe`, preserving its workflow log.

## Examples

``` r
if (FALSE) { # \dontrun{
# Book yield from two columns, then a log transform
features_m_df <- compute_formula(features_m_df, book_yield ~ book_value / market_cap)
features_m_df <- compute_formula(features_m_df, log_mktcap ~ log(market_cap))
# NA-safe additive combination: treat a missing Beta as 0
features_m_df <- compute_formula(features_m_df, combo ~ Alpha + Beta, ignore_NA = "Beta")
} # }
```
