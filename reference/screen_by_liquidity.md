# Generic function for screening a meta_dataframe based on liquidity classification, given a liquidity_floor_rule.

This function filters a `meta_dataframe` using
[`classify_stock_liquidity()`](https://pauloguimaraes871.github.io/factoRverse/reference/classify_stock_liquidity.md)
to remove illiquid stocks.

## Usage

``` r
screen_by_liquidity(
  meta_dataframe,
  liquidity_m_df,
  liquidity_floor_cutoffs,
  liquidity_floor_rule,
  ...
)

# S4 method for class 'meta_dataframe,meta_dataframe,data.frame,character'
screen_by_liquidity(
  meta_dataframe,
  liquidity_m_df,
  liquidity_floor_cutoffs,
  liquidity_floor_rule,
  verbose = TRUE
)
```

## Arguments

- meta_dataframe:

  A `meta_dataframe` object.

- liquidity_m_df:

  A `liquidity_m_df` meta_dataframe containing one or more market
  liquidity measures (e.g., inflation-adjusted mean financial volume).
  All ids in meta_dataframe must have a unique correspondence to this
  object.

- liquidity_floor_cutoffs:

  A data.frame containing cutoff values for liquidity metrics specified
  in `liquidity_m_df`. The names should match the metrics and values
  should be the minimum acceptable values (adjust for inflation) Stocks
  that have all metrics higher than defined in a
  `liquidity_floor_cutoffs` element will receive a liquidity
  classification at least equal to it. Elements should be: "micro_caps",
  "small_caps", "mid_caps", "large_caps" and "mega_caps" Classification
  should be in ascending order (from lest liquid to most liquid) for all
  metrics. If set in decimals, values will be interpreted as quantiles
  and classification will be set according to quantiles The first column
  is named "liquidity_classification" It has at most 5 rows and no
  duplicates or NAs.

- liquidity_floor_rule:

  Optional. Character string specifying the liquidity classification to
  apply the liquidity floor rule (eg. "nano_caps", "micro_caps",
  "small_caps", "mid_caps", "large_caps", "mega_caps").

- ...:

  Additional arguments to be passed to the method.

- verbose:

  Logical. If TRUE, prints additional information during processing.

## Value

A new `meta_dataframe` containing only the ids whose liquidity
classification meets or exceeds `liquidity_floor_rule` (via
[`classify_stock_liquidity()`](https://pauloguimaraes871.github.io/factoRverse/reference/classify_stock_liquidity.md)),
with a screening step appended to the `workflow` log. Errors if every
stock is filtered out.

## Examples

``` r
if (FALSE) { # \dontrun{
# Drop everything less liquid than small caps
screen_by_liquidity(features_m_df, liquidity_m_df, liquidity_floor_cutoffs,
                    liquidity_floor_rule = "small_caps")
} # }
```
