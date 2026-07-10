# Create Liquidity Floor Cutoffs

Construct a liquidity_floor_cutoffs data frame from scratch.

## Usage

``` r
create_liquidity_floor_cutoffs(metric_name, metric_cutoffs)
```

## Arguments

- metric_name:

  A character vector of metric names.

- metric_cutoffs:

  A list of named numeric vectors, one for each metric in metric_name.
  Each vector must have names exactly equal to c("micro_caps",
  "small_caps", "mid_caps", "large_caps", "mega_caps") (order may vary).

## Value

A data.frame with a column `liquidity_classification` and one column per
metric. The rows are ordered (after reordering each metric vector
according to the allowed levels) so that the main liquidity metric
(assumed to be the first element of metric_name) is in non-decreasing
order.

## Details

The function enforces that:

- The returned object is a data.frame.

- The column names (besides `liquidity_classification`) are numeric with
  no NAs.

- The `liquidity_classification` values are exactly the allowed levels:
  "micro_caps", "small_caps", "mid_caps", "large_caps", "mega_caps".

- The main liquidity metric (the first metric in metric_name) is in
  ascending order.

- The ordering (ranking) of liquidity classifications is consistent
  across metrics.
