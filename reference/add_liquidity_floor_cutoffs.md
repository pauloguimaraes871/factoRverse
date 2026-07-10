# Add Liquidity Floor Cutoffs

Add or update liquidity floor cutoff values in an existing object.

## Usage

``` r
add_liquidity_floor_cutoffs(object, metric_name, metric_cutoffs)
```

## Arguments

- object:

  An object of class `port_backtest_config` or `sb_backtest_config`. The
  object must have a slot `liquidity_floor_cutoffs` (which may be NULL)
  and a character slot `main_liquidity_metric`.

- metric_name:

  A character vector (or a single character) specifying the metric(s) to
  add or update.

- metric_cutoffs:

  For each metric, a named numeric vector containing cutoff values. For
  a single metric, this can be a named numeric vector.

## Value

The updated `object` with its `liquidity_floor_cutoffs` slot merged with
the new values.

## Details

If the object already has a liquidity_floor_cutoffs data.frame, the
function:

- Ensures that rows exist for all allowed liquidity classifications.

- For each provided metric, if the metric column already exists, it
  updates the rows corresponding to the names provided in
  `metric_cutoffs`; otherwise a new column is added.

- After merging, the resulting data.frame is validated against the
  enforced structure: it must be a data.frame, contain the main
  liquidity metric, be sorted in ascending order, have consistent orders
  across metrics, contain only allowed liquidity classifications, have
  numeric columns (except for the classification column) and contain no
  NAs.
