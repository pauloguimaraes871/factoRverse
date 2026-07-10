# Compute Across: Apply a Calculation Between meta_dataframe and meta_xts

Applies a predefined mathematical operation between a signal column in a
`meta_dataframe` and a metric column in a `meta_xts`. The operation is
performed for each row in the `meta_dataframe` that matches the same
date in the `meta_xts`.

## Usage

``` r
compute_across(
  meta_dataframe,
  meta_xts,
  FUN,
  feature_name = NULL,
  signal = NULL,
  metric,
  ...
)

# S4 method for class 'meta_dataframe,meta_xts,character'
compute_across(
  meta_dataframe,
  meta_xts,
  FUN,
  feature_name = NULL,
  signal = NULL,
  metric,
  ...
)
```

## Arguments

- meta_dataframe:

  A `meta_dataframe` object containing financial or factor data.

- meta_xts:

  A `meta_xts` object containing time series data (e.g., market
  metrics).

- FUN:

  A `character` string indicating the operation to apply. Must be one
  of: `"product"`, `"ratio"`, `"subtract"`, `"sum"`, `"just_append"`.

- feature_name:

  Optional `character` string giving the name for the new feature
  column. If `NULL`, a default name is constructed.

- signal:

  A `character` string specifying the column name in `meta_dataframe` to
  be used in the calculation (required unless `FUN = "just_append"`).

- metric:

  A `character` string specifying the column name in `meta_xts` to be
  used.

- ...:

  Additional arguments (not used currently).

## Value

A modified `meta_dataframe` object with a new computed column.

## Details

The function checks consistency of column names and dates, performs the
operation row-wise for matching dates, and appends the result to the
`meta_dataframe`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Multiply a signal by a same-date macro metric (e.g. inflation) drawn from a meta_xts
meta_df <- compute_across(meta_df, metrics_xts, FUN = "product", signal = "ir_3m", metric = "ipca")
# Simply append a macro time series as a per-date column
meta_df <- compute_across(meta_df, metrics_xts, FUN = "just_append", metric = "ipca")
} # }
```
