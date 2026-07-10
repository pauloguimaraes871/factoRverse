# Update meta_dataframe by appending a batch

Append a new batch (usually one month of observations) to an existing
`meta_dataframe`, validating compatibility and consolidating metadata.
Intended for incremental ingestion of feature tables produced monthly
(or daily) while preserving workflow provenance.

## Usage

``` r
update_meta_dataframe(old_features_m_df, new_features_m_df, ...)

# S4 method for class 'meta_dataframe,meta_dataframe'
update_meta_dataframe(
  old_features_m_df,
  new_features_m_df,
  batch_type = "monthly"
)
```

## Arguments

- old_features_m_df:

  A `meta_dataframe` containing the existing (historical) panel.

- new_features_m_df:

  A `meta_dataframe` containing the new batch to append. Typically the
  new object contains a single date (monthly batch) and the same set of
  columns as `old_features_m_df`.

- ...:

  Additional arguments (reserved / forwarded).

- batch_type:

  Character. Batch cadence: `"monthly"` (default) or `"daily"`. Controls
  validation of the number of unique dates in `new_features_m_df`.

## Value

A `meta_dataframe` with rows from `new_features_m_df` appended to
`old_features_m_df`, metadata consolidated via
[`consolidate_generic_meta_dataframes()`](https://pauloguimaraes871.github.io/factoRverse/reference/consolidate_generic_meta_dataframes.md),
and the `workflow` slot updated with a new `update_<date>` entry
describing the batch.

## Details

Update a meta_dataframe with a new batch of data

The method performs strict compatibility checks before merging (tests
assert these behaviours):

- Column names and classes must match exactly between old and new
  objects.

- There must be no overlapping `id` values or overlapping `dates`.

- For `batch_type == "monthly"` the new batch must contain exactly one
  unique date.

- For `batch_type == "daily"` the number of unique dates is validated to
  be in a reasonable range.

- `new_features_m_df@current_date` must equal one month after
  `old_features_m_df@current_date`.

- Neither object may be of class `raw_features_m_df`.

- Both objects must contain a `read_tickers_catalog` entry in their
  `workflow` (ensures consistent ticker mapping).

On success, the function calls
`consolidate_generic_meta_dataframes(..., type = "generic")` to merge
the tables and then appends a batch entry to the `workflow` slot
describing the new date, batch name, timestamp and the batch's own
workflow.

## Functions

- `update_meta_dataframe( old_features_m_df = meta_dataframe, new_features_m_df = meta_dataframe )`:
  Method implementation for meta_dataframe inputs

  Method signature: `old_features_m_df = "meta_dataframe"`,
  `new_features_m_df = "meta_dataframe"`.

## Examples

``` r
if (FALSE) { # \dontrun{
# old_mdf and new_mdf are meta_dataframe objects
# (new_mdf typically contains a single new monthly date)
updated <- update_meta_dataframe(old_mdf, new_mdf, batch_type = "monthly")
} # }
```
