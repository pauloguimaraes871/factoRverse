# Compute Sector-Based Mapped Values Across a Meta XTS

This function assigns values from a `meta_xts` object to a new column in
a `meta_dataframe` based on a sector mapping rule. The mapping
determines which column from `meta_xts` should be used based on the
sector classification in `meta_dataframe`, and an optional
transformation is applied.

## Usage

``` r
compute_sector_map_across(meta_dataframe, meta_xts, sector_column, mapper, ...)

# S4 method for class 'meta_dataframe,meta_xts,character,list'
compute_sector_map_across(
  meta_dataframe,
  meta_xts,
  sector_column,
  mapper,
  feature_name = NULL,
  ...
)
```

## Arguments

- meta_dataframe:

  A `meta_dataframe` object containing the target data.

- meta_xts:

  A `meta_xts` object containing the reference values.

- sector_column:

  A `character` specifying the column in `meta_dataframe` that contains
  sector classifications.

- mapper:

  A named `list` in which names represent sector names and elements are
  formulas defining how to compute values (e.g., `~-A`, `~A + B`).

- ...:

  Additional arguments passed to the formulas in `mapper`.

- feature_name:

  A `character` specifying the name of the new computed column. Default
  is `<sector_column>_sector_value`.

## Value

A modified `meta_dataframe` with the new column containing computed
values.

## Details

The function:

- Maps each row in `meta_dataframe` to the corresponding column in
  `meta_xts` using `mapper`

- Extracts values from `meta_xts` based on `dates`

- Applies optional transformations per sector

## Examples

``` r
if (FALSE) { # \dontrun{
# For each stock, pull a sector-specific column from the metrics xts and transform it
mapper <- list(Agro = ~ -A, Utilities = ~ B)
meta_df <- compute_sector_map_across(meta_df, metrics_xts,
                                     sector_column = "sector", mapper = mapper)
} # }
```
