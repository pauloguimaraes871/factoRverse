# Compute Sector-Based Column Transformations

This function creates a new column in a `meta_dataframe` based on sector
classification and applies transformations specified in a mapping list
of formulas.

## Usage

``` r
compute_sector_map(meta_dataframe, sector_column, mapper, ...)

# S4 method for class 'meta_dataframe,character,list'
compute_sector_map(
  meta_dataframe,
  sector_column,
  mapper,
  feature_name = NULL,
  ...
)
```

## Arguments

- meta_dataframe:

  A `meta_dataframe` object containing the target data.

- sector_column:

  A `character` specifying the column in `meta_dataframe` that contains
  sector classifications.

- mapper:

  A named `list` in which names represent sector names and elements are
  formulas defining how to compute values (e.g., `~-Alpha`,
  `~ Beta + Alpha`).

- ...:

  Additional arguments passed to the formulas in `mapper`.

- feature_name:

  A `character` specifying the name of the new computed column.

## Value

A modified `meta_dataframe` with the new column containing computed
values.

## Details

The function:

- Maps each row in `meta_dataframe` to the corresponding transformation
  based on `sector_column`

- Evaluates the formulas in `mapper` within the context of the
  `meta_dataframe`

## Examples

``` r
if (FALSE) { # \dontrun{
# Apply a sector-specific transformation (e.g. a tax adjustment for banks)
mapper <- list(Banks = ~ sharpe * (1 - 0.34), Agro = ~ sharpe)
features_m_df <- compute_sector_map(features_m_df, sector_column = "sector",
                                    mapper = mapper, feature_name = "sharpe_adj")
} # }
```
