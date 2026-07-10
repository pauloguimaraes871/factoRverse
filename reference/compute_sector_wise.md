# Compute Sector-Wise Calculation for a Given Signal in a meta_dataframe

This function computes a calculation for each observation in a
`meta_dataframe` object by applying a predefined function (specified by
a character) to all observations within the same sector on the same
date.

## Usage

``` r
compute_sector_wise(
  features_m_df,
  sector_column,
  signal,
  FUN,
  na.rm = TRUE,
  feature_name = NULL,
  min_non_na = 0
)

# S4 method for class 'meta_dataframe,character,character,character'
compute_sector_wise(
  features_m_df,
  sector_column,
  signal,
  FUN,
  na.rm = TRUE,
  feature_name = NULL,
  min_non_na = 0
)
```

## Arguments

- features_m_df:

  A `meta_dataframe` object.

- sector_column:

  A `character` specifying the column name representing sector
  classification in the dataset.

- signal:

  A `character` specifying the column name on which the function is
  computed.

- FUN:

  A `character` specifying the function to apply. Options are "median",
  "mean", "sd", "signal_to_noise".

- na.rm:

  A `logical` indicating whether to remove NA values (default TRUE).

- feature_name:

  A `character` specifying the name of the feature to be added to the
  meta_dataframe. If NULL, the feature name will be set to "*sector*".
  Default is NULL.

- min_non_na:

  A `numeric` value specifying the minimum number of non-NA values
  required to compute the metric. Default is 0.

## Value

A `meta_dataframe` object with an added column named
`<signal>_sector_<FUN>` in its `data` slot containing the computed
values.

## Details

For each row (current observation), the function groups observations by
`sector_column` and `dates`, then applies the specified function to the
`signal` column. If no matching observation is found, the resulting
value is `NA`. The available functions are:

- **median**: `stats::median(x, na.rm = na.rm)`

- **mean**: `stats::mean(x, na.rm = na.rm)`

- **sd**: `stats::sd(x, na.rm = na.rm)`

- **signal_to_noise**:
  `stats::mean(x, na.rm = na.rm) / stats::sd(x, na.rm = na.rm)`
