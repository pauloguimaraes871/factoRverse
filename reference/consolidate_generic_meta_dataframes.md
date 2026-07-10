# Consolidate Generic Meta Dataframes

Merges meta and base dataframes, handling cases where either dataframe
is NULL. If both are present, they are combined using a full join and
sorted by ID.

## Usage

``` r
consolidate_generic_meta_dataframes(
  main_generic_m_df,
  supplemental_generic_m_df,
  type,
  consolidate_name = TRUE,
  require_main = TRUE
)
```

## Arguments

- main_generic_m_df:

  A meta dataframe object.

- supplemental_generic_m_df:

  A base dataframe object.

- type:

  A character string indicating the type of data (e.g., "groups").

- consolidate_name:

  A logical indicating whether to consolidate the name of the meta
  dataframe.

- require_main:

  A logical indicating whether to return NULL if main_generic_m_df is
  NULL.

## Value

A consolidated dataframe combining main and new data or the appropriate
object depending on require_main
