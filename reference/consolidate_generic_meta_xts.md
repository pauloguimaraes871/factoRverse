# Consolidate Generic Meta XTS

Merges meta and base XTS objects, handling cases where either object is
NULL. If both are present, they are combined using bind_rows.

## Usage

``` r
consolidate_generic_meta_xts(
  main_generic_m_xts,
  supplemental_generic_m_xts,
  type = "returns",
  consolidate_name = TRUE,
  require_main = TRUE,
  operation = "merge"
)
```

## Arguments

- main_generic_m_xts:

  An XTS object representing the most important object.

- supplemental_generic_m_xts:

  An XTS object representing supplementary.

- type:

  A character string indicating the type of data (e.g., "groups").

- consolidate_name:

  A logical indicating whether to consolidate the name of the meta
  dataframe.

- require_main:

  A logical indicating whether to return NULL if main_generic_m_df is
  NULL.

- operation:

  A character indicating whether to 'merge' or 'bind_rows' the data.

## Value

A consolidated xts combining main and new data or the appropriate object
depending on require_main
