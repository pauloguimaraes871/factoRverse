# Summary Method for tickers_catalog Class

Provides a detailed summary of a `tickers_catalog` object, allowing
users to view key statistics and filter the catalog slot interactively.

## Usage

``` r
# S4 method for class 'tickers_catalog'
summary(object, summary_id = NULL)
```

## Arguments

- object:

  An instance of the `tickers_catalog` class.

- summary_id:

  A character string or numeric value specifying which summary to
  display.

  - By name: Options are "Catalog Overview" or "Yearly Summary".

  - By number: Provide a number corresponding to the summary (1 or 2).
    If `NULL` (default), the method lists available summaries and
    prompts for a selection.

## Value

Invisibly returns the input `object`.
