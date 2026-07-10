# Retrieve Processed Feature Positions

Gathers chosen signals and positions from a set of base backtest
objects, verifies consistency, and filters features according to
`features_passthrough`. Positions labeled `"force"` are mapped to
`"long"` in the final output.

## Usage

``` r
get_features_positions(
  base_sb_backtest_results_list,
  features_passthrough,
  features_m_df
)
```

## Arguments

- base_sb_backtest_results_list:

  A list of base backtest result objects, each of which may contain
  chosen signals and positions. Passed to the internal function
  [`get_and_check_chosen_signals_and_positions`](https://pauloguimaraes871.github.io/factoRverse/reference/get_and_check_chosen_signals_and_positions.md)
  to retrieve a reference set of signals/positions.

- features_passthrough:

  A character vector dictating which features are retained in the final
  output (subsetting the reference set). If `"all"`, all features
  remain; if `"none"`, returns `"none"`; otherwise, must be a subset of
  features found in the reference set.

- features_m_df:

  A meta-data object (e.g., a `meta_dataframe`) containing at least
  three key columns (often `date`, `symbol`, `target`) plus feature
  columns. Used primarily for consistency checks and reconstructing any
  missing signals/positions in `base_sb_backtest_results_list`.

## Value

A named character vector of positions (e.g., `"long"` or `"short"`),
indexed by feature name. If `features_passthrough = "none"`, returns the
string `"none"` directly.

## Details

1.  **Reference Signals**: The function first calls
    [`get_and_check_chosen_signals_and_positions`](https://pauloguimaraes871.github.io/factoRverse/reference/get_and_check_chosen_signals_and_positions.md)
    to ensure all base backtest elements share identical named
    positions. Missing elements are reconstructed as all-"long".

2.  **Feature Filtering**: Depending on `features_passthrough`, the
    reference set is either left intact (`"all"`), discarded (`"none"`),
    or subset to the specified feature names.

3.  **Force Handling**: Any positions labeled `"force"` are treated as
    `"long"` in the final output.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Example backtest results list
  base_list <- list(...)

  # meta_dataframe with columns date, symbol, target, and feature columns
  features_mdf <- create_meta_dataframe(...)

  # Retain all features
  get_features_positions(
    base_sb_backtest_results_list = base_list,
    features_passthrough = "all",
    features_m_df = features_mdf
  )

  # Retain only a subset of features
  get_features_positions(
    base_sb_backtest_results_list = base_list,
    features_passthrough = c("Feature1", "Feature2"),
    features_m_df = features_mdf
  )
} # }
```
