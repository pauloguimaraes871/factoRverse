# Consolidate Out-of-Sample Signal-Blending Outputs into a Meta Dataframe

This function consolidates out-of-sample (OOS) predictions from a list
of `sb_backtest_results` objects into a single meta dataframe. It
supports optional winsorization and normalization of predictions, as
well as passthrough of specific features to the output dataframe.

## Usage

``` r
consolidate_oos_sb_outputs_m_df(
  base_sb_backtest_outputs_list,
  winsorize_predictions = TRUE,
  normalize_predictions = TRUE,
  winsorization_probs = c(0.025, 0.975),
  features_passthrough_and_positions = "none",
  features_m_df = NULL,
  parallel = TRUE,
  verbose = TRUE
)
```

## Arguments

- base_sb_backtest_outputs_list:

  A list of `sb_backtest_results` objects, each containing OOS
  predictions and associated metadata.

- winsorize_predictions:

  Logical; if `TRUE`, performs winsorization on predictions to mitigate
  the effect of outliers. Default is `TRUE`.

- normalize_predictions:

  Logical; if `TRUE`, normalizes predictions to ensure comparability
  across models. Default is `TRUE`.

- winsorization_probs:

  A numeric vector of length 2 specifying the lower and upper quantile
  probabilities for winsorization. Default is `c(0.025, 0.975)`.

- features_passthrough_and_positions:

  Character; specifies which features from `features_m_df` to include in
  the output. Options are `"none"`, `"all"`, or specific feature names.
  Default is `"none"`.

- features_m_df:

  A meta dataframe of features to include in the passthrough. Only
  required if `features_passthrough` is not `"none"`. Default is `NULL`.

- parallel:

  Logical; if `TRUE`, parallel processing is used to speed up the
  consolidation process. Default is `TRUE`.

- verbose:

  Logical; if `TRUE`, progress messages are printed. Default is `TRUE`.

## Value

A `meta_dataframe` object containing the consolidated predictions,
optionally with normalized and winsorized values, and passthrough
features.

## Details

The function performs a series of validation checks to ensure the
consistency of input data:

- All elements in `base_sb_backtest_outputs_list` must have the same
  `oos_predictions_m_df` structure, with matching `id` values across all
  elements.

- The list must have unique, non-empty names to be used as column
  identifiers in the consolidated dataframe.

- If `features_passthrough` is not `"none"`, a `features_m_df` must be
  provided.

After performing these validations, the function consolidates
predictions into a single meta dataframe. Optional winsorization and
normalization can be applied to the predictions. Additionally, the
specified features can be passed through to the output dataframe.

## See also

[sb_backtest_results](https://pauloguimaraes871.github.io/factoRverse/reference/sb_backtest_results-class.md)
for the definition of `sb_backtest_results` objects.
[`create_meta_dataframe()`](https://pauloguimaraes871.github.io/factoRverse/reference/create_meta_dataframe.md)
for constructing a meta dataframe.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example with default options
consolidated_df <- consolidate_oos_sb_outputs_m_df(
  base_sb_backtest_outputs_list = list_of_sb_results,
  features_passthrough = "all",
  features_m_df = features_df
)
} # }
```
