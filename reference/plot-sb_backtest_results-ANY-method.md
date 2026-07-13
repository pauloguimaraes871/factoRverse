# Plot Signal Blending Walk-Forward Validation Results

This method generates various plots to visualize the performance of
machine learning models using walk-forward validation metrics. Users can
select which plot to display by specifying the `plot_id` parameter,
either by name or by number.

## Usage

``` r
# S4 method for class 'sb_backtest_results,ANY'
plot(
  x,
  plot_id = NULL,
  features_m_df = NULL,
  palette = "cyberpunk",
  ticker_to_explain = NULL,
  date_to_explain = NULL
)
```

## Arguments

- x:

  An object of class `sb_backtest_results` containing the results of the
  walk-forward validation.

- plot_id:

  A character string or numeric value specifying which plot to display.
  Available plots depend on `sb_algorithm`. For ML algorithms ("glmnet",
  "rf", "xgb", "nn"):

  - `"Chosen Evaluation Metric Over Time"`

  - `"Test vs Validation Chosen Evaluation Metric Over Time"`

  - `"Best Hyperparameters Over Time"`

  - `"Hyperparameters vs Error"`

  - `"All Evaluation Metrics Over Time"`

  - `"Consolidated OOS Testing Metrics"`

  - `"OOS Predictions, Errors and Targets"`

  - `"Consolidated OOS Testing Metrics vs Average Validation"`

  - `"Final Signal-Blending Model"`

  - `"Time-Series Feature Importance by Signal"`

  - `"Average Time-Series Feature Importance by Theme"`

  - `"Compare Feature Importance Side-by-Side by Signal"`

  - `"Compare Feature Importance Side-by-Side by Theme"`

  - `"Feature Importance Box-Plot by Signal"`

  - `"Feature Importance Box-Plot by Theme"`

  - `"Feature Importance Heatmap by Signal"`

  - `"Feature Importance Heatmap by Theme"`

  - `"Explain Prediction"` For "ols" and heuristic/portfolio algorithms,
    the tuning-specific plots (chosen-metric, test-vs-validation,
    hyperparameter, and consolidated-vs-validation plots) are omitted.
    Provide a number (index into the list shown when `plot_id` is
    `NULL`), or `NULL` (default) to list them.

- features_m_df:

  A `meta_dataframe` containing features used in the backtest. Required
  for plots like `"Explain Prediction"`.

- palette:

  Character. Color palette to use for the plot. Options include
  "cyberpunk", "br" and "journal". Default is "cyberpunk".

- ticker_to_explain:

  Character. Ticker symbol to explain in the "Explain Prediction" plot.

- date_to_explain:

  Date. Date to explain in the "Explain Prediction" plot.

## Value

Invisibly returns the input `x`.
