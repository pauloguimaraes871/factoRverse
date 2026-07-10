# Plot Method for sb_metabacktest_results Class

Generates various plots to visualize the performance and metrics of
meta-learning backtest results. Users can select which plot to display
by specifying the `plot_id` parameter, either by name or by number. The
plots include comparisons between base learners and meta learners over
time.

## Usage

``` r
# S4 method for class 'sb_metabacktest_results,ANY'
plot(
  x,
  plot_id = NULL,
  palette = "cyberpunk",
  chosen_metric = NULL,
  chosen_backtests = NULL,
  top_n = NULL,
  facet_by_year = NULL,
  add_overall_means = NULL
)
```

## Arguments

- x:

  An object of class `sb_metabacktest_results` containing the results of
  the meta-learning backtests.

- plot_id:

  A character string or numeric value specifying which plot to display.

  - By name: Options are:

    - `"Combined and Consolidated OOS Testing Metrics - All Dates"`

    - `"Combined and Averaged OOS Testing Metrics - Common Dates"`

    - `"Time Series OOS Testing Metrics"`

    - `"Mean Validation Metrics Comparison"`

    - `"Time Series Validation Metrics"`

    - `"Prediction Error Correlation"`

    - `"Base Learners vs Meta Learners Over Time"`

    - `"Hierarchical Feature Importance"`

  - By number: Provide a number corresponding to the plot (as listed
    when `plot_id` is `NULL`). If `NULL` (default), the method lists
    available plots.

- palette:

  Character. Color palette to use for the plot. Options include
  "cyberpunk" and "br". Default is "cyberpunk".

- chosen_metric:

  Character. The specific metric to plot (e.g., "rmse", "mae"). Required
  for certain plots.

- chosen_backtests:

  Character vector. Specific backtests to include in the plot.

- top_n:

  Numeric. If specified, limits the plot to the top N backtests based on
  the chosen metric.

- facet_by_year:

  Logical. If `TRUE`, facets the plot by year.

- add_overall_means:

  Logical. If `TRUE`, adds horizontal lines representing the overall
  mean of the chosen metric.

## Value

Invisibly returns the input `x`.
