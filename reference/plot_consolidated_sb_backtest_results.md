# Plot Various Consolidated Backtest Results

This function creates a variety of diagnostic and comparative plots for
both out-of-sample (OOS) and validation metrics obtained from a set of
backtest results. The type of plot depends on the value supplied to
`plot_name`.

## Usage

``` r
plot_consolidated_sb_backtest_results(
  combined_metrics,
  mean_validation_metrics,
  time_series_oos_testing_metrics,
  time_series_validation_metrics,
  base_learners,
  plot_name,
  chosen_backtests,
  palette
)
```

## Arguments

- combined_metrics:

  A named list containing out-of-sample testing metrics, typically the
  `combined_oos_testing_metrics` slot from an `sb_metabacktest_results`
  object. Must have elements `all_dates_oos_testing_metrics` and
  `common_dates_oos_testing_metrics`.

- mean_validation_metrics:

  A data frame containing aggregated validation metrics for each
  backtest, usually found in the `mean_validation_metrics` slot of
  `sb_metabacktest_results`.

- time_series_oos_testing_metrics:

  A named list of time-series objects (`meta_xts`) capturing OOS testing
  metrics. Each entry corresponds to a distinct metric.

- time_series_validation_metrics:

  A named list of time-series objects (`meta_xts`) capturing validation
  metrics. Each entry corresponds to a distinct metric.

- base_learners:

  A list of backtest objects used primarily when plotting the
  `\"Prediction Error Correlation\"`, since it needs the error outputs
  of each learner to build the correlation matrix.

- plot_name:

  A character string indicating which plot to generate. Options include:

  `\"Combined and Consolidated OOS Testing Metrics - All Dates\"`

  :   Shows a bar chart of OOS testing metrics across all backtests for
      the full date range.

  `\"Combined and Averaged OOS Testing Metrics - Common Dates\"`

  :   Shows a bar chart of OOS testing metrics across all backtests
      restricted to the common date range.

  `\"Time Series OOS Testing Metrics\"`

  :   Plots time-series OOS metrics for each backtest, typically over
      the entire out-of-sample period.

  `\"Mean Validation Metrics Comparison\"`

  :   Creates a bar chart comparing average validation metrics across
      backtests.

  `\"Time Series Validation Metrics\"`

  :   Plots validation metrics in a time series format, if such data
      exist.

  `\"Prediction Error Correlation\"`

  :   Builds a correlation heatmap of prediction errors among the
      selected backtest learners.

- chosen_backtests:

  A vector of backtest identifiers (e.g., c("backtest1", "backtest2"))

- palette:

  A character string indicating the color palette to use for the plots.

## Value

Called for its side effects of displaying plots. For some cases, a
ggplot2 plot object is returned invisibly (e.g., in the
`\"Prediction Error Correlation\"` case).

## Details

When creating each plot, the function re-labels lengthy backtest
identifiers with numeric labels for clarity. It also prints a legend
that maps the numeric label back to the underlying identifier. Note that
most plot types rely on subsets of the data contained in
`combined_metrics`, `mean_validation_metrics`,
`time_series_oos_testing_metrics`, and `time_series_validation_metrics`.

For the `\"Prediction Error Correlation\"` plot:

- The user can select a subset of backtests (by indices) or all of them.

- A correlation matrix is constructed from the merged error columns of
  each selected backtest, and a heatmap is displayed where only the
  lower triangle is filled.

- A legend maps each column index to the corresponding backtest
  identifier.

## Examples
