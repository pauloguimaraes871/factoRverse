# Consolidate Meta Backtest Results

This function takes a collection of `sb_backtest_results` objects along
with optional meta-backtest information, and consolidates out-of-sample
and validation metrics into both tabular and time-series formats.
Specifically, it:

- Identifies backtests that include validation metrics.

- Calculates common testing date ranges across all backtests.

- Reshapes both out-of-sample and validation metrics into a consistent
  `long` format, as well as into time-series `xts` objects
  (`time_series_oos_testing_metrics` and
  `time_series_validation_metrics`).

- Combines full-sample metrics and those restricted to common dates,
  while preserving a `testing_dates_range` column in the returned data
  frames.

## Usage

``` r
consolidate_sb_metabacktest_results(
  all_sb_backtest_results,
  meta_sb_name,
  base_sb_names
)
```

## Arguments

- all_sb_backtest_results:

  A list of `sb_backtest_results` objects that you wish to consolidate.

- meta_sb_name:

  An optional character string representing the name of the meta
  backtest results. If `NULL`, meta-related naming will be skipped.

- base_sb_names:

  A character vector containing the names of base backtest results.

## Value

A named list containing the following elements:

- `all_dates_oos_testing_metrics`: A data frame of out-of-sample metrics
  for the full date range.

- `common_dates_oos_testing_metrics`: A data frame of out-of-sample
  metrics restricted to the common date range found across all
  backtests.

- `mean_validation_metrics`: A data frame of validation metrics for the
  backtests that provide them.

- `time_series_oos_testing_metrics`: A named list of `xts` objects
  containing out-of-sample testing metrics (by metric).

- `time_series_validation_metrics`: A named list of `xts` objects
  containing validation metrics (by metric).

## Details

The function first determines which backtest objects contain validation
metrics by inspecting the `sb_algorithm` in each workflow. It then
computes the intersection of testing dates across all backtests and
extracts both out-of-sample and validation metrics into separate data
frames. Key columns, such as `testing_dates_range`, are included to
indicate the date span over which metrics were calculated. Subsequently,
the metrics are reshaped into both `long` and `wide` formats to
facilitate time-series representations in `xts`.

## Examples
