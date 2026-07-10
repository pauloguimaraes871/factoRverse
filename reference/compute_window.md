# Compute Rolling or Seasonal Calculations for a Given Metric in meta_dataframe or meta_xts

This method computes rolling or seasonal statistics for a specified
metric in a `meta_dataframe` or `meta_xts` object. The function applies
a predefined calculation (`FUN`) to values within a specified time
window.

## Usage

``` r
compute_window(data, period, FUN, ...)

# S4 method for class 'meta_dataframe,numeric,character'
compute_window(
  data,
  period,
  FUN,
  window = "rolling",
  signal,
  benchmark_returns_m_xts = NULL,
  selected_bench = NULL,
  na.rm = TRUE,
  only_unique = FALSE,
  feature_name = NULL,
  min_non_na = 0,
  count_condition_fun = NULL,
  offset_months = 0
)

# S4 method for class 'meta_xts,numeric,character'
compute_window(
  data,
  period,
  FUN,
  window = "rolling",
  col_name,
  benchmark_returns_m_xts = NULL,
  selected_bench = NULL,
  na.rm = TRUE,
  only_unique = FALSE,
  feature_name = NULL,
  min_non_na = 0,
  specific_dates = NULL,
  mult_last_n = 0,
  mult_by = if (FUN == "geom_mean_ret") 0 else -1,
  top_n = 1
)
```

## Arguments

- data:

  A `meta_dataframe` or `meta_xts` object.

- period:

  A `numeric` value indicating the time window:

- FUN:

  A `character` specifying the function to apply. Supported options
  depend on the input class:

  - For a `meta_dataframe`: "sum", "mean", "geom_mean_ret", "median",
    "sd", "skew", "sur", "cagr", "signal_to_noise", "res_mom",
    "idio_vol", "count_if", "max", "min", "lag".

  - For a `meta_xts`: "mean", "median", "geom_mean_ret", "max", "min",
    "sd", "skew", "sur", "cagr", "signal_to_noise", "alpha",
    "alpha_tstat", "beta", "correlation", "res_mom", "idio_vol". The
    benchmark-based FUNs ("res_mom", "idio_vol", "alpha", "alpha_tstat",
    "beta", "correlation") require `benchmark_returns_m_xts` and
    `selected_bench`.

- ...:

  Additional arguments passed to the function.

- window:

  A `character` specifying the window type: either "rolling" (default)
  or "seasonal".

- signal:

  (For `meta_dataframe`) A `character` specifying the column name on
  which the rolling function is computed.

- benchmark_returns_m_xts:

  A `meta_xts` object. Required for `FUN` "res_mom" and "idio_vol".

- selected_bench:

  A `character` specifying the column name in
  `benchmark_returns_m_xts@data` to use. Required for `FUN` "res_mom"
  and "idio_vol".

- na.rm:

  A `logical` indicating whether to remove `NA` values (default: TRUE).

- only_unique:

  A `logical` indicating whether to compute the metric using only unique
  values (default: FALSE).

- feature_name:

  A `character` specifying the name of the computed feature. If NULL
  (default), the feature name is set to
  `"<metric>_<window>_<period>_<FUN>"`.

- min_non_na:

  A `numeric` value specifying the minimum number of non-NA values
  required to compute the rolling statistic. Default is 0.

  - Exception: For `FUN = "cagr"`, the default is `period + 1` to ensure
    sufficient periods.

- count_condition_fun:

  A function that takes a numeric vector and returns a logical vector.
  The function should return `TRUE` for elements that should be counted.
  Only used for FUN = "count_if".

- offset_months:

  A `numeric` value specifying the number of months to offset ahead or
  backwards for `seasonal` window type.

- col_name:

  (For `meta_xts`) A `character` specifying the column name.

- specific_dates:

  A `Date` vector specifying specific dates to consider for the rolling
  calculation.

- mult_last_n:

  A `numeric` value indicating the number of most recent observations to
  multiply by a factor when computing certain metrics (default: 0).

- mult_by:

  A `numeric` value indicating the multiplication factor for the last
  `n` observations when computing certain metrics (default: -1 for most
  metrics, 0 for "geom_mean_ret").

- top_n:

  A `numeric` value indicating the number of top elements to consider
  when computing the "max" function (default: 1).

## Value

A modified `meta_dataframe` or `meta_xts` object with an additional
column named `"<metric>_<window>_<period>_<FUN>"`, storing the computed
values.

## Details

The window can be either:

- **Rolling:** Includes all dates within the range
  `[current_date - period months, current_date]`.

- **Fwd Seasonal:** Includes only observations from the same ticker
  whose months match the consecutive months immediately following the
  current observation's month.

The function filters observations within the specified time window and
applies the selected `FUN`. If no matching observations are found, the
result is `NA`.

Available functions:

- **sum**: `sum(x, na.rm = na.rm)` (meta_dataframe only).

- **mean**: `mean(x, na.rm = na.rm)`.

- **geom_mean_ret**: Geometric mean return over the window
  (percentage-point scale).

- **median**: `stats::median(x, na.rm = na.rm)`.

- **sd**: `stats::sd(x, na.rm = na.rm)`.

- **skew**: Bias-adjusted skewness of the values in the window.

- **sur**:
  `(final_value - mean(x, na.rm = na.rm)) / stats::sd(x, na.rm = na.rm)`,
  where `final_value` is the most recent value.

- **cagr**: Compound growth rate between the first and last value of the
  window.

- **signal_to_noise**: `mean(x) / sd(x)` (mean-to-standard-deviation
  ratio).

- **res_mom**: Rolling regression of the metric on the benchmark;
  returns the standardized sum of residuals (residual momentum).

- **idio_vol**: Rolling regression against the benchmark; returns
  `sqrt(sd(metric)^2 - beta^2 * sd(benchmark)^2)`.

- **alpha**, **alpha_tstat**, **beta**, **correlation**: Rolling
  CAPM-style statistics against the benchmark (meta_xts only).

- **count_if**: Counts the number of elements that satisfy
  `count_condition_fun` (meta_dataframe only).

- **max**: `max(x, na.rm = na.rm)` for a meta_dataframe; sum of the top
  `top_n` values for a meta_xts.

- **min**: `min(x, na.rm = na.rm)`.

- **lag**: Retrieves the observation that happened `period` months ago
  (meta_dataframe only).
