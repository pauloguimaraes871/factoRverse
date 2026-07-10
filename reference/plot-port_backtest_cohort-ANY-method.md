# Plot Method for port_backtest_cohort Class

Generates various plots to visualize metrics from a
`port_backtest_cohort` object. Users can select which plot to display by
specifying the `plot_id` parameter.

## Usage

``` r
# S4 method for class 'port_backtest_cohort,ANY'
plot(
  x,
  plot_id = NULL,
  vertical_lines = NULL,
  palette = "cyberpunk",
  selected_metric = NULL
)
```

## Arguments

- x:

  An object of class `port_backtest_cohort`.

- plot_id:

  A character string (plot name) or numeric index specifying which plot
  to display; if `NULL`, an interactive menu is shown. One of
  `"Time-Series of Raw Returns"`, `"Time-Series of Net Returns"`,
  `"Time-Series of Raw Active Returns"`,
  `"Time-Series of Net Active Returns"`,
  `"Cross-Sectional Raw Performance Metric Plot"`,
  `"Cross-Sectional Net Performance Metric Plot"`,
  `"Cross-Sectional Raw Active Performance Metric Plot"`,
  `"Cross-Sectional Net Active Performance Metric Plot"`,
  `"Time-Series of Direct Costs"`,
  `"Time-Series of Market Impact Costs"`,
  `"Time-Series of Total Costs"`, `"Time-Series of Turnover"`,
  `"Time-Series of Port Metrics"`, `"Time-Series of Port Stats"`,
  `"Weights Correlogram"`, or `"Weights Radar"`.

- vertical_lines:

  Optional. A named list or vector specifying vertical lines to add to
  time-series plots. Used for highlighting events.

- palette:

  A character string specifying the color palette to use for the plots.
  Only `"cyberpunk"` (default) and `"br"` are supported; any other value
  leaves the plot colors undefined and raises an error.

- selected_metric:

  Optional. A character string specifying a particular performance
  metric to focus on in the plots. If `NULL`, all available metrics will
  be plotted.

## Value

Invisibly returns the input object.
