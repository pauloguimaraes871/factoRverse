# Plot Method for port_backtest_results Class

Generates various plots to visualize metrics from the
`port_backtest_results` object. Users can select which plot to display
by specifying the `plot_id` parameter.

## Usage

``` r
# S4 method for class 'port_backtest_results,ANY'
plot(
  x,
  plot_id = NULL,
  vertical_lines = NULL,
  palette = "cyberpunk",
  group_col = NULL,
  facet_by_year = NULL
)
```

## Arguments

- x:

  An object of class `port_backtest_results`.

- plot_id:

  A character string (plot name) or numeric index specifying which plot
  to display; if `NULL`, an interactive menu is shown. One of
  `"Time-Series Weights by Tickers"`,
  `"Time-Series Weights by Stock Group"`,
  `"Cross-Sectional Weights Statistic by Tickers"`,
  `"Cross-Sectional Weights Statistic by Stock Group"`,
  `"Tile Heatmap of Weights by Tickers"`,
  `"Tile Heatmap of Weights by Stock Group"`,
  `"Time-Series of Groups Composition"`,
  `"Time-Series of Expected Return Score by Tickers"`,
  `"Time-Series of Expected Return Score by Stock Group"`,
  `"Time-Series of Expected Return Score by Eligibility"`,
  `"Box-plot of Expected Return Score by Stock Group"`,
  `"Box-plot of Expected Return Score by Eligibility"`,
  `"Plot Subjacent Final Port"`, `"Time-Series of Port Returns"`,
  `"Cross-Sectional Performance Metric Plot"`,
  `"Time-Series of Transaction Costs"`, `"Time-Series of Port Metrics"`,
  or `"Port Stats"`.

- vertical_lines:

  Optional. A vector of `Date` objects indicating vertical lines to
  display in time-series plots (e.g., rebalance dates). If `NULL`, the
  user will be prompted.

- palette:

  A character string specifying the color palette to use for the plots.
  Only `"cyberpunk"` (default) and `"br"` are supported; any other value
  leaves the plot colors undefined and raises an error.

- group_col:

  Optional. A character string specifying the column name in the
  backtest results to use for grouping in certain plots. If `NULL`, the
  user will be prompted to select from available columns.

- facet_by_year:

  Logical. If `TRUE`, time-series plots will be faceted by year. Default
  is `FALSE`.

## Value

Invisibly returns the input object.
