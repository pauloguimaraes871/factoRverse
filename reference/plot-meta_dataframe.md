# Plot meta_dataframe

S4 `plot` method for
[`meta_dataframe`](https://pauloguimaraes871.github.io/factoRverse/reference/meta_dataframe-class.md)
objects. Produces a variety of diagnostic and summary visualisations
(time series, cross-sectional, histogram, boxplot, composition,
regression, density2d, correlogram, radar, waterfall, tile heatmap,
frequency). The function performs internal filtering and summarisation
before rendering a `ggplot` (or base plot for radar charts).

## Usage

``` r
# S4 method for class 'meta_dataframe,missing'
plot(
  x,
  type = NULL,
  clustering_variables = "auto",
  variable = NULL,
  tickers = NULL,
  dates = NULL,
  calc_stat = NULL,
  custom_filter = NULL,
  filter_values = NULL,
  dep_y = NULL,
  numeric_aggregation = "decile",
  palette = "cyberpunk"
)
```

## Arguments

- x:

  A `meta_dataframe` object.

- type:

  Character. Plot type. One of `"cross_sectional"`, `"time_series"`,
  `"histogram"`, `"boxplot"`, `"composition"`, `"regression"`,
  `"density2d"`, `"correlogram"`, `"radar"`, `"waterfall"`,
  `"tile_heatmap"`, `"frequency"`.

- clustering_variables:

  Character vector or `"auto"`. Columns to group/facet by (e.g.
  `"dates"`, `"tickers"`). Use `NULL` for no clustering.

- variable:

  Character vector. Main numeric variable(s) to analyse or visualise
  (must be column names of `x@data`). For bivariate plots, supply two
  variables when appropriate.

- tickers:

  Character vector or `"all"`. Tickes to include (default `"all"`).

- dates:

  Either a single `Date`, vector of `Date`, a date range, or `"all"`.

- calc_stat:

  Character. Summary statistic or metric to compute (e.g. `"mean"`,
  `"sd"`, `"median"`, `"cor"`, `"beta"`, `"alpha"`, quantiles like
  `"q05"`, etc.). Defaults vary by `type`.

- custom_filter:

  Character or character vector. Column(s) to filter on (e.g.
  `"sector"`).

- filter_values:

  A vector or list of values used to filter `custom_filter` columns.

- dep_y:

  Optional character. Dependent variable used for bivariate statistics
  (required when `calc_stat` is `"cor"`, `"beta"`, `"alpha"` or `type`
  is `"regression"`).

- numeric_aggregation:

  Character. How to discretize numeric clustering variables: one of
  `"decile"`, `"quartile"`, `"tercile"`, `"median"`. Default:
  `"decile"`.

- palette:

  Character. Colour palette to use for the plot (e.g. `"cyberpunk"`,
  `"br"`).

## Value

A `ggplot` object (or base plot for radar). The plot is printed as a
side-effect.

## Details

Plot method for meta_dataframe objects

- Univariate plots use `variable`. Bivariate plots require `dep_y` or
  two variables.

- `tile_heatmap` and `composition` perform internal quantile binning and
  aggregation.

- When `clustering_variables="auto"` the function will interactively
  prompt for sensible defaults if run in an interactive session. Many
  arguments can be selected interactively when omitted.

- Required plotting packages: ggplot2, gridExtra, scales, ggdist,
  ggraph, ggrepel, igraph, RColorBrewer. The method stops if
  dependencies are missing.

## Examples

``` r
if (FALSE) { # \dontrun{
# Cross-sectional mean of 'score' by tickers on a given date
plot(mdf, type = "cross_sectional", variable = "score", calc_stat = "mean",
     clustering_variables = "tickers", dates = as.Date("2026-07-01"))

# Time series of 'momentum' for a specific ticker
plot(mdf, type = "time_series", variable = "momentum",
     clustering_variables = "dates", tickers = "AAPL")

# Histogram of a signal distribution
plot(mdf, type = "histogram", variable = "signal", clustering_variables = "tickers")

# Boxplot of a signal across tickers
plot(mdf, type = "boxplot", variable = "signal", clustering_variables = "tickers")

# Composition of themes (uses calc_stat = "mean")
plot(mdf, type = "composition", variable = "theme")

# Regression of signal_x against return_1m (bivariate)
plot(mdf, type = "regression", variable = "signal_x", dep_y = "return_1m",
     clustering_variables = "tickers")

# 2D density of two signals
plot(mdf, type = "density2d", variable = c("signal_x", "signal_y"), tickers = "all")

# Correlogram across several metrics
plot(mdf, type = "correlogram", variable = c("signal_a", "signal_b", "signal_c"))

# Radar chart (requires >= 3 variables)
plot(mdf, type = "radar", variable = c("IR", "alpha", "turnover"))

# Waterfall (e.g., cumulative contribution)
plot(mdf, type = "waterfall", variable = "exp_ret", calc_stat = "sum",
     clustering_variables = "tickers")

# Tile heatmap over dates × groups (quantile-binned)
plot(mdf, type = "tile_heatmap", variable = "score", clustering_variables = c("dates", "tickers"))

# Frequency plot for categorical/count diagnostics
plot(mdf, type = "frequency", variable = "sector")
} # }
```
