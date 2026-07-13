# Plot Method for meta_xts

Plots time series or performance summaries from a `meta_xts` (or
subclass) object using `ggplot2`. Offers interactive prompts for series
selection, cumulative return computation, faceting, and more.

## Usage

``` r
# S4 method for class 'meta_xts,missing'
plot(
  x,
  y,
  variable = NULL,
  clustering_list = NULL,
  facet_by_year = NULL,
  add_overall_means = NULL,
  vertical_lines = NULL,
  cumulative = NULL,
  plot_perf_metric = NULL,
  benchmark_returns_m_xts = NULL,
  active_returns = FALSE,
  chosen_metric = NULL,
  palette = "cyberpunk",
  ...
)
```

## Arguments

- x:

  A `meta_xts` or subclass object to be plotted.

- y:

  Not used. Included for S4 method compatibility.

- variable:

  Variable to plot. If `NULL`, user is prompted to select from available
  columns.

- clustering_list:

  Optional named list assigning column names to cluster groups. Used to
  vary linetype by group.

- facet_by_year:

  Logical. Whether to facet plots by calendar year. If `NULL`, user is
  prompted interactively.

- add_overall_means:

  Logical. Whether to add dashed horizontal lines representing
  series-wide means. Only applies if `cumulative = FALSE`. If `NULL`,
  user is prompted.

- vertical_lines:

  Optional vector of Dates or POSIXct timestamps at which to draw
  vertical dashed lines.

- cumulative:

  Logical. If `TRUE` and object is of class `returns_meta_xts`, plots
  cumulative returns (in %). If `NULL`, prompts the user.

- plot_perf_metric:

  Logical. For `returns_meta_xts` only. If `TRUE`, plots bar chart of
  selected performance metric instead of time series. If `NULL`, user is
  prompted.

- benchmark_returns_m_xts:

  Optional `meta_xts` object (single-column) with benchmark returns.
  Required if `active_returns = TRUE`.

- active_returns:

  Logical or `"yes"/"no"` string. Whether to compute active returns
  relative to the benchmark when `plot_perf_metric = TRUE`.

- chosen_metric:

  Character. Performance metric to plot when `plot_perf_metric = TRUE`.
  Options include "CAGR", "Volatility", "Sharpe Ratio", etc. If `NULL`,
  user is prompted.

- palette:

  Character. One of `"cyberpunk"` (default; dark background, neon series
  colors), `"br"` (light background, brand palette) or `"journal"`
  (white background, sober print-oriented colours).

- ...:

  Currently unused.

## Value

Invisibly returns the generated `ggplot` object.

## Details

The function behaves differently depending on whether the object is a
subclass like `returns_meta_xts`. It includes:

1.  Interactive column selection.

2.  Optional performance metric visualization for returns objects.

3.  Optional cumulative return computation.

4.  Option to facet plots by year.

5.  Option to overlay horizontal mean lines.

6.  Option to add vertical reference lines.

7.  Neon color palette and dark theme styling.

## Requirements

Requires the suggested packages `gridExtra`, `scales`, `ggdist`,
`ggraph`, `ggrepel`, `igraph`, and `RColorBrewer`; if any are missing,
an informative [`stop()`](https://rdrr.io/r/base/stop.html) is raised
rather than a cryptic failure.

## Interactivity

Any of `variable`, `facet_by_year`, `cumulative`, `plot_perf_metric`,
`active_returns`, or `chosen_metric` left as `NULL` triggers an
interactive [`readline()`](https://rdrr.io/r/base/readline.html) prompt.
Supply all of them explicitly to use this method in non-interactive
contexts (scripts, `knitr`, tests).

## See also

[`create_performance_m_df()`](https://pauloguimaraes871.github.io/factoRverse/reference/create_performance_m_df.md)
