# Plot Method for port_metabacktest_results Class

Plots a multi-portfolio meta backtest: how the allocation across base
portfolios moved, how the resulting portfolio compares against those
bases, and how the meta score fed through into the weights. Also
delegates to the stock-level backtest and to the cohort, which carry
their own plots.

## Usage

``` r
# S4 method for class 'port_metabacktest_results,ANY'
plot(x, plot_id = NULL, palette = "cyberpunk")
```

## Arguments

- x:

  An object of class `port_metabacktest_results`.

- plot_id:

  A character string naming a plot, or its numeric index. If `NULL`, a
  menu is shown. One of `"Meta Weights Over Time"`,
  `"Meta vs Base Cumulative Returns"`,
  `"Meta vs Base Costs and Turnover"`, `"Meta vs Base Portfolio Stats"`,
  `"Meta Score vs Meta Weight"`, `"Plot Stock-Level Backtest"` or
  `"Plot Base Portfolio Cohort"`.

- palette:

  One of `"cyberpunk"`, `"br"` or `"journal"`.

## Value

Invisibly returns the input object.
