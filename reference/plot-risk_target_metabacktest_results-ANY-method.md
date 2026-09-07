# Plot Method for risk_target_metabacktest_results Class

Plots a risk-targeted meta backtest: the capital-market-line view of the
sleeve blended against its residual, the targeting rule itself, and how
the realised risk compares against the level asked for.

## Usage

``` r
# S4 method for class 'risk_target_metabacktest_results,ANY'
plot(x, plot_id = NULL, palette = "cyberpunk", rolling_window = 12)
```

## Arguments

- x:

  An object of class `risk_target_metabacktest_results`.

- plot_id:

  A character string naming a plot, or its numeric index. If `NULL`, a
  menu is shown. One of `"Meta Weights Over Time"`,
  `"Meta vs Base Cumulative Returns"`, `"Meta Costs and Turnover"`,
  `"Capital Market Line"`, `"Risky Weight vs Sleeve Risk"`,
  `"Realised vs Target Risk"`, `"Exposure and Risk Ratio"`,
  `"Plot Stock-Level Backtest"` or `"Plot Base Portfolio Cohort"`.

- palette:

  One of `"cyberpunk"`, `"br"` or `"journal"`.

- rolling_window:

  Number of months used for the realised rolling risk. Default 12.

## Value

Invisibly returns the input object.
