# Plot Method for ss_backtest_results Class

Generates various plots to visualize metrics from the
`ss_backtest_results` object. Users can select which plot to display by
specifying the `plot_id` parameter (numeric index or exact name); if
omitted, an interactive menu is shown. Available plots for every
`ss_backtest_results` object: `"Time-Series Metrics by Signal"`,
`"Average Time-Series Metrics by Theme"`,
`"Compare Metrics Side-by-Side by Signals"`,
`"Compare Metrics Side-by-Side by Theme"`, `"Box-Plot by Theme"`,
`"Box-Plot by Eligibility"`, `"Waterfall Plot by Signal"`,
`"Waterfall Plot by Theme"`, `"Eligibility by Theme"`. When
`x@p_correction_method == "bayesian"`, the following
posterior-diagnostic plots are also available:
`"Posterior Individual Alphas"`, `"Posterior Individual Betas"`,
`"Posterior Random Effects"`,
`"Waterfall Plot of Posterior Variance Components"`,
`"Posterior Regression Lines"`,
`"Waterfall Plot of Return Decomposition by Signal"`,
`"Posterior Individual Alpha Distributions by Theme and Signal"`.

## Usage

``` r
# S4 method for class 'ss_backtest_results,ANY'
plot(x, plot_id = NULL, palette = "cyberpunk", variable = NULL)
```

## Arguments

- x:

  An object of class `ss_backtest_results`.

- plot_id:

  A character string or numeric value specifying which plot to display
  (see `@description` for the full list of names).

- palette:

  A character string specifying the color palette. Must be one of
  `"cyberpunk"` (dark theme) or `"br"` (light theme); any other value is
  not handled and will error. Default is `"cyberpunk"`.

- variable:

  Character vector of tickers to include, used only by the Bayesian
  posterior-diagnostic plots (`plot_id` 10 and above). If `NULL`, an
  interactive ticker-selection prompt is shown. Ignored for the other
  plot types.

## Value

Invisibly returns the input object.
