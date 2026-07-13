# Plot Method for port_backtest_config: Faceted Liquidity Floor Cutoffs (Ordered Within Facets)

Generates a faceted bar plot displaying liquidity floor cutoff metrics
from a `port_backtest_config` object. The liquidity floor cutoffs data
is expected to contain one grouping column (automatically identified as
the first non-numeric column) and one or more numeric columns. The data
is pivoted into a long format and, within each metric facet, the
grouping variable is reordered (from left to right) based on its numeric
value (smallest to largest). All text in the plot (including facet
titles) is displayed in white.

## Usage

``` r
# S4 method for class 'port_backtest_config,missing'
plot(x, palette = "cyberpunk", ...)
```

## Arguments

- x:

  A `port_backtest_config` object containing liquidity floor cutoffs
  data in the `liquidity_floor_cutoffs` slot.

- palette:

  A character string specifying the color palette to use for the bars.
  Options include "cyberpunk", "br" and "journal". Default is
  "cyberpunk".

- ...:

  Additional arguments (currently not used).

## Value

A `ggplot` object representing the faceted liquidity floor cutoffs plot.
