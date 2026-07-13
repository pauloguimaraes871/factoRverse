# Plot Method for 'port' Objects

This method generates plots for a `port` object, depending on the
specified `type`.

## Usage

``` r
# S4 method for class 'port,missing'
plot(
  x,
  type = NULL,
  palette = "cyberpunk",
  chosen_weights = NULL,
  chosen_risk_metrics = NULL,
  add_bench = NULL,
  tickers = NULL,
  level = NULL,
  by_group = NULL,
  groups = NULL,
  micro_port = NULL,
  active_weights = NULL,
  top_n = NULL,
  ...
)
```

## Arguments

- x:

  An object of class `"port"`.

- type:

  A character string specifying the type of plot to generate. If `NULL`,
  the user will be prompted.

- palette:

  A character string specifying the color palette to use for the plots.
  Default is "cyberpunk". Supported options include "cyberpunk", "br"
  and "journal".

- chosen_weights:

  A character vector of asset names to include in the weights plot (if
  `type = "weights"`). If `NULL`, all assets are included.

- chosen_risk_metrics:

  A character vector of risk metrics to include in the risk plots (if
  applicable). If `NULL`, all risk metrics are included.

- add_bench:

  A logical indicating whether to add benchmark weights to the weights
  plot (if `type = "weights"`). Default is `FALSE`.

- tickers:

  A character vector of asset tickers to include in the plot (if
  applicable). If `NULL`, all tickers are included.

- level:

  A character string specifying the grouping variable for the group
  composition plot (if type = "group_composition").#' @param by_group A
  logical indicating whether to facet the group composition plot by the
  grouping.

- by_group:

  A logical indicating whether to facet the group composition plot by
  the grouping variable (if `type = "group_composition"`). Default is
  `FALSE`.

- groups:

  A character vector of group names to include in the group composition
  plot (if `type = "group_composition"`). If `NULL`, all groups are
  included.

- micro_port:

  A logical indicating the micro portfolio to plot.

- active_weights:

  A logical indicating whether to plot active weights instead of total
  weights

- top_n:

  An integer specifying the number of top assets to include in the
  weights plot (if `type = "weights"`). If `NULL`, all assets are
  included.

- ...:

  Additional arguments for future extensions (currently unused).

## Value

A `ggplot` object (invisibly). The function also prints the plot.

## Details

The currently supported plot types are:

1.  `"weights"` - A bar chart of the portfolio weights (or active
    weights).

2.  `"exp_ret_score"` - A bar chart of the portfolio's expected return
    scores.

3.  `"risk_return"` - A scatterplot of expected return score (y) vs.
    risk (x).

4.  `"correlation"` - A heatmap of the asset correlation matrix.

5.  `"relative_risk_contribution"` - A bar chart comparing weights and
    risk contributions.

6.  `"efficient_frontier"` - A scatterplot of random portfolios with
    Sharpe ratio coloring and the optimal portfolio highlighted.

7.  `"random_weights_distribution"` - A jitter plot of random portfolio
    weights with constraints.

8.  `"group_composition"` - A grouped bar chart comparing portfolio and
    benchmark weights across classification variables (e.g., sectors).

9.  `"hierarchical_clustering"` - A dendrogram showing hierarchical
    clustering of assets.

This method is dispatched on signature `plot(x = "port", y = "missing")`
and does not use the `y` argument.
