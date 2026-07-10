# Plot Method for `grid_search_strategy`

Plot the values selected for each hyperparameter in `hyper_grid_domain`
for grid search strategy.

## Usage

``` r
# S4 method for class 'grid_search_strategy,missing'
plot(x, y, palette = "cyberpunk")
```

## Arguments

- x:

  An object of class `grid_search_strategy`.

- y:

  Unused. Included for consistency with the generic `plot` method.

- palette:

  Character. Color palette to use for the plot. Options include
  "cyberpunk" and "br". Default is "cyberpunk".

## Value

Invisibly returns the `ggplot` object visualizing the hyperparameter
grid and its predefined limits (also printed as a side effect).

## Requirements

Requires the suggested packages `gridExtra`, `scales`, `ggdist`,
`ggraph`, `ggrepel`, `igraph`, and `RColorBrewer`; if any are missing,
an informative [`stop()`](https://rdrr.io/r/base/stop.html) is raised.
