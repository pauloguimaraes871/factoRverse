# Plot Method for `random_search_strategy`

Draw a fresh random sample from each hyperparameter's assigned
distribution and plot the resulting histograms/violins for random
search.

## Usage

``` r
# S4 method for class 'random_search_strategy,missing'
plot(x, y, palette = "cyberpunk")
```

## Arguments

- x:

  An object of class `random_search_strategy`.

- y:

  Unused. Included for consistency with the generic `plot` method.

- palette:

  Character. Color palette to use for the plot. Options include
  "cyberpunk" and "br". Default is "cyberpunk".

## Value

Invisibly returns the `ggplot` object visualizing the hyperparameter
histograms and their predefined limits (also printed as a side effect).

## Requirements

Requires the suggested packages `gridExtra`, `scales`, `ggdist`,
`ggraph`, `ggrepel`, `igraph`, and `RColorBrewer`; if any are missing,
an informative [`stop()`](https://rdrr.io/r/base/stop.html) is raised.

## Note

Samples are drawn fresh on every call (no seed control) purely to
illustrate the shape of each distribution – they are **not** the actual
draws
[`hyper_tune()`](https://pauloguimaraes871.github.io/factoRverse/reference/hyper_tune.md)
will use during tuning, and repeated calls can look different,
especially for small `n_iter`.
