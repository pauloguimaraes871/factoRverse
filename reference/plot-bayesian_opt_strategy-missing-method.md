# Plot Method for `bayesian_opt_strategy`

Plot the lower/upper bounds for each hyperparameter in
`bayesian_opt_strategy`.

## Usage

``` r
# S4 method for class 'bayesian_opt_strategy,missing'
plot(x, y, palette = "cyberpunk", ...)
```

## Arguments

- x:

  An object of class `bayesian_opt_strategy`.

- y:

  Unused. Included for consistency with the generic `plot` method.

- palette:

  Character. Color palette to use for the plot. Options include
  "cyberpunk", "br" and "journal". Default is "cyberpunk".

- ...:

  Additional arguments passed to the plotting method (currently unused).

## Value

Invisibly returns the `ggplot` object visualizing the bounds (also
printed as a side effect).

## Requirements

Requires the suggested packages `gridExtra`, `scales`, `ggdist`,
`ggraph`, `ggrepel`, `igraph`, and `RColorBrewer`; if any are missing,
an informative [`stop()`](https://rdrr.io/r/base/stop.html) is raised.
