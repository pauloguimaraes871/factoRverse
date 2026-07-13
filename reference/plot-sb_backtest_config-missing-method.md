# Plot Method for `sb_backtest_config`

Calls the appropriate plot method for `tuning_strategy`.

## Usage

``` r
# S4 method for class 'sb_backtest_config,missing'
plot(x, y, palette = "cyberpunk")
```

## Arguments

- x:

  An object of class `sb_backtest_config`.

- y:

  Unused. Included for consistency with the generic `plot` method.

- palette:

  Character. Color palette to use for the plot. Options include
  "cyberpunk", "br" and "journal". Default is "cyberpunk".

## Value

A `ggplot` object visualizing the hyperparameter histograms with
possible limits.
