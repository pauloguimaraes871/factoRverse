# Plot Method for 'sb_model' Objects

This method dispatches plotting to the underlying model stored in the
`model` slot of a `sb_model` object.

## Usage

``` r
# S4 method for class 'sb_model,missing'
plot(x, type = NULL, palette = "cyberpunk", ...)
```

## Arguments

- x:

  An object of class `sb_model`.

- type:

  Currently unused. Included for compatibility with other plot methods.

- palette:

  Character. Color palette to use for the plot. Options include
  "cyberpunk" and "br". Default is "cyberpunk". This will be passed to
  the underlying model's plot method if applicable.

- ...:

  Additional arguments passed to the plot method of the underlying
  model.
