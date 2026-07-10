# Show a `port` object

Provides a concise summary of a `port` object, including its subclass,
portfolio name, construction method, eligible assets, weights,
covariance/correlation matrices, and key parameters for MVO or RP
methods.

## Usage

``` r
# S4 method for class 'port'
show(object)
```

## Arguments

- object:

  An instance of class `port` or one of its subclasses (e.g.,
  `signal_port`, `signal_blend_stock_port`, `single_signal_stock_port`).

## Value

Returns the `object` invisibly.

## See also

[port](https://pauloguimaraes871.github.io/factoRverse/reference/port-class.md)
