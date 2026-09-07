# Show Method for port_metabacktest_config Class

Displays a `port_metabacktest_config`: the meta-level allocation scheme,
how the meta universe reads the base portfolios, and the wrapped
`port_backtest_config`, which is delegated to its own `show` method.

## Usage

``` r
# S4 method for class 'port_metabacktest_config'
show(object)
```

## Arguments

- object:

  An object of class `port_metabacktest_config`.

## Value

Invisibly returns NULL.
