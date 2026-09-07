# Expand a Sub Portfolio Configuration into set_portfolio_weights Arguments

Turns a `sub_port_config` into the named argument list a recursive
[`set_portfolio_weights`](https://pauloguimaraes871.github.io/factoRverse/reference/set_portfolio_weights.md)
call expects. Layered (`*af`) methods use it so that an inner portfolio
is always parameterized from its own configuration, never from whatever
the parent method happened to be using.

## Usage

``` r
expand_sub_port_config(sub_port_config)
```

## Arguments

- sub_port_config:

  An object of class `sub_port_config`, or of a class extending it such
  as `mmaf_sub_port_config`.

## Value

A named list of arguments, always containing `port_construction_method`
and, for covariance-based methods, that method's parameters. Intended to
be spliced into a `do.call(set_portfolio_weights, ...)`.

## Details

Only the parameters of the configured method are expanded. A
configuration carrying, say, `rp_parameters` while its method is `"ew"`
produces no risk-parity arguments, so the inner call falls back to the
documented defaults of
[`set_portfolio_weights`](https://pauloguimaraes871.github.io/factoRverse/reference/set_portfolio_weights.md)
rather than silently inheriting a stray setting.

When the method requires a parameter object and none was supplied, the
corresponding default parameter object is created, matching the
behaviour of
[`create_port_backtest_config`](https://pauloguimaraes871.github.io/factoRverse/reference/create_port_backtest_config.md).

## See also

[`create_sub_port_config`](https://pauloguimaraes871.github.io/factoRverse/reference/create_sub_port_config.md),
[`set_portfolio_weights`](https://pauloguimaraes871.github.io/factoRverse/reference/set_portfolio_weights.md)
