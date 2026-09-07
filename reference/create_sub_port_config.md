# Create a Sub Portfolio Configuration

Constructor for a `sub_port_config` object, the specification of one
inner portfolio built by a layered (`*af`) portfolio construction
method. It says which construction method the recursive
[`set_portfolio_weights()`](https://pauloguimaraes871.github.io/factoRverse/reference/set_portfolio_weights.md)
call must use, and carries the parameters of that method.

## Usage

``` r
create_sub_port_config(
  port_construction_method,
  mvo_parameters = NULL,
  rp_parameters = NULL,
  hrp_parameters = NULL,
  class = "sub_port_config"
)
```

## Arguments

- port_construction_method:

  A character string with the method used to build the sub-portfolio.
  Must be one of 'ew', 'sw', 'cw', 'cs', 'rp', 'hrp' or 'mvo'.

- mvo_parameters:

  An object of class `mvo_parameters`. Only used when
  `port_construction_method` is 'mvo'. If missing for 'mvo', a default
  is created.

- rp_parameters:

  An object of class `rp_parameters`. Only used when
  `port_construction_method` is 'rp'. If missing for 'rp', a default is
  created.

- hrp_parameters:

  An object of class `hrp_parameters`. Only used when
  `port_construction_method` is 'hrp'. If missing for 'hrp', a default
  is created.

- class:

  A character string naming the class to instantiate. Defaults to
  `"sub_port_config"`. Any class extending it (e.g.
  `"mmaf_sub_port_config"`) is accepted, which lets each layered method
  keep a self-documenting class name while sharing one configuration
  contract.

## Value

An S4 object of class `sub_port_config` (or of the requested subclass).
