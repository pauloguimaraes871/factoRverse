# Sub Portfolio Configuration

An S4 class to represent the configuration of a sub-portfolio built by a
layered portfolio construction method (the `*af` family). A layered
method calls
[`set_portfolio_weights()`](https://pauloguimaraes871.github.io/factoRverse/reference/set_portfolio_weights.md)
recursively on a block of the universe, and one `sub_port_config` fully
specifies how that inner call must be made: which construction method to
use, and the parameters of that method.

## Details

This is the shared configuration type for every layered method.
`mmaf_sub_port_config` extends it without adding structure and is kept
as the name used by `mmaf_parameters`.

Only the parameter object matching `port_construction_method` is used.
The remaining parameter slots are ignored, so a configuration carrying,
say, `rp_parameters` while `port_construction_method` is `"ew"` is valid
but inert.

## Slots

- `port_construction_method`:

  A character string indicating the method used for constructing the
  sub-portfolio. Must be one of 'ew', 'sw', 'cw', 'cs', 'rp', 'hrp' or
  'mvo'.

- `mvo_parameters`:

  An object of class `mvo_parameters` representing the parameters for
  mean-variance optimization. This is only relevant for 'mvo'.

- `rp_parameters`:

  An object of class `rp_parameters` representing the parameters for
  risk parity. This is only relevant for 'rp'.

- `hrp_parameters`:

  An object of class `hrp_parameters` representing the parameters for
  hierarchical risk parity. This is only relevant for 'hrp'.
