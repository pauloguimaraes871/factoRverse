# MMAF Sub Portfolio Configuration

An S4 class to represent the configuration of micro or macro portfolios
in the MMAF portfolio construction method.

## Details

Extends
[sub_port_config](https://pauloguimaraes871.github.io/factoRverse/reference/sub_port_config-class.md)
without adding slots or validity rules. It exists so that MMAF
configurations keep a distinct, self-documenting class name while
sharing a single configuration contract with every other layered (`*af`)
method.

## Slots

- `port_construction_method`:

  A character string indicating the method used for constructing micro
  portfolios.

- `mvo_parameters`:

  An object of class `mvo_parameters` representing the parameters for
  mean-variance optimization. This is only relevant for 'mvo'.

- `rp_parameters`:

  An object of class `rp_parameters` representing the parameters for
  risk parity. This is only relevant for 'rp'.

- `hrp_parameters`:

  An object of class `hrp_parameters` representing the parameters for
  hierarchical risk parity. This is only relevant for 'hrp'.
