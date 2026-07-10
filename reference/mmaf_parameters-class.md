# Define the `mmaf_parameters` S4 Class

S4 class to represent a set of configurations for the MMAF portfolio
construction method.

## Slots

- `mmaf_method`:

  A character indicating the MMAF method to be used. Must be one of
  'top_down' or 'bottom_up'.

- `top_down_proxy_port_method`:

  A character indicating the method to be used for constructing the
  top-down proxy portfolio. Must be one of 'ew', 'rp', 'hrp', 'cs' or
  'sw' if mmaf_method is 'top_down' and NULL if mmaf_method is
  'bottom_up'.

- `mmaf_group_col`:

  A character string representing the MMAF group to which the assets
  belong. This is used to group assets when constructing micro and macro
  portfolios. It must be a length 1 character.

- `micro_port_config`:

  An object of class `mmaf_sub_port_config` representing the
  configuration for constructing micro portfolios.

- `macro_port_config`:

  An object of class `mmaf_sub_port_config` representing the
  configuration for constructing macro portfolios.
