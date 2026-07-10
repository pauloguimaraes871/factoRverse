# Turnover Constraint Policy

An S4 class to represent a turnover constraint policy in portfolio
construction.

## Slots

- `quantile_range_buffer`:

  A numeric indicating the increase in the quantile_range for an asset
  to be considered eligible to the buffer zone. Stocks in this quantile
  that were present in bop_port_weights will be included in the buffer
  zone, if they meet buffer_zones_rules.

- `turnover_cap_rules`:

  A named vector in which each element is a value indicating the maximum
  absolute weight deviation in relation to the bop_port_weights that can
  be assigned to assets, with the classification specified by its name.
  The names should be one of 'micro_caps', 'small_caps', 'mid_caps',
  'large_caps' or 'mega_caps'. A turnover_constraint_policy can contain
  as many buffer_zone_rules as needed, but names can't be repeated and a
  less liquid asset can't have a higher cap than a more liquid one. New
  cap rules can be added through the add_turnover_cap_rule function.
