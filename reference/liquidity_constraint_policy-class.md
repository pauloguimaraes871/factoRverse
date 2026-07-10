# Liquidity Constraint Policy

An S4 class to represent a liquidity constraint policy in portfolio
construction.

## Slots

- `liquidity_floor_rule`:

  A character indicating the minimum liquidity classification for an
  asset to be considered eligible. Should be one of 'micro_caps',
  'small_caps', 'mid_caps', 'large_caps' or 'mega_caps'. This constraint
  will work even if port_construction_method is not 'mvo'. It can be
  added via add_liquidity_floor_rule function.

- `liquidity_cap_rules`:

  A named vector in which each element is a value indicating the maximum
  (active) weight that can be assigned to assets with the classification
  specified by its name. The names should be one of 'micro_caps',
  'small_caps', 'mid_caps', 'large_caps' or 'mega_caps'. A
  liquidity_constraint_policy can contain as many liquidity_cap_rules as
  needed, but names can't be repeated and a less liquid asset can't have
  a higher cap than a more liquid one. New cap rules can be added
  through the add_liquidity_cap_rule function.
