# Concentration Constraint Policy

An S4 class to represent a concentration constraint policy in portfolio
construction.

## Slots

- `benchmark`:

  A character vector indicating which benchmark(s) to use. For stocks,
  must be a column in benchmarks_m_df. For signals, must be theme_ss or
  theme_sb

- `max_abs_active_individual_weight`:

  A numeric value indicating the maximum absolute active weight for
  individual assets.

- `max_abs_active_group_weight`:

  A **named** numeric vector indicating maximum absolute group weights
  in relation to the benchmark. Names should match columns in a
  groups_m_df.
