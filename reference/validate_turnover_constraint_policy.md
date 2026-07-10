# Validate Turnover Constraint Policy

Internal function to validate the turnover constraint policy.

## Usage

``` r
validate_turnover_constraint_policy(turnover_constraint_policy)
```

## Arguments

- turnover_constraint_policy:

  A list with the elements described above.

## Value

Invisibly returns TRUE if the policy is valid.

## Details

The policy is provided as a list that must contain the following
elements:

- quantile_range_buffer:

  A numeric value between 0 and 1 that defines the buffer for turnover
  quantiles.

- turnover_cap_rules:

  A named numeric vector whose names correspond to valid liquidity
  categories ("micro_caps", "small_caps", "mid_caps", "large_caps",
  "mega_caps"). Each value must be between 0 and 1. Additionally, for
  any two categories, if one category is less liquid than another then
  its cap must not exceed that of the more liquid category.
