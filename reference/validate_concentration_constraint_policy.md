# Validate Concentration Constraint Policy

Internal function to validate the structure and values of the
concentration constraint policy.

## Usage

``` r
validate_concentration_constraint_policy(
  concentration_constraint_policy,
  macro = FALSE
)
```

## Arguments

- concentration_constraint_policy:

  A list with the following possible elements:

  benchmark

  :   Benchmark weights (not validated by this function).

  max_abs_active_individual_weight

  :   A numeric value in (0, 1\] representing the maximum absolute
      active weight for an individual asset.

  max_abs_active_group_weight

  :   A named numeric vector in (0, 1\] representing the maximum
      absolute active weight for groups of assets. Names must be unique.

  @param macro If true, indicates concentration_constraint_policy is for
  a macro portfolio.

## Value

Invisibly returns TRUE if the policy is valid.
