# Create Concentration Constraint Policy

Constructor for a `concentration_constraint_policy` object.

## Usage

``` r
create_concentration_constraint_policy(
  benchmark = character(0),
  max_abs_active_individual_weight = NA_real_,
  max_abs_active_group_weight = numeric(0)
)
```

## Arguments

- benchmark:

  A character vector (can be empty if no benchmark specified).

- max_abs_active_individual_weight:

  A numeric indicating the max absolute active weight for individual
  assets.

- max_abs_active_group_weight:

  A named numeric vector for group constraints.

## Value

An S4 object of class `concentration_constraint_policy`.
