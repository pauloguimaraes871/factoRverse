# Create Turnover Constraint Policy

Constructor for a `turnover_constraint_policy` object.

## Usage

``` r
create_turnover_constraint_policy(
  quantile_range_buffer,
  turnover_cap_rules = NULL
)
```

## Arguments

- quantile_range_buffer:

  A numeric value indicating the increase in the quantile range for
  buffer zones.

- turnover_cap_rules:

  A named numeric vector indicating turnover cap rules.

## Value

An S4 object of class `turnover_constraint_policy`.
