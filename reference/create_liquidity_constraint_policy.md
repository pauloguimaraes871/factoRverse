# Create Liquidity Constraint Policy

Constructor for a `liquidity_constraint_policy` object.

## Usage

``` r
create_liquidity_constraint_policy(
  liquidity_floor_rule = NULL,
  liquidity_cap_rules = NULL
)
```

## Arguments

- liquidity_floor_rule:

  A character string indicating the minimum liquidity classification.

- liquidity_cap_rules:

  A named numeric vector indicating liquidity cap rules.

## Value

An S4 object of class `liquidity_constraint_policy`.
