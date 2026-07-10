# Add Liquidity Constraint Policy

Add an existing or dynamically create a `liquidity_constraint_policy` to
a portfolio backtest configuration object.

## Usage

``` r
add_liquidity_constraint_policy(object, policy, ...)

# S4 method for class 'port_backtest_config,liquidity_constraint_policy'
add_liquidity_constraint_policy(object, policy, ...)

# S4 method for class 'port_backtest_config,missing'
add_liquidity_constraint_policy(
  object,
  policy,
  liquidity_floor_rule = NULL,
  liquidity_cap_rules = NULL,
  ...
)
```

## Arguments

- object:

  An object of class `port_backtest_config`.

- policy:

  A `liquidity_constraint_policy` object. If missing, a new one is
  created.

- ...:

  Additional arguments (currently unused).

- liquidity_floor_rule:

  A character string (see details) used to create a new policy when
  `policy` is missing.

- liquidity_cap_rules:

  A named numeric vector used to create a new policy when `policy` is
  missing.

## Value

The updated `object` with the liquidity constraint policy added.

## Functions

- `add_liquidity_constraint_policy( object = port_backtest_config, policy = liquidity_constraint_policy )`:
  Add an existing `liquidity_constraint_policy` to a
  `port_backtest_config`.

- `add_liquidity_constraint_policy( object = port_backtest_config, policy = missing )`:
  Dynamically create a `liquidity_constraint_policy` and add it to a
  `port_backtest_config`.
