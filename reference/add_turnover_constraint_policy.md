# Add Turnover Constraint Policy

Add an existing or dynamically create a `turnover_constraint_policy` to
a portfolio backtest configuration object.

## Usage

``` r
add_turnover_constraint_policy(object, policy, ...)

# S4 method for class 'port_backtest_config,turnover_constraint_policy'
add_turnover_constraint_policy(object, policy, ...)

# S4 method for class 'port_backtest_config,missing'
add_turnover_constraint_policy(
  object,
  policy,
  quantile_range_buffer,
  turnover_cap_rules,
  ...
)
```

## Arguments

- object:

  An object of class `port_backtest_config` or `sb_backtest_config`.

- policy:

  A `turnover_constraint_policy` object. If missing, a new one is
  created.

- ...:

  Additional arguments (currently unused).

- quantile_range_buffer:

  A numeric value used to create a new policy when `policy` is missing.

- turnover_cap_rules:

  A named numeric vector used to create a new policy when `policy` is
  missing.

## Value

The updated `object` with the turnover constraint policy added.

## Functions

- `add_turnover_constraint_policy( object = port_backtest_config, policy = turnover_constraint_policy )`:
  Add an existing `turnover_constraint_policy` to a
  `port_backtest_config`.

- `add_turnover_constraint_policy(object = port_backtest_config, policy = missing)`:
  Dynamically create a `turnover_constraint_policy` and add it to a
  `port_backtest_config`.
