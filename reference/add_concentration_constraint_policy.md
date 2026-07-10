# Add Concentration Constraint Policy

Either add an existing `concentration_constraint_policy` to an object
(e.g., `port_backtest_config` or `sb_backtest_config`), or create one
dynamically when `policy` is missing.

## Usage

``` r
add_concentration_constraint_policy(object, policy, ...)

# S4 method for class 'port_backtest_config,concentration_constraint_policy'
add_concentration_constraint_policy(object, policy, ...)

# S4 method for class 'port_backtest_config,missing'
add_concentration_constraint_policy(
  object,
  policy,
  max_abs_active_individual_weight = NULL,
  max_abs_active_group_weight = NULL,
  ...
)

# S4 method for class 'sb_backtest_config,concentration_constraint_policy'
add_concentration_constraint_policy(object, policy, ...)

# S4 method for class 'sb_backtest_config,missing'
add_concentration_constraint_policy(
  object,
  policy,
  benchmark,
  max_abs_active_individual_weight = NULL,
  max_abs_active_group_weight = NULL,
  ...
)
```

## Arguments

- object:

  An object of class `port_backtest_config` or `sb_backtest_config`.

- policy:

  A `concentration_constraint_policy` object, or missing if a new one is
  to be created.

- ...:

  Additional arguments used to create a new
  `concentration_constraint_policy` if `policy` is missing. These
  typically include:

  - **benchmark** (character)

  - **max_abs_active_individual_weight** (numeric)

  - **max_abs_active_group_weight** (named numeric)

- max_abs_active_individual_weight:

  A numeric indicating the max absolute active weight for individual
  assets.

- max_abs_active_group_weight:

  A named numeric vector for group constraints.

- benchmark:

  A character vector indicating the benchmark to be used.

## Value

The updated `object` with the concentration policy added.

## Functions

- `add_concentration_constraint_policy( object = port_backtest_config, policy = concentration_constraint_policy )`:
  Add an existing `concentration_constraint_policy` to a
  `port_backtest_config`.

- `add_concentration_constraint_policy( object = port_backtest_config, policy = missing )`:
  Dynamically create a `concentration_constraint_policy` and add it to a
  `port_backtest_config`.

- `add_concentration_constraint_policy( object = sb_backtest_config, policy = concentration_constraint_policy )`:
  Add an existing `concentration_constraint_policy` to a
  `sb_backtest_config`. This method will store it inside
  `object@signal_port_parameters`.

- `add_concentration_constraint_policy( object = sb_backtest_config, policy = missing )`:
  Dynamically create a `concentration_constraint_policy` for
  `sb_backtest_config`.
