# Get the Concentration Constraint Policy

Accessor method to extract the `concentration_constraint_policy` from an
object.

## Usage

``` r
get_concentration_constraint_policy(object)

# S4 method for class 'port_backtest_config'
get_concentration_constraint_policy(object)

# S4 method for class 'sb_backtest_config'
get_concentration_constraint_policy(object)
```

## Arguments

- object:

  An object of class `port_backtest_config` or `sb_backtest_config`.

## Value

An S4 object of class `concentration_constraint_policy`.

## Functions

- `get_concentration_constraint_policy(port_backtest_config)`: Extract
  the concentration policy from `port_backtest_config`.

- `get_concentration_constraint_policy(sb_backtest_config)`: Extract the
  concentration policy from `sb_backtest_config`, which stores it inside
  `object@signal_port_parameters`.
