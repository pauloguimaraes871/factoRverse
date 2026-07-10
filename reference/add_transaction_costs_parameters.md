# Add Transaction Cost Parameters to a Portfolio Backtest Configuration

This generic function adds an existing or dynamically creates a
`transaction_cost_parameters` object to a portfolio backtest
configuration object.

## Usage

``` r
add_transaction_costs_parameters(object, transaction_costs_parameters, ...)

# S4 method for class 'port_backtest_config,transaction_costs_parameters'
add_transaction_costs_parameters(object, transaction_costs_parameters, ...)

# S4 method for class 'port_backtest_config,missing'
add_transaction_costs_parameters(object, transaction_costs_parameters, ...)
```

## Arguments

- object:

  An object of class `port_backtest_config`.

- transaction_costs_parameters:

  A `transaction_cost_parameters` object. If missing, a new one is
  created.

- ...:

  Additional arguments used when creating a new transaction cost
  parameters object.

## Value

The updated `object` with the transaction cost parameters added.

## Functions

- `add_transaction_costs_parameters( object = port_backtest_config, transaction_costs_parameters = transaction_costs_parameters )`:
  Add an existing `transaction_cost_parameters` to a
  `port_backtest_config`.

- `add_transaction_costs_parameters( object = port_backtest_config, transaction_costs_parameters = missing )`:
  Dynamically create a `transaction_cost_parameters` object and add it
  to a `port_backtest_config`.

  Additional arguments (such as `direct_transaction_cost`,
  `strategy_aum`, `alpha`, and `lambda`) are passed to
  `new_transaction_cost_parameters`.
