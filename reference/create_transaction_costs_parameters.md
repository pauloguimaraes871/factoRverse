# Create a New Transaction Cost Parameters Object

This function constructs a new `transaction_cost_parameters` S4 object.

## Usage

``` r
create_transaction_costs_parameters(
  direct_transaction_cost,
  strategy_aum,
  alpha,
  lambda
)
```

## Arguments

- direct_transaction_cost:

  A numeric value for the direct transaction cost.

- strategy_aum:

  A numeric value for the strategy's assets under management.

- alpha:

  A numeric value for the alpha parameter.

- lambda:

  A numeric value or the string "dynamic" for the lambda parameter.

## Value

An object of class `transaction_cost_parameters`.
