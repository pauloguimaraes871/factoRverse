# as.list Method for transaction_costs_parameters S4 Class

Converts a transaction_costs_parameters S4 object to a list with
elements `direct_transaction_cost`, `alpha`, `lambda` and
`strategy_aum`.

## Usage

``` r
# S4 method for class 'transaction_costs_parameters'
as.list(x, ...)
```

## Arguments

- x:

  A transaction_costs_parameters S4 object.

- ...:

  Additional arguments (unused).

## Value

A list with elements `quantile_range_buffer` and `turnover_cap_rules`.
