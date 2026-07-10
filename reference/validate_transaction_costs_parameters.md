# Validate Transaction Cost Parameters

This function validates a list of transaction cost parameters based on
the BARRA model. It checks that the list contains the names:
"direct_transaction_cost", "strategy_aum", "alpha", and "lambda".
Additionally, it ensures that:

- `direct_transaction_cost` is a single numeric value and positive.

- `strategy_aum` is a single numeric value and positive.

- `alpha` is a single numeric value and positive.

- `lambda` is a single value that is either numeric or exactly the
  string "dynamic".

## Usage

``` r
validate_transaction_costs_parameters(transaction_costs_parameters)
```

## Arguments

- transaction_costs_parameters:

  A list containing transaction cost parameters.

## Value

TRUE if all validations pass; otherwise, the function stops with an
error.
