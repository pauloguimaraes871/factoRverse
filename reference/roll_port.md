# Roll Portfolio Weights and Calculate Forward Returns

This function calculates forward portfolio returns and rolls the current
portfolio weights to the next period based on forward one-month stock
returns. It filters the target return data to remove missing values,
computes net portfolio returns (using a helper function), and then rolls
the portfolio weights (using another helper function).

## Usage

``` r
roll_port(
  fwd_return_m_d_ref,
  fwd_selected_benchmark_return,
  port_weights_m_d_ref,
  total_cost,
  verbose
)
```

## Arguments

- fwd_return_m_d_ref:

  A data frame containing stock return information with at least the
  columns: `id`, `tickers`, `dates`, and `fwd_return_1m` (forward
  1-month returns).

- fwd_selected_benchmark_return:

  The forward return of the selected benchmark. This can be a numeric
  value or a data structure as required by the helper functions.

- port_weights_m_d_ref:

  A data frame of current portfolio weights.

- total_cost:

  A numeric value representing the total transaction cost.

- verbose:

  Logical; if `TRUE`, progress messages will be printed.

## Value

A list with two components:

- `rolled_fwd_port_weights_m_d_ref`: A data frame of portfolio weights
  rolled forward to the next period.

- `fwd_port_returns_d_ref`: A data frame (or numeric) containing the net
  forward portfolio returns.

## Details

The function follows these steps:

1.  Extracts the forward 1-month stock returns from
    `fwd_return_m_d_ref`, dropping any rows with missing values.

2.  If there are valid returns (i.e., at least one row remains), it
    prints a message (if `verbose` is `TRUE`), calculates the net
    portfolio return using
    [`calculate_port_returns()`](https://pauloguimaraes871.github.io/factoRverse/reference/calculate_port_returns.md),
    and then rolls the portfolio weights to the next period using
    [`roll_fwd_port_weights()`](https://pauloguimaraes871.github.io/factoRverse/reference/roll_fwd_port_weights.md).

3.  If no valid forward returns are available, both the returns and
    rolled weights are set to `NULL`.
