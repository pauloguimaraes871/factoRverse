# Calculate Transaction Costs (Including Market Impact)

Estimates total transaction costs for a set of trades, incorporating
both direct and market impact costs.

## Usage

``` r
calculate_transaction_costs(
  transactions_m_d_ref,
  alpha,
  lambda,
  direct_transaction_cost,
  strategy_aum,
  verbose
)
```

## Arguments

- transactions_m_d_ref:

  A data frame containing transaction data. Must include the columns:
  `order`, `relative_order_size`, `daily_vol`, and `delta` (the last
  used to compute turnover).

- alpha:

  Numeric. Scaling factor for market impact cost.

- lambda:

  Either a numeric value or the string `"dynamic"`. When `"dynamic"`,
  the lambda value varies based on the relative order size.

- direct_transaction_cost:

  Numeric. Direct transaction cost applied per trade (e.g., 0.0005 for
  0.05%).

- strategy_aum:

  Numeric. The total assets under management for the strategy.

- verbose:

  Logical. If `TRUE`, prints detailed transaction cost information.

## Value

A named list with two components:

- `transactions_and_costs_m_d_ref`:

  The input transactions augmented with per-trade `alpha`, `lambda`,
  `direct_cost`, `market_impact_cost` and `total_cost` columns.

- `port_costs_d_ref`:

  A one-row data frame with portfolio-level `direct_cost`,
  `market_impact_cost`, `total_cost` and `turnover`.

## Details

The function computes two cost components: a direct cost based on the
trade size and a market impact cost based on the relative order size and
volatility.

When `lambda = "dynamic"`, lambda is set according to the relative order
size, using predefined thresholds.
