# Transaction Cost Parameters S4 Class

This S4 class stores transaction cost parameters based on the BARRA
model.

## Slots

- `direct_transaction_cost`:

  A numeric value representing the direct transaction cost (ie,
  brokerage fees). Should be in percentage (0.07 = 0.07%) .

- `strategy_aum`:

  A numeric value representing the strategy's assets under management
  (AUM). Should be in same units as main_liquidity_metrics

- `alpha`:

  A numeric value representing the alpha parameter.

- `lambda`:

  A numeric value or the string "dynamic" representing the lambda
  parameter.
