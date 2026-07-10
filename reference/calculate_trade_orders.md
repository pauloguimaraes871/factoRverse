# Calculate Trade Orders

This function calculates the required trades to move from a
beginning-of-period (BOP) portfolio weight to an end-of-period (EOP)
portfolio weight, given liquidity and volatility data.

## Usage

``` r
calculate_trade_orders(
  merged_port_results_list,
  updated_port_weights_m_lstd_ref,
  liquidity_m_d_ref,
  volatility_m_d_ref,
  main_liquidity_metric,
  strategy_aum,
  verbose = TRUE
)
```

## Arguments

- merged_port_results_list:

  A list of results from merge_and_rescale_weights.

- updated_port_weights_m_lstd_ref:

  A data frame containing the updated portfolio weights; must have a
  column named `tickers`.

- liquidity_m_d_ref:

  data frame containing liquidity information; must have a column
  matching `main_liquidity_metric`.

- volatility_m_d_ref:

  data frame containing volatility information; must contain
  `daily_vol`.

- main_liquidity_metric:

  character string naming the column in `liquidity_m_d_ref` with
  liquidity data.

- strategy_aum:

  numeric, the AUM used to size trades.

- verbose:

  logical. If TRUE, prints IPO/delisting info to console.

## Value

A data frame with one row per ticker in the union of the current and
previous universes, including `tickers`, `bop_port_weights`,
`eop_port_weights`, `delta` (EOP minus BOP), `order` (delta scaled by
`strategy_aum`), `relative_order_size` (order relative to the liquidity
metric), an `obs` tag marking each ticker as `"none"`, `"delisted"` or
`"IPO"`, and the joined liquidity/volatility columns (delisted names
receive imputed liquidity/volatility so costs can still be computed).
