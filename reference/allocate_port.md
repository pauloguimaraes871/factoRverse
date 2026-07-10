# Allocate Portfolio and Compute Transaction Costs

This function performs several steps to allocate a portfolio by merging
and rescaling portfolio weights, calculating trade orders, computing
transaction costs, and enhancing the portfolio allocation log with
additional metrics and (optionally) benchmark weights.

## Usage

``` r
allocate_port(
  port_weights_placeholder_m_d_ref,
  updated_port_weights_m_lstd_ref,
  stock_universe_m_d_ref,
  liquidity_m_d_ref,
  volatility_m_d_ref,
  main_liquidity_metric = main_liquidity_metric,
  transaction_costs_parameters,
  selected_benchmark_weights_m_d_ref,
  verbose = TRUE
)
```

## Arguments

- port_weights_placeholder_m_d_ref:

  A data frame containing the placeholder portfolio weights.

- updated_port_weights_m_lstd_ref:

  A data frame containing the updated portfolio weights.

- stock_universe_m_d_ref:

  A data frame representing the stock universe.

- liquidity_m_d_ref:

  A data frame containing liquidity metrics.

- volatility_m_d_ref:

  A data frame containing volatility metrics.

- main_liquidity_metric:

  The name (or indicator) of the primary liquidity metric to use. By
  default, it is expected to be defined externally if not explicitly
  passed.

- transaction_costs_parameters:

  A list of transaction cost parameters. Expected elements are:

  - `strategy_aum` - Strategy Assets Under Management.

  - `alpha` - Parameter for indirect transaction cost calculation.

  - `lambda` - Parameter for indirect transaction cost calculation.

  - `direct_transaction_cost` - Direct transaction cost value.

- selected_benchmark_weights_m_d_ref:

  (Optional) A data frame containing benchmark weights. If provided,
  these weights will be merged into the portfolio allocation log and
  missing values set to 0.

- verbose:

  Logical indicating whether to print additional messages (default is
  `TRUE`).

## Value

A list with the following components:

- `transactions_log_m_d_ref` - The transaction-and-costs log (from
  [`calculate_transaction_costs()`](https://pauloguimaraes871.github.io/factoRverse/reference/calculate_transaction_costs.md)):
  per-ticker orders enriched with direct, market-impact and total costs
  (and benchmark weights, if provided).

- `port_weights_m_d_ref` - The merged and rescaled end-of-period
  portfolio weights.

- `port_costs_d_ref` - The one-row portfolio transaction-cost summary
  (direct cost, market impact, total cost, turnover).

## Details

The function performs the following steps:

1.  **Initial Prep:** Extracts transaction cost parameters from
    `transaction_costs_parameters`.

2.  **Merge Portfolio Weights:** Uses
    [`merge_and_rescale_weights()`](https://pauloguimaraes871.github.io/factoRverse/reference/merge_and_rescale_weights.md)
    to merge placeholder and updated weights, ensuring alignment with
    the stock universe.

3.  **Calculate Transactions:** Computes trade orders using
    [`calculate_trade_orders()`](https://pauloguimaraes871.github.io/factoRverse/reference/calculate_trade_orders.md)
    with liquidity and volatility data.

4.  **Compute Transaction Costs:** Uses
    [`calculate_transaction_costs()`](https://pauloguimaraes871.github.io/factoRverse/reference/calculate_transaction_costs.md)
    to determine both indirect and direct costs.

5.  **Build Transaction Log:** Extracts the enriched
    transactions-and-costs data frame (`transactions_log_m_d_ref`) from
    the cost results. Benchmark weights, when
    `selected_benchmark_weights_m_d_ref` is supplied, are already merged
    during the merge step (2) and carried through in
    `port_weights_m_d_ref` and the transaction log.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Define input objects (for example, using your actual data frames)
  result <- allocate_port(
    port_weights_placeholder_m_d_ref = port_weights_placeholder,
    updated_port_weights_m_lstd_ref = updated_port_weights,
    stock_universe_m_d_ref = stock_universe,
    liquidity_m_d_ref = liquidity_data,
    volatility_m_d_ref = volatility_data,
    main_liquidity_metric = "liquidity_metric",
    transaction_costs_parameters = list(
      strategy_aum = 1000000,
      alpha = 0.01,
      lambda = 0.02,
      direct_transaction_cost = 0.005
    ),
    selected_benchmark_weights_m_d_ref = benchmark_weights,
    verbose = TRUE
  )

  # Access the transaction-and-costs log:
  allocation_log <- result$transactions_log_m_d_ref

  # Print the allocation log using cat and paste:
  cat(paste(capture.output(print(allocation_log)), collapse = "\n"))
} # }
```
