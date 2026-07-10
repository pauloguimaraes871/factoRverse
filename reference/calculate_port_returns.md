# Calculate Portfolio Returns

This function calculates the portfolio's forward returns, active
returns, net returns, and turnover, given stock- and benchmark-level
forward returns, transactions information, and cost data.

## Usage

``` r
calculate_port_returns(
  clean_fwd_return_1m_m_d_ref,
  fwd_selected_benchmark_return,
  port_weights_m_d_ref,
  total_cost,
  verbose = TRUE
)
```

## Arguments

- clean_fwd_return_1m_m_d_ref:

  A data frame containing forward stock returns. Must include a column
  named `id` (or an equivalent identifier) and a column named
  `fwd_return_1m` for returns.

- fwd_selected_benchmark_return:

  Numeric. The forward return of the selected benchmark.

- port_weights_m_d_ref:

  A data frame containing portfolio weights. Must include an `id` column
  (for the join) and an `eop_port_weights` column (end-of-period weights
  used to weight forward returns).

- total_cost:

  Numeric. The total cost associated with the portfolio.

- verbose:

  Logical. If `TRUE`, messages will be printed about the calculation
  process.

## Value

A one-row data frame (`fwd_port_returns_d_ref`) with columns
`fwd_raw_return` and `fwd_net_return`. When
`fwd_selected_benchmark_return` is supplied, it also includes
`fwd_selected_bench_return`, `fwd_raw_active_return`, and
`fwd_net_active_return`.

## Details

The function joins `port_weights_m_d_ref` with
`clean_fwd_return_1m_m_d_ref` by `id` and computes:

- **Raw Return:** Weighted sum of `fwd_return_1m` using
  `eop_port_weights` (NA returns are ignored).

- **Net Return:** Raw Return minus `total_cost`.

- **Raw Active Return:** Raw Return minus the benchmark forward return
  (only when `fwd_selected_benchmark_return` is supplied).

- **Net Active Return:** Net Return minus the benchmark forward return
  (only when `fwd_selected_benchmark_return` is supplied).
