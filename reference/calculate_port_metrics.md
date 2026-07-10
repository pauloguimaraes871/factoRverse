# Calculate Consolidated Portfolio and Benchmark Metrics

This function joins custom stock metrics to portfolio allocation data
and computes consolidated portfolio metrics by calculating the weighted
sum of each metric column. Optionally, if benchmark weights are provided
(i.e. if `selected_benchmark` is not `NULL`), the function also computes
benchmark metrics and merges them with the portfolio metrics.

## Usage

``` r
calculate_port_metrics(port_weights_m_d_ref, custom_stock_metrics_m_d_ref)
```

## Arguments

- port_weights_m_d_ref:

  A data frame containing portfolio weights. Must include an `id` column
  and an `eop_port_weights` column (end-of-period weights). May
  optionally include a `bench_weights` column, in which case
  benchmark-weighted metrics (prefixed `bench_`) are also computed and
  merged.

- custom_stock_metrics_m_d_ref:

  A data frame of custom per-stock metrics. Must include an `id` column
  (used for the join) plus one or more numeric metric columns;
  `tickers`/`dates`, if present, are ignored.

## Value

A data frame with consolidated portfolio metrics. If benchmark metrics
are calculated, the data frame will also include columns with a
`"bench_"` prefix.
