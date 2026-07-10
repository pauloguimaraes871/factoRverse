# Create Portfolio Backtest Cohort

This function creates a `port_backtest_cohort` object by merging a list
of `port_backtest_results` objects. It checks for compatibility across
backtests by verifying that key elements of the `port_backtest_workflow`
slot are identical.

## Usage

``` r
create_port_backtest_cohort(port_backtest_results_list, cohort_name)
```

## Arguments

- port_backtest_results_list:

  A list of `port_backtest_results` objects.

- cohort_name:

  A character string representing the name for the merged cohort.

## Value

An object of class `port_backtest_cohort`.

## Details

The function then merges:

- **port_weights_m_df:** Merges the underlying meta data.frames
  (accessed via `@data`) by matching `id`, `tickers` and `dates`. The
  individual `eop_port_weights` columns are renamed to the corresponding
  backtest identifier. If a benchmark was used (i.e.
  `selected_benchmark` is not `NULL`), the common `bench_weights` column
  is checked for consistency and included only once.

- **port_costs_m_xts:** For each cost type (direct_cost,
  market_impact_cost, total_cost, turnover), the function extracts the
  respective column from each backtest’s `@port_costs_m_xts@data`,
  renames it to the backtest identifier, and then merges them (after
  checking that dates match).

- **port_returns_m_xts:** Separately merges columns for raw_return,
  net_return, raw_active_return, and net_active_return. For raw_return
  and net_return, if a benchmark is present the common
  `selected_bench_return` column is included after checking consistency.

- **port_metrics_m_xts:** As the metrics may be built from custom stock
  metrics, the function iterates over the primary metric columns (i.e.
  those not starting with `bench_`) and merges them across backtests. If
  a corresponding bench column exists (and benchmarks are used) and is
  identical across backtests, it is included once.

After merging, the function uses `create_meta_dataframe` and
`create_meta_xts` (from your package) to reconstruct the meta objects.
The resulting `port_backtest_cohort` object contains the merged data and
the common backtest workflow parameters.
