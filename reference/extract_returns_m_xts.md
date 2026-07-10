# Extract Returns from port_backtest_cohort Object

This method extracts the `net_returns_m_xts` slot from the
`port_backtest_cohort` object, optionally simplifying the column names
based on the configuration names of the cohort.

## Usage

``` r
extract_returns_m_xts(
  port_backtest_cohort,
  signals_m_df,
  benchmark_returns_m_xts,
  simplify_name = TRUE,
  verbose = TRUE
)
```

## Arguments

- port_backtest_cohort:

  An object of class `port_backtest_cohort`.

- signals_m_df:

  An object of class `signals_m_df` containing the signals used in the
  backtest.

- benchmark_returns_m_xts:

  An object of class `meta_xts` containing the benchmark returns used in
  the backtest.

- simplify_name:

  Logical. If `TRUE`, the column names will be simplified using the
  configuration names.

- verbose:

  Logical. If `TRUE`, messages will be printed about the simplification
  process.

## Value

An `xts` object containing the extracted backtest returns.
