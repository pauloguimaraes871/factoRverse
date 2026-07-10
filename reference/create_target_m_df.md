# Build target_m_df for forward horizons

Generate a `target_m_df` containing forward-return targets (and optional
active returns) aligned to the rows of a features/metafeatures
`meta_dataframe`. This routine is the canonical helper used by the
package to build prediction targets for signal-blending and meta-model
training.

## Usage

``` r
create_target_m_df(
  daily_returns_m_df,
  daily_bench_returns_m_xts,
  features_m_df,
  ...
)

# S4 method for class 'meta_dataframe,meta_xts,meta_dataframe'
create_target_m_df(
  daily_returns_m_df,
  daily_bench_returns_m_xts,
  features_m_df,
  past_ret_column,
  selected_bench,
  fwd_horizon,
  active_returns,
  parallel = TRUE
)
```

## Arguments

- daily_returns_m_df:

  A `meta_dataframe` containing daily (or higher-frequency) asset
  returns. Must cover a date range that includes the forward windows
  required by `fwd_horizon`.

- daily_bench_returns_m_xts:

  A `meta_xts` of benchmark returns (time-series indexed by Date). A
  column specified by `selected_bench` will be used to compute active
  returns and to fill NAs for delisted series when appropriate.

- features_m_df:

  A `meta_dataframe` whose rows (ids) define the observations for which
  targets will be produced. Targets are computed for each
  `features_m_df@data$id` / date pair.

- ...:

  Additional arguments forwarded (internal use).

- past_ret_column:

  Character scalar. Name of the return column inside
  `daily_returns_m_df@data` that holds realized returns (e.g. `"ret"`).

- selected_bench:

  Character scalar. Column name in `daily_bench_returns_m_xts` to use as
  the benchmark.

- fwd_horizon:

  Integer. Forward horizon in months for the target (e.g., `3` for
  3-month forward).

- active_returns:

  Logical. If `TRUE` the returned target is active (asset return minus
  benchmark return).

- parallel:

  Logical. If `TRUE` uses furrr for parallelizing per-date processing;
  otherwise processes serially.

## Value

An S4 object of class `target_m_df`. The `data` slot is a long
`data.frame` with the canonical first-three columns `id`, `tickers`,
`dates` and one or more forward-target metric columns named like
`fwd_<metric>_<Nhorizon>m`. The `workflow` slot is appended with a
record describing the call and parameters used to create the targets.

## Details

Create a target_m_df (prediction targets for backtests and meta-models)

- For each row in `features_m_df`, the function locates the closest or
  matching date in `daily_returns_m_df` and aggregates returns across
  the forward window (days from next-day through end of the horizon
  window).

- Missing forward returns are conservatively replaced with benchmark
  returns only when appropriate (e.g., trailing NAs caused by
  delisting). The helper checks that NA patterns are trailing blocks for
  each series; violations stop with an error.

- If any forward dates exceed `daily_returns_m_df@current_date` the
  corresponding target rows are returned with NA values.

- Uses
  [`create_meta_xts()`](https://pauloguimaraes871.github.io/factoRverse/reference/create_meta_xts.md)
  and
  [`summarize_performance()`](https://pauloguimaraes871.github.io/factoRverse/reference/summarize_performance.md)
  internally to compute summary forward metrics (alpha, IR, active
  returns, etc.) and then converts those summaries into a `target_m_df`
  (naming columns with the horizon suffix).

- Parallel execution uses furrr with reproducible seeding when
  available.

## Functions

- `create_target_m_df( daily_returns_m_df = meta_dataframe, daily_bench_returns_m_xts = meta_xts, features_m_df = meta_dataframe )`:
  Method implementation for meta_dataframe inputs

  Method signature: `daily_returns_m_df = "meta_dataframe"`,
  `daily_bench_returns_m_xts = "meta_xts"`,
  `features_m_df = "meta_dataframe"`.

## Input validation (as exercised by tests)

- `features_m_df@current_date` and `daily_returns_m_df@current_date`
  must be compatible (tests expect same current_date).

- All tickers referenced in `features_m_df` must exist in
  `daily_returns_m_df`; otherwise an informative error is thrown.

- Benchmark column `selected_bench` must exist in
  `daily_bench_returns_m_xts`.

- `fwd_horizon` must be \>= 1.

## See also

[`target_m_df-class`](https://pauloguimaraes871.github.io/factoRverse/reference/target_m_df-class.md),
[`create_meta_dataframe`](https://pauloguimaraes871.github.io/factoRverse/reference/create_meta_dataframe.md),
[`run_sb_backtest`](https://pauloguimaraes871.github.io/factoRverse/reference/run_sb_backtest.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# daily_returns_m_df and features_m_df are meta_dataframe objects;
# daily_bench_returns_m_xts is a meta_xts
tgt <- create_target_m_df(
  daily_returns_m_df = daily_returns_m_df,
  daily_bench_returns_m_xts = daily_bench_returns_m_xts,
  features_m_df = features_m_df,
  past_ret_column = "ret",
  selected_bench = "ibov",
  fwd_horizon = 3,
  active_returns = TRUE,
  parallel = FALSE
)

# Returned object is of class 'target_m_df'
class(tgt)
head(tgt@data)
} # }
```
