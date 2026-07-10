# weights_m_df-class

`weights_m_df` stores per-asset weight columns (benchmarks, strategy
weights, etc.) aligned with the canonical `meta_dataframe` layout (rows
= asset-date observations). It enforces numeric weight bounds and
performs per-date checks that group sums are approximately 1 (with
tolerance).

## Details

An S4 subclass of `meta_dataframe` representing weight matrices used for
portfolio construction.

This class is intended as the canonical container for portfolio weight
matrices produced by selection and portfolio-construction helpers. The
per-date sum check helps catch data or aggregation errors
(bench_weights-only deviations are surfaced as warnings to allow
tolerant downstream workflows).

## Slots

- `data`:

  A `data.frame` (first three columns: `id`, `tickers`, `dates`).
  Remaining columns are numeric positive weight vectors.

- `workflow`:

  ANY. Record of preprocessing / portfolio workflow that produced this
  object.

- `signals`:

  character. Names of columns in `data` that represent signals/metrics
  (may be empty for weights objects).

- `unique_dates`:

  numeric. Number of distinct dates in `data`.

- `unique_tickers`:

  numeric. Number of distinct tickers in `data`.

- `n_obs`:

  numeric. Total number of observations (rows) in `data`.

- `current_date`:

  Date. Reference/current date for the object.

- `meta_dataframe_name`:

  character. Short identifier for this meta_dataframe instance.

## Validity

Objects must satisfy parent-class validation and additionally:

- All columns except `id`, `tickers`, and `dates` must be numeric and
  lie between 0 and 1.

- For each date and weight-variable the sum of weights is checked; a
  tolerance of 0.1 is used and violations emit a warning listing
  problematic variable-date combinations (failures do not currently
  abort construction).

## See also

[`meta_dataframe-class`](https://pauloguimaraes871.github.io/factoRverse/reference/meta_dataframe-class.md),
portfolio construction and backtest helpers

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(
  id = c("A-2026-07-01","B-2026-07-01"),
  tickers = c("A","B"),
  dates = as.Date(c("2026-07-01","2026-07-01")),
  bench_weights = c(0.6, 0.4),
  strategy_weights = c(0.7, 0.3),
  stringsAsFactors = FALSE
)
w <- new(
  "weights_m_df",
  data = df,
  workflow = list(step = "construct_weights"),
  signals = character(),
  unique_dates = length(unique(df$dates)),
  unique_tickers = length(unique(df$tickers)),
  n_obs = nrow(df),
  current_date = Sys.Date(),
  meta_dataframe_name = "weights_example"
)
} # }
```
