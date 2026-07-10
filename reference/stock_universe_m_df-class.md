# stock_universe_m_df-class

`stock_universe_m_df` is the canonical container for per-asset
expected-return scores and eligibility flags used by portfolio
construction and backtesting helpers (e.g.
`derive_stock_universe_m_d_ref`, `select_universe`,
`construct_portfolio`). It derives from
[`meta_dataframe-class`](https://pauloguimaraes871.github.io/factoRverse/reference/meta_dataframe-class.md)
and carries additional metadata about the portfolio/backtest workflow.

## Details

An S4 subclass of `meta_dataframe` representing a stock-level universe
used for portfolio construction.

- `pre_eligible_assets` indicates stocks that pass initial asset-level
  filters before portfolio rules are applied.

- `is_eligible` is a logical indicating final eligibility for portfolio
  inclusion.

- `exp_ret_score` is numeric and used to rank or weight assets; it may
  be derived from out-of-sample predictions or chosen signal metrics and
  optionally scaled by a `scaler`.

## Slots

- `data`:

  A data.frame (first three columns: id, tickers, dates). Must also
  include pre_eligible_assets, is_eligible, and exp_ret_score.

- `workflow`:

  ANY. Record of preprocessing / backtest workflow that produced this
  object.

- `signals`:

  character. Names of columns in data that represent signals / metrics.

- `unique_dates`:

  numeric. Number of distinct dates in data.

- `unique_tickers`:

  numeric. Number of distinct tickers in data.

- `n_obs`:

  numeric. Total number of observations (rows) in data.

- `current_date`:

  Date. Reference/current date for the object.

- `meta_dataframe_name`:

  character. Short identifier for this meta_dataframe instance.

- `port_backtest_workflow`:

  ANY. Metadata describing portfolio/backtest parameters used to build
  the stock universe.

## Validity

Objects must satisfy parent-class validation and must include the
columns `pre_eligible_assets`, `is_eligible`, and `exp_ret_score`. If a
scaler is used, `exp_ret_score` is the post-scaling score; when only raw
signals/predictions are used it equals the raw expected-return score.
Typical construction is via
[`derive_stock_universe_m_d_ref()`](https://pauloguimaraes871.github.io/factoRverse/reference/derive_stock_universe_m_d_ref.md)
which enforces additional checks (mutual exclusivity of inputs,
scaler/shrinkage semantics, winsorization).

## See also

[`meta_dataframe-class`](https://pauloguimaraes871.github.io/factoRverse/reference/meta_dataframe-class.md),
[`signal_universe_m_df-class`](https://pauloguimaraes871.github.io/factoRverse/reference/signal_universe_m_df-class.md),
[`derive_stock_universe_m_d_ref`](https://pauloguimaraes871.github.io/factoRverse/reference/derive_stock_universe_m_d_ref.md)

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(
  id = c("A-2026-07-01","B-2026-07-01"),
  tickers = c("A","B"),
  dates = as.Date(c("2026-07-01","2026-07-01")),
  pre_eligible_assets = c(TRUE, TRUE),
  is_eligible = c(TRUE, FALSE),
  exp_ret_score = c(0.12, -0.03),
  stringsAsFactors = FALSE
)
su <- new(
  "stock_universe_m_df",
  data = df,
  workflow = list(step = "derive_stock_universe"),
  signals = character(),
  port_backtest_workflow = list(params = "example"),
  unique_dates = length(unique(df$dates)),
  unique_tickers = length(unique(df$tickers)),
  n_obs = nrow(df),
  current_date = Sys.Date(),
  meta_dataframe_name = "stock_universe_example"
)
} # }
```
