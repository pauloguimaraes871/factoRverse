# feature_importance_m_df-class

`feature_importance_m_df` inherits all slots and behavior from
[`meta_dataframe-class`](https://pauloguimaraes871.github.io/factoRverse/reference/meta_dataframe-class.md).
It is the canonical container for global-surrogate feature importance
produced by
[`run_sb_backtest()`](https://pauloguimaraes871.github.io/factoRverse/reference/run_sb_backtest.md)
(the package's signal-blending backtest). Importance values are obtained
from the chosen global surrogate model (e.g. OLS coefficients or tree
variable.importance) and then normalized to produce
`normalized_importance`.

## Details

An S4 subclass of `meta_dataframe` that holds feature-importance results
produced by the package's signal-blending / interpretability pipeline.

Valid objects must include at least two columns in `data`:

- `importance` — numeric importance measure (e.g. OLS coef, tree
  importance).

- `normalized_importance` — numeric z-scored / standardized importance.

Typical additional columns produced by
[`run_sb_backtest()`](https://pauloguimaraes871.github.io/factoRverse/reference/run_sb_backtest.md)
and present in examples are `tickers`, `theme`, `is_eligible`, `dates`,
and `id` (where `id = paste0(tickers, "-", dates)`). The validity method
enforces presence of the two required importance columns; other slot
checks come from the parent `meta_dataframe` class.

## Slots

- `data`:

  A `data.frame` containing importance results (must include
  `importance` and `normalized_importance`).

- `workflow`:

  Inherited: list describing preprocessing and modeling steps that led
  to these results.

- `signals`:

  Inherited: typically the signal names considered when building the
  surrogate model.

- `unique_dates,`:

  unique_tickers, n_obs, current_date, meta_dataframe_name Inherited
  summary slots.

## See also

[`meta_dataframe-class`](https://pauloguimaraes871.github.io/factoRverse/reference/meta_dataframe-class.md),
[`run_sb_backtest`](https://pauloguimaraes871.github.io/factoRverse/reference/run_sb_backtest.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Typical object produced by run_sb_backtest()
# Here is a minimal manual construction for testing:
df <- data.frame(
  id = c("A-2026-07-01","B-2026-07-01"),
  tickers = c("A","B"),
  dates = as.Date(c("2026-07-01","2026-07-01")),
  importance = c(0.5, -0.2),
  normalized_importance = c(1.0, -1.0),
  stringsAsFactors = FALSE
)
fi <- new(
  "feature_importance_m_df",
  data = df,
  workflow = list(steps = "run_sb_backtest() (mock)"),
  signals = c("A","B"),
  unique_dates = length(unique(df$dates)),
  unique_tickers = length(unique(df$tickers)),
  n_obs = nrow(df),
  current_date = Sys.Date(),
  meta_dataframe_name = "fi_example"
)
} # }
```
