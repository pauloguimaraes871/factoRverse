# target_m_df-class

`target_m_df` stores one or more prediction targets used by
[`run_sb_backtest()`](https://pauloguimaraes871.github.io/factoRverse/reference/run_sb_backtest.md)
and other modeling routines. It inherits all slots from
[`meta_dataframe-class`](https://pauloguimaraes871.github.io/factoRverse/reference/meta_dataframe-class.md)
and adds a naming convention requirement for target columns.

## Details

An S4 subclass of `meta_dataframe` for target variables used by
signal-blending models.

Use
[`create_target_m_df()`](https://pauloguimaraes871.github.io/factoRverse/reference/create_target_m_df.md)
to build valid target objects; it populates slots and enforces naming
and structural rules.

## Slots

- `data`:

  Inherited: a `data.frame` whose first three columns must be `id`,
  `tickers`, `dates`.

- `workflow`:

  Inherited: preprocessing / generation history.

- `signals`:

  Inherited: character vector naming the target columns (must follow the
  pattern above).

- `unique_dates,`:

  unique_tickers, n_obs, current_date, meta_dataframe_name Inherited
  summary slots.

## Validity

Each name listed in the `signals` slot must match the pattern:
`^[A-Za-z_]+_[0-9]{1,2}m$`. That is, "\<target_name\>\_m" where
`horizon` is 1–99 months. If any target name fails this check object
construction is halted with an informative error.

## See also

[`meta_dataframe-class`](https://pauloguimaraes871.github.io/factoRverse/reference/meta_dataframe-class.md),
[`create_target_m_df`](https://pauloguimaraes871.github.io/factoRverse/reference/create_target_m_df.md),
[`run_sb_backtest`](https://pauloguimaraes871.github.io/factoRverse/reference/run_sb_backtest.md)

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(
  id = "A-2026-07-01",
  tickers = "A",
  dates = as.Date("2026-07-01"),
  return_1m = 0.02,
  stringsAsFactors = FALSE
)
tgt <- new(
  "target_m_df",
  data = df,
  workflow = list(step = "create_target"),
  signals = "return_1m",
  unique_dates = 1,
  unique_tickers = 1,
  n_obs = 1,
  current_date = Sys.Date(),
  meta_dataframe_name = "target_example"
)
} # }
```
