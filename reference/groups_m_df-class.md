# groups_m_df-class

`groups_m_df` inherits from
[`meta_dataframe-class`](https://pauloguimaraes871.github.io/factoRverse/reference/meta_dataframe-class.md)
and is intended for tables where one or more columns represent
categorical groupings (for example sector, theme, or factor families).
The `signals` slot lists the grouping column names to be validated.

## Details

An S4 subclass of `meta_dataframe` for holding grouping/classification
variables (e.g., sectors, themes).

## Slots

- `data`:

  Inherited: a `data.frame` whose first three columns must be `id`,
  `tickers`, `dates`.

- `workflow`:

  Inherited: list describing preprocessing steps that produced the
  grouping table.

- `signals`:

  Inherited: character vector with names of grouping columns in `data`.

- `unique_dates,`:

  unique_tickers, n_obs, current_date, meta_dataframe_name Inherited
  summary slots.

## Validity

For every grouping column named in `signals`, the validity method counts
distinct classifications per `tickers`. If a ticker has more than one
classification for a given grouping column a user-facing warning is
emitted (the object is still created). Parent-class validation (column
order, types, uniqueness, id format, etc.) is also enforced by
`meta_dataframe`.

## See also

[`meta_dataframe-class`](https://pauloguimaraes871.github.io/factoRverse/reference/meta_dataframe-class.md),
[`create_meta_dataframe`](https://pauloguimaraes871.github.io/factoRverse/reference/create_meta_dataframe.md)

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(
  id = c("A-2026-07-01","B-2026-07-01"),
  tickers = c("A","B"),
  dates = as.Date(c("2026-07-01","2026-07-01")),
  sector = c("Energy", "Financials"),
  theme = c("Value", "Value"),
  stringsAsFactors = FALSE
)
gm <- new(
  "groups_m_df",
  data = df,
  workflow = list(step = "assign_groups"),
  signals = c("sector","theme"),
  unique_dates = length(unique(df$dates)),
  unique_tickers = length(unique(df$tickers)),
  n_obs = nrow(df),
  current_date = Sys.Date(),
  meta_dataframe_name = "groups_example"
)
} # }
```
