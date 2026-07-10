# raw_features_m_df-class

Inherits all slots and behavior from
[`meta_dataframe-class`](https://pauloguimaraes871.github.io/factoRverse/reference/meta_dataframe-class.md)
but adds no extra validity checks. Use for intermediate/raw outputs (may
contain NA) produced by
[`create_meta_dataframe()`](https://pauloguimaraes871.github.io/factoRverse/reference/create_meta_dataframe.md)
or similar constructors.

## Details

An S4 subclass of `meta_dataframe` representing raw feature tables.

All slots are inherited (data, workflow, signals, unique_dates,
unique_tickers, n_obs, current_date, meta_dataframe_name).

## See also

[`meta_dataframe-class`](https://pauloguimaraes871.github.io/factoRverse/reference/meta_dataframe-class.md),
[`signals_m_df-class`](https://pauloguimaraes871.github.io/factoRverse/reference/signals_m_df-class.md)

## Examples

``` r
if (FALSE) { # \dontrun{
new(
  "raw_features_m_df",
  data = df,
  workflow = list(),
  signals = character(),
  unique_dates = length(unique(df$dates)),
  unique_tickers = length(unique(df$tickers)),
  n_obs = nrow(df),
  current_date = Sys.Date(),
  meta_dataframe_name = "raw_features"
)
} # }
```
