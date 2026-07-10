# signals_m_df-class

`signals_m_df` inherits from `meta_dataframe` and tightens validation
for modelling: the underlying `data` must contain no missing values (no
NA in any column). This ensures downstream modelling functions (the
`run_*` family) receive complete data.

## Details

An S4 subclass of `meta_dataframe` that represents signal-ready data for
modelling.

All slots are inherited from
[`meta_dataframe-class`](https://pauloguimaraes871.github.io/factoRverse/reference/meta_dataframe-class.md):

- `data` : data.frame (first columns: `id`, `tickers`, `dates`)

- `workflow` : list describing preprocessing steps

- `signals` : character vector of signal column names

- `unique_dates`, `unique_tickers`, `n_obs` : numeric summary slots

- `current_date` : Date

- `meta_dataframe_name` : character

## Validity

The validity method fails if any value in `data` is `NA`. Use your
package constructor (e.g.
[`create_meta_dataframe()`](https://pauloguimaraes871.github.io/factoRverse/reference/create_meta_dataframe.md)
then a conversion helper if provided) to build valid objects.

## See also

[`meta_dataframe-class`](https://pauloguimaraes871.github.io/factoRverse/reference/meta_dataframe-class.md),
[`is_coercible_to_meta_dataframe`](https://pauloguimaraes871.github.io/factoRverse/reference/is_coercible_to_meta_dataframe.md)

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(
 id = c("A-2020-01-01", "B-2020-01-02"),
 tickers = c("A", "B"),
 dates = as.Date(c("2020-01-01","2020-01-02")),
 s1 = c(0.1, 0.2),
 stringsAsFactors = FALSE
)
new_obj <- new(
  "signals_m_df",
  data = df,
  workflow = list(),
  signals = "s1",
  unique_dates = length(unique(df$dates)),
  unique_tickers = length(unique(df$tickers)),
  n_obs = nrow(df),
  current_date = Sys.Date(),
  meta_dataframe_name = "signals_example"
)
} # }
```
