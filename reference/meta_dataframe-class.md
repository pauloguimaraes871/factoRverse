# meta_dataframe-class

The `meta_dataframe` class encapsulates a `data.frame` together with
metadata and a recorded workflow of preprocessing and
feature-engineering steps used in the factoRverse backtesting pipeline.
It enforces a strict table layout and several validation rules to ensure
downstream methods (imputation, normalization, portfolio construction,
signal blending, etc.) receive a well-formed input.

## Details

An S4 container that wraps a data.frame with backtest workflow metadata
and structural validation.

This class is intended as the canonical input for the package's
preprocessing, modeling, and backtesting methods. Storing both the raw
table (`data`) and a reproducible `workflow` history enables
reproducible pipelines and safe application of subsequent transforms.

Use the package constructor (e.g.
[`create_meta_dataframe()`](https://pauloguimaraes871.github.io/factoRverse/reference/create_meta_dataframe.md))
when available; it will populate slots and run validation. The low-level
S4 constructor `new("meta_dataframe", ...)` may be used for testing but
must satisfy all validation rules.

## Slots

- `data`:

  A `data.frame` containing the actual tabular observations. Required
  first columns: `id`, `tickers`, `dates`.

- `workflow`:

  A list (typically a sb_backtest_workflow object or plain `list`)
  recording the sequence of preprocessing / modeling steps applied to
  the `data`. Each element should describe one operation (name,
  parameters, timestamp, etc.).

- `signals`:

  A `character` vector with the names of columns in `data` that are
  considered signals (features, targets, groupings, etc.).

- `unique_dates`:

  Numeric: number of distinct dates present in `data`.

- `unique_tickers`:

  Numeric: number of distinct tickers present in `data`.

- `n_obs`:

  Numeric: total number of observations (rows) in `data`.

- `current_date`:

  A `Date` object representing the reference/current date for the
  object.

- `meta_dataframe_name`:

  A short `character` identifier for this meta_dataframe instance.

## Key validation rules

- The underlying `data` must be a `data.frame`.

- The first three columns of `data` must be exactly `id`, `tickers`,
  `dates` in that order.

- `tickers` must be a character vector; `dates` must be of class `Date`;
  `id` must be character and equal to `paste0(tickers, "-", dates)`.

- Rows must be ordered by `id` (alphabetical order) and `dates` must be
  non-decreasing.

- Column names (variable names) must be unique and must not include the
  prefix `"low_"` in any `signals` (this prefix breaks certain
  backtesting routines).

- The validity function also calls
  [`is_coercible_to_meta_dataframe()`](https://pauloguimaraes871.github.io/factoRverse/reference/is_coercible_to_meta_dataframe.md)
  which prints helpful messages when coercion checks fail.

## See also

[`is_coercible_to_meta_dataframe`](https://pauloguimaraes871.github.io/factoRverse/reference/is_coercible_to_meta_dataframe.md),
[`create_meta_dataframe`](https://pauloguimaraes871.github.io/factoRverse/reference/create_meta_dataframe.md),
related S4 classes: `raw_features_m_df`, `signals_m_df`, `target_m_df`,
`groups_m_df`, `priors_m_df`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Assume create_meta_dataframe() is the user-facing constructor
df <- data.frame(
  id = c("A-2020-01-01", "B-2020-01-02"),
  tickers = c("A", "B"),
  dates = as.Date(c("2020-01-01", "2020-01-02")),
  signal1 = c(0.1, -0.2),
  stringsAsFactors = FALSE
)

mdf <- create_meta_dataframe(
  data = df,
  workflow = list(),
  signals = c("signal1"),
  meta_dataframe_name = "example_mdf"
)

# low-level creation (not recommended unless you ensure validation)
new_mdf <- new(
  "meta_dataframe",
  data = df,
  workflow = list(),
  signals = "signal1",
  unique_dates = length(unique(df$dates)),
  unique_tickers = length(unique(df$tickers)),
  n_obs = nrow(df),
  current_date = Sys.Date(),
  meta_dataframe_name = "example_lowlevel"
)
} # }
```
