# priors_m_df-class

`priors_m_df` is the canonical container for datasets used to fit
frequentist/hierarchical models whose parameter estimates become
informative priors for subsequent Bayesian selection (see
[`run_ss_backtest()`](https://pauloguimaraes871.github.io/factoRverse/reference/run_ss_backtest.md)).
It inherits all slots from
[`meta_dataframe-class`](https://pauloguimaraes871.github.io/factoRverse/reference/meta_dataframe-class.md)
and enforces structural and content constraints required by the
frequentist fitting helpers (e.g.
[`derive_informative_priors_from_data()`](https://pauloguimaraes871.github.io/factoRverse/reference/derive_informative_priors_from_data.md),
[`fit_frequentist_hierarchical_model()`](https://pauloguimaraes871.github.io/factoRverse/reference/fit_frequentist_hierarchical_model.md)).

## Details

An S4 subclass of `meta_dataframe` that stores returns and ancillary
columns used to derive informative priors.

Typical workflow: build a `priors_m_df` from historical returns
(possibly from another geography or period), pass it to
[`derive_informative_priors_from_data()`](https://pauloguimaraes871.github.io/factoRverse/reference/derive_informative_priors_from_data.md)
which fits frequentist/hierarchical models and returns parameter
estimates used as priors for Bayesian inference in
[`run_ss_backtest()`](https://pauloguimaraes871.github.io/factoRverse/reference/run_ss_backtest.md).
Ensuring no missing values and the presence of the required columns
allows stable frequentist estimation and correct prior derivation.

## Slots

- `data`:

  A `data.frame` containing returns and explanatory columns; must
  include `return`, `theme`, and `market_factor_proxy`.

- `workflow`:

  Inherited: list describing preprocessing and modeling steps that
  produced the priors data.

- `signals`:

  Inherited: names of columns considered as signals / grouping
  variables.

- `unique_dates,`:

  unique_tickers, n_obs, current_date, meta_dataframe_name Inherited
  summary slots.

## Validity

- No missing values are allowed in `data` (all NA values cause
  construction to fail).

- The data must contain the columns `return`, `theme`, and
  `market_factor_proxy` (these names must appear among
  `colnames(data)`). Parent-class validation (first three columns `id`,
  `tickers`, `dates`; types; unique ids; etc.) also applies.

## See also

[`meta_dataframe-class`](https://pauloguimaraes871.github.io/factoRverse/reference/meta_dataframe-class.md),
[`derive_informative_priors_from_data`](https://pauloguimaraes871.github.io/factoRverse/reference/derive_informative_priors_from_data.md),
[`run_ss_backtest`](https://pauloguimaraes871.github.io/factoRverse/reference/run_ss_backtest.md)

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(
  id = c("A-2026-01-01","B-2026-01-01"),
  tickers = c("A","B"),
  dates = as.Date(c("2026-01-01","2026-01-01")),
  return = c(0.01, -0.02),
  theme = c("Value","Value"),
  market_factor_proxy = c(0.003, 0.004),
  stringsAsFactors = FALSE
)
pri <- new(
  "priors_m_df",
  data = df,
  workflow = list(step = "prepare_priors"),
  signals = c("theme"),
  unique_dates = length(unique(df$dates)),
  unique_tickers = length(unique(df$tickers)),
  n_obs = nrow(df),
  current_date = Sys.Date(),
  meta_dataframe_name = "priors_example"
)
} # }
```
