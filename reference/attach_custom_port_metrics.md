# Attach user-supplied per-portfolio metrics to a port universe

Joins a `meta_dataframe` of user-computed metrics onto the derived
universe by `id`, mirroring how
[`derive_signal_universe_m_df`](https://pauloguimaraes871.github.io/factoRverse/reference/derive_signal_universe_m_df.md)
handles `custom_signal_universe_metrics_m_df`. This is the route for
characteristics the cohort does not compute, or more timely versions of
ones it does, for example a trailing-window tracking error in place of
the base backtests' expanding-window one.

## Usage

``` r
attach_custom_port_metrics(
  port_universe_m_df,
  custom_port_metrics_m_df,
  verbose = TRUE
)
```

## Arguments

- port_universe_m_df:

  The universe assembled so far, carrying `id`, `tickers` and `dates`.

- custom_port_metrics_m_df:

  A `meta_dataframe` whose `tickers` are the base backtest identifiers.

- verbose:

  Logical, default `TRUE`.

## Value

`port_universe_m_df` with the supplied metric columns joined on.

## Details

The validation mirrors the signal-blending path: coercible to a
`meta_dataframe`, numeric columns only, no missing values, a complete
ticker-by-date panel, and every base portfolio covered.

Two deliberate departures from that path:

- Rows are **not** dropped when the supplied object covers fewer dates
  than the cohort. A port universe legitimately carries missing values
  (no realized statistics exist at the first rebalance date, no prior
  cost exists on the first decision date), so dropping incomplete rows
  would delete valid dates rather than clean the data. Uncovered rows
  are left as `NA` and reported.

- A supplied column whose name matches one already derived from the
  cohort is refused rather than joined. A silent join would rename both
  to `<name>.x` and `<name>.y`, leaving the meta score to select an
  arbitrary one of the two.
