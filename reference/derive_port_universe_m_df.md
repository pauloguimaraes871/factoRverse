# Derive a Portfolio Universe from a Backtest Cohort

Assembles a
[`port_universe_m_df-class`](https://pauloguimaraes871.github.io/factoRverse/reference/port_universe_m_df-class.md)
from a
[`port_backtest_cohort-class`](https://pauloguimaraes871.github.io/factoRverse/reference/port_backtest_cohort-class.md):
one row per base portfolio per date, carrying the portfolio analytics,
running cost figures and aggregated custom metrics that could be used to
set a meta-level allocation weight.

## Usage

``` r
derive_port_universe_m_df(
  port_backtest_cohort,
  return_basis = c("net", "raw"),
  cost_lookback = NULL,
  custom_port_metrics_m_df = NULL,
  port_universe_name = NULL,
  allow_single_portfolio = FALSE,
  verbose = TRUE
)
```

## Arguments

- port_backtest_cohort:

  An object of class `port_backtest_cohort`.

- return_basis:

  Character, `"net"` (default) or `"raw"`. Selects which row of each
  base `port_stats_m_df` is used: net of transaction costs, or gross.

- cost_lookback:

  Optional positive integer, counted in **calendar months** rather than
  in realized cost observations. The cost series carries a row per
  backtest date, so a lookback of 3 averages the last three months and
  not the last three rebalances. For a portfolio that rebalances
  quarterly or less often most of those months carry no trade, and the
  average is diluted toward zero accordingly. Leave `NULL` for an
  expanding average.

- custom_port_metrics_m_df:

  Optional `meta_dataframe` of user-computed per-portfolio metrics,
  joined on `id`. Its `tickers` must be the base backtest identifiers
  and it must carry only numeric, non-missing columns on a complete
  ticker-by-date panel. Column names may not collide with statistics
  already derived from the cohort. Dates it does not cover are left as
  `NA` rather than dropped.

- port_universe_name:

  Optional character naming the resulting object. Defaults to the cohort
  name suffixed with the return basis.

- allow_single_portfolio:

  Logical. When TRUE, a cohort holding one base portfolio does not raise
  a warning. Set by the risk-targeted path, where a single risky sleeve
  scaled against a residual is the required shape rather than too small
  a cross-section.

- verbose:

  Logical, default `TRUE`.

## Value

An object of class `port_universe_m_df`, ordered by `id`, with `tickers`
holding the base backtest identifiers.

## What is joined

- **Portfolio statistics** come from each base result's
  `port_stats_m_df`, filtered to the requested return basis. These exist
  only on base rebalance dates, so they are carried forward to
  intervening dates (see below).

- **Costs** come from `port_costs_m_xts_list` as a running average,
  prefixed `avg_`.

- **Custom metrics** come from `port_metrics_m_xts_list` at the row's
  date, prefixed `metric_`.

- **User-supplied metrics** come from `custom_port_metrics_m_df`, joined
  on `id`, under whatever names the user gave them. This is the route
  for characteristics the cohort does not compute, or more timely
  versions of ones it does.

## Carry-forward and staleness

Base portfolio statistics are produced only on the base backtests' own
rebalance dates, so a meta rebalance date need not have a fresh value.
The most recent available value is carried forward and
`stats_age_months` records how far. This is look-ahead safe, since a
carried value was formed strictly earlier, but it is not harmless: a
tracking error or information ratio formed six months ago may no longer
describe the portfolio. A warning is raised whenever any value is
carried.

## Point-in-time construction

Every column on date `t` is knowable at `t`. Base statistics are
computed at their own rebalance date from returns realized up to that
date and from positions held then. Cost averages use observations
*strictly before* `t`: costs are stamped one day after the rebalance
they pay for, so this excludes the cost of the rebalance currently being
decided rather than assuming it is observable. Custom metrics are
weight-aggregations of data at `t`.

## Realized versus ex-ante statistics

A base `port_stats_m_df` row splices two families whose names do not
advertise the difference. `track_err` and `info_ratio` are computed from
a realized return series, while `act_risk` and `IR` are computed from
the portfolio's positions and a covariance matrix at the formation date.
Both are carried into this object under their original names;
[`message_meta_score_basis`](https://pauloguimaraes871.github.io/factoRverse/reference/message_meta_score_basis.md)
announces which kind a chosen meta score is, so the two cannot be
confused at selection time.

Note also that the ex-ante figures are identical on the `raw` and `net`
bases: that block is joined to both rows of each base `port_stats_m_df`
and is derived from positions, not returns. Only the realized figures
respond to `return_basis`.

## Units

Returns and the statistics derived from them are in percentage points
(2.0 means 2 percent), matching
[`returns_meta_xts-class`](https://pauloguimaraes871.github.io/factoRverse/reference/returns_meta_xts-class.md).

## See also

[`port_universe_m_df-class`](https://pauloguimaraes871.github.io/factoRverse/reference/port_universe_m_df-class.md),
[`create_port_backtest_cohort`](https://pauloguimaraes871.github.io/factoRverse/reference/create_port_backtest_cohort.md),
[`message_meta_score_basis`](https://pauloguimaraes871.github.io/factoRverse/reference/message_meta_score_basis.md)
