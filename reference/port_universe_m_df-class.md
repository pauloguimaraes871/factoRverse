# port_universe_m_df-class

`port_universe_m_df` is the meta-level analogue of
[`stock_universe_m_df-class`](https://pauloguimaraes871.github.io/factoRverse/reference/stock_universe_m_df-class.md):
each row is one base portfolio on one date, and each column is a
characteristic of that portfolio that could be used to set its meta
weight. It is produced by
[`derive_port_universe_m_df`](https://pauloguimaraes871.github.io/factoRverse/reference/derive_port_universe_m_df.md)
from a
[`port_backtest_cohort-class`](https://pauloguimaraes871.github.io/factoRverse/reference/port_backtest_cohort-class.md).

## Details

An S4 subclass of `meta_dataframe` representing a universe of
already-backtested portfolios, used as the input to a meta-portfolio
allocation.

## Slots

- `port_metabacktest_workflow`:

  ANY. Metadata describing how the universe was derived (source cohort,
  return basis, cost window, dates covered).

## Column families

- portfolio statistics:

  Carried over unchanged from each base backtest's `port_stats_m_df`,
  filtered to one return basis. These mix two kinds of number under
  similar names: figures such as `track_err` and `info_ratio` are
  computed from a realized return series, while `act_risk` and `IR` are
  computed from the portfolio's positions and a covariance matrix at the
  formation date. Selecting one where the other was intended silently
  changes what a meta allocation optimizes, so
  [`message_meta_score_basis`](https://pauloguimaraes871.github.io/factoRverse/reference/message_meta_score_basis.md)
  announces which kind a chosen score is.

- `avg_`:

  Running average of a realized cost or turnover figure, over cost
  observations strictly preceding the row's date.

- `metric_`:

  Weight-aggregated custom stock metric at the row's date.

Base statistics are produced only on base rebalance dates and are
carried forward to intervening dates. `stats_age_months` reports how
stale each row is; a value greater than zero means the statistics were
formed at an earlier date, which matters most for risk figures.

## Validity

Objects must satisfy parent-class validation and additionally:

- every column other than `id`, `tickers` and `dates` must be numeric;

- `stats_age_months` must be present;

- a column named `exp_ret_score` must **not** be present. The meta score
  is created downstream by
  [`derive_stock_universe_m_d_ref`](https://pauloguimaraes871.github.io/factoRverse/reference/derive_stock_universe_m_d_ref.md)
  from the user's chosen metric, so its presence here would mean a
  characteristic was silently promoted to the score.

Unlike `stock_universe_m_df`, `pre_eligible_assets` and `is_eligible`
are *not* required: this object is built before
[`classify_investment_universe`](https://pauloguimaraes871.github.io/factoRverse/reference/classify_investment_universe.md)
runs.

## See also

[`stock_universe_m_df-class`](https://pauloguimaraes871.github.io/factoRverse/reference/stock_universe_m_df-class.md),
[`port_backtest_cohort-class`](https://pauloguimaraes871.github.io/factoRverse/reference/port_backtest_cohort-class.md),
[`derive_port_universe_m_df`](https://pauloguimaraes871.github.io/factoRverse/reference/derive_port_universe_m_df.md)
