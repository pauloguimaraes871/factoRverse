# Project Meta Portfolio Weights Down to Stocks

Turns an allocation across base portfolios into the stock-level weights
that allocation implies, by multiplying each base portfolio's meta
weight through its own end-of-period stock weights and summing across
portfolios.

## Usage

``` r
project_meta_weights_to_stocks(
  meta_weights_m_df,
  port_backtest_cohort,
  signals_m_df = NULL,
  residual_ticker = NULL,
  tolerance = 1e-06,
  verbose = TRUE
)
```

## Arguments

- meta_weights_m_df:

  A `data.frame` or `meta_dataframe` with columns `id`, `tickers` and
  `dates`, plus a `weights` column. Its `tickers` are base backtest
  identifiers, not stocks.

- port_backtest_cohort:

  The `port_backtest_cohort` whose `port_weights_m_df` supplies each
  base portfolio's stock weights.

- signals_m_df:

  Optional `meta_dataframe` or `data.frame` of the stock signals the
  meta backtest will run on. When supplied, the result is extended to
  cover its full asset-by-date panel, which is what
  [`check_inputs_port_backtest()`](https://pauloguimaraes871.github.io/factoRverse/reference/check_inputs_port_backtest.md)
  requires. It is needed rather than a plain vector of dates because the
  cohort's weights begin at its own buffer, so only this object knows
  which stocks were quoted on the earlier dates. Defaults to `NULL`,
  covering just the dates the cohort spans.

- residual_ticker:

  Optional character naming a meta-level asset that is a single
  stock-level holding rather than a portfolio, typically the residual
  sleeve of a risk-targeted allocation. Its meta weight becomes that
  ticker's stock weight directly, with no portfolio weights to multiply
  through. It must not be one of the cohort's base portfolios, and it
  must be a row of `signals_m_df`.

- tolerance:

  Numeric, default `1e-6`. How far a per-date weight sum may sit from
  one before the projection is refused. Both sides are exact by
  construction, since meta and base weights alike come from
  set_portfolio_weights(), so this is floating-point slack rather than
  an allowance for cash or leverage.

- verbose:

  Logical, default `TRUE`.

## Value

A `weights_m_df` with columns `id`, `tickers`, `dates` and `weights`,
suitable as `custom_stock_weights_m_df`.

## Details

For a stock \\s\\ on date \\t\\, the projected weight is \$\$w(s, t) =
\sum_p m(p, t) \times v(p, s, t)\$\$ where \\m(p, t)\\ is the meta
weight of base portfolio \\p\\ and \\v(p, s, t)\\ is that portfolio's
own weight in \\s\\. Since each base portfolio's weights sum to one and
the meta weights sum to one, the projection sums to one as well.

## Why project rather than allocate across portfolios directly

Chiefly so that costs are charged on the trades that actually happen.
Projecting first lets the existing engine price each stock-level order
against that stock's own liquidity and volatility, and brings delisting
and IPO handling along unchanged, none of which a portfolio-level run
could do without inventing liquidity and volatility for a portfolio.

Offsetting trades are a second reason, but a smaller one than it first
appears. Where two base portfolios hold the same name and move it in
opposite directions, the trades net at stock level, and implied turnover
is then at most the meta-weighted average of the base portfolios' own.
That inequality always holds when the meta weights are unchanged, but
how much it bites depends entirely on how differently the sleeves trade:
measured on a toy cohort of two long-only sleeves driven by different
signals and rebalancing on the same dates, the saving ranged from
nothing to about two percent of turnover, and two sleeves driven by the
same signal saved almost nothing. Treat netting as a bonus that scales
with genuine disagreement between sleeves, not as the headline reason
for this design.

## Dates between meta rebalances

Meta weights are set on the meta rebalance schedule. The backtest engine
reads custom weights only on its own rebalance dates, but
[`check_inputs_port_backtest()`](https://pauloguimaraes871.github.io/factoRverse/reference/check_inputs_port_backtest.md)
requires a complete panel that sums to one on *every* date, so
intervening dates are filled by holding the last meta weights constant
and re-projecting them onto that date's base weights. Those rows satisfy
the contract; they are not a claim about how the allocation drifts
between rebalances.

## Dates before the first meta weight

The meta backtest's own buffer normally starts at or after the first
meta rebalance, so earlier dates are never consumed. They still have to
be present and sum to one, and are filled with an equal weight across
the stocks quoted on that date. An equal weight is used deliberately
rather than the first projected vector, so no row carries information
from a later date even though nothing reads it.

## See also

[`derive_port_universe_m_df`](https://pauloguimaraes871.github.io/factoRverse/reference/derive_port_universe_m_df.md),
[`create_port_backtest_cohort`](https://pauloguimaraes871.github.io/factoRverse/reference/create_port_backtest_cohort.md)
