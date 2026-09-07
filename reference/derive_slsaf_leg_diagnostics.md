# Derive Leg Diagnostics for a SLSAF Portfolio Backtest

Turns the stock universe of a Simulated Long-Short Allocation Framework
backtest into tidy per-date, per-leg aggregates. Every `slsaf` plot and
summary is built from this one function, so the leg accounting is
defined in a single place and can be tested without any plotting
machinery.

## Usage

``` r
derive_slsaf_leg_diagnostics(
  stock_universe_m_df,
  selected_benchmark,
  group_col = NULL
)
```

## Arguments

- stock_universe_m_df:

  A `stock_universe_m_df`, `meta_dataframe` or `data.frame` carrying the
  backtest universe. Must contain `dates`, `tickers`, `weights`,
  `is_long_candidate`, `is_short_candidate` and the benchmark weight
  column. Optional columns enrich the output: `exp_ret_score`,
  `exp_ret_score_raw`, `act_rel_risk_contr`, `act_weights`,
  `liquidity_classification`, and any group column. The underweight
  profile's `badness` is graded from `exp_ret_score_raw`, the column the
  short leg is built from, and is `NA` when that column is absent.

- selected_benchmark:

  Character scalar naming the benchmark, used to locate the
  `<selected_benchmark>_bench_weights` column.

- group_col:

  Optional character naming the column to use for the sector breakdown.
  Defaults to `"sectors"` when present.

## Value

A named list of `data.frame`s:

- leg_summary:

  One row per date and leg: asset counts, benchmark mass, portfolio
  mass, active weight, weighted and unweighted mean expected return
  score, and share of active risk contribution.

- leg_budget:

  One row per date: the four components of the weight decomposition,
  which sum to 1 by construction, alongside `port_weight_total`, the
  portfolio total taken directly from the weights.

- leg_sector:

  One row per date, leg and group, when a group column exists.

- leg_liquidity:

  One row per date, leg and liquidity classification, when that column
  exists.

- underweight_profile:

  One row per date and short-block asset: benchmark weight, retained
  weight, relative trim, badness score and whether the cap bound.

## Details

The framework splits the universe into a long block (names the
eligibility cascade is willing to buy) and a short block (index
constituents it rejected, which may only be underweighted). Almost every
question worth asking of such a portfolio is a contrast between those
two blocks over time: how much of the benchmark sits in each, how their
scores compare, which one drives tracking error, and how each is
composed by sector and by capitalization.

The aggregates are computed on rebalance dates only, since those are the
dates at which the universe is rebuilt and the blocks are defined.

## See also

[`create_slsaf_portfolio`](https://pauloguimaraes871.github.io/factoRverse/reference/create_slsaf_portfolio.md)
