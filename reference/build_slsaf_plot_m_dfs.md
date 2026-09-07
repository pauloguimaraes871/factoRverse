# Build Plot-Ready Objects for a SLSAF Portfolio Backtest

Wraps the aggregates from
[`derive_slsaf_leg_diagnostics`](https://pauloguimaraes871.github.io/factoRverse/reference/derive_slsaf_leg_diagnostics.md)
into `meta_dataframe` objects so the existing plot methods can render
them directly. Every `slsaf` plot is therefore an ordinary
`meta_dataframe` plot over a purpose-built frame, rather than bespoke
plotting code.

## Usage

``` r
build_slsaf_plot_m_dfs(
  stock_universe_m_df,
  selected_benchmark,
  group_col = NULL
)
```

## Arguments

- stock_universe_m_df:

  The backtest universe, as produced by an `slsaf` run.

- selected_benchmark:

  Character scalar naming the benchmark.

- group_col:

  Optional character naming the sector column.

## Value

A named list of `meta_dataframe` objects (`budget`, `coverage`,
`leg_score`, `leg_risk`, `underweight`) plus `universe_with_leg`, the
input universe augmented with a `leg` label, and `diagnostics`, the raw
aggregates.

## Details

The `meta_dataframe` contract requires an `id`, `tickers` and `dates`
triple, with `id` equal to `tickers-dates`. The aggregates are not
per-asset, so the `tickers` slot is used to carry whatever the series is
keyed by: the leg name, the budget component, or the asset itself for
the per-name profile. This is the same device the macro-level plots use
to render group series.
