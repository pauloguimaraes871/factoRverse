# Create Port Meta Backtest Configuration

The `create_port_metabacktest_config` function creates a
`port_metabacktest_config` object that configures a meta-portfolio
backtest: an allocation across several already-backtested portfolios,
rebalanced on a schedule, driven by those portfolios' own
characteristics. It wraps a single `port_backtest_config` describing the
meta allocation together with the rules for reading the base portfolios'
characteristics out of a cohort. The base portfolios themselves are
supplied later, as a `port_backtest_cohort`, to
[`run_port_backtest()`](https://pauloguimaraes871.github.io/factoRverse/reference/run_port_backtest.md).

## Usage

``` r
create_port_metabacktest_config(meta_port_backtest_config, ...)

# S4 method for class 'port_backtest_config'
create_port_metabacktest_config(
  meta_port_backtest_config,
  type = c("multi_port", "risk_targeted"),
  return_basis = "net",
  cost_lookback = NULL,
  risk_target_parameters = NULL,
  stock_cov_matrix_sample_size = 252,
  config_name = "not_identified",
  verbose = TRUE,
  ...
)
```

## Arguments

- meta_port_backtest_config:

  A `port_backtest_config` describing the meta allocation. Its

- ...:

  Additional arguments (not used).

- type:

  Either `"multi_port"`, allocating across a cohort, or
  `"risk_targeted"`, scaling one sleeve against a residual.

- return_basis:

  Character, `"net"` (default) or `"raw"`. Which return basis of the
  base portfolios' statistics feeds the meta universe.

- cost_lookback:

  `NULL` (default) for an expanding cost average, or a single positive
  whole number of trailing months.

- risk_target_parameters:

  A `risk_target_parameters` object, used only when `type` is
  `"risk_targeted"`. May be `NULL` at construction and supplied later
  with
  [`add_risk_target_parameters()`](https://pauloguimaraes871.github.io/factoRverse/reference/add_risk_target_parameters.md).

- stock_cov_matrix_sample_size:

  Numeric, in **trading days**: the covariance window the stock-level
  run uses for its analytics. Separate from the meta-level window, which
  counts **months** because the meta level allocates over portfolios.
  Defaults to 252. `port_construction_method` must be one of `"ew"`,
  `"sw"`, `"rp"`, `"hrp"` or `"mvo"`, and its
  `chosen_score_metric_and_position` must name a column of the derived
  `port_universe_m_df`.

- config_name:

  Name of the backtest configuration.

- verbose:

  Logical, default `TRUE`. Whether to report the meta score's basis.

## Value

A `port_metabacktest_config` object.

## Details

The meta score is the `chosen_score_metric_and_position` of the wrapped
config, and it must name a column of the
[port_universe_m_df](https://pauloguimaraes871.github.io/factoRverse/reference/port_universe_m_df-class.md)
that
[`derive_port_universe_m_df()`](https://pauloguimaraes871.github.io/factoRverse/reference/derive_port_universe_m_df.md)
builds from the cohort. Because that object carries some statistics in
both an ex-ante and a realized flavour, this constructor reports which
flavour the chosen score is; see
[`message_meta_score_basis()`](https://pauloguimaraes871.github.io/factoRverse/reference/message_meta_score_basis.md).

See
[port_metabacktest_config](https://pauloguimaraes871.github.io/factoRverse/reference/port_metabacktest_config-class.md)
for which slots of the wrapped config act at the meta level and which
act at the stock level.

## Functions

- `create_port_metabacktest_config(port_backtest_config)`: Create a
  meta-backtest config from a meta-level `port_backtest_config`.

## See also

[port_metabacktest_config](https://pauloguimaraes871.github.io/factoRverse/reference/port_metabacktest_config-class.md),
[`derive_port_universe_m_df()`](https://pauloguimaraes871.github.io/factoRverse/reference/derive_port_universe_m_df.md),
[`create_port_backtest_config()`](https://pauloguimaraes871.github.io/factoRverse/reference/create_port_backtest_config.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  # Allocate across base portfolios by their realized information ratio, signal-weighted.
  meta_config <- create_port_backtest_config(
    chosen_score_metric_and_position = c(ann_info_ratio = "long"),
    eligibility_quantile_range = c(0, 1),
    initial_buffer_period = 24,
    rebalancing_months = c(6, 12),
    selected_benchmark = "ibov",
    main_liquidity_metric = "mean_volfin_3m",
    port_construction_method = "sw",
    config_name = "meta_sw_ir"
  )

  port_meta_config <- create_port_metabacktest_config(
    meta_port_backtest_config = meta_config,
    return_basis = "net",
    config_name = "meta_sw_ir"
  )
} # }
```
