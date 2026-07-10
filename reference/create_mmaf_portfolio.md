# Create a Micro–Macro Allocation Framework (MMAF) portfolio

Implements a two-layer portfolio construction framework where the
behavior is controlled by `mmaf_method`:

- **"top_down"**: Build *proxy* intra-sector portfolios first (no sector
  budgets), aggregate to sector-level metrics, construct sector (macro)
  weights, then re-optimize **inside each sector** under its sector
  budget (constraints scaled by the sector budget), and finally
  reconcile to final stock weights (sector weight × intra-sector
  weight).

- **"bottom_up"**: Build a single micro-level portfolio across all
  stocks, aggregate the resulting weights to sector-level metrics,
  construct sector weights (macro), then reconcile stock weights so
  sector totals match macro weights. (Micro-level concentration
  constraints may not hold globally after reconciliation.)

## Usage

``` r
create_mmaf_portfolio(
  universe_m_d_ref,
  mmaf_method = "bottom_up",
  eligible_returns_m_xts_upd_ref = NULL,
  covariance_matrix = NULL,
  selected_benchmark_m_xts_upd_ref = NULL,
  active_returns = if (is.null(selected_benchmark_m_xts_upd_ref)) FALSE else TRUE,
  cov_estimation_method = "sample",
  cov_matrix_sample_size = if (is.null(eligible_returns_m_xts_upd_ref)) NULL else
    nrow(eligible_returns_m_xts_upd_ref),
  groups_m_d_ref,
  mmaf_group_col,
  liquidity_m_d_ref,
  top_down_proxy_port_method = "rp",
  micro_port_construction_method,
  linkage = "single",
  liquidity_constraint_policy = NULL,
  turnover_constraint_policy = NULL,
  concentration_constraint_policy = NULL,
  cap_weighting_metric = NULL,
  n_random_ports = 2000,
  random_ports_method = "sample",
  opt_objective = "sharpe",
  opt_method = "random",
  ridge_pen = NULL,
  n_resamples = 0,
  exp_ret_score_jitter = 0,
  cov_eigval_jitter = 0,
  rp_method = "cyclical-spinu",
  exp_ret_score_tilt = NULL,
  exp_ret_score_tilt_eta = NULL,
  macro_port_construction_method,
  macro_concentration_constraint_policy = NULL,
  macro_cap_weighting_metric = NULL,
  macro_n_random_ports = 2000,
  macro_random_ports_method = "sample",
  macro_opt_objective = "sharpe",
  macro_opt_method = "random",
  macro_ridge_pen = NULL,
  macro_n_resamples = 0,
  macro_exp_ret_score_jitter = 0,
  macro_cov_eigval_jitter = 0,
  macro_rp_method = "cyclical-spinu",
  macro_exp_ret_score_tilt = NULL,
  macro_exp_ret_score_tilt_eta = NULL,
  macro_linkage = "single",
  selected_benchmark = NULL,
  bench_assets_returns_m_xts_upd_ref = NULL,
  lower_quantile_winsorization = 0.025,
  upper_quantile_winsorization = 0.975,
  parallel = FALSE,
  verbose = TRUE
)
```

## Arguments

- universe_m_d_ref:

  A data.frame/tibble with (at least) columns: `id`, `tickers`, `dates`,
  `is_eligible`, `exp_ret_score`, and any optional columns used by the
  micro methods (e.g., benchmark/target weights, liquidity features).
  Should refer to a **single date**.

- mmaf_method:

  Character scalar: `"top_down"` or `"bottom_up"`.

- eligible_returns_m_xts_upd_ref:

  An optional `xts` object containing return data for the eligible
  tickers, used in covariance matrix estimation for Risk-Parity and MVO
  methods.

- covariance_matrix:

  Numeric covariance matrix with row/col names that exactly match the
  `tickers` of eligible names (same ordering is enforced).

- selected_benchmark_m_xts_upd_ref:

  An optional `xts` object containing benchmark returns used to compute
  active returns (only if `active_returns = TRUE`).

- active_returns:

  Logical. If `TRUE`, covariance estimation will use active returns
  (asset returns minus benchmark). Defaults to `FALSE` if
  `selected_benchmark_m_xts_upd_ref` is `NULL`, otherwise `TRUE`.

- cov_estimation_method:

  An optional character string specifying the method for estimating the
  covariance matrix. Defaults to `NULL`.

- cov_matrix_sample_size:

  Integer. Number of time periods (rows in
  `eligible_returns_m_xts_upd_ref`) used to estimate the covariance
  matrix. If `NULL`, uses all available observations.

- groups_m_d_ref:

  An optional data frame used for group constraints and covariance
  matrix estimation. Should include group information if used. Defaults
  to `NULL`.

- mmaf_group_col:

  Character scalar naming the column in `groups_m_d_ref` that carries
  the group/sector classification. (Your wrapper enforces this as the
  4th column.)

- liquidity_m_d_ref:

  Optional data.frame/tibble with liquidity features aligned by `id`,
  `tickers`, `dates`. If provided, group-level liquidity metrics are
  aggregated as weighted means of intra-sector micro weights.

- top_down_proxy_port_method:

  Character: micro method to build *proxy* intra-sector portfolios in
  the initial pass of `"top_down"` (e.g., `"rp"`, `"hrp"`, `"ew"`).

- micro_port_construction_method:

  Character micro method used for the actual intra-sector optimization
  (e.g., `"mvo"`, `"rp"`, `"hrp"`, `"ew"`, or custom handled by
  [`set_portfolio_weights()`](https://pauloguimaraes871.github.io/factoRverse/reference/set_portfolio_weights.md)).

- linkage:

  Character, passed to hierarchical methods when applicable.

- liquidity_constraint_policy:

  Optional list describing liquidity caps at micro level.

- turnover_constraint_policy:

  Optional list describing turnover caps at micro level.

- concentration_constraint_policy:

  Optional list with e.g. `max_abs_active_individual_weight` for micro
  level.

- cap_weighting_metric:

  Optional character for capitalization weighting metric (if used).

- n_random_ports:

  Integer, number of random portfolios (for MVO/random search).

- random_ports_method:

  Character, sampling method for random portfolios.

- opt_objective:

  Character, optimization objective (e.g., `"sharpe"`).

- opt_method:

  Character, optimizer selector for MVO/random search.

- ridge_pen:

  Numeric or `NULL`, ridge penalty for MVO (micro).

- n_resamples:

  Integer, number of resamples for robust MVO (micro).

- exp_ret_score_jitter:

  Numeric, jitter on expected return score (micro MVO).

- cov_eigval_jitter:

  Numeric, jitter on covariance eigenvalues (micro MVO).

- rp_method:

  Character, risk parity method at micro level (e.g.,
  `"cyclical-spinu"`).

- exp_ret_score_tilt:

  Optional numeric vector/column name for RP tilt (micro).

- exp_ret_score_tilt_eta:

  Optional numeric, tilt intensity for micro RP.

- macro_port_construction_method:

  Character macro method used to allocate across groups (e.g., `"ew"`,
  `"rp"`, `"hrp"`, `"mvo"`). For a strictly *neutral* top-down sector
  allocation, prefer `"ew"`, `"rp"` or `"hrp"` and keep
  `macro_exp_ret_score_tilt = NULL`.

- macro_concentration_constraint_policy:

  Optional list with group-level weight caps.

- macro_cap_weighting_metric:

  Optional character for cap-based macro weighting (if used).

- macro_n_random_ports:

  Integer, number of random portfolios at macro level.

- macro_random_ports_method:

  Character, sampling method (macro).

- macro_opt_objective:

  Character, optimization objective at macro.

- macro_opt_method:

  Character, optimizer selector at macro.

- macro_ridge_pen:

  Numeric or `NULL`, ridge penalty for macro MVO.

- macro_n_resamples:

  Integer, number of resamples for macro MVO.

- macro_exp_ret_score_jitter:

  Numeric, jitter on sector ER (macro MVO).

- macro_cov_eigval_jitter:

  Numeric, jitter on macro covariance eigenvalues.

- macro_rp_method:

  Character, risk parity method at macro.

- macro_exp_ret_score_tilt:

  Optional numeric vector/column name for RP tilt (macro).

- macro_exp_ret_score_tilt_eta:

  Optional numeric, tilt intensity for macro RP.

- macro_linkage:

  Character, passed to macro hierarchical methods when applicable.

- selected_benchmark:

  It controls whether a 'port' object for the benchmark should be
  created and added to portolio results.

- bench_assets_returns_m_xts_upd_ref:

  Optional. A 'xts' object containing returns for all stocks. Needed for
  computing benchmark and port stats.

- lower_quantile_winsorization, upper_quantile_winsorization:

  Numerics in (0,1), passed through to micro and macro
  [`set_portfolio_weights()`](https://pauloguimaraes871.github.io/factoRverse/reference/set_portfolio_weights.md).

- parallel:

  Logical, whether to parallelize micro-level sector runs via
  [`furrr::future_map()`](https://furrr.futureverse.org/reference/future_map.html).

- verbose:

  Logical, print progress and timing via `tictoc`.

## Value

A list with:

- universe_m_d_ref:

  Input universe joined with final `weights` (stock-level).

- group_weights:

  Named numeric vector of sector weights (macro).

- macro:

  Return object from
  [`set_portfolio_weights()`](https://pauloguimaraes871.github.io/factoRverse/reference/set_portfolio_weights.md)
  at macro level.

- micro:

  Top-down: list of per-sector micro return objects. Bottom-up: a
  single-element list `list(consolidated = <micro_object>)`.

- group_cov_matrix:

  Sector-by-sector covariance matrix implied by micro solutions.

## Details

**Top-down** emphasizes sector allocation: sectors are first represented
by proxy intra-sector portfolios to estimate sector ER/liquidity and the
sector-by-sector covariance. After optimizing sector weights (macro),
intra-sector portfolios are re-optimized under sector budgets.
Constraint policies (concentration/turnover/liquidity) are **scaled by
the sector budget** when applicable.

**Bottom-up** emphasizes stock selection: a single micro portfolio is
built across all names. Sector metrics are aggregated from this
solution, macro sector weights are computed, and final stock weights are
reconciled to match macro sector totals. Micro-level concentration
constraints may no longer hold exactly after reconciliation.

The function assumes (by your upstream contract) that:

- `universe_m_d_ref` / `groups_m_d_ref` / `liquidity_m_d_ref` are all
  single-date.

- `covariance_matrix` row/col names match the eligible tickers (same
  ordering).

- `port@universe_m_d_ref@data` carries liquidity columns when those are
  used.

- `mmaf_group_col` is the 4th column of `groups_m_d_ref` (enforced by a
  wrapper).

## Examples

``` r
if (FALSE) { # \dontrun{
# Minimal sketch (objects must be prepared consistently upstream):
res_td <- create_mmaf_portfolio(
  universe_m_d_ref = universe_df,
  mmaf_method = "top_down",
  covariance_matrix = Sigma,
  groups_m_d_ref = groups_df,
  mmaf_group_col = "sector",
  liquidity_m_d_ref = liq_df,
  top_down_proxy_port_method = "rp",
  micro_port_construction_method = "mvo",
  macro_port_construction_method = "hrp",
  verbose = TRUE
)

res_bu <- create_mmaf_portfolio(
  universe_m_d_ref = universe_df,
  mmaf_method = "bottom_up",
  covariance_matrix = Sigma,
  groups_m_d_ref = groups_df,
  mmaf_group_col = "sector",
  liquidity_m_d_ref = liq_df,
  micro_port_construction_method = "mvo",
  macro_port_construction_method = "rp",
  verbose = TRUE
)

# Check reconciliation by sector (example):
# dplyr::summarise sum of final weights by sector should match res_td$group_weights
} # }
```
