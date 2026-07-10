# Compute portfolio statistics (portfolio- and group-level)

Computes portfolio metrics (HHI, effective N, entropy, Gini, top-k
concentration, diversification ratio, weighted average pairwise
correlation, gross/net exposure, RRC-based metrics) PLUS portfolio
expected return, risk (stdev), and Sharpe (rf = 0). Optionally, also
computes a parallel set of metrics for a provided **group universe**
(e.g., sector or sleeve portfolio), with column names prefixed by
`group_`.

## Usage

``` r
calculate_port_stats(
  universe_m_d_ref,
  covariance_matrix = NULL,
  group_universe_m_d_ref = NULL,
  group_cov_matrix = NULL,
  selected_benchmark = NULL,
  bench_universe_m_d_ref = NULL,
  all_returns_m_xts_upd_ref = NULL,
  cov_estimation_method = "sample",
  cov_matrix_sample_size = if (is.null(all_returns_m_xts_upd_ref)) NULL else
    nrow(all_returns_m_xts_upd_ref),
  groups_m_d_ref = NULL
)
```

## Arguments

- universe_m_d_ref:

  A `data.frame` with at least columns: `tickers` (character),
  `is_eligible` (integer/logical), `weights` (numeric), and optionally
  `exp_ret_score` (numeric), `rel_risk_contr` (numeric). Only rows with
  `is_eligible == 1` are used for the **portfolio**.

- covariance_matrix:

  Optional covariance matrix for the eligible tickers when benchmark is
  not provided

- group_universe_m_d_ref:

  Optional `data.frame` in the same format but representing a **group
  portfolio** (e.g., pre-aggregated sector/sleeve weights). Only rows
  with `is_eligible == 1` are used. A separate metrics block is
  returned, prefixed with `group_`.

- group_cov_matrix:

  Optional covariance matrix for the **group** universe (row/column
  names must match `group` tickers provided).

- selected_benchmark:

  Character indicating the benchmark to use

- bench_universe_m_d_ref:

  Optional `data.frame` in the same format as `universe_m_d_ref` for the
  **benchmark**; if provided, active weights are used
  (`w_port - w_bench`) on the union of tickers (missing side = 0).

- all_returns_m_xts_upd_ref:

  An optional `xts` object containing return data for all tickers, used
  in covariance matrix estimation for Risk-Parity and MVO methods.

- cov_estimation_method:

  Character. Covariance matrix estimation method to use.

- cov_matrix_sample_size:

  Integer. Number of time periods (rows in `all_returns_m_xts_upd_ref`)
  used to estimate the covariance matrix. If `NULL`, uses all available
  observations.

- groups_m_d_ref:

  An optional data frame used for group constraints and covariance
  matrix estimation. Should include group information if used. Defaults
  to `NULL`.

## Value

A named list with components:

- `port_stats`:

  A one-row `data.frame` of portfolio metrics. Includes
  `group_`-prefixed metrics when `group_universe_m_d_ref` is supplied,
  and `act_`-prefixed active variants (with `sharpe` renamed
  `info_ratio`) when a benchmark is supplied.

- `assets_stats`:

  A `data.frame` of per-asset `tickers`, `weights` (active weights when
  a benchmark is supplied), and `rel_risk_contr`.

- `covariance_matrix`:

  The covariance matrix actually used (estimated or subset), or `NULL`
  when none was available.
