# Build aggregated group-level universe objects (and optional group covariance)

Aggregates stock-level information into a **group-level universe**
(e.g., sectors) for a single date snapshot, optionally computing:

1.  weighted group summaries (expected return score and liquidity
    metrics), and

2.  a group-by-group covariance matrix using the asset-level covariance.

Internally, within each group the function builds a per-group weight
vector (from `eligible_universe_m_d_ref`). If a group's weights sum to
zero, it **falls back to equal-weights** for that group and warns once.
The final outputs are:

- `group_universe_m_d_ref`: one row per group with aggregated metrics;

- `group_covariance_matrix`: optional \\G \times G\\ covariance between
  groups;

- `group_liquidity_m_d_ref`: optional group-level liquidity frame
  aligned to `liquidity_m_d_ref`'s columns;

- `micro_universe_m_d_ref_list`: the per-group weights actually used.

## Usage

``` r
compute_agg_macro_objects(
  universe_m_d_ref,
  covariance_matrix = NULL,
  group_col,
  micro_universe_m_d_ref_list = NULL,
  liquidity_m_d_ref = NULL
)
```

## Arguments

- universe_m_d_ref:

  `data.frame`. Must contain at least:

  - `tickers` (`character`): asset identifiers;

  - `dates` (`Date`): snapshot date;

  - `is_eligible` (`integer`/`logical`): must be 1 or 0;

  - `exp_ret_score` (`numeric`): stock-level expected return score (used
    in aggregation);

  - `weights` (`numeric`): portfolio weights for the snapshot (used to
    aggregate within group);

  - `{group_col}` (`character`): grouping column name provided via
    `group_col`.

- covariance_matrix:

  `matrix` or `NULL`. Optional **asset-level** covariance \\\Sigma\\
  whose row/column names are exactly
  `eligible_universe_m_d_ref$tickers`. If provided, a **group covariance
  matrix** is computed via \\\Sigma_G\[g_1,g_2\] = w\_{g_1}^\top
  \Sigma\_{g_1,g_2} w\_{g_2}\\ using the per-group weights in
  `micro_universe_m_d_ref_list`.

- group_col:

  `character(1)`. Column name in `eligible_universe_m_d_ref` that
  defines group membership (e.g., `"Sector"`).

- micro_universe_m_d_ref_list:

  `NULL` or `named list`. Optional override for within-group weights.
  Names must cover **all** groups present in
  `eligible_universe_m_d_ref[[group_col]]`. Each element is a
  `data.frame` with columns:

  - `tickers` (`character`);

  - `weights` (`numeric`) — expected to be **within-group** weights (the
    function will normalize if the sum is nonzero; if the sum is zero,
    it falls back to EW with warning). When `NULL`, the function
    constructs this list from `eligible_universe_m_d_ref`.

- liquidity_m_d_ref:

  `NULL` or `data.frame`. Optional stock-level liquidity metrics aligned
  by `id`/`tickers`/`dates`. All columns **after** the first three (id,
  tickers, dates) are aggregated by **weighted mean** using the group's
  within-group weights. If `NULL`, no liquidity aggregation is produced.

## Value

`list` with components:

- `group_universe_m_d_ref` (`data.frame`): one row per group containing:

  - `id` (`character`): `"group-current_date"`;

  - `tickers` (`character`): group identifier (the group name);

  - `dates` (`Date`): snapshot date;

  - `{liquidity columns}` (`numeric`, optional): weighted means of each
    liquidity metric provided in `liquidity_m_d_ref` (if any);

  - `{*_bench_weights}/{target_weights}` (`numeric`, optional):
    group-level sums of matching columns found in
    `eligible_universe_m_d_ref`;

  - `exp_ret_score` (`numeric`): **weighted mean** of stock
    `exp_ret_score` using within-group weights;

  - `is_eligible` (`integer`): set to 1 for groups retained.

- `group_covariance_matrix` (`matrix` or `NULL`): group-by-group
  covariance matrix (dimnames = groups), or `NULL` when
  `covariance_matrix` is `NULL`.

- `group_liquidity_m_d_ref` (`data.frame` or `NULL`): group-level
  liquidity frame containing the same columns as `liquidity_m_d_ref` (if
  provided), else `NULL`.

- `micro_universe_m_d_ref_list` (`named list`): the within-group weights
  used for aggregation and for the group covariance computation.

## Details

**Groups** are inferred from `eligible_universe_m_d_ref[[group_col]]`
for the provided snapshot. The function enforces:

1.  `eligible_universe_m_d_ref` contains **only** eligible rows
    (`is_eligible == 1`);

2.  when `covariance_matrix` is provided, its row/column names are
    exactly `eligible_universe_m_d_ref$tickers`;

3.  when `micro_universe_m_d_ref_list` is `NULL`, per-group weights are
    built from `eligible_universe_m_d_ref$weights` and normalized within
    group; if a group's sum of weights is zero, it **falls back to
    equal-weights** with a warning.

**Aggregation rules**

- `exp_ret_score`:
  [`stats::weighted.mean()`](https://rdrr.io/r/stats/weighted.mean.html)
  within each group using the group's weights.

- Liquidity columns: each column in `liquidity_m_d_ref` (except the
  first three identifier columns) is aggregated by
  [`stats::weighted.mean()`](https://rdrr.io/r/stats/weighted.mean.html)
  using the same within-group weights.

- Group `{*_bench_weights}`/`target_weights`: computed by
  [`dplyr::summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
  as **sums across constituent stocks** for each group.

**Group covariance (optional)** If `covariance_matrix` is provided, the
function calls
[`calculate_group_covariance_matrix()`](https://pauloguimaraes871.github.io/factoRverse/reference/calculate_group_covariance_matrix.md)
with the per-group weights list and returns \\\Sigma_G\\. This is
coherent with ex-ante risk decomposition when the group weights reflect
the portfolio's within-group exposures.

## Errors and Warnings

- **Errors:** missing `group_col`; non-matching ticker sets between
  `eligible_universe_m_d_ref$tickers` and `covariance_matrix` (when
  provided); invalid `micro_universe_m_d_ref_list` structure; presence
  of ineligible rows.

- **Warnings:** when a group's weights sum to zero, the function falls
  back to equal-weights for that group (message: *"Weights for group 'g'
  sum to zero. Fallback to equal weights."*).

## See also

[`calculate_group_covariance_matrix()`](https://pauloguimaraes871.github.io/factoRverse/reference/calculate_group_covariance_matrix.md),
[`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html),
[`dplyr::summarise()`](https://dplyr.tidyverse.org/reference/summarise.html),
[`stats::weighted.mean()`](https://rdrr.io/r/stats/weighted.mean.html),
[`purrr::map()`](https://purrr.tidyverse.org/reference/map.html) /
[`purrr::map_dfr()`](https://purrr.tidyverse.org/reference/map_dfr.html).

## Examples

``` r
NULL
#> NULL
```
