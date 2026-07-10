# Compute a group-by-group (block) covariance matrix

Builds a \\G \times G\\ matrix of covariances between groups (e.g.,
sectors) using a full asset-level covariance matrix \\\Sigma\\ and
per-group weight vectors. Each entry \\(g_1, g_2)\\ is computed as
\$\$w\_{g_1}^\top \\ \Sigma\_{g_1,g_2} \\ w\_{g_2},\$\$ where
\\\Sigma\_{g_1,g_2}\\ is the block of \\\Sigma\\ whose rows are the
tickers in group \\g_1\\ and columns are the tickers in group \\g_2\\.

## Usage

``` r
calculate_group_covariance_matrix(
  eligible_universe_m_d_ref,
  groups,
  covariance_matrix,
  group_col,
  micro_universe_m_d_ref_list = NULL
)
```

## Arguments

- eligible_universe_m_d_ref:

  A data.frame containing at least the columns `tickers` (character) and
  the grouping column indicated by `group_col`. Only rows with non-`NA`
  `group_col` values are used.

- groups:

  Character vector of group names (e.g., sectors) for which to compute
  the covariance matrix. Must match values in
  `eligible_universe_m_d_ref[[group_col]]`.

- covariance_matrix:

  Numeric covariance matrix \\\Sigma\\ with row and column names equal
  to asset tickers. Must be square and cover all tickers present in
  `eligible_universe_m_d_ref$tickers`.

- group_col:

  Character scalar with the name of the column in
  `eligible_universe_m_d_ref` that defines group membership (e.g.,
  `"sector"`).

- micro_universe_m_d_ref_list:

  Named list whose names are the elements of `groups`. Each element is a
  data.frame with columns:

  - `tickers` (character): tickers belonging to that group; and

  - `weights` (numeric): corresponding weights for the group-level
    aggregation.

## Value

A numeric \\G \times G\\ matrix with dimnames set to `groups`, where
entry \\(g_1, g_2)\\ equals \\w\_{g_1}^\top \\ \Sigma\_{g_1,g_2} \\
w\_{g_2}\\.

## Details

**Input requirements and alignment**

- All tickers in `eligible_universe_m_d_ref$tickers` must be present as
  both row and column names of `covariance_matrix`.

- For each `g` in `groups`, `micro_universe_m_d_ref_list[[g]]` must
  include `tickers` and `weights` for **all** tickers that belong to
  group `g` according to `eligible_universe_m_d_ref[[group_col]]`.
  Internally, weights are aligned to the ticker order used to extract
  the \\\Sigma\\ blocks.

**Normalization** The function does **not** renormalize group weights;
results therefore reflect the absolute scaling implied by the provided
weights. If you need pure within-group aggregation (i.e., each group’s
weights summing to 1), normalize the weights in
`micro_universe_m_d_ref_list` beforehand.

**Complexity** The computation is \\O(G^2)\\ block multiplications,
where \\G = \\ `length(groups)`, plus the cost of extracting each block
from \\\Sigma\\.

## Note

This function assumes `eligible_universe_m_d_ref$tickers` are unique
within each group for the given snapshot. If duplicates may occur,
deduplicate or aggregate beforehand.

## Errors and Validation

The function raises errors when:

- `covariance_matrix` is not a numeric matrix with named rows/columns.

- `groups` is empty or not a character vector.

- `micro_universe_m_d_ref_list` is not a named list covering all
  `groups`.

- Any ticker in `eligible_universe_m_d_ref$tickers` is missing from
  `covariance_matrix` row/column names.

- For any group, some required tickers are missing in the corresponding
  `micro_universe_m_d_ref_list[[g]]$tickers`, or the weights frame lacks
  the required `tickers`/`weights` columns.

## See also

- [`stats::cov`](https://rdrr.io/r/stats/cor.html) for covariance
  computation at the asset level (if needed upstream).

- [`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html),
  [`dplyr::pull`](https://dplyr.tidyverse.org/reference/pull.html),
  [`dplyr::slice`](https://dplyr.tidyverse.org/reference/slice.html) and
  [`rlang::sym`](https://rlang.r-lib.org/reference/sym.html) used here
  for data handling.
