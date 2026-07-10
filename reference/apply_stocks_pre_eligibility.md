# Apply Pre-Eligibility Filtering with Optional Fallback

This function applies a pre-eligibility filter to a universe of assets
based on a signal's expected return score (`exp_ret_score`). If the
parameter `min_eligible_assets_fallback` is provided (i.e., not `NULL`),
then the function checks whether the number of assets with scores within
the quantile range defined by `eligibility_quantile_range` is at least
the fallback number. If not, the function iteratively expands the
quantile range (decreasing the lower quantile by 0.05 and increasing the
upper quantile by 0.05) until either the fallback number is reached or
the difference between the upper and lower quantile reaches 0.50 (in
which case the function stops with an error).

## Usage

``` r
apply_stocks_pre_eligibility(
  stock_universe_m_d_ref,
  eligibility_quantile_range,
  min_eligible_assets_fallback = NULL,
  verbose = FALSE
)
```

## Arguments

- stock_universe_m_d_ref:

  A data frame that contains at least the column `exp_ret_score`.

- eligibility_quantile_range:

  A numeric vector of length 2 with values in \[0,1\] indicating the
  initial quantile range to select eligible assets.

- min_eligible_assets_fallback:

  A numeric value indicating the minimum number of eligible assets
  desired. If `NULL`, no fallback logic is applied.

- verbose:

  Logical; if `TRUE` prints additional information.

## Value

A list with two elements:

- universe_m_d_ref:

  The updated universe data frame with a new column
  `pre_eligible_assets` (1/0).

- eligibility_quantile_range:

  The final quantile range used.
