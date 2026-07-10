# Create a Hierarchical Risk Parity (HRP) portfolio

Constructs a portfolio using the Hierarchical Risk Parity algorithm
(López de Prado, 2016). Assets are recursively grouped by hierarchical
clustering, and risk is allocated inversely to cluster variances.
Optionally, final weights can be tilted by expected return scores.

## Usage

``` r
create_hrp_portfolio(
  universe_m_d_ref,
  covariance_matrix,
  linkage = "single",
  exp_ret_score_tilt_eta = NULL,
  exp_ret_score_tilt = NULL,
  verbose = TRUE
)
```

## Arguments

- universe_m_d_ref:

  A data.frame with at least columns `tickers`, `is_eligible`, and
  `exp_ret_score`. Only rows with `is_eligible == 1` are considered.

- covariance_matrix:

  A covariance matrix of eligible assets, with row and column names
  matching `tickers`.

- linkage:

  Linkage method for hierarchical clustering. Passed to
  [`hclust`](https://rdrr.io/r/stats/hclust.html). Default is
  `"single"`.

- exp_ret_score_tilt_eta:

  numeric or NULL. Tilt intensity used for BOTH "inner" and "final".

- exp_ret_score_tilt:

  character or NULL. If "inner", tilt is applied at each split; if
  "final", a post overlay is applied; if NULL, no tilt.

- verbose:

  Logical, whether to print progress messages. Default is TRUE.

## Value

A list with:

- universe_m_d_ref:

  Input data.frame merged with portfolio weights.

- weights:

  Final portfolio weights (numeric vector).

- dist_matrix:

  Distance matrix used in clustering.

- clusters:

  `hclust` object with the dendrogram.

## References

López de Prado, M. (2016). Building Diversified Portfolios that Perform
Well Out-of-Sample. The Journal of Portfolio Management, 42(4), 59–69.
