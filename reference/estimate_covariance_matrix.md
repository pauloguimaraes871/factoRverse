# Estimate a covariance matrix based on stock returns

Estimate a covariance matrix based on stock returns

## Usage

``` r
estimate_covariance_matrix(
  tickers,
  returns_m_xts_upd_ref,
  cov_matrix_sample_size,
  cov_estimation_method,
  groups_m_d_ref = NULL,
  active_returns,
  selected_benchmark_m_xts_upd_ref,
  verbose = TRUE
)
```

## Arguments

- tickers:

  A character vector with tickers to be used to estimate the covariance
  matrix

- returns_m_xts_upd_ref:

  A dataframe in which columns represent tickers present in
  current_universe_df and row represent days It should include all
  stocks in assets and a dates column with at least
  cov_matrix_sample_size days before current_date

- cov_matrix_sample_size:

  Number of periods to subset returns_d_ref sample when estimating the
  covariance_matrix. A high number will provide higher degrees of
  freedom, but old returns might not reflect current risk due to
  parameter shift. A low number will tend to expose estimation to
  dimensionality curse.

- cov_estimation_method:

  One of SAM (Sample), EWMA, CC (Constant Correlation), PCA1, PCA2,
  Shrink_ID or Shrink_CC. If NULL, blending_method can only be EW or IR

- groups_m_d_ref:

  A dataframe with id, tickers and dates with dummy group
  classifications to be used to fill NAs

- active_returns:

  A character string indicating whether covariance matrix should be
  calculated based on active returns or raw returns. If TRUE,
  returns_m_xts_upd_ref will be adjusted by subtracting the selected
  market factor proxy in benchmark_returns_m_xts.

- selected_benchmark_m_xts_upd_ref:

  A dataframe in which columns represent benchmarks returns and row
  represent days

- verbose:

  If TRUE, will print messages to the console
