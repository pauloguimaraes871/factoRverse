# Create Covariance Estimation Method

Constructor for creating an instance of `cov_est_method`.

## Usage

``` r
create_cov_est_method(
  cov_estimation_method = "sample",
  cov_matrix_sample_size,
  active_returns = TRUE,
  cov_matrix_benchmark = NULL
)
```

## Arguments

- cov_estimation_method:

  A character string representing the covariance estimation method. Must
  be one of 'sample', 'ewma', 'cc', 'pca1', 'pca2', 'shrink_id' or
  'shrink_cc'.

- cov_matrix_sample_size:

  Number of periods to subset return sample when estimating the
  covariance matrix. A high number will provide higher degrees of
  freedom, but old returns might not reflect current risk due to
  parameter shift. A low number will tend to expose estimation to
  dimensionality curse.

- active_returns:

  logical. If TRUE, the covariance matrix will be estimated using active
  returns. If FALSE, the covariance matrix will be estimated using raw
  returns.

- cov_matrix_benchmark:

  A character string representing the benchmark for covariance matrix
  estimation. If NULL, the covariance matrix will be estimated using the
  active returns.
