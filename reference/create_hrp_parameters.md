# Create HRP (Hierarchical Risk Parity) Parameters

Constructor function for creating an instance of `hrp_parameters`.

## Usage

``` r
create_hrp_parameters(
  linkage = "single",
  exp_ret_score_tilt = "none",
  exp_ret_score_tilt_eta = NULL
)
```

## Arguments

- linkage:

  A character indicating the linkage method to use for hierarchical
  clustering. Possible options are 'single', 'complete', 'average',
  'weighted', 'centroid', 'median', or 'ward.D2'. Default is 'single.'

- exp_ret_score_tilt:

  A character value indicating the tilt to apply to the expected return
  score. It is used to compute the expected return score tilting the
  risk-parity solution towards higher expected return assets. Default is
  'none', meaning no tilt is applied.

- exp_ret_score_tilt_eta:

  A character string indicating the eta to compute the expected return
  score tilt.

## Value

An S4 object of class `hrp_parameters`.
