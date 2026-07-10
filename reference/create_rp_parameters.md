# Create RP (Risk Parity) Parameters

Constructor function for creating an instance of `rp_parameters`.

## Usage

``` r
create_rp_parameters(
  rp_method = "cyclical-spinu",
  exp_ret_score_tilt = "none",
  exp_ret_score_tilt_eta = NULL
)
```

## Arguments

- rp_method:

  A character indicating the method to compute the risk-parity vanilla
  solution. It is passed to
  [`riskParityPortfolio::riskParityPortfolio()`](https://rdrr.io/pkg/riskParityPortfolio/man/riskParityPortfolio.html)
  function as `method_init`. Default is `"cyclical-spinu"`.

- exp_ret_score_tilt:

  A character value indicating the tilt to apply to the expected return
  score. It is used to compute the expected return score tilting the
  risk-parity solution towards higher expected return assets. Default is
  'none', meaning no tilt is applied.

- exp_ret_score_tilt_eta:

  A character string indicating the eta to compute the expected return
  score tilt.

## Value

An S4 object of class `rp_parameters`.
