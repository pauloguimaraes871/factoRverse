# Compute the Minimum Track Record Length

Calculates the minimum number of observations required for the observed
Sharpe Ratio to be statistically greater than a reference Sharpe Ratio
at a given confidence level, adjusting for skewness and kurtosis.

## Usage

``` r
min_track_record(x)
```

## Arguments

- x:

  A numeric vector of returns (in decimal form).

## Value

A named list with:

- min_trl:

  Minimum track record length required.

- is_significant:

  Logical value: is the current Sharpe Ratio statistically significant?

- num_of_extra_obs_needed:

  Number of additional observations needed (0 if already significant).

## Details

This implementation follows the methodology from Bailey and Lopez de
Prado (2012), and assumes the returns will maintain similar statistical
properties out-of-sample.
