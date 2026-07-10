# Calculate Relative Risk Contribution

Computes the relative risk contribution of each asset in a portfolio,
given a vector of weights and a covariance matrix

## Usage

``` r
relative_risk_contribution(weights, covariance_matrix)
```

## Arguments

- weights:

  A numeric vector of weights for each asset in the portfolio

- covariance_matrix:

  A numeric matrix representing the covariance matrix of the assets

## Value

A numeric vector of the same length as `weights` with the relative risk
contribution of each asset
