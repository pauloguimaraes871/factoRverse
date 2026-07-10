# Compute the Probabilistic Sharpe Ratio

Calculates the probability that the observed Sharpe Ratio exceeds a
reference value, adjusting for non-normality via skewness and kurtosis.

## Usage

``` r
prob_sharpe_ratio(x)
```

## Arguments

- x:

  A numeric vector of cleaned returns

## Value

A single numeric value: the probabilistic Sharpe ratio, or NA if not
computable.

## Details

This implementation is based on the formula proposed in Lopez de Prado
(2018), and always incorporates skewness and kurtosis corrections. It
does not use any external dependencies.
