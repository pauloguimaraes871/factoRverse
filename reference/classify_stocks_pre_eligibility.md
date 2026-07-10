# Classify Stocks for Pre-Eligibility Based on Expected Return Score

This helper function updates the stock universe data frame by flagging
stocks as pre-eligible. It calculates the lower and upper quantile
boundaries based on the provided quantile range and then assigns a flag
of `1L` to stocks with an expected return score within these boundaries,
and `0L` otherwise.

## Usage

``` r
classify_stocks_pre_eligibility(
  stock_universe_m_d_ref,
  eligibility_quantile_range,
  categorical_variable = FALSE
)
```

## Arguments

- stock_universe_m_d_ref:

  A data frame that contains at least an `exp_ret_score` column,
  representing the expected return score for each stock.

- eligibility_quantile_range:

  A numeric vector of length two, specifying the lower and upper
  quantile probabilities (e.g., `c(0.25, 0.75)`). The function uses
  [`min()`](https://rdrr.io/r/base/Extremes.html) and
  [`max()`](https://rdrr.io/r/base/Extremes.html) of this vector to
  define the quantile boundaries.

- categorical_variable:

  A logical value indicating whether the expected return score is
  categorical (i.e., only two unique values).

## Value

A modified version of `stock_universe_m_d_ref` with an additional column
named `pre_eligible_assets`. This column is `1L` if the stock's expected
return score falls between the calculated lower and upper quantile
boundaries (inclusive), and `0L` otherwise.
