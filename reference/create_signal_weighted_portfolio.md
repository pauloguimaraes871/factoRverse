# Create a Signal-Weighted Portfolio for Signals or Stocks

This function calculates portfolio weights based on signal scores (e.g.,
expected return scores) for eligible assets. It is typically used after
applying selection filters to the investment universe.

## Usage

``` r
create_signal_weighted_portfolio(universe_m_d_ref, verbose = TRUE)
```

## Arguments

- universe_m_d_ref:

  A data.frame containing the investment universe with the following
  columns:

  `tickers`

  :   Asset or signal identifier.

  `is_eligible`

  :   Binary flag (0/1) indicating if the asset is eligible for
      inclusion.

  `exp_ret_score`

  :   Expected return score or signal strength, used to derive portfolio
      weights.

  This object can be the result of `filter_stock_universe()` or
  constructed inside a custom signal blending function.

- verbose:

  Logical. If `TRUE`, prints timing and status messages to the console
  using `tictoc` and `crayon`.

## Value

A list with the following elements:

- `universe_m_d_ref`:

  The original data frame augmented with the computed weights.

- `weights`:

  A numeric vector of portfolio weights summing to 1 (within tolerance).

- `exp_ret_score`:

  The original expected return score vector used to define the weights.
