# Roll Forward Portfolio Weights

This function updates (or "rolls forward") a set of end-of-period
portfolio weights using forward returns. It multiplies each weight by
`(1 + fwd_return_1m/100)` and then normalizes the result so that the
updated weights sum to 1.

## Usage

``` r
roll_fwd_port_weights(port_weights_m_d_ref, clean_fwd_return_1m_m_d_ref)
```

## Arguments

- port_weights_m_d_ref:

  A data frame containing the current end-of-period (EOP) portfolio
  weights. Must include columns:

  - `tickers`: A unique identifier for each asset.

  - `eop_port_weights`: The end-of-period weights of each ticker.

- clean_fwd_return_1m_m_d_ref:

  A data frame containing forward monthly returns (in percent). Must
  include columns:

  - `tickers`: The same identifier used in `port_weights_m_d_ref`.

  - `fwd_return_1m`: The forward return in percentage points (e.g., 2
    means 2%).

## Value

A data frame that includes all joined columns (except for the removed
`eop_port_weights` and `fwd_return_1m` columns) plus a newly calculated
`updated_port_weights` column.

## Details

The function joins `port_weights_m_d_ref` with
`clean_fwd_return_1m_m_d_ref` by `tickers`, calculates the new weights
by multiplying each `eop_port_weights` by `(1 + fwd_return_1m / 100)`,
and then normalizes the resulting vector to ensure the weights sum to 1.

The columns `eop_port_weights` and `fwd_return_1m` are removed in the
final output, leaving `updated_port_weights` along with any remaining
columns brought in by the join.

## Examples

``` r
if (FALSE) { # \dontrun{
library(dplyr)

# Example data
port_weights_m_d_ref <- data.frame(
  tickers = c("A", "B", "C"),
  eop_port_weights = c(0.5, 0.3, 0.2)
)

clean_fwd_return_1m_m_d_ref <- data.frame(
  tickers = c("A", "B", "C"),
  fwd_return_1m = c(2, -1, 3)  # 2% for A, -1% for B, 3% for C
)

# Roll forward the weights
new_weights <- roll_fwd_port_weights(
  port_weights_m_d_ref,
  clean_fwd_return_1m_m_d_ref
)

# Inspect updated weights
new_weights
} # }
```
