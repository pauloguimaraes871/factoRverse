# Merge and Rescale Portfolio Weights

This function updates an existing set of portfolio weights by merging it
with new or updated weights. It identifies delisted and newly listed
stocks (IPOs) across two universes (old and current) and optionally
prints the results. The final set of weights is then rescaled to ensure
the sum of weights equals 1. If the sum of weights is zero or
significantly different from 1, the function stops with an error.

## Usage

``` r
merge_and_rescale_weights(
  port_weights_placeholder_m_d_ref,
  updated_port_weights_m_lstd_ref,
  selected_benchmark_weights_m_d_ref,
  stock_universe_m_d_ref = NULL,
  verbose = TRUE
)
```

## Arguments

- port_weights_placeholder_m_d_ref:

  A data frame containing the current universe of stocks (typically with
  columns `tickers`, `id`, and any other relevant columns for the
  current period).

- updated_port_weights_m_lstd_ref:

  A data frame containing the updated or lagged portfolio weights,
  (typically with columns `tickers` and `bop_port_weights`) referring to
  weights carried over from the last period.

- selected_benchmark_weights_m_d_ref:

  (Optional) A data frame with benchmark weights to merge into the final
  output. It should contain columns `id` and the benchmark weight. If
  provided, it will be joined to the final portfolio using `id`.

- stock_universe_m_d_ref:

  A data frame (default `NULL`) representing a rebalanced set of stocks
  (typically with columns `id`, `weights`), which, if provided, is used
  directly to assign `eop_port_weights`. If `NULL`, the function uses
  the `bop_port_weights` from `updated_port_weights_m_lstd_ref` and
  rescales them to sum to 1.

- verbose:

  A logical value indicating whether to print information about delisted
  tickers and IPO tickers. Defaults to `TRUE`.

## Value

A list with the following elements:

- `port_weights_m_d_ref`:

  A data frame with the updated and rescaled `eop_port_weights` column.

- `tickers_both_universes`:

  A character vector of tickers present in both the old and current
  universes.

- `delisted_tickers_old_universe`:

  A character vector of tickers from the old universe that are no longer
  present in the current universe.

- `delisted_tickers_old_portfolio`:

  A character vector of delisted tickers that had a positive weight in
  the old portfolio.

- `ipo_tickers`:

  A character vector of newly introduced tickers in the current universe
  (i.e., IPOs).

## Details

- **Delisted Tickers**:

  Stocks present in the old universe but not in the new universe. If
  these delisted tickers were part of the portfolio (i.e., had a weight
  \> 0), they are also reported.

- **IPO Tickers**:

  Stocks present in the new universe but absent from the old universe,
  hence considered as newly listed.

- **Rescaling**:

  If `stock_universe_m_d_ref` is not `NULL`, `eop_port_weights` is taken
  from the `weights` column of `stock_universe_m_d_ref`. Otherwise, the
  function uses `bop_port_weights` from
  `updated_port_weights_m_lstd_ref` and rescales them such that the
  total weight sums to 1.
