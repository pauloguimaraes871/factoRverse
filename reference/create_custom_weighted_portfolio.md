# Create a custom_weighted portfolio for signals or stocks

Create a custom_weighted portfolio for signals or stocks

## Usage

``` r
create_custom_weighted_portfolio(
  universe_m_d_ref,
  custom_weights_m_d_ref,
  verbose = TRUE
)
```

## Arguments

- universe_m_d_ref:

  A dataframe with identifiers (tickers or signal column), is_eligible
  and a weighting column as defined by signal_weighting metric. This
  object could either be the result of filter_stock_universe or created
  in the context of the blend_signals_function

- custom_weights_m_d_ref:

  A dataframe with identifiers (tickers or signal column) and custom
  weights.

- verbose:

  If TRUE, will print messages to the console
