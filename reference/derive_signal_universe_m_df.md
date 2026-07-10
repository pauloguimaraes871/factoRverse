# Derive Signal Universe for Systematic Backtest

Generates or extracts a signal universe for systematic backtesting, with
flexible input methods.

## Usage

``` r
derive_signal_universe_m_df(
  config,
  features_m_df,
  ss_backtest_results,
  backtest_returns_m_xts,
  benchmark_returns_m_xts,
  signal_themes_m_df,
  priors_m_df,
  custom_signal_universe_metrics_m_df,
  verbose,
  parallel,
  winsorization_probs
)
```

## Arguments

- config:

  SB Config

- features_m_df:

  Feature matrix dataframe

- ss_backtest_results:

  Signal Selection Backtest results

- backtest_returns_m_xts:

  XTS of backtest returns

- benchmark_returns_m_xts:

  XTS of benchmark returns

- signal_themes_m_df:

  Dataframe of signal themes (optional)

- priors_m_df:

  Priors dataframe (optional)

- custom_signal_universe_metrics_m_df:

  Custom metrics dataframe (optional)

- verbose:

  Logical to enable verbose output

- parallel:

  Logical to enable parallel processing

- winsorization_probs:

  Winsorization probabilities

## Value

A dataframe representing the signal universe with eligibility and
metadata

## Details

This function can derive a signal universe through two primary methods:

1.  Using provided signal selection backtest results

2.  Generating an artificial signal universe based on input
    configurations

## Note

- Requires careful input matching and validation

- Supports full and partial signal selection

- Handles both long and short signal positions
