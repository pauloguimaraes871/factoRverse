# Derive Adapted Custom Signal Universe Metrics

This function consolidates and adapts custom signal universe metrics. It
processes evaluation metrics across multiple backtest results and
integrates them with meta-level data.

## Usage

``` r
derive_adapted_custom_signal_universe_m_df(
  meta_custom_objective,
  base_sb_backtest_results_list,
  meta_custom_signal_universe_metrics_m_df,
  base_custom_signal_universe_metrics_m_df
)
```

## Arguments

- meta_custom_objective:

  A character string representing the custom objective (e.g.,
  'min_rss').

- base_sb_backtest_results_list:

  A list of backtest results for processing.

- meta_custom_signal_universe_metrics_m_df:

  Meta-level metrics dataframe.

- base_custom_signal_universe_metrics_m_df:

  Base-level metrics dataframe.

## Value

A combined and processed dataframe of signal universe metrics.
