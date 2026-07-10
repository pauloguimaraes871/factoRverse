# Validate Meta Signal Selection and Data Inputs

This function performs a series of validation checks on the meta-level
signal selection, features passthrough, meta dataframes, and xts
objects.

## Usage

``` r
check_inputs_meta_sb_backtest(
  config,
  features_m_df,
  target_m_df,
  base_sb_backtest_results_list,
  base_signal_themes_m_df,
  base_custom_signal_weights_m_df,
  base_custom_signal_universe_metrics_m_df,
  meta_signal_themes_m_df,
  meta_custom_signal_weights_m_df,
  meta_custom_signal_universe_metrics_m_df,
  base_backtest_returns_m_xts,
  base_benchmark_returns_m_xts,
  meta_backtest_returns_m_xts,
  meta_benchmark_returns_m_xts,
  verbose = TRUE
)
```

## Arguments

- config:

  An object containing the meta backtest configuration.

- features_m_df:

  A dataframe containing features

- target_m_df:

  A dataframe containing targets

- base_sb_backtest_results_list:

  A list of base signal blend backtest results.

- base_signal_themes_m_df:

  Optional base signal themes meta dataframe.

- base_custom_signal_weights_m_df:

  Optional base custom signal weights meta dataframe.

- base_custom_signal_universe_metrics_m_df:

  Optional base custom signal universe metrics meta dataframe.

- meta_signal_themes_m_df:

  Optional meta signal themes meta dataframe.

- meta_custom_signal_weights_m_df:

  Optional meta custom signal weights meta dataframe.

- meta_custom_signal_universe_metrics_m_df:

  Optional meta custom signal universe metrics meta dataframe.

- base_backtest_returns_m_xts:

  Optional xts object for base backtest returns.

- base_benchmark_returns_m_xts:

  Optional xts object for base benchmark returns.

- meta_backtest_returns_m_xts:

  Optional xts object for meta backtest returns.

- meta_benchmark_returns_m_xts:

  Optional xts object for meta benchmark returns.

- verbose:

  A boolean indicating whether to print detailed messages.

## Value

None. Stops execution if validation checks fail.
