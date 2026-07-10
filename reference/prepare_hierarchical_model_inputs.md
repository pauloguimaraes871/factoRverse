# Prepare inputs for a hierarchical model.

This functions takes wide-formar backtest and benchmark returns data,
and transforms it into a long format. It also adds a market factor proxy
to the data frame. The function then merges the data with signal themes
and prepares a formula for the Bayesian hierarchical model.

## Usage

``` r
prepare_hierarchical_model_inputs(
  selected_backtest_returns_corrected_positions_m_xts_upd_ref,
  selected_market_factor_proxy_m_xts_upd_ref,
  selected_signal_themes_m_d_ref,
  selected_backtest_returns_corrected_positions_m_upd_ref = NULL,
  model_spec_theme_level
)
```

## Arguments

- selected_backtest_returns_corrected_positions_m_xts_upd_ref:

  A xts containing the backtest returns data for various signals.

  - The first column should include dates.

  - Remaining columns represent signals (e.g., tickers) and their
    respective active returns.

- selected_market_factor_proxy_m_xts_upd_ref:

  A xts containing benchmark returns data.

- selected_signal_themes_m_d_ref:

  A data frame containing metadata about signals. This data frame should
  include:

  - `tickers`: Signal identifiers matching those in
    `selected_backtest_returns_corrected_positions_m_xts_upd_ref`.

  - `theme`: Group membership for each signal, defining the clusters for
    the Bayesian hierarchical model.

  - `dates`: Dates corresponding to the backtest data. This input
    ensures proper alignment between signals and their associated
    themes.

- selected_backtest_returns_corrected_positions_m_upd_ref:

  An already processed
  `selected_backtest_returns_corrected_positions_m_xts_upd_ref`. This
  data.frame is already in long format and contemplates both the
  `selected_market_factor_proxy_m_xts_upd_ref` and the
  `selected_signal_themes_m_d_ref` theme data.

- model_spec_theme_level:

  A character string specifying the desired Bayesian model structure.
  Options include:

  - `"random_intercept_fixed_slope"`: Includes random effects for the
    intercept at the theme level.

  - `"theme_specific_intercept_fixed_slope"`: Uses fixed intercepts for
    each theme.

  - `"theme_specific_intercept_theme_specific_slope"`: Includes fixed
    intercepts and slopes for each theme.

  - `"fixed_intercept_fixed_slope"`: Omits theme-level intercepts but
    includes random effects at the theme:signal level.
