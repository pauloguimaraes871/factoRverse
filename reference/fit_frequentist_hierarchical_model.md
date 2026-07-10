# Fit a Frequentist Hierarchical Regression Model and Summarize Metrics

This function fits a frequentist hierarchical regression model using the
`lme4` package and computes summary statistics for signal-level metrics.
The model captures signal clustering within themes and provides detailed
metrics for use in performance analysis.

## Usage

``` r
fit_frequentist_hierarchical_model(
  signal_universe_m_d_ref,
  selected_backtest_returns_corrected_positions_m_xts_upd_ref,
  selected_market_factor_proxy_m_xts_upd_ref,
  selected_backtest_returns_corrected_positions_m_upd_ref = NULL,
  selected_signal_themes_m_d_ref,
  model_spec_theme_level,
  lmer_optimizer,
  lmer_optimization_objective,
  hierarchical_p_value_method
)
```

## Arguments

- signal_universe_m_d_ref:

  A data frame containing the signal universe. If provided, data in this
  object will be updated with hierarchical model metrics.

- selected_backtest_returns_corrected_positions_m_xts_upd_ref:

  A data frame containing the backtest returns data for various signals.

  - The first column should include dates.

  - Remaining columns represent signals (e.g., tickers) and their
    respective active returns.

- selected_market_factor_proxy_m_xts_upd_ref:

  A numeric vector containing benchmark returns data. The vector will be
  recycled to match the length of the backtest returns data.

- selected_backtest_returns_corrected_positions_m_upd_ref:

  An already processed
  `selected_backtest_returns_corrected_positions_m_xts_upd_ref`. This
  data.frame is already in long format and contemplates both the
  `selected_market_factor_proxy_m_xts_upd_ref` and the
  `selected_signal_themes_m_d_ref` theme data.

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

- model_spec_theme_level:

  A character string specifying the desired model structure. Options
  include:

  - `"random_intercept_fixed_slope"`: Includes random effects for the
    intercept at the theme level.

  - `"theme_specific_intercept_fixed_slope"`: Uses fixed intercepts for
    each theme.

  - `"theme_specific_intercept_theme_specific_slope"`: Includes fixed
    intercepts and slopes for each theme.

  - `"fixed_intercept_fixed_slope"`: Omits theme-level intercepts but
    includes random effects at the theme:signal level.

- lmer_optimizer:

  A character string specifying the optimizer to be used in the `lmer`
  function.

- lmer_optimization_objective:

  A logical indicating whether to optimize the model using the `lmer`
  function. If `TRUE`, the model will be optimized.

- hierarchical_p_value_method:

  A character string specifying the method for calculating p-values.

## Value

A named list containing:

- `lmer_model`: The fitted `lmerModLmerTest` object (from
  [`lmerTest::lmer`](https://rdrr.io/pkg/lmerTest/man/lmer.html)).

- `pooled_CAPM_metrics_m_d_ref`: A data frame with per-signal CAPM
  metrics extracted from `lmer_model` (see
  [`summarize_lmer_model()`](https://pauloguimaraes871.github.io/factoRverse/reference/summarize_lmer_model.md)),
  or `NULL` if `signal_universe_m_d_ref` was `NULL`.

## Details

The function performs the following steps:

1.  **Preprocessing**: Converts the backtest returns data into long
    format, merges metadata about signals and themes, and adds the
    market factor proxy.

2.  **Formula Specification**: Defines the model formula based on the
    chosen `model_spec_theme_level`.

3.  **Model Fitting**: Fits the frequentist model using the `lmer`
    function from the `lme4` package, incorporating the prepared data.

4.  **Summarization**: Updates the `signal_universe_m_d_ref` data frame
    with new metrics.

This hierarchical approach allows the model to capture both global
(theme-level) and local (signal-level) effects.

'@section P-value calculation The calculation of p-values is a
controversial topic regarding linear mixed-effects models, which is the
reason why [`lme4::lmer`](https://rdrr.io/pkg/lme4/man/lmer.html) does
not provide p-values by default. The `afex` package provides a range of
methods to calculate p-values for mixed models
