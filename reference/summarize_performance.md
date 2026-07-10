# Summarize Performance Metrics of Backtests

Computes performance metrics for a set of signal backtests using either
simple (no-pooled) linear CAPM models or hierarchical mixed-effects CAPM
models. The output includes per-signal metrics such as alphas, betas,
specific risk, t-statistics, p-values, Treynor ratios, and appraisal
ratios, adjusted for active returns if applicable.

## Usage

``` r
summarize_performance(
  selected_backtest_returns_corrected_positions_m_xts_upd_ref,
  selected_market_factor_proxy_m_xts_upd_ref,
  model_structure,
  model_spec_theme_level,
  lmer_control,
  selected_signal_themes_m_d_ref,
  custom_signal_universe_metrics_m_upd_ref = NULL,
  active_returns = TRUE,
  verbose = TRUE
)
```

## Arguments

- selected_backtest_returns_corrected_positions_m_xts_upd_ref:

  An `xts` object containing backtest returns for each signal (columns
  are signal tickers, rows are dates).

- selected_market_factor_proxy_m_xts_upd_ref:

  An `xts` object containing benchmark or market factor returns. Used to
  estimate systematic risk exposures (betas).

- model_structure:

  A character string indicating the CAPM model structure. Must be one
  of:

  - `"no_pooled"`: Fits separate CAPM models per signal using OLS.

  - `"partial_pooled"`: Fits a hierarchical CAPM model using
    [`lme4::lmer`](https://rdrr.io/pkg/lme4/man/lmer.html).

- model_spec_theme_level:

  A character string defining the structure of the hierarchical CAPM.
  Must be one of:

  - `"random_intercept_fixed_slope"`

  - `"theme_specific_intercept_fixed_slope"`

  - `"theme_specific_intercept_theme_specific_slope"`

  - `"fixed_intercept_fixed_slope"`

- lmer_control:

  A named list of control parameters for
  [`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html). Valid
  entries include:

  - `lmer_optimizer`: Optimizer used (e.g., `"nloptwrap"`).

  - `lmer_optimization_objective`: `"REML"` or `"ML"`.

  - `hierarchical_p_value_method`: Method for p-value approximation;
    `"Satterthwaite"`, `"Kenward-Roger"`, or `"lme4"`.

- selected_signal_themes_m_d_ref:

  A dataframe describing signal membership in themes. Must include:

  - `tickers`: Signal identifiers matching those in the return matrix.

  - `theme`: Group (e.g., sector) membership of each signal.

  - `dates`: Date of signal activity.

- custom_signal_universe_metrics_m_upd_ref:

  Optional dataframe with additional signal-level metrics. Most recent
  available date is used. It must include:

  - `tickers`, `dates`, and `id`

  - Any additional performance metrics to append.

- active_returns:

  Logical. If `TRUE`, return series are converted to active returns
  (i.e., excess returns over the benchmark) before estimation.

- verbose:

  Logical. If `TRUE`, prints progress messages.

## Value

A list with two elements:

- `signal_universe_m_d_ref`:

  A meta dataframe with enriched performance metrics including alphas,
  betas, t-statistics, risk-adjusted ratios, and p-values.

- `frequentist_fit_results_list`:

  A list of fitted model objects:

  - For `"no_pooled"`: A list of `lm` models (one per signal).

  - For `"partial_pooled"`: A fitted `lmer` model.
