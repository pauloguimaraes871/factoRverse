# Summarize Hierarchical CAPM Model (lmer)

Summarizes the output of a hierarchical CAPM fitted using
[`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html), extracting
fixed and random effects, computing relevant statistics (alphas, betas,
p-values, t-stats, Treynor ratio, appraisal ratio), and aggregating
results according to a specified model structure (theme-level or
signal-level).

## Usage

``` r
summarize_lmer_model(
  lmer_model,
  signal_universe_m_d_ref = NULL,
  selected_signal_themes_m_d_ref,
  model_spec_theme_level,
  hierarchical_p_value_method
)
```

## Arguments

- lmer_model:

  A fitted [`lme4::lmer`](https://rdrr.io/pkg/lme4/man/lmer.html) model
  object, typically estimating a hierarchical CAPM across signals.

- signal_universe_m_d_ref:

  *(Optional)* A meta dataframe including all signals under
  consideration. This argument is not currently used but can be included
  for consistency or future extension.

- selected_signal_themes_m_d_ref:

  A meta dataframe including signals that passed eligibility filters,
  containing columns like `tickers` and `theme`. Used to join results
  and determine final structure.

- model_spec_theme_level:

  A character string indicating the hierarchical structure of the model.
  Must be one of:

  - `"random_intercept_fixed_slope"`: Random intercepts per theme,
    shared slope.

  - `"theme_specific_intercept_fixed_slope"`: Fixed intercepts per
    theme, shared slope.

  - `"theme_specific_intercept_theme_specific_slope"`: Fixed intercepts
    and slopes per theme.

  - `"fixed_intercept_fixed_slope"`: No grouping; simple fixed-effects
    model with intercept and slope.

- hierarchical_p_value_method:

  A character string indicating the degrees of freedom approximation
  method for fixed-effect p-values. Must be one of: `"Satterthwaite"`,
  `"Kenward-Roger"`, or `"lme4"` (which skips approximation).

## Value

A dataframe (`pooled_CAPM_metrics_m_d_ref`) with per-signal CAPM
metrics, including:

- `individual_alpha`, `individual_beta`: Total signal-specific alpha and
  beta.

- `theme_alpha`, `theme_beta`: Theme-level alpha and beta components.

- `alpha_se`, `alpha_t_stat`: Standard error and t-statistic of alpha.

- `p_value`: One-sided p-value for alpha.

- `specific_risk`: Residual volatility (sigma).

- `treynor_ratio`, `appraisal_ratio`: Risk-adjusted performance
  measures.

## Details

This function is used to decompose the result of mixed-effects CAPM
models into interpretable metrics. It handles different model structures
flexibly and integrates fixed and random effects appropriately.
