# Summarize Posterior Draws for Signal Universe

This function computes various posterior summary statistics for a given
set of signals, themes, and posterior draws. It updates the
`signal_universe_m_d_ref` data frame with posterior statistics including
alphas, betas, sigmas, and other metrics.

## Usage

``` r
summarize_posteriors_draws(
  brm_model,
  signal_universe_m_d_ref = NULL,
  selected_signal_themes_m_d_ref,
  model_spec_theme_level,
  compute_predictives_full = FALSE,
  n_draws_predictive = NULL
)
```

## Arguments

- brm_model:

  A bayesian model fit with
  [`brms::brm`](https://paulbuerkner.com/brms/reference/brm.html).

- signal_universe_m_d_ref:

  Optional. The performance-enriched signal universe from
  [`summarize_performance()`](https://pauloguimaraes871.github.io/factoRverse/reference/summarize_performance.md)
  (`id`, `tickers`, `dates`, plus frequentist CAPM metric columns). If
  `NULL`, the function instead returns the raw tidy posterior-draws
  objects (see `@return`).

- selected_signal_themes_m_d_ref:

  A (meta) data frame with id, tickers ("signals") and dates column
  contemplating all signals in `signals_m_df` and a "theme" column
  providing group membership for each signal, which is needed for
  defining clusters in bayesian hierarchical model. It should contain
  data only for current date.

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

- compute_predictives_full:

  If TRUE, computes the full posterior predictive distribution.

- n_draws_predictive:

  Number of posterior draws to use for predictive computations. If NULL,
  all available draws are used.

## Value

A named list. When `signal_universe_m_d_ref` is supplied:
`signal_universe_m_d_ref` (the input, left-joined with the posterior
alpha/beta/sigma/Treynor/appraisal metrics) and
`posterior_draws_summaries` (a list of `intercept_summary`,
`slope_summary`, `sd_summary`, `epred_summary`, `predicted_summary` data
frames, each a median/89\\ When `signal_universe_m_d_ref` is `NULL`: a
list of the raw tidy posterior-draws data frames
(`tidy_posterior_draws_intercept`, `tidy_posterior_draws_slope`,
`tidy_posterior_draws_sd`, `tidy_posterior_epred_draws`,
`tidy_posterior_predicted_draws`).

## Details

The function performs the following operations for each theme:

- Computes and updates the posterior overall alpha and individual alpha
  for each signal.

- Computes and updates the probability of the (positive) direction for
  posterior alphas.

- Computes and updates the posterior overall beta and individual beta
  for each signal.

- Computes and updates the posterior sigma for each signal.

- Computes and updates posterior metrics such as active return, tracking
  error, and information ratio (IR).

- Computes and updates additional performance metrics like Appraisal
  Ratio (AP) and Treynor ratio.
