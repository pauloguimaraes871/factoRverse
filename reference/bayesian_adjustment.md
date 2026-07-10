# Bayesian Adjustment Function

Performs Bayesian p-value adjustment by setting priors, fitting a
Bayesian hierarchical model to signals, and summarizing posterior draws.
Optionally provides progress updates if `verbose` is set to `TRUE`.

## Usage

``` r
bayesian_adjustment(
  signal_universe_m_d_ref,
  selected_backtest_returns_corrected_positions_m_xts_upd_ref,
  selected_market_factor_proxy_m_xts_upd_ref,
  priors_m_upd_ref = NULL,
  model_spec_theme_level,
  user_priors = NULL,
  lmer_optimization_objective = "REML",
  half_t_df = 30,
  lmer_optimizer = "nloptwrap",
  selected_signal_themes_m_d_ref,
  chains = 4,
  iter = 2000,
  warmup = floor(iter/2),
  thin = 1,
  seed = NA,
  adapt_delta = 0.8,
  parallel = TRUE,
  verbose = TRUE
)
```

## Arguments

- signal_universe_m_d_ref:

  Data frame. The performance-enriched signal universe produced by
  [`summarize_performance()`](https://pauloguimaraes871.github.io/factoRverse/reference/summarize_performance.md),
  containing at least `id`, `tickers`, `dates`, and the frequentist CAPM
  metrics (`alpha`, `alpha_se`, `beta`, `specific_risk`, `alpha_t_stat`,
  `treynor_ratio`, `appraisal_ratio`, `p_value` for `"no_pooled"`, or
  the theme/individual alpha and beta columns for `"partial_pooled"`).
  It is updated in place with posterior statistics.

- selected_backtest_returns_corrected_positions_m_xts_upd_ref:

  Meta xts containing backtest returns for selected signals

- selected_market_factor_proxy_m_xts_upd_ref:

  Meta xts containing backtest returns for selected markte factor proxy

- priors_m_upd_ref:

  Data frame. A (meta)data frame of exogenous (out-of-sample) return
  observations used to derive informative priors via
  [`derive_informative_priors_from_data()`](https://pauloguimaraes871.github.io/factoRverse/reference/derive_informative_priors_from_data.md),
  with the following columns:

  id

  :   Identifier for each observation.

  tickers

  :   Signal (ticker) identifier associated with each observation.

  dates

  :   Date of each observation.

  return

  :   Return of the signal at that date.

  market_factor_proxy

  :   Market factor proxy return at that date.

  theme

  :   Theme associated with each signal, used for clustering in the
      hierarchical Bayesian model.

  **Note:** This dataframe should contain observations up to (but not
  beyond) the current date.

- model_spec_theme_level:

  Character string. Specifies the structure of the hierarchical Bayesian
  model. Options include:

  `"random_intercept_fixed_slope"`

  :   Random intercepts for signals, fixed slope across all themes.

  `"theme_specific_intercept_fixed_slope"`

  :   Fixed intercepts per theme, fixed slope across themes.

  `"theme_specific_intercept_theme_specific_slope"`

  :   Fixed intercepts and slopes per theme.

  `"fixed_intercept_fixed_slope"`

  :   One fixed intercept and slope across all observations.

- user_priors:

  List. A list containing user-defined priors for the hierarchical
  Bayesian model, used when `priors_type` is `"user"`. The list should
  conform to the structure required by the `brms` package.

- lmer_optimization_objective:

  A character string indicating whether estimates should be chosen to
  optimize the 'REML' criterion or the 'likelihood'.

- half_t_df:

  Numeric. The degrees of freedom in the half-t distribution applied to
  model random effects. This parameter controls the tails of the
  distribution. Default is `30`.

- lmer_optimizer:

  Character string. Specifies the optimizer to be used in the
  [`lme4::lmer`](https://rdrr.io/pkg/lme4/man/lmer.html) function.
  Options include:

  `"nloptwrap"`

  :   Non-linear optimization using the NLopt library.

  `"bobyqa"`

  :   Bound optimization BY quadratic approximation.

  `"Nelder_Mead"`

  :   Simplex-based Nelder-Mead optimization.

  `"nlminbwrap"`

  :   Wrapper for the `nlminb` optimizer.

  Default is `"nloptwrap"`.

- selected_signal_themes_m_d_ref:

  Data frame. A (meta)data frame containing metadata about signals with
  the following columns:

  id

  :   Identifier for each observation.

  tickers

  :   Signal identifiers matching those in `signal_universe_m_d_ref` and
      `selected_backtest_returns_corrected_positions_upd_ref`.

  dates

  :   Dates corresponding to the backtest data.

  theme

  :   Group membership for each signal, defining the clusters for the
      Bayesian hierarchical model.

  **Note:** This dataframe should contain data only for the current
  date.

- chains:

  Integer. The number of Markov chains to run for the MCMC sampling.
  Default is `4`.

- iter:

  Integer. The total number of iterations per chain for the MCMC
  sampling. Default is `2000`.

- warmup:

  Integer. The number of warmup (burn-in) iterations per chain for the
  MCMC sampling. Default is `floor(iter / 2)`.

- thin:

  Integer. The thinning interval for MCMC sampling. Default is `1`.

- seed:

  Integer or `NA`. The seed for random number generation to ensure
  reproducibility. Set to a specific integer for reproducible results or
  `NA` for random seeding. Default is `NA`.

- adapt_delta:

  Numeric. The target acceptance probability for the Hamiltonian Monte
  Carlo sampler. Higher values can lead to better convergence at the
  cost of slower sampling. Must be between `0` and `1`. Default is
  `0.99`.

- parallel:

  Logical. Indicates whether to enable parallel computation using the
  `future` package. Default is `TRUE`.

- verbose:

  Logical. Indicates whether to print progress messages during the
  Bayesian model fitting process. If `TRUE`, progress messages are
  printed; otherwise, they are suppressed. Default is `TRUE`.

## Value

A named list with the following components:

- `posterior_signal_universe_m_d_ref`:

  Data frame. The input `signal_universe_m_d_ref` updated with posterior
  summary statistics derived from the Bayesian model fitting. This
  includes metrics such as posterior alphas, betas, sigmas, active
  returns, tracking errors, information ratios (IR), appraisal ratios
  (AP), and Treynor ratios.

- `brm_model`:

  `brmsfit` object. The fitted Bayesian hierarchical model containing
  posterior distributions, parameter estimates, diagnostics, and other
  details of the model fit.

- `posterior_draws_summaries`:

  List. Summary statistics (median and 89\\

- `elected_priors`:

  `brmsprior` data frame, or `NULL` if uninformative priors were used.
  The priors used in the Bayesian model, either derived from data,
  provided by the user, or `NULL`.

- `elected_priors_frequentist_model`:

  [`lme4::lmer`](https://rdrr.io/pkg/lme4/man/lmer.html) object, or
  `NULL`. The fitted frequentist linear mixed-effects model used to
  derive informative priors (only set when `priors_m_upd_ref` was
  supplied).

## Details

This function performs Bayesian p-value adjustment through the following
steps:

1.  **Initial Checks**: Validates input parameters, ensuring
    `priors_type` is valid and that necessary prior data is provided
    based on the specified `priors_type`.

2.  **Set Priors**:

    - If `priors_m_upd_ref` is provided, it sets priors based on the
      provided informative data using the
      `derive_informative_priors_from_data` function.

    - If `user_priors_list` is provided, it utilizes user-defined priors
      provided.

    - If neither `user_priors_list` or `priors_m_upd_ref` are provided,
      it employs default uninformative priors as defined by the `brms`
      package.

3.  **Fit Bayesian Hierarchical Model**: Fits a Bayesian hierarchical
    model to each theme in `selected_signal_themes_m_d_ref` using the
    `fit_bayesian_model` function. This process leverages parallel
    processing for efficiency. Progress messages are displayed if
    `verbose` is `TRUE`.

4.  **Extract and Summarize Posteriors**: Extracts posterior draws from
    the fitted models and summarizes them using the
    `summarize_posterior_draws` function. The summary includes metrics
    such as alphas, betas, sigmas, active returns, tracking errors,
    information ratios, appraisal ratios, and Treynor ratios.

The Bayesian hierarchical model accounts for both theme-level and
signal-level variations, allowing for nuanced adjustments based on the
hierarchical structure of the data. Informative priors can be derived
from existing data or specified by the user, providing flexibility in
modeling approaches.
