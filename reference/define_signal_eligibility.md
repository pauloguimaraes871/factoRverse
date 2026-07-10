# Define Signal Eligibility

This function evaluates the eligibility of signals for inclusion in the
investment universe based on various performance metrics and statistical
adjustments. It performs initial data checks, computes performance
metrics, applies statistical adjustments (both frequentist and
Bayesian), and classifies signals according to the specified selection
policy.

## Usage

``` r
define_signal_eligibility(
  selected_backtest_returns_corrected_positions_m_xts_upd_ref,
  selected_market_factor_proxy_m_xts_upd_ref,
  custom_signal_universe_metrics_m_upd_ref = NULL,
  p_correction_method = "none",
  signal_significance_threshold = 0.05,
  enable_theme_representativeness = TRUE,
  model_structure = "no_pooled",
  theme_level_intercept = NULL,
  theme_level_slope = NULL,
  lmer_control = list(lmer_optimizer = "nloptwrap", lmer_optimization_objective = "REML",
    hierarchical_p_value_method = "Satterthwaite"),
  active_returns = TRUE,
  priors_m_upd_ref = NULL,
  user_priors = NULL,
  brms_control = list(iter = 2000, chains = 4, thin = 1, seed = NA, adapt_delta = 0.99),
  prior_derivation_control = list(half_t_df = 30),
  selected_signal_themes_m_d_ref,
  lower_quantile_winsorization = 0.025,
  upper_quantile_winsorization = 0.975,
  verbose = TRUE,
  parallel = TRUE
)
```

## Arguments

- selected_backtest_returns_corrected_positions_m_xts_upd_ref:

  A `xts` object containing backtest returns for various signals.

- selected_market_factor_proxy_m_xts_upd_ref:

  A `xts` object containing benchmark returns data. The first column
  should be dates, and the subsequent columns should contain returns.

- custom_signal_universe_metrics_m_upd_ref:

  Optional meta-dataframe with custom metrics for signals, such as
  precomputed alpha, IR, etc.

- p_correction_method:

  The method for p-value correction. Possible options are:

  - `"none"`: No correction.

  - `"bayesian"`: Bayesian hierarchical model (via `brms`) is used for
    adjustment.

  - `"bonferroni"`, `"holm"`, `"hochberg"`, `"hommel"`: FWER-controlling
    methods.

  - `"BH"`, `"fdr"`, `"BY"`: FDR-controlling methods.

- signal_significance_threshold:

  A numeric value for the alpha significance level. If set to 1, all
  signals with valid alphas are selected. Signals are retained only if
  their CAPM alpha is statistically significant.

- enable_theme_representativeness:

  Logical. If `TRUE`, ensures at least one signal per theme is selected,
  even if no signal passes the significance threshold, by choosing the
  signal with the highest alpha t-stat.

- model_structure:

  A character string, either `"partial_pooled"` or `"no_pooled"`,
  indicating the type of model structure to use for mixed-effects
  estimation.

- theme_level_intercept:

  A string specifying intercept structure at the theme level: one of
  `"random"`, `"theme_specific"`, or `"fixed"`. Only relevant when
  `model_structure == "partial_pooled"`; combined with
  `theme_level_slope` to build `model_spec_theme_level` (see **Model
  Specifications at Theme Level** below).

- theme_level_slope:

  A string specifying slope structure at the theme level: one of
  `"fixed"` or `"theme_specific"`. Only relevant when
  `model_structure == "partial_pooled"`.

- lmer_control:

  A list of arguments passed to
  [`lme4::lmer`](https://rdrr.io/pkg/lme4/man/lmer.html). Expected
  components:

  - `lmer_optimizer`: Optimization algorithm used (e.g., `"nloptwrap"`,
    `"bobyqa"`, `"Nelder_Mead"`, `"nlminbwrap"`).

  - `lmer_optimization_objective`: Either `"REML"` or `"ML"`.

  - `hierarchical_p_value_method`: p-value calculation method (e.g.,
    `"Satterthwaite"`).

- active_returns:

  Logical. If `TRUE`, returns are adjusted by subtracting benchmark
  returns before computing performance metrics.

- priors_m_upd_ref:

  A meta-dataframe used to derive informative priors via
  [`derive_informative_priors_from_data()`](https://pauloguimaraes871.github.io/factoRverse/reference/derive_informative_priors_from_data.md).
  Must include columns: `id`, `tickers`, `dates`, `return`,
  `market_factor_proxy`, `theme`. Should not be provided if
  `user_priors` is set.

- user_priors:

  A list of
  [`brms::set_prior()`](https://paulbuerkner.com/brms/reference/set_prior.html)
  objects manually defined by the user. Only used when
  `priors_m_upd_ref` is `NULL`.

- brms_control:

  A list of parameters to control the Bayesian model fitting with
  [`brms::brm`](https://paulbuerkner.com/brms/reference/brm.html):

  - `chains`: Number of MCMC chains (default 4).

  - `iter`: Total iterations per chain (default 2000).

  - `warmup`: Warmup iterations per chain (default = `floor(iter / 2)`).

  - `thin`: Thinning interval (default 1).

  - `seed`: Random seed (default `NA`).

  - `adapt_delta`: Target acceptance rate for HMC sampler (default
    0.99).

- prior_derivation_control:

  A list of additional parameters for prior generation:

  - `half_t_df`: Degrees of freedom in half-t distribution for scale
    parameters (default 30).

- selected_signal_themes_m_d_ref:

  A meta-dataframe with columns `id`, `dates`, and `theme`. Specifies
  group membership for each signal. Should include data for the current
  date only.

- lower_quantile_winsorization:

  Numeric between 0 and 1. Lower quantile threshold for winsorization of
  signal metrics (e.g., alpha t-stats).

- upper_quantile_winsorization:

  Numeric between 0 and 1. Upper quantile threshold for winsorization of
  signal metrics.

- verbose:

  Logical. If `TRUE`, prints messages to the console.

- parallel:

  Logical. Enables parallel execution (only for Bayesian models).

## Value

A list with the following components:

- `signal_universe_m_d_ref`:

  A dataframe with computed performance metrics, adjusted p-values, and
  classification flags.

- `frequentist_results`:

  Results from the frequentist mixed-effects model (if applicable).

- `bayesian_results`:

  Results from the Bayesian model (if applicable).

## Details

When `priors_m_upd_ref` is provided, the function uses a frequentist
mixed-effects model to derive priors, later used in a Bayesian model:

- Priors for location parameters (e.g., alpha, beta) follow normal
  distributions.

- Priors for scale parameters (e.g., random effect std. devs) follow
  half-t distributions.

- Correlation priors follow an LKJ distribution.

## Model Specifications at Theme Level

`model_spec_theme_level` is built as
`paste0(theme_level_intercept, "_intercept_", theme_level_slope, "_slope")`
and must resolve to one of:

- `random_intercept_fixed_slope`:

  Random intercepts at the theme level, fixed (global) slope; random
  intercepts and slopes at the theme-signal level.

- `theme_specific_intercept_fixed_slope`:

  Fixed intercepts for each theme, fixed (global) slope; random
  intercepts and slopes at the theme-signal level.

- `theme_specific_intercept_theme_specific_slope`:

  Fixed intercepts and slopes for each theme; random intercepts and
  slopes at the theme-signal level.

- `fixed_intercept_fixed_slope`:

  A single fixed (global) intercept and slope; random intercepts and
  slopes at the theme-signal level only (no theme-level effect).
