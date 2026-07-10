# Create an alpha_test_strategy object

Constructor for objects of class `alpha_test_strategy` or its subclasses
(`frequentist_alpha_test_strategy`, `bayesian_alpha_test_strategy`). The
function validates the model configuration and constructs the
appropriate strategy object based on the hypothesis testing methodology
and hierarchical model structure.

## Usage

``` r
create_alpha_test_strategy(
  model_structure = "no_pooled",
  theme_level_intercept = NULL,
  theme_level_slope = NULL,
  signal_significance_threshold = 0.05,
  p_correction_method = "none",
  market_factor_proxy,
  bayesian_model_parameters = NULL,
  enable_theme_representativeness = TRUE,
  lmer_control = NULL
)
```

## Arguments

- model_structure:

  Character string specifying the hierarchical model structure. Must be
  one of:

  - `"partial_pooled"`: Uses theme-level effects with partial pooling.

  - `"no_pooled"`: No pooling; estimates signal-level parameters
    independently.

- theme_level_intercept:

  Character string indicating how intercepts are modeled at the theme
  level (only used when `model_structure = "partial_pooled"`). Valid
  options are:

  - `"fixed"`: A common intercept across themes.

  - `"random"`: Intercepts modeled as random effects.

  - `"theme_specific"`: Each theme has its own fixed intercept.

  Must be `NULL` if `model_structure = "no_pooled"`.

- theme_level_slope:

  Character string indicating how slopes are modeled at the theme level
  (only used when `model_structure = "partial_pooled"`). Valid options
  are:

  - `"fixed"`: A common slope across themes.

  - `"theme_specific"`: Each theme has its own slope.

  Must be `NULL` if `model_structure = "no_pooled"`.

- signal_significance_threshold:

  Numeric. Significance level for alpha tests (e.g., 0.05). Determines
  the rejection region for the null hypothesis of zero alpha. Must be
  between 0 and 1.

- p_correction_method:

  Character string specifying the p-value adjustment method used in
  multiple hypothesis testing correction. Options include:

  - `"none"`: No correction.

  - `"bonferroni"`, `"holm"`, `"hochberg"`, `"hommel"`: Classical FWER
    control methods.

  - `"BH"`, `"fdr"`, `"BY"`: FDR control methods.

  - `"bayesian"`: Use Bayesian hypothesis testing instead of p-values.

- market_factor_proxy:

  Character string specifying the identifier of the market factor proxy.
  This variable is typically used as the market return in a CAPM-style
  alpha test model.

- bayesian_model_parameters:

  An optional object of class `bayesian_model_parameters`. If
  `p_correction_method = "bayesian"`, this must either be provided
  explicitly or will be initialized with default (uninformative) prior
  settings. Ignored for frequentist strategies.

- enable_theme_representativeness:

  Logical. If `TRUE`, enables extra diagnostics and modeling logic to
  account for representativeness at the theme level. Useful when themes
  are considered key grouping variables for inference.

- lmer_control:

  Optional. A list of control parameters passed to
  [`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html) (frequentist)
  or [`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html)
  (Bayesian) for customizing model fitting behavior. Can include
  convergence tolerances, optimizers, or verbosity flags.

## Value

An object of class:

- `frequentist_alpha_test_strategy`, if a frequentist method is
  selected;

- `bayesian_alpha_test_strategy`, if Bayesian inference is selected.

The returned object can then be passed to downstream workflows that
evaluate and classify alpha signals.
