# Derive Informative Priors from Data

This function fits a frequentist hierarchical linear mixed-effects model
using the `lme4` package, based on the input data and the specified
model structure. The resulting parameter estimates are used to derive
informative priors for Bayesian modeling using the `brms` package.

## Usage

``` r
derive_informative_priors_from_data(
  priors_m_upd_ref,
  model_spec_theme_level,
  half_t_df = 30,
  lmer_optimizer = "nloptwrap",
  lmer_optimization_objective = "REML"
)
```

## Arguments

- priors_m_upd_ref:

  A (meta)data frame containing the following columns:

  - `id`: Identifier for each observation.

  - `tickers`: tickers corresponding to individual securities or
    instruments.

  - `dates`: Date of each observation.

  - `return`: Return of the signal at that date.

  - `market_factor_proxy`: Market factor proxy return at that date.

  - `theme`: Theme associated with each signal, used for clustering in
    the hierarchical Bayesian model (e.g., "value", "growth"). Each
    ticker must be uniquely linked to a single theme.

  The data frame should only include observations up to the current
  date.

- model_spec_theme_level:

  A character string indicating the structure of the hierarchical
  Bayesian model. This parameter controls the specification of
  parameters at the `theme` level, assuming tickers are uniquely nested
  within each theme. Options include:

  - `"random_intercept_fixed_slope"`: Random effects on the
    `theme`-level intercept, fixed (global) slope. Includes random
    intercepts for themes and both random intercepts and slopes for each
    theme-signal combination.

  - `"theme_specific_intercept_fixed_slope"`: Fixed intercepts for each
    `theme`, with a global slope for the market factor proxy. Nested
    variability within themes is modeled using random intercepts and
    slopes for theme-signal combinations.

  - `"theme_specific_intercept_theme_specific_slope"`: Fixed intercepts
    and slopes for each `theme`, via interaction terms between themes
    and the market factor proxy, plus random intercepts and slopes for
    theme-signal combinations.

  - `"fixed_intercept_fixed_slope"`: A single fixed (global) intercept
    and slope, with random intercepts and slopes for theme-signal
    combinations only (no theme-level fixed or random intercept/slope).

- half_t_df:

  A numeric indicating the degrees of freedom in the half-t distribution
  to be applied to model random effects. Although the function specifies
  a regular student t distribution, `brms` will use half-t distribution,
  ensuring strictly positive parameters.

- lmer_optimizer:

  A character string specifying the optimizer to be used in the
  [`lme4::lmer`](https://rdrr.io/pkg/lme4/man/lmer.html) function. It
  will be passed to lme4::lmerControl, which will be used in the
  [`lme4::lmer`](https://rdrr.io/pkg/lme4/man/lmer.html) function.
  Options include: 'nloptwrap', 'bobyqa', 'Nelder_Mead' or 'nlminbwrap'

- lmer_optimization_objective:

  A character string indicating whether estimates should be chosen to
  optimize the 'REML' criterion or the 'likelihood'.

## Value

A list with two components:

- `priors`: A list of
  [`brms::set_prior`](https://paulbuerkner.com/brms/reference/set_prior.html)
  objects specifying the derived priors.

- `model`: The fitted linear mixed-effects model
  ([`lme4::lmer`](https://rdrr.io/pkg/lme4/man/lmer.html) object).

## Details

The function uses frequentist linear mixed-effects models to estimate
parameters that are subsequently translated into Bayesian priors:

- Priors for location parameters (e.g., intercepts, slopes) follow a
  normal distribution.

- Priors for scale parameters (e.g., random effect standard deviations)
  follow a half-t distribution.

- Correlation priors for random effects are modeled using the LKJ
  (Lewandowski-Kurowicka-Joe) distribution.

### Model Specifications at Theme Level

#### `random_intercept_fixed_slope`

This model includes:

- Fixed intercept and slope for the market factor proxy.

- Random intercepts at the `theme` level.

- Random intercepts and slopes for each theme-signal combination.

The model equation is: \$\$y_i = \beta_0 + \beta_1 \cdot x_i +
b\_{0,t_i} + b\_{0,g_i} + b\_{1,g_i} \cdot x_i + \epsilon_i\$\$ See the
detailed breakdown in the example section.

#### `theme_specific_intercept_fixed_slope`

This model includes:

- Fixed intercepts for each `theme`, expressed as a summation over all
  themes.

- A global fixed slope for the market factor proxy.

- Random intercepts and slopes for theme-signal combinations.

The model equation is: \$\$y\_{i} = \sum\_{k} \beta\_{k} \cdot
\text{theme}\_{k,i} + \beta\_{m} \cdot x\_{i} + b\_{0,g\_{i}} +
b\_{1,g\_{i}} \cdot x\_{i} + \epsilon\_{i}\$\$

#### `theme_specific_intercept_theme_specific_slope`

This model includes:

- Fixed intercepts and slopes for each `theme`.

- Interaction terms between themes and the market factor proxy (no
  separate global slope term).

- Random intercepts and slopes for theme-signal combinations.

The model equation is: \$\$y\_{it} = \sum_k \beta_k \cdot
\text{theme}\_{k,i} + \sum_k \gamma_k \cdot \text{theme}\_{k,i} \cdot
x\_{it} + b\_{0,g\_{i}} + b\_{1,g\_{i}} \cdot x\_{it} +
\epsilon\_{it}\$\$

#### `fixed_intercept_fixed_slope`

This model includes:

- A single fixed (global) intercept and slope for the market factor
  proxy (no theme-level fixed effect).

- Random intercepts and slopes for theme-signal combinations.

The model equation is: \$\$y\_{it} = \beta_0 + \beta_1 \cdot x\_{it} +
b\_{0,g\_{i}} + b\_{1,g\_{i}} \cdot x\_{it} + \epsilon\_{it}\$\$
