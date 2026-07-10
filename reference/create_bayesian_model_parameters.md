# Create Bayesian Model Parameters

Constructor for an S4 object of class `bayesian_model_parameters`.

## Usage

``` r
create_bayesian_model_parameters(
  user_priors = NULL,
  prior_derivation_control = NULL,
  brms_control = list(chains = 4, iter = 2000, warmup = 1000, thin = 1, seed = NA,
    adapt_delta = 0.8)
)
```

## Arguments

- user_priors:

  An object of class `brmsprior`, or `NULL`. Structured according to the
  `model_spec_theme_level`.

- prior_derivation_control:

  A list of additional parameters for deriving priors when `priors_type`
  is `"informative_exogenous_dataset"`. Must be a list with the
  following elements:

  - `half_t_df`: Degrees of freedom for the half-t distribution applied
    to sd priors.

  - `lmer_optimizer`: Optimizer to be used in
    [`lme4::lmer`](https://rdrr.io/pkg/lme4/man/lmer.html) for deriving
    priors.

  - `lmer_optimization_objective`: Criteria to be optimized in
    [`lme4::lmer`](https://rdrr.io/pkg/lme4/man/lmer.html) for deriving
    priors, e.g. `"likelihood"` or `"REML"`.

- brms_control:

  A list of parameters to be passed to
  [`brms::brm`](https://paulbuerkner.com/brms/reference/brm.html) for
  MCMC sampling, including:

  - `chains`: Number of Markov chains (default is 4).

  - `iter`: Number of iterations per chain (default is 2000).

  - `warmup`: Number of warmup iterations per chain (default is
    `floor(iter / 2)`).

  - `thin`: Thinning interval (default is 1).

  - `seed`: Seed for reproducibility (default is `NA`).

  - `adapt_delta`: Target acceptance probability for HMC (default is
    0.80).

## Value

An S4 object of class `bayesian_model_parameters`.
