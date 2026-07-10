# bayesian_model_parameters Class

A class encapsulating parameters necessary to specify the hierarchical
Bayesian model and its priors.

## Slots

- `user_priors`:

  An object of class `brmsprior` with user-defined priors for the
  hierarchical Bayesian model. Should be structured according to the
  `model_spec_theme_level`.

- `prior_derivation_control`:

  A list of additional parameters for deriving priors when `priors_type`
  is `"informative_exogenous_dataset"`. Should include:

  - `half_t_df`: Degrees of freedom for the half-t distribution applied
    to sd priors.

  - `lmer_optimizer`: Optimizer to be used in
    [`lme4::lmer`](https://rdrr.io/pkg/lme4/man/lmer.html) for deriving
    priors. Options include: `"nloptwrap"`, `"bobyqa"`, `"Nelder_Mead"`,
    `"nlminbwrap"`.

  - `lmer_optimization_objective`: Criteria to be optimized in
    [`lme4::lmer`](https://rdrr.io/pkg/lme4/man/lmer.html) for deriving
    priors. Options include: `likelihood`, `REML`.

- `brms_control`:

  A list of additional parameters to be passed to
  [`brms::brm`](https://paulbuerkner.com/brms/reference/brm.html) for
  MCMC sampling, including:

  - `chains`: Number of Markov chains to run (default is 4).

  - `iter`: Total number of iterations per chain (default is 2000).

  - `warmup`: Number of warmup iterations per chain (default is
    `floor(iter / 2)`).

  - `thin`: Thinning interval for MCMC sampling (default is 1).

  - `seed`: Seed for reproducibility (default is `NA` for random
    seeding).

  - `adapt_delta`: Target acceptance probability for the Hamiltonian
    Monte Carlo sampler (default is 0.99).
