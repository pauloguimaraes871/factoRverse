# Add Prior to Bayesian Alpha Test Strategy

Adds a prior to a `bayesian_alpha_test_strategy` object based on the
specified effect, type, distribution, and parameters.

## Usage

``` r
add_brms_prior(object, ...)

# S4 method for class 'bayesian_alpha_test_strategy'
add_brms_prior(
  object,
  effect,
  type,
  theme = NULL,
  distribution_choice,
  pars,
  level = "signals"
)

# S4 method for class 'ss_backtest_config'
add_brms_prior(
  object,
  effect,
  type,
  theme = NULL,
  distribution_choice,
  pars,
  level = "signals"
)
```

## Arguments

- object:

  An object of class `bayesian_alpha_test_strategy`.

- ...:

  Additional arguments.

- effect:

  A character string specifying the effect of the prior. Must be one of
  "fixed" or "random".

- type:

  A character string specifying the type of prior. Options: "intercept",
  "slope", "sigma", or "cor".

- theme:

  A character vector specifying themes (e.g., "value", "momentum").
  Applicable for fixed effects.

- distribution_choice:

  A character vector specifying the distribution of the prior (e.g.,
  "normal", "student_t").

- pars:

  A list of named numeric vectors specifying parameters for the chosen
  distribution.

- level:

  A character string specifying the hierarchical level for random
  effects. Options: "signals" (default), "tickers", or "theme".

## Value

An updated `bayesian_alpha_test_strategy` object with the added prior.

## Details

- For `effect = "fixed"`, you can specify global intercepts or slopes,
  or theme-specific priors using the `theme` argument.

- For `effect = "random"`, you can define hierarchical priors with
  `level` specifying the grouping structure.

Supported `distribution_choice` options include:

- `"normal"`: Requires `pars` with `mean` and `sd`.

- `"student_t"`: Requires `pars` with `df`, `mean`, and `sd`.

- `"lkj"`: Requires `pars` with `eta` (only for `type = "cor"`).
