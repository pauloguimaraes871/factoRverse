# Add Bayesian Model Parameters

Generic function to add Bayesian model parameters.

## Usage

``` r
add_bayesian_model_parameters(
  object,
  user_priors = NULL,
  prior_derivation_control = NULL,
  brms_control = NULL
)

# S4 method for class 'bayesian_alpha_test_strategy'
add_bayesian_model_parameters(
  object,
  user_priors = NULL,
  prior_derivation_control = NULL,
  brms_control = NULL
)

# S4 method for class 'ss_backtest_config'
add_bayesian_model_parameters(
  object,
  user_priors = NULL,
  prior_derivation_control = NULL,
  brms_control = NULL
)
```

## Arguments

- object:

  The object to which Bayesian model parameters will be added.

- user_priors:

  An optional object of class `brmsprior`.

- prior_derivation_control:

  An optional list containing prior derivation control parameters.

- brms_control:

  An optional list of parameters for
  [`brms::brm`](https://paulbuerkner.com/brms/reference/brm.html).

## Value

The updated object with the `bayesian_model_parameters` added.
