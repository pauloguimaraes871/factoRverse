# Get Bayesian Model Parameters

Extracts the `bayesian_model_parameters` from an object (e.g. a
`bayesian_alpha_test_strategy` or an `ss_backtest_config` holding a
Bayesian strategy).

## Usage

``` r
get_bayesian_model_parameters(object)

# S4 method for class 'bayesian_alpha_test_strategy'
get_bayesian_model_parameters(object)

# S4 method for class 'ss_backtest_config'
get_bayesian_model_parameters(object)
```

## Arguments

- object:

  An S4 object, typically `bayesian_alpha_test_strategy` or
  `ss_backtest_config`.

## Value

An object of class `bayesian_model_parameters`.

## Functions

- `get_bayesian_model_parameters(bayesian_alpha_test_strategy)`: Extract
  parameters from a `bayesian_alpha_test_strategy`.

- `get_bayesian_model_parameters(ss_backtest_config)`: Extract
  parameters from an `ss_backtest_config`, if it has a Bayesian alpha
  test strategy.
