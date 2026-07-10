# Show Bayesian Alpha Test Strategy

Prints the contents of a `bayesian_alpha_test_strategy` object: a
header, followed by the inherited `alpha_test_strategy` display, plus a
`bayesian_model_parameters` section (delegated to its own `show`
method), or a "No Bayesian Model Parameters set" message if `NULL`.

## Usage

``` r
# S4 method for class 'bayesian_alpha_test_strategy'
show(object)
```

## Arguments

- object:

  A `bayesian_alpha_test_strategy` object to be displayed.
