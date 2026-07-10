# Show Method for `bayesian_opt_strategy`

Custom show method for displaying information about objects of class
`bayesian_opt_strategy`. This method will display the tuning method,
machine learning algorithm, validation sample size, and details specific
to Bayesian optimization such as `n_iter`, acquisition function (`acq`),
initial points, and hyperparameter bounds.

## Usage

``` r
# S4 method for class 'bayesian_opt_strategy'
show(object)
```

## Arguments

- object:

  An object of class `bayesian_opt_strategy`.

## Value

Printed information about the object.
