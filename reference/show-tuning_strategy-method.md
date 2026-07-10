# Show Method for `tuning_strategy`

Custom show method for displaying the general information of objects
that extend `tuning_strategy`. This method prints the tuning method,
machine learning algorithm, validation sample size, split method,
evaluation metric, early stopping criteria, and the hyperparameter grid
domain.

## Usage

``` r
# S4 method for class 'tuning_strategy'
show(object)
```

## Arguments

- object:

  An object of class `tuning_strategy` or its subclasses
  (`grid_search_strategy`, `random_search_strategy`, or
  `bayesian_opt_strategy`).

## Value

Printed information about the base properties of the object.
