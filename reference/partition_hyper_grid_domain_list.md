# Split a Bayesian-Optimization Domain into Searched and Fixed Hyperparameters

Internal helper. Separates the entries of a `bayesian_opt`
hyperparameter domain into those the surrogate should search over and
those the user pinned to a constant.

## Usage

``` r
partition_hyper_grid_domain_list(hyper_grid_domain_list)
```

## Arguments

- hyper_grid_domain_list:

  A named list. Each entry is either a numeric vector of length 2 giving
  `c(lower, upper)` bounds, or a list with
  `distribution_choice = "constant"` and a single numeric `value`.

## Value

A list with two elements:

- searched:

  Named list of length-2 numeric bounds, to be passed as `bounds`.
  Retains the order in which the hyperparameters were declared.

- fixed:

  Named list of single numeric values, to be spliced into the learner
  call and appended to the optimal hyperparameters.

## Details

A constant cannot be expressed to `ParBayesianOptimization` as a
zero-width bound. Candidates are rescaled by `(upper - lower)`, so
`c(x, x)` produces an all-`NaN` column with no error and no warning, and
`gsPoints` defaults to `pmax(100, length(bounds)^3)`, so the pinned
hyperparameter still costs a full dimension of search. Holding a
hyperparameter constant therefore means removing it from the surrogate's
input space and re-inserting its value only when the learner is called.

Constants use the same shape `random_search` already accepts, a list
with `distribution_choice = "constant"` and a single numeric `value`, so
one configuration reads the same way under either tuning method.
