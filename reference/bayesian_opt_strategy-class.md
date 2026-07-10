# Bayesian Optimization Tuning Strategy

Subclass of
[`tuning_strategy-class`](https://pauloguimaraes871.github.io/factoRverse/reference/tuning_strategy-class.md)
for Bayesian optimization via
[`ParBayesianOptimization::bayesOpt()`](https://rdrr.io/pkg/ParBayesianOptimization/man/bayesOpt.html).

## Slots

- `n_iter`:

  Numeric. Number of evaluations after the initialization phase.

- `acq`:

  Character. Acquisition function: `"ucb"` (default), `"ei"`, or
  `"poi"`.

- `init_points`:

  Numeric. Number of initial random points; must exceed the number of
  hyperparameters being tuned.

- `k_iter`:

  Numeric. Number of times to sample the scoring function per epoch.
  Must be `<= n_iter`, and, when running in parallel, is best set to a
  multiple of the number of cores (and preferably of `n_iter`).

## `hyper_grid_domain` shape

`hyper_grid_domain@hyperparameter_list` must be a list of named numeric
vectors of length 2, one per hyperparameter, giving its
`c(lower, upper)` bounds.

## Validity

In addition to the checks inherited from
[`tuning_strategy-class`](https://pauloguimaraes871.github.io/factoRverse/reference/tuning_strategy-class.md),
`acq` must be one of `"ucb"`, `"ei"`, or `"poi"`.

## See also

[`tuning_strategy-class`](https://pauloguimaraes871.github.io/factoRverse/reference/tuning_strategy-class.md)
for the inherited slots.
