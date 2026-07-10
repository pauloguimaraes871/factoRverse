# Random Search Tuning Strategy

Subclass of
[`tuning_strategy-class`](https://pauloguimaraes871.github.io/factoRverse/reference/tuning_strategy-class.md)
for random search: hyperparameter values are drawn from user-specified
distributions instead of being enumerated exhaustively.

## Slots

- `n_iter`:

  Numeric. Number of random draws generated per hyperparameter that has
  a distribution assigned (hyperparameters with a `"constant"`
  `distribution_choice` are exempt). Draws across different
  hyperparameters are then combined exhaustively, so for `n_iter = 5`
  and 2 non-constant hyperparameters the model is generally evaluated
  \\5^2 = 25\\ times.

## `hyper_grid_domain` shape

`hyper_grid_domain@hyperparameter_list` must be a list of named lists,
one per hyperparameter, each with:

- `distribution_choice`: one of `"normal"`, `"uniform"`, `"lognormal"`,
  or `"constant"`.

- `pars`: named numeric vector of distribution parameters
  (`c(mean, sd)`, `c(min, max)`, or `c(meanlog, sdlog)` as appropriate)
  – omitted when `distribution_choice = "constant"`.

- `value`: a single numeric value, present only when
  `distribution_choice = "constant"`.

## See also

[`tuning_strategy-class`](https://pauloguimaraes871.github.io/factoRverse/reference/tuning_strategy-class.md)
for the inherited slots.
