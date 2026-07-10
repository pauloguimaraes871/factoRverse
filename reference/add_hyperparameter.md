# Add a Hyperparameter to a `hyper_grid_domain`, whether inside a `sb_backtest_config`, a `tuning_strategy` or on its own.

Adds (or, for a name already present, upserts) one or more
hyperparameters in a `hyper_grid_domain`, dispatching on where it lives:
a bare `hyper_grid_domain`, a `tuning_strategy` subclass
(`grid_search_strategy`, `random_search_strategy`,
`bayesian_opt_strategy`), or a `sb_backtest_config`.

## Usage

``` r
add_hyperparameter(object, hyperparameter, ...)

# S4 method for class 'hyper_grid_domain'
add_hyperparameter(object, new_hyperparameter_list)

# S4 method for class 'grid_search_strategy'
add_hyperparameter(object, hyperparameter, grid, ...)

# S4 method for class 'random_search_strategy'
add_hyperparameter(object, hyperparameter, distribution_choice, pars, ...)

# S4 method for class 'bayesian_opt_strategy'
add_hyperparameter(object, hyperparameter, bounds, ...)

# S4 method for class 'sb_backtest_config'
add_hyperparameter(
  object,
  hyperparameter,
  grid = NULL,
  distribution_choice = NULL,
  pars = NULL,
  bounds = NULL
)
```

## Arguments

- object:

  A `hyper_grid_domain`, a `tuning_strategy` (or subclass:
  `grid_search_strategy`, `random_search_strategy`,
  `bayesian_opt_strategy`), or a `sb_backtest_config` object.

- hyperparameter:

  A character vector naming the hyperparameter(s) to add. Options are:

  - **glmnet**: alpha, lambda.min.ratio

  - **rf**: mtry, num.trees, max.depth, min.bucket

  - **xgb**: min_child_weight, max_depth, subsample, colsample_bytree,
    eta, gamma, nrounds

  - **nn**: regularizer_l1, regularizer_l2, droprate, lr, size_of_batch,
    number_of_epochs

- ...:

  Method-specific arguments: `grid` (grid search),
  `distribution_choice`/`pars` (random search), `bounds` (Bayesian
  optimization), or `new_hyperparameter_list` (bare `hyper_grid_domain`)
  – see the individual methods below.

- new_hyperparameter_list:

  A named list of already-shaped hyperparameter entries. Names matching
  existing entries in `object@hyperparameter_list` overwrite them; other
  existing names are preserved.

- grid:

  A numeric vector (single hyperparameter) or list of numeric vectors
  (one per name in `hyperparameter`, same order) giving the exact values
  to try.

- distribution_choice:

  Character vector, one of `"uniform"`, `"normal"`, `"lognormal"`, or
  `"constant"` per hyperparameter in `hyperparameter`.

- pars:

  A named numeric vector (or list of them, one per hyperparameter) with
  the parameters for the chosen distribution (`c(min, max)`,
  `c(mean, sd)`, `c(meanlog, sdlog)`), or the constant value itself when
  `distribution_choice = "constant"`.

- bounds:

  A numeric vector of length 2 (`c(lower, upper)`), or a list of such
  vectors (one per hyperparameter in `hyperparameter`, same order).

## Value

The input object with `hyperparameter_list` (nested inside
`hyper_grid_domain` for the strategy/config methods) updated: names
shared with the existing list are overwritten, others are added, and
everything already present under an unrelated name is left untouched.

## Functions

- `add_hyperparameter(hyper_grid_domain)`: Upsert entries directly into
  a `hyper_grid_domain`'s `hyperparameter_list`.

  This is the merge primitive the
  `grid_search_strategy`/`random_search_strategy`/
  `bayesian_opt_strategy` methods below delegate to after building a
  properly shaped `new_hyperparameter_list` from their own
  `hyperparameter`/`grid`/ `distribution_choice`/`pars`/`bounds`
  arguments. It can also be called directly if you already have the
  hyperparameter list in the shape
  [`tuning_strategy-class`](https://pauloguimaraes871.github.io/factoRverse/reference/tuning_strategy-class.md)
  expects for the target `tuning_method`.

- `add_hyperparameter(grid_search_strategy)`: Add/upsert
  hyperparameter(s) in a `grid_search_strategy`'s `hyper_grid_domain`.

- `add_hyperparameter(random_search_strategy)`: Add/upsert
  hyperparameter(s) in a `random_search_strategy`'s `hyper_grid_domain`.

  There is no separate `value` argument for
  `distribution_choice = "constant"`: pass the constant through `pars`
  (it is stored internally as `value`).

- `add_hyperparameter(bayesian_opt_strategy)`: Add/upsert
  hyperparameter(s) in a `bayesian_opt_strategy`'s `hyper_grid_domain`.

- `add_hyperparameter(sb_backtest_config)`: Add/upsert hyperparameter(s)
  via a `sb_backtest_config`'s attached `tuning_strategy`.

  Delegates to whichever strategy-specific method matches
  `object@tuning_strategy`'s class; supply only the argument(s) relevant
  to that strategy's `tuning_method` (`grid`,
  `distribution_choice`/`pars`, or `bounds`) and leave the rest `NULL`.
  Requires `object@tuning_strategy` to already be set (see
  [`add_tuning_strategy()`](https://pauloguimaraes871.github.io/factoRverse/reference/add_tuning_strategy.md)).
