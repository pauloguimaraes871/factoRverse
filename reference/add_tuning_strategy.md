# Add a `tuning_strategy` to an Existing `sb_backtest_config`

Attaches a hyperparameter tuning strategy to a `sb_backtest_config`,
either by supplying an existing `tuning_strategy` object or by supplying
the parameters needed to build one on the fly.

## Usage

``` r
add_tuning_strategy(object, tuning_strategy, ...)

# S4 method for class 'sb_backtest_config,tuning_strategy'
add_tuning_strategy(object, tuning_strategy)

# S4 method for class 'sb_backtest_config,missing'
add_tuning_strategy(
  object,
  tuning_strategy = NULL,
  tuning_method,
  validation_sample_size,
  chosen_eval_metric = NULL,
  hyper_grid_domain = NULL,
  early_stop = NULL,
  n_iter = NULL,
  acq = "ucb",
  init_points = NULL,
  k_iter = NULL
)
```

## Arguments

- object:

  A `sb_backtest_config` object.

- tuning_strategy:

  `NULL`; a new `tuning_strategy` is created from the remaining
  arguments.

- ...:

  Parameters forwarded to
  [`create_tuning_strategy()`](https://pauloguimaraes871.github.io/factoRverse/reference/create_tuning_strategy.md)
  when `tuning_strategy` is missing (`tuning_method`,
  `validation_sample_size`, `chosen_eval_metric`, `hyper_grid_domain`,
  `early_stop`, `n_iter`, `acq`, `init_points`, `k_iter`).

- tuning_method:

  Character string indicating the hyperparameter tuning method. Must be
  one of 'grid_search', 'random_search', or 'bayesian_opt'.

- validation_sample_size:

  Numeric value representing the size of the validation sample.

- chosen_eval_metric:

  Character or `NULL`; see Details. If provided, must be one of `"rss"`,
  `"rmse"`, `"cp"`, `"mae"`, `"mphe"`, `"mpe"`, `"mape"`, `"hr"`,
  `"mb"`.

- hyper_grid_domain:

  An object of class `hyper_grid_domain`, or `NULL` to start with an
  empty one.

- early_stop:

  Optional, stopping criteria for early termination. Can be of any type.

- n_iter:

  Numeric, number of iterations for 'random_search' or 'bayesian_opt'.

- acq:

  Character string specifying the acquisition function for Bayesian
  optimization (for 'bayesian_opt' only). Defaults to `"ucb"`.

- init_points:

  Numeric, number of initial random points for Bayesian optimization
  (for 'bayesian_opt' only).

- k_iter:

  Numeric, number of samples to evaluate during Bayesian optimization
  (for 'bayesian_opt' only).

## Value

An updated `sb_backtest_config` object with the specified or newly
created `tuning_strategy`.

The updated `sb_backtest_config` object with the provided
`tuning_strategy`.

An updated `sb_backtest_config` object with a newly created
`grid_search_strategy`, `random_search_strategy`, or
`bayesian_opt_strategy`, depending on the selected `tuning_method`.

## Functions

- `add_tuning_strategy( object = sb_backtest_config, tuning_strategy = tuning_strategy )`:
  Add an existing `tuning_strategy` to the `sb_backtest_config`.

  Replaces any existing tuning strategy. If
  `tuning_strategy@validation_sample_size` is in `(0, 1)`, it is
  rescaled to an absolute count as
  `round(validation_sample_size * object@training_sample_size)` before
  being stored. Only blocks `sb_algorithm == "ols"`; unlike the sibling
  method below, it does not check for other heuristic (non-ML)
  algorithms.

- `add_tuning_strategy(object = sb_backtest_config, tuning_strategy = missing)`:
  Create and add a new `tuning_strategy` to the `sb_backtest_config`.

  Builds a `tuning_strategy` via
  [`create_tuning_strategy()`](https://pauloguimaraes871.github.io/factoRverse/reference/create_tuning_strategy.md)
  and attaches it. If `chosen_eval_metric` is `NULL`, it is inferred
  from `object@custom_objective` (`"pseudo_huber_error"` -\> `"mphe"`,
  `"quantile_error"` -\> `"quantile_loss"`, `"absolute_error"` -\>
  `"mae"`, otherwise `"rmse"`). `validation_sample_size` in `(0, 1)` is
  rescaled the same way as in the `tuning_strategy`-signature method.
  Errors if `object@sb_algorithm` is a heuristic (non-ML) algorithm that
  does not require tuning.
