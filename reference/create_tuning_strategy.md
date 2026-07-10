# Hyperparameter Tuning Strategy Constructor

Builds a
[`grid_search_strategy-class`](https://pauloguimaraes871.github.io/factoRverse/reference/grid_search_strategy-class.md),
[`random_search_strategy-class`](https://pauloguimaraes871.github.io/factoRverse/reference/random_search_strategy-class.md),
or
[`bayesian_opt_strategy-class`](https://pauloguimaraes871.github.io/factoRverse/reference/bayesian_opt_strategy-class.md)
object, depending on `tuning_method`.

## Usage

``` r
create_tuning_strategy(
  tuning_method,
  validation_sample_size,
  chosen_eval_metric,
  hyper_grid_domain = NULL,
  early_stop = NULL,
  n_iter = NULL,
  acq = NULL,
  init_points = NULL,
  k_iter = NULL
)
```

## Arguments

- tuning_method:

  Character. One of `"grid_search"`, `"random_search"`, or
  `"bayesian_opt"`.

- validation_sample_size:

  Numeric. Size of the validation sample. A value in `(0, 1)` is later
  treated as a training-sample proportion by
  [`add_tuning_strategy()`](https://pauloguimaraes871.github.io/factoRverse/reference/add_tuning_strategy.md);
  this constructor stores it as-is.

- chosen_eval_metric:

  Character. Evaluation metric to optimize; must be one of `"rss"`,
  `"rmse"`, `"cp"`, `"mae"`, `"mphe"`, `"mpe"`, `"mape"`, `"hr"`,
  `"mb"`. Required – the resulting object's validity rejects a missing
  value.

- hyper_grid_domain:

  A
  [`hyper_grid_domain-class`](https://pauloguimaraes871.github.io/factoRverse/reference/hyper_grid_domain-class.md)
  object. If `NULL`, an empty one is created; populate it via
  [`add_hyperparameter()`](https://pauloguimaraes871.github.io/factoRverse/reference/add_hyperparameter.md)
  before use.

- early_stop:

  Numeric or `NULL`. Epochs with no improvement before stopping early;
  only meaningful for `xgb`/`nn`.

- n_iter:

  Numeric. Required for `"random_search"` (draws per hyperparameter) and
  `"bayesian_opt"` (evaluations after initialization); ignored for
  `"grid_search"`.

- acq:

  Character. Acquisition function; required when
  `tuning_method = "bayesian_opt"`, unused otherwise.

- init_points:

  Numeric. Required when `tuning_method = "bayesian_opt"`, unused
  otherwise.

- k_iter:

  Numeric. Required when `tuning_method = "bayesian_opt"`, unused
  otherwise.

## Value

An object of class `grid_search_strategy`, `random_search_strategy`, or
`bayesian_opt_strategy`.
