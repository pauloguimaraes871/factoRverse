# Base Class for Hyperparameter Tuning Strategies

Common slots and validity rules shared by the three tuning-strategy
subclasses:
[`grid_search_strategy-class`](https://pauloguimaraes871.github.io/factoRverse/reference/grid_search_strategy-class.md),
[`random_search_strategy-class`](https://pauloguimaraes871.github.io/factoRverse/reference/random_search_strategy-class.md),
and
[`bayesian_opt_strategy-class`](https://pauloguimaraes871.github.io/factoRverse/reference/bayesian_opt_strategy-class.md).
Not normally instantiated directly; use
[`create_tuning_strategy()`](https://pauloguimaraes871.github.io/factoRverse/reference/create_tuning_strategy.md),
which builds the subclass matching `tuning_method` for you.

## Slots

- `tuning_method`:

  Character. One of `"grid_search"`, `"random_search"`, or
  `"bayesian_opt"` – fixed by the subclass's `prototype`, not meant to
  be set independently.

- `validation_sample_size`:

  Numeric. Size of the validation sample. A value in `(0, 1)` is treated
  as a proportion of the training sample size and rescaled by
  [`add_tuning_strategy()`](https://pauloguimaraes871.github.io/factoRverse/reference/add_tuning_strategy.md);
  a value `>= 1` is used as an absolute observation count.

- `chosen_eval_metric`:

  Character. The evaluation metric to optimize; required (despite the
  name, `NULL` is rejected by validity) and must be one of `"rss"`,
  `"rmse"`, `"cp"`, `"mae"`, `"mphe"`, `"mpe"`, `"mape"`, `"hr"`,
  `"mb"`. **Caveat:**
  [`check_inputs_sb_backtest()`](https://pauloguimaraes871.github.io/factoRverse/reference/check_inputs_sb_backtest.md),
  the gate used by
  [`run_sb_backtest()`](https://pauloguimaraes871.github.io/factoRverse/reference/run_sb_backtest.md),
  only accepts 8 of these 9 values – `"mb"` passes this class's validity
  but is rejected once the strategy is actually run.

- `hyper_grid_domain`:

  A
  [`hyper_grid_domain-class`](https://pauloguimaraes871.github.io/factoRverse/reference/hyper_grid_domain-class.md)
  object holding the hyperparameter search space. The required shape of
  its `hyperparameter_list` slot depends on `tuning_method`; see the
  relevant subclass.

- `early_stop`:

  Numeric or `NULL`. Epochs with no improvement before stopping early;
  only meaningful for `xgb`/`nn` algorithms.

## Validity

- `tuning_method` must be one of the three supported methods.

- `chosen_eval_metric` must be non-`NULL` and one of the nine values
  listed above.

- `hyper_grid_domain@hyperparameter_list` must match the shape required
  by `tuning_method` (see subclasses).
