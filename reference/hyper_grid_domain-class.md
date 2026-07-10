# Define the `hyper_grid_domain` S4 Class

Holds the set of hyperparameters (and, once populated by
[`add_hyperparameter()`](https://pauloguimaraes871.github.io/factoRverse/reference/add_hyperparameter.md),
their tuning-method-specific values) that
[`hyper_tune()`](https://pauloguimaraes871.github.io/factoRverse/reference/hyper_tune.md)
– invoked internally by
[`run_sb_backtest()`](https://pauloguimaraes871.github.io/factoRverse/reference/run_sb_backtest.md)
– will search over. This class only validates that hyperparameter
*names* are recognized; it does not check the *shape* of their values (a
numeric vector, a distribution spec, or a bounds pair) – that shape
check belongs to
[`tuning_strategy-class`](https://pauloguimaraes871.github.io/factoRverse/reference/tuning_strategy-class.md)
and depends on the chosen `tuning_method`.

## Slots

- `hyperparameter_list`:

  A named list, one entry per hyperparameter, drawn from:

  - **glmnet**: `alpha`, `lambda.min.ratio`

  - **rf**: `mtry`, `num.trees`, `max.depth`, `min.bucket`

  - **xgb**: `min_child_weight`, `max_depth`, `subsample`,
    `colsample_bytree`, `eta`, `gamma`, `nrounds`

  - **nn**: `regularizer_l1`, `regularizer_l2`, `droprate`, `lr`,
    `size_of_batch`, `number_of_epochs`
