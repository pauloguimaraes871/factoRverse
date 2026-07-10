# Grid Search Tuning Strategy

Subclass of
[`tuning_strategy-class`](https://pauloguimaraes871.github.io/factoRverse/reference/tuning_strategy-class.md)
for grid search: every combination of user-supplied hyperparameter
values is evaluated exhaustively (see
[`hyper_tune()`](https://pauloguimaraes871.github.io/factoRverse/reference/hyper_tune.md)).

## `hyper_grid_domain` shape

`hyper_grid_domain@hyperparameter_list` must be a list of named numeric
vectors, one per hyperparameter, each holding the exact values to try
(e.g. `list(alpha = c(0, 0.5, 1))`).

## See also

[`tuning_strategy-class`](https://pauloguimaraes871.github.io/factoRverse/reference/tuning_strategy-class.md)
for the inherited slots (`validation_sample_size`, `chosen_eval_metric`,
`hyper_grid_domain`, `early_stop`).
