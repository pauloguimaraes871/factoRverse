# Build the Evaluation Function for Hyperparameter Tuning

Factory returning the closure
[`hyper_tune`](https://pauloguimaraes871.github.io/factoRverse/reference/hyper_tune.md)
uses to score one hyperparameter candidate for a given ML algorithm. The
shape of the returned function depends on `tuning_method`, because
grid/random search and Bayesian optimization consume it differently.

## Usage

``` r
set_eval_function(ml_algorithm, tuning_method)
```

## Arguments

- ml_algorithm:

  Character, algorithm to build an evaluator for (`"glmnet"`, `"rf"`,
  `"xgb"`, `"nn"`).

- tuning_method:

  Character, one of `"grid_search"`, `"random_search"`,
  `"bayesian_opt"`; selects the calling convention.

## Value

A closure passed to
[`hyper_tune`](https://pauloguimaraes871.github.io/factoRverse/reference/hyper_tune.md):
a direct evaluator for grid/random search, or a data-capturing wrapper
for Bayesian optimization.

## Details

- **`"grid_search"` / `"random_search"`**: returns a function whose
  formals are the hyperparameters (plus data and eval-metric arguments),
  suitable for
  [`purrr::pmap()`](https://purrr.tidyverse.org/reference/pmap.html) /
  [`furrr::future_pmap()`](https://furrr.futureverse.org/reference/future_map2.html)
  over an expanded grid. It fits on the training sample, predicts on the
  validation sample, and returns the
  [`calculate_eval_metrics`](https://pauloguimaraes871.github.io/factoRverse/reference/calculate_eval_metrics.md)
  data frame (or the fitted model when `return_all_info = TRUE`).

- **`"bayesian_opt"`**: returns a *wrapper* that captures data /
  eval-metric arguments via `...`, then exposes an inner `fit()` taking
  only the hyperparameters and returning the named list of scalar scores
  expected by
  [`ParBayesianOptimization::bayesOpt()`](https://rdrr.io/pkg/ParBayesianOptimization/man/bayesOpt.html).

Supported algorithms: `"glmnet"`
([`glmnet::glmnet`](https://glmnet.stanford.edu/reference/glmnet.html)),
`"rf"`
([`ranger::ranger`](http://imbs-hl.github.io/ranger/reference/ranger.md);
`mtry` is treated as a proportion of predictors), `"xgb"`
([`xgboost::xgb.train`](https://rdrr.io/pkg/xgboost/man/xgb.train.html)),
`"nn"`
([`fit_keras_model`](https://pauloguimaraes871.github.io/factoRverse/reference/fit_keras_model.md)).

## See also

[`hyper_tune`](https://pauloguimaraes871.github.io/factoRverse/reference/hyper_tune.md),
[`calculate_eval_metrics`](https://pauloguimaraes871.github.io/factoRverse/reference/calculate_eval_metrics.md),
[`fit_keras_model`](https://pauloguimaraes871.github.io/factoRverse/reference/fit_keras_model.md)
