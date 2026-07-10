# Perform Hyperparameter Tuning for Machine Learning Models

This function tunes hyperparameters of machine learning models using
grid search, random search, or Bayesian optimization. It evaluates
different hyperparameter combinations and returns the best-performing
set, selected by maximizing the `Score` column from
[`calculate_eval_metrics()`](https://pauloguimaraes871.github.io/factoRverse/reference/calculate_eval_metrics.md).

## Usage

``` r
hyper_tune(
  tuning_method,
  ml_algorithm,
  target_fwd_name,
  full_data_training_sample_clean,
  features_validation_sample,
  target_validation_sample,
  eval_function,
  custom_objective_translated,
  chosen_eval_metric_translated,
  early_stop,
  chosen_eval_metric,
  huber_delta,
  quantile_tau,
  hyper_grid_domain_list,
  n_iter,
  init_points,
  k_iter,
  acq,
  keras_architecture_parameters,
  parallel,
  verbose
)
```

## Arguments

- tuning_method:

  A character string specifying the tuning method. Possible values are
  `"random_search"`, `"grid_search"`, or `"bayesian_opt"`.

- ml_algorithm:

  A character string indicating the machine learning algorithm to be
  used.

- target_fwd_name:

  A character string specifying the name of the target variable in
  forward prediction.

- full_data_training_sample_clean:

  A data frame containing the clean training sample data.

- features_validation_sample:

  A data frame with feature values for validation.

- target_validation_sample:

  A data frame with target values for validation.

- eval_function:

  A function that computes the evaluation metric for given
  hyperparameters.

- custom_objective_translated:

  An optional custom objective function to be used in tuning.

- chosen_eval_metric_translated:

  The evaluation metric to be used internally for model performance
  assessment.

- early_stop:

  An integer specifying the number of iterations with no improvement
  before stopping early.

- chosen_eval_metric:

  A character string specifying the evaluation metric to be used.

- huber_delta:

  A numeric value for the Huber loss delta parameter, applicable if
  using Huber loss.

- quantile_tau:

  A numeric value for the quantile parameter, applicable if using
  quantile loss.

- hyper_grid_domain_list:

  A list specifying the domain of hyperparameters to search over.

- n_iter:

  An integer specifying the number of iterations for grid or random
  search or Bayesian optimization.

- init_points:

  An integer specifying the number of initial random points for Bayesian
  optimization.

- k_iter:

  An integer specifying the number of iterations for scoring function
  sampling in Bayesian optimization.

- acq:

  A character string specifying the acquisition function for Bayesian
  optimization.

- keras_architecture_parameters:

  A list of parameters specifying the architecture of the Keras model.

- parallel:

  A logical indicating whether to evaluate candidates in parallel
  (future backend).

- verbose:

  A logical indicating whether to print timing and progress information.

## Value

A list containing:

- chosen_eval_metric_validation_current_date:

  A data frame of evaluation metrics for each set of hyperparameters
  tried.

- optimal_hyper:

  A named vector of the optimal hyperparameter values found.

- validation_eval_metrics_hyper_choice_current_date:

  A named vector of evaluation metrics corresponding to the optimal
  hyperparameter set.

## See also

[`set_eval_function()`](https://pauloguimaraes871.github.io/factoRverse/reference/set_eval_function.md),
[`calculate_eval_metrics()`](https://pauloguimaraes871.github.io/factoRverse/reference/calculate_eval_metrics.md)
