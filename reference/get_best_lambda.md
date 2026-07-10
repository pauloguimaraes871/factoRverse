# Get Best Lambda from Glmnet Model Based on Training/Test Sets

Given a glmnet model fit and a lambda sequence, returns the lambda
(regularization parameter) that yields the best performance metric on a
validation sample.

## Usage

``` r
get_best_lambda(
  glmnet_fit,
  lambda_seq,
  features_validation_sample_clean,
  target_validation_sample,
  huber_delta,
  quantile_tau,
  chosen_eval_metric
)
```

## Arguments

- glmnet_fit:

  A glmnet object obtained from fitting the model using glmnet.

- lambda_seq:

  Numeric vector of lambda values used in the glmnet model.

- features_validation_sample_clean:

  Matrix or data frame of validation set features.

- target_validation_sample:

  Vector of true values for the validation set.

- huber_delta:

  Huber loss parameter (optional, default is 1.345).

- quantile_tau:

  Quantile loss parameter (optional, default is 0.5).

- chosen_eval_metric:

  Character string specifying the evaluation metric to optimize (e.g.,
  "mse", "mae", "quantile").

## Value

The lambda value from lambda_seq that maximizes the chosen evaluation
metric.
