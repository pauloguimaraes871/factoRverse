# Translate metrics based on machine learning algorithm and chosen evaluation metric.

This function adapts the chosen evaluation metric based on the provided
parameters.

## Usage

``` r
translate_metrics(
  sb_algorithm,
  chosen_eval_metric,
  custom_objective,
  early_stop,
  huber_delta,
  exp_ret_score_tilt,
  verbose
)
```

## Arguments

- sb_algorithm:

  Character string specifying the machine learning algorithm.

- chosen_eval_metric:

  Character string specifying the chosen evaluation metric.

- custom_objective:

  Character string specifying the custom objective function.

- early_stop:

  Numeric or null, indicating whether early stopping is enabled.

- huber_delta:

  Numeric specifying the delta parameter for Huber loss if applicable.

- exp_ret_score_tilt:

  Character specifying whether the tilt should be applied 'inner',
  'final' or 'none' in RP and HRP

- verbose:

  Logical indicating whether to display verbose messages.

## Value

A list containing the adapted chosen evaluation metric and custom
objective function.

## Details

- If `chosen_eval_metric` is `NULL`, it selects an appropriate metric
  based on `custom_objective`.

- Provides commentary if the selected evaluation metric is not supported
  for early stopping.

- Adjusts `custom_objective` based on the specified `sb_algorithm`.
