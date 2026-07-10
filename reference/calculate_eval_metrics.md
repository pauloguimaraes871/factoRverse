# Calculate Out-of-Sample Evaluation Metrics

Computes a panel of out-of-sample (OOS) prediction-quality metrics
comparing a vector of predictions against realized targets. Used inside
the signal-blending tuning loop (see
[`hyper_tune`](https://pauloguimaraes871.github.io/factoRverse/reference/hyper_tune.md))
to score each hyperparameter candidate, and again to report the metrics
of the chosen model.

## Usage

``` r
calculate_eval_metrics(
  pred,
  target,
  huber_delta = 1,
  quantile_tau = 0.5,
  chosen_eval_metric = "rmse",
  early_stop = NULL,
  best_iteration = NULL,
  return_error = FALSE
)
```

## Arguments

- pred:

  Numeric vector of predicted values.

- target:

  Numeric vector of realized target values (same length as `pred`).

- huber_delta:

  Numeric scalar, delta for the Pseudo-Huber error (`mphe`). Default
  `1`.

- quantile_tau:

  Numeric scalar in `(0, 1)`, tau for the Pinball error (`mpe`). Default
  `0.5`.

- chosen_eval_metric:

  Character, metric used to build `Score`. One of `"rss"`, `"rmse"`,
  `"cp"`, `"mae"`, `"mphe"`, `"mpe"`, `"mape"`, `"hr"`. Default
  `"rmse"`. Note `"mb"` is reported but not selectable here.

- early_stop:

  Numeric or `NULL`. When numeric, a `best_iteration` column is appended
  (for `xgb`/`nn` early stopping).

- best_iteration:

  Numeric or `NULL`. Best iteration index to record when `early_stop` is
  set.

- return_error:

  Logical. If `TRUE`, returns a list of the metrics data frame plus the
  raw `error` vector. Default `FALSE`.

## Value

A one-row `data.frame` of metrics (or a list of that plus `error` when
`return_error = TRUE`). Columns:

- `Score`: `chosen_eval_metric` re-signed so higher is better (tuning
  target).

- `rss`: Out-of-sample R-squared, \\1 - \sum error^2 / \sum target^2\\.

- `cp`: Mean cross-product, `mean(pred * target)`.

- `rmse`: Root Mean Squared Error.

- `mae`: Mean Absolute Error.

- `mphe`: Mean Pseudo-Huber Error (uses `huber_delta`).

- `mpe`: Mean Pinball Error (uses `quantile_tau`).

- `mape`: Mean Absolute Percentage Error.

- `hr`: Hit Rate (share of predictions whose sign matches the target).

- `mb`: Mean Bias, `mean(error)` (reported only).

- `best_iteration`: Present only when `early_stop` is numeric.

## Details

The error convention is `error = target - pred`. If every element of
`error`/`target` is `NA`, or any prediction is `NA`, all metrics are
returned as `NA` rather than propagating silently. The `Score` column is
the single scalar the tuner maximizes: the `chosen_eval_metric`
re-signed so that larger is always better (error-type metrics `rmse`,
`mae`, `mphe`, `mpe`, `mape` are negated; `rss`, `cp`, `hr` are kept
as-is).
