# Fit a Keras Neural Network Model

Builds and trains a feed-forward Keras neural network (1–5 dense layers)
for signal blending, given hyperparameters, an architecture
specification, and loss / early-stopping settings. Used both during
tuning (with a validation set for early stopping) and at refit time (no
early stopping).

## Usage

``` r
fit_keras_model(
  regularizer_l1,
  regularizer_l2,
  droprate,
  lr,
  number_of_epochs,
  size_of_batch,
  keras_architecture_parameters,
  early_stop = NULL,
  custom_objective_translated,
  huber_delta,
  features_matrix_train_clean,
  target_vector_train,
  verbose,
  n_ensembles = 1,
  ...
)
```

## Arguments

- regularizer_l1:

  Numeric. L1 regularization parameter.

- regularizer_l2:

  Numeric. L2 regularization parameter.

- droprate:

  Numeric. Dropout rate.

- lr:

  Numeric. Learning rate.

- number_of_epochs:

  Integer. Maximum number of training epochs.

- size_of_batch:

  Integer. Batch size for training.

- keras_architecture_parameters:

  List, containing n_layers, units, activation, nn_optimizer and
  batch_norm_option

- early_stop:

  Integer or NULL. Number of epochs with no improvement to stop early,
  or NULL for no early stopping.

- custom_objective_translated:

  Custom objective in keras format

- huber_delta:

  Numeric. Delta parameter for Huber loss function.

- features_matrix_train_clean:

  Matrix. Training features matrix.

- target_vector_train:

  Vector. Training target vector.

- verbose:

  Integer. Verbosity level during training.

- n_ensembles:

  Integer \>= 1. Number of independently initialised networks to train
  and average. Each member sees the same data and the same
  hyperparameters and differs only in its random weight initialisation
  and its own early-stopping decision; forecasts are averaged, never
  weights. Defaults to 1, which trains a single network and returns
  exactly what earlier versions returned. Values above 1 follow Gu,
  Kelly and Xiu (2020), who average 10 networks per topology, and
  Rubesam (2021), who averages 50, both to remove the variance a single
  random initialisation injects into the forecast.

  This is a deliberate function argument rather than a field read off
  `keras_architecture_parameters`: the tuning path reaches this function
  with the same architecture specification as the refit, so reading it
  from there would make every candidate evaluation ensemble too and
  multiply the search cost by `n_ensembles`. Only the refit passes it.

- ...:

  Additional arguments consumed only when early stopping is active:
  `features_validation_sample_clean`, `target_validation_sample`, and
  `chosen_eval_metric_translated` (its `$name`/`$mode` configure
  [`keras::callback_early_stopping()`](https://rdrr.io/pkg/keras/man/callback_early_stopping.html)).

## Value

A list containing:

- model_nn:

  When `n_ensembles == 1`, the trained Keras model object. When
  `n_ensembles > 1`, a
  [`keras_ensemble`](https://pauloguimaraes871.github.io/factoRverse/reference/keras_ensemble-class.md)
  holding the trained members, which predicts as their average.

- fit_nn:

  When `n_ensembles == 1`, the Keras training `history` object
  (per-epoch metrics). When `n_ensembles > 1`, a list of one such
  history per member, since members may stop at different epochs.

## Details

Each hidden layer applies L1/L2 kernel regularization, optional batch
normalization, and dropout; the output layer is a single linear unit
(regression). The Keras session is cleared via
[`on.exit()`](https://rdrr.io/r/base/on.exit.html) after each call to
bound memory growth across the many refits of a walk-forward backtest.
Note that Keras models are mutable: re-fitting the same object continues
training rather than starting fresh.
