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

- ...:

  Additional arguments consumed only when early stopping is active:
  `features_validation_sample_clean`, `target_validation_sample`, and
  `chosen_eval_metric_translated` (its `$name`/`$mode` configure
  [`keras::callback_early_stopping()`](https://rdrr.io/pkg/keras/man/callback_early_stopping.html)).

## Value

A list containing:

- model_nn:

  The trained Keras model object.

- fit_nn:

  The Keras training `history` object (per-epoch metrics).

## Details

Each hidden layer applies L1/L2 kernel regularization, optional batch
normalization, and dropout; the output layer is a single linear unit
(regression). The Keras session is cleared via
[`on.exit()`](https://rdrr.io/r/base/on.exit.html) after each call to
bound memory growth across the many refits of a walk-forward backtest.
Note that Keras models are mutable: re-fitting the same object continues
training rather than starting fresh.
