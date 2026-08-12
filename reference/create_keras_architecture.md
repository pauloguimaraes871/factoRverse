# Create Keras Architecture

Constructor for creating an instance of `keras_architecture_parameters`.

## Usage

``` r
create_keras_architecture(
  nn_optimizer,
  units = NULL,
  activation = NULL,
  batch_norm_option = NULL,
  n_ensembles = 1
)
```

## Arguments

- nn_optimizer:

  A character string specifying the optimizer to use (e.g., "adam").

- units:

  A numeric value for the number of units in the new layer.

- activation:

  A character string specifying the activation function for the new
  layer (e.g., "relu").

- batch_norm_option:

  A character string indicating whether to apply batch normalization for
  the new layer (e.g., "yes").

- n_ensembles:

  A single integer (\>= 1) giving how many independently initialised
  networks are trained at refit time and averaged into one forecast.
  Defaults to 1, a single network, which is what earlier versions always
  did. Values above 1 remove initialisation variance from the forecast,
  at a proportional cost in refit time: Gu, Kelly and Xiu (2020) average
  10 networks, Rubesam (2021) averages 50. `NULL` is accepted and read
  as 1, so architectures recovered from runs recorded before this
  argument existed rebuild as the single network they were fitted as.

## Value

An object of class `keras_architecture_parameters`.
