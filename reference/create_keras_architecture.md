# Create Keras Architecture

Constructor for creating an instance of `keras_architecture_parameters`.

## Usage

``` r
create_keras_architecture(
  nn_optimizer,
  units = NULL,
  activation = NULL,
  batch_norm_option = NULL
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

## Value

An object of class `keras_architecture_parameters`.
