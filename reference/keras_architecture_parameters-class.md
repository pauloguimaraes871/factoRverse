# Keras Architecture Parameters

Class to encapsulate parameters for constructing a Keras neural network
architecture.

## Slots

- `units`:

  A numeric vector specifying the number of units (neurons) for each
  layer.

- `n_layers`:

  A numeric value representing the total number of layers in the model.

- `activation`:

  A character vector containing the activation functions for each layer.

- `nn_optimizer`:

  A character string indicating the optimization algorithm used (length
  = 1).

- `batch_norm_option`:

  A character vector specifying whether to apply batch normalization for
  each layer.
