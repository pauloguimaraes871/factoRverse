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

- `n_ensembles`:

  A single integer (\>= 1) giving how many independently initialised
  networks are trained at refit time and averaged into one forecast.
  Defaults to 1, which trains a single network and reproduces the
  behaviour of earlier versions exactly. Values above 1 follow the
  practice of averaging over random initialisations to remove
  initialisation variance from the forecast: Gu, Kelly and Xiu (2020)
  average 10 networks per topology, Rubesam (2021) averages 50. Applies
  to the refit only; hyperparameter tuning always fits a single network.
