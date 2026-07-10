# Convert Keras Architecture Parameters to List

Converts a `keras_architecture_parameters` object to a list.

This method extracts the relevant attributes from a
`keras_architecture_parameters` object and returns them as a list,
making it easier to work with the parameters in a more general R
context.

## Usage

``` r
# S4 method for class 'keras_architecture_parameters'
as.list(x)
```

## Arguments

- x:

  A `keras_architecture_parameters` object that contains the
  architecture parameters for a Keras model.

## Value

A list containing the following elements:

- units:

  The number of units in the layer.

- n_layers:

  The number of layers in the architecture.

- activation:

  The activation function used in the architecture.

- nn_optimizer:

  The optimizer used for training the neural network.

- batch_norm_option:

  Indicates if batch normalization is applied.
