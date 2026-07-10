# Add Layer to Keras Architecture

Method to add a new layer to the Keras architecture.

## Usage

``` r
add_keras_layer(object, units, activation, batch_norm_option)

# S4 method for class 'keras_architecture_parameters'
add_keras_layer(object, units, activation, batch_norm_option)

# S4 method for class 'sb_backtest_config'
add_keras_layer(object, units, activation, batch_norm_option)
```

## Arguments

- object:

  An object of class `sb_backtest_config`

- units:

  A numeric value for the number of units in the new layer.

- activation:

  A character string specifying the activation function for the new
  layer (e.g., "relu").

- batch_norm_option:

  A character string indicating whether to apply batch normalization for
  the new layer (e.g., "yes").

## Value

An updated object of class `keras_architecture_parameters`.

## Functions

- `add_keras_layer(keras_architecture_parameters)`: Add a keras layer to
  an object of class `keras_architecture_parameters`

- `add_keras_layer(sb_backtest_config)`: Add a keras layer to an object
  of class `sb_backtest_config`
