# Add Keras Architecture

Method to add a `keras_architecture_parameters` to a
`sb_backtest_config`.

This function allows you to either directly add a pre-existing
`keras_architecture_parameters` object or create one dynamically by
passing additional arguments. When `keras_architecture_parameters` is
not provided, a new one will be created using the values for
`nn_optimizer`, `units`, `activation`, and `batch_norm_option` passed
via the `...` argument.

## Usage

``` r
add_keras_architecture(object, keras_architecture_parameters, ...)

# S4 method for class 'sb_backtest_config,keras_architecture_parameters'
add_keras_architecture(object, keras_architecture_parameters)

# S4 method for class 'sb_backtest_config,missing'
add_keras_architecture(object, keras_architecture_parameters = NULL, ...)
```

## Arguments

- object:

  An object of class `sb_backtest_config`.

- keras_architecture_parameters:

  Should be missing to dynamically create a new architecture.

- ...:

  Additional parameters used to create the
  `keras_architecture_parameters`, including:

  - **nn_optimizer**: A character string specifying the optimizer to use
    (e.g., "adam").

  - **units**: A numeric value for the number of units in the new layer.

  - **activation**: A character string specifying the activation
    function for the new layer (e.g., "relu").

  - **batch_norm_option**: A character string indicating whether to
    apply batch normalization for the new layer (e.g., "yes").

## Value

An updated object of class `sb_backtest_config` with the
`keras_architecture_parameters` added.

An updated `sb_backtest_config` object with the provided
`keras_architecture_parameters`.

An updated `sb_backtest_config` object with the newly created
`keras_architecture_parameters`.

## Functions

- `add_keras_architecture( object = sb_backtest_config, keras_architecture_parameters = keras_architecture_parameters )`:
  Add existing `keras_architecture_parameters` object

  This method allows you to add an already existing
  `keras_architecture_parameters` object to an `sb_backtest_config`.

- `add_keras_architecture( object = sb_backtest_config, keras_architecture_parameters = missing )`:
  Dynamically create and add `keras_architecture_parameters` object

  This method creates a new `keras_architecture_parameters` object
  dynamically when `keras_architecture_parameters` is not provided. The
  parameters required to create this object must be passed via `...` and
  include:

  - **nn_optimizer**: A character string specifying the optimizer to use
    (e.g., "adam").

  - **units**: A numeric value for the number of units in the new layer.

  - **activation**: A character string specifying the activation
    function for the new layer (e.g., "relu").

  - **batch_norm_option**: A character string indicating whether to
    apply batch normalization for the new layer (e.g., "yes").
