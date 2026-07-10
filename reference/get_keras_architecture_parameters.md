# Get Keras Architecture Parameters

Accessor function to retrieve the keras architecture parameters.

## Usage

``` r
get_keras_architecture_parameters(object)

# S4 method for class 'sb_backtest_config'
get_keras_architecture_parameters(object)

# S4 method for class 'sb_metabacktest_config'
get_keras_architecture_parameters(object)

# S4 method for class 'sb_backtest_results'
get_keras_architecture_parameters(object)

# S4 method for class 'sb_model'
get_keras_architecture_parameters(object)
```

## Arguments

- object:

  A sb_backtest_config, a sb_metabacktest_config or a
  sb_backtest_results object.

## Value

A `keras_architecture_parameters` S4 class.
