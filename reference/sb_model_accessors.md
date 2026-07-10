# Accessor Methods for sb_model

These methods retrieve slots from an object of class `sb_model`,
including its algorithm, fitted model, and hyperparameters.

## Usage

``` r
get_sb_algorithm(object)

# S4 method for class 'sb_model'
get_sb_algorithm(object)

get_best_hyperparameters(object)

# S4 method for class 'sb_model'
get_best_hyperparameters(object)

get_model(object)

# S4 method for class 'sb_model'
get_model(object)
```

## Arguments

- object:

  An object of class `sb_model`.

## Value

The respective slot from the `sb_model` object:

- `get_sb_algorithm`:

  Returns the algorithm used (character).

- `get_best_hyperparameters`:

  Returns the list of best hyperparameters, or `NULL` if not applicable.

- `get_model`:

  Returns the fitted model object.
