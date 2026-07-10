# Get Expected Hyperparameters for a Machine Learning Algorithm or Configuration

The `hyperparameters` function returns a character vector of expected
hyperparameters for a given machine learning algorithm or configuration.

## Usage

``` r
hyperparameters(object)

# S4 method for class 'character'
hyperparameters(object)

# S4 method for class 'sb_backtest_config'
hyperparameters(object)
```

## Arguments

- object:

  An `sb_backtest_config` object.

## Value

A character vector containing the names of the expected hyperparameters
for the specified algorithm.

A character vector of expected hyperparameters. If the algorithm is
"ols" or not recognized, it returns an empty character vector.

A character vector of expected hyperparameters for the algorithm
specified in the configuration. If the algorithm is "ols" or not
recognized, it returns an empty character vector.

## Methods (by class)

- `hyperparameters(character)`: Get expected hyperparameters for a given
  machine learning algorithm.

- `hyperparameters(sb_backtest_config)`: Get expected hyperparameters
  from an `sb_backtest_config` object.
