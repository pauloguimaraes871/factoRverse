# Predict Method for keras_ensemble Class

Generates a forecast from an ensemble of independently initialised Keras
networks by averaging the forecasts of its members.

## Usage

``` r
# S4 method for class 'keras_ensemble'
predict(object, x, ...)
```

## Arguments

- object:

  An instance of the `keras_ensemble` class, as returned in the
  `model_nn` element of
  [`fit_keras_model()`](https://pauloguimaraes871.github.io/factoRverse/reference/fit_keras_model.md)
  when `n_ensembles > 1`.

- x:

  A numeric matrix of features, with one row per observation and columns
  in the same order the members were trained on.

- ...:

  Further arguments passed to the members' own `predict` methods.

## Value

A numeric vector with one averaged prediction per row of `x`.

## Details

Forecasts are averaged, not weights: hidden units are permutation
symmetric, so the coordinates of independently initialised networks are
not comparable and averaging weights would be meaningless.

Averaging is done with [`Reduce()`](https://rdrr.io/r/base/funprog.html)
rather than by binding member predictions into a matrix, because a
single-row `x` (a one-asset cross-section, which does occur at sparse
rebalancing dates) would collapse such a matrix to a bare vector and
silently average across members instead of across rows.
