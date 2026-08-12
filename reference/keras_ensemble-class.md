# Ensemble of Independently Initialised Keras Networks

Holds several trained Keras networks that share an architecture and
hyperparameters but differ in their random weight initialisation, and
behaves as a single model through its `predict` method.

## Details

A single network fit is one draw from a distribution over networks
induced by the random initialisation, so its forecast carries
initialisation variance that nothing else in the walk-forward scheme
removes. Averaging the forecasts of `k` independently initialised
members reduces that variance component by roughly a factor of `k`.

Forecasts are averaged, never weights. Hidden units are permutation
symmetric, so the weights of independently initialised networks are not
comparable coordinate by coordinate and averaging them is meaningless.

## Slots

- `members`:

  A list of trained Keras models, each fitted on the same data with the
  same hyperparameters and its own random initialisation.

## See also

[`fit_keras_model`](https://pauloguimaraes871.github.io/factoRverse/reference/fit_keras_model.md)
