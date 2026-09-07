# Show SLSAF Parameters

Prints the contents of an `slsaf_parameters` object: the long leg
configuration, the two score exponents and the active budget ceiling.
The short leg has no configuration of its own, since it is always signal
weighted on the badness score.

## Usage

``` r
# S4 method for class 'slsaf_parameters'
show(object)
```

## Arguments

- object:

  An `slsaf_parameters` object.
