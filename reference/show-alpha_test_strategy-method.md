# Show Alpha Test Strategy

Prints the contents of an `alpha_test_strategy` object: main information
(signal significance threshold, p-value correction method, market factor
proxy, theme representativeness); model structure (model structure, and
theme-level intercept/slope if set); and `lmer_control` parameters, if
set.

## Usage

``` r
# S4 method for class 'alpha_test_strategy'
show(object)
```

## Arguments

- object:

  An `alpha_test_strategy` object to be displayed.
