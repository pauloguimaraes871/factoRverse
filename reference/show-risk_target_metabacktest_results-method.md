# Show Method for risk_target_metabacktest_results Class

Displays a `risk_target_metabacktest_results` object. Where the
multi-portfolio summary reports a cross-sectional allocation, this one
reports the targeting rule at work: the risk estimated for the sleeve,
the weight the rule set from it, how often the bounds bound, and how the
realised risk compares against the level asked for.

## Usage

``` r
# S4 method for class 'risk_target_metabacktest_results'
show(object)
```

## Arguments

- object:

  An object of class `risk_target_metabacktest_results`.

## Value

Invisibly returns the input object.
