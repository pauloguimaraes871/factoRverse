# Turn a Risk Estimate Into a Weight on the Risky Sleeve

Applies the risk-targeting rule \\w = (target / risk)^{p}\\ and clips it
to the configured bounds. A risk estimate at the target gives full
exposure; twice the target gives half at \\p = 1\\ and a quarter at \\p
= 2\\.

## Usage

``` r
risk_to_weight(risk, risk_target_params, exposure = 1)
```

## Arguments

- risk:

  A single annualised risk figure, in the target's units.

- risk_target_params:

  A `risk_target_parameters` object.

## Value

A single weight in `[min_weight, max_weight]`, or `NA_real_` when the
risk estimate is missing or not positive.
