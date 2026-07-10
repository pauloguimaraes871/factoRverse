# Calculate Mean-to-Standard Deviation Ratio (Signal-to-Noise Ratio)

This function computes the mean-to-standard deviation ratio of values in
a numeric vector. This ratio is also known as the inverse of the
coefficient of variation or, in finance, as the Sharpe Ratio when the
risk-free rate is zero. Optional sign inversion on the last n
observations

## Usage

``` r
signal_to_noise(values, na.rm = TRUE, mult_last_n = 0L, mult_by = -1)
```

## Arguments

- values:

  Numeric vector.

- na.rm:

  Logical.

- mult_last_n:

  Integer \>= 0. If \> 0, multiply the last n (in time order) by a
  number.

- mult_by:

  Numeric scalar. The number to multiply the last n values by. Default
  is -1 (inversion).

## Value

Numeric scalar.
