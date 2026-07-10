# Signal Transformation Function

This function performs a sequence of transformations on a numeric
vector:

- Winsorizes the values based on specified quantiles.

- Computes the z-score of the winsorized values.

- Transforms the z-scores into a new vector based on their sign.

## Usage

``` r
signal_transform(
  vector,
  lower_quantile_winsorization = 0.05,
  upper_quantile_winsorization = 0.95
)
```

## Arguments

- vector:

  A numeric vector to be transformed.

- lower_quantile_winsorization:

  A numeric value between 0 and 1 specifying the quantile threshold for
  lower winsorization.

- upper_quantile_winsorization:

  A numeric value between 0 and 1 specifying the quantile threshold for
  upper winsorization.

## Value

A numeric vector of the same length as `vector` with transformed values.

## Details

The function first applies winsorization to the `vector` based on the
provided quantile thresholds. Values exceeding the upper quantile are
replaced with the upper quantile value, and values below the lower
quantile are replaced with the lower quantile value. The function then
computes the z-scores of the winsorized values. Finally, it transforms
these z-scores:

- Positive z-scores are adjusted to \\1 + Z\\

- Negative z-scores are transformed to \\\frac{1}{1 - Z}\\

- A z-score of zero is transformed to 1#'
