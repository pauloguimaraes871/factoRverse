# Calculate Standardized Unexpected Realization (SUR)

Computes the Standardized Unexpected Realization (SUR), defined as the
standardized deviation of a final observation from past values.

## Usage

``` r
sur(final_value, past_values, na.rm = TRUE)
```

## Arguments

- final_value:

  A numeric value representing the latest observation.

- past_values:

  A numeric vector of historical observations.

- na.rm:

  Logical. Should missing values be removed before computation? Default
  is `TRUE`.

## Value

A numeric SUR value. Returns `NA_real_` if the standard deviation is
zero or undefined.
