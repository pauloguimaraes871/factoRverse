# Predict method for signal_port class

This method generates predictions using a signal_port obejct based on
the provided new feature data. It accommodates different port
construction methods. The function handles signal weighting and quantile
winsorization.

## Usage

``` r
# S4 method for class 'signal_port'
predict(
  object,
  new_features_m_df,
  upper_quantile_winsorization,
  lower_quantile_winsorization
)
```

## Arguments

- object:

  An instance of the `signal_port` class containing the signal portfolio
  and respective weights.

- new_features_m_df:

  A data frame or an object of class `meta_dataframe` containing new
  feature data for which predictions are to be made. The data frame must
  be structured correctly and should not include the first three
  columns, which are reserved for identifiers.

- upper_quantile_winsorization:

  Numeric value for upper winsorization

- lower_quantile_winsorization:

  Numeric value for lower winsorization

## Value

A numeric vector of blended-signal predictions (weighted sum of the
eligible signals, winsorized by the quantile bounds).
