# Predict Method for sb_model Class and meta_dataframe

This method generates predictions using a refitted sb model based on the
provided new feature data. It accommodates different machine learning
algorithms and applies the appropriate prediction logic.

## Usage

``` r
# S4 method for class 'sb_model'
predict(
  object,
  new_features_m_df,
  lower_quantile_winsorization = 0.025,
  upper_quantile_winsorization = 0.975
)
```

## Arguments

- object:

  An instance of the `sb_model` class containing the refitted model and
  its parameters.

- new_features_m_df:

  A `meta_dataframe` or a coercible data.frame containing new feature
  data for which predictions are to be made.

- lower_quantile_winsorization:

  Numeric value for lower winsorization

- upper_quantile_winsorization:

  Numeric value for upper winsorization

## Value

A numeric vector of predictions for the new feature data.

## Details

The function first validates that `new_features_m_df` is coercible to a
`meta_dataframe`. It extracts the relevant data from the input and uses
the appropriate prediction method based on the specified signal blending
algo
