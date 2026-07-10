# Predict Method for sb_backtest_results Class

This method generates predictions using a sb model that has been
validated through walk-forward validation. It uses the provided new
feature data and applies the appropriate prediction logic based on the
underlying model and its hyperparameters.

## Usage

``` r
# S4 method for class 'sb_backtest_results'
predict(
  object,
  new_features_m_df,
  lower_quantile_winsorization = 0.025,
  upper_quantile_winsorization = 0.975
)
```

## Arguments

- object:

  An instance of the `sb_backtest_results` class containing the
  validated model, metadata, and best hyperparameters.

- new_features_m_df:

  A data frame or an object of class `meta_dataframe` containing new
  feature data for which predictions are to be made. The data frame must
  be structured correctly and should not include the first three
  columns, which are reserved for identifiers.

- lower_quantile_winsorization:

  Numeric value for lower winsorization

- upper_quantile_winsorization:

  Numeric value for upper winsorization

## Value

A numeric vector of predictions for the new feature data.

## Details

The function validates that `new_features_m_df` is coercible to a
`meta_dataframe`. It extracts the relevant data and uses the appropriate
prediction method based on the specified machine learning algorithm
(e.g., OLS, GLMNET, RF, XGB, NN). The method retrieves the refitted
model and the best hyperparameters for making predictions.
