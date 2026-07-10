# Explain Prediction

The `explain_prediction` function decomposes the out‐of‐sample (OOS)
prediction for a selected ticker and date using the OLS Global Surrogate
Model (GSM) backtest results. It supports both standard backtest results
and meta-learner backtest results. If a meta-learner backtest object is
passed, the function decomposes base learners' contributions before
computing the final breakdown.

## Usage

``` r
explain_prediction(
  sb_backtest_results,
  features_m_df,
  selected_ticker,
  selected_date,
  palette = "cyberpunk"
)

# S4 method for class 'sb_backtest_results,meta_dataframe,character,Date'
explain_prediction(
  sb_backtest_results,
  features_m_df,
  selected_ticker,
  selected_date,
  palette = "cyberpunk"
)

# S4 method for class 'sb_metabacktest_results,meta_dataframe,character,Date'
explain_prediction(
  sb_backtest_results,
  features_m_df,
  selected_ticker,
  selected_date,
  palette = "cyberpunk"
)
```

## Arguments

- sb_backtest_results:

  An S4 object containing the backtest results, either of class
  `sb_backtest_results` or `sb_metabacktest_results`.

- features_m_df:

  An S4 object containing the features metadata used in the backtest. It
  must match the one stored in `sb_backtest_results`.

- selected_ticker:

  A character string representing the ticker (e.g., `"AAPL"`) for which
  to explain the prediction.

- selected_date:

  A Date object representing the prediction date.

- palette:

  A character string indicating the color palette to use for the plot.

## Value

A `data.frame` of the waterfall decomposition (feature/label,
contribution, and plotting coordinates); the waterfall `ggplot2` plot is
drawn as a side effect.

## Details

The function operates in several stages:

**Initial Checks:**

- Ensures that the provided backtest object is valid.

- Ensures that the `features_m_df` matches the backtest object's
  expected feature metadata.

- Ensures the selected ticker and date exist in the data.

**Feature Importance and Partial Contributions:**

- Extracts the most recent feature importance data up to the
  `selected_date`.

- If a meta-learner backtest is used, decomposes base learners'
  contributions before processing further.

- Calculates the partial contribution for each feature as the product of
  its importance and current value.

**Plot Generation:**

- Generates a waterfall plot using `ggplot2`, showing how each feature
  (including the intercept) contributes to the prediction.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Example usage:
  explain_prediction(sb_backtest_results, features_m_df, "AAPL", as.Date("2023-07-15"))
} # }
```
