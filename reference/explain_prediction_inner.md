# Decompose and Visualize OOS Prediction for a Selected Ticker and Date

This internal function explains an out-of-sample prediction by
decomposing it into the individual contributions of each feature and the
model intercept. It is designed to work with a linear meta-model
(`gsm_algorithm = "ols"`) and creates a waterfall plot showing the
additive contributions, including a residual complexity component when
applicable.

## Usage

``` r
explain_prediction_inner(
  sb_backtest_workflow,
  oos_sb_outputs_m_df,
  feature_importance_m_df,
  gsm_algorithm,
  features_m_df,
  selected_ticker,
  selected_date,
  palette = "cyberpunk"
)
```

## Arguments

- sb_backtest_workflow:

  A list representing the signal blending backtest workflow. Must
  include at least the element `rebalance_dates` (used to validate the
  date range) and `target_fwd_name` (used for the plot title).

- oos_sb_outputs_m_df:

  A `meta_dataframe` with out-of-sample model predictions. Must include
  `id` (paste0(ticker, "-", date)) and `pred` columns.

- feature_importance_m_df:

  A `meta_dataframe` with feature importances from the GSM model. Must
  contain columns: `tickers`, `importance`, and `dates`.

- gsm_algorithm:

  A character string indicating the algorithm used for the GSM model.
  Currently, only `"ols"` is supported.

- features_m_df:

  A `meta_dataframe` of standardized or corrected features used by the
  GSM model. Must contain at least columns `id`, `tickers`, `dates`, and
  the feature variables.

- selected_ticker:

  A character string indicating the ticker to analyze.

- selected_date:

  A `Date` object specifying the date for which the prediction is to be
  explained.

- palette:

  A character string indicating the color palette to use for the plot.

## Value

A `data.frame` with the following columns:

- `tickers`: Feature or meta-label (e.g., base_pred, complexity).

- `ContributionType`: Label for plot grouping (e.g., Most Important
  Positive).

- `TotalContribution`: Contribution value.

- `Cumulative`, `PrevCumulative`, `Midpoint`, `x`, `xmin`, `xmax`,
  `ymin`, `ymax`: Intermediate values used to build the waterfall plot.

- `fill_type`: Positive or Negative contribution.

## Details

The function performs the following steps:

1.  Validates the selected ticker-date combination against
    `oos_sb_outputs_m_df` and `features_m_df`.

2.  Retrieves the most recent feature importance estimates available
    prior to `selected_date`.

3.  Extracts and normalizes the individual feature values for the
    selected ticker/date.

4.  Computes the linear contribution of each feature (feature value ×
    coefficient).

5.  Separates the contributions into positive/negative, and highlights
    the most important ones.

6.  Calculates the GSM model prediction and compares it to the complex
    model's prediction.

7.  Visualizes the contributions using a waterfall-style `ggplot2` bar
    plot with neon color scheme.

The plot includes:

- Base prediction (intercept)

- Most and less important positive/negative feature contributions

- Residual (complexity) not explained by the GSM linear model

## Note

This function is not exported and is intended for internal diagnostic
use only.
