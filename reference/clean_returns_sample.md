# Generate a sample of returns to estimate covariance matrix

Generate a sample of returns to estimate covariance matrix

## Usage

``` r
clean_returns_sample(
  returns_m_xts_sample,
  groups_m_d_ref = NULL,
  fill = TRUE,
  fill_by = NULL,
  verbose = TRUE
)
```

## Arguments

- returns_m_xts_sample:

  A xts in which columns represent tickers present in
  current_universe_df and row represent days It should include all
  stocks in assets and a dates column with at least
  covariance_matrix_sample_size days before current_date

- groups_m_d_ref:

  A dataframe with id, tickers and dates with group classifications to
  be used to fill NAs

- fill:

  If TRUE, will fill rows NAs with groups medians. If groups_median are
  NAs, it will fill with row's median

- fill_by:

  If fill is TRUE, it will fill NAs with the median of the group defined
  by this column. If NULL, it will use the last column of groups_m_d_ref

- verbose:

  If TRUE, will print messages to the console
