# Un-dummy sector features

This functions takes dummy sectors columns and melt them into the single
original column that created the dummies

## Usage

``` r
undummy_sector_features(sectors_m_df)
```

## Arguments

- sectors_m_df:

  A dataframe with id, tickers and dates with dummy sectors
  classifications to be used to fill NAs
