# Get and Validate Chosen Signals and Positions

Extracts `chosen_signals_and_positions` from a list of
`base_sb_backtest_results` objects, ensures all elements are identical,
and checks conformity with `features_passthrough`.

## Usage

``` r
get_and_check_chosen_signals_and_positions(
  base_sb_backtest_results_list,
  features_passthrough,
  features_m_df
)
```

## Arguments

- base_sb_backtest_results_list:

  A list of `sb_backtest_results` objects.

- features_passthrough:

  A character vector indicating features to pass through. Can also be
  `"all"` or `"none"`.

- features_m_df:

  A `meta_dataframe` containing the available features.

## Value

A named character vector of signal positions ("long"/"short") for each
feature.
