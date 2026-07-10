# Summary Method for sb_metabacktest_config Class

Produces an interactive table summarizing the counts of configurations
by `sb_algorithm` and other parameters using the `DT` package. Neural
networks (`nn`) are grouped by the number of hidden layers, resulting in
rows like `nn_1`, `nn_2`, etc.

## Usage

``` r
# S4 method for class 'sb_metabacktest_config'
summary(object, ...)
```

## Arguments

- object:

  An `sb_metabacktest_config` object.

- ...:

  Additional arguments (not used).

## Value

Invisibly returns a `DT` table object. This function is primarily called
for its side effect of displaying the table.

## Details

The table supports horizontal and vertical scrolling with the first
column frozen and includes visual enhancements using specified colors.
Underscores in column headers are replaced with spaces.

This version also summarizes information from any associated
`ss_backtest_config` or `ss_backtest_workflow`:

- `model_structure`

- `p_correction_method`

For `ss_backtest_workflow`, we look at
`sb_backtest_config@ss_backtest_results@ss_backtest_workflow`.
