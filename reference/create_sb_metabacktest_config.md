# Create SB Meta Backtest Configuration

The `create_sb_metabacktest_config` function creates an
`sb_metabacktest_config` object that configures a meta-learning
(stacking) backtest. It wraps a single meta-learner `sb_backtest_config`
together with the rules for assembling the meta feature set from base
learners' out-of-sample predictions (`features_passthrough`,
`normalize_base_predictions`, `winsorize_base_predictions`). The base
learners themselves are supplied later, as a list of
`sb_backtest_results`, to
[`run_sb_backtest()`](https://pauloguimaraes871.github.io/factoRverse/reference/run_sb_backtest.md).

## Usage

``` r
create_sb_metabacktest_config(
  meta_sb_backtest_config,
  features_passthrough,
  ...
)

# S4 method for class 'sb_backtest_config,character'
create_sb_metabacktest_config(
  meta_sb_backtest_config,
  features_passthrough = "none",
  config_name = "not_identified",
  normalize_base_predictions = TRUE,
  winsorize_base_predictions = TRUE,
  ...
)
```

## Arguments

- meta_sb_backtest_config:

  A `sb_backtest_config` with the configuration for the meta learner.

- features_passthrough:

  A character vector naming features from `features_m_df` to append to
  the meta-learner's inputs; or 'all' (all features) or 'none' (none).
  Default 'none'.

- ...:

  Additional arguments (not used).

- config_name:

  Name of the backtest configuration.

- normalize_base_predictions:

  A logical value indicating whether to normalize the base predictions.

- winsorize_base_predictions:

  A logical value indicating whether to winsorize the base predictions.

## Value

An `sb_metabacktest_config` object.

An `sb_metabacktest_config` object.

## Functions

- `create_sb_metabacktest_config( meta_sb_backtest_config = sb_backtest_config, features_passthrough = character )`:
  Create a meta-backtest config from a meta-learner
  `sb_backtest_config`.
