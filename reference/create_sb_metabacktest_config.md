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
  allow_heterogeneous_base_features = FALSE,
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

- allow_heterogeneous_base_features:

  Logical. If `TRUE`, permits base learners fitted on different feature
  sets, and on different `features_m_df` objects, to be stacked
  together, for research designs that combine learners trained on
  different representations of the same investable universe. Requires
  `features_passthrough = "none"`; the validity function refuses any
  other pairing, since only on that path does the meta learner stop
  reading the feature columns of `features_m_df` and build its design
  matrix purely from the base learners' predictions joined on `id`. All
  base learners must still score an identical `id` set.

  It is a declaration that the pool **is** mixed, not a permission that
  may go unused: a pool on which neither relaxed check would have fired
  is refused. Either axis alone satisfies it, so two learners drawn from
  one `features_m_df` that disagree on `chosen_signals_and_positions`
  qualify.

  Note that a mixed run is named after whichever `features_m_df` was
  supplied, so its provenance string records one vintage rather than the
  pool, and
  [`explain_prediction()`](https://pauloguimaraes871.github.io/factoRverse/reference/explain_prediction.md)
  is unavailable on the result. Defaults to `FALSE`, which reproduces
  historical predictions exactly.

## Value

An `sb_metabacktest_config` object.

An `sb_metabacktest_config` object.

## Functions

- `create_sb_metabacktest_config( meta_sb_backtest_config = sb_backtest_config, features_passthrough = character )`:
  Create a meta-backtest config from a meta-learner
  `sb_backtest_config`.
