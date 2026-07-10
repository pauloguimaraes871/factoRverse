# sb_metabacktest_config Class

The sb_metabacktest_config class configures a meta-learning (stacking)
backtest: it wraps a single meta-learner `sb_backtest_config` together
with the rules for building the meta feature set from base learners'
out-of-sample predictions (feature passthrough, normalization,
winsorization).

## Slots

- `meta_sb_backtest_config`:

  A `sb_backtest_config` with the configuration for the meta learner

- `features_passthrough`:

  A character vector indicating which features from `features_m_df` are
  to be passed through to the meta learner. Alternatively, if `'all'`,
  all features are passed through. If `'none'`, no features are passed
  through. Default is `'none'`.

- `normalize_base_predictions`:

  Logical; if `TRUE`, normalizes the base learners' predictions before
  passing them to the meta learner. Default is `TRUE`.

- `winsorize_base_predictions`:

  Logical; if `TRUE`, winsorizes the base learners' predictions before
  passing them to the meta learner. Default is `FALSE`.

- `config_name`:

  A character string with the name of the configuration

## Validity

- The meta learner's `sb_algorithm` may not be `rp`/`hrp`/`mvo`/`mmaf`.

- The meta learner requires a `tuning_strategy` unless it is `ols` or
  heuristic.

- `chosen_signals_and_positions` must be `"all"` (positions are
  corrected via `features_passthrough`).

- `features_passthrough` may not contain `"long"`, `"short"` or
  `"force"`.
