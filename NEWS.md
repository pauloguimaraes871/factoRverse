# factoRverse 0.6.1.9000

Development version. Adds an opt-in relaxation that allows a meta learner to
stack base learners fitted on different feature sets.

* `run_sb_backtest()` (the `sb_metabacktest_config` method) gains
  `.allow_heterogeneous_base_features`, defaulting to `FALSE`. When `TRUE`, base
  learners whose `chosen_signals_and_positions` differ may be blended together.
  This supports research designs that combine learners trained on different
  representations of the same investable universe — for example heuristic
  learners fitted on aggregated signal clusters alongside machine-learning
  learners fitted on the underlying individual signals.

  The relaxation is deliberately narrow. It requires
  `features_passthrough = "none"`, because only in that configuration does the
  meta learner ignore `features_m_df` entirely: `consolidate_oos_sb_outputs_m_df()`
  then builds the meta design matrix purely from the base learners' predictions
  joined on `id`, so which features each base learner saw is provenance rather
  than a correctness requirement. With any other `features_passthrough`, the
  meta learner must select pass-through columns from a single `features_m_df`
  and a heterogeneous pool makes that ill-posed; the function now errors rather
  than resolving it silently against one arbitrary learner's feature set.

  In exchange for the relaxed provenance check, the substantive invariant is
  asserted explicitly: all base learners must score an identical `id` set. This
  was previously enforced only incidentally, and with a generic message, inside
  `consolidate_oos_sb_outputs_m_df()`.

  Not relaxed, and still enforced under the flag: the `features_object_name`
  check, the RP/HRP/MVO `signal_themes`/`backtest_returns` provenance checks,
  and the `target_object_name`, `target_fwd_name`,
  `training_sample_size + validation_sample_size` and `dates_testing_sample`
  invariants.

  `.allow_heterogeneous_base_features = FALSE` reproduces previous behaviour
  exactly; a regression test asserts that turning the flag on for a homogeneous
  pool leaves both the meta design matrix and the meta learner's realised
  out-of-sample predictions bit-identical.

  Note: `explain_prediction()` on an `sb_metabacktest_results` object built from
  a heterogeneous pool is not supported, since it requires every base learner's
  feature columns to be present in one supplied `features_m_df`.

# factoRverse 0.6.1

Bug fix for meta-portfolio backtests that use group (sector) representativeness.

* `calculate_group_covariance_matrix()` now returns an exactly symmetric
  matrix. Each off-diagonal entry `[i, j]` is the bilinear form
  `w_i' Sigma w_j`, while the mirror entry `[j, i]` was computed as a separate
  matrix product; in floating point the two diverged by ~1e-18, so the
  aggregated sector covariance could be asymmetric at the round-off level. When
  that matrix became the `@covariance_matrix` slot of a macro `port` object,
  the class validity guard (`isSymmetric()`) rejected it with
  `"covariance_matrix must be symmetric"`, intermittently aborting `rp`, `hrp`
  and `mmaf` meta-portfolio backtests. The two triangles are now averaged
  (`(G + t(G)) / 2`) before the matrix is returned. The true aggregate
  covariance is symmetric, so this removes only round-off; portfolio weights
  and statistics are unchanged to ~1e-15.

# factoRverse 0.6.0

* New feature: a `"journal"` plotting palette across all `plot()` methods,
  alongside the existing `"cyberpunk"` and `"br"` themes. The journal theme is
  a sober, print-oriented light theme in the style of economics and finance
  journals: white ground, near-black ink, hairline grid, and eight muted
  categorical hues chosen for separation in grayscale and for colourblind
  readers. Positive/negative readings map to a muted green and brick red.
  - The palette is defined once (internal `.journal_palette()`) and consumed
    by every plot method, so figures stay consistent across the four
    workflows' results objects.
  - The journal theme shares the light-theme code paths of `"br"`; the two
    config-object plots (`plot(<ss_backtest_config>)`,
    `plot(<sb_backtest_config>)`) treat any non-cyberpunk palette as a
    generic light theme, as documented.
  - `"cyberpunk"` and `"br"` output is unchanged.

# factoRverse 0.5.1

Packaging and documentation refinements after the 0.5.0 release. No changes to
the analytical API.

* Dependencies:
  - Moved `ParBayesianOptimization` (archived on CRAN) from Imports to
    Suggests. It is only needed for `tuning_method = "bayesian_opt"`, and
    `hyper_tune()` now fails fast with installation guidance when it is
    missing. A plain `devtools::install_github()` no longer needs to resolve
    the archived package; the `renv` lockfile keeps providing it for CI and
    reproducible installs.
  - Tests that run Bayesian-optimization tuning are now skipped when
    `ParBayesianOptimization` is not installed. They continue to run in CI,
    where the lockfile installs it.
* Continuous integration:
  - Authenticated GitHub API calls in the check and release-gate workflows
    (`GITHUB_PAT`), fixing rate-limit failures when `renv` resolves the
    `Remotes` field on shared runners.
* Documentation:
  - Added the FactorOps map, a visual overview of the four workflows, their
    features, orchestrated packages, shared engines, and the built-in plot
    inventory, to the package website, with a navbar entry.
  - Added a "One interface to the R quant stack" section to the README with a
    workflow-to-package mapping table.
  - Mermaid diagrams now render on the package website.

# factoRverse 0.5.0

Continuous integration / delivery and packaging improvements. No changes to the
analytical API.

* Continuous integration:
  - The `R-CMD-check` workflow now restores the exact `renv` lockfile and runs
    `R CMD check --as-cran` (via `rcmdcheck`) on Ubuntu and Windows, so CI
    exercises the same pinned dependency versions the package is tested against.
  - Added a `test-coverage` workflow reporting to Codecov.
* Releases:
  - Added a tag-triggered `release` workflow that builds the source tarball and
    publishes it together with the `renv` lockfile as release assets, with the
    release notes taken from this file. Publishing is gated on `R CMD check`
    passing on the tagged commit.
* Packaging and documentation:
  - Declared `SystemRequirements` for the optional TensorFlow/Keras (Python) and
    Stan/C++ (via `brms`) backends.
  - Documented the reproducible installation route (release tarball + `renv`
    lockfile) in the README.
  - Added `Remotes:` for `ParBayesianOptimization`, which was archived on CRAN,
    so `devtools::install_github()` can resolve it.
  - Added test skip guards for when the TensorFlow or Stan backends are
    unavailable.
  - Corrected the CITATION entries with the papers' SSRN URLs.

# factoRverse 0.4.8

First public-facing release. Highlights of the documentation and packaging work:

* Added a package website built with pkgdown, with a curated reference index
  organised around the four core workflows.
* Rewrote the README with an overview of the FactorOps design and runnable
  sketches of each workflow.
* Added package-level documentation (`?factoRverse`) and citation information.

Earlier versions (0.1.0 through 0.4.7) were developed privately and are not
covered by these release notes.
