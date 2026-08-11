# factoRverse 0.6.1.9000

## New features

* Hyperparameters can now be held constant under `tuning_method = "bayesian_opt"`,
  using the same declaration `random_search` already accepts:

  ```r
  add_hyperparameter(strategy, hyperparameter = "subsample",
                     distribution_choice = "constant", pars = 0.8)
  ```

  A hyperparameter declared this way is removed from the Gaussian process's
  input space and its value re-inserted when the learner is called. Both the
  dimension the surrogate must fit and `gsPoints`, which defaults to
  `pmax(100, length(bounds)^3)`, fall accordingly: tuning xgb over three
  hyperparameters with the other five pinned takes `gsPoints` from 512 to the
  floor of 100. The fixed values are reported in the optimal hyperparameters and
  recorded in the tuning history, so downstream fitting, plots and summaries see
  a complete set exactly as before.

  `init_points` is now required to exceed the number of *searched*
  hyperparameters rather than the number declared, so pinning hyperparameters
  genuinely reduces the initial design.

## Deprecations

* Collapsing a hyperparameter's bounds to a single point, `c(x, x)`, under
  `bayesian_opt` now warns. It was the only way to pin a hyperparameter before
  constants existed, but it does not do what it appears to:
  `ParBayesianOptimization` rescales candidates by `upper - lower`, so a
  zero-width range reaches the Gaussian process as an undefined (`NaN`) input,
  with no error and no warning of its own, while still counting towards the
  search dimension and `gsPoints`.

  The behaviour of such configurations is **unchanged** in this release. They
  are not silently reinterpreted as constants, because doing so would alter the
  searched dimension, and hence the candidates drawn and the hyperparameters
  selected, for every existing configuration using the idiom. Migrating to
  `distribution_choice = "constant"` is opt-in and will change tuning results,
  which is the point: the previous search space contained an undefined
  dimension.

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
