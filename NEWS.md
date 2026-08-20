# factoRverse 0.7.0

## New features

* New portfolio construction method `"slsaf"`, the Simulated Long-Short
  Allocation Framework, configured through the new `slsaf_parameters` object
  (`create_slsaf_parameters()`, `add_slsaf_parameters()`). It addresses a
  structural defect of benchmark-relative construction: when a single name
  carries a large index weight, most methods cannot express a meaningful active
  position in it, so mega caps are persistently underweighted regardless of how
  attractive they are.

  The method builds a long-only portfolio as the benchmark plus a
  self-financing active overlay. `classify_investment_universe()` gains
  `include_benchmark_in_universe`, which splits the universe into a long block
  (what the eligibility cascade is willing to buy) and a short block (index
  constituents it rejected, which may only be underweighted). Underweights are
  graded by conviction and capped by the position actually held,
  `u_i = min(T * s_i, b_i)`, and the budget they release is spent on the long
  block. Four properties hold by construction and are asserted before any
  portfolio is returned: active weights sum to zero, weights sum to one with no
  renormalization, a rejected constituent is never overweighted nor shorted, and
  an eligible name is never underweighted.

  Two exponents shape the short leg, and they are not symmetric.
  `bench_weight_tilt_eta` sets the basis: because the budget is fully spent if
  and only if the short weights are benchmark-proportional, an exponent of 1
  sits exactly on that maximum, which is also the ungraded corner where every
  rejected constituent is sold in full. It is a basis rather than a budget dial,
  and no directional claim about it is safe once the second tilt is active.
  `badness_tilt_eta` then walks the budget-versus-grading frontier away from that
  anchor, concentrating underweight on the worst names and giving up budget in
  exchange. Maximum budget and graded underweights are mutually exclusive.

  The short leg is always reconstructed from `exp_ret_score_raw`, never from the
  scaled score, so a return-predictive scaler such as `1 / idio_vol` is never
  inverted into "underweight the low-volatility names the most".

  Because every active weight is expressed against the index position, the
  benchmark must actually sum to one. Gaps within `2e-3` are renormalized over
  the covered universe and persisted, so the weights and the statistics reported
  about them describe the same benchmark; gaps below `1e-4` are treated as
  rounding and repaired silently, and anything between the two warns. Larger gaps
  are refused rather than repaired: renormalizing them would inflate every
  surviving weight enough to measure tracking error against an index that exists
  nowhere. The refusal is symmetric, since a benchmark summing above one is not
  an incomplete universe but duplicated or overstated constituents.

  Since eligibility governs what may be bought rather than what may be
  underweighted, a rule other than the score can push high-scoring constituents
  into the short block; a liquidity floor strict enough to exclude index
  heavyweights can turn the method against its own signal. `create_slsaf_portfolio()`
  therefore warns when the underweighted names score better on average than the
  overweighted ones.

  Nine `slsaf`-specific plots are added to `plot(port_backtest_results)`, along
  with two `summary()` tables, all contrasting the two legs: the weight
  decomposition, benchmark coverage, leg scores as a series and as a
  distribution, tracking error attribution, sector and capitalization
  composition, the per-constituent underweight intensity profile, and the
  budget-versus-grading view. Per-rebalance diagnostics travel in
  `port_stats_m_df` as `slsaf_short_budget`, `slsaf_active_budget`,
  `slsaf_n_long`, `slsaf_n_short` and `slsaf_n_zeroed`, so the endogenous active
  budget is visible rather than inferred.

  `concentration_constraint_policy` and `turnover_constraint_policy` are
  rejected for this method: the overlay already determines every active weight,
  and the turnover buffer gates on `bop_port_weights > 0`, which holds for every
  constituent not fully sold, so the whole short block would drain into the long
  block. A `ridge_pen` on an MVO long leg is rejected as well: `target_weights`
  is joined into the universe only when the top-level ridge penalty is set, which
  never happens under `slsaf`, and routing a target into the long block is not
  merely plumbing, since the target is defined over the whole universe while the
  long block is a strict subset and renormalizing it changes what the penalty
  shrinks towards.

## Bug fixes

* The `"Stats Summary"` table of `summary(port_backtest_results)` referenced
  `port_stats_m_df` without it ever being extracted from the object, so the
  table could never render for any portfolio construction method.

* Neural-network signal blending can now average over several independently
  initialised networks at refit time, through the new `n_ensembles` argument of
  `create_keras_architecture()` (and of `add_keras_architecture()`). A single
  network fit is one draw from a distribution over networks induced by the
  random weight initialisation, so its forecast carries initialisation variance
  that nothing else in the walk-forward scheme removes; averaging the forecasts
  of `k` independently initialised members reduces that component by roughly a
  factor of `k`. Gu, Kelly and Xiu (2020) average 10 networks per topology and
  Rubesam (2021) averages 50.

  `n_ensembles` defaults to `1`, which trains a single network and reproduces
  the previous behaviour and return values exactly, so existing configurations
  and stored results are unaffected. Ensembling applies to the refit only:
  hyperparameter tuning always fits a single network per candidate, so the
  search cost does not change. With `n_ensembles > 1` the fitted model is a new
  `keras_ensemble` object whose `predict()` method averages its members'
  forecasts; forecasts are averaged, never weights, because hidden units are
  permutation symmetric across initialisations.

  Two consequences worth noting when the feature is switched on: neural-network
  feature importance is then computed from the ensemble mean via the global
  surrogate model, and an ensemble mean is less dispersed cross-sectionally than
  a single draw, which shifts effective weight in an equal-weighted meta-blend
  unless `normalize_base_predictions` is enabled.

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

## Bug fixes

* `fit_keras_model()` now assembles the requested network when
  `batch_norm_option` is `FALSE` for a layer. The inline selection of the
  optional batch-normalization layer called the piped model as a function on the
  `FALSE` branch, which aborted layer assembly part way through. The returned
  network then consisted of the first dense layer alone, with no output unit, so
  it produced one prediction per hidden unit per row instead of one prediction
  per row. The failure was silent in both directions: the assembly error was
  reported through `message()` and execution continued, and training then
  appeared to succeed because the loss broadcast the `(n, units)` output against
  the `(n,)` target.

  This affected every topology (1 to 5 layers) at any layer whose
  `batch_norm_option` was `FALSE`. Neural-network results produced with such a
  configuration were generated by a network that was not the one specified, and
  should be regenerated. Configurations with `batch_norm_option = TRUE` on every
  layer were assembled correctly and are unaffected; their results are unchanged.

  New tests assert the assembled depth, a single output unit, and one prediction
  per row scored, for all five topologies under all-`TRUE`, all-`FALSE` and mixed
  `batch_norm_option`.

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
