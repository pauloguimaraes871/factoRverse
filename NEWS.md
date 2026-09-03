# factoRverse 0.9.0

## New features

* Meta portfolio backtesting: allocate across a cohort of already-backtested
  portfolios, or scale a single portfolio against a passive residual so the
  combination targets a stated level of risk. Configured through the new
  `port_metabacktest_config` (`create_port_metabacktest_config()`), which wraps
  an ordinary `port_backtest_config` describing the meta allocation and is
  dispatched through the existing `run_port_backtest()` generic alongside a
  `port_backtest_cohort`.

  A `type` selector chooses between the two paths. Under `"multi_port"` the base
  portfolios form a cross-section, scored on a column of the new
  `port_universe_m_df` (`derive_port_universe_m_df()`), which carries each base
  portfolio's realised and ex-ante statistics along with running cost averages.
  The methods that carry over to a handful of portfolios are `"ew"`, `"sw"`,
  `"rp"`, `"hrp"` and `"mvo"`; `"slsaf"` and `"mmaf"` are refused because they
  are overlays on a stock cross-section, and `"cw"` and `"cs"` because they
  weight by a liquidity metric a portfolio does not have.

  Under `"risk_targeted"` there is no cross-section to rank. One risky sleeve is
  scaled against a residual sleeve by
  `w = s * (target / risk)^p`, clipped to `min_weight` and `max_weight`, with
  the residual taking the remainder. At `p = 1` this is ordinary risk targeting
  and at `p = 2` the inverse-variance response of a volatility-managed
  portfolio. The exposure multiplier `s` comes from
  `derive_exposure_signal()`, which turns a metric into a lean through a trend
  rule, a ratio to the metric's own history, or a pass-through. It is deliberately
  separate from the risk ratio: a constant `s` folds into the target by
  rescaling it, so only a time-varying signal adds anything, and offering an
  inverse-of-risk mapping there as well would let the volatility scaling be
  applied twice without showing up in the output.

  The residual and the target metric have to agree, and nothing errors when they
  do not. A residual that tracks the benchmark makes tracking error scale
  linearly toward zero as the sleeve is cut, so `target_metric =
  "tracking_error"` is valid. A residual that is riskless makes total volatility
  scale linearly instead, so `"volatility"` is valid. Crossing them does not
  fail, it stops working: a constant-return residual has a tracking error equal
  to the benchmark's own volatility, and blending toward it turns the portfolio
  into a large underweight of the market and raises tracking error rather than
  lowering it. Validation checks the residual's realised tracking error and
  warns when the pairing looks wrong.

  Both paths produce meta weights that are projected down to individual stocks
  by `project_meta_weights_to_stocks()` and run as an ordinary stock-level
  backtest through `"custom_weights"`, so returns, costs and turnover are the
  real ones, netted across base portfolios that hold the same names. Results are
  returned as `port_metabacktest_results`, or `risk_target_metabacktest_results`
  on the targeted path, both carrying `show` and `plot` methods.
  `update_port_backtest()` extends a meta backtest by one month, given a cohort
  whose base portfolios have themselves been rolled forward.

  Current risk on the targeted path comes from `estimate_sleeve_risk()`, which
  defaults to re-estimating a covariance matrix from daily stock returns over a
  short window and applying the sleeve's current weights. That describes the
  portfolio held now, where a rolling window of past monthly portfolio returns
  describes a chain of past compositions instead, and inheriting the figure from
  the base backtest's `port_stats` would use a long window that is also stale
  between rebalances.

* `port_backtest_config` accepts `port_construction_method = "custom_weights"`,
  which it previously refused. Supplied weights reach the engine through a
  weights-based route in `classify_investment_universe()`, which takes the
  positively-weighted assets as the eligible set, so no expected-return score is
  derived and none may be supplied alongside them.

* `run_sb_backtest()` (the `sb_metabacktest_config` method) gains
  `.allow_heterogeneous_base_features`, defaulting to `FALSE`. When `TRUE`, base
  learners whose `chosen_signals_and_positions` differ may be blended together.
  This supports research designs that combine learners trained on different
  representations of the same investable universe, for example heuristic
  learners fitted on aggregated signal clusters alongside machine-learning
  learners fitted on the underlying individual signals.

  The relaxation is deliberately narrow. It requires
  `features_passthrough = "none"`, because only in that configuration does the
  meta learner ignore `features_m_df` entirely: `consolidate_oos_sb_outputs_m_df()`
  then builds the meta design matrix purely from the base learners' predictions
  joined on `id`, so which features each base learner saw is provenance rather
  than a correctness requirement. With any other `features_passthrough` the meta
  learner must select pass-through columns from a single `features_m_df` and a
  heterogeneous pool makes that ill-posed; the function errors rather than
  resolving it silently against one arbitrary learner's feature set.

  In exchange for the relaxed provenance check, the substantive invariant is
  asserted explicitly: all base learners must score an identical `id` set. This
  was previously enforced only incidentally, and with a generic message, inside
  `consolidate_oos_sb_outputs_m_df()`.

  Two checks are relaxed, and only these two: the `chosen_signals_and_positions`
  comparison between base learners, and the `features_object_name` comparison
  between each base learner and the supplied `features_m_df`. Both have to go,
  because a genuinely mixed pool trips both: the meta run is handed a single
  `features_m_df`, so every learner drawn from the other object fails the second
  comparison. Relaxing only the first would permit a pool faked by rewriting a
  workflow batch and never a real one.

  Still enforced under the flag: the RP/HRP/MVO
  `signal_themes`/`backtest_returns` provenance checks, and the
  `target_object_name`, `target_fwd_name`,
  `training_sample_size + validation_sample_size` and `dates_testing_sample`
  invariants.

  The cost of relaxing the `features_object_name` comparison is a provenance
  one, and it is worth stating plainly: `oos_predictions_m_df` is named after
  whichever `features_m_df` the meta run was given, so a mixed run records one
  of its vintages rather than the pool. Nothing else reads that object on this
  path, so the consequence is a label, not a number.

  `.allow_heterogeneous_base_features = FALSE` reproduces previous behaviour
  exactly; a regression test asserts that turning the flag on for a homogeneous
  pool leaves both the meta design matrix and the meta learner's realised
  out-of-sample predictions bit-identical.

  The meta workflow batch now records `heterogeneous_base_features`, so a stored
  result declares the pool it was built from. `update_sb_backtest()` reads that
  field back and continues a heterogeneous run without being told again, the same
  way it already recovers `gsm_algorithm` and the winsorization bounds. An update
  is a continuation of a decision already taken, not a new one, and requiring the
  flag every month would mean the month it was forgotten a running book stopped
  dead. Results written before the field existed carry `NULL`, which is read as
  `FALSE`, so every stored homogeneous backtest keeps the guard; a run stored as
  homogeneous still refuses a heterogeneous pool on update.

  Note: `explain_prediction()` on an `sb_metabacktest_results` built from a
  heterogeneous pool is not supported. It requires every base learner's feature
  columns to be present in the one supplied `features_m_df`, which no single
  object can satisfy for such a pool. It fails with an explicit message rather
  than returning a misleading attribution.

## Bug fixes

* The `port_metabacktest_config` S4 class is exported again. A code comment
  inside a validity function was written as `###'custom_weights'`, and `roxygen2`
  treats any `#`-run followed by an apostrophe as a documentation line, so the
  comment was parsed as roxygen and swallowed the `@export` tag of the
  neighbouring class. The class kept its manual page while losing its
  `exportClasses()` entry, which left `methods::is()`, `new()` and S4 dispatch
  from other packages unable to see it even though `create_port_metabacktest_config()`
  was exported normally. The same breakage stopped `devtools::document()`
  running at all, so `NAMESPACE` and `man/` could not be regenerated; this is why
  it survived a release. R CMD check validates the committed `man/` rather than
  rebuilding it, so continuous integration stayed green throughout.

* `create_slsaf_portfolio()` aborted with `Weights do not sum to 1` on some
  configurations and not others, with no economic pattern, at a rate that scaled
  with the number of rebalance dates rather than with the parameters. Benchmark
  files are published rounded to six decimal places, so every per-date sum is an
  exact multiple of `1e-6` and a gap of exactly one quantum is the ordinary case
  rather than an accident. The repair was gated at the same tolerance the
  sum-to-one assertion uses, and both comparisons were strict, so such a gap was
  neither repaired nor reliably accepted: the overlay is self-financing, so
  `sum(w) = sum(b)` exactly, and the inherited gap landed on the assertion
  boundary where the floating-point noise of the particular long-leg arithmetic
  decided whether the build survived. Every gap is now repaired. Which inputs are
  accepted is unchanged: the renormalization allowance still refuses an input
  that is further than `2e-3` from 1, and the `1e-4` band still warns.

* `derive_slsaf_leg_diagnostics()` could refuse a portfolio that had been built
  successfully, taking every `slsaf` leg plot down with it. It recomposed the
  portfolio total from three separately accumulated group sums and compared it
  against its own hardcoded tolerance, so it and the constructor could land on
  opposite sides of the same boundary for the same portfolio. The invariant is
  now asserted on `sum(weights)`, and the decomposition is checked against that
  total at a floating-point tolerance, which still catches an asset belonging to
  neither leg. `leg_budget` gains `port_weight_total`.

* The `Micro` plot level was offered only when `port_construction_method` was
  `"mmaf"`, so an `slsaf` portfolio could not reach either of its legs even
  though it carries both and the dispatch below the gate is already generic. The
  level is now offered whenever the portfolio carries a populated sub-portfolio,
  giving `slsaf` users `Micro -> long / short`. Empty legs are not offered, since
  `slsaf` leaves the short leg empty when every constituent is eligible.

* `estimate_covariance_matrix()` sampled `cov_matrix_sample_size + 1`
  observations whenever more were available, because it indexed from
  `n - size` inclusive. The window is now exactly the number asked for. This
  changes every covariance estimate in the package by one observation, so
  results stored under earlier versions will not reproduce bit for bit, though
  the economic difference at a 252-day window is negligible.

* `estimate_covariance_matrix()` also short-circuits to `stats::var()` when
  exactly one ticker is passed, and that branch returned before the estimation
  window was selected. A one-name portfolio was therefore measured over its
  whole history while the same portfolio holding two names was measured over
  `cov_matrix_sample_size`, so risk figures from portfolios of different breadth
  were not comparable. The window is now selected before the branch, and the
  not-enough-dates guard applies to both paths.

* One covariance window was serving two frequencies in a meta backtest. The meta
  level counts months, because it allocates over portfolios whose returns are
  monthly, while the stock level counts trading days when daily returns are
  supplied. Forwarding the meta number to both meant a value of 36 stood for 36
  months at one level and 36 days at the other.
  `port_metabacktest_config` gains `stock_cov_matrix_sample_size` for the daily
  side, and `create_risk_target_parameters()` warns when a window looks written
  for the wrong frequency, or when a covariance method is supplied to a
  `vol_source` that never estimates one.

* The stock-level `port_backtest_results` produced by a meta backtest was not a
  well-formed object of its class: it carried a flat workflow rather than one
  batch keyed by date, and reported its identifier as `"not_identified"`, so
  `show()` on the slot failed outright.

* `create_port_backtest_cohort()` read `dates_covered` and `dates_backtest` from
  the last workflow batch of each result. That is correct for a backtest that ran
  once and wrong for one that has been updated, where the last batch describes
  only the window that update recomputed. A cohort of updated portfolios claimed
  a span far shorter than what it held, and `derive_port_universe_m_df()`
  truncated the meta universe to it. The two date grids are now unioned across
  batches.

# factoRverse 0.8.0

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

# factoRverse 0.7.0

## New features

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
