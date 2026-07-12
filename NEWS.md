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
