# Skip helpers for optional heavy backends
#
# These centralise the conditions under which tests that depend on external
# toolchains (a TensorFlow Python backend, or Stan via brms) should be skipped,
# so the conditions stay consistent across test files and are declared in one
# place.

## skip_if_no_tensorflow
### Skip tests that build or fit Keras/TensorFlow models. Skipped on CRAN
### (configuring a Python environment there is not feasible) and whenever a
### working TensorFlow Python backend is not available on the machine, even if
### the 'keras'/'tensorflow' R packages themselves are installed.
skip_if_no_tensorflow <- function() {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("keras")
  testthat::skip_if_not_installed("tensorflow")

  ### The R packages can be present without a configured Python backend; the
  ### model-building calls only work when the 'tensorflow' Python module loads.
  if (!requireNamespace("reticulate", quietly = TRUE) ||
      !reticulate::py_module_available("tensorflow")) {
    testthat::skip("TensorFlow Python backend not available")
  }
}

## skip_if_no_stan
### Skip tests that fit brms/Stan models. Skipped on CRAN (Stan model
### compilation is too slow for CRAN time limits) and whenever 'brms' is not
### installed.
skip_if_no_stan <- function() {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("brms")

  ### 'brms' being installed does not imply a usable Stan backend: it defers all
  ### model compilation to 'rstan', whose shared object can fail to load even
  ### when the package is present (e.g. a TBB/RcppParallel ABI mismatch between
  ### the installed binary and the toolchain). requireNamespace() is what
  ### actually triggers dyn.load(), so probe it here and treat a failure as "no
  ### backend" rather than letting it surface as an error deep inside brms::brm().
  stan_loadable <- tryCatch(
    isTRUE(requireNamespace("rstan", quietly = TRUE)),
    error = function(e) FALSE
  )
  if (!stan_loadable) {
    testthat::skip("Stan backend not available: 'rstan' could not be loaded")
  }
}

## skip_if_no_parbayes
### Skip tests that run Bayesian-optimization tuning. ParBayesianOptimization
### is a Suggests dependency that was archived on CRAN, so it must be used
### conditionally; the renv lockfile still provides it in CI.
skip_if_no_parbayes <- function() {
  testthat::skip_if_not_installed("ParBayesianOptimization")
}
