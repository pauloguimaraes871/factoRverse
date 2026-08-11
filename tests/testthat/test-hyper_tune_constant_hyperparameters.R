# Tests for holding a hyperparameter constant under Bayesian optimization
#
# Declaring a constant by collapsing its bounds (lower == upper) does not work.
# ParBayesianOptimization scales every candidate by (upper - lower), so a
# zero-width bound yields an all-NaN column with no error and no warning, and
# `gsPoints` defaults to `pmax(100, length(bounds)^3)`, so the pinned
# hyperparameter still costs a full dimension. A constant must therefore be
# removed from the surrogate's input space entirely and re-inserted only when
# the learner is called.
#
# The declaration mirrors what `random_search` already accepts, namely
# `distribution_choice = "constant"` with `pars`, so the same configuration
# reads the same way under either tuning method.

## Helpers -------------------------------------------------------------------

make_bayes_strategy <- function() {
  factoRverse::create_tuning_strategy(
    tuning_method = "bayesian_opt",
    validation_sample_size = 12,
    chosen_eval_metric = "rmse",
    n_iter = 2, acq = "ucb", init_points = 4, k_iter = 1
  )
}


# Declaring a constant ---------------------------------------------------

test_that("a bayesian_opt strategy accepts a constant hyperparameter", {
  strategy <- make_bayes_strategy()
  strategy <- factoRverse::add_hyperparameter(
    strategy, hyperparameter = "eta", bounds = c(0.01, 0.5)
  )
  strategy <- factoRverse::add_hyperparameter(
    strategy, hyperparameter = "subsample",
    distribution_choice = "constant", pars = 0.8
  )

  hyperparameter_list <- strategy@hyper_grid_domain@hyperparameter_list

  ### The searched hyperparameter keeps its bounds shape unchanged.
  testthat::expect_equal(hyperparameter_list$eta, c(0.01, 0.5))

  ### The constant takes the same shape random_search already produces, so a
  ### configuration reads identically under either tuning method.
  testthat::expect_equal(hyperparameter_list$subsample$distribution_choice, "constant")
  testthat::expect_equal(hyperparameter_list$subsample$value, 0.8)
})

test_that("bounds-only bayesian_opt strategies are unaffected", {
  strategy <- make_bayes_strategy()

  ### Regression guard: the existing declaration must keep working untouched.
  strategy <- factoRverse::add_hyperparameter(
    strategy,
    hyperparameter = c("eta", "max_depth"),
    bounds = list(c(0.01, 0.5), c(2, 8))
  )

  testthat::expect_equal(strategy@hyper_grid_domain@hyperparameter_list$eta, c(0.01, 0.5))
  testthat::expect_equal(strategy@hyper_grid_domain@hyperparameter_list$max_depth, c(2, 8))
})

test_that("malformed constants are rejected", {
  strategy <- make_bayes_strategy()

  ### A vector-valued constant is not a constant.
  testthat::expect_error(
    factoRverse::add_hyperparameter(strategy, hyperparameter = "subsample",
                                    distribution_choice = "constant", pars = c(0.8, 0.9))
  )
  ### A non-numeric value would reach the learner untyped.
  testthat::expect_error(
    factoRverse::add_hyperparameter(strategy, hyperparameter = "subsample",
                                    distribution_choice = "constant", pars = "0.8")
  )
})

test_that("non-finite constants are rejected at construction", {
  strategy <- make_bayes_strategy()
  strategy <- factoRverse::add_hyperparameter(strategy, hyperparameter = "eta",
                                              bounds = c(0.01, 0.5))

  ### NA, NaN and Inf are all numeric of length 1, so a check that stops at
  ### numeric-and-scalar lets them through. They then reach the per-hyperparameter
  ### domain checks, where comparing a missing value against an interval yields
  ### NA and surfaces as "missing value where TRUE/FALSE needed", an error that
  ### says nothing about the hyperparameter that caused it. Reject them here,
  ### where the message can name the problem.
  for (non_finite_value in list(NA_real_, NaN, Inf, -Inf)) {
    testthat::expect_error(
      factoRverse::add_hyperparameter(strategy, hyperparameter = "subsample",
                                      distribution_choice = "constant",
                                      pars = non_finite_value),
      "finite"
    )
  }
})

test_that("non-finite constants are rejected by class validity", {
  ### The constraint must also hold for objects built by any route that reaches
  ### validity directly, not only through add_hyperparameter().
  build_directly <- function(value) {
    methods::new(
      "bayesian_opt_strategy",
      tuning_method = "bayesian_opt",
      validation_sample_size = 12,
      chosen_eval_metric = "rmse",
      hyper_grid_domain = methods::new(
        "hyper_grid_domain",
        hyperparameter_list = list(
          eta = c(0.01, 0.5),
          subsample = list(distribution_choice = "constant", value = value)
        )
      ),
      early_stop = NULL, n_iter = 2, acq = "ucb", init_points = 4, k_iter = 1
    )
  }

  testthat::expect_no_error(build_directly(0.8))
  for (non_finite_value in list(NA_real_, NaN, Inf)) {
    testthat::expect_error(build_directly(non_finite_value), "finite")
  }
})

test_that("a domain with no searched hyperparameter is rejected", {
  strategy <- make_bayes_strategy()

  ### With every hyperparameter fixed there is nothing for the surrogate to
  ### optimize, and bayesOpt would be handed empty bounds.
  testthat::expect_error(
    factoRverse::add_hyperparameter(strategy, hyperparameter = "subsample",
                                    distribution_choice = "constant", pars = 0.8) %>%
      factoRverse::add_hyperparameter(hyperparameter = "eta",
                                      distribution_choice = "constant", pars = 0.1)
  )
})


# Partitioning the domain ------------------------------------------------

test_that("the domain splits into searched bounds and fixed values", {
  hyper_grid_domain_list <- list(
    eta = c(0.01, 0.5),
    max_depth = c(2, 8),
    subsample = list(distribution_choice = "constant", value = 0.8),
    colsample_bytree = list(distribution_choice = "constant", value = 1)
  )

  partition <- partition_hyper_grid_domain_list(hyper_grid_domain_list)

  ### Only genuine ranges reach the surrogate, which is what makes gsPoints and
  ### the lengthscale count fall.
  testthat::expect_named(partition$searched, c("eta", "max_depth"))
  testthat::expect_equal(partition$searched$eta, c(0.01, 0.5))

  ### Fixed values come back as a plain named list ready to splice into the
  ### learner call.
  testthat::expect_named(partition$fixed, c("subsample", "colsample_bytree"))
  testthat::expect_equal(partition$fixed$subsample, 0.8)
  testthat::expect_equal(partition$fixed$colsample_bytree, 1)
})

test_that("a domain of only bounds partitions to no fixed values", {
  partition <- partition_hyper_grid_domain_list(list(eta = c(0.01, 0.5)))

  testthat::expect_named(partition$searched, "eta")
  testthat::expect_length(partition$fixed, 0)
})

test_that("a zero-width range warns but is left in the search space", {
  hyper_grid_domain_list <- list(
    eta = c(0.01, 0.5),
    size_of_batch = c(512L, 512L)
  )

  ### Collapsed bounds were the only way to pin a hyperparameter before
  ### constants existed, so existing configurations use them. They are
  ### pathological, but silently reinterpreting them as constants would change
  ### the searched dimension and therefore the tuning results of every such
  ### configuration. The warning explains the migration; behaviour is untouched
  ### until the user takes it.
  testthat::expect_warning(
    partition <- partition_hyper_grid_domain_list(hyper_grid_domain_list),
    "zero-width range"
  )

  testthat::expect_named(partition$searched, c("eta", "size_of_batch"))
  testthat::expect_length(partition$fixed, 0)
})

test_that("a clean domain converts without warning", {
  ### The warning must fire only for collapsed ranges, not on every call.
  testthat::expect_no_warning(
    partition_hyper_grid_domain_list(list(
      eta = c(0.01, 0.5),
      subsample = list(distribution_choice = "constant", value = 0.8)
    ))
  )
})


# Tuning with a constant -------------------------------------------------

test_that("a constant is held fixed and reported among the optimal hyperparameters", {
  skip_if_no_parbayes()

  ### A self-contained objective standing in for a learner: it records what it
  ### was called with, so the test can assert the fixed value actually reached
  ### the learner rather than merely surviving in the bookkeeping.
  ### The objective must report the same metric set a real eval function does,
  ### because hyper_tune() pulls those columns out of the score summary.
  seen_arguments <- list()
  fake_eval_function <- function(...) {
    context <- list(...)
    function(eta, subsample) {
      seen_arguments[[length(seen_arguments) + 1]] <<- list(eta = eta, subsample = subsample)
      error <- (eta - 0.3)^2
      list(Score = -error,
           rss = error, cp = error, rmse = error, mae = error, mphe = error,
           mpe = error, mape = error, hr = error, mb = error)
    }
  }

  hyper_grid_domain_list <- list(
    eta = c(0.01, 0.5),
    subsample = list(distribution_choice = "constant", value = 0.8)
  )

  set.seed(1)
  tuning_output <- hyper_tune(
    tuning_method = "bayesian_opt", ml_algorithm = "xgb", target_fwd_name = "target_fwd",
    full_data_training_sample_clean = NULL,
    features_validation_sample = NULL, target_validation_sample = NULL,
    eval_function = fake_eval_function, custom_objective_translated = NULL,
    chosen_eval_metric_translated = NULL, early_stop = NULL,
    chosen_eval_metric = "rmse", huber_delta = 1, quantile_tau = 0.5,
    hyper_grid_domain_list = hyper_grid_domain_list, n_iter = 2,
    init_points = 3, k_iter = 1, acq = "ucb",
    keras_architecture_parameters = NULL,
    parallel = FALSE, verbose = FALSE
  )

  optimal_hyper <- tuning_output$optimal_hyper

  ### The constant must come back in optimal_hyper. getBestPars() returns only
  ### the searched dimensions, and fit_sb_model() reads hyperparameters by name,
  ### so a dropped constant would arrive at the learner as NA.
  testthat::expect_true("subsample" %in% names(optimal_hyper))
  testthat::expect_equal(unname(optimal_hyper[["subsample"]]), 0.8)
  testthat::expect_true("eta" %in% names(optimal_hyper))

  ### And it must have been genuinely constant at every evaluation.
  testthat::expect_true(length(seen_arguments) > 0)
  testthat::expect_true(all(vapply(seen_arguments, function(a) a$subsample == 0.8, logical(1))))
  testthat::expect_gt(length(unique(vapply(seen_arguments, function(a) a$eta, numeric(1)))), 1)
})


# Input checking ---------------------------------------------------------

test_that("input checking accepts constants and counts only the searched ones", {
  load(paste(test_path(), "/testdata/", "artificial_signal_blending_obj.RData", sep = ""))

  ### Five of the eight xgb hyperparameters pinned, leaving three searched.
  ### This is the configuration the feature exists to make possible: it takes
  ### gsPoints from 8^3 = 512 down to the floor of 100.
  ### max_depth bounds must be integers; that is an existing xgb-specific check
  ### in check_inputs_sb_backtest() and is unrelated to constants.
  mostly_constant_domain <- list(
    min_child_weight = list(distribution_choice = "constant", value = 1),
    max_depth = c(2L, 8L),
    subsample = list(distribution_choice = "constant", value = 0.8),
    colsample_bytree = list(distribution_choice = "constant", value = 1),
    eta = c(0.01, 0.5),
    alpha = list(distribution_choice = "constant", value = 0),
    gamma = list(distribution_choice = "constant", value = 0),
    nrounds = c(100, 200)
  )

  check_domain <- function(init_points) {
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      training_sample_size = 4,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = NULL,
      benchmark_returns_m_xts = NULL,
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      custom_signal_weights_m_df = NULL,
      gsm_algorithm = "ols",
      validation_sample_size = 1,
      rebalancing_months = 9,
      tuning_method = "bayesian_opt",
      hyper_grid_domain_list = mostly_constant_domain,
      sb_algorithm = "xgb",
      chosen_eval_metric = "rss",
      verbose = FALSE,
      split_method = "expanding",
      quantile_tau = 0.5,
      huber_delta = 0.5,
      custom_objective = "squared_error",
      early_stop = NULL,
      n_iter = 2,
      init_points = init_points,
      k_iter = 1,
      acq = "ei",
      target_fwd_name = "fwd_premium_1m"
    )
  }

  ### Four initial points against three searched hyperparameters is legal, even
  ### though the domain declares eight in total. Counting the fixed ones would
  ### demand initial points for dimensions that are never searched, cancelling
  ### the saving the feature exists for.
  testthat::expect_no_error(check_domain(init_points = 4))

  ### Three initial points against three searched hyperparameters is not.
  testthat::expect_error(check_domain(init_points = 3),
                         "init_points must be greater than number of hyperparameters")
})
