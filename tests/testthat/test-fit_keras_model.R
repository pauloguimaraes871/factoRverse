# Tests for neural-network initialisation ensembling
#
# The feature averages the forecasts of several independently initialised
# networks to remove initialisation variance from the refit (Gu, Kelly & Xiu
# 2020 average 10; Rubesam 2021 averages 50). The overriding constraint is that
# `n_ensembles = 1` must reproduce the pre-existing single-network behaviour
# exactly, so that configurations that never set the argument are unaffected.

## Helpers -------------------------------------------------------------------

### Minimal architecture list in the shape `fit_keras_model()` consumes, i.e.
### the output of `as.list()` on a `keras_architecture_parameters` object.
### batch_norm_option is TRUE deliberately. With FALSE the network builder in
### fit_keras_model() hits a separate, pre-existing defect: the
### `%>% {if (batch_norm) layer_batch_normalization() else .}()` idiom calls the
### piped model as a function on the FALSE branch, which aborts layer assembly
### and leaves a network with no output layer. That is unrelated to ensembling
### and is not what these tests are here to exercise.
make_arch_list <- function(n_ensembles = 1) {
  list(
    units = 4,
    n_layers = 1,
    activation = "relu",
    nn_optimizer = "Adam",
    batch_norm_option = TRUE,
    n_ensembles = n_ensembles
  )
}

### Small, well-conditioned regression problem. Kept tiny because these tests
### fit real networks; the point is structural behaviour, not fit quality.
make_training_data <- function(n = 60, p = 3, seed = 1) {
  ### Draw the data under a fixed seed, then restore the caller's RNG state so
  ### that seeding the data does not also pin the network initialisations.
  old_seed <- if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    get(".Random.seed", envir = globalenv())
  } else {
    NULL
  }
  on.exit({
    if (!is.null(old_seed)) assign(".Random.seed", old_seed, envir = globalenv())
  }, add = TRUE)

  set.seed(seed)
  features <- matrix(stats::rnorm(n * p), nrow = n, ncol = p)
  colnames(features) <- paste0("signal_", seq_len(p))
  target <- as.numeric(features %*% c(0.5, -0.3, 0.2) + stats::rnorm(n, sd = 0.1))
  list(features = as.data.frame(features), target = target)
}

### The loss / metric shapes produced by `translate_metrics()` for `nn`.
nn_objective <- "mean_squared_error"
nn_metric <- list(metric = "mean_squared_error",
                  name = "val_mean_squared_error",
                  mode = "min")

fit_test_model <- function(n_ensembles, data = make_training_data(), epochs = 2) {
  factoRverse::fit_keras_model(
    regularizer_l1 = 0, regularizer_l2 = 0, droprate = 0.1, lr = 0.01,
    number_of_epochs = epochs, size_of_batch = 16,
    keras_architecture_parameters = make_arch_list(n_ensembles),
    early_stop = NULL,
    custom_objective_translated = nn_objective, huber_delta = 1,
    features_matrix_train_clean = data$features,
    target_vector_train = data$target,
    verbose = FALSE,
    n_ensembles = n_ensembles,
    chosen_eval_metric_translated = nn_metric
  )
}


# Class and constructor --------------------------------------------------
## These need no TensorFlow backend: they only exercise S4 construction.

test_that("architectures built without n_ensembles default to a single network", {
  arch <- factoRverse::create_keras_architecture(
    nn_optimizer = "Adam", units = 8, activation = "relu", batch_norm_option = FALSE
  )

  testthat::expect_equal(arch@n_ensembles, 1)
})

test_that("create_keras_architecture carries an explicit n_ensembles", {
  arch <- factoRverse::create_keras_architecture(
    nn_optimizer = "Adam", units = 8, activation = "relu", batch_norm_option = FALSE,
    n_ensembles = 10
  )

  testthat::expect_equal(arch@n_ensembles, 10)
})

test_that("invalid n_ensembles values are rejected at construction", {
  build <- function(value) {
    factoRverse::create_keras_architecture(
      nn_optimizer = "Adam", units = 8, activation = "relu", batch_norm_option = FALSE,
      n_ensembles = value
    )
  }

  ### Zero and negative counts have no interpretation as a number of networks.
  testthat::expect_error(build(0))
  testthat::expect_error(build(-1))
  ### A fractional count would silently round somewhere downstream.
  testthat::expect_error(build(2.5))
  testthat::expect_error(build(NA_real_))
  ### A vector would recycle into the training loop.
  testthat::expect_error(build(c(1, 2)))
  testthat::expect_error(build("many"))
})

test_that("as.list exposes n_ensembles to the fitting path", {
  arch <- factoRverse::create_keras_architecture(
    nn_optimizer = "Adam", units = 8, activation = "relu", batch_norm_option = FALSE,
    n_ensembles = 4
  )

  ### `run_sb_backtest()` converts the S4 object to a list before anything
  ### downstream sees it, so a field missing here never reaches the fitter.
  arch_list <- as.list(arch)

  testthat::expect_true("n_ensembles" %in% names(arch_list))
  testthat::expect_equal(arch_list$n_ensembles, 4)
})

test_that("architectures serialised before the slot existed still convert to a list", {
  arch <- factoRverse::create_keras_architecture(
    nn_optimizer = "Adam", units = 8, activation = "relu", batch_norm_option = FALSE
  )

  ### S4 slots are attributes, so dropping the attribute reproduces an object
  ### deserialised from a pin written before `n_ensembles` was introduced.
  ### A class prototype does NOT repair such objects; only a guarded read does.
  legacy_arch <- arch
  attr(legacy_arch, "n_ensembles") <- NULL
  testthat::expect_false(methods::.hasSlot(legacy_arch, "n_ensembles"))

  arch_list <- testthat::expect_no_error(as.list(legacy_arch))
  testthat::expect_equal(arch_list$n_ensembles, 1)
})

test_that("n_ensembles survives a round trip through an sb_model", {
  sb_model <- methods::new(
    "sb_model",
    model = NULL,
    eligible_signals = "signal_1",
    model_class = "keras",
    sb_algorithm = "nn",
    best_hyperparameters = NULL,
    custom_objective = "squared_error",
    huber_delta = 1,
    keras_architecture_parameters = list(
      units = 8, n_layers = 1, activation = "relu",
      nn_optimizer = "Adam", batch_norm_option = FALSE, n_ensembles = 7
    )
  )

  ### The accessor rebuilds the object via create_keras_architecture(); before
  ### this change it silently reset any ensemble setting back to 1.
  rebuilt <- get_keras_architecture_parameters(sb_model)
  testthat::expect_equal(rebuilt@n_ensembles, 7)
})

test_that("an sb_model stored without n_ensembles rebuilds as a single network", {
  sb_model <- methods::new(
    "sb_model",
    model = NULL,
    eligible_signals = "signal_1",
    model_class = "keras",
    sb_algorithm = "nn",
    best_hyperparameters = NULL,
    custom_objective = "squared_error",
    huber_delta = 1,
    keras_architecture_parameters = list(
      units = 8, n_layers = 1, activation = "relu",
      nn_optimizer = "Adam", batch_norm_option = FALSE
    )
  )

  rebuilt <- get_keras_architecture_parameters(sb_model)
  testthat::expect_equal(rebuilt@n_ensembles, 1)
})


# Fitting: single-network path unchanged ---------------------------------

test_that("n_ensembles = 1 returns the pre-existing list shape", {
  skip_if_no_tensorflow()

  fitted_nn <- fit_test_model(n_ensembles = 1)

  ### The legacy contract: a two-element list consumed as `$model_nn` by
  ### fit_sb_model() and as `$fit_nn` by set_eval_function().
  testthat::expect_type(fitted_nn, "list")
  testthat::expect_named(fitted_nn, c("model_nn", "fit_nn"))
  testthat::expect_false(methods::is(fitted_nn$model_nn, "keras_ensemble"))
  testthat::expect_true(inherits(fitted_nn$model_nn, "keras.engine.training.Model") ||
                          inherits(fitted_nn$model_nn, "keras.src.models.model.Model") ||
                          inherits(fitted_nn$model_nn, "python.builtin.object"))
})

test_that("an architecture without n_ensembles fits a single network", {
  skip_if_no_tensorflow()

  arch_list <- make_arch_list()
  arch_list$n_ensembles <- NULL

  fitted_nn <- factoRverse::fit_keras_model(
    regularizer_l1 = 0, regularizer_l2 = 0, droprate = 0.1, lr = 0.01,
    number_of_epochs = 2, size_of_batch = 16,
    keras_architecture_parameters = arch_list,
    early_stop = NULL,
    custom_objective_translated = nn_objective, huber_delta = 1,
    features_matrix_train_clean = make_training_data()$features,
    target_vector_train = make_training_data()$target,
    verbose = FALSE,
    chosen_eval_metric_translated = nn_metric
  )

  testthat::expect_named(fitted_nn, c("model_nn", "fit_nn"))
  testthat::expect_false(methods::is(fitted_nn$model_nn, "keras_ensemble"))
})

test_that("a member that could not be built aborts instead of returning nothing", {
  skip_if_no_tensorflow()

  ### fit_keras_model() only knows how to assemble 1 to 5 layers; outside that
  ### range no branch matches and no network is ever created. The member must
  ### then abort rather than be collected as an empty slot, which would shorten
  ### the ensemble and silently average over fewer networks than requested.
  unsupported_arch <- make_arch_list(n_ensembles = 2)
  unsupported_arch$n_layers <- 7
  data <- make_training_data()

  testthat::expect_error(
    factoRverse::fit_keras_model(
      regularizer_l1 = 0, regularizer_l2 = 0, droprate = 0.1, lr = 0.01,
      number_of_epochs = 2, size_of_batch = 16,
      keras_architecture_parameters = unsupported_arch,
      early_stop = NULL,
      custom_objective_translated = nn_objective, huber_delta = 1,
      features_matrix_train_clean = data$features,
      target_vector_train = data$target,
      verbose = FALSE,
      n_ensembles = 2,
      chosen_eval_metric_translated = nn_metric
    ),
    regexp = "ensemble member"
  )
})

test_that("fit_keras_model rejects invalid n_ensembles", {
  skip_if_no_tensorflow()

  testthat::expect_error(fit_test_model(n_ensembles = 0))
  testthat::expect_error(fit_test_model(n_ensembles = 2.5))
  testthat::expect_error(fit_test_model(n_ensembles = c(1, 2)))
})


# Fitting: ensemble path -------------------------------------------------

test_that("n_ensembles > 1 returns a keras_ensemble of the requested size", {
  skip_if_no_tensorflow()

  fitted_nn <- fit_test_model(n_ensembles = 3)

  testthat::expect_named(fitted_nn, c("model_nn", "fit_nn"))
  testthat::expect_s4_class(fitted_nn$model_nn, "keras_ensemble")
  testthat::expect_length(fitted_nn$model_nn@members, 3)
  ### One training history per member: early stopping is a property of a fit,
  ### so members may stop at different epochs and each history is kept.
  testthat::expect_length(fitted_nn$fit_nn, 3)
})

test_that("ensemble members are independently initialised", {
  skip_if_no_tensorflow()

  data <- make_training_data()
  fitted_nn <- fit_test_model(n_ensembles = 3, data = data)

  member_predictions <- lapply(
    fitted_nn$model_nn@members,
    function(member) as.numeric(stats::predict(member, x = as.matrix(data$features)))
  )

  ### If a caller pinned a single global seed and the loop failed to advance the
  ### RNG, every member would train identically and the feature would silently
  ### be a no-op. Distinct members are the whole point of averaging.
  testthat::expect_false(isTRUE(all.equal(member_predictions[[1]], member_predictions[[2]])))
  testthat::expect_false(isTRUE(all.equal(member_predictions[[1]], member_predictions[[3]])))
})

test_that("the ensemble forecast is the mean of its members' forecasts", {
  skip_if_no_tensorflow()

  data <- make_training_data()
  fitted_nn <- fit_test_model(n_ensembles = 3, data = data)
  new_data <- as.matrix(data$features)

  member_predictions <- vapply(
    fitted_nn$model_nn@members,
    function(member) as.numeric(stats::predict(member, x = new_data)),
    numeric(nrow(new_data))
  )
  expected <- rowMeans(member_predictions)

  ### Forecasts are averaged, never weights: hidden units are permutation
  ### symmetric, so averaging weights across initialisations is meaningless.
  actual <- predict(fitted_nn$model_nn, x = new_data)

  testthat::expect_equal(actual, expected, tolerance = 1e-6)
})

test_that("the ensemble predicts on a single-row cross-section", {
  skip_if_no_tensorflow()

  data <- make_training_data()
  fitted_nn <- fit_test_model(n_ensembles = 3, data = data)

  ### A one-asset cross-section is a real case at a sparse rebalance date, and
  ### it is where a vapply/rowMeans implementation collapses to a bare vector.
  one_row <- as.matrix(data$features[1, , drop = FALSE])
  prediction <- predict(fitted_nn$model_nn, x = one_row)

  testthat::expect_length(prediction, 1)
  testthat::expect_true(is.finite(prediction))
})


# Tuning stays single-network --------------------------------------------

test_that("hyperparameter tuning fits one network even when the architecture asks for more", {
  skip_if_no_tensorflow()

  data <- make_training_data()
  training_sample <- data$features
  training_sample$target_fwd <- data$target

  eval_function <- factoRverse::set_eval_function(ml_algorithm = "nn", tuning_method = "random_search")

  ### Ensembling applies to the refit only. Both reference papers tune a single
  ### network and average only at refit time; letting the tuning path ensemble
  ### would multiply the search cost by n_ensembles with no stated benefit.
  tuning_output <- eval_function(
    regularizer_l1 = 0, regularizer_l2 = 0, droprate = 0.1, lr = 0.01,
    number_of_epochs = 2, size_of_batch = 16,
    target_fwd_name = "target_fwd",
    full_data_training_sample_clean = training_sample,
    features_validation_sample = cbind(
      data.frame(id = seq_len(10), tickers = "AAAA3", dates = Sys.Date()),
      data$features[seq_len(10), ]
    ),
    target_validation_sample = data$target[seq_len(10)],
    chosen_eval_metric = "rmse", huber_delta = 1, quantile_tau = 0.5,
    custom_objective_translated = nn_objective,
    chosen_eval_metric_translated = nn_metric,
    keras_architecture_parameters = make_arch_list(n_ensembles = 3),
    early_stop = NULL,
    return_all_info = TRUE,
    verbose = FALSE
  )

  testthat::expect_false(methods::is(tuning_output$model_nn, "keras_ensemble"))
})


# Variance reduction ------------------------------------------------------

test_that("averaging reduces the dispersion of the refit forecast", {
  skip_if_no_tensorflow()
  testthat::skip_on_ci()
  testthat::skip_on_cran()

  ### This is the test that demonstrates the feature does what it claims, but it
  ### is a stochastic comparison over repeated network fits: slow, and not a
  ### pass/fail signal that belongs in the default suite. Variance of a mean of
  ### k independent draws scales as 1/k, so with k = 5 the ensemble forecast
  ### should be markedly more stable across repetitions than a single network.

  ### Seed the network initialisations. make_training_data() restores the caller's
  ### RNG state precisely so that seeding the data does not pin them, and nothing
  ### seeded the TensorFlow generator at all, so the two standard deviations below
  ### were compared over an unseeded draw and the assertion flipped intermittently.
  ### Seeding once here leaves the fits distinct from one another while making the
  ### whole sequence reproducible.
  tensorflow::set_random_seed(123)

  data <- make_training_data()
  new_data <- as.matrix(data$features)

  ### A seed alone would make the test reproducible without making it informative:
  ### the sd of 5 values carries 4 degrees of freedom, so even granting the ideal
  ### 1/k variance reduction the ordering reverses often enough that a fixed seed
  ### would be pinning a near coin flip rather than the feature. Averaging also only
  ### cancels initialisation noise, not the signal the networks share from identical
  ### training data, so the realised separation is smaller than 1/k. More
  ### repetitions is what buys the comparison its power; the runtime is affordable
  ### because this test is excluded from CI by the skips above and only ever runs on
  ### a machine with a configured backend.
  n_repetitions <- 15

  forecast_at_first_asset <- function(n_ensembles) {
    vapply(seq_len(n_repetitions), function(i) {
      fitted_nn <- fit_test_model(n_ensembles = n_ensembles, data = data, epochs = 5)
      as.numeric(predict(fitted_nn$model_nn, x = new_data))[1]
    }, numeric(1))
  }

  single_network_sd <- stats::sd(forecast_at_first_asset(1))
  ensemble_sd <- stats::sd(forecast_at_first_asset(5))

  testthat::expect_lt(ensemble_sd, single_network_sd)
})


# Batch-normalization layer assembly ---------------------------------------
#
# fit_keras_model() assembles 1 to 5 hidden blocks, each of which is a dense
# layer, an optional batch-normalization layer, and a dropout layer, followed
# by a single linear output unit. The optional layer is selected inline in
# the magrittr pipeline, and that selection is what these tests pin: the
# assembled network must have the expected depth and must end in one output
# unit for every combination of depth and batch_norm_option, so that the
# number of predictions always equals the number of rows scored.
#
# The build, compile and fit steps report failures through message() and
# carry on, so a mis-assembled network does not announce itself. Asserting
# on the assembled model is therefore the only way these defects surface.

fit_architecture <- function(batch_norm_option, data = make_training_data()) {
  n_layers <- length(batch_norm_option)
  architecture <- list(
    units = c(4, 3, 2, 2, 2)[seq_len(n_layers)],
    n_layers = n_layers,
    activation = rep("relu", n_layers),
    nn_optimizer = "Adam",
    batch_norm_option = batch_norm_option
  )

  factoRverse::fit_keras_model(
    regularizer_l1 = 0, regularizer_l2 = 0, droprate = 0.1, lr = 0.01,
    number_of_epochs = 2, size_of_batch = 16,
    keras_architecture_parameters = architecture,
    early_stop = NULL,
    custom_objective_translated = nn_objective, huber_delta = 1,
    features_matrix_train_clean = data$features,
    target_vector_train = data$target,
    verbose = FALSE,
    chosen_eval_metric_translated = nn_metric
  )
}

### Each hidden block contributes a dense layer, a batch-normalization layer
### only when requested, and a dropout layer; the output unit adds one more.
expected_layer_count <- function(batch_norm_option) {
  2 * length(batch_norm_option) + sum(batch_norm_option) + 1
}

expect_well_formed_network <- function(batch_norm_option) {
  data <- make_training_data()
  fitted_nn <- fit_architecture(batch_norm_option, data = data)
  model_nn <- fitted_nn$model_nn

  ### Depth: a dropped or short-circuited block shows up here first.
  testthat::expect_equal(length(model_nn$layers), expected_layer_count(batch_norm_option))

  ### The network must terminate in a single linear unit. Without it the last
  ### hidden width leaks out as the output width, and training still appears to
  ### succeed because the loss broadcasts against the target.
  testthat::expect_equal(as.integer(utils::tail(model_nn$output_shape, 1)[[1]]), 1L)

  ### The behavioural consequence, and the one that corrupts a backtest: one
  ### prediction per row scored, so predictions stay aligned with the id/date keys.
  predictions <- as.numeric(stats::predict(model_nn, x = as.matrix(data$features)))
  testthat::expect_length(predictions, nrow(data$features))

  ### Training must actually have happened.
  testthat::expect_false(is.null(fitted_nn$fit_nn))
}


## Batch normalization enabled on every layer -------------------------------

test_that("networks assemble correctly with batch normalization on every layer", {
  skip_if_no_tensorflow()

  for (n_layers in 1:5) {
    expect_well_formed_network(rep(TRUE, n_layers))
  }
})


## Batch normalization disabled on every layer ------------------------------

test_that("networks assemble correctly with batch normalization on no layer", {
  skip_if_no_tensorflow()

  ### Regression test. The inline selection used to call the piped model as a
  ### function on the FALSE branch, which aborted layer assembly and left a
  ### network consisting of the first dense layer alone: no output unit, and
  ### one prediction per hidden unit per row instead of one prediction per row.
  for (n_layers in 1:5) {
    expect_well_formed_network(rep(FALSE, n_layers))
  }
})


## Batch normalization mixed across layers -----------------------------------

test_that("networks assemble correctly with batch normalization on some layers", {
  skip_if_no_tensorflow()

  ### Mixed settings exercise both branches within a single assembly, which is
  ### where a per-layer indexing error would show up.
  expect_well_formed_network(c(TRUE, FALSE, TRUE))
  expect_well_formed_network(c(FALSE, TRUE, FALSE, TRUE, FALSE))
})
