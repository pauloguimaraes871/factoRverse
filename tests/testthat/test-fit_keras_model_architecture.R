# Tests for Keras network assembly across topologies and batch-normalization settings
#
# fit_keras_model() assembles 1 to 5 hidden blocks, each of which is a dense
# layer, an optional batch-normalization layer, and a dropout layer, followed by
# a single linear output unit. The optional layer is selected inline in the
# magrittr pipeline, and that selection is what these tests pin: the assembled
# network must have the expected depth and must end in one output unit for every
# combination of depth and batch_norm_option, so that the number of predictions
# always equals the number of rows scored.
#
# The build, compile and fit steps report failures through message() and carry
# on, so a mis-assembled network does not announce itself. Asserting on the
# assembled model is therefore the only way these defects surface.

## Helpers -------------------------------------------------------------------

### Small, well-conditioned regression problem; these tests fit real networks,
### so the point is the shape of the model, not the quality of the fit.
make_training_data <- function(n = 60, p = 3, seed = 1) {
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

### Loss and metric shapes produced by translate_metrics() for "nn".
nn_objective <- "mean_squared_error"
nn_metric <- list(metric = "mean_squared_error",
                  name = "val_mean_squared_error",
                  mode = "min")

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


# Batch normalization enabled on every layer -----------------------------

test_that("networks assemble correctly with batch normalization on every layer", {
  skip_if_no_tensorflow()

  for (n_layers in 1:5) {
    expect_well_formed_network(rep(TRUE, n_layers))
  }
})


# Batch normalization disabled on every layer ----------------------------

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


# Batch normalization mixed across layers --------------------------------

test_that("networks assemble correctly with batch normalization on some layers", {
  skip_if_no_tensorflow()

  ### Mixed settings exercise both branches within a single assembly, which is
  ### where a per-layer indexing error would show up.
  expect_well_formed_network(c(TRUE, FALSE, TRUE))
  expect_well_formed_network(c(FALSE, TRUE, FALSE, TRUE, FALSE))
})
