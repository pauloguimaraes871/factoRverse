# Define test cases
test_that("calculate_eval_metrics computes metrics correctly", {

  # Example data
  pred <- c(1.1, 2.2, 1.05)
  target <- c(1.0, 2.0, 1.0)
  error <- target - pred

  # Calculate metrics
  metrics <- calculate_eval_metrics(pred, target, huber_delta = 1.2, quantile_tau = 0.7, chosen_eval_metric = "mphe")

  #Check RSS
  expect_equal(metrics$rss,
  1 - sum(error^2)/sum(target^2)
  )

  #Check CP
  expect_equal(metrics$cp,
               mean(pred*target)
  )

  #Check RMSE
  expect_equal(metrics$rmse,
  yardstick::rmse(data = data.frame(estimate = pred, truth = target), truth = truth, estimate = estimate)$.estimate
  )

  #Check MAE
  expect_equal(metrics$mae,
               yardstick::mae(data = data.frame(estimate = pred, truth = target), truth = truth, estimate = estimate)$.estimate
  )

  #Check MPHE
  expect_equal(metrics$mphe,
               yardstick::huber_loss_pseudo(data = data.frame(estimate = pred, truth = target), truth = truth, estimate = estimate, delta = 1.2)$.estimate
  )

  #Check MPE
  expect_equal(metrics$mpe,
               mean(ifelse(error>=0, error*0.7, -error*0.3))
  )

  #Check MAPE
  expect_equal(metrics$mape,
               yardstick::mape(data = data.frame(estimate = pred, truth = target), truth = truth, estimate = estimate)$.estimate/100
  )

  #Check HR
  expect_equal(metrics$hr,
              mean(pred*target > 0)
  )

  #Check MB
  expect_equal(metrics$mb,
               mean(error)
  )

  # Check if the output is a single-row data frame
  expect_equal(nrow(metrics), 1)


})

# Define test cases
test_that("calculate_eval_metrics correctly changes huber_delta and quantile_tau", {

  # Example data
  pred <- c(1.1, 2.2, 1.05)
  target <- c(1.0, 2.0, 1.0)
  error <- target - pred

  # Calculate metrics
  metrics <- calculate_eval_metrics(pred, target, huber_delta = 0.7, quantile_tau = 0.9, chosen_eval_metric = "mphe")

  #Check MPHE
  expect_equal(metrics$mphe,
               yardstick::huber_loss_pseudo(data = data.frame(estimate = pred, truth = target), truth = truth, estimate = estimate, delta = 0.7)$.estimate
  )


  #Check MPE
  expect_equal(metrics$mpe,
               mean(ifelse(error>=0, error*0.9, -error*0.1))
  )

  #Check MB
  expect_equal(metrics$mb,
               mean(error))


})

#Define test case
test_that("calculate_eval_metric correctly sets chosen_eval_metric as score", {

  # Example data
  pred <- c(1.1, 2.2, 1.05)
  target <- c(1.0, 2.0, 1.0)
  error <- target - pred

  #Compare
  expect_equal(calculate_eval_metrics(pred, target, huber_delta = 1, quantile_tau = 0.5, chosen_eval_metric = "rmse")$Score,
               calculate_eval_metrics(pred, target, huber_delta = 1, quantile_tau = 0.5, chosen_eval_metric = "rmse")$rmse*-1)

  expect_equal(calculate_eval_metrics(pred, target, huber_delta = 1, quantile_tau = 0.5, chosen_eval_metric = "rss")$Score,
               calculate_eval_metrics(pred, target, huber_delta = 1, quantile_tau = 0.5, chosen_eval_metric = "rss")$rss)

  expect_equal(calculate_eval_metrics(pred, target, huber_delta = 1, quantile_tau = 0.5, chosen_eval_metric = "cp")$Score,
               calculate_eval_metrics(pred, target, huber_delta = 1, quantile_tau = 0.5, chosen_eval_metric = "cp")$cp)

  expect_equal(calculate_eval_metrics(pred, target, huber_delta = 1, quantile_tau = 0.5, chosen_eval_metric = "mae")$Score,
               calculate_eval_metrics(pred, target, huber_delta = 1, quantile_tau = 0.5, chosen_eval_metric = "mae")$mae*-1)

  expect_equal(calculate_eval_metrics(pred, target, huber_delta = 1.1, quantile_tau = 0.5, chosen_eval_metric = "mphe")$Score,
               calculate_eval_metrics(pred, target, huber_delta = 1.1, quantile_tau = 0.5, chosen_eval_metric = "mphe")$mphe*-1)

  expect_equal(calculate_eval_metrics(pred, target, huber_delta = 1, quantile_tau = 0.7, chosen_eval_metric = "mpe")$Score,
               calculate_eval_metrics(pred, target, huber_delta = 1, quantile_tau = 0.7, chosen_eval_metric = "mpe")$mpe*-1)

  expect_equal(calculate_eval_metrics(pred, target, huber_delta = 1, quantile_tau = 0.5, chosen_eval_metric = "mape")$Score,
               calculate_eval_metrics(pred, target, huber_delta = 1, quantile_tau = 0.5, chosen_eval_metric = "mape")$mape*-1)

  expect_equal(calculate_eval_metrics(pred, target, huber_delta = 1, quantile_tau = 0.5, chosen_eval_metric = "hr")$Score,
               calculate_eval_metrics(pred, target, huber_delta = 1, quantile_tau = 0.5, chosen_eval_metric = "hr")$hr)



})

# Define test cases
test_that("calculate_eval_metrics returns error", {

  # Example data
  error <- c(0.1, -0.2, -0.05)
  pred <- c(0.9, 2.2, 1.05)
  target <- c(1.0, 2.0, 1.0)

  expect_equal(
    calculate_eval_metrics(pred = pred, target = target, huber_delta = 1, quantile_tau= 0.5, return_error = TRUE)$error,
    error
  )



})


# Define test cases
test_that("calculate_eval_metrics handles NA in pred and target", {

  # Example data
  error <- c(0.1, -0.2, 0.05)
  pred <- c(NA, 2.2, 1.05)
  target <- c(1.0, 2.0, 1.0)

  expect_equal(
    calculate_eval_metrics(pred = pred, target = target, huber_delta = 1, quantile_tau= 0.5),
    data.frame(Score = NA_real_, rss = NA, cp = NA, rmse = NA, mae = NA, mphe = NA, mpe = NA, mape = NA, hr = NA, mb = NA)
  )

  # Example data
  error <- c(0.1, -0.2, 0.05)
  pred <- c(NA, NA, NA)
  target <- c(1.0, 2.0, 1.0)

  expect_equal(
    calculate_eval_metrics(pred = pred, target = target, huber_delta = 1, quantile_tau= 0.5),
    data.frame(Score = NA_real_, rss = NA, cp = NA, rmse = NA, mae = NA, mphe = NA, mpe = NA, mape = NA, hr = NA, mb = NA)
  )



  # Example data
  error <- c(0.1, -0.2, 0.05)
  pred <- c(0.9, 2.2, 1.05)
  target <- c(NA, NA, NA)

  expect_equal(
    calculate_eval_metrics(pred = pred, target = target, huber_delta = 1, quantile_tau= 0.5),
    data.frame(Score = NA_real_, rss = NA, cp = NA, rmse = NA, mae = NA, mphe = NA, mpe = NA, mape = NA, hr = NA, mb = NA)
  )



})

# Define test cases
test_that("calculate_eval_metrics throws an error when chosen_eval_metric is wrong", {

  # Example data
  error <- c(0.1, -0.2, 0.05)
  pred <- c(0.9, 2.2, 1.05)
  target <- c(1.0, 2.0, 1.0)

  expect_error(
    calculate_eval_metrics(pred = pred, target = target, huber_delta = 1, quantile_tau= 0.5, chosen_eval_metric = "RMSE"),
    "chosen_eval_metric should be one of rmse, rss, cp, mae, mphe, mpe, mape, hr"
  )

})

# Define test cases
test_that("calculate_eval_metrics correctly handles best iteration from early stopping", {

  # Example data
  pred <- c(1.1, 2.2, 1.05)
  target <- c(1.0, 2.0, 1.0)
  error <- target - pred
  best_iteration <- 25
  early_stop <- 15


  # Calculate metrics
  metrics <- calculate_eval_metrics(pred, target, huber_delta = 1.2, quantile_tau = 0.7, chosen_eval_metric = "mphe",
                                    best_iteration = best_iteration, early_stop = early_stop)

  #Check
  expect_equal(metrics$best_iteration,
               25
  )

  #Check rows
  expect_equal(length(metrics),
               11)

  #Check NULL

  # Calculate metrics
  metrics <- calculate_eval_metrics(pred, target, huber_delta = 1.2, quantile_tau = 0.7, chosen_eval_metric = "mphe",
                                    best_iteration = NULL)

  expect_null(metrics$best_iteration
  )


  #Check Error message
  expect_error(calculate_eval_metrics(pred, target, huber_delta = 1.2, quantile_tau = 0.7, chosen_eval_metric = "mphe",
                                    best_iteration = "character"),
               "best_iteration should either be NULL or numeric.")





})

