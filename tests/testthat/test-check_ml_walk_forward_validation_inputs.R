# Define your test
test_that("ml_walk_forward_validation throws an error when features_m_df is not matrix or data.frame", {

  load(paste(test_path(),"/testdata/","artificial_ml_wf_val_obj.RData", sep =""))

  wrong_features_m_df <- c("Stock A-2001-03-15",
                            "Stock A-2001-04-15","Stock A-2001-05-15",
                            "Stock A-2001-06-15","Stock A-2001-07-15",
                            "Stock A-2001-08-15",
                            "Stock B-2001-03-15","Stock B-2001-04-15",
                            "Stock B-2001-05-15","Stock B-2001-06-15",
                            "Stock B-2001-07-15","Stock B-2001-08-15",
                            "Stock C-2001-03-15","Stock C-2001-04-15",
                            "Stock C-2001-05-15",
                            "Stock C-2001-06-15","Stock C-2001-07-15",
                            "Stock C-2001-08-15","Stock D-2001-03-15",
                            "Stock D-2001-04-15","Stock D-2001-05-15",
                            "Stock D-2001-06-15",
                            "Stock D-2001-07-15","Stock D-2001-08-15",
                            "Stock E-2001-03-15","Stock E-2001-04-15",
                            "Stock E-2001-05-15","Stock E-2001-06-15",
                            "Stock E-2001-07-15","Stock E-2001-08-15")

  expect_error(
    suppressMessages(suppressWarnings({
      ml_walk_forward_validation(
      features_m_df = wrong_features_m_df,
      target_m_df = target_m_df,
      training_sample_size = 4,
      rebalancing_months = 9,
      ml_algorithm = "ols",
      target_fwd_name = "fwd_premium_1m")
    })),
    "features_m_df should be coercible to meta_dataframe object"
  )

})

# Define your test
test_that("ml_walk_forward_validation throws an error when features_m_df don't have adequate structure", {

  load(paste(test_path(),"/testdata/","artificial_ml_wf_val_obj.RData", sep =""))

  wrong_features_m_df <- features_m_df[,-1]

  expect_error(
    suppressMessages(suppressWarnings({
      ml_walk_forward_validation(
        features_m_df = wrong_features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        rebalancing_months = 9,
        ml_algorithm = "ols",
        target_fwd_name = "fwd_premium_1m")
    })),
    "features_m_df should be coercible to meta_dataframe object"
  )

  wrong_features_m_df <- features_m_df
  wrong_features_m_df$tickers[1] <- 2

  expect_error(
    suppressMessages(suppressWarnings({
      ml_walk_forward_validation(
        features_m_df = wrong_features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        rebalancing_months = 9,
        ml_algorithm = "ols",
        target_fwd_name = "fwd_premium_1m")
    })),
    "features_m_df should be coercible to meta_dataframe object"
  )

  wrong_features_m_df <- features_m_df
  wrong_features_m_df$Alpha[1] <- "two"

  expect_error(
    suppressMessages(suppressWarnings({
      ml_walk_forward_validation(
        features_m_df = wrong_features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        rebalancing_months = 9,
        ml_algorithm = "ols",
        target_fwd_name = "fwd_premium_1m")
    })),
    "features_m_df should contain only numeric columns."
  )

  wrong_features_m_df <- features_m_df
  wrong_features_m_df$Alpha[1] <- NA

  expect_error(
    suppressMessages(suppressWarnings({
      ml_walk_forward_validation(
        features_m_df = wrong_features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        rebalancing_months = 9,
        ml_algorithm = "ols",
        target_fwd_name = "fwd_premium_1m")
    })),
    "features_m_df should contain only numeric columns with non-NAs."
  )

  wrong_features_m_df <- features_m_df
  wrong_features_m_df$Alpha[1] <- NA
  expect_error(
    suppressMessages(suppressWarnings({
      ml_walk_forward_validation(
        features_m_df = wrong_features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        rebalancing_months = 9,
        ml_algorithm = "ols",
        target_fwd_name = "fwd_premium_1m")
    })),
    "features_m_df should contain only numeric columns with non-NAs."
  )

  #DATES IN ASCENDING ORDER
  wrong_features_m_df <- features_m_df
  wrong_features_m_df <- wrong_features_m_df[order(wrong_features_m_df$dates, decreasing = TRUE),]
  wrong_target_m_df <- target_m_df
  wrong_target_m_df <- wrong_target_m_df[order(wrong_target_m_df$dates, decreasing = TRUE),]


  suppressWarnings(expect_error(
    suppressMessages(ml_walk_forward_validation(
      features_m_df = wrong_features_m_df,
      target_m_df = wrong_target_m_df,
      training_sample_size = 4,
      rebalancing_months = 9,
      ml_algorithm = "ols",
      target_fwd_name = "fwd_premium_1m")
      )

  ))


})

# Define your test
test_that("ml_walk_forward_validation throws an error when target_m_df is not matrix or data.frame", {

  load(paste(test_path(),"/testdata/","artificial_ml_wf_val_obj.RData", sep =""))

  wrong_target_m_df <- target_m_df
  wrong_target_m_df <- c(1,2,3,4,5,6)

  expect_error(
    suppressMessages(suppressWarnings({
      ml_walk_forward_validation(
        features_m_df = features_m_df,
        target_m_df = wrong_target_m_df,
        training_sample_size = 4,
        rebalancing_months = 9,
        ml_algorithm = "ols",
        target_fwd_name = "fwd_premium_1m")
    })),
    "target_m_df should be coercible to meta_dataframe object"
  )

})

# Define your test
test_that("ml_walk_forward_validation throws an error when target_m_df do not have adequate structure", {

  load(paste(test_path(),"/testdata/","artificial_ml_wf_val_obj.RData", sep =""))

  wrong_target_m_df <- target_m_df
  wrong_target_m_df$tickers <- NULL

  expect_error(
    suppressMessages(suppressWarnings({
      ml_walk_forward_validation(
        features_m_df = features_m_df,
        target_m_df = wrong_target_m_df,
        training_sample_size = 4,
        rebalancing_months = 9,
        ml_algorithm = "ols",
        target_fwd_name = "fwd_premium_1m")
    })),
    "target_m_df should be coercible to meta_dataframe object"
  )


  wrong_target_m_df <- target_m_df
  wrong_target_m_df$tickers[2] <- 2

  expect_error(
    suppressMessages(suppressWarnings({
      ml_walk_forward_validation(
        features_m_df = features_m_df,
        target_m_df = wrong_target_m_df,
        training_sample_size = 4,
        rebalancing_months = 9,
        ml_algorithm = "ols",
        target_fwd_name = "fwd_premium_1m")
    })),
    "target_m_df should be coercible to meta_dataframe object"
  )


  wrong_target_m_df <- target_m_df
  wrong_target_m_df$fwd_premium_1m[1] <- "NA"

  expect_error(
    suppressMessages(suppressWarnings({
      ml_walk_forward_validation(
        features_m_df = features_m_df,
        target_m_df = wrong_target_m_df,
        training_sample_size = 4,
        rebalancing_months = 9,
        ml_algorithm = "ols",
        target_fwd_name = "fwd_premium_1m")
    }))
  )


  wrong_target_m_df <- target_m_df
  wrong_target_m_df[which(target_m_df$dates %in% c("2001-05-15", "2001-06-15", "2001-07-15", "2001-08-15")),-c(1:3)] <- NA

  expect_error(
    suppressMessages(suppressWarnings({
      ml_walk_forward_validation(
        features_m_df = features_m_df,
        target_m_df = wrong_target_m_df,
        training_sample_size = 4,
        rebalancing_months = 9,
        ml_algorithm = "ols",
        target_fwd_name = "fwd_premium_3m")
    })),
    "target_m_df can't have NAs until the last target_fwd periods"
  )

  wrong_target_m_df <- target_m_df
  wrong_target_m_df$fwd_premium_1m[2] <- NA

  expect_error(
    suppressMessages(suppressWarnings({
      ml_walk_forward_validation(
        features_m_df = features_m_df,
        target_m_df = wrong_target_m_df,
        training_sample_size = 4,
        rebalancing_months = 9,
        ml_algorithm = "ols",
        target_fwd_name = "fwd_premium_1m")
    })),
    "target_m_df can't have NAs until the last target_fwd periods"
  )


  #No error if adeaute number of NAs
  right_target_m_df <- target_m_df
  right_target_m_df[which(right_target_m_df$dates %in% c("2001-06-15", "2001-07-15", "2001-08-15")),-c(1:3)] <- NA

  expect_no_error(
    suppressMessages(suppressWarnings({
      check_inputs_ml_wf_val(
        features_m_df = features_m_df,
        target_m_df = right_target_m_df,
        training_sample_size = 4,
        rebalancing_months = 9,
        split_method = "expanding",
        validation_sample_size = 0,
        chosen_eval_metric = "rmse",
        quantile_tau = 0.5,
        early_stop = NULL,
        huber_delta = 1,
        n_iter = 3,
        custom_objective = "squared_error",
        tuning_method = "random_search",
        ml_algorithm = "ols",
        target_fwd_name = "fwd_premium_3m",
        verbose = TRUE)
    }))
  )

  #But yes error for target_fwd = 1

  expect_error(
    suppressMessages(suppressWarnings({
      check_inputs_ml_wf_val(
        features_m_df = features_m_df,
        target_m_df = right_target_m_df,
        training_sample_size = 4,
        rebalancing_months = 9,
        split_method = "expanding",
        validation_sample_size = 0,
        chosen_eval_metric = "rmse",
        quantile_tau = 0.5,
        early_stop = NULL,
        huber_delta = 1,
        n_iter = 3,
        custom_objective = "squared_error",
        tuning_method = "random_search",
        ml_algorithm = "ols",
        target_fwd_name = "fwd_premium_1m",
        verbose = TRUE)
    }))
  )


})

#Define your test
test_that("ml_walk_forward_validation throws an error when target_m_df do not have same structure as features_m_df.", {

  load(paste(test_path(),"/testdata/","artificial_ml_wf_val_obj.RData", sep =""))

  wrong_target_m_df <- target_m_df
  wrong_target_m_df <- wrong_target_m_df[-1,]

  expect_error(
    suppressMessages(
      ml_walk_forward_validation(
        features_m_df = features_m_df,
        target_m_df = wrong_target_m_df,
        training_sample_size = 4,
        rebalancing_months = 9,
        ml_algorithm = "ols",
        target_fwd_name = "fwd_premium_1m")
    )
    ,
    "features_m_df and target_m_df must possess same number of rows."
  )

  wrong_target_m_df <- target_m_df
  wrong_target_m_df$id[1] <- c("Stock A-2001-02-15")
  wrong_target_m_df$dates[1] <- as.Date(c("2001-02-15"))


  expect_error(
    suppressMessages(
      ml_walk_forward_validation(
        features_m_df = features_m_df,
        target_m_df = wrong_target_m_df,
        training_sample_size = 4,
        rebalancing_months = 9,
        ml_algorithm = "ols",
        target_fwd_name = "fwd_premium_1m")
    )
    ,
    "id in features_m_df and in target_m_df must match."
  )

  wrong_target_m_df <- target_m_df
  wrong_target_m_df$tickers[3] <- c("Stock Z")

  expect_error(
    suppressMessages(
      ml_walk_forward_validation(
        features_m_df = features_m_df,
        target_m_df = wrong_target_m_df,
        training_sample_size = 4,
        rebalancing_months = 9,
        ml_algorithm = "ols",
        target_fwd_name = "fwd_premium_1m")
    )
  )


  wrong_target_m_df <- target_m_df
  wrong_target_m_df$dates[3] <- as.Date(c("2001-04-16"), format = "%Y-%m-%d")
  wrong_target_m_df$id[3] <- c("Stock A-2001-04-16")

  expect_error(
    suppressMessages(
      ml_walk_forward_validation(
        features_m_df = features_m_df,
        target_m_df = wrong_target_m_df,
        training_sample_size = 4,
        rebalancing_months = 9,
        ml_algorithm = "ols",
        target_fwd_name = "fwd_premium_1m")
    )
    ,
    "id in features_m_df and in target_m_df must match."
  )


  wrong_features_m_df <- features_m_df
  wrong_features_m_df$dates <- as.factor(features_m_df$dates)
  wrong_target_m_df <- target_m_df
  wrong_target_m_df$dates <- as.factor(target_m_df$dates)


  suppressWarnings(expect_error(
    suppressMessages(ml_walk_forward_validation(
      features_m_df = wrong_features_m_df,
      target_m_df = wrong_target_m_df,
      training_sample_size = 4,
      rebalancing_months = 9,
      ml_algorithm = "ols",
      n_iter = NULL,
      target_fwd_name = "fwd_premium_1m"))
  ))


})

# Define your test
test_that("ml_walk_forward_validation throws an error when dates are less than target_fwd", {

  load(paste(test_path(),"/testdata/","artificial_ml_wf_val_obj.RData", sep =""))

  wrong_dates_m_vector <- features_m_df$dates
  short_features_m_df <- features_m_df[c(1:2),]
  short_target_m_df <- target_m_df[c(1:2),]
  short_dates_m_vector <-  as.Date(c("2001-03-15", "2001-04-15"), format = "%Y-%m-%d")


  expect_error(
    suppressMessages(suppressWarnings({
      ml_walk_forward_validation(
        features_m_df = short_features_m_df,
        target_m_df = short_target_m_df,
        training_sample_size = 4,
        rebalancing_months = 9,
        ml_algorithm = "ols",
        target_fwd_name = "fwd_premium_3m")
    })),
    "target_m_df and features_m_df should have more dates than the prediction horizon"
  )


})

# Define your test
test_that("ml_walk_forward_validation throws an error when dates are not in correct order", {

  load(paste(test_path(),"/testdata/","artificial_ml_wf_val_obj.RData", sep =""))

  wrong_features_m_df <- features_m_df[order(features_m_df$dates, decreasing = TRUE), ]
  wrong_target_m_df <- features_m_df[order(features_m_df$dates, decreasing = TRUE), ]


  expect_error(
    suppressMessages(suppressWarnings({
      ml_walk_forward_validation(
        features_m_df = wrong_features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        rebalancing_months = 9,
        ml_algorithm = "ols",
        target_fwd_name = "fwd_premium_3m")
    })),
    "features_m_df should be coercible to meta_dataframe object"
  )


})

# Define your test
test_that("ml_walk_forward_validation throws an error when rebalancing_months, training_sample_size, validation_sample_size, split_method are not numeric or not appropriate.", {

  load(paste(test_path(),"/testdata/","artificial_ml_wf_val_obj.RData", sep =""))

  expect_error(
    suppressMessages(suppressWarnings({
      ml_walk_forward_validation(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        rebalancing_months = "nine",
        ml_algorithm = "ols",
        target_fwd_name = "fwd_premium_3m")
    })),
    "rebalancing_months should be numeric."
  )

  expect_error(
    suppressMessages(suppressWarnings({
      ml_walk_forward_validation(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = "four",
        rebalancing_months = 9,
        ml_algorithm = "ols",
        target_fwd_name = "fwd_premium_3m")
    })),
    "training_sample_size should be numeric."
  )

  expect_error(
    suppressMessages(suppressWarnings({
      ml_walk_forward_validation(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 2,
        rebalancing_months = 9,
        ml_algorithm = "ols",
        target_fwd_name = "fwd_premium_3m")
    })),
    "ols do not support validation split."
  )


  expect_error(
    suppressMessages(suppressWarnings({
      ml_walk_forward_validation(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = "one",
        rebalancing_months = 9,
        tuning_method = "grid_search",
        hyper_grid_domain_list = list(alpha = c(1,0.5), lambda.min.ratio = c(0.1,0.2)),
        ml_algorithm = "glmnet",
        target_fwd_name = "fwd_premium_1m")
    })),
    "validation_sample_size should be numeric."
  )

  expect_error(
    suppressMessages(suppressWarnings({
      ml_walk_forward_validation(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        tuning_method = "grid_search",
        hyper_grid_domain_list = list(alpha = c(1,0.5), lambda.min.ratio = c(0.1,0.2)),
        ml_algorithm = "glmnet",
        split_method = "rolling",
        target_fwd_name = "fwd_premium_1m")
    })),
    "split_method should be expanding."
  )

})

# Define your test
test_that("ml_walk_forward_validation throws an error when eval_metric not correctly set.", {

  load(paste(test_path(),"/testdata/","artificial_ml_wf_val_obj.RData", sep =""))

  expect_error(
    suppressMessages(suppressWarnings({
      ml_walk_forward_validation(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        tuning_method = "grid_search",
        hyper_grid_domain_list = list(alpha = c(1,0.5), lambda.min.ratio = c(0.1,0.2)),
        ml_algorithm = "glmnet",
        chosen_eval_metric = "rsquared",
        target_fwd_name = "fwd_premium_1m")
    })),
    "chosen_eval_metric choice not supported."
  )

  expect_error(
    suppressMessages(suppressWarnings({
      ml_walk_forward_validation(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        tuning_method = "grid_search",
        hyper_grid_domain_list = list(alpha = c(1,0.5), lambda.min.ratio = c(0.1,0.2)),
        ml_algorithm = "glmnet",
        huber_delta = "one",
        target_fwd_name = "fwd_premium_1m")
    })),
    "huber_delta should be numeric."
  )




  expect_error(
    suppressMessages(suppressWarnings({
      ml_walk_forward_validation(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        tuning_method = "grid_search",
        hyper_grid_domain_list = list(alpha = c(1,0.5), lambda.min.ratio = c(0.1,0.2)),
        ml_algorithm = "glmnet",
        quantile_tau = 0,
        target_fwd_name = "fwd_premium_1m")
    })),
    "quantile_tau should be > 0 and less than 1."
  )


})

# Define your test
test_that("ml_walk_forward_validation throws an error when keras network is not correctly set.", {

  load(paste(test_path(),"/testdata/","artificial_ml_wf_val_obj.RData", sep =""))

  #Keras Architecture Set as DF
  expect_error(
    suppressMessages(suppressWarnings({
      ml_walk_forward_validation(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        tuning_method = "grid_search",
        hyper_grid_domain_list = list(regularizer_l1 = c(1,0.5), regularizer_l2 = c(1,2), droprate = c(0.50), lr = 0.02, size_of_batch = 512, number_of_epochs = 100),
        ml_algorithm = "nn",
        keras_architecture_parameters = data.frame(units = 32,  n_layers = 1, activation = 'relu', nn_optimizer = 'Adam', batch_norm_option = TRUE),
        chosen_eval_metric = "rss",
        target_fwd_name = "fwd_premium_1m")
    })),
    "keras_architecture_parameters should be a list with units, n_layers, activation, nn_optimizer and batch_norm_option elements"
  )

  #Keras Architecture missing nn_optimizer
  expect_error(
    suppressMessages(suppressWarnings({
      ml_walk_forward_validation(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        tuning_method = "grid_search",
        hyper_grid_domain_list = list(regularizer_l1 = c(1,0.5), regularizer_l2 = c(1,2), droprate = c(0.50), lr = 0.02, size_of_batch = 512, number_of_epochs = 100),
        ml_algorithm = "nn",
        keras_architecture_parameters = list(units = 32,  n_layers = 1, activation = 'relu', batch_norm_option = TRUE),
        chosen_eval_metric = "rss",
        target_fwd_name = "fwd_premium_1m")
    })),
    "keras_architecture_parameters should be a list with units, n_layers, activation, nn_optimizer and batch_norm_option elements"
  )

  #Keras Architecture with wrong n_layersr
  expect_error(
    suppressMessages(suppressWarnings({
      ml_walk_forward_validation(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        tuning_method = "grid_search",
        hyper_grid_domain_list = list(regularizer_l1 = c(1,0.5), regularizer_l2 = c(1,2), droprate = c(0.50), lr = 0.02, size_of_batch = 512, number_of_epochs = 100),
        ml_algorithm = "nn",
        keras_architecture_parameters = list(units = 32,  n_layers = 6, activation = 'relu', nn_optimizer = 'Adam', batch_norm_option = TRUE),
        chosen_eval_metric = "rss",
        target_fwd_name = "fwd_premium_1m")
    })),
    "n_layers should be an integer between 1 and 5."
  )

  #Keras Architecture with wrong activation
  expect_error(
    suppressMessages(suppressWarnings({
      ml_walk_forward_validation(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        tuning_method = "grid_search",
        hyper_grid_domain_list = list(regularizer_l1 = c(1,0.5), regularizer_l2 = c(1,2), droprate = c(0.50), lr = 0.02, size_of_batch = 512, number_of_epochs = 100),
        ml_algorithm = "nn",
        keras_architecture_parameters = list(units = 32,  n_layers = 5, activation = 'relus', nn_optimizer = 'Adam', batch_norm_option = TRUE),
        chosen_eval_metric = "rss",
        target_fwd_name = "fwd_premium_1m")
    })),
    "activation should be one of relu, sigmoid, softmax, softplus, tanh or leaky_relu."
  )

  #Keras with wrong optimizer
  expect_error(
    suppressMessages(suppressWarnings({
      ml_walk_forward_validation(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        tuning_method = "grid_search",
        hyper_grid_domain_list = list(regularizer_l1 = c(1,0.5), regularizer_l2 = c(1,2), droprate = c(0.50), lr = 0.02, size_of_batch = 512, number_of_epochs = 100),
        ml_algorithm = "nn",
        keras_architecture_parameters = list(units = c(32,16),  n_layers = 2, activation = c('relu','relu'), nn_optimizer = 'SGD', batch_norm_option = c(FALSE,FALSE)),
        chosen_eval_metric = "rss",
        target_fwd_name = "fwd_premium_1m")
    })),
    "nn_optimizer should be Adam or RMSProp."
  )

  #Keras with wrong number of units
  expect_error(
    suppressMessages(suppressWarnings({
      ml_walk_forward_validation(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        tuning_method = "grid_search",
        hyper_grid_domain_list = list(regularizer_l1 = c(1,0.5), regularizer_l2 = c(1,2), droprate = c(0.50), lr = 0.02, size_of_batch = 512, number_of_epochs = 100),
        ml_algorithm = "nn",
        keras_architecture_parameters = list(units = c(32,16),  n_layers = 3, activation = c('relu', 'relu', 'softmax'), nn_optimizer = 'Adam',
                                             batch_norm_option = c(TRUE, FALSE, TRUE)),
        chosen_eval_metric = "rss",
        target_fwd_name = "fwd_premium_1m")
    })),
    "length of units, activation and batch_norm_option should match n_layers"
  )

})

# Define your test
test_that("ml_walk_forward_validation throws no error when keras network is correctly set.", {
  load(paste(test_path(),"/testdata/","artificial_ml_wf_val_obj.RData", sep =""))


  #Keras 1-Layer Architecture
  expect_no_error(
    suppressMessages(suppressWarnings({
      check_inputs_ml_wf_val(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        parallel = FALSE,
        early_stop = NULL,
        custom_objective = "squared_error",
        n_iter = NULL,
        quantile_tau = 0.5,
        verbose = TRUE,
        huber_delta = 0.5,
        split_method = "expanding",
        tuning_method = "grid_search",
        hyper_grid_domain_list = list(regularizer_l1 = c(1,0.5), regularizer_l2 = c(1,2), droprate = c(0.50), lr = 0.02,
                                      size_of_batch = 512, number_of_epochs = 100),
        ml_algorithm = "nn",
        keras_architecture_parameters = list(units = 32,  n_layers = 1, activation = 'relu', nn_optimizer = 'Adam', batch_norm_option = TRUE),
        chosen_eval_metric = "rss",
        target_fwd_name = "fwd_premium_1m")
    }))
  )

  #Keras 2-Layers Architecture
  expect_no_error(
    suppressMessages(suppressWarnings({
      check_inputs_ml_wf_val(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        verbose = TRUE,
        parallel = FALSE,
        early_stop = NULL,
        custom_objective = "squared_error",
        n_iter = NULL,
        quantile_tau = 0.5,
        huber_delta = 0.5,
        split_method = "expanding",
        tuning_method = "grid_search",
        hyper_grid_domain_list = list(regularizer_l1 = c(1,0.5), regularizer_l2 = c(1,2), droprate = c(0.50), lr = 0.02,
                                      size_of_batch = 512, number_of_epochs = 100),
        ml_algorithm = "nn",
        keras_architecture_parameters = list(units = c(32,16),  n_layers = 2, activation = c('relu', 'relu'), nn_optimizer = 'Adam', batch_norm_option = c(TRUE, FALSE)),
        chosen_eval_metric = "rss",
        target_fwd_name = "fwd_premium_1m")
    }))
  )

  #Keras 3-Layers Architecture
  expect_no_error(
    suppressMessages(suppressWarnings({
      check_inputs_ml_wf_val(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        verbose = TRUE,
        rebalancing_months = 9,
        parallel = FALSE,
        early_stop = NULL,
        custom_objective = "squared_error",
        n_iter = NULL,
        quantile_tau = 0.5,
        huber_delta = 0.5,
        split_method = "expanding",
        tuning_method = "grid_search",
        hyper_grid_domain_list = list(regularizer_l1 = c(1,0.5), regularizer_l2 = c(1,2), droprate = c(0.50), lr = 0.02,
                                      size_of_batch = 512, number_of_epochs = 100),
        ml_algorithm = "nn",
        keras_architecture_parameters = list(units = c(32,16,8),  n_layers = 3, activation = c('relu', 'relu', 'tanh'), nn_optimizer = 'Adam', batch_norm_option = c(TRUE, FALSE, FALSE)),
        chosen_eval_metric = "rss",
        target_fwd_name = "fwd_premium_1m")
    }))
  )

})

# Define your test
test_that("ml_walk_forward_validation does not throw an error when hyperparameters_grid_list are correctly set.", {

  load(paste(test_path(),"/testdata/","artificial_ml_wf_val_obj.RData", sep =""))

  #GLMNET
  suppressWarnings(
  expect_no_error(
      ml_walk_forward_validation(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        tuning_method = "grid_search",
        hyper_grid_domain_list = list(alpha = c(1,0.5), lambda.min.ratio = c(0.1,0.2)),
        ml_algorithm = "glmnet",
        chosen_eval_metric = "rss",
        target_fwd_name = "fwd_premium_1m")
  )
  )


  #RF
  suppressWarnings(
    expect_no_error(
      ml_walk_forward_validation(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        tuning_method = "grid_search",
        hyper_grid_domain_list = list(mtry = c(1,0.5), num.trees = c(200),  max.depth = c(2, 2), min.bucket = 5),
        ml_algorithm = "rf",
        show_plots = FALSE,
        chosen_eval_metric = "rss",
        target_fwd_name = "fwd_premium_1m")

    )
  )


  #XGB
  suppressMessages(suppressWarnings(
    expect_no_error(
      ml_walk_forward_validation(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        tuning_method = "grid_search",
        hyper_grid_domain_list = list(min_child_weight = c(1,0.5), max_depth = c(2, 5), subsample = c(0.2), colsample_bytree = 0.5,
                                      eta = c(0.5), alpha = c(0), gamma = 0, nrounds = 100),
        ml_algorithm = "xgb",
        chosen_eval_metric = "rss",
        verbose = FALSE,
        show_plots = FALSE,
        target_fwd_name = "fwd_premium_1m")

    )
  )
  )


  #NN
  suppressMessages(suppressWarnings(
    expect_no_error(
      ml_walk_forward_validation(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        tuning_method = "grid_search",
        hyper_grid_domain_list = list(regularizer_l1 = c(1,0.5), regularizer_l2 = c(2, 5), droprate = c(0.2), lr = 0.5,
                                      size_of_batch = c(512), number_of_epochs = c(100)),
        ml_algorithm = "nn",
        keras_architecture_parameters = list(units = 32, n_layers = 1, activation = 'relu', nn_optimizer = 'Adam', batch_norm_option = TRUE),
        chosen_eval_metric = "rss",
        verbose = FALSE,
        show_plots = FALSE,
        target_fwd_name = "fwd_premium_1m")

    )
  )
  )


})

# Define your test
test_that("ml_walk_forward_validation does not throw an error when hyperparameters_grid_list is not correctly set.", {

  load(paste(test_path(),"/testdata/","artificial_ml_wf_val_obj.RData", sep =""))

  #GLMNET
  suppressWarnings(
    expect_error(
      ml_walk_forward_validation(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        tuning_method = "grid_search",
        hyper_grid_domain_list = list(alpha = c(1,1.1), lambda.min.ratio = c(0.1,0.2)),
        ml_algorithm = "glmnet",
        chosen_eval_metric = "rss",
        target_fwd_name = "fwd_premium_1m")
    ), "alpha should be set in interval [0,1]"
  )

  suppressWarnings(
    expect_error(
      ml_walk_forward_validation(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        tuning_method = "grid_search",
        hyper_grid_domain_list = list(alpha = c(1,0.9), lambda.min.ratio = c(0.1,1)),
        ml_algorithm = "glmnet",
        chosen_eval_metric = "rss",
        target_fwd_name = "fwd_premium_1m")
    ), "lambda.min.ratio should be set in interval [0,1)"
  )


  #RF
  suppressWarnings(
    expect_error(
      ml_walk_forward_validation(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        tuning_method = "random_search",
        hyper_grid_domain_list = list(mtry = list(distribution_choice = "uniform",
                                                  pars = c(min = 0.5, max = 1)),
                                      num.trees = list(distribution_choice = "uniform",
                                                  pars = c(min = 2, max = 3)),
                                      max.depth = list(distribution_choice = "uniform",
                                                  pars = c(min = 1L, max = 2L)),
                                      min.bucket = list(distribution_choice = "uniform",
                                                  pars = c(min = 1, max = 3))),
        ml_algorithm = "rf",
        show_plots = FALSE,
        n_iter = 2,
        chosen_eval_metric = "rss",
        target_fwd_name = "fwd_premium_1m")

    ), "num.trees should be integer"
  )


  #XGB
  suppressMessages(suppressWarnings(
    expect_error(
      ml_walk_forward_validation(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        tuning_method = "bayesian_opt",
        hyper_grid_domain_list = list(min_child_weight = c(1,0.5), max_depth = c(2, 5), subsample = c(0.2, 0.5),
                                      colsample_bytree = c(0.5,1),
                                      eta = c(0.5,1), alpha = c(0,2), gamma = c(0,1), nrounds = c(100,200)),
        ml_algorithm = "xgb",
        chosen_eval_metric = "rss",
        verbose = FALSE,
        show_plots = FALSE,
        n_iter = 2,
        init_points = 10,
        k_iter = 1,
        target_fwd_name = "fwd_premium_1m")
    ), "max_depth should be integer"
  )
  )


  #NN
  suppressMessages(suppressWarnings(
    expect_error(
      ml_walk_forward_validation(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        tuning_method = "random_search",
        hyper_grid_domain_list = list(regularizer_l1 = list(distribution_choice = "constant",
                                                            value = c(0, 1)),
                                      regularizer_l2 = list(distribution_choice = "uniform",
                                                            pars = c(min = 2, max = 5)),
                                      droprate = list(distribution_choice = "uniform",
                                                            pars = c(min = 2, max = 5)),
                                      lr = list(distribution_choice = "constant",
                                                            value = c(0, 1)),
                                      size_of_batch = list(distribution_choice = "constant",
                                                            value = c(0, 1)),
                                      number_of_epochs = list(distribution_choice = "constant",
                                                            value = c(0, 1))),
        ml_algorithm = "nn",
        keras_architecture_parameters = list(units = 32, n_layers = 1, activation = 'relu', nn_optimizer = 'Adam', batch_norm_option = TRUE),
        chosen_eval_metric = "rss",
        verbose = FALSE,
        n_iter = 2,
        show_plots = FALSE,
        parallel = FALSE,
        target_fwd_name = "fwd_premium_1m")
    ), "droprate should be set in interval [0,1]"
  )
  )

})

# Define your test
test_that("ml_walk_forward_validation throws an error when grid_search not correctly set.", {


  load(paste(test_path(),"/testdata/","artificial_ml_wf_val_obj.RData", sep =""))

  expect_error(
    suppressMessages(suppressWarnings({
      ml_walk_forward_validation(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        tuning_method = "grd_search",
        hyper_grid_domain_list = list(alpha = c(1,0.5), lambda.min.ratio = c(0.1,0.2)),
        ml_algorithm = "glmnet",
        chosen_eval_metric = "rss",
        target_fwd_name = "fwd_premium_1m")
    })),
    "tuning_method should be one of random_search, grid_search or bayesian_opt."
  )

  expect_error(
    suppressMessages(suppressWarnings({
      ml_walk_forward_validation(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        tuning_method = "grid_search",
        hyper_grid_domain_list = data.frame(alpha=c(1,0.5), lambda.min.ratio = c(0.1,0.2)),
        ml_algorithm = "glmnet",
        chosen_eval_metric = "rss",
        target_fwd_name = "fwd_premium_1m")
    })),
    "hyper_grid_domain_list not in correct format for grid_search tuning."
  )

})

# Define your test
test_that("ml_walk_forward_validation throws an error when random_search not correctly set.", {


  load(paste(test_path(),"/testdata/","artificial_ml_wf_val_obj.RData", sep =""))

  expect_error(
    suppressMessages(suppressWarnings({
      ml_walk_forward_validation(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        tuning_method = "rand_search",
        hyper_grid_domain_list = list(alpha = c(1,0.5), lambda.min.ratio = c(0.1,0.2)),
        ml_algorithm = "glmnet",
        chosen_eval_metric = "rss",
        target_fwd_name = "fwd_premium_1m")
    })),
    "tuning_method should be one of random_search, grid_search or bayesian_opt."
  )

  expect_error(
    suppressMessages(suppressWarnings({
      ml_walk_forward_validation(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        tuning_method = "random_search",
        hyper_grid_domain_list = data.frame(alpha=c(1,0.5), lambda.min.ratio = c(0.1,0.2)),
        ml_algorithm = "glmnet",
        chosen_eval_metric = "rss",
        target_fwd_name = "fwd_premium_1m")
    })),
    "hyper_grid_domain_list not in correct format for random_search tuning."
  )

  expect_error(
    suppressMessages(suppressWarnings({
      ml_walk_forward_validation(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        tuning_method = "random_search",
        hyper_grid_domain_list = list(alpha=c(1,0.5), lambda.min.ratio = c(0.1,0.2)),
        ml_algorithm = "glmnet",
        chosen_eval_metric = "rss",
        target_fwd_name = "fwd_premium_1m")
    })),
    "hyper_grid_domain_list not in correct format for random_search tuning."
  )

  expect_error(
    suppressMessages(suppressWarnings({
      ml_walk_forward_validation(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        tuning_method = "random_search",
        hyper_grid_domain_list = list(alpha = data.frame(distribution_choice = "uniform", pars = c(min = 0,max = 1)),
                                      lambda.min.ratio = list(distribution_choice = "uniform", pars = c(min = 0.1, max = 0.2))),
        ml_algorithm = "glmnet",
        chosen_eval_metric = "rss",
        target_fwd_name = "fwd_premium_1m")
    })),
    "hyper_grid_domain_list not in correct format for random_search tuning."
  )


  expect_error(
    suppressMessages(suppressWarnings({
      ml_walk_forward_validation(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        tuning_method = "random_search",
        hyper_grid_domain_list = list(alpha = list(distribution = "uniform", pars = c(min = 0,max = 1)),
                                      lambda.min.ratio = list(distribution_choice = "uniform", pars = c(min = 0.1, max = 0.9))),
        ml_algorithm = "glmnet",
        chosen_eval_metric = "rss",
        target_fwd_name = "fwd_premium_1m")
    })),
    "hyper_grid_domain_list not in correct format for random_search tuning."
  )

  expect_error(
    suppressMessages(suppressWarnings({
      ml_walk_forward_validation(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        tuning_method = "random_search",
        hyper_grid_domain_list = list(alpha = list(distribution_choice = "uniform", pars = c(a = 0,b = 1)),
                                      lambda.min.ratio = list(distribution_choice = "uniform", pars = c(min = 0.1, max = 0.5))),
        ml_algorithm = "glmnet",
        chosen_eval_metric = "rss",
        target_fwd_name = "fwd_premium_1m")
    })),
    "hyper_grid_domain_list not in correct format for random_search tuning."
  )

  expect_error(
    suppressMessages(suppressWarnings({
      ml_walk_forward_validation(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        tuning_method = "random_search",
        hyper_grid_domain_list = list(alpha = list(distribution_choice = "lognormal", pars = c(mean = 0,sd = 1)),
                                      lambda.min.ratio = list(distribution_choice = "uniform", pars = c(min = 0.1, max = 0.9))),
        ml_algorithm = "glmnet",
        chosen_eval_metric = "rss",
        target_fwd_name = "fwd_premium_1m")
    })),
    "hyper_grid_domain_list not in correct format for random_search tuning."
  )


})

# Define your test
test_that("ml_walk_forward_validation throws an error when bayesian_opt not correctly set.", {


  load(paste(test_path(),"/testdata/","artificial_ml_wf_val_obj.RData", sep =""))

  #Three elements instead of two
  expect_error(
    suppressMessages(suppressWarnings({
      ml_walk_forward_validation(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        tuning_method = "bayesian_opt",
        hyper_grid_domain_list = list(alpha = c(0,0.5,1), lambda.min.ratio = c(0.1,0.2)),
        ml_algorithm = "glmnet",
        chosen_eval_metric = "rss",
        target_fwd_name = "fwd_premium_1m")
    })),
    "hyper_grid_domain_list not in correct format for bayesian_opt tuning."
  )

  #Not numeric
  expect_error(
    suppressMessages(suppressWarnings({
      ml_walk_forward_validation(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        n_iter = "four",
        init_points = 2,
        k_iter = 3,
        tuning_method = "bayesian_opt",
        hyper_grid_domain_list = list(alpha = c(0,1), lambda.min.ratio = c(0.1,0.2)),
        ml_algorithm = "glmnet",
        chosen_eval_metric = "rss",
        target_fwd_name = "fwd_premium_1m")
    })),
    "n_iter, k_iter and init_points must be numeric."
  )


})

# Define your test
test_that("ml_walk_forward_validation throws an error when custom_objective wrongly set.", {


  load(paste(test_path(),"/testdata/","artificial_ml_wf_val_obj.RData", sep =""))

  #Setting custom obj for glmnet
  expect_error(
    suppressMessages(suppressWarnings({
      ml_walk_forward_validation(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        tuning_method = "bayesian_opt",
        hyper_grid_domain_list = list(alpha = c(0,0.8), lambda.min.ratio = c(0.1,0.2)),
        n_iter = 3,
        k_iter = 1,
        init_points = 4,
        custom_objective = "pseudo_huber_error",
        ml_algorithm = "glmnet",
        chosen_eval_metric = "rss",
        target_fwd_name = "fwd_premium_1m")
    })),
    "Custom objective functions are only allowed for xgb or nn ml_algorithm choices"
  )


  #Seting wrong custom opbj
  expect_error(
    suppressMessages(suppressWarnings({
      ml_walk_forward_validation(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        tuning_method = "grid_search",
        hyper_grid_domain_list = list(min_child_weight = c(1,3), max_depth = c(1,2), subsample = c(0.3),
                                      colsample_bytree = c(0,0.2),
                                      eta = c(0,1), alpha = c(0,2), gamma = c(0,1), nrounds = 200),
        custom_objective = "quantile_error",
        ml_algorithm = "xgb",
        chosen_eval_metric = "rss",
        target_fwd_name = "fwd_premium_1m")
    })),
    "Possible choices for custom_objective are squared_error, pseudo_huber_error and absolute_error"
  )

  #Early stop
  expect_error(
    suppressMessages(suppressWarnings({
      ml_walk_forward_validation(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        tuning_method = "grid_search",
        hyper_grid_domain_list = list(alpha = c(0,1), lambda.min.ratio = c(0.1,0.2)),
        custom_objective = "squared_error",
        ml_algorithm = "glmnet",
        chosen_eval_metric = "rss",
        early_stop = 10,
        target_fwd_name = "fwd_premium_1m")
    })),
    "Early stop only allowed for xgb or nn ml_algorithm choices"
  )


})


#Data quality tests
# Define your test
test_that("toy_preprocessed_features_and_targets has adequate format",{
  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))
  #User inputs
  target_fwd_name = "fwd_premium_3m"
  ml_algorithm = "rf"
  tuning_method = "grid_search"
  chosen_eval_metric = "rmse"
  custom_objective = "squared_error"
  split_method = "expanding"
  training_sample_size <- 6
  validation_sample_size <- 4
  huber_delta <-  1
  quantile_tau = 0.5
  hyper_grid_domain_list <- list(mtry = c(0, 1), num.trees = c(200, 500),
                                 max.depth = c(2), min.bucket = c(1, 10,15))


  #Check Inputs
  expect_no_error(
  suppressWarnings(
  check_inputs_ml_wf_val(features_m_df = toy_preprocessed_features, target_m_df = toy_preprocessed_targets,
                         training_sample_size = training_sample_size, target_fwd_name = target_fwd_name,
                         validation_sample_size = validation_sample_size, rebalancing_months = 6, split_method = split_method,
                         chosen_eval_metric = chosen_eval_metric,
                         ml_algorithm = ml_algorithm, custom_objective = custom_objective, huber_delta = huber_delta, quantile_tau = quantile_tau,
                         hyper_grid_domain_list = hyper_grid_domain_list, tuning_method = tuning_method,
                         n_iter = NULL, k_iter = NULL, acq = NULL, init_points = NULL, early_stop = NULL, keras_architecture_parameters = NULL,
                         parallel = FALSE, verbose = TRUE
  )
  )
  )


})


# Define your test
test_that("artificial_ml_wf_val_obj has adequate format",{
  load(paste(test_path(),"/testdata/","artificial_ml_wf_val_obj.RData", sep =""))
  #User inputs
  target_fwd_name = "fwd_premium_3m"
  ml_algorithm = "rf"
  tuning_method = "grid_search"
  chosen_eval_metric = "rmse"
  custom_objective = "squared_error"
  split_method = "expanding"
  training_sample_size <- 6
  validation_sample_size <- 4
  huber_delta <-  1
  quantile_tau = 0.5
  hyper_grid_domain_list <- list(mtry = c(0, 1), num.trees = c(200, 500),
                                 max.depth = c(2), min.bucket = c(1, 10,15))


  #Check Inputs
  expect_no_error(
    check_inputs_ml_wf_val(features_m_df = features_m_df, target_m_df = target_m_df,
                           training_sample_size = training_sample_size, target_fwd_name = target_fwd_name,
                           validation_sample_size = validation_sample_size, rebalancing_months = 6, split_method = split_method,
                           chosen_eval_metric = chosen_eval_metric,
                           ml_algorithm = ml_algorithm, custom_objective = custom_objective, huber_delta = huber_delta, quantile_tau = quantile_tau,
                           hyper_grid_domain_list = hyper_grid_domain_list, tuning_method = tuning_method,
                           n_iter = NULL, k_iter = NULL, acq = NULL, init_points = NULL, early_stop = NULL, keras_architecture_parameters = NULL,
                           parallel = FALSE, verbose = TRUE
    )
  )


})


# Define your test
test_that("toy_fulldates_features_and_targets has adequate format",{
  load(paste(test_path(),"/testdata/","toy_fulldates_features_and_targets.RData", sep =""))
  #User inputs
  target_fwd_name = "fwd_premium_3m"
  ml_algorithm = "rf"
  tuning_method = "grid_search"
  chosen_eval_metric = "rmse"
  custom_objective = "squared_error"
  split_method = "expanding"
  training_sample_size <- 6
  validation_sample_size <- 4
  huber_delta <-  1
  quantile_tau = 0.5
  hyper_grid_domain_list <- list(mtry = c(0, 1), num.trees = c(200, 500),
                                 max.depth = c(2), min.bucket = c(1, 10,15))


  #Check Inputs
  expect_no_error(
    check_inputs_ml_wf_val(features_m_df = toy_features_full_dates, target_m_df = toy_target_full_date,
                           training_sample_size = training_sample_size, target_fwd_name = target_fwd_name,
                           validation_sample_size = validation_sample_size, rebalancing_months = 6, split_method = split_method,
                           chosen_eval_metric = chosen_eval_metric,
                           ml_algorithm = ml_algorithm, custom_objective = custom_objective, huber_delta = huber_delta, quantile_tau = quantile_tau,
                           hyper_grid_domain_list = hyper_grid_domain_list, tuning_method = tuning_method,
                           n_iter = NULL, k_iter = NULL, acq = NULL, init_points = NULL, early_stop = NULL, keras_architecture_parameters = NULL,
                           parallel = FALSE, verbose = TRUE
    )
  )


})



