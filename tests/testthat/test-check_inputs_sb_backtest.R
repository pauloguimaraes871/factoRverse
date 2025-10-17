# Define your test
test_that("check_inputs_sb_backtest throws an error when features_m_df don't have adequate structure", {


  load(paste(test_path(),"/testdata/","artificial_signal_blending_obj.RData", sep =""))

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
    check_inputs_sb_backtest(
      features_m_df = wrong_features_m_df,
      target_m_df = target_m_df,
      training_sample_size = 4,
      rebalancing_months = 9,
      sb_algorithm = "ols",
      target_fwd_name = "fwd_premium_1m"),
    "features_m_df should be coercible to meta_dataframe object"
  )


  load(paste(test_path(),"/testdata/","artificial_signal_blending_obj.RData", sep =""))

  wrong_features_m_df <- features_m_df[,-1]

  expect_error(
      check_inputs_sb_backtest(
        features_m_df = wrong_features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        rebalancing_months = 9,
        sb_algorithm = "ols",
        target_fwd_name = "fwd_premium_1m"),
    "features_m_df should be coercible to meta_dataframe object"
  )

  wrong_features_m_df <- features_m_df
  wrong_features_m_df$tickers[1] <- 2

  expect_error(
      check_inputs_sb_backtest(
        features_m_df = wrong_features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        rebalancing_months = 9,
        sb_algorithm = "ols",
        target_fwd_name = "fwd_premium_1m"),
    "features_m_df should be coercible to meta_dataframe object"
  )

  wrong_features_m_df <- features_m_df
  wrong_features_m_df$Alpha[1] <- "two"

  expect_error(
      check_inputs_sb_backtest(
        features_m_df = wrong_features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        rebalancing_months = 9,
        sb_algorithm = "ols",
        target_fwd_name = "fwd_premium_1m"),
    "features_m_df should contain only numeric columns with non-NAs."
  )

  wrong_features_m_df <- features_m_df
  wrong_features_m_df$Alpha[1] <- NA

  expect_error(
      check_inputs_sb_backtest(
        features_m_df = wrong_features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        rebalancing_months = 9,
        sb_algorithm = "ols",
        target_fwd_name = "fwd_premium_1m")
    ,
    "features_m_df should contain only numeric columns with non-NAs."
  )

  wrong_features_m_df <- features_m_df
  wrong_features_m_df$Alpha[1] <- NA
  expect_error(
      check_inputs_sb_backtest(
        features_m_df = wrong_features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        rebalancing_months = 9,
        sb_algorithm = "ols",
        target_fwd_name = "fwd_premium_1m")
    ,
    "features_m_df should contain only numeric columns with non-NAs."
  )

  #DATES IN ASCENDING ORDER
  wrong_features_m_df <- features_m_df
  wrong_features_m_df <- wrong_features_m_df[order(wrong_features_m_df$dates, decreasing = TRUE),]
  wrong_target_m_df <- target_m_df
  wrong_target_m_df <- wrong_target_m_df[order(wrong_target_m_df$dates, decreasing = TRUE),]


  expect_error(
  check_inputs_sb_backtest(
      features_m_df = wrong_features_m_df,
      target_m_df = wrong_target_m_df,
      training_sample_size = 4,
      rebalancing_months = 9,
      sb_algorithm = "ols",
      target_fwd_name = "fwd_premium_1m"
      ),
    "features_m_df should be coercible to meta_dataframe object"
  )

  load(paste(test_path(),"/testdata/","artificial_signal_blending_obj.RData", sep =""))

  wrong_features_m_df <- features_m_df
  colnames(wrong_features_m_df)[5] <- "low_Beta"

  expect_error(
    check_inputs_sb_backtest(
      features_m_df = wrong_features_m_df,
      target_m_df = target_m_df,
      training_sample_size = 4,
      rebalancing_months = 9,
      sb_algorithm = "ols",
      target_fwd_name = "fwd_premium_1m"),
    "features_m_df column names should not contain 'low_'."
  )

})

# Define your test
test_that("check_inputs_sb_backtest throws an error when target_m_df do not have adequate structure", {

  load(paste(test_path(),"/testdata/","artificial_signal_blending_obj.RData", sep =""))

  wrong_target_m_df <- target_m_df
  wrong_target_m_df <- c(1,2,3,4,5,6)

  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = wrong_target_m_df,
      training_sample_size = 4,
      validation_sample_size = 0,
      split_method = "expanding",
      gsm_algorithm = "ols",
      rebalancing_months = 9,
      sb_algorithm = "ols",
      target_fwd_name = "fwd_premium_1m"),
    "target_m_df should be coercible to meta_dataframe object"
  )

  load(paste(test_path(),"/testdata/","artificial_signal_blending_obj.RData", sep =""))

  wrong_target_m_df <- target_m_df
  wrong_target_m_df$tickers <- NULL

  expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        target_m_df = wrong_target_m_df,
        training_sample_size = 4,
        rebalancing_months = 9,
        validation_sample_size = 0,
        split_method = "expanding",
        gsm_algorithm = "ols",
        sb_algorithm = "ols",
        target_fwd_name = "fwd_premium_1m"),
    "target_m_df should be coercible to meta_dataframe object"
  )


  wrong_target_m_df <- target_m_df
  wrong_target_m_df$tickers[2] <- 2

  expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        target_m_df = wrong_target_m_df,
        training_sample_size = 4,
        rebalancing_months = 9,
        sb_algorithm = "ols",
        validation_sample_size = 0,
        split_method = "expanding",
        gsm_algorithm = "ols",
        target_fwd_name = "fwd_premium_1m"),
    "target_m_df should be coercible to meta_dataframe object"
  )


  wrong_target_m_df <- target_m_df
  wrong_target_m_df$fwd_premium_1m[1] <- "NA"

  expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        target_m_df = wrong_target_m_df,
        training_sample_size = 4,
        validation_sample_size = 0,
        split_method = "expanding",
        gsm_algorithm = "ols",
        rebalancing_months = 9,
        sb_algorithm = "ols",
        target_fwd_name = "fwd_premium_1m")
  )


  wrong_target_m_df <- target_m_df
  wrong_target_m_df[which(target_m_df$dates %in% c("2001-05-15", "2001-06-15", "2001-07-15", "2001-08-15")),-c(1:3)] <- NA

  expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        target_m_df = wrong_target_m_df,
        training_sample_size = 4,
        validation_sample_size = 0,
        split_method = "expanding",
        gsm_algorithm = "ols",
        rebalancing_months = 9,
        sb_algorithm = "ols",
        target_fwd_name = "fwd_premium_3m"),
    "target_m_df can't have NAs until the last target_fwd periods"
  )

  wrong_target_m_df <- target_m_df
  wrong_target_m_df$fwd_premium_1m[2] <- NA

  expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        target_m_df = wrong_target_m_df,
        training_sample_size = 4,
        rebalancing_months = 9,
        validation_sample_size = 0,
        split_method = "expanding",
        gsm_algorithm = "ols",
        sb_algorithm = "ols",
        target_fwd_name = "fwd_premium_1m"),
    "target_m_df can't have NAs until the last target_fwd periods"
  )

  wrong_target_m_df <- target_m_df
  wrong_target_m_df[which(wrong_target_m_df$dates %in% c("2001-05-15", "2001-06-15", "2001-07-15", "2001-08-15")),-c(1:3)] <- NA

  expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        target_m_df = wrong_target_m_df,
        training_sample_size = 4,
        rebalancing_months = 9,
        validation_sample_size = 0,
        split_method = "expanding",
        gsm_algorithm = "ols",
        sb_algorithm = "ols",
        target_fwd_name = "fwd_premium_3m"),
    "target_m_df can't have NAs until the last target_fwd periods"
  )

  wrong_target_m_df <- target_m_df %>% dplyr::filter(dates %in% c("2001-03-15", "2001-04-15", "2001-05-15"))
  wrong_features_m_df <- features_m_df %>% dplyr::filter(dates %in% c("2001-03-15", "2001-04-15", "2001-05-15"))

  expect_error(
      check_inputs_sb_backtest(
        features_m_df = wrong_features_m_df,
        target_m_df = wrong_target_m_df,
        training_sample_size = 4,
        rebalancing_months = 9,
        split_method = "expanding",
        validation_sample_size = 0,
        gsm_algorithm = "ols",
        sb_algorithm = "ols",
        target_fwd_name = "fwd_premium_3m"),
    "training_sample_size plus validation_sample_size should be less than the number of unique dates in features_m_df."
  )


  #Only NAs in first rebalancing
  wrong_target_m_df <- target_m_df %>% dplyr::filter(dates %in% c("2001-04-15", "2001-05-15", "2001-06-15"))
  wrong_target_m_df[which(wrong_target_m_df$dates %in% c("2001-04-15", "2001-05-15", "2001-06-15")), c("fwd_premium_3m")] <- NA
  wrong_target_m_df[which(wrong_target_m_df$dates %in% c("2001-06-15")), c("fwd_premium_1m", "fwd_sharpe_1m")] <- NA

  wrong_features_m_df <- features_m_df %>% dplyr::filter(dates %in% c("2001-04-15", "2001-05-15", "2001-06-15"))

  expect_error(
      check_inputs_sb_backtest(
        features_m_df = wrong_features_m_df,
        target_m_df = wrong_target_m_df,
        training_sample_size = 3,
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
        sb_algorithm = "ols",
        target_fwd_name = "fwd_premium_3m",
        signal_universe_m_df = signal_universe_m_df,
        backtest_returns_m_xts = NULL,
        custom_signal_weights_m_df = NULL,
        signal_themes_m_df = NULL,
        benchmark_returns_m_xts = NULL,
        concentration_constraint_policy = NULL,
        hyper_grid_domain_list = NULL,
        gsm_algorith = "ols",
        verbose = TRUE)
  )



  #No error if adequate number of NAs
  right_target_m_df <- target_m_df
  right_target_m_df[which(right_target_m_df$dates %in% c("2001-06-15", "2001-07-15", "2001-08-15")),-c(1:3)] <- NA

  expect_no_error(
      check_inputs_sb_backtest(
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
        hyper_grid_domain_list = NULL,
        custom_objective = "squared_error",
        tuning_method = "random_search",
        sb_algorithm = "ols",
        signal_themes_m_df = NULL,
        gsm_algorithm = "ols",
        concentration_constraint_policy = NULL,
        custom_signal_weights_m_df = NULL,
        target_fwd_name = "fwd_premium_3m",
        signal_universe_m_df = signal_universe_m_df,
        backtest_returns_m_xts = NULL,
        benchmark_returns_m_xts = NULL,
        verbose = TRUE)
  )

  #But yes error for target_fwd = 1

  expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        target_m_df = right_target_m_df,
        training_sample_size = 4,
        rebalancing_months = 9,
        split_method = "expanding",
        validation_sample_size = 0,
        chosen_eval_metric = "rmse",
        gsm_algorithm = "ols",
        quantile_tau = 0.5,
        early_stop = NULL,
        huber_delta = 1,
        n_iter = 3,
        custom_objective = "squared_error",
        tuning_method = "random_search",
        sb_algorithm = "ols",
        target_fwd_name = "fwd_premium_1m",
        verbose = TRUE)
  )


  wrong_target_m_df <- target_m_df
  colnames(wrong_target_m_df)[5] <- "premium_2"

  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = NULL,
      benchmark_returns_m_xts = NULL,
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      custom_signal_weights_m_df = NULL,
      gsm_algorithm = "ols",
      target_m_df = wrong_target_m_df,
      training_sample_size = 3,
      validation_sample_size = 3,
      rebalancing_months = 9,
      split_method = "expanding",
      quantile_tau = 0.5,
      huber_delta = 1,
      custom_objective = "squared_error",
      n_iter =NULL,
      early_stop = NULL,
      tuning_method = "grid_search",
      hyper_grid_domain_list = list(alpha = c(1,0.5), lambda.min.ratio = c(0.1,0.2)),
      sb_algorithm = "glmnet",
      chosen_eval_metric = "rss",
      target_fwd_name = "fwd_premium_1m"),
    "target_m_df colnames should follow the format XXXX_number_m, where ' XXXX is the name of the target variable, number is the amount of forward periods and m indicates periods are measured in months."
  )

  wrong_target_m_df <- target_m_df
  wrong_target_m_df <- wrong_target_m_df %>% dplyr::mutate(fwd_premium_1m = dplyr::if_else(dates %in% c("2001-05-15", "2001-04-15"), NA, fwd_premium_1m))


  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = NULL,
      benchmark_returns_m_xts = NULL,
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      custom_signal_weights_m_df = NULL,
      gsm_algorithm = "ols",
      target_m_df = wrong_target_m_df,
      training_sample_size = 2,
      validation_sample_size = 1,
      rebalancing_months = 9,
      split_method = "expanding",
      quantile_tau = 0.5,
      huber_delta = 1,
      custom_objective = "squared_error",
      n_iter =NULL,
      early_stop = NULL,
      tuning_method = "grid_search",
      hyper_grid_domain_list = list(alpha = c(1,0.5), lambda.min.ratio = c(0.1,0.2)),
      sb_algorithm = "glmnet",
      chosen_eval_metric = "rss",
      target_fwd_name = "fwd_premium_1m"),
    "target_m_df can't have only NAs in the first rebalancing period"
  )

})

#Define your test
test_that("check_inputs_sb_backtest throws an error when target_m_df do not have same structure as features_m_df.", {

  load(paste(test_path(),"/testdata/","artificial_signal_blending_obj.RData", sep =""))

  wrong_target_m_df <- target_m_df
  wrong_target_m_df <- wrong_target_m_df[-1,]

  expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        target_m_df = wrong_target_m_df,
        split_method = "expanding",
        validation_sample_size = 0,
        gsm_algorithm = "ols",
        training_sample_size = 4,
        rebalancing_months = 9,
        sb_algorithm = "ols",
        target_fwd_name = "fwd_premium_1m")
    ,
    "features_m_df and target_m_df must possess same number of rows."
  )

  wrong_target_m_df <- target_m_df
  wrong_target_m_df$id[1] <- c("Stock A-2001-02-15")
  wrong_target_m_df$dates[1] <- as.Date(c("2001-02-15"))


  expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        target_m_df = wrong_target_m_df,
        split_method = "expanding",
        validation_sample_size = 0,
        gsm_algorithm = "ols",
        training_sample_size = 4,
        rebalancing_months = 9,
        sb_algorithm = "ols",
        target_fwd_name = "fwd_premium_1m")
    ,
    "id in features_m_df and in target_m_df must match."
  )

  wrong_target_m_df <- target_m_df
  wrong_target_m_df$tickers[3] <- c("Stock Z")

  expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        target_m_df = wrong_target_m_df,
        split_method = "expanding",
        validation_sample_size = 0,
        gsm_algorithm = "ols",
        training_sample_size = 4,
        rebalancing_months = 9,
        sb_algorithm = "ols",
        target_fwd_name = "fwd_premium_1m")

  )


  wrong_target_m_df <- target_m_df
  wrong_target_m_df$dates[3] <- as.Date(c("2001-04-16"), format = "%Y-%m-%d")
  wrong_target_m_df$id[3] <- c("Stock A-2001-04-16")

  expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        target_m_df = wrong_target_m_df,
        split_method = "expanding",
        validation_sample_size = 0,
        gsm_algorithm = "ols",
        training_sample_size = 4,
        rebalancing_months = 9,
        sb_algorithm = "ols",
        target_fwd_name = "fwd_premium_1m")
    ,
    "id in features_m_df and in target_m_df must match."
  )


  wrong_features_m_df <- features_m_df
  wrong_features_m_df$dates <- as.factor(features_m_df$dates)
  wrong_target_m_df <- target_m_df
  wrong_target_m_df$dates <- as.factor(target_m_df$dates)


  expect_error(
    check_inputs_sb_backtest(
      features_m_df = wrong_features_m_df,
      target_m_df = wrong_target_m_df,
      split_method = "expanding",
      validation_sample_size = 0,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      rebalancing_months = 9,
      sb_algorithm = "ols",
      n_iter = NULL,
      target_fwd_name = "fwd_premium_1m")
    )


})

# Define your test
test_that("check_inputs_sb_backtest throws an error when target_fwd_name is wrong",{

  load(paste(test_path(),"/testdata/","artificial_signal_blending_obj.RData", sep =""))

  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = NULL,
      benchmark_returns_m_xts = NULL,
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      custom_signal_weights_m_df = NULL,
      gsm_algorithm = "olms",
      target_m_df = target_m_df,
      training_sample_size = 4,
      validation_sample_size = 1,
      rebalancing_months = 9,
      split_method = "expanding",
      quantile_tau = 0.5,
      huber_delta = 1,
      custom_objective = "squared_error",
      n_iter =NULL,
      early_stop = NULL,
      tuning_method = "grid_search",
      hyper_grid_domain_list = list(alpha = c(1,0.5), lambda.min.ratio = c(0.1,0.2)),
      sb_algorithm = "glmnet",
      chosen_eval_metric = "rss",
      target_fwd_name = 3),
    "target_fwd_name should be character."
  )


  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = NULL,
      benchmark_returns_m_xts = NULL,
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      custom_signal_weights_m_df = NULL,
      gsm_algorithm = "olms",
      target_m_df = target_m_df,
      training_sample_size = 4,
      validation_sample_size = 1,
      rebalancing_months = 9,
      split_method = "expanding",
      quantile_tau = 0.5,
      huber_delta = 1,
      custom_objective = "squared_error",
      n_iter =NULL,
      early_stop = NULL,
      tuning_method = "grid_search",
      hyper_grid_domain_list = list(alpha = c(1,0.5), lambda.min.ratio = c(0.1,0.2)),
      sb_algorithm = "glmnet",
      chosen_eval_metric = "rss",
      target_fwd_name = "premium_3"),
    "target_fwd_name is not in the right pattern"
  )

  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = NULL,
      benchmark_returns_m_xts = NULL,
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      custom_signal_weights_m_df = NULL,
      gsm_algorithm = "ols",
      target_m_df = target_m_df,
      training_sample_size = 3,
      validation_sample_size = 3,
      rebalancing_months = 9,
      split_method = "expanding",
      quantile_tau = 0.5,
      huber_delta = 1,
      custom_objective = "squared_error",
      n_iter =NULL,
      early_stop = NULL,
      tuning_method = "grid_search",
      hyper_grid_domain_list = list(alpha = c(1,0.5), lambda.min.ratio = c(0.1,0.2)),
      sb_algorithm = "glmnet",
      chosen_eval_metric = "rss",
      target_fwd_name = "fwd_premium_2m"),
    "target_fwd_name is not in target_m_df"
  )


})

# Define your test
test_that("check_inputs_sb_backtest throws an error when dates are less than target_fwd", {

  load(paste(test_path(),"/testdata/","artificial_signal_blending_obj.RData", sep =""))

  wrong_dates_m_vector <- features_m_df$dates
  short_features_m_df <- features_m_df[c(1:2),]
  short_target_m_df <- target_m_df[c(1:2),]
  short_dates_m_vector <-  as.Date(c("2001-03-15", "2001-04-15"), format = "%Y-%m-%d")


  expect_error(
      check_inputs_sb_backtest(
        features_m_df = short_features_m_df,
        target_m_df = short_target_m_df,
        training_sample_size = 4,
        rebalancing_months = 9,
        split_method = "expanding",
        validation_sample_size = 0,
        gsm_algorithm = "ols",
        sb_algorithm = "ols",
        target_fwd_name = "fwd_premium_3m")
    ,
    "training_sample_size plus validation_sample_size should be less than the number of unique dates in features_m_df."
  )


})

# Define your test
test_that("check_inputs_sb_backtest throws an error when dates are not in correct order", {

  load(paste(test_path(),"/testdata/","artificial_signal_blending_obj.RData", sep =""))

  wrong_features_m_df <- features_m_df[order(features_m_df$dates, decreasing = TRUE), ]
  wrong_target_m_df <- features_m_df[order(features_m_df$dates, decreasing = TRUE), ]


  expect_error(
      check_inputs_sb_backtest(
        features_m_df = wrong_features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        rebalancing_months = 9,
        split_method = "expanding",
        validation_sample_size = 0,
        gsm_algorithm = "ols",
        sb_algorithm = "ols",
        target_fwd_name = "fwd_premium_3m"),
    "features_m_df should be coercible to meta_dataframe object"
  )


})

# Define your test
test_that("check_inputs_sb_backtest throws an error for wrong gsm obj",{

  load(paste(test_path(),"/testdata/","artificial_signal_blending_obj.RData", sep =""))

  #GSM
  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = NULL,
      benchmark_returns_m_xts = NULL,
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      custom_signal_weights_m_df = NULL,
      gsm_algorithm = "olms",
      target_m_df = target_m_df,
      training_sample_size = 4,
      validation_sample_size = 1,
      rebalancing_months = 9,
      split_method = "expanding",
      quantile_tau = 0.5,
      huber_delta = 1,
      custom_objective = "squared_error",
      n_iter =NULL,
      early_stop = NULL,
      tuning_method = "grid_search",
      hyper_grid_domain_list = list(alpha = c(1,0.5), lambda.min.ratio = c(0.1,0.2)),
      sb_algorithm = "glmnet",
      chosen_eval_metric = "rss",
      target_fwd_name = "fwd_premium_1m"),
    "gsm_algorithm should be either 'ols' or 'tree'."
  )
})

# Define your test
test_that("check_inputs_sb_backtest throws an error for wrong schema dates obj",{

  load(paste(test_path(),"/testdata/","artificial_signal_blending_obj.RData", sep =""))

  #rebal months
  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = NULL,
      benchmark_returns_m_xts = NULL,
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      custom_signal_weights_m_df = NULL,
      gsm_algorithm = "ols",
      target_m_df = target_m_df,
      training_sample_size = 4,
      validation_sample_size = 1,
      rebalancing_months = 15,
      split_method = "expanding",
      quantile_tau = 0.5,
      huber_delta = 1,
      custom_objective = "squared_error",
      n_iter =NULL,
      early_stop = NULL,
      tuning_method = "grid_search",
      hyper_grid_domain_list = list(alpha = c(1,0.5), lambda.min.ratio = c(0.1,0.2)),
      sb_algorithm = "glmnet",
      chosen_eval_metric = "rss",
      target_fwd_name = "fwd_premium_1m"),
    "rebalancing_months should be between 1 and 12."
  )

  #tra
  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = NULL,
      benchmark_returns_m_xts = NULL,
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      custom_signal_weights_m_df = NULL,
      gsm_algorithm = "ols",
      target_m_df = target_m_df,
      training_sample_size = 4,
      validation_sample_size = 1,
      rebalancing_months = 15,
      split_method = "expanding",
      quantile_tau = 0.5,
      huber_delta = 1,
      custom_objective = "squared_error",
      n_iter =NULL,
      early_stop = NULL,
      tuning_method = "grid_search",
      hyper_grid_domain_list = list(alpha = c(1,0.5), lambda.min.ratio = c(0.1,0.2)),
      sb_algorithm = "glmnet",
      chosen_eval_metric = "rss",
      target_fwd_name = "fwd_premium_1m"),
    "rebalancing_months should be between 1 and 12."
  )


  #val
  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = NULL,
      benchmark_returns_m_xts = NULL,
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      custom_signal_weights_m_df = NULL,
      gsm_algorithm = "ols",
      target_m_df = target_m_df,
      training_sample_size = 4,
      validation_sample_size = -2,
      rebalancing_months = 12,
      split_method = "expanding",
      quantile_tau = 0.5,
      huber_delta = 1,
      custom_objective = "squared_error",
      n_iter =NULL,
      early_stop = NULL,
      tuning_method = "grid_search",
      hyper_grid_domain_list = list(alpha = c(1,0.5), lambda.min.ratio = c(0.1,0.2)),
      sb_algorithm = "glmnet",
      chosen_eval_metric = "rss",
      target_fwd_name = "fwd_premium_1m"),
    "validation_sample_size should be positive."
  )

  #train + val
  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = NULL,
      benchmark_returns_m_xts = NULL,
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      custom_signal_weights_m_df = NULL,
      gsm_algorithm = "ols",
      target_m_df = target_m_df,
      training_sample_size = 6,
      validation_sample_size = 0,
      rebalancing_months = 12,
      split_method = "expanding",
      quantile_tau = 0.5,
      huber_delta = 1,
      custom_objective = "squared_error",
      n_iter =NULL,
      early_stop = NULL,
      tuning_method = "grid_search",
      hyper_grid_domain_list = list(alpha = c(1,0.5), lambda.min.ratio = c(0.1,0.2)),
      sb_algorithm = "glmnet",
      chosen_eval_metric = "rss",
      target_fwd_name = "fwd_premium_6m"),
    "training_sample_size should be bigger than target_fwd"
  )

  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = NULL,
      benchmark_returns_m_xts = NULL,
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      custom_signal_weights_m_df = NULL,
      gsm_algorithm = "olms",
      target_m_df = target_m_df,
      training_sample_size = 13,
      validation_sample_size = 1,
      rebalancing_months = 9,
      split_method = "expanding",
      quantile_tau = 0.5,
      huber_delta = 1,
      custom_objective = "squared_error",
      n_iter =NULL,
      early_stop = NULL,
      tuning_method = "grid_search",
      hyper_grid_domain_list = list(alpha = c(1,0.5), lambda.min.ratio = c(0.1,0.2)),
      sb_algorithm = "glmnet",
      chosen_eval_metric = "rss",
      target_fwd_name = "fwd_premium_12m"),
    "training_sample_size should be bigger than training_sample_size \\+ validation_sample_size \\- target_fwd"
  )

})

# Define your test
test_that("check_inputs_sb_backtest throws an error when rebalancing_months, training_sample_size, validation_sample_size, split_method are not numeric or not appropriate.", {

  load(paste(test_path(),"/testdata/","artificial_signal_blending_obj.RData", sep =""))

  expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        signal_universe_m_df = signal_universe_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        split_method = "expanding",
        validation_sample_size = 0,
        gsm_algorithm = "ols",
        rebalancing_months = "nine",
        sb_algorithm = "ols",
        target_fwd_name = "fwd_premium_3m"),
    "rebalancing_months should be numeric."
  )

  expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        signal_universe_m_df = signal_universe_m_df,
        target_m_df = target_m_df,
        training_sample_size = "four",
        rebalancing_months = 9,
        sb_algorithm = "ols",
        target_fwd_name = "fwd_premium_3m"),
    "training_sample_size should be numeric."
  )

  expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        signal_universe_m_df = signal_universe_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 2,
        rebalancing_months = 9,
        sb_algorithm = "ols",
        target_fwd_name = "fwd_premium_3m"),
    "ols and heuristic sb algorithms do not support validation split."
  )


  expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        signal_universe_m_df = signal_universe_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = "one",
        rebalancing_months = 9,
        tuning_method = "grid_search",
        hyper_grid_domain_list = list(alpha = c(1,0.5), lambda.min.ratio = c(0.1,0.2)),
        sb_algorithm = "glmnet",
        target_fwd_name = "fwd_premium_1m"),
    "validation_sample_size should be numeric."
  )

  expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        signal_universe_m_df = signal_universe_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        tuning_method = "grid_search",
        hyper_grid_domain_list = list(alpha = c(1,0.5), lambda.min.ratio = c(0.1,0.2)),
        sb_algorithm = "glmnet",
        split_method = "rolling",
        target_fwd_name = "fwd_premium_1m"),
    "split_method should be expanding."
  )

})

# Define your test
test_that("check_inputs_sb_backtest throws an error when eval_metric not correctly set.", {

  load(paste(test_path(),"/testdata/","artificial_signal_blending_obj.RData", sep =""))

  expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        signal_universe_m_df = signal_universe_m_df,
        validation_sample_size = 1,
        split_method = "expanding",
        gsm_algorithm = "ols",
        backtest_returns_m_xts = NULL,
        benchmark_returns_m_xts = NULL,
        signal_themes_m_df = NULL,
        concentration_constraint_policy = NULL,
        custom_signal_weights_m_df = NULL,
        rebalancing_months = 9,
        tuning_method = "grid_search",
        hyper_grid_domain_list = list(alpha = c(1,0.5), lambda.min.ratio = c(0.1,0.2)),
        sb_algorithm = "glmnet",
        chosen_eval_metric = "rsquared",
        target_fwd_name = "fwd_premium_1m"),
    "chosen_eval_metric choice not supported."
  )

  expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        signal_universe_m_df = signal_universe_m_df,
        validation_sample_size = 1,
        split_method = "expanding",
        gsm_algorithm = "ols",
        backtest_returns_m_xts = NULL,
        benchmark_returns_m_xts = NULL,
        signal_themes_m_df = NULL,
        concentration_constraint_policy = NULL,
        custom_signal_weights_m_df = NULL,
        rebalancing_months = 9,
        tuning_method = "grid_search",
        chosen_eval_metric = "rmse",
        quantile_tau = 0.5,
        hyper_grid_domain_list = list(alpha = c(1,0.5), lambda.min.ratio = c(0.1,0.2)),
        sb_algorithm = "glmnet",
        huber_delta = "one",
        target_fwd_name = "fwd_premium_1m"),
    "huber_delta should be numeric."
  )




  expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        split_method = "expanding",
        gsm_algorithm = "ols",
        backtest_returns_m_xts = NULL,
        benchmark_returns_m_xts = NULL,
        signal_themes_m_df = NULL,
        concentration_constraint_policy = NULL,
        custom_signal_weights_m_df = NULL,
        signal_universe_m_df = signal_universe_m_df,
        rebalancing_months = 9,
        tuning_method = "grid_search",
        hyper_grid_domain_list = list(alpha = c(1,0.5), lambda.min.ratio = c(0.1,0.2)),
        sb_algorithm = "glmnet",
        chosen_eval_metric = "rmse",
        quantile_tau = 0,
        target_fwd_name = "fwd_premium_1m"),
    "quantile_tau should be > 0 and less than 1."
  )


})

# Define your test
test_that("check_inputs_sb_backtest throws an error when keras network is not correctly set.", {

  load(paste(test_path(),"/testdata/","artificial_signal_blending_obj.RData", sep =""))

  #Keras Architecture Set as DF
  expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        signal_universe_m_df = signal_universe_m_df,
        rebalancing_months = 9,
        tuning_method = "grid_search",
        hyper_grid_domain_list = list(regularizer_l1 = c(1,0.5), regularizer_l2 = c(1,2), droprate = c(0.50), lr = 0.02, size_of_batch = 512, number_of_epochs = 100),
        sb_algorithm = "nn",
        keras_architecture_parameters = data.frame(units = 32,  n_layers = 1, activation = 'relu', nn_optimizer = 'Adam', batch_norm_option = TRUE),
        chosen_eval_metric = "rss",
        gsm_algorithm = "ols",
        split_method = "expanding",
        quantile_tau = 0.5,
        huber_delta = 1,
        custom_objective = "squared_error",
        n_iter =NULL,
        early_stop = 10,
        backtest_returns_m_xts = NULL,
        benchmark_returns_m_xts = NULL,
        signal_themes_m_df = NULL,
        concentration_constraint_policy = NULL,
        custom_signal_weights_m_df = NULL,
        target_fwd_name = "fwd_premium_1m"),
    "keras_architecture_parameters should be a list with units, n_layers, activation, nn_optimizer and batch_norm_option elements"
  )

  #Keras Architecture missing nn_optimizer
  expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        signal_universe_m_df = signal_universe_m_df,
        tuning_method = "grid_search",
        hyper_grid_domain_list = list(regularizer_l1 = c(1,0.5), regularizer_l2 = c(1,2), droprate = c(0.50), lr = 0.02, size_of_batch = 512, number_of_epochs = 100),
        sb_algorithm = "nn",
        keras_architecture_parameters = list(units = 32,  n_layers = 1, activation = 'relu', batch_norm_option = TRUE),
        chosen_eval_metric = "rss",
        gsm_algorithm = "ols",
        split_method = "expanding",
        quantile_tau = 0.5,
        huber_delta = 1,
        custom_objective = "squared_error",
        n_iter =NULL,
        early_stop = 10,
        backtest_returns_m_xts = NULL,
        benchmark_returns_m_xts = NULL,
        signal_themes_m_df = NULL,
        concentration_constraint_policy = NULL,
        custom_signal_weights_m_df = NULL,
        target_fwd_name = "fwd_premium_1m")
    ,"keras_architecture_parameters should be a list with units, n_layers, activation, nn_optimizer and batch_norm_option elements"
  )

  #Keras Architecture with wrong n_layersr
  expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        signal_universe_m_df = signal_universe_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        tuning_method = "grid_search",
        hyper_grid_domain_list = list(regularizer_l1 = c(1,0.5), regularizer_l2 = c(1,2), droprate = c(0.50), lr = 0.02, size_of_batch = 512, number_of_epochs = 100),
        sb_algorithm = "nn",
        keras_architecture_parameters = list(units = 32,  n_layers = 6, activation = 'relu', nn_optimizer = 'Adam', batch_norm_option = TRUE),
        chosen_eval_metric = "rss",
        gsm_algorithm = "ols",
        split_method = "expanding",
        quantile_tau = 0.5,
        huber_delta = 1,
        custom_objective = "squared_error",
        n_iter =NULL,
        early_stop = 10,
        backtest_returns_m_xts = NULL,
        benchmark_returns_m_xts = NULL,
        signal_themes_m_df = NULL,
        concentration_constraint_policy = NULL,
        custom_signal_weights_m_df = NULL,
        target_fwd_name = "fwd_premium_1m")
    ,
    "n_layers should be an integer between 1 and 5."
  )

  #Keras Architecture with wrong activation
  expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        signal_universe_m_df = signal_universe_m_df,
        validation_sample_size = 1,
        rebalancing_months = 9,
        tuning_method = "grid_search",
        hyper_grid_domain_list = list(regularizer_l1 = c(1,0.5), regularizer_l2 = c(1,2), droprate = c(0.50), lr = 0.02, size_of_batch = 512, number_of_epochs = 100),
        sb_algorithm = "nn",
        keras_architecture_parameters = list(units = 32,  n_layers = 5, activation = 'relus', nn_optimizer = 'Adam', batch_norm_option = TRUE),
        chosen_eval_metric = "rss",
        gsm_algorithm = "ols",
        split_method = "expanding",
        quantile_tau = 0.5,
        huber_delta = 1,
        custom_objective = "squared_error",
        n_iter =NULL,
        early_stop = 10,
        backtest_returns_m_xts = NULL,
        benchmark_returns_m_xts = NULL,
        signal_themes_m_df = NULL,
        concentration_constraint_policy = NULL,
        custom_signal_weights_m_df = NULL,
        target_fwd_name = "fwd_premium_1m"),
    "activation should be one of relu, sigmoid, softmax, softplus, tanh or leaky_relu."
  )

  #Keras with wrong optimizer
  expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        signal_universe_m_df = signal_universe_m_df,
        rebalancing_months = 9,
        tuning_method = "grid_search",
        hyper_grid_domain_list = list(regularizer_l1 = c(1,0.5), regularizer_l2 = c(1,2), droprate = c(0.50), lr = 0.02, size_of_batch = 512, number_of_epochs = 100),
        sb_algorithm = "nn",
        keras_architecture_parameters = list(units = c(32,16),  n_layers = 2, activation = c('relu','relu'), nn_optimizer = 'SGD', batch_norm_option = c(FALSE,FALSE)),
        chosen_eval_metric = "rss",
        gsm_algorithm = "ols",
        split_method = "expanding",
        quantile_tau = 0.5,
        huber_delta = 1,
        custom_objective = "squared_error",
        n_iter =NULL,
        early_stop = 10,
        backtest_returns_m_xts = NULL,
        benchmark_returns_m_xts = NULL,
        signal_themes_m_df = NULL,
        concentration_constraint_policy = NULL,
        custom_signal_weights_m_df = NULL,
        target_fwd_name = "fwd_premium_1m")
    ,
    "nn_optimizer should be Adam or RMSProp."
  )

  #Keras with wrong number of units
  expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        signal_universe_m_df = signal_universe_m_df,
        rebalancing_months = 9,
        tuning_method = "grid_search",
        hyper_grid_domain_list = list(regularizer_l1 = c(1,0.5), regularizer_l2 = c(1,2), droprate = c(0.50), lr = 0.02, size_of_batch = 512, number_of_epochs = 100),
        sb_algorithm = "nn",
        keras_architecture_parameters = list(units = c(32,16),  n_layers = 3, activation = c('relu', 'relu', 'softmax'), nn_optimizer = 'Adam',
                                             batch_norm_option = c(TRUE, FALSE, TRUE)),
        chosen_eval_metric = "rss",
        gsm_algorithm = "ols",
        split_method = "expanding",
        quantile_tau = 0.5,
        huber_delta = 1,
        custom_objective = "squared_error",
        n_iter =NULL,
        early_stop = 10,
        backtest_returns_m_xts = NULL,
        benchmark_returns_m_xts = NULL,
        signal_themes_m_df = NULL,
        concentration_constraint_policy = NULL,
        custom_signal_weights_m_df = NULL,
        target_fwd_name = "fwd_premium_1m"),
    "length of units, activation and batch_norm_option should match n_layers"
  )

})

# Define your test
test_that("check_inputs_sb_backtest throws no error when keras network is correctly set.", {

  load(paste(test_path(),"/testdata/","artificial_signal_blending_obj.RData", sep =""))


  #Keras 1-Layer Architecture
  expect_no_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        signal_universe_m_df = signal_universe_m_df,
        backtest_returns_m_xts = NULL,
        benchmark_returns_m_xts = NULL,
        signal_themes_m_df = NULL,
        concentration_constraint_policy = NULL,
        custom_signal_weights_m_df = NULL,
        gsm_algorithm = "ols",
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
        sb_algorithm = "nn",
        keras_architecture_parameters = list(units = 32,  n_layers = 1, activation = 'relu', nn_optimizer = 'Adam', batch_norm_option = TRUE),
        chosen_eval_metric = "rss",
        target_fwd_name = "fwd_premium_1m")
    )


  #Keras 2-Layers Architecture
  expect_no_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        signal_universe_m_df = signal_universe_m_df,
        backtest_returns_m_xts = NULL,
        benchmark_returns_m_xts = NULL,
        signal_themes_m_df = NULL,
        concentration_constraint_policy = NULL,
        custom_signal_weights_m_df = NULL,
        gsm_algorithm = "ols",
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
        sb_algorithm = "nn",
        keras_architecture_parameters = list(units = c(32,16),  n_layers = 2, activation = c('relu', 'relu'), nn_optimizer = 'Adam', batch_norm_option = c(TRUE, FALSE)),
        chosen_eval_metric = "rss",
        target_fwd_name = "fwd_premium_1m")
    )


  #Keras 3-Layers Architecture
  expect_no_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        signal_universe_m_df = signal_universe_m_df,
        backtest_returns_m_xts = NULL,
        benchmark_returns_m_xts = NULL,
        signal_themes_m_df = NULL,
        concentration_constraint_policy = NULL,
        custom_signal_weights_m_df = NULL,
        gsm_algorithm = "ols",
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
        sb_algorithm = "nn",
        keras_architecture_parameters = list(units = c(32,16,8),  n_layers = 3, activation = c('relu', 'relu', 'tanh'), nn_optimizer = 'Adam', batch_norm_option = c(TRUE, FALSE, FALSE)),
        chosen_eval_metric = "rss",
        target_fwd_name = "fwd_premium_1m")
  )

})

# Define your test
test_that("check_inputs_sb_backtest does not throw an error when hyperparameters_grid_list are correctly set.", {

  load(paste(test_path(),"/testdata/","artificial_signal_blending_obj.RData", sep =""))

  #GLMNET
    expect_no_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        signal_universe_m_df = signal_universe_m_df,
        backtest_returns_m_xts = NULL,
        benchmark_returns_m_xts = NULL,
        signal_themes_m_df = NULL,
        concentration_constraint_policy = NULL,
        custom_signal_weights_m_df = NULL,
        gsm_algorithm = "ols",
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        split_method = "expanding",
        quantile_tau = 0.5,
        huber_delta = 1,
        custom_objective = "squared_error",
        n_iter =NULL,
        early_stop = NULL,
        tuning_method = "grid_search",
        hyper_grid_domain_list = list(alpha = c(1,0.5), lambda.min.ratio = c(0.1,0.2)),
        sb_algorithm = "glmnet",
        chosen_eval_metric = "rss",
        target_fwd_name = "fwd_premium_1m")
    )

    #GLMNET
    expect_warning(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        signal_universe_m_df = signal_universe_m_df,
        backtest_returns_m_xts = NULL,
        benchmark_returns_m_xts = NULL,
        signal_themes_m_df = NULL,
        concentration_constraint_policy = NULL,
        custom_signal_weights_m_df = NULL,
        gsm_algorithm = "ols",
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        split_method = "expanding",
        quantile_tau = 0.5,
        n_iter = 5,
        huber_delta = 1,
        custom_objective = "squared_error",
        early_stop = NULL,
        tuning_method = "grid_search",
        hyper_grid_domain_list = list(alpha = c(1,0.5), lambda.min.ratio = c(0.1,0.2)),
        sb_algorithm = "glmnet",
        chosen_eval_metric = "rss",
        target_fwd_name = "fwd_premium_1m"),
      "When tuning_method is grid_search, hyperparameters are combined exhaustively. Ignoring any user set n_iter value"
    )



  #RF
    expect_no_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        signal_universe_m_df = signal_universe_m_df,
        backtest_returns_m_xts = NULL,
        benchmark_returns_m_xts = NULL,
        signal_themes_m_df = NULL,
        concentration_constraint_policy = NULL,
        custom_signal_weights_m_df = NULL,
        gsm_algorithm = "ols",
        split_method = "expanding",
        quantile_tau = 0.5,
        huber_delta = 0.5,
        n_iter = NULL,
        custom_objective = "squared_error",
        early_stop = NULL,
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        tuning_method = "grid_search",
        hyper_grid_domain_list = list(mtry = c(1,0.5), num.trees = c(200),  max.depth = c(2, 2), min.bucket = 5),
        sb_algorithm = "rf",
        chosen_eval_metric = "rss",
        target_fwd_name = "fwd_premium_1m")
    )



  #XGB
    expect_no_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        signal_universe_m_df = signal_universe_m_df,
        backtest_returns_m_xts = NULL,
        benchmark_returns_m_xts = NULL,
        signal_themes_m_df = NULL,
        concentration_constraint_policy = NULL,
        custom_signal_weights_m_df = NULL,
        split_method = "expanding",
        quantile_tau = 0.5,
        huber_delta = 0.5,
        n_iter = NULL,
        custom_objective = "squared_error",
        early_stop = NULL,
        gsm_algorithm = "ols",
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        tuning_method = "grid_search",
        hyper_grid_domain_list = list(min_child_weight = c(1,0.5), max_depth = c(2, 5), subsample = c(0.2), colsample_bytree = 0.5,
                                      eta = c(0.5), alpha = c(0), gamma = 0, nrounds = 100),
        sb_algorithm = "xgb",
        chosen_eval_metric = "rss",
        verbose = FALSE,
        target_fwd_name = "fwd_premium_1m")
  )


  #NN

    expect_no_error(
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
        split_method = "expanding",
        quantile_tau = 0.5,
        huber_delta = 0.5,
        n_iter = NULL,
        custom_objective = "squared_error",
        early_stop = NULL,
        tuning_method = "grid_search",
        parallel = FALSE,
        hyper_grid_domain_list = list(regularizer_l1 = c(1,0.5), regularizer_l2 = c(2, 5), droprate = c(0.2), lr = 0.5,
                                      size_of_batch = c(512), number_of_epochs = c(100)),
        sb_algorithm = "nn",
        keras_architecture_parameters = list(units = 32, n_layers = 1, activation = 'relu', nn_optimizer = 'Adam', batch_norm_option = TRUE),
        chosen_eval_metric = "rss",
        verbose = FALSE,
        target_fwd_name = "fwd_premium_1m")
    )



})

# Define your test
test_that("check_inputs_sb_backtest throws an error when hyperparameters_grid_list is not correctly set.", {

  load(paste(test_path(),"/testdata/","artificial_signal_blending_obj.RData", sep =""))

  #OLS
  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = NULL,
      benchmark_returns_m_xts = NULL,
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      custom_signal_weights_m_df = NULL,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      validation_sample_size = 0,
      rebalancing_months = 9,
      split_method = "expanding",
      quantile_tau = 0.5,
      huber_delta = 0.5,
      n_iter = NULL,
      custom_objective = "squared_error",
      early_stop = NULL,
      tuning_method = "grid_search",
      hyper_grid_domain_list = list(alpha = c(1,1.1), lambda.min.ratio = c(0.1,0.2)),
      sb_algorithm = "ols",
      chosen_eval_metric = "rss",
      target_fwd_name = "fwd_premium_1m"
    ), "ols and heuristic sb algorithms do not support hyperparameters.")

  #OLS
  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = NULL,
      benchmark_returns_m_xts = NULL,
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      custom_signal_weights_m_df = NULL,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      validation_sample_size = 2,
      rebalancing_months = 9,
      split_method = "expanding",
      quantile_tau = 0.5,
      huber_delta = 0.5,
      n_iter = NULL,
      custom_objective = "squared_error",
      early_stop = NULL,
      tuning_method = "grid_search",
      hyper_grid_domain_list = NULL,
      sb_algorithm = "glmnet",
      chosen_eval_metric = "rss",
      target_fwd_name = "fwd_premium_1m"
    ), "hyper_grid_domain must be set when sb_algorithm is different from ols.")


  #GLMNET
    expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        signal_universe_m_df = signal_universe_m_df,
        backtest_returns_m_xts = NULL,
        benchmark_returns_m_xts = NULL,
        signal_themes_m_df = NULL,
        concentration_constraint_policy = NULL,
        custom_signal_weights_m_df = NULL,
        gsm_algorithm = "ols",
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        split_method = "expanding",
        quantile_tau = 0.5,
        huber_delta = 0.5,
        n_iter = NULL,
        custom_objective = "squared_error",
        early_stop = NULL,
        tuning_method = "grid_search",
        hyper_grid_domain_list = list(alpha = c(1,1.1), lambda.min.ratio = c(0.1,0.2)),
        sb_algorithm = "glmnet",
        chosen_eval_metric = "rss",
        target_fwd_name = "fwd_premium_1m"
    ), "alpha should be set in interval \\[0,1\\]")

    #GLMNET
    expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        signal_universe_m_df = signal_universe_m_df,
        backtest_returns_m_xts = NULL,
        benchmark_returns_m_xts = NULL,
        signal_themes_m_df = NULL,
        concentration_constraint_policy = NULL,
        custom_signal_weights_m_df = NULL,
        gsm_algorithm = "ols",
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        split_method = "expanding",
        quantile_tau = 0.5,
        huber_delta = 0.5,
        n_iter = NULL,
        custom_objective = "squared_error",
        early_stop = NULL,
        tuning_method = "grid_search",
        hyper_grid_domain_list = list(alpha = c(1,1.1), lambda_min_ratio = c(0.1,0.2)),
        sb_algorithm = "glmnet",
        chosen_eval_metric = "rss",
        target_fwd_name = "fwd_premium_1m"
      ), "hyperparameters do not match sb_algorithm choice")


    expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        signal_universe_m_df = signal_universe_m_df,
        backtest_returns_m_xts = NULL,
        benchmark_returns_m_xts = NULL,
        signal_themes_m_df = NULL,
        concentration_constraint_policy = NULL,
        custom_signal_weights_m_df = NULL,
        gsm_algorithm = "ols",
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        split_method = "expanding",
        quantile_tau = 0.5,
        huber_delta = 0.5,
        n_iter = NULL,
        custom_objective = "squared_error",
        early_stop = NULL,
        tuning_method = "grid_search",
        hyper_grid_domain_list = list(alpha = c(1,0.9), lambda.min.ratio = c(0.1,1)),
        sb_algorithm = "glmnet",
        chosen_eval_metric = "rss",
        target_fwd_name = "fwd_premium_1m")
    , "lambda.min.ratio should be set in interval \\[0,1\\)"
  )


  #RF
    expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        signal_universe_m_df = signal_universe_m_df,
        backtest_returns_m_xts = NULL,
        benchmark_returns_m_xts = NULL,
        signal_themes_m_df = NULL,
        concentration_constraint_policy = NULL,
        custom_signal_weights_m_df = NULL,
        gsm_algorithm = "ols",
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        split_method = "expanding",
        quantile_tau = 0.5,
        huber_delta = 0.5,
        custom_objective = "squared_error",
        early_stop = NULL,
        tuning_method = "random_search",
        hyper_grid_domain_list = list(mtry = list(distribution_choice = "uniform",
                                                  pars = c(min = 0.5, max = 1)),
                                      num.trees = list(distribution_choice = "uniform",
                                                       pars = c(min = 2, max = 3)),
                                      max.depth = list(distribution_choice = "uniform",
                                                       pars = c(min = 1L, max = 2L)),
                                      min.bucket = list(distribution_choice = "uniform",
                                                        pars = c(min = 1, max = 3))),
        sb_algorithm = "rf",
        n_iter = 2,
        chosen_eval_metric = "rss",
        target_fwd_name = "fwd_premium_1m")
    , "num.trees should be integer"
  )

    expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        signal_universe_m_df = signal_universe_m_df,
        backtest_returns_m_xts = NULL,
        benchmark_returns_m_xts = NULL,
        signal_themes_m_df = NULL,
        concentration_constraint_policy = NULL,
        custom_signal_weights_m_df = NULL,
        gsm_algorithm = "ols",
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        split_method = "expanding",
        quantile_tau = 0.5,
        huber_delta = 0.5,
        custom_objective = "squared_error",
        early_stop = NULL,
        tuning_method = "grid_search",
        hyper_grid_domain_list = list(mtry = c(0.5, 0.7),
                                      num.trees = c(2.5, 3),
                                      max.depth = c(1L, 2L),
                                      min.bucket = c(1,3)),
        sb_algorithm = "rf",
        n_iter = NULL,
        chosen_eval_metric = "rss",
        target_fwd_name = "fwd_premium_1m")
      , "num.trees should have no decimals"
    )


    expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        signal_universe_m_df = signal_universe_m_df,
        backtest_returns_m_xts = NULL,
        benchmark_returns_m_xts = NULL,
        signal_themes_m_df = NULL,
        concentration_constraint_policy = NULL,
        custom_signal_weights_m_df = NULL,
        gsm_algorithm = "ols",
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        split_method = "expanding",
        quantile_tau = 0.5,
        huber_delta = 0.5,
        custom_objective = "squared_error",
        early_stop = NULL,
        tuning_method = "grid_search",
        hyper_grid_domain_list = list(mtry = c(0.5, 0.7),
                                      num.trees = c(-2, 3),
                                      max.depth = c(1L, 2L),
                                      min.bucket = c(1,3)),
        sb_algorithm = "rf",
        n_iter = NULL,
        chosen_eval_metric = "rss",
        target_fwd_name = "fwd_premium_1m")
      , "num.trees should be positive"
    )

    expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        signal_universe_m_df = signal_universe_m_df,
        backtest_returns_m_xts = NULL,
        benchmark_returns_m_xts = NULL,
        signal_themes_m_df = NULL,
        concentration_constraint_policy = NULL,
        custom_signal_weights_m_df = NULL,
        gsm_algorithm = "ols",
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        split_method = "expanding",
        quantile_tau = 0.5,
        huber_delta = 0.5,
        custom_objective = "squared_error",
        early_stop = NULL,
        tuning_method = "grid_search",
        hyper_grid_domain_list = list(mtry = c(0.5, 7),
                                      num.trees = c(2, 3),
                                      max.depth = c(1L, 2L),
                                      min.bucket = c(1,3)),
        sb_algorithm = "rf",
        n_iter = NULL,
        chosen_eval_metric = "rss",
        target_fwd_name = "fwd_premium_1m")
      , "mtry should be set in interval \\[0,1\\]"
    )

    expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        signal_universe_m_df = signal_universe_m_df,
        backtest_returns_m_xts = NULL,
        benchmark_returns_m_xts = NULL,
        signal_themes_m_df = NULL,
        concentration_constraint_policy = NULL,
        custom_signal_weights_m_df = NULL,
        gsm_algorithm = "ols",
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        split_method = "expanding",
        quantile_tau = 0.5,
        huber_delta = 0.5,
        custom_objective = "squared_error",
        early_stop = NULL,
        tuning_method = "grid_search",
        hyper_grid_domain_list = list(mtry = c(0.5, 0.7),
                                      num.trees = c(2, 3),
                                      max.depth = c(1.5, 2L),
                                      min.bucket = c(1,3)),
        sb_algorithm = "rf",
        n_iter = NULL,
        chosen_eval_metric = "rss",
        target_fwd_name = "fwd_premium_1m")
      , "max.depth should have no decimals"
    )

    expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        signal_universe_m_df = signal_universe_m_df,
        backtest_returns_m_xts = NULL,
        benchmark_returns_m_xts = NULL,
        signal_themes_m_df = NULL,
        concentration_constraint_policy = NULL,
        custom_signal_weights_m_df = NULL,
        gsm_algorithm = "ols",
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        split_method = "expanding",
        quantile_tau = 0.5,
        huber_delta = 0.5,
        custom_objective = "squared_error",
        early_stop = NULL,
        tuning_method = "grid_search",
        hyper_grid_domain_list = list(mtry = c(0.5, 0.7),
                                      num.trees = c(2, 3),
                                      max.depth = c(-1, 2),
                                      min.bucket = c(1,3)),
        sb_algorithm = "rf",
        n_iter = NULL,
        chosen_eval_metric = "rss",
        target_fwd_name = "fwd_premium_1m")
      , "max.depth should be positive"
    )





    expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        signal_universe_m_df = signal_universe_m_df,
        backtest_returns_m_xts = NULL,
        benchmark_returns_m_xts = NULL,
        signal_themes_m_df = NULL,
        concentration_constraint_policy = NULL,
        custom_signal_weights_m_df = NULL,
        gsm_algorithm = "ols",
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        split_method = "expanding",
        quantile_tau = 0.5,
        huber_delta = 0.5,
        custom_objective = "squared_error",
        early_stop = NULL,
        tuning_method = "random_search",
        hyper_grid_domain_list = list(mtry = list(distribution_choice = "uniform",
                                                  pars = c(min = 0.5, max = 1)),
                                      num.trees = list(distribution_choice = "uniform",
                                                       pars = c(min = 2L, max = 5L)),
                                      max.depth = list(distribution_choice = "uniform",
                                                       pars = c(min = 1L, max = 2L)),
                                      min.bucket = list(distribution_choice = "uniform",
                                                        pars = c(min = 1, max = 3))),
        sb_algorithm = "rf",
        n_iter = "2",
        chosen_eval_metric = "rss",
        target_fwd_name = "fwd_premium_1m")
      , "n_iter must be numeric."
    )

    expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        signal_universe_m_df = signal_universe_m_df,
        backtest_returns_m_xts = NULL,
        benchmark_returns_m_xts = NULL,
        signal_themes_m_df = NULL,
        concentration_constraint_policy = NULL,
        custom_signal_weights_m_df = NULL,
        gsm_algorithm = "ols",
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        split_method = "expanding",
        quantile_tau = 0.5,
        huber_delta = 0.5,
        custom_objective = "squared_error",
        early_stop = NULL,
        tuning_method = "random_search",
        hyper_grid_domain_list = list(mtry = list(distribution_choice = "uniform",
                                                  pars = c(min = 0.5, max = 1)),
                                      num_trees = list(distribution_choice = "uniform",
                                                       pars = c(min = 1L, max = 3L)),
                                      max.depth = list(distribution_choice = "uniform",
                                                       pars = c(min = 1L, max = 2L)),
                                      min.bucket = list(distribution_choice = "uniform",
                                                        pars = c(min = 1, max = 3))),
        sb_algorithm = "rf",
        n_iter = 2,
        chosen_eval_metric = "rss",
        target_fwd_name = "fwd_premium_1m")
      , "hyperparameters do not match sb_algorithm choice"
    )


  #XGB
    expect_error(
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
        hyper_grid_domain_list = list(min_child_weight = c(1,0.5), max_depth = c(2, 5), subsample = c(0.2, 0.5),
                                      colsample_bytree = c(0.5,1),
                                      eta = c(0.5,1), alpha = c(0,2), gamma = c(0,1), nrounds = c(100,200)),
        sb_algorithm = "xgb",
        chosen_eval_metric = "rss",
        verbose = FALSE,
        split_method = "expanding",
        quantile_tau = 0.5,
        huber_delta = 0.5,
        custom_objective = "squared_error",
        early_stop = NULL,
        n_iter = 2,
        init_points = 10,
        k_iter = 1,
        acq = "ei",
        target_fwd_name = "fwd_premium_1m"
    ), "max_depth should be integer")

    expect_error(
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
        hyper_grid_domain_list = list(min_child_weight = c(1,0.5), max_depth = c(-2L, 5L), subsample = c(0.2, 0.5),
                                      colsample_bytree = c(0.5,1),
                                      eta = c(0.5,1), alpha = c(0,2), gamma = c(0,1), nrounds = c(100,200)),
        sb_algorithm = "xgb",
        chosen_eval_metric = "rss",
        verbose = FALSE,
        split_method = "expanding",
        quantile_tau = 0.5,
        huber_delta = 0.5,
        custom_objective = "squared_error",
        early_stop = NULL,
        n_iter = 2,
        init_points = 10,
        k_iter = 1,
        acq = "ei",
        target_fwd_name = "fwd_premium_1m"
      ), "max_depth should be positive")

    expect_error(
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
        hyper_grid_domain_list = list(min_child_weight = c(1,0.5), max_depth = c(2L, 5L), subsample = c(0.2, 0.5),
                                      colsample_bytree = c(0.5,2),
                                      eta = c(0.5,1), alpha = c(0,2), gamma = c(0,1), nrounds = c(100,200)),
        sb_algorithm = "xgb",
        chosen_eval_metric = "rss",
        verbose = FALSE,
        split_method = "expanding",
        quantile_tau = 0.5,
        huber_delta = 0.5,
        custom_objective = "squared_error",
        early_stop = NULL,
        n_iter = 2,
        init_points = 10,
        k_iter = 1,
        acq = "ei",
        target_fwd_name = "fwd_premium_1m"
      ), "colsample_bytree should be set in interval \\[0,1\\]")

    expect_error(
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
        hyper_grid_domain_list = list(min_child_weight = c(1,0.5), max_depth = c(2L, 5L), subsample = c(0.2, 5),
                                      colsample_bytree = c(0.5,0.9),
                                      eta = c(0.5,1), alpha = c(0,2), gamma = c(0,1), nrounds = c(100,200)),
        sb_algorithm = "xgb",
        chosen_eval_metric = "rss",
        verbose = FALSE,
        split_method = "expanding",
        quantile_tau = 0.5,
        huber_delta = 0.5,
        custom_objective = "squared_error",
        early_stop = NULL,
        n_iter = 2,
        init_points = 10,
        k_iter = 1,
        acq = "ei",
        target_fwd_name = "fwd_premium_1m"
      ), "subsample should be set in interval \\[0,1\\]")



    expect_error(
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
        hyper_grid_domain_list = list(min_child_weight = c(1,0.5), max.depth = c(2L, 5L), subsample = c(0.2, 0.5),
                                      colsample_bytree = c(0.5,1),
                                      eta = c(0.5,1), alpha = c(0,2), gamma = c(0,1), nrounds = c(100,200)),
        sb_algorithm = "xgb",
        chosen_eval_metric = "rss",
        verbose = FALSE,
        split_method = "expanding",
        quantile_tau = 0.5,
        huber_delta = 0.5,
        custom_objective = "squared_error",
        early_stop = NULL,
        n_iter = 2,
        init_points = 10,
        k_iter = 1,
        acq = "ei",
        target_fwd_name = "fwd_premium_1m"
      ), "hyperparameters do not match sb_algorithm choice")

    expect_error(
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
        hyper_grid_domain_list = list(min_child_weight = c(1,0.5), max_depth = c(2L, 5L), subsample = c(0.2, 0.5),
                                      colsample_bytree = c(0.5,1),
                                      eta = c(0.5,1), alpha = c(0,2), gamma = c(0,1), nrounds = c(100,200)),
        sb_algorithm = "xgb",
        chosen_eval_metric = "rss",
        verbose = FALSE,
        split_method = "expanding",
        quantile_tau = 0.5,
        huber_delta = 0.5,
        custom_objective = "squared_error",
        early_stop = NULL,
        n_iter = 2,
        init_points = 10,
        k_iter = 1,
        acq = "eib",
        target_fwd_name = "fwd_premium_1m"
      ), "acq should be one of ucb, ei or poi")

    expect_error(
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
        hyper_grid_domain_list = list(min_child_weight = c(1,0.5), max_depth = c(2L, 5L), subsample = c(0.2, 0.5),
                                      colsample_bytree = c(0.5,1),
                                      eta = c(0.5,1), alpha = c(0,2), gamma = c(0,1), nrounds = c(100,200)),
        sb_algorithm = "xgb",
        chosen_eval_metric = "rss",
        verbose = FALSE,
        split_method = "expanding",
        quantile_tau = 0.5,
        huber_delta = 0.5,
        custom_objective = "squared_error",
        early_stop = NULL,
        n_iter = 2,
        init_points = 1,
        k_iter = 1,
        acq = "ei",
        target_fwd_name = "fwd_premium_1m"
      ), "init_points must be greater than number of hyperparameters")

    expect_error(
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
        hyper_grid_domain_list = list(min_child_weight = c(1,0.5), max_depth = c(2L, 5L), subsample = c(0.2, 0.5),
                                      colsample_bytree = c(0.5,1),
                                      eta = c(0.5,1), alpha = c(0,2), gamma = c(0,1), nrounds = c(100,200)),
        sb_algorithm = "xgb",
        chosen_eval_metric = "rss",
        verbose = FALSE,
        split_method = "expanding",
        quantile_tau = 0.5,
        huber_delta = 0.5,
        custom_objective = "squared_error",
        early_stop = NULL,
        n_iter = 2,
        init_points = 10,
        k_iter = 5,
        acq = "ei",
        target_fwd_name = "fwd_premium_1m"
      ), "n_iter must be greater than k_iter")


  #NN
    expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        signal_universe_m_df = signal_universe_m_df,
        backtest_returns_m_xts = NULL,
        benchmark_returns_m_xts = NULL,
        signal_themes_m_df = NULL,
        concentration_constraint_policy = NULL,
        custom_signal_weights_m_df = NULL,
        gsm_algorithm = "ols",
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
        sb_algorithm = "nn",
        keras_architecture_parameters = list(units = 32, n_layers = 1, activation = 'relu', nn_optimizer = 'Adam', batch_norm_option = TRUE),
        chosen_eval_metric = "rss",
        verbose = FALSE,
        n_iter = 2,
        parallel = FALSE,
        quantile_tau = 0.5,
        huber_delta = 1,
        custom_objective = "squared_error",
        early_stop = NULL,
        split_method = "expanding",
        target_fwd_name = "fwd_premium_1m"
    ), "droprate should be set in interval \\[0,1\\)")

    expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        signal_universe_m_df = signal_universe_m_df,
        backtest_returns_m_xts = NULL,
        benchmark_returns_m_xts = NULL,
        signal_themes_m_df = NULL,
        concentration_constraint_policy = NULL,
        custom_signal_weights_m_df = NULL,
        gsm_algorithm = "ols",
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        tuning_method = "random_search",
        hyper_grid_domain_list = list(regularizer_l1 = list(distribution_choice = "constant",
                                                            value = c(0, 1)),
                                      regularizer_l2 = list(distribution_choice = "uniform",
                                                            pars = c(min = 2, max = 5)),
                                      droprate = list(distribution_choice = "uniform",
                                                      pars = c(min = 0.2, max = 0.5)),
                                      lr = list(distribution_choice = "constant",
                                                value = c(0, 1)),
                                      size_of_batch = list(distribution_choice = "constant",
                                                           value = c(2, 1)),
                                      number_of_epochs = list(distribution_choice = "constant",
                                                              value = c(1L, 2L))),
        sb_algorithm = "nn",
        keras_architecture_parameters = list(units = 32, n_layers = 1, activation = 'relu', nn_optimizer = 'Adam', batch_norm_option = TRUE),
        chosen_eval_metric = "rss",
        verbose = FALSE,
        n_iter = 2,
        parallel = FALSE,
        quantile_tau = 0.5,
        huber_delta = 1,
        custom_objective = "squared_error",
        early_stop = NULL,
        split_method = "expanding",
        target_fwd_name = "fwd_premium_1m"
      ), "size_of_batch should be integer")

    expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        signal_universe_m_df = signal_universe_m_df,
        backtest_returns_m_xts = NULL,
        benchmark_returns_m_xts = NULL,
        signal_themes_m_df = NULL,
        concentration_constraint_policy = NULL,
        custom_signal_weights_m_df = NULL,
        gsm_algorithm = "ols",
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        tuning_method = "random_search",
        hyper_grid_domain_list = list(regularizer_l1 = list(distribution_choice = "constant",
                                                            value = c(0, 1)),
                                      regularizer_l2 = list(distribution_choice = "uniform",
                                                            pars = c(min = 2, max = 5)),
                                      droprate = list(distribution_choice = "uniform",
                                                      pars = c(min = 0.2, max = 0.5)),
                                      lr = list(distribution_choice = "constant",
                                                value = c(0, 1)),
                                      size_of_batch = list(distribution_choice = "constant",
                                                           value = c(-2L, 1L)),
                                      number_of_epochs = list(distribution_choice = "constant",
                                                              value = c(1L, 2L))),
        sb_algorithm = "nn",
        keras_architecture_parameters = list(units = 32, n_layers = 1, activation = 'relu', nn_optimizer = 'Adam', batch_norm_option = TRUE),
        chosen_eval_metric = "rss",
        verbose = FALSE,
        n_iter = 2,
        parallel = FALSE,
        quantile_tau = 0.5,
        huber_delta = 1,
        custom_objective = "squared_error",
        early_stop = NULL,
        split_method = "expanding",
        target_fwd_name = "fwd_premium_1m"
      ), "size_of_batch should be positive")

    expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        signal_universe_m_df = signal_universe_m_df,
        backtest_returns_m_xts = NULL,
        benchmark_returns_m_xts = NULL,
        signal_themes_m_df = NULL,
        concentration_constraint_policy = NULL,
        custom_signal_weights_m_df = NULL,
        gsm_algorithm = "ols",
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        tuning_method = "random_search",
        hyper_grid_domain_list = list(regularizer_l1 = list(distribution_choice = "constant",
                                                            value = c(0, 1)),
                                      regularizer_l2 = list(distribution_choice = "uniform",
                                                           pars = c(min = 2, max = 5)),
                                      droprate = list(distribution_choice = "uniform",
                                                      pars = c(min = 0, max = 0.9)),
                                      lr = list(distribution_choice = "constant",
                                                value = c(0, 1)),
                                      size_of_batch = list(distribution_choice = "constant",
                                                           value = c(0, 1.5)),
                                      number_of_epochs = list(distribution_choice = "constant",
                                                              value = c(0, 1))),
        sb_algorithm = "nn",
        keras_architecture_parameters = list(units = 32, n_layers = 1, activation = 'relu', nn_optimizer = 'Adam', batch_norm_option = TRUE),
        chosen_eval_metric = "rss",
        verbose = FALSE,
        n_iter = 2,
        parallel = FALSE,
        quantile_tau = 0.5,
        huber_delta = 1,
        custom_objective = "squared_error",
        early_stop = NULL,
        split_method = "expanding",
        target_fwd_name = "fwd_premium_1m"
      ), "number_of_epochs should be integer")


    expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        signal_universe_m_df = signal_universe_m_df,
        backtest_returns_m_xts = NULL,
        benchmark_returns_m_xts = NULL,
        signal_themes_m_df = NULL,
        concentration_constraint_policy = NULL,
        custom_signal_weights_m_df = NULL,
        gsm_algorithm = "ols",
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        tuning_method = "random_search",
        hyper_grid_domain_list = list(regularizer_l1 = list(distribution_choice = "constant",
                                                            value = c(0, 1)),
                                      regularizer_l2 = list(distribution_choice = "uniform",
                                                            pars = c(min = 2, max = 5)),
                                      droprate = list(distribution_choice = "uniform",
                                                      pars = c(min = 0, max = 0.9)),
                                      lr = list(distribution_choice = "constant",
                                                value = c(0, 1)),
                                      size_of_batch = list(distribution_choice = "constant",
                                                           value = c(0, 1.5)),
                                      number_of_epochs = list(distribution_choice = "constant",
                                                              value = c(-1L, 1L))),
        sb_algorithm = "nn",
        keras_architecture_parameters = list(units = 32, n_layers = 1, activation = 'relu', nn_optimizer = 'Adam', batch_norm_option = TRUE),
        chosen_eval_metric = "rss",
        verbose = FALSE,
        n_iter = 2,
        parallel = FALSE,
        quantile_tau = 0.5,
        huber_delta = 1,
        custom_objective = "squared_error",
        early_stop = NULL,
        split_method = "expanding",
        target_fwd_name = "fwd_premium_1m"
      ), "number_of_epochs should be positive")


    expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        signal_universe_m_df = signal_universe_m_df,
        backtest_returns_m_xts = NULL,
        benchmark_returns_m_xts = NULL,
        signal_themes_m_df = NULL,
        concentration_constraint_policy = NULL,
        custom_signal_weights_m_df = NULL,
        gsm_algorithm = "ols",
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        tuning_method = "random_search",
        hyper_grid_domain_list = list(regularizer_l1 = list(distribution_choice = "constant",
                                                            value = c(0, 1)),
                                      regularizerl2 = list(distribution_choice = "uniform",
                                                            pars = c(min = 2, max = 5)),
                                      droprate = list(distribution_choice = "uniform",
                                                      pars = c(min = 0, max = 0.9)),
                                      lr = list(distribution_choice = "constant",
                                                value = c(0, 1)),
                                      size_of_batch = list(distribution_choice = "constant",
                                                           value = c(0, 1)),
                                      number_of_epochs = list(distribution_choice = "constant",
                                                              value = c(0, 1))),
        sb_algorithm = "nn",
        keras_architecture_parameters = list(units = 32, n_layers = 1, activation = 'relu', nn_optimizer = 'Adam', batch_norm_option = TRUE),
        chosen_eval_metric = "rss",
        verbose = FALSE,
        n_iter = 2,
        parallel = FALSE,
        quantile_tau = 0.5,
        huber_delta = 1,
        custom_objective = "squared_error",
        early_stop = NULL,
        split_method = "expanding",
        target_fwd_name = "fwd_premium_1m"
      ), "hyperparameters do not match sb_algorithm choice")



})

# Define your test
test_that("check_inputs_sb_backtest throws an error when grid_search not correctly set.", {


  load(paste(test_path(),"/testdata/","artificial_signal_blending_obj.RData", sep =""))

  expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        signal_universe_m_df = signal_universe_m_df,
        backtest_returns_m_xts = NULL,
        benchmark_returns_m_xts = NULL,
        signal_themes_m_df = NULL,
        concentration_constraint_policy = NULL,
        custom_signal_weights_m_df = NULL,
        gsm_algorithm = "ols",
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        tuning_method = "grd_search",
        hyper_grid_domain_list = list(alpha = c(1,0.5), lambda.min.ratio = c(0.1,0.2)),
        sb_algorithm = "glmnet",
        chosen_eval_metric = "rss",
        split_method = "expanding",
        quantile_tau = 0.5,
        huber_delta = 1,
        target_fwd_name = "fwd_premium_1m")
    ,
    "tuning_method should be one of random_search, grid_search or bayesian_opt."
  )

  expect_error(
    suppressMessages(suppressWarnings({
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        signal_universe_m_df = signal_universe_m_df,
        backtest_returns_m_xts = NULL,
        benchmark_returns_m_xts = NULL,
        signal_themes_m_df = NULL,
        concentration_constraint_policy = NULL,
        custom_signal_weights_m_df = NULL,
        gsm_algorithm = "ols",
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        tuning_method = "grid_search",
        hyper_grid_domain_list = data.frame(alpha=c(1,0.5), lambda.min.ratio = c(0.1,0.2)),
        sb_algorithm = "glmnet",
        chosen_eval_metric = "rss",
        split_method = "expanding",
        quantile_tau = 0.5,
        huber_delta = 1,
        target_fwd_name = "fwd_premium_1m")
    })),
    "hyper_grid_domain_list not in correct format for grid_search tuning."
  )

})

# Define your test
test_that("check_inputs_sb_backtest throws an error when random_search not correctly set.", {


  load(paste(test_path(),"/testdata/","artificial_signal_blending_obj.RData", sep =""))

  expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        signal_universe_m_df = signal_universe_m_df,
        backtest_returns_m_xts = NULL,
        benchmark_returns_m_xts = NULL,
        signal_themes_m_df = NULL,
        concentration_constraint_policy = NULL,
        custom_signal_weights_m_df = NULL,
        gsm_algorithm = "ols",
        rebalancing_months = 9,
        tuning_method = "rand_search",
        split_method = "expanding",
        quantile_tau = 0.5,
        huber_delta = 1,
        hyper_grid_domain_list = list(alpha = c(1,0.5), lambda.min.ratio = c(0.1,0.2)),
        sb_algorithm = "glmnet",
        chosen_eval_metric = "rss",
        target_fwd_name = "fwd_premium_1m")    ,
    "tuning_method should be one of random_search, grid_search or bayesian_opt."
  )

  expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        signal_universe_m_df = signal_universe_m_df,
        backtest_returns_m_xts = NULL,
        benchmark_returns_m_xts = NULL,
        signal_themes_m_df = NULL,
        concentration_constraint_policy = NULL,
        custom_signal_weights_m_df = NULL,
        gsm_algorithm = "ols",
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        tuning_method = "random_search",
        hyper_grid_domain_list = data.frame(alpha=c(1,0.5), lambda.min.ratio = c(0.1,0.2)),
        sb_algorithm = "glmnet",
        chosen_eval_metric = "rss",
        split_method = "expanding",
        n_iter = 3,
        quantile_tau = 0.5,
        huber_delta = 1,
        target_fwd_name = "fwd_premium_1m"),
    "hyper_grid_domain_list not in correct format for random_search tuning."
  )

  expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        signal_universe_m_df = signal_universe_m_df,
        backtest_returns_m_xts = NULL,
        benchmark_returns_m_xts = NULL,
        signal_themes_m_df = NULL,
        concentration_constraint_policy = NULL,
        custom_signal_weights_m_df = NULL,
        gsm_algorithm = "ols",
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        tuning_method = "random_search",
        split_method = "expanding",
        quantile_tau = 0.5,
        huber_delta = 1,
        n_iter = 3,
        hyper_grid_domain_list = list(alpha=c(1,0.5), lambda.min.ratio = c(0.1,0.2)),
        sb_algorithm = "glmnet",
        chosen_eval_metric = "rss",
        target_fwd_name = "fwd_premium_1m"
    ),
    "hyper_grid_domain_list not in correct format for random_search tuning."
  )

  suppressWarnings(
  expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        signal_universe_m_df = signal_universe_m_df,
        backtest_returns_m_xts = NULL,
        benchmark_returns_m_xts = NULL,
        signal_themes_m_df = NULL,
        concentration_constraint_policy = NULL,
        custom_signal_weights_m_df = NULL,
        gsm_algorithm = "ols",
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        tuning_method = "random_search",
        hyper_grid_domain_list = list(alpha = data.frame(distribution_choice = "uniform", pars = c(min = 0,max = 1)),
                                      lambda.min.ratio = list(distribution_choice = "uniform", pars = c(min = 0.1, max = 0.2))),
        sb_algorithm = "glmnet",
        split_method = "expanding",
        quantile_tau = 0.5,
        huber_delta = 1,
        n_iter = 3,
        chosen_eval_metric = "rss",
        target_fwd_name = "fwd_premium_1m")
    ,
    "hyper_grid_domain_list not in correct format for random_search tuning."
  )
  )


  expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        signal_universe_m_df = signal_universe_m_df,
        backtest_returns_m_xts = NULL,
        benchmark_returns_m_xts = NULL,
        signal_themes_m_df = NULL,
        concentration_constraint_policy = NULL,
        custom_signal_weights_m_df = NULL,
        gsm_algorithm = "ols",
        rebalancing_months = 9,
        tuning_method = "random_search",
        hyper_grid_domain_list = list(alpha = list(distribution = "uniform", pars = c(min = 0,max = 1)),
                                      lambda.min.ratio = list(distribution_choice = "uniform", pars = c(min = 0.1, max = 0.9))),
        sb_algorithm = "glmnet",
        chosen_eval_metric = "rss",
        split_method = "expanding",
        quantile_tau = 0.5,
        huber_delta = 1,
        n_iter = 3,
        target_fwd_name = "fwd_premium_1m"),
    "hyper_grid_domain_list not in correct format for random_search tuning."
  )

  expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        signal_universe_m_df = signal_universe_m_df,
        backtest_returns_m_xts = NULL,
        benchmark_returns_m_xts = NULL,
        signal_themes_m_df = NULL,
        concentration_constraint_policy = NULL,
        custom_signal_weights_m_df = NULL,
        gsm_algorithm = "ols",
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        tuning_method = "random_search",
        hyper_grid_domain_list = list(alpha = list(distribution_choice = "uniform", pars = c(a = 0,b = 1)),
                                      lambda.min.ratio = list(distribution_choice = "uniform", pars = c(min = 0.1, max = 0.5))),
        sb_algorithm = "glmnet",
        chosen_eval_metric = "rss",
        split_method = "expanding",
        quantile_tau = 0.5,
        huber_delta = 1,
        n_iter = 3,
        target_fwd_name = "fwd_premium_1m"),
    "hyper_grid_domain_list not in correct format for random_search tuning."
  )

  expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        signal_universe_m_df = signal_universe_m_df,
        backtest_returns_m_xts = NULL,
        benchmark_returns_m_xts = NULL,
        signal_themes_m_df = NULL,
        concentration_constraint_policy = NULL,
        custom_signal_weights_m_df = NULL,
        gsm_algorithm = "ols",
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        tuning_method = "random_search",
        hyper_grid_domain_list = list(alpha = list(distribution_choice = "lognormal", pars = c(mean = 0,sd = 1)),
                                      lambda.min.ratio = list(distribution_choice = "uniform", pars = c(min = 0.1, max = 0.9))),
        sb_algorithm = "glmnet",
        chosen_eval_metric = "rss",
        split_method = "expanding",
        quantile_tau = 0.5,
        huber_delta = 1,
        n_iter = 3,
        target_fwd_name = "fwd_premium_1m"),
    "hyper_grid_domain_list not in correct format for random_search tuning."
  )


})

# Define your test
test_that("check_inputs_sb_backtest throws an error when bayesian_opt not correctly set.", {


  load(paste(test_path(),"/testdata/","artificial_signal_blending_obj.RData", sep =""))

  #Three elements instead of two
  expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        signal_universe_m_df = signal_universe_m_df,
        backtest_returns_m_xts = NULL,
        benchmark_returns_m_xts = NULL,
        signal_themes_m_df = NULL,
        concentration_constraint_policy = NULL,
        custom_signal_weights_m_df = NULL,
        gsm_algorithm = "ols",
        rebalancing_months = 9,
        tuning_method = "bayesian_opt",
        hyper_grid_domain_list = list(alpha = c(0,0.5,1), lambda.min.ratio = c(0.1,0.2)),
        sb_algorithm = "glmnet",
        split_method = "expanding",
        quantile_tau = 0.5,
        huber_delta = 1,
        n_iter = 3,
        chosen_eval_metric = "rss",
        target_fwd_name = "fwd_premium_1m"),
    "hyper_grid_domain_list not in correct format for bayesian_opt tuning."
  )

  #Not numeric
  expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        signal_universe_m_df = signal_universe_m_df,
        backtest_returns_m_xts = NULL,
        benchmark_returns_m_xts = NULL,
        signal_themes_m_df = NULL,
        concentration_constraint_policy = NULL,
        custom_signal_weights_m_df = NULL,
        gsm_algorithm = "ols",
        rebalancing_months = 9,
        n_iter = "four",
        init_points = 2,
        k_iter = 3,
        tuning_method = "bayesian_opt",
        hyper_grid_domain_list = list(alpha = c(0,1), lambda.min.ratio = c(0.1,0.2)),
        sb_algorithm = "glmnet",
        split_method = "expanding",
        quantile_tau = 0.5,
        huber_delta = 1,
        acq = "ei",
        chosen_eval_metric = "rss",
        target_fwd_name = "fwd_premium_1m"),
    "n_iter, k_iter and init_points must be numeric."
  )


})

# Define your test
test_that("check_inputs_sb_backtest throws an error when custom_objective wrongly set.", {


  load(paste(test_path(),"/testdata/","artificial_signal_blending_obj.RData", sep =""))

  #Setting custom obj for glmnet
  expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        tuning_method = "bayesian_opt",
        signal_universe_m_df = signal_universe_m_df,
        backtest_returns_m_xts = NULL,
        benchmark_returns_m_xts = NULL,
        signal_themes_m_df = NULL,
        concentration_constraint_policy = NULL,
        custom_signal_weights_m_df = NULL,
        gsm_algorithm = "ols",
        hyper_grid_domain_list = list(alpha = c(0,0.8), lambda.min.ratio = c(0.1,0.2)),
        n_iter = 3,
        k_iter = 1,
        init_points = 4,
        split_method = "expanding",
        quantile_tau = 0.5,
        huber_delta = 1,
        acq = "ei",
        custom_objective = "pseudo_huber_error",
        sb_algorithm = "glmnet",
        chosen_eval_metric = "rss",
        target_fwd_name = "fwd_premium_1m"),
    "Custom objective functions are only allowed for xgb, nn, sw or mvo sb_algorithm choices"
  )


  #Seting wrong custom obj
  expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        rebalancing_months = 9,
        signal_universe_m_df = signal_universe_m_df,
        backtest_returns_m_xts = NULL,
        benchmark_returns_m_xts = NULL,
        signal_themes_m_df = NULL,
        n_iter = NULL,
        concentration_constraint_policy = NULL,
        custom_signal_weights_m_df = NULL,
        tuning_method = "grid_search",
        hyper_grid_domain_list = list(min_child_weight = c(1,3), max_depth = c(1,2), subsample = c(0.3),
                                      colsample_bytree = c(0,0.2),
                                      eta = c(0,1), alpha = c(0,2), gamma = c(0,1), nrounds = 200),
        custom_objective = "quantile_error",
        sb_algorithm = "xgb",
        chosen_eval_metric = "rss",
        split_method = "expanding",
        gsm_algorithm = "ols",
        quantile_tau = 0.5,
        huber_delta = 1,
        target_fwd_name = "fwd_premium_1m")
    ,
    "Invalid custom_objective. Choose from 'squared_error', 'pseudo_huber_error', or 'absolute_error'."
  )

  #Seting inexistent heuristic sb metric
  expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        split_method = "expanding",
        quantile_tau = 0.5,
        gsm_algorithm = "ols",
        training_sample_size = 4,
        validation_sample_size = 0,
        huber_delta = 1,
        rebalancing_months = 9,
        hyper_grid_domain_list = NULL,
        n_iter = NULL,
        k_iter = NULL,
        tuning_method = NULL,
        signal_universe_m_df = signal_universe_m_df,
        backtest_returns_m_xts = xts::xts(data.frame(Alpha = rnorm(6), Beta = rnorm(6), Gamma = rnorm(6)),
                                          order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-08-15"), by = "month")),
        benchmark_returns_m_xts =  xts::xts(data.frame(IBOV = rnorm(6), SMLL = rnorm(6), IDIV = rnorm(6)),
                                            order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-08-15"), by = "month")),
        signal_themes_m_df = NULL,
        concentration_constraint_policy = NULL,
        cov_matrix_sample_size = 3,
        custom_signal_weights_m_df = NULL,
        custom_objective = "max_sharpe",
        sb_algorithm = "sw",
        chosen_eval_metric = "rss",
        active_returns = FALSE,
        cov_matrix_benchmark = "IBOV",
        target_fwd_name = "fwd_premium_1m"),
    "heuristic signal blending metric not found in signal_universe_m_df"
  )


  #invalid heuristic sb metric
  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 3,
      validation_sample_size = 2,
      huber_delta = 1,
      rebalancing_months = 9,
      hyper_grid_domain_list = list(alpha = c(0,0.8), lambda.min.ratio = c(0.1,0.2)),
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = "grid_search",
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = xts::xts(data.frame(Alpha = rnorm(6), Beta = rnorm(6), Gamma = rnorm(6)),
                                        order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-08-15"), by = "month")),
      benchmark_returns_m_xts =  xts::xts(data.frame(IBOV = rnorm(6), SMLL = rnorm(6), IDIV = rnorm(6)),
                                          order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-08-15"), by = "month")),
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = NULL,
      custom_objective = "max_sharpe",
      sb_algorithm = "sww",
      chosen_eval_metric = "rss",
      active_returns = FALSE,
      cov_matrix_benchmark = "IBOV",
      target_fwd_name = "fwd_premium_1m"),
    "sb_algorithm choice not supported."
  )

  #Early stop
  expect_error(
      check_inputs_sb_backtest(
        features_m_df = features_m_df,
        target_m_df = target_m_df,
        training_sample_size = 4,
        validation_sample_size = 1,
        signal_universe_m_df = signal_universe_m_df,
        backtest_returns_m_xts = NULL,
        benchmark_returns_m_xts = NULL,
        signal_themes_m_df = NULL,
        concentration_constraint_policy = NULL,
        custom_signal_weights_m_df = NULL,
        rebalancing_months = 9,
        tuning_method = "grid_search",
        hyper_grid_domain_list = list(alpha = c(0,1), lambda.min.ratio = c(0.1,0.2)),
        custom_objective = "squared_error",
        sb_algorithm = "glmnet",
        chosen_eval_metric = "rss",
        split_method = "expanding",
        gsm_algorithm = "ols",
        quantile_tau = 0.5,
        huber_delta = 1,
        n_iter = NULL,
        early_stop = 10,
        target_fwd_name = "fwd_premium_1m"),
    "Early stop only allowed for xgb or nn sb_algorithm choices"
  )


})

# Define your test
test_that("check_inputs_sb_backtest throws an error when there is a mismatch with signal_universe_m_df", {

  load(paste(test_path(),"/testdata/","artificial_signal_blending_obj.RData", sep =""))

  expanded_signal_universe_m_df <- signal_universe_m_df %>% dplyr::mutate(tickers = dplyr::if_else(tickers == "Gamma", "Vega", tickers)) %>%
    dplyr::mutate(id = paste0(tickers,"-",dates)) %>% dplyr::arrange(id)


  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      signal_universe_m_df = expanded_signal_universe_m_df,
      backtest_returns_m_xts = NULL,
      benchmark_returns_m_xts = NULL,
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      custom_signal_weights_m_df = NULL,
      gsm_algorithm = "ols",
      target_m_df = target_m_df,
      training_sample_size = 4,
      validation_sample_size = 1,
      rebalancing_months = 9,
      split_method = "expanding",
      tuning_method = "grid_search",
      hyper_grid_domain_list = list(alpha = c(1,0.5), lambda.min.ratio = c(0.1,0.2)),
      sb_algorithm = "glmnet",
      chosen_eval_metric = "rss",
      target_fwd_name = "fwd_premium_1m"),
    "There are eligible signals not present in features_m_df: Vega"
  )

  load(paste(test_path(),"/testdata/","artificial_signal_blending_obj.RData", sep =""))

  expanded_signal_universe_m_df <- signal_universe_m_df %>% dplyr::mutate(tickers = dplyr::if_else(tickers == "Gamma", "Vega", tickers)) %>%
    dplyr::mutate(id = paste0(tickers,"-",dates)) %>% dplyr::arrange(id)

  adjusted_features_m_df <- features_m_df %>% dplyr::rename(Vega = Gamma)

  backtest_returns_m_xts <- xts::xts(data.frame(Alpha = rnorm(6), Beta = rnorm(6), Gamma = rnorm(6)), order.by = features_m_df$dates %>% unique())


  expect_error(
    check_inputs_sb_backtest(
      features_m_df = adjusted_features_m_df,
      signal_universe_m_df = expanded_signal_universe_m_df,
      backtest_returns_m_xts = backtest_returns_m_xts,
      benchmark_returns_m_xts = NULL,
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      custom_signal_weights_m_df = NULL,
      gsm_algorithm = "ols",
      target_m_df = target_m_df,
      training_sample_size = 4,
      validation_sample_size = 1,
      rebalancing_months = 9,
      split_method = "expanding",
      tuning_method = "grid_search",
      hyper_grid_domain_list = list(alpha = c(1,0.5), lambda.min.ratio = c(0.1,0.2)),
      sb_algorithm = "glmnet",
      chosen_eval_metric = "rss",
      target_fwd_name = "fwd_premium_1m"),
    "There is a signal mismatch between eligible_signals and backtest_returns_m_xts: Vega"
  )



  load(paste(test_path(),"/testdata/","artificial_signal_blending_obj.RData", sep =""))

  expanded_signal_universe_m_df <- signal_universe_m_df %>% dplyr::mutate(tickers = dplyr::if_else(tickers == "Gamma", "Vega", tickers)) %>%
    dplyr::mutate(id = paste0(tickers,"-",dates)) %>% dplyr::arrange(id)

  adjusted_features_m_df <- features_m_df %>% dplyr::rename(Vega = Gamma)

  backtest_returns_m_xts <- xts::xts(data.frame(Alpha = rnorm(6), Beta = rnorm(6), Vega = rnorm(6)), order.by = features_m_df$dates %>% unique())

  signal_themes_m_df <- signal_universe_m_df %>% dplyr::select(-is_eligible) %>%
    dplyr::mutate(theme = dplyr::case_when(tickers == "Alpha" ~ "theme1", tickers == "Beta" ~ "theme2", tickers == "Gamma" ~ "theme3"))

  benchmark_returns_m_xts <-  xts::xts(data.frame(Alpha = rnorm(6), Beta = rnorm(6), Vega = rnorm(6)), order.by = features_m_df$dates %>% unique())


  expect_error(
    check_inputs_sb_backtest(
      features_m_df = adjusted_features_m_df,
      signal_universe_m_df = expanded_signal_universe_m_df,
      backtest_returns_m_xts = backtest_returns_m_xts,
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      signal_themes_m_df = signal_themes_m_df,
      concentration_constraint_policy = NULL,
      custom_signal_weights_m_df = NULL,
      gsm_algorithm = "ols",
      target_m_df = target_m_df,
      training_sample_size = 4,
      rebalancing_months = 9,
      validation_sample_size = 0,
      split_method = "expanding",
      cov_matrix_sample_size = 2,
      tuning_method = "grid_search",
      hyper_grid_domain_list = list(alpha = c(1,0.5), lambda.min.ratio = c(0.1,0.2)),
      sb_algorithm = "rp",
      chosen_eval_metric = "rss",
      active_returns = TRUE,
      cov_matrix_benchmark = "Alpha",
      target_fwd_name = "fwd_premium_1m"),
    "all ids in signal_universe_m_df must have a theme classification"
  )




})

# Define your test
test_that("check_inputs_sb_backtest throws an error for wrong backtest_returns_m_xts", {

  load(paste(test_path(),"/testdata/","artificial_signal_blending_obj.RData", sep =""))

  backtest_returns_m_xts = xts::xts(data.frame(Alpha = rnorm(6), Beta = rnorm(6), Gamma = rnorm(6)),
                                    order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-08-15"), by = "month"))

  #df
  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      validation_sample_size = 0,
      huber_delta = 1,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = as.data.frame(backtest_returns_m_xts),
      benchmark_returns_m_xts =  xts::xts(data.frame(IBOV = rnorm(6), SMLL = rnorm(6), IDIV = rnorm(6)),
                                          order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-08-15"), by = "month")),
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = NULL,
      custom_objective = "squared_error",
      sb_algorithm = "rp",
      chosen_eval_metric = "rss",
      active_returns = FALSE,
      cov_matrix_benchmark = "IBOV",
      target_fwd_name = "fwd_premium_1m"),
    "backtest_returns_m_xts must be a xts object"
  )

  load(paste(test_path(),"/testdata/","artificial_signal_blending_obj.RData", sep =""))

  backtest_returns_m_xts = xts::xts(data.frame(Alpha = rnorm(6), Beta = rnorm(6), Gamma = rnorm(6)),
                                    order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-08-15"), by = "month"))

  wrong_backtest_returns_m_xts <- backtest_returns_m_xts
  wrong_backtest_returns_m_xts[2,3] <- NA

  #Seting inexistent heuristic sb metric
  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      validation_sample_size = 0,
      huber_delta = 1,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = wrong_backtest_returns_m_xts,
      benchmark_returns_m_xts =  xts::xts(data.frame(IBOV = rnorm(6), SMLL = rnorm(6), IDIV = rnorm(6)),
                                          order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-08-15"), by = "month")),
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = NULL,
      custom_objective = "squared_error",
      sb_algorithm = "rp",
      chosen_eval_metric = "rss",
      active_returns = FALSE,
      cov_matrix_benchmark = "IBOV",
      target_fwd_name = "fwd_premium_1m"),
    "backtest_returns_m_xts must not have any NA"
  )

  backtest_returns_m_xts = xts::xts(data.frame(Alpha = rnorm(6), Beta = rnorm(6), Gamma = rnorm(6)),
                                    order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-08-15"), by = "month"))

  wrong_backtest_returns_m_xts <- backtest_returns_m_xts[c(1:3),]

  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      validation_sample_size = 0,
      huber_delta = 1,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = wrong_backtest_returns_m_xts,
      benchmark_returns_m_xts =  xts::xts(data.frame(IBOV = rnorm(6), SMLL = rnorm(6), IDIV = rnorm(6)),
                                          order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-08-15"), by = "month")),
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = NULL,
      custom_objective = "squared_error",
      sb_algorithm = "rp",
      chosen_eval_metric = "rss",
      active_returns = FALSE,
      cov_matrix_benchmark = "IBOV",
      target_fwd_name = "fwd_premium_1m"),
    "backtest_returns_m_xts must have at least training_sample_size \\+ validation_sample_size rows"
  )

  wrong_backtest_returns_m_xts = xts::xts(data.frame(Alpha = rnorm(5), Beta = rnorm(5), Gamma = rnorm(5)),
                                    order.by = seq.Date(as.Date("2001-04-15"), as.Date("2001-08-15"), by = "month"))


  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      validation_sample_size = 0,
      huber_delta = 1,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = wrong_backtest_returns_m_xts,
      benchmark_returns_m_xts =  xts::xts(data.frame(IBOV = rnorm(6), SMLL = rnorm(6), IDIV = rnorm(6)),
                                          order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-08-15"), by = "month")),
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = NULL,
      custom_objective = "squared_error",
      sb_algorithm = "rp",
      chosen_eval_metric = "rss",
      active_returns = FALSE,
      cov_matrix_benchmark = "IBOV",
      target_fwd_name = "fwd_premium_1m"),
    "all dates in signal_universe_m_df must be present in backtest_returns_m_xts"
  )

  wrong_backtest_returns_m_xts <- xts::xts(data.frame(Alpha = rnorm(5), Beta = rnorm(5), Gamma = rnorm(5)),
                                          order.by = seq.Date(as.Date("2001-04-15"), as.Date("2001-08-15"), by = "month"))
  wrong_signal_universe_m_df <- signal_universe_m_df %>% dplyr::filter(!dates == "2001-03-15")


  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      validation_sample_size = 0,
      huber_delta = 1,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = wrong_signal_universe_m_df,
      backtest_returns_m_xts = wrong_backtest_returns_m_xts,
      benchmark_returns_m_xts =  xts::xts(data.frame(IBOV = rnorm(6), SMLL = rnorm(6), IDIV = rnorm(6)),
                                          order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-08-15"), by = "month")),
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = NULL,
      custom_objective = "squared_error",
      sb_algorithm = "rp",
      chosen_eval_metric = "rss",
      active_returns = FALSE,
      cov_matrix_benchmark = "IBOV",
      target_fwd_name = "fwd_premium_1m"),
    "dates in benchmark_returns_m_xts and backtest_returns_m_xts must be the same"
  )


  wrong_backtest_returns_m_xts <- xts::xts(data.frame(Alpha = rnorm(5), Beta = rnorm(5), Gamma = rnorm(5)),
                                           order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-07-15"), by = "month"))
  wrong_signal_universe_m_df <- signal_universe_m_df %>% dplyr::filter(!dates == "2001-08-15")
  wrong_benchmark_returns_m_xts <- xts::xts(data.frame(IBOV = rnorm(5), SMLL = rnorm(5), IDIV = rnorm(5)),
                                      order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-07-15"), by = "month"))


  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      validation_sample_size = 0,
      huber_delta = 1,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = wrong_signal_universe_m_df,
      backtest_returns_m_xts = wrong_backtest_returns_m_xts,
      benchmark_returns_m_xts =  wrong_benchmark_returns_m_xts,
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = NULL,
      custom_objective = "squared_error",
      sb_algorithm = "rp",
      chosen_eval_metric = "rss",
      active_returns = FALSE,
      cov_matrix_benchmark = "IBOV",
      target_fwd_name = "fwd_premium_1m"),
    "all backtest_dates derived from features_m_df must be present in backtest_returns_m_xts"
  )

  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 2,
      validation_sample_size = 0,
      huber_delta = 1,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = backtest_returns_m_xts,
      benchmark_returns_m_xts =  benchmark_returns_m_xts,
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = NULL,
      custom_objective = "squared_error",
      sb_algorithm = "rp",
      chosen_eval_metric = "rss",
      active_returns = FALSE,
      cov_matrix_benchmark = "IBOV",
      target_fwd_name = "fwd_premium_1m"),
    "There is only one date in backtest_returns_m_xts before the first training date"
  )

  backtest_returns_m_xts <- xts::xts(data.frame(Alpha = rnorm(6), Beta = rnorm(6), Gamma = rnorm(6)),
                                           order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-08-15"), by = "month"))
  benchmark_returns_m_xts <- xts::xts(data.frame(IBOV = rnorm(6), SMLL = rnorm(6), IDIV = rnorm(6)),
                                            order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-08-15"), by = "month"))

  wrong_backtest_returns_m_xts <- backtest_returns_m_xts
  wrong_backtest_returns_m_xts <- backtest_returns_m_xts[-2,]
  wrong_benchmark_returns_m_xts <- benchmark_returns_m_xts
  wrong_benchmark_returns_m_xts <- benchmark_returns_m_xts[-2,]

  wrong_signal_universe_m_df <- signal_universe_m_df %>% dplyr::filter(!dates == "2001-04-15")

  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      validation_sample_size = 0,
      huber_delta = 1,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = wrong_signal_universe_m_df,
      backtest_returns_m_xts = wrong_backtest_returns_m_xts,
      benchmark_returns_m_xts =  wrong_benchmark_returns_m_xts,
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = NULL,
      custom_objective = "squared_error",
      sb_algorithm = "rp",
      chosen_eval_metric = "rss",
      active_returns = FALSE,
      cov_matrix_benchmark = "IBOV",
      target_fwd_name = "fwd_premium_1m"),
    "backtest_returns_m_xts must have consecutive dates"
  )


  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      validation_sample_size = 0,
      huber_delta = 1,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = backtest_returns_m_xts,
      benchmark_returns_m_xts =  xts::xts(data.frame(IBOV = rnorm(6), SMLL = rnorm(6), IDIV = rnorm(6)),
                                          order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-08-15"), by = "month")),
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 30,
      custom_signal_weights_m_df = NULL,
      custom_objective = "squared_error",
      sb_algorithm = "rp",
      chosen_eval_metric = "rss",
      active_returns = FALSE,
      cov_matrix_benchmark = "IBOV",
      target_fwd_name = "fwd_premium_1m"),
    "backtest_returns_m_xts must have more dates than cov_matrix_sample_size"
  )



  #Absent when needed
  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      validation_sample_size = 0,
      huber_delta = 1,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = NULL,
      benchmark_returns_m_xts =  xts::xts(data.frame(IBOV = rnorm(6), SMLL = rnorm(6), IDIV = rnorm(6)),
                                          order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-08-15"), by = "month")),
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = NULL,
      custom_objective = "max_sharpe",
      sb_algorithm = "rp",
      chosen_eval_metric = "rss",
      active_returns = FALSE,
      cov_matrix_benchmark = "IBOV",
      target_fwd_name = "fwd_premium_1m"),
    "backtest_returns_m_xts are strictly needed when sb_algorithm is either rp or mvo."
  )

})

# Define your test
test_that("check_inputs_sb_backtest throws an error for wrong benchmark_returns_m_xts", {

  load(paste(test_path(),"/testdata/","artificial_signal_blending_obj.RData", sep =""))

  backtest_returns_m_xts = xts::xts(data.frame(Alpha = rnorm(6), Beta = rnorm(6), Gamma = rnorm(6)),
                                    order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-08-15"), by = "month"))
  benchmark_returns_m_xts = xts::xts(data.frame(IBOV = rnorm(6), SMLL = rnorm(6), IDIV = rnorm(6)),
                                     order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-08-15"), by = "month"))

  wrong_benchmark_returns_m_xts <- benchmark_returns_m_xts
  wrong_benchmark_returns_m_xts[2,3] <- NA


  #df
  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      validation_sample_size = 0,
      huber_delta = 1,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = backtest_returns_m_xts,
      benchmark_returns_m_xts = wrong_benchmark_returns_m_xts,
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = NULL,
      custom_objective = "squared_error",
      sb_algorithm = "rp",
      chosen_eval_metric = "rss",
      active_returns = FALSE,
      cov_matrix_benchmark = "IBOV",
      target_fwd_name = "fwd_premium_1m"),
    "benchmark_returns_m_xts must not have any NA values"
  )

  backtest_returns_m_xts = xts::xts(data.frame(Alpha = rnorm(6), Beta = rnorm(6), Gamma = rnorm(6)),
                                    order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-08-15"), by = "month"))
  benchmark_returns_m_xts = xts::xts(data.frame(IBOV = rnorm(6), SMLL = rnorm(6), IDIV = rnorm(6)),
                                     order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-08-15"), by = "month"))

  wrong_benchmark_returns_m_xts <- benchmark_returns_m_xts[-2,]


  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      validation_sample_size = 0,
      huber_delta = 1,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = backtest_returns_m_xts,
      benchmark_returns_m_xts = wrong_benchmark_returns_m_xts,
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = NULL,
      custom_objective = "squared_error",
      sb_algorithm = "rp",
      chosen_eval_metric = "rss",
      active_returns = FALSE,
      cov_matrix_benchmark = "IBOV",
      target_fwd_name = "fwd_premium_1m"),
    "dates in benchmark_returns_m_xts and backtest_returns_m_xts must be the same"
  )


  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      validation_sample_size = 0,
      huber_delta = 1,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = backtest_returns_m_xts,
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = NULL,
      custom_objective = "squared_error",
      sb_algorithm = "rp",
      chosen_eval_metric = "rss",
      active_returns = FALSE,
      cov_matrix_benchmark = "AGR",
      target_fwd_name = "fwd_premium_1m"),
    "cov_matrix_benchmark must be present in benchmark_returns_m_xts"
  )

  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      validation_sample_size = 0,
      huber_delta = 1,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = backtest_returns_m_xts,
      benchmark_returns_m_xts = as.data.frame(benchmark_returns_m_xts),
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = NULL,
      custom_objective = "squared_error",
      sb_algorithm = "rp",
      chosen_eval_metric = "rss",
      active_returns = FALSE,
      cov_matrix_benchmark = "IBOV",
      target_fwd_name = "fwd_premium_1m"),
    "benchmark_returns_m_xts must be a xts object"
  )

  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      validation_sample_size = 0,
      huber_delta = 1,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = backtest_returns_m_xts,
      benchmark_returns_m_xts = NULL,
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = NULL,
      custom_objective = "squared_error",
      sb_algorithm = "rp",
      chosen_eval_metric = "rss",
      active_returns = TRUE,
      cov_matrix_benchmark = "IBOV",
      target_fwd_name = "fwd_premium_1m"),
    "benchmark_returns_m_xts are strictly needed when sb_algorithm is either rp or mvo and active_returns is set to TRUE."
  )





})

# Define your test
test_that("check_inputs_sb_backtest throws an error for wrong signal_themes_m_df", {

  load(paste(test_path(),"/testdata/","artificial_signal_blending_obj.RData", sep =""))

  backtest_returns_m_xts = xts::xts(data.frame(Alpha = rnorm(6), Beta = rnorm(6), Gamma = rnorm(6)),
                                    order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-08-15"), by = "month"))
  benchmark_returns_m_xts = xts::xts(data.frame(IBOV = rnorm(6), SMLL = rnorm(6), IDIV = rnorm(6)),
                                     order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-08-15"), by = "month"))

  signal_themes_m_df <- signal_universe_m_df %>% dplyr::select(-is_eligible) %>%
    dplyr::mutate(theme = dplyr::case_when(
      tickers %in% "Alpha" ~ "value",
      tickers %in% "Beta" ~ "momentum",
      tickers %in% "Gamma" ~ "value"
    ))
  wrong_signal_themes_m_df <- signal_themes_m_df
  colnames(wrong_signal_themes_m_df)[4] <- "them"

  #df
  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      validation_sample_size = 0,
      huber_delta = 1,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = backtest_returns_m_xts,
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      signal_themes_m_df = wrong_signal_themes_m_df,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = NULL,
      custom_objective = "squared_error",
      sb_algorithm = "rp",
      chosen_eval_metric = "rss",
      active_returns = FALSE,
      cov_matrix_benchmark = "IBOV",
      target_fwd_name = "fwd_premium_1m"),
    "signal_themes_m_df must have columns 'id', 'tickers', 'dates' and 'theme'"
  )

  wrong_signal_themes_m_df <- signal_themes_m_df
  wrong_signal_themes_m_df$theme <- 1

  #df
  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      validation_sample_size = 0,
      huber_delta = 1,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = backtest_returns_m_xts,
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      signal_themes_m_df = wrong_signal_themes_m_df,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = NULL,
      custom_objective = "squared_error",
      sb_algorithm = "rp",
      chosen_eval_metric = "rss",
      active_returns = FALSE,
      cov_matrix_benchmark = "IBOV",
      target_fwd_name = "fwd_premium_1m"),
    "theme column in signal_themes_m_df must be character"
  )

  wrong_signal_themes_m_df <- signal_themes_m_df
  wrong_signal_themes_m_df$theme[1:6] <- "high_value"

  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      validation_sample_size = 0,
      huber_delta = 1,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = backtest_returns_m_xts,
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      signal_themes_m_df = wrong_signal_themes_m_df,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = NULL,
      custom_objective = "squared_error",
      sb_algorithm = "rp",
      chosen_eval_metric = "rss",
      active_returns = FALSE,
      cov_matrix_benchmark = "IBOV",
      target_fwd_name = "fwd_premium_1m"),
    "No underscores allowed in signal_themes_m_df theme names"
  )


  wrong_signal_themes_m_df <- signal_themes_m_df %>% dplyr::filter(!dates == "2001-03-15")

  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      validation_sample_size = 0,
      huber_delta = 1,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = backtest_returns_m_xts,
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      signal_themes_m_df = wrong_signal_themes_m_df,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = NULL,
      custom_objective = "squared_error",
      sb_algorithm = "rp",
      chosen_eval_metric = "rss",
      active_returns = FALSE,
      cov_matrix_benchmark = "IBOV",
      target_fwd_name = "fwd_premium_1m"),
    "dates in signal_themes_m_df and features_m_df must be the same"
  )

  wrong_signal_themes_m_df <- signal_themes_m_df
  wrong_signal_themes_m_df$theme[3] <- NA

  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      validation_sample_size = 0,
      huber_delta = 1,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = backtest_returns_m_xts,
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      signal_themes_m_df = wrong_signal_themes_m_df,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = NULL,
      custom_objective = "squared_error",
      sb_algorithm = "rp",
      chosen_eval_metric = "rss",
      active_returns = FALSE,
      cov_matrix_benchmark = "IBOV",
      target_fwd_name = "fwd_premium_1m"),
    "signal_themes_m_df should not have NAs"
  )

  wrong_signal_universe_m_df <- signal_universe_m_df
  wrong_signal_universe_m_df <- wrong_signal_universe_m_df %>%
    dplyr::bind_rows(data.frame(id = "Iota-2001-03-15", tickers = "Iota", dates = as.Date("2001-03-15"), is_eligible = 1))

  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      validation_sample_size = 0,
      huber_delta = 1,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = wrong_signal_universe_m_df,
      backtest_returns_m_xts = backtest_returns_m_xts,
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      signal_themes_m_df = wrong_signal_themes_m_df,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = NULL,
      custom_objective = "squared_error",
      sb_algorithm = "rp",
      chosen_eval_metric = "rss",
      active_returns = FALSE,
      cov_matrix_benchmark = "IBOV",
      target_fwd_name = "fwd_premium_1m"),
    "all ids in signal_universe_m_df must have a theme classification"
  )




})

# Define your test
test_that("check_inputs_sb_backtest throws an error for wrong custom_signal_weights", {

  load(paste(test_path(),"/testdata/","artificial_signal_blending_obj.RData", sep =""))

  backtest_returns_m_xts = xts::xts(data.frame(Alpha = rnorm(6), Beta = rnorm(6), Gamma = rnorm(6)),
                                    order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-08-15"), by = "month"))
  benchmark_returns_m_xts = xts::xts(data.frame(IBOV = rnorm(6), SMLL = rnorm(6), IDIV = rnorm(6)),
                                     order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-08-15"), by = "month"))

  custom_signal_weights_m_df <- signal_universe_m_df %>% dplyr::select(-is_eligible) %>%
    dplyr::group_by(dates) %>%
    dplyr::mutate(weights = 1/ dplyr::n()) %>%
    dplyr::ungroup()
  wrong_custom_signal_weights_m_df <- custom_signal_weights_m_df
  colnames(wrong_custom_signal_weights_m_df)[4] <- "weight"

  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      validation_sample_size = 0,
      huber_delta = 1,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = backtest_returns_m_xts,
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = wrong_custom_signal_weights_m_df,
      custom_objective = "squared_error",
      sb_algorithm = "custom_weights",
      chosen_eval_metric = "rss",
      active_returns = FALSE,
      cov_matrix_benchmark = "IBOV",
      target_fwd_name = "fwd_premium_1m"),
    "custom_signal_weights_m_df must have a 'weights' column"
  )

  wrong_custom_signal_weights_m_df <- custom_signal_weights_m_df
  wrong_custom_signal_weights_m_df$weights <- "2"

  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      validation_sample_size = 0,
      huber_delta = 1,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = backtest_returns_m_xts,
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = wrong_custom_signal_weights_m_df,
      custom_objective = "squared_error",
      sb_algorithm = "custom_weights",
      chosen_eval_metric = "rss",
      active_returns = FALSE,
      cov_matrix_benchmark = "IBOV",
      target_fwd_name = "fwd_premium_1m"),
    "weights column in custom_signal_weights_m_df must be numeric"
  )


  wrong_custom_signal_weights_m_df <- custom_signal_weights_m_df %>%
    dplyr::mutate(tickers = dplyr::if_else(tickers == "Alpha", "Iota", tickers))

  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      validation_sample_size = 0,
      huber_delta = 1,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = backtest_returns_m_xts,
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = wrong_custom_signal_weights_m_df,
      custom_objective = "squared_error",
      sb_algorithm = "custom_weights",
      chosen_eval_metric = "rss",
      active_returns = FALSE,
      cov_matrix_benchmark = "IBOV",
      target_fwd_name = "fwd_premium_1m"),
    "custom_signal_weights_m_df is not coercible to meta dataframe"
  )


  wrong_custom_signal_weights_m_df <- custom_signal_weights_m_df %>%
    dplyr::mutate(tickers = dplyr::if_else(tickers == "Alpha", "Iota", tickers)) %>%
    dplyr::mutate(id = paste0(tickers, "-", dates)) %>%
    dplyr::arrange(id)

  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      validation_sample_size = 0,
      huber_delta = 1,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = backtest_returns_m_xts,
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = wrong_custom_signal_weights_m_df,
      custom_objective = "squared_error",
      sb_algorithm = "custom_weights",
      chosen_eval_metric = "rss",
      active_returns = FALSE,
      cov_matrix_benchmark = "IBOV",
      target_fwd_name = "fwd_premium_1m"),
    "all ids in signal_universe_m_df should have a correspondence in custom_signal_weights_m_df"
  )


  wrong_signal_universe_m_df <- signal_universe_m_df
  wrong_signal_universe_m_df$is_eligible[4] <- 0

  expect_error(
  expect_message(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      validation_sample_size = 0,
      huber_delta = 1,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = wrong_signal_universe_m_df,
      backtest_returns_m_xts = backtest_returns_m_xts,
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = custom_signal_weights_m_df,
      custom_objective = "squared_error",
      sb_algorithm = "custom_weights",
      chosen_eval_metric = "rss",
      active_returns = FALSE,
      cov_matrix_benchmark = "IBOV",
      target_fwd_name = "fwd_premium_1m"),
    "Some ids in custom_signal_weights_m_df are not eligible: Alpha-2001-06-15"
  )
  )

  wrong_custom_signal_weights_m_df <- custom_signal_weights_m_df %>%
    dplyr::mutate(tickers = dplyr::if_else(tickers == "Alpha", "Iota", tickers)) %>%
    dplyr::mutate(id = paste0(tickers, "-", dates)) %>%
    dplyr::arrange(id)

  wrong_signal_universe_m_df <- signal_universe_m_df  %>%
    dplyr::mutate(tickers = dplyr::if_else(tickers == "Alpha", "Iota", tickers)) %>%
    dplyr::mutate(id = paste0(tickers, "-", dates)) %>%
    dplyr::arrange(id)

  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      validation_sample_size = 0,
      huber_delta = 1,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = wrong_signal_universe_m_df,
      backtest_returns_m_xts = backtest_returns_m_xts,
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = wrong_custom_signal_weights_m_df,
      custom_objective = "squared_error",
      sb_algorithm = "custom_weights",
      chosen_eval_metric = "rss",
      active_returns = FALSE,
      cov_matrix_benchmark = "IBOV",
      target_fwd_name = "fwd_premium_1m"),
    "There is a signal mismatch between non zero-weight signals in custom_signal_weights_m_df and features_m_df: Iota"
  )


  wrong_custom_signal_weights_m_df <- custom_signal_weights_m_df
  wrong_custom_signal_weights_m_df$weights[1] <- 0

  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      validation_sample_size = 0,
      huber_delta = 1,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = backtest_returns_m_xts,
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = wrong_custom_signal_weights_m_df,
      custom_objective = "squared_error",
      sb_algorithm = "custom_weights",
      chosen_eval_metric = "rss",
      active_returns = FALSE,
      cov_matrix_benchmark = "IBOV",
      target_fwd_name = "fwd_premium_1m"),
    "custom_signal_weights_m_df do not sum to 1 at dates: 2001-03-15"
  )


  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      validation_sample_size = 0,
      huber_delta = 1,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = backtest_returns_m_xts,
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = NULL,
      custom_objective = "squared_error",
      sb_algorithm = "custom_weights",
      chosen_eval_metric = "rss",
      active_returns = FALSE,
      cov_matrix_benchmark = "IBOV",
      target_fwd_name = "fwd_premium_1m"),
    "custom_signal_weights_m_df must be provided if sb_algorithm is custom_weights."
  )


  wrong_signal_universe_m_df <- signal_universe_m_df %>%
    dplyr::bind_rows(data.frame(id = "Iota-2001-08-15", tickers = "Iota", dates = as.Date("2001-08-15"), is_eligible = 1))

  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      validation_sample_size = 0,
      huber_delta = 1,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = wrong_signal_universe_m_df,
      backtest_returns_m_xts = backtest_returns_m_xts,
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = custom_signal_weights_m_df,
      custom_objective = "squared_error",
      sb_algorithm = "custom_weights",
      chosen_eval_metric = "rss",
      active_returns = FALSE,
      cov_matrix_benchmark = "IBOV",
      target_fwd_name = "fwd_premium_1m"),
    "all ids in signal_universe_m_df should have a correspondence in custom_signal_weights_m_df"
  )


})

#Define your test
test_that("check_inputs_sb_backtest throws an error for wrong signal_universe", {

  load(paste(test_path(),"/testdata/","artificial_signal_blending_obj.RData", sep =""))

  backtest_returns_m_xts = xts::xts(data.frame(Alpha = rnorm(6), Beta = rnorm(6), Gamma = rnorm(6)),
                                    order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-08-15"), by = "month"))
  benchmark_returns_m_xts = xts::xts(data.frame(IBOV = rnorm(6), SMLL = rnorm(6), IDIV = rnorm(6)),
                                     order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-08-15"), by = "month"))

  wrong_signal_universe_m_df <- signal_universe_m_df %>% dplyr::mutate(var = dplyr::if_else(tickers == "Alpha", NA, 2))

  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      validation_sample_size = 0,
      huber_delta = 1,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = wrong_signal_universe_m_df,
      backtest_returns_m_xts = backtest_returns_m_xts,
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = wrong_custom_signal_weights_m_df,
      custom_objective = "max_var",
      sb_algorithm = "sw",
      chosen_eval_metric = "rss",
      active_returns = FALSE,
      cov_matrix_benchmark = "IBOV",
      target_fwd_name = "fwd_premium_1m"),
    "heuristic signal blending metric contains NAs"
  )



  wrong_signal_universe_m_df <- signal_universe_m_df %>% dplyr::arrange(desc(id))

  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      validation_sample_size = 0,
      huber_delta = 1,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = wrong_signal_universe_m_df,
      backtest_returns_m_xts = backtest_returns_m_xts,
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = wrong_custom_signal_weights_m_df,
      custom_objective = "max_var",
      sb_algorithm = "sw",
      chosen_eval_metric = "rss",
      active_returns = FALSE,
      cov_matrix_benchmark = "IBOV",
      target_fwd_name = "fwd_premium_1m"),
    "signal_universe_m_df should be coercible to meta_dataframe object"
  )

  wrong_signal_universe_m_df <- signal_universe_m_df %>% dplyr::filter(!dates <= "2001-06-15")

  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      validation_sample_size = 0,
      huber_delta = 1,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = wrong_signal_universe_m_df,
      backtest_returns_m_xts = backtest_returns_m_xts,
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = NULL,
      custom_objective = "squared_error",
      sb_algorithm = "rp",
      chosen_eval_metric = "rss",
      active_returns = FALSE,
      cov_matrix_benchmark = "IBOV",
      target_fwd_name = "fwd_premium_1m"),
    "First date of signal_universe_m_df should be before first training date."
  )



})

# Define your test
test_that("check_inputs_sb_backtest throws an error for wrong concentration_constraint_policy", {

  load(paste(test_path(),"/testdata/","artificial_signal_blending_obj.RData", sep =""))

  backtest_returns_m_xts = xts::xts(data.frame(Alpha = rnorm(6), Beta = rnorm(6), Gamma = rnorm(6)),
                                    order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-08-15"), by = "month"))
  benchmark_returns_m_xts = xts::xts(data.frame(IBOV = rnorm(6), SMLL = rnorm(6), IDIV = rnorm(6)),
                                     order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-08-15"), by = "month"))

  signal_themes_m_df <- signal_universe_m_df %>% dplyr::select(-is_eligible) %>%
    dplyr::mutate(theme = dplyr::case_when(
      tickers %in% "Alpha" ~ "value",
      tickers %in% "Beta" ~ "momentum",
      tickers %in% "Gamma" ~ "value"
    ))

  concentration_constraint_policy <- list(benchmark = "ibov", max_abs_active_group_weight = 0.2)


  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      validation_sample_size = 0,
      huber_delta = 1,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = backtest_returns_m_xts,
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      signal_themes_m_df = NULL,
      concentration_constraint_policy = concentration_constraint_policy,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = NULL,
      custom_objective = "squared_error",
      sb_algorithm = "rp",
      chosen_eval_metric = "rss",
      active_returns = FALSE,
      cov_matrix_benchmark = "IBOV",
      target_fwd_name = "fwd_premium_1m"),
    "signal_themes_m_df must be provided if max_abs_active_group_weight is given."
  )


  wrong_concentration_constraint_policy <- list(benchmarks = "ibov", max_abs_active_group_weight = 0.2)


  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      validation_sample_size = 0,
      huber_delta = 1,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = backtest_returns_m_xts,
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      signal_themes_m_df = signal_themes_m_df,
      concentration_constraint_policy = wrong_concentration_constraint_policy,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = NULL,
      custom_objective = "squared_error",
      sb_algorithm = "rp",
      chosen_eval_metric = "rss",
      active_returns = FALSE,
      cov_matrix_benchmark = "IBOV",
      target_fwd_name = "fwd_premium_1m"),
    "Error in concentration_constraint_policy: elements of concentration_constraint_policy should be one of benchmark, max_abs_active_individual_weight or max_abs_active_group_weight."
  )

  wrong_signal_universe_m_df <- signal_universe_m_df %>%
    dplyr::mutate(is_eligible = dplyr::if_else(tickers == "Beta", 0, 1))

  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      validation_sample_size = 0,
      huber_delta = 1,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = wrong_signal_universe_m_df,
      backtest_returns_m_xts = backtest_returns_m_xts,
      benchmark_returns_m_xts = benchmark_returns_m_xts,
      signal_themes_m_df = signal_themes_m_df,
      concentration_constraint_policy = concentration_constraint_policy,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = NULL,
      custom_objective = "squared_error",
      sb_algorithm = "rp",
      chosen_eval_metric = "rss",
      active_returns = FALSE,
      cov_matrix_benchmark = "IBOV",
      target_fwd_name = "fwd_premium_1m"),
    "All themes in signal_themes_m_df must be present in eligible signals when max_abs_active_group_weight is given.
                 Running run_ss_backtest with enable_theme_representativeness as TRUE may help."
  )







})

# Define your test
test_that("check_inputs_sb_backtest throws error for heuristic methods", {

  load(paste(test_path(),"/testdata/","artificial_signal_blending_obj.RData", sep =""))

  backtest_returns_m_xts = xts::xts(data.frame(Alpha = rnorm(6), Beta = rnorm(6), Gamma = rnorm(6)),
                                    order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-08-15"), by = "month"))

  signal_themes_m_df <- signal_universe_m_df %>% dplyr::select(-is_eligible) %>%
    dplyr::mutate(theme = dplyr::case_when(
      tickers %in% "Alpha" ~ "value",
      tickers %in% "Beta" ~ "momentum",
      tickers %in% "Gamma" ~ "value"
    ))

  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      validation_sample_size = 0,
      huber_delta = 1,
      rebalancing_months = 9,
      exp_ret_score_tilt = NULL,
      exp_ret_score_tilt_eta = NULL,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = backtest_returns_m_xts,
      benchmark_returns_m_xts =  xts::xts(data.frame(IBOV = rnorm(6), SMLL = rnorm(6), IDIV = rnorm(6)),
                                          order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-08-15"), by = "month")),
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = NULL,
      custom_objective = "squared_error",
      sb_algorithm = "rp",
      chosen_eval_metric = "rss",
      active_returns = FALSE,
      cov_matrix_benchmark = "IBOV",
      cov_estimation_method = NULL,
      early_stop = NULL,
      target_fwd_name = "fwd_premium_1m"),
    "cov_estimation_method should be set for rp, hrp, mvo and mmaf algorithms"
  )



  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      validation_sample_size = 0,
      huber_delta = 1,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      exp_ret_score_tilt = NULL,
      exp_ret_score_tilt_eta = NULL,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = backtest_returns_m_xts,
      benchmark_returns_m_xts =  xts::xts(data.frame(IBOV = rnorm(6), SMLL = rnorm(6), IDIV = rnorm(6)),
                                          order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-08-15"), by = "month")),
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 5,
      custom_signal_weights_m_df = NULL,
      custom_objective = "squared_error",
      sb_algorithm = "rp",
      chosen_eval_metric = "rss",
      active_returns = FALSE,
      cov_matrix_benchmark = "IBOV",
      cov_estimation_method = "sample",
      early_stop = NULL,
      target_fwd_name = "fwd_premium_1m"),
    "cov_matrix_sample_size should be smaller than or equal to training_sample_size"
  )

  ## Invalid custom obj
  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      validation_sample_size = 0,
      huber_delta = 1,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      exp_ret_score_tilt = "all",
      exp_ret_score_tilt_eta = NULL,
      ridge_pen = NULL,
      macro_ridge_pen = NULL,
      micro_port_construction_method = NULL,
      macro_port_construction_method = NULL,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = backtest_returns_m_xts,
      benchmark_returns_m_xts =  xts::xts(data.frame(IBOV = rnorm(6), SMLL = rnorm(6), IDIV = rnorm(6)),
                                          order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-08-15"), by = "month")),
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = NULL,
      custom_objective = "squared_error",
      sb_algorithm = "rp",
      chosen_eval_metric = "rss",
      active_returns = TRUE,
      cov_matrix_benchmark = "IBOV",
      cov_estimation_method = "sample",
      early_stop = NULL,
      target_fwd_name = "fwd_premium_1m")
    )


  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      validation_sample_size = 0,
      huber_delta = 1,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      exp_ret_score_tilt = "all",
      exp_ret_score_tilt_eta = NULL,
      ridge_pen = NULL,
      macro_ridge_pen = NULL,
      micro_port_construction_method = NULL,
      macro_port_construction_method = NULL,
      signal_universe_m_df = signal_universe_m_df %>% dplyr::mutate(info_ratio = rnorm(dplyr::n())),
      backtest_returns_m_xts = backtest_returns_m_xts,
      benchmark_returns_m_xts =  xts::xts(data.frame(IBOV = rnorm(6), SMLL = rnorm(6), IDIV = rnorm(6)),
                                          order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-08-15"), by = "month")),
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = NULL,
      custom_objective = "max_info_ratio",
      sb_algorithm = "rp",
      chosen_eval_metric = "rss",
      active_returns = TRUE,
      cov_matrix_benchmark = "IBOV",
      cov_estimation_method = "sample",
      early_stop = NULL,
      target_fwd_name = "fwd_premium_1m"),
    "exp_ret_score_tilt must be one of 'none', 'inner' or 'final'"
  )

  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      validation_sample_size = 0,
      huber_delta = 1,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      exp_ret_score_tilt = "all",
      exp_ret_score_tilt_eta = NULL,
      ridge_pen = NULL,
      macro_ridge_pen = NULL,
      micro_port_construction_method = NULL,
      macro_port_construction_method = NULL,
      signal_universe_m_df = signal_universe_m_df %>% dplyr::mutate(info_ratio = rnorm(dplyr::n())),
      backtest_returns_m_xts = backtest_returns_m_xts,
      benchmark_returns_m_xts =  xts::xts(data.frame(IBOV = rnorm(6), SMLL = rnorm(6), IDIV = rnorm(6)),
                                          order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-08-15"), by = "month")),
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = NULL,
      custom_objective = "max_info_ratio",
      sb_algorithm = "sw",
      chosen_eval_metric = "rss",
      active_returns = TRUE,
      cov_matrix_benchmark = "IBOV",
      cov_estimation_method = "sample",
      early_stop = NULL,
      target_fwd_name = "fwd_premium_1m"),
    "exp_ret_score_tilt must be provided only for 'rp' or 'hrp'"
  )

  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      validation_sample_size = 0,
      huber_delta = 1,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      exp_ret_score_tilt = "none",
      exp_ret_score_tilt_eta = 2,
      ridge_pen = NULL,
      macro_ridge_pen = NULL,
      micro_port_construction_method = NULL,
      macro_port_construction_method = NULL,
      signal_universe_m_df = signal_universe_m_df %>% dplyr::mutate(info_ratio = rnorm(dplyr::n())),
      backtest_returns_m_xts = backtest_returns_m_xts,
      benchmark_returns_m_xts =  xts::xts(data.frame(IBOV = rnorm(6), SMLL = rnorm(6), IDIV = rnorm(6)),
                                          order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-08-15"), by = "month")),
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = NULL,
      custom_objective = "max_info_ratio",
      sb_algorithm = "hrp",
      chosen_eval_metric = "rss",
      active_returns = TRUE,
      cov_matrix_benchmark = "IBOV",
      cov_estimation_method = "sample",
      early_stop = NULL,
      target_fwd_name = "fwd_premium_1m"),
    "exp_ret_score_tilt_eta should be NULL when exp_ret_score_tilt = 'none'"
  )

  signal_universe_m_df <- signal_universe_m_df %>% dplyr::mutate(point = 3)
  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      opt_method = "sample",
      validation_sample_size = 0,
      huber_delta = 1,
      exp_ret_score_tilt = NULL,
      exp_ret_score_tilt_eta = NULL,
      ridge_pen = NULL,
      macro_ridge_pen = NULL,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = backtest_returns_m_xts,
      benchmark_returns_m_xts =  xts::xts(data.frame(IBOV = rnorm(6), SMLL = rnorm(6), IDIV = rnorm(6)),
                                          order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-08-15"), by = "month")),
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = NULL,
      custom_objective = "max_point",
      sb_algorithm = "mvo",
      chosen_eval_metric = "rss",
      active_returns = TRUE,
      cov_matrix_benchmark = "IBOV",
      n_random_ports = "3",
      cov_estimation_method = "sample",
      early_stop = NULL,
      target_fwd_name = "fwd_premium_1m"),
    "Currently, 'opt_method' must be 'random'"
  )


  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      opt_method = "random",
      validation_sample_size = 0,
      huber_delta = 1,
      exp_ret_score_tilt = NULL,
      exp_ret_score_tilt_eta = NULL,
      ridge_pen = NULL,
      macro_ridge_pen = NULL,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = backtest_returns_m_xts,
      benchmark_returns_m_xts =  xts::xts(data.frame(IBOV = rnorm(6), SMLL = rnorm(6), IDIV = rnorm(6)),
                                          order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-08-15"), by = "month")),
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = NULL,
      custom_objective = "max_point",
      sb_algorithm = "mvo",
      chosen_eval_metric = "rss",
      active_returns = TRUE,
      cov_matrix_benchmark = "IBOV",
      n_random_ports = "3",
      random_ports_method = "sample",
      cov_estimation_method = "sample",
      early_stop = NULL,
      opt_objective = "ir",
      target_fwd_name = "fwd_premium_1m"),
    "opt_objective must be one of 'return', 'risk', 'sharpe'"
  )

  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      opt_method = "random",
      validation_sample_size = 0,
      huber_delta = 1,
      exp_ret_score_tilt = NULL,
      exp_ret_score_tilt_eta = NULL,
      ridge_pen = 2,
      macro_ridge_pen = NULL,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = backtest_returns_m_xts,
      benchmark_returns_m_xts =  xts::xts(data.frame(IBOV = rnorm(6), SMLL = rnorm(6), IDIV = rnorm(6)),
                                          order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-08-15"), by = "month")),
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = NULL,
      custom_objective = "max_point",
      sb_algorithm = "mvo",
      chosen_eval_metric = "rss",
      active_returns = TRUE,
      cov_matrix_benchmark = "IBOV",
      n_random_ports = "3",
      random_ports_method = "sample",
      cov_estimation_method = "sample",
      early_stop = NULL,
      opt_objective = "sharpe",
      n_resamples = 2,
      exp_ret_score_jitter = 0,
      cov_eigval_jitter = 0,
      target_port_m_df = NULL,
      target_fwd_name = "fwd_premium_1m"),
    "target_port_m_df must be provided when ridge_pen is not NULL"
  )

  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      opt_method = "random",
      validation_sample_size = 0,
      huber_delta = 1,
      exp_ret_score_tilt = NULL,
      exp_ret_score_tilt_eta = NULL,
      ridge_pen = 2,
      macro_ridge_pen = NULL,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = backtest_returns_m_xts,
      benchmark_returns_m_xts =  xts::xts(data.frame(IBOV = rnorm(6), SMLL = rnorm(6), IDIV = rnorm(6)),
                                          order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-08-15"), by = "month")),
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = NULL,
      custom_objective = "squared_error",
      sb_algorithm = "rp",
      chosen_eval_metric = "rss",
      active_returns = TRUE,
      cov_matrix_benchmark = "IBOV",
      micro_port_construction_method = NULL,
      macro_port_construction_method = NULL,
      n_random_ports = "3",
      random_ports_method = "sample",
      cov_estimation_method = "sample",
      early_stop = NULL,
      opt_objective = "sharpe",
      n_resamples = 2,
      exp_ret_score_jitter = 0,
      cov_eigval_jitter = 0,
      target_port_m_df = signal_universe_m_df %>% dplyr::rename(target_weights = point),
      target_fwd_name = "fwd_premium_1m"),
    "ridge_pen can only be used for 'mvo'"
  )

  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      opt_method = "random",
      validation_sample_size = 0,
      huber_delta = 1,
      exp_ret_score_tilt = NULL,
      exp_ret_score_tilt_eta = NULL,
      ridge_pen = 2,
      macro_ridge_pen = NULL,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = backtest_returns_m_xts,
      benchmark_returns_m_xts =  xts::xts(data.frame(IBOV = rnorm(6), SMLL = rnorm(6), IDIV = rnorm(6)),
                                          order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-08-15"), by = "month")),
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = NULL,
      custom_objective = "max_point",
      sb_algorithm = "mvo",
      chosen_eval_metric = "rss",
      active_returns = TRUE,
      cov_matrix_benchmark = "IBOV",
      n_random_ports = "3",
      random_ports_method = "sample",
      cov_estimation_method = "sample",
      early_stop = NULL,
      opt_objective = "sharpe",
      n_resamples = 2,
      exp_ret_score_jitter = 0,
      cov_eigval_jitter = 0,
      target_port_m_df = features_m_df,
      target_fwd_name = "fwd_premium_1m"),
    "all id's from signals_m_df after initial_buffer_period must have a correspondence in target_port_m_df"
  )


  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      opt_method = "random",
      validation_sample_size = 0,
      huber_delta = 1,
      exp_ret_score_tilt = NULL,
      exp_ret_score_tilt_eta = NULL,
      ridge_pen = 2,
      macro_ridge_pen = NULL,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = backtest_returns_m_xts,
      benchmark_returns_m_xts =  xts::xts(data.frame(IBOV = rnorm(6), SMLL = rnorm(6), IDIV = rnorm(6)),
                                          order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-08-15"), by = "month")),
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = NULL,
      custom_objective = "max_point",
      sb_algorithm = "mvo",
      chosen_eval_metric = "rss",
      active_returns = TRUE,
      cov_matrix_benchmark = "IBOV",
      n_random_ports = "3",
      random_ports_method = "sample",
      cov_estimation_method = "sample",
      early_stop = NULL,
      opt_objective = "sharpe",
      n_resamples = 2,
      exp_ret_score_jitter = 0,
      cov_eigval_jitter = 0,
      target_port_m_df = signal_universe_m_df %>%
        dplyr::rename(target_weights = point) %>%
        dplyr::select(-is_eligible),
      target_fwd_name = "fwd_premium_1m"),
    "weights in target_port_m_df should be between 0 and 1"
  )



  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      opt_method = "random",
      validation_sample_size = 0,
      huber_delta = 1,
      exp_ret_score_tilt = NULL,
      exp_ret_score_tilt_eta = NULL,
      ridge_pen = NULL,
      macro_ridge_pen = NULL,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = backtest_returns_m_xts,
      benchmark_returns_m_xts =  xts::xts(data.frame(IBOV = rnorm(6), SMLL = rnorm(6), IDIV = rnorm(6)),
                                          order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-08-15"), by = "month")),
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = NULL,
      custom_objective = "max_point",
      sb_algorithm = "mvo",
      chosen_eval_metric = "rss",
      active_returns = TRUE,
      cov_matrix_benchmark = "IBOV",
      n_random_ports = "3",
      random_ports_method = "sample",
      cov_estimation_method = "sample",
      early_stop = NULL,
      opt_objective = "sharpe",
      n_resamples = 2,
      exp_ret_score_jitter = 0,
      cov_eigval_jitter = 0,
      target_fwd_name = "fwd_premium_1m")
  )

  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      opt_method = "random",
      validation_sample_size = 0,
      huber_delta = 1,
      exp_ret_score_tilt = NULL,
      exp_ret_score_tilt_eta = NULL,
      ridge_pen = NULL,
      macro_ridge_pen = NULL,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = backtest_returns_m_xts,
      benchmark_returns_m_xts =  xts::xts(data.frame(IBOV = rnorm(6), SMLL = rnorm(6), IDIV = rnorm(6)),
                                          order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-08-15"), by = "month")),
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = NULL,
      custom_objective = "max_point",
      sb_algorithm = "mmaf",
      chosen_eval_metric = "rss",
      active_returns = TRUE,
      cov_matrix_benchmark = "IBOV",
      n_random_ports = "3",
      random_ports_method = "sample",
      cov_estimation_method = "sample",
      early_stop = NULL,
      opt_objective = "sharpe",
      n_resamples = 2,
      exp_ret_score_jitter = 1,
      macro_port_construction_method = "ew",
      macro_exp_ret_score_tilt = NULL,
      macro_exp_ret_score_tilt_eta = NULL,
      micro_port_construction_method = "mvo",
      mmaf_group_col = "sector",
      cov_eigval_jitter = 1,
      target_fwd_name = "fwd_premium_1m"),
    "groups_m_df must be provided and mmaf_group_col must be present in groups_m_df for 'mmaf'"
  )


  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      opt_method = "random",
      validation_sample_size = 0,
      huber_delta = 1,
      exp_ret_score_tilt = NULL,
      exp_ret_score_tilt_eta = NULL,
      ridge_pen = NULL,
      macro_ridge_pen = NULL,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = backtest_returns_m_xts,
      benchmark_returns_m_xts =  xts::xts(data.frame(IBOV = rnorm(6), SMLL = rnorm(6), IDIV = rnorm(6)),
                                          order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-08-15"), by = "month")),
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = NULL,
      custom_objective = "max_point",
      sb_algorithm = "mmaf",
      chosen_eval_metric = "rss",
      active_returns = TRUE,
      cov_matrix_benchmark = "IBOV",
      n_random_ports = "3",
      random_ports_method = "sample",
      cov_estimation_method = "sample",
      early_stop = NULL,
      opt_objective = "sharpe",
      n_resamples = 2,
      exp_ret_score_jitter = 1,
      macro_port_construction_method = "ew",
      macro_exp_ret_score_tilt = NULL,
      macro_exp_ret_score_tilt_eta = NULL,
      micro_port_construction_method = "mvo",
      mmaf_group_col = "theme",
      cov_eigval_jitter = 1,
      target_fwd_name = "fwd_premium_1m"),
    "groups_m_df must be provided and mmaf_group_col must be present in groups_m_df for 'mmaf'"
  )

  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      opt_method = "random",
      validation_sample_size = 0,
      huber_delta = 1,
      exp_ret_score_tilt = NULL,
      exp_ret_score_tilt_eta = NULL,
      ridge_pen = NULL,
      macro_ridge_pen = NULL,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = backtest_returns_m_xts,
      benchmark_returns_m_xts =  xts::xts(data.frame(IBOV = rnorm(6), SMLL = rnorm(6), IDIV = rnorm(6)),
                                          order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-08-15"), by = "month")),
      signal_themes_m_df = signal_themes_m_df,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = NULL,
      custom_objective = "max_point",
      sb_algorithm = "mmaf",
      chosen_eval_metric = "rss",
      active_returns = TRUE,
      cov_matrix_benchmark = "IBOV",
      n_random_ports = "3",
      random_ports_method = "sample",
      cov_estimation_method = "sample",
      early_stop = NULL,
      opt_objective = "sharpe",
      n_resamples = 2,
      mmaf_method = "top_down",
      exp_ret_score_jitter = 1,
      macro_port_construction_method = "ew",
      macro_exp_ret_score_tilt = NULL,
      macro_exp_ret_score_tilt_eta = NULL,
      micro_port_construction_method = "mvo",
      mmaf_group_col = "theme",
      top_down_proxy_port_method = "cw",
      cov_eigval_jitter = 1,
      target_fwd_name = "fwd_premium_1m"),
    "top_down_proxy_port_method must be one of 'ew', 'sw', 'cs', 'rp' or 'hrp'"
  )


  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      opt_method = "random",
      validation_sample_size = 0,
      huber_delta = 1,
      exp_ret_score_tilt = NULL,
      exp_ret_score_tilt_eta = NULL,
      ridge_pen = NULL,
      macro_ridge_pen = NULL,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = backtest_returns_m_xts,
      benchmark_returns_m_xts =  xts::xts(data.frame(IBOV = rnorm(6), SMLL = rnorm(6), IDIV = rnorm(6)),
                                          order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-08-15"), by = "month")),
      signal_themes_m_df = signal_themes_m_df,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = NULL,
      custom_objective = "max_point",
      sb_algorithm = "mmaf",
      chosen_eval_metric = "rss",
      active_returns = TRUE,
      cov_matrix_benchmark = "IBOV",
      n_random_ports = 1,
      random_ports_method = "sample",
      cov_estimation_method = "sample",
      early_stop = NULL,
      opt_objective = "win_money",
      n_resamples = 2,
      exp_ret_score_jitter = 1,
      macro_port_construction_method = "ew",
      macro_exp_ret_score_tilt = NULL,
      macro_exp_ret_score_tilt_eta = NULL,
      micro_port_construction_method = "mvo",
      mmaf_group_col = "theme",
      cov_eigval_jitter = 1,
      mmaf_method = "top_down",
      top_down_proxy_port_method = "ew",
      target_fwd_name = "fwd_premium_1m"),
    "opt_objective must be one of 'return', 'risk', 'sharpe'."
  )

  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      opt_method = "random",
      validation_sample_size = 0,
      huber_delta = 1,
      exp_ret_score_tilt = NULL,
      exp_ret_score_tilt_eta = NULL,
      ridge_pen = NULL,
      macro_ridge_pen = NULL,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = backtest_returns_m_xts,
      benchmark_returns_m_xts =  xts::xts(data.frame(IBOV = rnorm(6), SMLL = rnorm(6), IDIV = rnorm(6)),
                                          order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-08-15"), by = "month")),
      signal_themes_m_df = signal_themes_m_df,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = NULL,
      custom_objective = "max_point",
      sb_algorithm = "mmaf",
      chosen_eval_metric = "rss",
      active_returns = TRUE,
      cov_matrix_benchmark = "IBOV",
      n_random_ports = 1,
      random_ports_method = "sample",
      cov_estimation_method = "sample",
      early_stop = NULL,
      opt_objective = "sharpe",
      n_resamples = 2,
      exp_ret_score_jitter = 1,
      macro_port_construction_method = "cw",
      macro_exp_ret_score_tilt = NULL,
      macro_exp_ret_score_tilt_eta = NULL,
      micro_port_construction_method = "mvo",
      mmaf_group_col = "theme",
      cov_eigval_jitter = 1,
      mmaf_method = "top_down",
      top_down_proxy_port_method = "ew",
      target_fwd_name = "fwd_premium_1m"),
    "micro_port_construction_method and macro_port_construction_method cannot be 'cw' or 'cs' for signal_portfolios"
  )

  signal_universe_m_df <- signal_universe_m_df %>% dplyr::mutate(point = 3)
  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      opt_method = "random",
      validation_sample_size = 0,
      huber_delta = 1,
      exp_ret_score_tilt = NULL,
      exp_ret_score_tilt_eta = NULL,
      ridge_pen = NULL,
      macro_ridge_pen = NULL,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = backtest_returns_m_xts,
      benchmark_returns_m_xts =  xts::xts(data.frame(IBOV = rnorm(6), SMLL = rnorm(6), IDIV = rnorm(6)),
                                          order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-08-15"), by = "month")),
      signal_themes_m_df = signal_themes_m_df,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = NULL,
      custom_objective = "max_point",
      sb_algorithm = "mmaf",
      chosen_eval_metric = "rss",
      active_returns = TRUE,
      cov_matrix_benchmark = "IBOV",
      n_random_ports = 1,
      random_ports_method = "GRID",
      cov_estimation_method = "sample",
      early_stop = NULL,
      opt_objective = "sharpe",
      n_resamples = 2,
      exp_ret_score_jitter = 1,
      macro_port_construction_method = "sw",
      macro_exp_ret_score_tilt = NULL,
      macro_exp_ret_score_tilt_eta = NULL,
      micro_port_construction_method = "mvo",
      mmaf_group_col = "theme",
      cov_eigval_jitter = 1,
      mmaf_method = "top_down",
      top_down_proxy_port_method = "ew",
      target_fwd_name = "fwd_premium_1m"),
    "random_ports_method must be one of 'sample', 'simplex', 'grid'"
  )

  signal_universe_m_df <- signal_universe_m_df %>% dplyr::mutate(point = 3)
  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      opt_method = "random",
      validation_sample_size = 0,
      huber_delta = 1,
      exp_ret_score_tilt = NULL,
      exp_ret_score_tilt_eta = NULL,
      ridge_pen = NULL,
      macro_ridge_pen = NULL,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = backtest_returns_m_xts,
      benchmark_returns_m_xts =  xts::xts(data.frame(IBOV = rnorm(6), SMLL = rnorm(6), IDIV = rnorm(6)),
                                          order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-08-15"), by = "month")),
      signal_themes_m_df = signal_themes_m_df,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = NULL,
      custom_objective = "max_point",
      sb_algorithm = "mmaf",
      chosen_eval_metric = "rss",
      active_returns = TRUE,
      cov_matrix_benchmark = "IBOV",
      n_random_ports = 1,
      random_ports_method = "GRID",
      cov_estimation_method = "sample",
      early_stop = NULL,
      opt_objective = "RET",
      n_resamples = 2,
      exp_ret_score_jitter = 1,
      macro_port_construction_method = "sw",
      macro_exp_ret_score_tilt = NULL,
      macro_exp_ret_score_tilt_eta = NULL,
      micro_port_construction_method = "mvo",
      mmaf_group_col = "theme",
      cov_eigval_jitter = 1,
      mmaf_method = "top_down",
      top_down_proxy_port_method = "ew",
      target_fwd_name = "fwd_premium_1m"),
    "random_ports_method must be one of 'sample', 'simplex', 'grid'"
  )

  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      opt_method = "random",
      validation_sample_size = 0,
      huber_delta = 1,
      exp_ret_score_tilt = NULL,
      exp_ret_score_tilt_eta = NULL,
      ridge_pen = NULL,
      macro_ridge_pen = NULL,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = backtest_returns_m_xts,
      benchmark_returns_m_xts =  xts::xts(data.frame(IBOV = rnorm(6), SMLL = rnorm(6), IDIV = rnorm(6)),
                                          order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-08-15"), by = "month")),
      signal_themes_m_df = signal_themes_m_df,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = NULL,
      custom_objective = "max_point",
      sb_algorithm = "mmaf",
      chosen_eval_metric = "rss",
      active_returns = TRUE,
      cov_matrix_benchmark = "IBOV",
      n_random_ports = 1,
      random_ports_method = "sample",
      cov_estimation_method = "sample",
      early_stop = NULL,
      opt_objective = "sharpe",
      n_resamples = 2,
      exp_ret_score_jitter = 1,
      macro_port_construction_method = "sw",
      macro_exp_ret_score_tilt = NULL,
      macro_exp_ret_score_tilt_eta = NULL,
      micro_port_construction_method = "mvo",
      mmaf_group_col = "theme",
      cov_eigval_jitter = 1,
      mmaf_method = "top_down",
      top_down_proxy_port_method = "ew",
      concentration_constraint_policy = list(benchmark = "smll", max_abs_active_group_weight = 0.3),
      target_fwd_name = "fwd_premium_1m"),
    "concentration_constraint_policy's benchmark should be set to theme_sb or theme_ss"
  )


  expect_error(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      opt_method = "random",
      validation_sample_size = 0,
      huber_delta = 1,
      exp_ret_score_tilt = NULL,
      exp_ret_score_tilt_eta = NULL,
      ridge_pen = NULL,
      macro_ridge_pen = NULL,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = backtest_returns_m_xts,
      benchmark_returns_m_xts =  xts::xts(data.frame(IBOV = rnorm(6), SMLL = rnorm(6), IDIV = rnorm(6)),
                                          order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-08-15"), by = "month")),
      signal_themes_m_df = signal_themes_m_df,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = NULL,
      custom_objective = "max_point",
      sb_algorithm = "mmaf",
      chosen_eval_metric = "rss",
      active_returns = TRUE,
      cov_matrix_benchmark = "IBOV",
      n_random_ports = 1,
      random_ports_method = "sample",
      cov_estimation_method = "sample",
      early_stop = NULL,
      opt_objective = "sharpe",
      n_resamples = 2,
      exp_ret_score_jitter = 1,
      macro_port_construction_method = "sw",
      macro_exp_ret_score_tilt = NULL,
      macro_exp_ret_score_tilt_eta = NULL,
      micro_port_construction_method = "mvo",
      mmaf_group_col = "theme",
      cov_eigval_jitter = 1,
      mmaf_method = "top_down",
      top_down_proxy_port_method = "ew",
      concentration_constraint_policy = list(benchmark = "theme_ss", max_abs_active_group_weight = 0.3),
      target_fwd_name = "fwd_premium_1m"),
    "concentration_constraint_policy's benchmark should be present in signal_universe_m_df"
  )


})


test_that("check_inputs_sb_backtest shows warning when combining signals naively", {

  load(paste(test_path(),"/testdata/","artificial_signal_blending_obj.RData", sep =""))

  expect_warning(
    check_inputs_sb_backtest(
      features_m_df = features_m_df,
      target_m_df = target_m_df,
      split_method = "expanding",
      quantile_tau = 0.5,
      gsm_algorithm = "ols",
      training_sample_size = 4,
      validation_sample_size = 0,
      huber_delta = 1,
      exp_ret_score_tilt = NULL,
      exp_ret_score_tilt_eta = NULL,
      ridge_pen = NULL,
      macro_ridge_pen = NULL,
      macro_port_construction_method = NULL,
      macro_exp_ret_score_tilt = NULL,
      macro_exp_ret_score_tilt_eta = NULL,
      micro_port_construction_method = NULL,
      rebalancing_months = 9,
      hyper_grid_domain_list = NULL,
      n_iter = NULL,
      k_iter = NULL,
      tuning_method = NULL,
      signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = xts::xts(data.frame(Alpha = rnorm(6), Beta = rnorm(6), Gamma = rnorm(6)),
                                        order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-08-15"), by = "month")),
      benchmark_returns_m_xts =  xts::xts(data.frame(IBOV = rnorm(6), SMLL = rnorm(6), IDIV = rnorm(6)),
                                          order.by = seq.Date(as.Date("2001-03-15"), as.Date("2001-08-15"), by = "month")),
      signal_themes_m_df = NULL,
      concentration_constraint_policy = NULL,
      cov_matrix_sample_size = 3,
      custom_signal_weights_m_df = NULL,
      custom_objective = "squared_error",
      sb_algorithm = "ew",
      chosen_eval_metric = "rss",
      active_returns = "FALSE",
      cov_matrix_benchmark = "IBOV",
      cov_estimation_method = "sample",
      early_stop = NULL,
      target_fwd_name = "fwd_premium_1m"),
    "All signals are 'long' and sb_algorithm is 'ew'. Please check if this is intended."
  )


})






