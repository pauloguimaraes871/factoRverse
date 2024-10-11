#' Perform out-of-sample testing for ML Algorithms with walk-forward time series validation
#'
#' This function performs walk-forward validation for time series data using
#' a range of ML models. It supports hyperparameter tuning via random search, grid search,
#' or Bayesian optimization. The function divides the data into training, validation,
#' and testing samples, and iteratively refits the model at specified rebalancing dates.
#'
#' @param features_m_df A meta dataframe or data frame containing features, with columns: id, tickers, dates.
#' @param target_m_df A meta dataframe or data frame containing target variable(s), with corresponding dates. Columns should follow the format XXXX_number_m, where
#' XXXX is the name of the target variable, number is the amount of forward periods and m indicates periods are measured in months.
#' @param training_sample_size Number of observations to include in each training sample.
#' @param validation_sample_size Number of observations to include in each validation sample.
#' @param rebalancing_months Months (numeric) when model should be rebalanced.
#' @param target_fwd_name Name of the target variable in `target_m_df`.
#' @param ml_algorithm Choice of ml_algorithm: ols (Ordinary Least Squares), glmnet (Elastic Net), rf (Random Forest), xgb (eXtreme Gradient Boosting), and nn (Keras Neural Networks).
#' @param split_method Choice of split method (expanding or rolling).
#' @param hyper_grid_domain_list A named list containing hyperparameter definitions. The structure of this list depends on the specified tuning method:
#' \itemize{
#'   \item \strong{For grid search:} Must be a list of named vectors.
#'   \item \strong{For random search:} Must be a list of named lists, where each named list contains:
#'     \itemize{
#'       \item \code{distribution_choice}: A character string specifying the distribution (one of "normal", "uniform", "lognormal", "constant").
#'       \item \code{pars}: A named numeric vector of parameters corresponding to the chosen distribution.
#'       \item \code{value}: A numeric value (only present if \code{distribution_choice} is "constant").
#'     }
#'   \item \strong{For Bayesian optimization:} Must be a list of named numeric vectors, each of length 2, representing the boundaries for the hyperparameters.
#' }
#' @examples
#' # Example of creating hyper_grid_domain_list for random search
#' hyper_grid <- list(
#'   alpha = list(distribution_choice = "uniform", pars = c(min = 0, max = 1), value = NULL),
#'   lambda.min.ratio = list(distribution_choice = "uniform", pars = c(min = 0, max = 0.9), value = NULL)
#' )
#'
#' @param tuning_method Method for hyperparameter tuning: "random_search", "grid_search", or "bayesian_opt".
#' @param n_iter Number of iterations for random search.
#' @param acq Acquisition function for Bayesian optimization: "ucb", "ei", or "poi".
#' @param init_points Number of initial random points for Bayesian optimization.
#' @param k_iter Integer that specifies the number of times to sample eval_function at each Epoch during Bayesian optimization.
#' If running in parallel, set iters.k to a multiple of the number of cores. Must be lower and preferably a multiple of n_iter.
#' @param custom_objective Custom objective (double differentiable loss function) for xgboost and nn algorithms.
#' (current options are squared_error, absolute_error and (pseudo)-huber loss)
#' @param early_stop Sets a halting criteria to prevent overfitting in xgb and nn.
#' @param chosen_eval_metric Metric to optimize during tuning: "rss", "rmse", "cp", "mae", "mphe", "mpe", "mape", "hr", and "mb".
#' @param show_plots Logical, indicating whether to plot results (default is TRUE).
#' @param keras_architecture_parameters Named list, containing units (a vector containing number of neurons in each layer),
#' n_layers (number of layers), activation (a character vector of activation function at each layer), nn_optimizer (Adam or RMSProp), and
#' batch_norm_option (a logical vector indicating whether normalization after each respective layer should be performed).
#' @param huber_delta A single numeric value indicating the boundary that separates where the loss function turns from quadratic to linear.
#' @param quantile_tau A single numeric value indicating target quantile when calculating quantile loss.
#' @param verbose Logical, indicating whether to print progress messages (default is TRUE).
#' @param parallel Logical, indicating whether to run hyperparameter tuning in parallel (default is TRUE).
#'
#' @return List with various outputs including model predictions, errors, and validation metrics.
#'
#' @details
#' The function ensures all inputs are correctly formatted and performs checks to
#' validate the integrity of input data and parameters. It employs the glmnet package
#' for elastic net regularization, supporting both Lasso and Ridge regression.
#'
#' @section Running in Parallel:
#' By default, tuning_method %in% c("random_search", "grid_search") utilizes furrr::future_pmap, which means they can run according to the built-in backends
#' from the future package. Therefore, if the user does not specify a different evaluation strategy with future::plan(),
#' tuning will be done sequentially by default (equivalent to future::plan(sequential)). In this case, however,
#' random number generator will be set to RNGkind("L'Ecuyer-CMRG"), instead of R default (RNGkind("Mersenne-Twister")), making results
#' not reproducible regarding using purrr:pmap(). In order to run using R's default random number generator, set parallel = FALSE.
#' Using a different evaluation strategy (e.g., future::plan(multisession)) will tune hyperparameters asynchronously (in parallel).
#'
#' For tuning_method = "bayesian_opt", the ParBayesianOptimization::bayesOpt function runs in parallel by using foreach::foreach with the %dopar% operator.
#' Therefore, in this case, the user can either: (i) use doFuture::registerDoFuture(), in order to use the %dofuture% foreach adapter
#' (actually, in this case, doFuture::withDoRNG is used to turn %dopar% into %dorng% in order to use parallel-safe RNG), which allows
#' usage of backends from the future package or (ii) use parallel::makeCluster(), doParallel::registerDoParallel(), doParallel::clusterExport() and
#' doParallel::clusterEvalQ(), as exemplified by ParBayesianOptimization. If parallel = TRUE and neither strategy is being used,
#' code will result in error. Therefore, to run bayesian_opt synchronously, either use doFuture::registerDoFuture() with plan(sequential)
#' or set parallel = FALSE.
#'
#' Keras has some limitations when working in parallel, especially when using bayesian optimization as tuning method.
#'
#' @seealso
#' \code{\link{glmnet}}, \code{\link{ranger}}, \code{\link{xgboost}}, \code{\link{keras}}, \code{\link{time_series_split}}
#'
#' @export
ml_walk_forward_validation <- function(
    #Basic Objects Inputs
  features_m_df, target_m_df, training_sample_size, target_fwd_name,
  #Splits
  validation_sample_size = 0, rebalancing_months, split_method = "expanding",
  #Choice of ML algorithm
  ml_algorithm,
  #Loss/Eval Functions and Related
  custom_objective = "squared_error", chosen_eval_metric = NULL, huber_delta = 1, quantile_tau = 0.5,
  #Hyperparameter tuning Inputs
  hyper_grid_domain_list = NULL, tuning_method = NULL, n_iter = NULL, k_iter = NULL, acq = "ucb", init_points = NULL, early_stop = NULL,
  #Keras architecture Parameters
  keras_architecture_parameters = NULL,
  #Misc
  show_plots = TRUE, verbose = FALSE, parallel = TRUE
){

  #Measure time to run and run gc
  elapsed_time <- system.time({

    #Visible binding for global variables
    squared_error <- pseudo_huber_error <- quantile_error <- NULL

    #Get data from S4 object
      ##features_m_df
      if(is_meta_dataframe(features_m_df)){
        features_m_df <- features_m_df@data #Get features_m_df
      }
      ##target_m_df
      if(is_meta_dataframe(target_m_df)){
        target_m_df <- target_m_df@data #Get target_m_df
      }

    ################
    ##Check Parameters: This function will test whether inputs match format and current functionalities
    check_inputs_ml_wf_val(
      features_m_df = features_m_df, target_m_df = target_m_df, training_sample_size = training_sample_size, target_fwd_name = target_fwd_name,
      validation_sample_size = validation_sample_size, rebalancing_months = rebalancing_months, split_method = split_method, ml_algorithm = ml_algorithm,
      custom_objective = custom_objective, chosen_eval_metric = chosen_eval_metric, huber_delta = huber_delta, quantile_tau = quantile_tau,
      hyper_grid_domain_list = hyper_grid_domain_list, tuning_method = tuning_method, n_iter = n_iter, k_iter = k_iter, acq = acq,
      init_points = init_points, early_stop = early_stop, keras_architecture_parameters = keras_architecture_parameters, show_plots = show_plots,
      verbose = verbose, parallel = parallel
    )

    ################

    #Initial Setup: Making some changes to metrics if needed and displaying initial setup
    adjusted_metrics <- translate_metrics(ml_algorithm = ml_algorithm, chosen_eval_metric = chosen_eval_metric,
                                          custom_objective = custom_objective, early_stop = early_stop, huber_delta = huber_delta,
                                          verbose = verbose)

    #Pass adjusted metrics
    custom_objective_translated <- adjusted_metrics$custom_objective_translated
    chosen_eval_metric_translated <- adjusted_metrics$chosen_eval_metric_translated
    chosen_eval_metric <- adjusted_metrics$chosen_eval_metric

    #Prints for initial setup
    if(verbose == TRUE){
      cat(crayon::green(paste(ml_algorithm, "chosen as predictive algorithm")))
      cat("\n")
      print(paste(custom_objective, "chosen as custom objective"), quote = FALSE)
      print(paste(chosen_eval_metric, "chosen as eval metric for tuning"), quote = FALSE)
    } else {}

    ##################

    ###Init objects###
    ##################
    #Extract dates
    dates_m_vector <- unique(as.Date(features_m_df$dates, format = "%Y-%m-%d")) #coerce just to be sure
    dates_m_vector <- dates_m_vector[order(dates_m_vector)] #Re-order ascending just to be sure
    #Takes column corresponding to specific target
    target_vector <- target_m_df[, which(colnames(target_m_df) == target_fwd_name)]
    target_fwd <- as.numeric(gsub(".*?([0-9]+).*", "\\1", target_fwd_name))

    #Print for target
     if(verbose)   cat("Predicting a", target_fwd, "months ahead target: ", target_fwd_name, "\n")

    #Testing Sample Size
    testing_sample_size <- length(dates_m_vector) - training_sample_size - validation_sample_size #calculate testing sample size

    #Rebalancing Dates
    dates_testing_sample <- dates_m_vector[(training_sample_size + validation_sample_size):
                                             (training_sample_size + validation_sample_size + testing_sample_size)] #These are dates inside testing sample

    first_rebalance_date <- min(dates_testing_sample) #Get first rebalancing date
    rebalance_dates <- unique( #Unique is to eliminate repeated dates, in case month of first_rebalance_date is a rebalancing month
      c(first_rebalance_date, dates_testing_sample[which(lubridate::month(dates_testing_sample) %in% rebalancing_months)]) #Dates corresponding to rebalancing_months
    )
    rebalance_dates <- rebalance_dates[order(rebalance_dates)] #Re-order

    #Number of rebalancing months
    n_rebalance_months <- length(rebalance_dates)

    #Last rebalance date
    last_rebalance_date <- max(rebalance_dates)
    #Time expanding validation
    if(ml_algorithm != "ols"){
      #Store hyperparameters choice (model complexity)
      #Store validation chosen eval
      chosen_eval_metric_validation <- list()
      #Store validation eval
      validation_eval_metrics_hyper_choice <- data.frame(
        rss = as.vector(rep(NA, n_rebalance_months)), #R2
        cp = as.vector(rep(NA, n_rebalance_months)), #CP
        rmse = as.vector(rep(NA, n_rebalance_months)), #Root Mean Squared Error
        mae = as.vector(rep(NA, n_rebalance_months)), #Mean Absolute Error
        mphe = as.vector(rep(NA, n_rebalance_months)), #Mean Pseudo huber
        mpe = as.vector(rep(NA, n_rebalance_months)), #Mean Pinball Error
        mape = as.vector(rep(NA, n_rebalance_months)), #Mean Absolute Percentage Error
        hr = as.vector(rep(NA, n_rebalance_months)), #Hit Rate
        mb = as.vector(rep(NA, n_rebalance_months)), #Mean Bias
        row.names = rebalance_dates
      )

      #Store hyper_choice_df based on existence of early stop and best_lam
      hyper_choice_df <- as.data.frame(matrix(NA, nrow = n_rebalance_months, ncol = length(hyper_grid_domain_list)))
      rownames(hyper_choice_df) <- rebalance_dates #Set rownames as rebalance dates
      colnames(hyper_choice_df) <- names(hyper_grid_domain_list) #Set colnames as hyperparameters
      #Add best-lam and best-iteration
      hyper_choice_df$best_lam <- if(ml_algorithm == "glmnet") NA
      hyper_choice_df$best_iteration <- if(!is.null(early_stop)) NA

    } else {}

    #Time expanding test
    #Store test eval
    oos_testing_eval_metrics <- data.frame(
      rss = as.vector(rep(NA, testing_sample_size + 1)), #+1 bco first date is also a testing date
      cp = as.vector(rep(NA, testing_sample_size + 1)),
      rmse = as.vector(rep(NA, testing_sample_size + 1)),
      mae = as.vector(rep(NA, testing_sample_size + 1)),
      mphe = as.vector(rep(NA, testing_sample_size + 1)),
      mpe = as.vector(rep(NA, testing_sample_size + 1)),
      mape = as.vector(rep(NA, testing_sample_size + 1)),
      hr = as.vector(rep(NA, testing_sample_size + 1)),
      mb = as.vector(rep(NA, testing_sample_size + 1))
    )

    rownames(oos_testing_eval_metrics) <- dates_testing_sample


    #Prediction, error and Y objects
    oos_prediction_list <- list() #initialize prediction list. Each element will be a vector of predictions for that date
    oos_error_list <- list() #Initialize error list.
    oos_y_list <- list() #Initialize y list.


    ##################

    ###Start Fitting###
    ##################
    #Loop through
    for(d in (training_sample_size + validation_sample_size):(training_sample_size + validation_sample_size + testing_sample_size)){
      #Get current date
      current_date <- dates_m_vector[d]
      #Check if it's a rebalancing month
      if((lubridate::month(current_date) %in% rebalancing_months) || d == (training_sample_size + validation_sample_size)){
        #Print refitting message
        if(verbose == TRUE){
          cat("\n")
          cat(crayon::yellow(paste("Starting model rebalancing at:", current_date)))
          cat("\n")
        } else {}

        ###Estimate model
        #In case of ML models, there is a validation split to search for hyperparameters. Therefore, the sample is split in three

        ts_splits <- time_series_split(
          #Data
          features_m_df = features_m_df, target_m_df = target_m_df, target_fwd = target_fwd, target_fwd_name = target_fwd_name,
          #Dates
          current_date = current_date, dates_m_vector = dates_m_vector,
          #Splits
          training_sample_size = training_sample_size, split_method = split_method, validation_sample_size = validation_sample_size
        )

        #OLS warning
        if(verbose == TRUE & ml_algorithm == "OLS"){
          cat("OLS chosen as ml_algorithm. Data will be split only in training and test sets")
        }

        if(ml_algorithm != "ols"){ #If ml_algorithm is OLS, one just need to fit model do traininig + validation samples

          #Training Sample
          #################

          #Get training sample objects
          full_data_training_sample_clean <- ts_splits$training$full_data_training_sample_clean #Clean means no id colms

          ###################

          #Validation Sample
          #################

          #Get validation sample objects
          features_validation_sample <- ts_splits$validation$features_validation_sample
          target_validation_sample <- ts_splits$validation$target_validation_sample

          ##################

          ###Hyperparameter tuning!
          #########################

          #Set eval_function: function to fit model to training data and calculate eval metrics to validation function
          eval_function <- set_eval_function(
            #Choice of ml algo
            ml_algorithm = ml_algorithm,
            #Choice of tuning method
            tuning_method = tuning_method
          )


          #Hyper tune according to chosen tuning_method
          #Print
          if(verbose == TRUE){
            cat(paste("Starting", tuning_method, "hyperparameter tuning at ", current_date))
            cat("\n")
          }

          hyper_tune_results <- hyper_tune(
            #General Parameters
            tuning_method = tuning_method, ml_algorithm = ml_algorithm, target_fwd_name = target_fwd_name,
            #Data
            full_data_training_sample_clean = full_data_training_sample_clean,
            features_validation_sample = features_validation_sample, target_validation_sample = target_validation_sample,
            #Eval Function and custom obj
            eval_function = eval_function, custom_objective_translated = custom_objective_translated,
            #Early Stop
            chosen_eval_metric_translated = chosen_eval_metric_translated, early_stop = early_stop,
            #Chosen eval metric
            chosen_eval_metric = chosen_eval_metric, huber_delta = huber_delta, quantile_tau = quantile_tau,
            #Grid/Random Searches
            hyper_grid_domain_list = hyper_grid_domain_list, n_iter = n_iter,
            #Bayesian Optimization
            init_points = init_points, k_iter = k_iter, acq = acq,
            #Keras Parameters
            keras_architecture_parameters = keras_architecture_parameters,
            #Parallelization
            parallel = parallel,
            #Verbose
            verbose = verbose
          )

          ###Store results

          #Fill chosen_eval_metric_validation
          chosen_eval_metric_validation[[which(rebalance_dates == current_date)]] <- #Get correct position for list
            hyper_tune_results$chosen_eval_metric_validation_current_date

          #Get Optimal Hypers and fill hyper_choice_df
          optimal_hyper <- hyper_tune_results$optimal_hyper
          hyper_choice_df[paste(current_date),] <-  #Get the row corresponding to the rebalancing date
            optimal_hyper[names(hyper_choice_df)] #Replace hyper_choice_df with correct order

          #Fill validation_eval_metrics_hyper_choice
          validation_eval_metrics_hyper_choice[paste(current_date),] <-
            hyper_tune_results$validation_eval_metrics_hyper_choice_current_date[,colnames(validation_eval_metrics_hyper_choice)] #Take rights columns




          ###################
        } else {}

        #Refitting
        ###################


        #Refit new model using data from d - target_fwd
        features_m_refit <- ts_splits$refit$features_m_refit #Subset
        target_m_refit <- ts_splits$refit$target_m_refit #Subset
        full_data_m_refit_clean <- ts_splits$refit$full_data_m_refit_clean #Full data


        #(RE)Fit
        refit_model <- switch(ml_algorithm,
                              ols = stats::lm(paste(target_fwd_name,'~.'), data = full_data_m_refit_clean),

                              glmnet::glmnet(features_m_refit[,-c(1:3)], target_m_refit, #Features and target
                                             #Hyperparameters
                                             alpha = optimal_hyper["alpha"],
                                             lambda.min.ratio = optimal_hyper["lambda.min.ratio"],
                                             verbose = verbose
                              ),

                              rf = ranger::ranger(paste(target_fwd_name,'~.'), data = janitor::clean_names(full_data_m_refit_clean), #Features and target
                                                  #Hyperparameters
                                                  mtry = optimal_hyper["mtry"] * (ncol(full_data_m_refit_clean) - 1),
                                                  num.trees = optimal_hyper["num.trees"],
                                                  max.depth = optimal_hyper["max.depth"],
                                                  min.bucket = optimal_hyper["min.bucket"],
                                                  verbose = verbose
                              ),

                              xgb = xgboost::xgb.train(data = xgboost::xgb.DMatrix(data = as.matrix(features_m_refit[,-c(1:3)]), #Features and target
                                                                                   label = target_m_refit),
                                                       objective = custom_objective_translated,
                                                       huber_slope = huber_delta,
                                                       #quantile_alpha = quantile_tau,
                                                       #Hyperparameters
                                                       min_child_weight = optimal_hyper["min_child_weight"],
                                                       max_depth = round(optimal_hyper["max_depth"],0),
                                                       subsample = optimal_hyper["subsample"],
                                                       colsample_bytree = optimal_hyper["colsample_bytree"],
                                                       eta = optimal_hyper["eta"],
                                                       alpha = optimal_hyper["alpha"],
                                                       gamma = optimal_hyper["gamma"],
                                                       nrounds = if(is.null(early_stop)){
                                                         c(optimal_hyper["nrounds"])
                                                       } else {
                                                         c(optimal_hyper["best_iteration"])
                                                       },
                                                       verbose = verbose
                              ),

                              nn = fit_keras_model(features_matrix_train_clean = features_m_refit[,-c(1:3)], #Feature
                                                   target_vector_train = target_m_refit, #Target
                                                   custom_objective = custom_objective_translated, #No need for switch
                                                   huber_slope = huber_delta, #Huber loss
                                                   chosen_eval_metric_translated = chosen_eval_metric_translated, #Is this really necessary?

                                                   #Keras Parameters
                                                   #Architecture
                                                   keras_architecture_parameters = keras_architecture_parameters,

                                                   #Hyperparameters
                                                   #Training
                                                   number_of_epochs = if(is.null(early_stop)){
                                                     c(optimal_hyper["number_of_epochs"])
                                                   } else {
                                                     c(optimal_hyper["best_iteration"])
                                                   },
                                                   size_of_batch = optimal_hyper["size_of_batch"],
                                                   lr = optimal_hyper["lr"],

                                                   #Regularization
                                                   regularizer_l1 = optimal_hyper["regularizer_l1"],
                                                   regularizer_l2 = optimal_hyper["regularizer_l2"],
                                                   droprate = optimal_hyper["droprate"],


                                                   verbose = verbose
                              )$model_nn #This is a wrapper for keras
        )

        ###################

      }

      #Prediction
      ############
      #d stands for the date in which the features are being calculated. Therefore, in d, we have new features, but we still don't have the target, as it is in future.
      d_ref <- which(as.Date(features_m_df$dates,  format = "%Y-%m-%d") == current_date) #What references correspond to this date?
      #Subsets for date
      target_vector_ref <- target_vector[d_ref] #targets for new date
      features_m_d_ref <- features_m_df[d_ref,] #features for new date.
      #Reference for input in testing list
      testing_lists_ref <- d - training_sample_size - validation_sample_size + 1

      #Make predictions
      oos_prediction_list[[testing_lists_ref]] <- predict_ml_model(
        #Ml algo and model
        ml_algorithm = ml_algorithm, refit_model = refit_model,
        #Best lam in case of glmnet
        best_lam = if(ml_algorithm == "glmnet") as.numeric(optimal_hyper["best_lam"]) else NULL,
        #New features to predict
        new_features_m_d_ref = features_m_d_ref)

      names(oos_prediction_list[[testing_lists_ref]]) <- features_m_d_ref$tickers #Rename

      #Inform targets
      oos_y_list[[testing_lists_ref]] <- as.numeric(target_vector_ref)
      names(oos_y_list[[testing_lists_ref]]) <- features_m_d_ref$tickers  #Rename

      #Calculate eval metrics and error on testing sample
      testing_metrics <- calculate_eval_metrics(pred = oos_prediction_list[[testing_lists_ref]], target = oos_y_list[[testing_lists_ref]],
                                                huber_delta = huber_delta, quantile_tau = quantile_tau, chosen_eval_metric = chosen_eval_metric, return_error = TRUE)

      #Fill error
      oos_error_list[[testing_lists_ref]] <- as.numeric(testing_metrics$error) #Calculate error
      names(oos_error_list[[testing_lists_ref]]) <- features_m_d_ref$tickers  #Rename


      #Test Eval Metrics
      oos_testing_eval_metrics[testing_lists_ref,] <- testing_metrics$df_eval_metrics[,colnames(oos_testing_eval_metrics)]


    }

    #################

    ###Init Results List
    ##################

    #initialize result list
    result_list <- list()

    #OOS Prediction List
    result_list[[1]] <- oos_prediction_list
    names(result_list[[1]]) <- dates_testing_sample #Rename

    #OOS Error List
    result_list[[2]] <- oos_error_list
    names(result_list[[2]]) <- dates_testing_sample #Rename

    #OOS Y List
    result_list[[3]] <- oos_y_list
    names(result_list[[3]]) <- dates_testing_sample #Rename

    #OOS Testing Eval-metrics
    result_list[[4]] <- oos_testing_eval_metrics

    #Model
    result_list[[5]] <- refit_model

    if(!ml_algorithm == "ols"){

      #Validation eval for all hyperparameters
      names(chosen_eval_metric_validation) <- rebalance_dates #Change names
      result_list[[6]] <- chosen_eval_metric_validation
      #Hyper_choice
      result_list[[7]] <- hyper_choice_df

      #Validation eval-metrics
      result_list[[8]] <- validation_eval_metrics_hyper_choice

      #Fill names
      names(result_list) <- c("oos_prediction_list", "oos_error_list", "oos_y_list", "oos_testing_eval_metrics", "final_model",
                              "chosen_eval_metric_validation","best_hyperparameters", "validation_eval_metrics_hyper_choice") #ML Specific

    } else {
      #Fill names
      names(result_list) <- c("oos_prediction_list", "oos_error_list", "oos_y_list", "oos_testing_eval_metrics", "final_model")
    }

    #PLOTS
    ###############
    plots_list <- plot_ml_walk_forward_validation(
      #Eval metrics
      oos_testing_eval_metrics = oos_testing_eval_metrics, validation_eval_metrics_hyper_choice = validation_eval_metrics_hyper_choice,
      #Hyper choice and chosen eval metric
      hyper_choice_df = hyper_choice_df, chosen_eval_metric = chosen_eval_metric, chosen_eval_metric_validation = chosen_eval_metric_validation,
      #Backtest parameters
      ml_algorithm = ml_algorithm, rebalance_dates = rebalance_dates, show_plots = show_plots
    )

    ###############




  })

  #Print elapsed time
  print(elapsed_time)

  #Add plots
  result_list$plots <- plots_list

  #Metadata
  result_list$metadata <- list(
    #Algo
    ml_algorithm = ml_algorithm,
    custom_objective = custom_objective,
    #Dates
    dates_covered = dates_m_vector,
    n_dates = length(dates_m_vector),
    training_sample_size = training_sample_size,
    validation_sample_size = validation_sample_size,
    testing_sample_size = testing_sample_size,
    dates_testing_sample = dates_testing_sample,
    first_rebalance_date = first_rebalance_date,
    rebalance_dates = rebalance_dates,
    split_method = split_method,
    #Stocks
    ids = features_m_df$id,
    nobs = length(features_m_df$id),
    tickers = unique(features_m_df$tickers),
    n_stocks = length(unique(features_m_df$tickers)),
    #Target
    target_fwd_name = target_fwd_name,
    target_fwd = target_fwd,
    #Features
    features = colnames(features_m_df[,-c(1:3)]),
    #Tuning
    tuning_method = tuning_method,
    n_iter = n_iter,
    k_iter = k_iter,
    acq = acq,
    init_points = init_points,
    hyper_grid_domain_list = hyper_grid_domain_list,
    chosen_eval_metric = chosen_eval_metric,
    huber_delta = huber_delta,
    quantile_tau = quantile_tau,
    early_stop = early_stop,
    #Keras
    keras_architecture_parameters = keras_architecture_parameters,
    #Performance
    completion_time = Sys.time(),
    elapsed_time = elapsed_time,
    parallel = parallel,
    #Call
    call = match.call()
  )

  #Return List
  return(result_list)


}
