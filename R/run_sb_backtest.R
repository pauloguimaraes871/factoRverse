#' Run Signal Blending Backtest
#'
#' The `run_sb_backtest` function performs out-of-sample testing for a range of signal-blending algorithms (including machine-learning),
#' using walk-forward time series validation.
#' It supports many algorithms and can handle different configurations for objective functions and tuning strategies through S4 objects.
#' The function divides the data into training, validation, and testing samples, and iteratively refits the model at specified rebalancing dates.
#'
#' @param features_m_df A meta_dataframe containing features.
#' @param target_m_df A meta_dataframe containing target variable(s), with corresponding dates. Columns should follow the format XXXX_number_m, where
#' XXXX is the name of the target variable, number is the amount of forward periods and m indicates periods are measured in months.
#' @param config An object specifying the backtest configuration. Can be an `sb_backtest_config` or `sb_metabacktest_config` object.
#' @param verbose Logical vector, indicating whether to print progress messages (default is TRUE).
#' @param parallel Logical vector, indicating whether to run  hyperparameter tuning in parallel (default is TRUE).
#' @param ... Additional arguments (not used in this method).
#'
#' @return An object of class `sb_backtest_results` or `sb_metabacktest_results` with various outputs including model predictions, errors, and validation metrics.
#'
#' @details
#' The function ensures all inputs are correctly formatted and performs checks to validate the integrity of input data and parameters. It employs the glmnet package
#' for elastic net regularization, supporting both Lasso and Ridge regression.
#'
#' @section Running in Parallel:
#' The function supports parallel execution for runing multiple ml backtests and for hyperparameter tuning, both using the future package.
#'
#' The method for `sb_metabacktest_config` is basically a wrapper for `sb_backtest_config` method, which is called iteratively for each configuration,
#' and possibly run in parallel.
#'
#' By default, the method for `sb_metabacktest_config` and, individually, the method for `sb_backtest_config` when tuning_method %in% c("random_search", "grid_search"),
#' utilizes furrr::future_pmap, which means they can run according to the built-in backends from the future package. Therefore, if the user does not specify a different evaluation strategy with future::plan(),
#' tuning will be done sequentially by default (equivalent to future::plan(sequential)). In this case, however,
#' random number generator will be set to RNGkind("L'Ecuyer-CMRG"), instead of R default (RNGkind("Mersenne-Twister")), making results
#' not reproducible regarding using purrr:pmap(). In order to run using R's default random number generator, set parallel = FALSE.
#' Using a different evaluation strategy (e.g., future::plan(multisession)) will tune hyperparameters asynchronously (in parallel).
#'
#' For tuning_method = "bayesian_opt", under the application in `sb_backtest_config` method, the ParBayesianOptimization::bayesOpt function runs in parallel by using foreach::foreach with the %dopar% operator.
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
#' \code{\link{glmnet}}, \code{\link{ranger}}, \code{\link{xgboost}}, \code{\link{keras}}, \code{\link{time_series_split},
#' \code{\link{sb_backtest_config}}, \code{\link{sb_metabacktest_config}}
#'
#' @export
setGeneric("run_sb_backtest", function(features_m_df, target_m_df, target_fwd_name, config, ...) standardGeneric("run_sb_backtest"))


#' @describeIn run_sb_backtest Runs a backtest with a single configuration.
#' @param config An `sb_backtest_config` object containing the configuration for the backtest.
#' @examples
#' # Assuming features_m_df and target_m_df are meta_dataframe objects
#' # and config is an `sb_backtest_config` object
#' result <- run_sb_backtest(features_m_df, target_m_df, config, training_sample_size = 30, rebalacing_months = c(6,12), verbose = TRUE, parallel = TRUE)
#' @export
setMethod("run_sb_backtest",
          signature(features_m_df = "meta_dataframe", target_m_df = "meta_dataframe", target_fwd_name = "character",
                    config = "sb_backtest_config"),

          function(features_m_df, target_m_df, target_fwd_name, config, verbose = TRUE, parallel = TRUE) {


            #Assign default values for internal function
            ###########################
            split_method <- "expanding"
            validation_sample_size <- 0
            custom_objective <- "squared_error"
            chosen_eval_metric <- NULL
            huber_delta <- 1
            quantile_tau <- 0.5
            hyper_grid_domain_list <- NULL
            tuning_method <- NULL
            n_iter <- NULL
            k_iter <- NULL
            acq <- "ucb"
            init_points <- NULL
            early_stop <- NULL
            keras_architecture_parameters <- NULL

            ##Set missing values to TRUE
            if(missing(verbose)){
              verbose <- TRUE
            }
            if(missing(parallel)){
              parallel <- TRUE
            }

            ###########################

            #Get data from S4 objects
            ###########################
            ##features_m_df
            features_workflow <- features_m_df@workflow #Get workflow
            features_object_name <- features_m_df@meta_dataframe_name #Get mdf name
            features_m_df <- features_m_df@data #Get features_m_df

            ##target_m_df
            target_workflow <- target_m_df@workflow #Get workflow
            target_object_name <- target_m_df@meta_dataframe_name #Get mdf name
            target_m_df <- target_m_df@data #Get target_m_df


            ##Get general Information from config
            sb_algorithm <- config@sb_algorithm #Get sb_algorithm
            training_sample_size <- config@training_sample_size #Get training sample size
            rebalancing_months <- config@rebalancing_months #Get rebalancing months
            split_method <- config@split_method #Get split method
            custom_objective <- config@custom_objective #Get custom_objective
            keras_architecture_parameters <- if(sb_algorithm == "nn") as.list(config@keras_architecture_parameters) #Get keras_architecture_parameters
            huber_delta <- config@huber_delta #Get huber_delta
            quantile_tau <- config@quantile_tau #Get quantile_tau

            ##Tuning Strategy
            if(!sb_algorithm %in% c("ols", "ew", "sw", "rp", "mto")){
              tuning_strategy <- config@tuning_strategy #Get tuning strategy
              ###Get objects inside tuning strategy
              tuning_method <- tuning_strategy@tuning_method #Get tuning method
              validation_sample_size <- tuning_strategy@validation_sample_size #Get validation sample size
              chosen_eval_metric <- tuning_strategy@chosen_eval_metric #Get chosen eval metric
              hyper_grid_domain_list <- tuning_strategy@hyper_grid_domain@hyperparameter_list #Get hyper_grid_domain_list
              early_stop <- tuning_strategy@early_stop #Get early_stop

              ###Random search
              if(tuning_method == "random_search"){
                n_iter <- tuning_strategy@n_iter #Get n_iter
              }

              ###Bayesian Opt
              if(tuning_method == "bayesian_opt"){
                n_iter <- tuning_strategy@n_iter #Get n_iter
                acq <- tuning_strategy@acq #Get acq
                k_iter <- tuning_strategy@k_iter #Get k_iter
                init_points <- tuning_strategy@init_points #Get init_points
              }
            }
            ###########################

            #Run ML Backtest
            ###########################
            sb_backtest_results <- run_sb_backtest_internal(
              features_m_df = features_m_df, target_m_df = target_m_df, sb_algorithm = sb_algorithm,
              target_fwd_name = target_fwd_name, custom_objective = custom_objective,
              keras_architecture_parameters = keras_architecture_parameters,
              huber_delta = huber_delta, quantile_tau = quantile_tau,
              tuning_method = tuning_method,
              split_method = split_method, validation_sample_size = validation_sample_size,
              chosen_eval_metric = chosen_eval_metric, hyper_grid_domain_list = hyper_grid_domain_list,
              early_stop = early_stop, n_iter = n_iter, acq = acq, k_iter = k_iter, init_points = init_points,
              training_sample_size = training_sample_size, rebalancing_months = rebalancing_months, verbose = verbose,
              parallel = parallel
            )
            ###########################

            #Adjust ML Backtest WF
            ###########################

            #Add workflows, config_name and objects for target and features
            ###Target
            sb_backtest_results@sb_backtest_workflow$target_object_name <- target_object_name
            sb_backtest_results@sb_backtest_workflow$target_workflow <- target_workflow

            ###Features
            sb_backtest_results@sb_backtest_workflow$features_object_name <- features_object_name
            sb_backtest_results@sb_backtest_workflow$features_workflow <- features_workflow

            ###IDs
            sb_backtest_results@sb_backtest_workflow$config_name <- config@config_name
            sb_backtest_results@sb_backtest_workflow$backtest_identifier <-
              paste0("c:",config@config_name, "_f:", features_object_name, "_t:", target_object_name,"-",target_fwd_name)
            sb_backtest_results@backtest_identifier <- sb_backtest_results@sb_backtest_workflow$backtest_identifier

            ###Call
            sb_backtest_results@sb_backtest_workflow$call <- sys.call(-2)

            return(sb_backtest_results)

          }
)



#' @describeIn run_sb_backtest Run Signal Blending Meta-Backtest
#' @description Runs signal blending backtests for base learners and a meta learner using the provided configurations.
#' Users can provide their own base learner results or have the function compute them internally.
#' @param config A `sb_metabacktest_config` object containing a meta learner configuration and multiple individual configurations.
#' @param winsorize_predictions Logical; if \code{TRUE}, winsorizes the base learners' predictions before passing them to the meta learner. Default is \code{TRUE}.
#' @param winsorization_probs Numeric vector of length 2 specifying the lower and upper quantiles for winsorization. Default is \code{c(0.025, 0.975)}.
#' @param normalize_predictions Logical; if \code{TRUE}, normalizes the base learners' predictions before passing them to the meta learner. Default is \code{TRUE}.
#' @param features_passthrough A character vector indicating which features from \code{features_m_df} are to be passed through to the meta learner. Should correspond to column names in \code{features_m_df}.
#'   Alternatively, if \code{'all'}, all features are passed through. If \code{'none'}, no features are passed through. Default is \code{'none'}.
#' @return An \code{sb_metabacktest_results} object containing the results of the meta-backtest.
#' @examples
#' # Assuming features_m_df and target_m_df are meta_dataframe objects
#' # and meta_config is an sb_metabacktest_config object
#' results_list <- run_sb_backtest(features_m_df, target_m_df, training_sample_size = 30, rebalacing_months = c(6,12), meta_config)
#' @export
setMethod("run_sb_backtest",
          signature(features_m_df = "meta_dataframe", target_m_df = "meta_dataframe", target_fwd_name = "character",
                    config = "sb_metabacktest_config"),

          function(features_m_df, target_m_df, target_fwd_name, config, verbose = TRUE, parallel = TRUE,
                   winsorize_predictions = TRUE, winsorization_probs = c(0.025, 0.975), normalize_predictions = TRUE,
                   features_passthrough = "none") {

            ## Initial Preparations
            #######################

            if (is.null(config@base_sb_backtest_results)) {
              # Run base SB backtests
              base_sb_backtest_configs <- config@base_sb_backtest_configs

              ###Check for name uniqueness
              if(length(unique(sapply(base_sb_backtest_configs, function(x) x@config_name))) != length(base_sb_backtest_configs)){
                stop("Base SB backtest configurations must have unique names.")
              }

              #Run Individual Backtests
              base_sb_backtest_results_list <- run_base_sb_backtests(
                features_m_df = features_m_df,
                target_m_df = target_m_df,
                target_fwd_name = target_fwd_name,
                base_sb_backtest_configs = base_sb_backtest_configs,
                verbose = verbose,
                parallel = parallel
              )

            } else {
              base_sb_backtest_results_list <- config@base_sb_backtest_results
              #Check if is right format
              if (all(sapply(base_sb_backtest_results_list, function(x) class(x)) != "sb_backtest_results")) {
                stop("base_sb_backtest_results must be a list of sb_backtest_results objects.")
              }
              # Use provided base_sb_backtest_results_list
              if (verbose == TRUE) {
                cat(crayon::green("Using provided base ML backtest results.\n"))
              }
            }

            # Ensure base_sb_backtest_results_list is correctly named
            names(base_sb_backtest_results_list) <- sapply(base_sb_backtest_results_list, function(x) x@backtest_identifier)
            base_sb_backtest_configs_names <- sapply(base_sb_backtest_results_list, function(x) x@sb_backtest_workflow$config_name)

            ###Check for name uniqueness
            if(length(unique(base_sb_backtest_configs_names)) != length(base_sb_backtest_configs_names)){
              stop("Base ML backtest configurations must have unique names.")
            }

            for (i in 1:length(base_sb_backtest_results_list)) {
              base_sb_backtest_results_list[[i]]@sb_backtest_workflow$config_name <- base_sb_backtest_configs_names[i]
            }

            #######################

            # Generate oos_predictions_m_df and adapt target_m_df
            #######################
            # Create base object
            oos_predictions_m_df <- convert_oos_predictions_list_to_m_df(base_sb_backtest_results_list,
                                                                         winsorize_predictions = winsorize_predictions, winsorization_probs = winsorization_probs, # Winsorization
                                                                         normalize_predictions = normalize_predictions) # Normalization

            # Add Pass-through features
            if (features_passthrough == "none") {
              # If none, do nothing
              oos_predictions_and_features_m_df <- oos_predictions_m_df
              oos_predictions_and_features_m_df@meta_dataframe_name <- paste0(config@config_name, "_bpreds")
              oos_predictions_and_features_m_df@workflow <- c(oos_predictions_and_features_m_df@workflow, "passthrough_none")
            } else {
              if (features_passthrough == "all") {
                # If all, pass everything except for tickers and dates
                oos_predictions_and_features_m_df <- oos_predictions_m_df
                oos_predictions_and_features_m_df@data <- dplyr::left_join(oos_predictions_m_df@data,
                                                                           dplyr::select(features_m_df@data, -tickers, -dates), by = "id")
                oos_predictions_and_features_m_df@meta_dataframe_name <- paste0(config@config_name, "_bpreds")
                oos_predictions_and_features_m_df@workflow <- c(oos_predictions_and_features_m_df@workflow, "passthrough_all")
              } else {
                # If specific features, pass only those
                oos_predictions_and_features_m_df <- oos_predictions_m_df
                oos_predictions_and_features_m_df@data <- dplyr::left_join(oos_predictions_m_df@data,
                                                                           dplyr::select(features_m_df@data, dplyr::all_of(c("id", features_passthrough))), by = "id")
                oos_predictions_and_features_m_df@meta_dataframe_name <- paste0(config@config_name, "_bpreds")
                oos_predictions_and_features_m_df@workflow <- c(oos_predictions_and_features_m_df@workflow, features_passthrough)
              }
            }

            # Adapt target_m_df
            adapted_target_m_df <- target_m_df
            adapted_target_m_df@data <- dplyr::filter(target_m_df@data, id %in% oos_predictions_m_df@data$id)
            adapted_target_m_df@meta_dataframe_name <- paste0(adapted_target_m_df@meta_dataframe_name, "_adj")

            #######################
            # Run sb_backtest with predictions_m_df
            # Fit Meta Model
            meta_learner_backtest_results <- tryCatch({

              if (verbose == TRUE) {
                cat(crayon::cyan("Starting Meta SB backtesting\n"))
                tictoc::tic(msg = crayon::green("Meta SB backtest finished\n"))
              }

              run_sb_backtest(
                config = config@meta_sb_backtest_config, # Meta SB Configuration
                features_m_df = oos_predictions_and_features_m_df, # Features are oos predictions for base models
                target_m_df = adapted_target_m_df, # Target is the original target
                target_fwd_name = target_fwd_name, # Target forward name
                verbose = verbose,
                parallel = parallel
              )
            }, error = function(e) {
              stop("An error occurred while running the meta SB backtest. Please check the configurations and input data. Details: ", e$message)
            })

            # Change SB Metadata
            # Add workflows, config_name and objects for target and features
            ### Target
            target_object_name <- adapted_target_m_df@meta_dataframe_name
            meta_learner_backtest_results@sb_backtest_workflow$target_object_name <- target_object_name
            meta_learner_backtest_results@sb_backtest_workflow$target_workflow <- adapted_target_m_df@workflow

            ### Features
            features_object_name <- oos_predictions_and_features_m_df@meta_dataframe_name
            meta_learner_backtest_results@sb_backtest_workflow$features_object_name <- features_object_name
            meta_learner_backtest_results@sb_backtest_workflow$features_workflow <- oos_predictions_and_features_m_df@workflow
            meta_learner_backtest_results@sb_backtest_workflow$features_passthrough <- features_passthrough

            ### IDs
            meta_learner_backtest_results@sb_backtest_workflow$config_name <- config@config_name
            meta_learner_backtest_results@sb_backtest_workflow$backtest_identifier <-
              paste0("c:", config@config_name, "_f:", features_object_name, "_t:", target_object_name, "-", target_fwd_name)
            meta_learner_backtest_results@backtest_identifier <- meta_learner_backtest_results@sb_backtest_workflow$backtest_identifier
            meta_learner_backtest_results@sb_backtest_workflow$backtest_type <- "meta_learner"

            ### Call
            meta_learner_backtest_results@sb_backtest_workflow$call <- sys.call(-2)

            ### Add Base Learners Info
            meta_learner_backtest_results@sb_backtest_workflow$config_name_bl <- sapply(base_sb_backtest_results_list, function(x) x@sb_backtest_workflow$config_name)
            meta_learner_backtest_results@sb_backtest_workflow$sb_algorithm_bl <- sapply(base_sb_backtest_results_list, function(x) x@sb_backtest_workflow$sb_algorithm)
            meta_learner_backtest_results@sb_backtest_workflow$dates_covered_bl <- unique(sapply(base_sb_backtest_results_list, function(x) x@sb_backtest_workflow$dates_covered))
            meta_learner_backtest_results@sb_backtest_workflow$n_dates_bl <- length(unique(sapply(base_sb_backtest_results_list, function(x) x@sb_backtest_workflow$dates_covered)))
            meta_learner_backtest_results@sb_backtest_workflow$training_sample_size_bl <- unique(sapply(base_sb_backtest_results_list, function(x) x@sb_backtest_workflow$training_sample_size))
            meta_learner_backtest_results@sb_backtest_workflow$validation_sample_size_bl <- unique(sapply(base_sb_backtest_results_list, function(x) x@sb_backtest_workflow$validation_sample_size))
            meta_learner_backtest_results@sb_backtest_workflow$testing_sample_size_bl <- unique(sapply(base_sb_backtest_results_list, function(x) x@sb_backtest_workflow$testing_sample_size))
            meta_learner_backtest_results@sb_backtest_workflow$dates_testing_sample_bl <- unique(sapply(base_sb_backtest_results_list, function(x) x@sb_backtest_workflow$dates_testing_sample))
            meta_learner_backtest_results@sb_backtest_workflow$rebalance_dates_bl <- unique(sapply(base_sb_backtest_results_list, function(x) x@sb_backtest_workflow$rebalance_dates))

            # Displays how much time it took
            if (verbose == TRUE) {
              tictoc::toc()
            }

            #######################
            # Get ensemble benchmarks

            ## Extract info
            ### Ensemble Eval Metric
            ensemble_eval_metric <- if (config@meta_sb_backtest_config@sb_algorithm == "ols") {
              "rmse"
            } else {
              config@meta_sb_backtest_config@tuning_strategy@chosen_eval_metric # Extract chosen eval metric
            }
            ### Ensemble Huber Delta
            ensemble_huber_delta <- mean(sapply(base_sb_backtest_results_list, function(x) x@sb_backtest_workflow$huber_delta), na.rm = TRUE)
            ### Ensemble Tau
            ensemble_quantile_tau <- mean(sapply(base_sb_backtest_results_list, function(x) x@sb_backtest_workflow$quantile_tau), na.rm = TRUE)

            ### Run Meta Learner Backtest
            heuristic_ensembles_sb_backtest_results_list <- create_heuristic_ensembles(
              base_sb_backtest_results_list, # Base Learners results
              ensemble_eval_metric = ensemble_eval_metric, # Eval metric
              ensemble_huber_delta = ensemble_huber_delta, ensemble_quantile_tau = ensemble_quantile_tau # Huber delta and tau
            )

            # Create object with list of sb_metabacktest_results
            ## List with all ensembles
            meta_sb_backtest_results_list <- list(
              meta_learner_backtest_results, # Meta Learner
              heuristic_ensembles_sb_backtest_results_list$ew_ensemble,
              heuristic_ensembles_sb_backtest_results_list$optimal_ensemble
            ) # Heuristic ensembles
            ## Rename
            names(meta_sb_backtest_results_list) <- sapply(meta_sb_backtest_results_list, function(x) x@backtest_identifier)

            sb_metabacktest_results <- create_sb_metabacktest_results(
              meta_sb_backtest_results_list = meta_sb_backtest_results_list,
              base_sb_backtest_results_list = base_sb_backtest_results_list,
              oos_predictions_m_df = oos_predictions_m_df
            )

            return(sb_metabacktest_results)
          }
)



#' @describeIn run_sb_backtest Run ML Backtest
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
#' @param validation_sample_size Number of observations to include in each validation sample. If provided a decimal, it will be considered as a percentage of the training sample size.
#' @param rebalancing_months Months (numeric) when model should be rebalanced.
#' @param target_fwd_name Name of the target variable in `target_m_df`.
#' @param sb_algorithm Choice of sb_algorithm: ols (Ordinary Least Squares), glmnet (Elastic Net), rf (Random Forest), xgb (eXtreme Gradient Boosting), and nn (Keras Neural Networks).
#' @param split_method Choice of split method (expanding or rolling).
#' @param hyper_grid_domain A named list containing hyperparameter definitions. The structure of this list depends on the specified tuning method:
#' \itemize{
#'   \item \strong{For grid search:} Must be a list of named vectors:
#'   \item \strong{For random search:} Must be a list of named lists, where each named list contains:
#'     \itemize{
#'       \item \code{distribution_choice}: A character string specifying the distribution (one of "normal", "uniform", "lognormal", "constant").
#'       \item \code{pars}: A named numeric vector of parameters corresponding to the chosen distribution.
#'       \item \code{value}: A numeric value (only present if \code{distribution_choice} is "constant").
#'     }
#'   \item \strong{For Bayesian optimization:} Must be a list of named numeric vectors, each of length 2, representing the boundaries for the hyperparameters.
#' }
#' @examples
#' # Example of creating hyper_grid_domain_list for grid search
#' hyper_grid <- list(
#'    alpha = c(0.2, 0.5),
#'    lambda.min.ratio = c(0.1, 0.5, 0.9)
#'    )
#'
#' # Example of creating hyper_grid_domain_list for random search
#' hyper_grid <- list(
#'   alpha = list(distribution_choice = "uniform", pars = c(min = 0, max = 1), value = NULL),
#'   lambda.min.ratio = list(distribution_choice = "uniform", pars = c(min = 0, max = 0.9), value = NULL)
#' )
#'
#' # Example of creating hyper_grid_domain_list for bayesian optimization
#' hyper_grid <- list(
#'    alpha = c(0.2, 0.9),
#'    lambda.min.ratio = c(0.1, 0.9)
#'    )
#'
#' @param tuning_method Method for hyperparameter tuning: "random_search", "grid_search", or "bayesian_opt".
#' @param n_iter Number of iterations.
#' For grid_search, the value does not make difference, as the number of times the ml algorithm validation error will be evaluated equals the exhaustive combination of unique hyperparameters values provided.
#' For random_search, it should be the number of random draws for each hyperparameter. Random samples of n_iter size will be generated for each hyperparameter and their unique values will be exhaustively combined.
#' Therefore, for n_iter = 5 and 2 hyperparameters, the ml algorithm validation error should be generally evaluated 5² = 25 times.
#' For bayesian_opt, it should be the number of times the ml algorithm will be evaluated after initialization.
#' @param acq Acquisition function for Bayesian optimization: "ucb", "ei", or "poi".
#' @param init_points Number of initial random points for Bayesian optimization.
#' @param k_iter Integer that specifies the number of times to sample eval_function at each Epoch during Bayesian optimization.
#' If running in parallel, set iters.k to a multiple of the number of cores. Must be lower and preferably a multiple of n_iter.
#' @param custom_objective Custom objective (double differentiable loss function) for xgboost and nn algorithms.
#' (current options are squared_error, absolute_error and (pseudo)-huber loss)
#' @param early_stop Sets a halting criteria to prevent overfitting in xgb and nn.
#' @param chosen_eval_metric Metric to optimize during tuning: "rss", "rmse", "cp", "mae", "mphe", "mpe", "mape", "hr", and "mb".
#' @param keras_architecture_parameters A named list containing parameters for configuring the Keras neural network architecture. It includes:
#' \itemize{
#'   \item \strong{units}: A numeric vector specifying the number of neurons in each layer.
#'   \item \strong{n_layers}: An integer indicating the total number of layers in the neural network.
#'   \item \strong{activation}: A character vector listing the activation functions for each layer (e.g., "relu", "sigmoid", "tanh").
#'   \item \strong{nn_optimizer}: A character string specifying the optimizer used for training the model (options: "Adam" or "RMSProp").
#'   \item \strong{batch_norm_option}: A logical vector indicating whether batch normalization should be applied after each respective layer (TRUE or FALSE).
#' }
#' @examples
#' # Example of creating a 3 layers keras_architecture_parameters for neural networks.
#' keras_architecture_parameters = list(units = c(32,16,8), n_layers = 3, activation = c("relu", "relu", "relu"),
#'                                      nn_optimizer = "Adam", batch_norm_option = c(TRUE,TRUE,TRUE)
#' )
#' @param signal_universe_m_d_ref A data frame containing the signal universe. If provided, data in this object will be updated with posteriors.
#' @param backtest_returns_xts A xts containing historical backtested returns named according to signals in `signals_universe_m_df`,
#' @param benchmark_returns_xts A xts with benchmark returns, named accordingly.

#' @param huber_delta A single numeric value indicating the boundary that separates where the loss function turns from quadratic to linear.
#' @param quantile_tau A single numeric value indicating target quantile when calculating quantile loss.
#' @param verbose Logical, indicating whether to print progress messages (default is TRUE).
#' @param parallel Logical, indicating whether to run hyperparameter tuning in parallel (default is TRUE).
#'
#' @return An object of class sb_backtest_results with various outputs including model predictions, errors, and validation metrics.
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
run_sb_backtest_internal <- function(
    #Basic Objects Inputs
  features_m_df, target_m_df, training_sample_size, target_fwd_name,
  #Splits
  validation_sample_size = 0, rebalancing_months, split_method = "expanding",
  #Heuristic SB
  signal_universe_m_df, backtest_returns_xts = NULL, selected_market_factor_proxy_xts = NULL,
  covariance_matrix_sample_size = 36, covariance_estimation_method = "sample", active_returns = TRUE, #COV (for RP and MVO)
  rp_method = "cyclical-spinu", n_random_ports = 2000, random_ports_method = "sample", opt_objective = "sharpe", concentration_constraint_policy, #RP/MVO
  #Choice of SB algorithm
  sb_algorithm = "ols",
  #Loss/Eval Functions and Related
  custom_objective = "squared_error", chosen_eval_metric = NULL, huber_delta = 1, quantile_tau = 0.5,
  #Hyperparameter tuning Inputs
  hyper_grid_domain_list = NULL, tuning_method = NULL, n_iter = NULL, k_iter = NULL, acq = "ucb", init_points = NULL, early_stop = NULL,
  #Keras architecture Parameters
  keras_architecture_parameters = NULL,
  #Misc
  verbose = FALSE, parallel = TRUE,
  #Winsorization
  upper_quantile_winsorization = 0.95, lower_quantile_winsorization = 0.05
){

  #Measure time to run and run gc
  elapsed_time <- system.time({

    #Visible binding for global variables
    squared_error <- pseudo_huber_error <- quantile_error <- NULL

    ################
    ##Check Parameters: This function will test whether inputs match format and current functionalities
    check_inputs_sb_backtest(
      features_m_df = features_m_df, target_m_df = target_m_df, training_sample_size = training_sample_size, target_fwd_name = target_fwd_name,
      validation_sample_size = validation_sample_size, rebalancing_months = rebalancing_months, split_method = split_method,
      signal_universe_m_df = signal_universe_m_df, backtest_returns_xts = backtest_returns_xts, selected_market_factor_proxy_xts = selected_market_factor_proxy_xts,
      covariance_matrix_sample_size = covariance_matrix_sample_size, covariance_estimation_method = covariance_estimation_method, active_returns = active_returns,
      rp_method = rp_method, n_random_ports = n_random_ports, random_ports_method = random_ports_method, opt_objective = opt_objective, concentration_constraint_policy = concentration_constraint_policy,
      sb_algorithm = sb_algorithm, custom_objective = custom_objective, chosen_eval_metric = chosen_eval_metric, huber_delta = huber_delta, quantile_tau = quantile_tau,
      hyper_grid_domain_list = hyper_grid_domain_list, tuning_method = tuning_method, n_iter = n_iter, k_iter = k_iter, acq = acq,
      init_points = init_points, early_stop = early_stop, keras_architecture_parameters = keras_architecture_parameters, verbose = verbose, parallel = parallel
    )

    ################

    #Initial Setup: Making some changes to metrics if needed and displaying initial setup
    ##Adjust custom obj and chosen eval metric
    if(verbose) cat("=============================\n")
    adjusted_metrics <- translate_metrics(sb_algorithm = sb_algorithm, chosen_eval_metric = chosen_eval_metric, custom_objective = custom_objective, early_stop = early_stop, huber_delta = huber_delta, verbose = verbose)


    #Pass adjusted metrics
    custom_objective_translated <- adjusted_metrics$custom_objective_translated
    chosen_eval_metric_translated <- adjusted_metrics$chosen_eval_metric_translated
    chosen_eval_metric <- adjusted_metrics$chosen_eval_metric

    #Prints for initial setup
    if(verbose){
      cat(crayon::green(paste("Predictive ML algo:", sb_algorithm)))
      cat("\n")
      cat(paste("Custom objective:", custom_objective, "\n"))
      cat(paste("Eval Metric:", chosen_eval_metric, "\n"))
      cat(paste("Training sample size:", training_sample_size, "\n"))
      cat(paste("Validation sample size:", validation_sample_size, "\n"))
    }

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
    testing_sample_size <- length(dates_m_vector) - training_sample_size - validation_sample_size + 1 #calculate testing sample size

    #Rebalancing Dates
    dates_testing_sample <- dates_m_vector[(training_sample_size + validation_sample_size):
                                             (training_sample_size + validation_sample_size + testing_sample_size - 1)] #These are dates inside testing sample

    first_rebalance_date <- min(dates_testing_sample) #Get first rebalancing date
    rebalance_dates <- unique( #Unique is to eliminate repeated dates, in case month of first_rebalance_date is a rebalancing month
      c(first_rebalance_date, dates_testing_sample[which(lubridate::month(dates_testing_sample) %in% rebalancing_months)]) #Dates corresponding to rebalancing_months
    )
    rebalance_dates <- rebalance_dates[order(rebalance_dates)] #Re-order

    #Eligible signals dates
    eligible_signals_dates <- signal_universe_m_df %>% dplyr::filter(is_eligible == 1) %>% dplyr::select(dates) %>% unique() %>% dplyr::pull()

    #Number of rebalancing months
    n_rebalance_months <- length(rebalance_dates)

    #Last rebalance date
    last_rebalance_date <- max(rebalance_dates)
    #Time expanding validation
    if(!sb_algorithm %in% c("ols", "sw", "ew", "rp", "mto")){
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
      hyper_choice_df$best_lam <- if(sb_algorithm == "glmnet") NA
      hyper_choice_df$best_iteration <- if(!is.null(early_stop)) NA

    }

    #Time expanding test
    #Store test eval
    oos_testing_eval_metrics <- data.frame(
      rss = as.vector(rep(NA, testing_sample_size)), #+1 bco first date is also a testing date
      cp = as.vector(rep(NA, testing_sample_size)),
      rmse = as.vector(rep(NA, testing_sample_size)),
      mae = as.vector(rep(NA, testing_sample_size)),
      mphe = as.vector(rep(NA, testing_sample_size)),
      mpe = as.vector(rep(NA, testing_sample_size)),
      mape = as.vector(rep(NA, testing_sample_size)),
      hr = as.vector(rep(NA, testing_sample_size)),
      mb = as.vector(rep(NA, testing_sample_size))
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
    for(d in (training_sample_size + validation_sample_size):(training_sample_size + validation_sample_size + testing_sample_size - 1)){
      #Get current date
      current_date <- dates_m_vector[d]
      #Check if it's a rebalancing month
      if((lubridate::month(current_date) %in% rebalancing_months) || d == (training_sample_size + validation_sample_size)){
        #Print refitting message
        if(verbose){
          cat("\n")
          cat(crayon::yellow(paste("Starting model rebalancing at:", current_date)))
          cat("\n")
        }

        ###Select and correct signals
        ##################

        ####Get current_eligible_signals
        most_recent_eligible_signals_date <- eligible_signals_dates[which(eligible_signals_dates <= current_date)] %>% max()
        most_recent_signal_universe_m_d_ref <- signal_universe_m_df %>% dplyr::filter(dates == most_recent_eligible_signals_date)
        current_eligible_signals <- most_recent_signal_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers)

        ####Reconstruct chosen_signals_and_positions
        chosen_signals_and_positions <- ifelse(stringr::str_detect(current_eligible_signals, pattern = "low_"), "short", "long")
        names(chosen_signals_and_positions) <- stringr::str_remove_all(current_eligible_signals, pattern = "low_")

        ####Select and correct signals and backtests
        selected_signals_and_backtest_list <- select_and_correct_signals(
          signals_m_df = features_m_df, #Extract eligible signals from features_m_df and then correct them (multiply short signal by -1)
          chosen_signals_and_positions = chosen_signals_and_positions, #Get instruction on what to change features
          backtest_returns_xts = backtest_returns_xts #Backtest returns to be corrected
        )
        ####Get results
        selected_signals_corrected_positions_m_df <- selected_signals_and_backtest_list$selected_signals_corrected_positions_m_df
        selected_backtest_returns_corrected_positions_xts <- selected_signals_and_backtest_list$selected_backtest_returns_corrected_positions_xts


        #Print message
        if(verbose){
          cat("\n")
          cat("Selecting and correcting signals:")
          cat("\n")
          cat("Most recent signal universe:\n")
          print(most_recent_signal_universe_m_d_ref)
          cat("\n")
          cat("Eligible signals and positions:\n")
          print(chosen_signals_and_positions)
        }


        ##################

        ###Time Series Splits
        ##################
        #In case of ML models, there is a validation split to search for hyperparameters. Therefore, the sample is split in three

        ###Features and target split
        ts_splits <- time_series_split(
          #Data
          features_m_df = selected_features_corrected_positions_m_df, target_m_df = target_m_df, target_fwd = target_fwd, target_fwd_name = target_fwd_name,
          #Dates
          current_date = current_date, dates_m_vector = dates_m_vector,
          #Splits
          training_sample_size = training_sample_size, split_method = split_method, validation_sample_size = validation_sample_size
        )

        ####No tuning warning
        if(verbose & sb_algorithm %in% c("ols", "sw", "ew", "rp", "mto")){
          cat(sb_algorithm, " chosen as sb_algorithm. Data will be split only in training and test sets")
        }

        ###backtest and selected market factor proxy split (get up to date references)
        selected_backtest_returns_corrected_positions_xts_upd_ref <- selected_backtest_returns_corrected_positions_xts[which(zoo::index(selected_backtest_returns_corrected_positions_xts) <= current_date), ] #Get backtest returns until current date
        selected_market_factor_proxy_xts_upd_ref <- selected_market_factor_proxy_xts[which(zoo::index(selected_market_factor_proxy_xts) <= current_date), ]


        ##################

        ###Hyperparameter tuning!
        #########################
        if(!sb_algorithm %in% c("ols", "sw", "ew", "rp", "mto")){ #If sb_algorithm is OLS or heuristic, one just need to fit model do traininig + validation samples

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


          #Set eval_function: function to fit model to training data and calculate eval metrics to validation function
          ##################
          eval_function <- set_eval_function(
            #Choice of ml algorithm
            ml_algorithm = sb_algorithm,
            #Choice of tuning method
            tuning_method = tuning_method
          )
          ##################

          #Hyper tune according to chosen tuning_method
          ##################
          ###Print
          if(verbose == TRUE){
            cat(paste("Starting", tuning_method, "hyperparameter tuning at:", current_date))
            cat("\n")
          }

          hyper_tune_results <- hyper_tune(
            #General Parameters
            tuning_method = tuning_method, sb_algorithm = sb_algorithm, target_fwd_name = target_fwd_name,
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
        }

        #Refitting
        ###################

        ###Set objects
        ####Refit new model using data from d - target_fwd
          features_m_refit <- ts_splits$refit$features_m_refit #Subset
          target_m_refit <- ts_splits$refit$target_m_refit #Subset
          full_data_m_refit_clean <- ts_splits$refit$full_data_m_refit_clean #Full data


        #(RE)Fit SB Model





        #Create S4 Object
        refit_sb_model <- new("sb_model",
                              model = sb_model,
                              model_class = class(refit_model),
                              sb_algorithm = sb_algorithm,
                              best_hyperparameters = if(sb_algorithm == "ols") NULL else optimal_hyper,
                              custom_objective = custom_objective_translated,
                              huber_delta = huber_delta,
                              keras_architecture_parameters = keras_architecture_parameters
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
      oos_prediction_list[[testing_lists_ref]] <- predict(refit_ml_model, new_features_m_df = features_m_d_ref)

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
      ##Create consolidated row
      consolidated_eval_metrics <- calculate_eval_metrics(pred = unlist(oos_prediction_list), target = unlist(oos_y_list),
                                                          huber_delta = huber_delta, quantile_tau = quantile_tau, chosen_eval_metric = chosen_eval_metric)[-1] #-1 to eliminate Score
      rownames(consolidated_eval_metrics) <- "consolidated"

    result_list[[4]] <- rbind(oos_testing_eval_metrics, consolidated_eval_metrics)

    #Model
    result_list[[5]] <- refit_ml_model

    if(!sb_algorithm == "ols"){

      #Validation eval for all hyperparameters
      names(chosen_eval_metric_validation) <- rebalance_dates #Change names
      result_list[[6]] <- chosen_eval_metric_validation
      #Hyper_choice
      result_list[[7]] <- hyper_choice_df

      #Validation eval-metrics
        ##Create average row
        avg_validation_eval_metrics_hyper_choice <- as.data.frame(t(colMeans(validation_eval_metrics_hyper_choice)))
        rownames(avg_validation_eval_metrics_hyper_choice) <- "average"

      result_list[[8]] <- rbind(validation_eval_metrics_hyper_choice, avg_validation_eval_metrics_hyper_choice)

      #Fill names
      names(result_list) <- c("oos_prediction_list", "oos_error_list", "oos_y_list", "oos_testing_eval_metrics", "final_model",
                              "chosen_eval_metric_validation","best_hyperparameters", "validation_eval_metrics_hyper_choice") #ML Specific

    } else {
      #Fill names
      names(result_list) <- c("oos_prediction_list", "oos_error_list", "oos_y_list", "oos_testing_eval_metrics", "final_model")
    }


  })

  #Print elapsed time
  print(elapsed_time)
  if(verbose) cat("=============================\n")

  #sb_backtest_workflow
  result_list$sb_backtest_workflow <- list(
    #Algo
    sb_algorithm = sb_algorithm,
    config_name = "not_identified",
    backtest_identifier = "not_identified",
    custom_objective = custom_objective,
    backtest_type = "base_learner",
    #Dates
    dates_covered = dates_m_vector,
    n_dates = length(dates_m_vector),
    training_sample_size = training_sample_size,
    validation_sample_size = validation_sample_size,
    testing_sample_size = testing_sample_size,
    dates_testing_sample = dates_testing_sample,
    first_rebalance_date = first_rebalance_date,
    rebalancing_months = rebalancing_months,
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
    target_workflow = NULL,
    target_object_name = "not_identified",
    #Features
    features = colnames(features_m_df[,-c(1:3)]),
    features_workflow = NULL,
    features_object = "not_identified",
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
    timestamps = c(initialization = Sys.time()),
    elapsed_time = elapsed_time,
    parallel = parallel,
    #Call
    call = match.call()
  )

  #Get S4 object
  sb_backtest_results_object <-
    new("sb_backtest_results",
        oos_prediction_list = result_list$oos_prediction_list,
        oos_error_list = result_list$oos_error_list,
        oos_y_list = result_list$oos_y_list,
        oos_testing_eval_metrics = result_list$oos_testing_eval_metrics,
        final_model = result_list$final_model,
        chosen_eval_metric_validation = result_list$chosen_eval_metric_validation,
        best_hyperparameters = result_list$best_hyperparameters,
        validation_eval_metrics_hyper_choice = result_list$validation_eval_metrics_hyper_choice,
        sb_backtest_workflow = result_list$sb_backtest_workflow,
        backtest_identifier = result_list$sb_backtest_workflow$backtest_identifier
        )


  #Return List
  return(sb_backtest_results_object)

}


#' @title Run Base Signal Blending Backtests
#' @description Runs signal blending backtests for base learners using the provided configurations.
#' This helper function is used within \code{run_sb_backtest} but can be utilized independently if needed.
#'
#' @param features_m_df A \code{meta_dataframe} object containing the features data.
#' @param target_m_df A \code{meta_dataframe} object containing the target data.
#' @param target_fwd_name A character string specifying the name of the target forward variable in \code{target_m_df}.
#' @param base_sb_backtest_configs A list of \code{sb_backtest_config} objects containing configurations for each base learner.
#' @param verbose Logical; if \code{TRUE}, prints progress messages. Default is \code{TRUE}.
#' @param parallel Logical; if \code{TRUE}, runs the backtests in parallel using the future framework. Default is \code{TRUE}.
#' @return A list of \code{sb_backtest_results} objects, one for each base learner configuration.
#' @examples
#' # Assuming features_m_df and target_m_df are meta_dataframe objects
#' # and base_sb_backtest_configs is a list of sb_backtest_config objects
#' base_results <- run_base_sb_backtests(
#'   features_m_df = features_m_df,
#'   target_m_df = target_m_df,
#'   target_fwd_name = "target",
#'   base_sb_backtest_configs = base_sb_backtest_configs,
#'   verbose = TRUE,
#'   parallel = TRUE
#' )
#' @export
run_base_sb_backtests <- function(features_m_df, target_m_df, target_fwd_name, base_sb_backtest_configs, verbose = TRUE, parallel = TRUE) {

  base_sb_backtest_configs_names <- sapply(base_sb_backtest_configs, function(x) x@config_name)

  if (verbose == TRUE) {
    cat(crayon::green("Starting Base ML backtests:\n"))
    cat(paste("Number of configurations: ", length(base_sb_backtest_configs), "\n"))
    cat(paste("Configuration names: ", paste(base_sb_backtest_configs_names, collapse = ", "), "\n"))
    tictoc::tic(msg = crayon::green("Base ML backtests finished\n"))
  }

  tryCatch({
    #In Parallel
    if(parallel){
      base_sb_backtest_results_list <- furrr::future_map(base_sb_backtest_configs, #List of backtest configurations
                                                         ~ run_sb_backtest( #Backtesting function
                                                           ...,
                                                           #Data
                                                           features_m_df = features_m_df, target_m_df = target_m_df, target_fwd_name = target_fwd_name,
                                                           #Misc
                                                           verbose = FALSE, parallel = parallel
                                                         ),
                                                         .options = furrr::furrr_options(seed = TRUE),
                                                         .progress = verbose
      )
      cat("\n")

    } else { #If not running in parallel
      base_sb_backtest_results_list <-  purrr::map(base_sb_backtest_configs,
                                                   run_sb_backtest, #Backtesting function
                                                   #Data
                                                   features_m_df = features_m_df, target_m_df = target_m_df, target_fwd_name = target_fwd_name,
                                                   #Misc
                                                   verbose = FALSE, parallel = parallel
      )
    }
  }, error = function(e) {
    stop("An error occurred while running the base ML backtests. Please check the configurations and input data. Details: ", e$message)
  })

  # Displays how much time it took
  if (verbose == TRUE) {
    tictoc::toc()

    # Show some results
    cat("Consolidated OOS Testing Eval Metrics:\n")
    # Create consolidated OOS Testing Eval Metrics
    all_consolidated_oos_testing_eval_metrics <- data.frame(
      t(sapply(
        base_sb_backtest_results_list,
        function(x) x@oos_testing_eval_metrics[which(rownames(x@oos_testing_eval_metrics) == "consolidated"), ]
      )),
      row.names = base_sb_backtest_configs_names
    )
    # Print results
    print(all_consolidated_oos_testing_eval_metrics)
    cat("\n")
  }

  # Name Base Learners in Metadata and also the list
  names(base_sb_backtest_results_list) <- sapply(base_sb_backtest_results_list, function(x) x@backtest_identifier)
  for (i in 1:length(base_sb_backtest_results_list)) {
    base_sb_backtest_results_list[[i]]@sb_backtest_workflow$config_name <- base_sb_backtest_configs_names[i]
  }

  return(base_sb_backtest_results_list)
}




