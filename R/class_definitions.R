#' Define the `meta_dataframe` S4 Class
#'
#' This class represents a metadata-enhanced data frame. It extends the functionality
#' of a standard data frame by including additional metadata slots. The class is designed
#' to ensure that the input data frame adheres to specific structural requirements, including
#' unique identifiers, valid date formats, and unique column names.
#'
#' @slot data A \code{data.frame} containing the actual data.
#' @slot workflow A \code{list} for storing metadata about the data manipulation workflow.
#' @slot signals A \code{character} vector containing the names of columns that represent signals.
#' @slot unique_dates A \code{numeric} value representing the count of unique dates in the data.
#' @slot unique_tickers A \code{numeric} value representing the count of unique tickers in the data.
#' @slot n_obs A \code{numeric} value representing the total number of observations in the data.
#'
#' @details
#' The \code{meta_dataframe} class ensures that the data frame is structured correctly with the required columns,
#' and includes metadata about the data. The \code{signals} slot holds the names of columns representing various signals.
#' The \code{unique_dates}, \code{unique_tickers}, and \code{n_obs} slots store the metadata related to the number of unique dates,
#' tickers, and total observations respectively.
#'
#' @examples
#' # Define a sample data frame
#' df <- data.frame(
#'   id = c("A-2024-01-01", "B-2024-02-01"),
#'   tickers = c("A", "B"),
#'   dates = as.Date(c("2024-01-01", "2024-02-01")),
#'   value = c(10, 20)
#' )
#'
#' # Create a meta_dataframe object
#' meta_df <- create_meta_dataframe(df)
#'
#' # Print the meta_dataframe object
#' print(meta_df)
#'
#' @export
setClass("meta_dataframe",
         slots = c(
           data = "data.frame",        # Slot for the data frame
           workflow = "list",          # Slot for storing metadata about the data manipulation workflow
           signals = "character",      # Slot for storing column names
           unique_dates = "numeric",   # Slot for storing count of unique dates
           unique_tickers = "numeric", # Slot for storing count of unique tickers
           n_obs = "numeric"           # Slot for storing total number of observations
         ))


#' Define the `hyper_grid_domain` S4 Class
#'
#' This class represents parameters for defining the hyperparameter domain based on which tuning will be performed.
#' It helps the user in correctly setting this object in the context of the `ml_walk_forward_validation` function.
#'
#' @slot hyperparameter_list A list with the hyperparameters relevant to the specified machine learning algorithm.
#'
#'
#' @export
setClass(
  "hyper_grid_domain",
  slots = list(
    hyperparameter_list = "list"
  ), validity = function(object){

    #Check for valid choice in hyperparameter
    if(length(object@hyperparameter_list) != 0){
    valid_hyperparameters <- c("alpha", "lambda.min.ratio", "mtry", "num.trees", "max.depth", "min.bucket", "min_child_weight", "max_depth", "subsample", "colsample_bytree",
                               "eta", "gamma", "nrounds", "regularizer_l1", "regularizer_l2", "droprate", "lr", "size_of_batch", "number_of_epochs")

    if (any(!names(object@hyperparameter_list) %in% valid_hyperparameters) ){
      return("Invalid choice for hyperparameter. Should be one of alpha, lambda.min.ratio (glmnet), mtry, num.trees, max.depth, min.bucket (rf),
             min_child_weight, max_depth, subsample, colsample_bytree, eta, gamma, nrounds (xgb),
             regularizer_l1, regularizer_l2, droprate, lr, size_of_batch, number_of_epochs (nn)")
    }

  }

  })

#' @title Base class for hyperparameter tuning strategies
#' @description This class defines the common slots and structure for hyperparameter tuning strategies such as grid search, random search, and Bayesian optimization.
#' @slot tuning_method Character string indicating the hyperparameter tuning method ('grid_search', 'random_search', or 'bayesian_opt').
#' @slot validation_sample_size Numeric value representing the size of the validation sample.
#' @slot split_method Character string indicating the data splitting method ('expanding' or 'rolling').
#' @slot chosen_eval_metric Character or NULL, specifying the evaluation metric to be optimized.
#' @slot hyper_grid_domain An object of class `hyper_grid_domain`, representing the hyperparameter domain based on which tuning will be performed.
#' It contains a list slot `hyperparameter_list` with the hyperparameters relevant to the specified machine learning algorithm.
#' The structure of this list depends on the specified tuning method:
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
#' @slot early_stop Sets a halting criteria to prevent overfitting in xgb and nn.
#' @export
setClass(
  "tuning_strategy",
  slots = list(
    tuning_method = "character",
    validation_sample_size = "numeric",
    split_method = "character",
    chosen_eval_metric = "character",
    hyper_grid_domain = "hyper_grid_domain",
    early_stop = "ANY"
    ),
  prototype = list(
    split_method = "expanding"
  ),
  validity = function(object) {
    if (!(object@tuning_method %in% c("grid_search", "random_search", "bayesian_opt"))) {
      return("Invalid tuning_method.")
    }
    if(is.null(object@chosen_eval_metric)){
      stop("chosen_eval_metric can't be missing.")
    }
    valid_eval_metrics <- c("rss", "rmse", "cp", "mae", "mphe", "mpe", "mape", "hr", "mb")
    if (!is.null(object@chosen_eval_metric) && !(object@chosen_eval_metric %in% valid_eval_metrics)) {
      return("Invalid chosen_eval_metric. Choose from 'rss', 'rmse', 'cp', 'mae', 'mphe', 'mpe', 'mape', 'hr', 'mb'.")
    }
    if (object@split_method != "expanding") {
      return("split_method should be expanding.")
    }
    # Validate hyperparameters based on tuning_method

    ##Grid search
    if (object@tuning_method == "grid_search") {
      if (!all(sapply(object@hyper_grid_domain@hyperparameter_list, function(x) is.numeric(x) && is.vector(x)))) {
        stop("For 'grid_search', hyperparameters must be a list of numeric vectors.")
      }

      ##Random search
    }

    else if (object@tuning_method == "random_search") {
      for (name in names(object@hyper_grid_domain@hyperparameter_list)) {
        if (!is.list(object@hyper_grid_domain@hyperparameter_list[[name]]) || !all(c("distribution_choice") %in% names(object@hyper_grid_domain@hyperparameter_list[[name]]))) {
          stop("For 'random_search', each hyperparameters must be a list with 'distribution_choice'.")
        }

        distribution_choice <- object@hyper_grid_domain@hyperparameter_list[[name]]$distribution_choice

        if (is.null(distribution_choice) || !(distribution_choice %in% c("normal", "uniform", "lognormal", "constant"))) {
          stop("distribution_choice must be one of 'normal', 'uniform', 'lognormal', or 'constant'.")
        }

        if (distribution_choice == "constant") {
          if (is.null(object@hyper_grid_domain@hyperparameter_list[[name]]$value) || !is.numeric(object@hyper_grid_domain@hyperparameter_list[[name]]$value)) {
            stop("For 'constant', the second argument must be a numeric vector named 'value'.")
          }
        } else {
          if (!is.null(object@hyper_grid_domain@hyperparameter_list[[name]]$value)) {
            stop("For distributions other than 'constant', do not specify 'value'.")
          }
          pars <- object@hyper_grid_domain@hyperparameter_list[[name]]$pars
          if (is.null(pars) || !is.numeric(pars) || !is.vector(pars)) {
            stop("For distributions, the second argument must be a numeric vector named 'pars'.")
          }

          # Additional checks based on distribution_choice
          if (distribution_choice == "normal") {
            if (!all(names(pars) %in% c("mean", "sd"))) {
              stop("For 'normal', 'pars' must have names 'mean' and 'sd'.")
            }
          } else if (distribution_choice == "uniform") {
            if (!all(names(pars) %in% c("min", "max"))) {
              stop("For 'uniform', 'pars' must have names 'min' and 'max'.");
            }
          } else if (distribution_choice == "lognormal") {
            if (!all(names(pars) %in% c("meanlog", "sdlog"))) {
              stop("For 'lognormal', 'pars' must have names 'meanlog' and 'sdlog'.");
            }
          }
        }
      }
    }

    #Bayesian Optimization
    else if (object@tuning_method == "bayesian_opt") {
      if (any(sapply(object@hyper_grid_domain@hyperparameter_list, function(x) !is.numeric(x) || length(x) != 2))) {
        stop("For 'bayesian_opt', each hyperparameters must be a numeric vector of length 2 representing the bounds.")
      }
    }

    else {
      stop("Invalid tuning_method. Only 'grid_search', 'random_search', and 'bayesian_opt' are supported.")
    }

   return(TRUE)
  }
)


#' @title Grid Search Tuning Strategy
#' @description A subclass of `tuning_strategy` that implements grid search.
#' @slot tuning_method The tuning method is set to 'grid_search'.
#' @export
setClass(
  "grid_search_strategy",
  contains = "tuning_strategy",
  slots = list(),
  prototype = list(tuning_method = "grid_search")
)

#' @title Random Search Tuning Strategy
#' @description A subclass of `tuning_strategy` that implements random search.
#' @slot tuning_method The tuning method is set to 'random_search'.
#' @slot n_iter For random_search, it should be the number of random draws for each hyperparameter to which a distribution has been assigned.
#' Random samples of n_iter size will be generated for each hyperparameter and their unique values will be exhaustively combined.
#' Therefore, for n_iter = 5 and 2 hyperparameters, the ml algorithm validation error should be generally evaluated 5² = 25 times.
#' In case a constant vector is passed, the n_iter argument is not applied to this hyperparameter.

#' @export
setClass(
  "random_search_strategy",
  contains = "tuning_strategy",
  slots = list(
    n_iter = "numeric"
  ),
  prototype = list(tuning_method = "random_search")
)

#' @title Bayesian Opt Tuning Strategy
#' @description A subclass of `tuning_strategy` that implements bayesian optimization.
#' @slot tuning_method The tuning method is set to 'bayesian_opt'.
#' @slot n_iter For bayesian_opt, it should be the number of times the ml algorithm will be evaluated after initialization.
#' @slot acq Acquisition function for Bayesian optimization: "ucb", "ei", or "poi".
#' @param init_points Number of initial random points for Bayesian optimization.
#' @param k_iter Integer that specifies the number of times to sample eval_function at each Epoch during Bayesian optimization.
#' If the intention is running in parallel, set k_iter to a multiple of the number of cores. Must be lower and preferably a multiple of n_iter.

#' @export
setClass(
  "bayesian_opt_strategy",
  contains = "tuning_strategy",
  slots = list(
    n_iter = "numeric",
    acq = "character",
    init_points = "numeric",
    k_iter = "numeric"
  ),
  prototype = list(
    tuning_method = "bayesian_opt",
    acq = "ucb"),
  validity = function(object){
    if (!object@acq %in% c("ucb", "ei", "poi")) {
      stop("acq must be one of 'ucb', 'ei', or 'poi'.")
    }
    return(TRUE)
  }
)


#' @title Keras Architecture Parameters
#' @description Class to encapsulate parameters for constructing a Keras neural network architecture.
#'
#' @slot units A numeric vector specifying the number of units (neurons) for each layer.
#' @slot n_layers A numeric value representing the total number of layers in the model.
#' @slot activation A character vector containing the activation functions for each layer.
#' @slot nn_optimizer A character string indicating the optimization algorithm used (length = 1).
#' @slot batch_norm_option A character vector specifying whether to apply batch normalization for each layer.
#'
#' @export
setClass(
  "keras_architecture_parameters",
  slots = list(
    units = "numeric",            # Vector of numeric units per layer
    n_layers = "numeric",         # Total number of layers
    activation = "character",     # Vector of activation functions
    nn_optimizer = "character",    # Optimization algorithm
    batch_norm_option = "logical" # Vector of batch normalization options
  )
)

#' @title ml_backtest_config Class
#' @description The ml_backtest_config class is designed to define an end-to-end machine learning experiment, including the hyperparameter tuning strategy, algorithm parameters, and other experiment-specific configurations.
#' @slot target_fwd_name Character string indicating the forward target variable's name. Should be in the format 'xxxxxx_ym' and should be included in
#' `target_m_df`.
#' @slot ml_algorithm Character string specifying the machine learning algorithm to be used. Should be one of
#' ols (Ordinary Least Squares), glmnet (Elastic Net), rf (Random Forest), xgb (eXtreme Gradient Boosting), and nn (Keras Neural Networks).
#' @slot tuning_strategy An object of class `tuning_strategy`, specifying the strategy for tuning hyperparameters.
#' @slot custom_objective Character string specifying the custom objective function ('squared_error', 'pseudo_huber_error', 'absolute_error') or NULL.
#' Custom objective  should be a double differentiable loss function and is only applicable for xgboost and nn algorithms.
#' @slot keras_architecture_parameters An object of class `keras_architecture_parameters` or NULL, providing parameters specific to keras-based neural networks.
#' It includes:
#' \itemize{
#'   \item \strong{units}: A numeric vector specifying the number of neurons in each layer.
#'   \item \strong{n_layers}: An integer indicating the total number of layers in the neural network.
#'   \item \strong{activation}: A character vector listing the activation functions for each layer (e.g., "relu", "sigmoid", "tanh").
#'   \item \strong{nn_optimizer}: A character string specifying the optimizer used for training the model (options: "Adam" or "RMSProp").
#'   \item \strong{batch_norm_option}: A logical vector indicating whether batch normalization should be applied after each respective layer (TRUE or FALSE).
#' }
#' @slot quantile_tau A single numeric value indicating the tau parameter used for quantile regression, between 0 and 1.
#' @slot huber_delta A single positive numeric value indicating the boundary that separates where the loss function turns from quadratic to linear.
#' @export
setClass(
  "ml_backtest_config",
  slots = list(
    target_fwd_name = "character",
    ml_algorithm = "character",
    tuning_strategy = "ANY",
    custom_objective = "character",
    keras_architecture_parameters = "ANY",
    quantile_tau = "numeric",
    huber_delta = "numeric"
  ),
  prototype = list(
    ml_algorithm = "ols",
    custom_objective = "squared_error",
    quantile_tau = 0.5,
    huber_delta = 1
  ),
  validity = function(object) {
    valid_ml_algorithms <- c("ols", "glmnet", "rf", "xgb", "nn")
    if(!(object@ml_algorithm %in% valid_ml_algorithms)) {
      return("Invalid ml_algorithm. Choose from ols, glmnet, rf, xgb, or nn.")
    }
    if (!is.null(object@custom_objective) && !(object@custom_objective %in% c("squared_error", "pseudo_huber_error", "absolute_error"))) {
      return("Invalid custom_objective. Choose from 'squared_error', 'pseudo_huber_error', or 'absolute_error'.")
    }
    if (!(object@ml_algorithm %in% c("xgb", "nn")) && !is.null(object@custom_objective) && object@custom_objective != "squared_error") {
      return("Invalid custom_objective. Custom objectives are only allowed for 'xgb' or 'nn' algorithms.")
    }
    if (!(object@ml_algorithm %in% c("xgb", "nn")) && !is.null(object@tuning_strategy) && !is.null(object@tuning_strategy@early_stop)) {
      return("Invalid early_stop. Early stop is only allowed for 'xgb' or 'nn' algorithms.")
    }
    if(object@ml_algorithm != "ols" && is.null(object@tuning_strategy)){
      message("when ml_algorithm is not ols, a tuning_strategy must be set")
    }
    target_fwd_name_right_pattern <- "^[A-Za-z_]+_[0-9]{1,2}m$"
    if(!grepl(target_fwd_name_right_pattern, object@target_fwd_name)){
      stop("target_fwd_name is not in the right pattern")
    }
    if(!is.null(object@keras_architecture_parameters)){
      if(!is_keras_architecture_parameters(object@keras_architecture_parameters)){
        return("Invalid keras_architecture_parameters Should be of class keras_architecture_parameters")
      }
      if(object@ml_algorithm != "nn"){
        return("keras_architecture_parameters is only needed when ml_algorithm is nn")
      }
    }
    if (!is.null(object@quantile_tau) && (object@quantile_tau <= 0 || object@quantile_tau >= 1)) {
      return("quantile_tau must be between 0 and 1.")
    }
    if (!is.null(object@huber_delta) && object@huber_delta <= 0) {
      return("huber_delta must be greater than 0.")
    }
    if (!is.null(object@quantile_tau) && object@quantile_tau != 0.5) {
      message("changing quantile_tau impacts both chosen_eval_metric and custom_objective.")
    }
    if (!is.null(object@huber_delta) && object@huber_delta != 1) {
      message("changing huber_delta impacts both chosen_eval_metric and custom_objective. ")
    }

    #Check if hypers are correctly set
    if(!is.null(object@tuning_strategy)){
      if(!is_tuning_strategy(object@tuning_strategy)){
        return("Invalid tuning_strategy. Should be of class tuning_strategy")
      }
      if(object@ml_algorithm == "ols"){
        return("ols does not support hyperparameter tuning")
      }

        # Check hyperparameters validity based on ml_algorithm
        hyperparameters_names <- names(object@tuning_strategy@hyper_grid_domain@hyperparameter_list)

        # GLMNET
        expected_hyperparameters_glmnet <- c("alpha", "lambda.min.ratio")
        if (object@ml_algorithm == "glmnet" && any(!hyperparameters_names %in% expected_hyperparameters_glmnet)) {
          stop("hyperparameters do not match ml_algorithm choice for 'glmnet'")
        }
        hyperparameters_missing <- expected_hyperparameters_glmnet[which(!expected_hyperparameters_glmnet %in% hyperparameters_names)]
        if(length(hyperparameters_missing) != 0 && object@ml_algorithm == "glmnet"){
          cat("\n")
          message(paste("The following hyperparameter(s) must still be configured for the",
                        object@ml_algorithm, "algorithm:",
                        paste(hyperparameters_missing, collapse = ", ")))
          cat("\n")
        }


        # RF
        expected_hyperparameters_rf <- c("mtry", "num.trees", "max.depth", "min.bucket")
        if (object@ml_algorithm == "rf" && any(!hyperparameters_names %in% expected_hyperparameters_rf)) {
          stop("hyperparameters do not match ml_algorithm choice for 'rf'")
        }
        hyperparameters_missing <- expected_hyperparameters_rf[which(!expected_hyperparameters_rf %in% hyperparameters_names)]
        if(length(hyperparameters_missing) != 0 && object@ml_algorithm == "rf"){
          cat("\n")
          message(paste("The following hyperparameter(s) must still be configured for the",
                        object@ml_algorithm, "algorithm:",
                        paste(hyperparameters_missing, collapse = ", ")))
          cat("\n")
        }


        # XGB
        expected_hyperparameters_xgb <- c("min_child_weight", "max_depth", "subsample", "colsample_bytree", "eta", "alpha", "gamma", "nrounds")
        if (object@ml_algorithm == "xgb" && any(!hyperparameters_names %in% expected_hyperparameters_xgb)) {
          stop("hyperparameters do not match ml_algorithm choice for 'xgb'")
        }
        hyperparameters_missing <- expected_hyperparameters_xgb[which(!expected_hyperparameters_xgb %in% hyperparameters_names)]
        if(length(hyperparameters_missing) != 0 && object@ml_algorithm == "xgb"){
          cat("\n")
          message(paste("The following hyperparameter(s) must still be configured for the",
                        object@ml_algorithm, "algorithm:",
                        paste(hyperparameters_missing, collapse = ", ")))
          cat("\n")
        }


        # NN
        expected_hyperparameters_nn <- c("regularizer_l1", "regularizer_l2", "droprate", "lr", "size_of_batch", "number_of_epochs")
        if (object@ml_algorithm == "nn" && any(!hyperparameters_names %in% expected_hyperparameters_nn)) {
          stop("hyperparameters do not match ml_algorithm choice for 'nn'")
        }
        hyperparameters_missing <- expected_hyperparameters_nn[which(!expected_hyperparameters_nn %in% hyperparameters_names)]
        if(length(hyperparameters_missing) != 0 && object@ml_algorithm == "nn"){
          cat("\n")
          message(paste("The following hyperparameter(s) must still be configured for the",
                        object@ml_algorithm, "algorithm:",
                        paste(hyperparameters_missing, collapse = ", ")))
          cat("\n")
        }

    }


    # Validate hyperparameters based on ml_algorithm
    if(!is.null(object@tuning_strategy) && !is.null(object@tuning_strategy@hyper_grid_domain@hyperparameter_list)){

    #Hyperparameters
    hyperparameter_list <- object@tuning_strategy@hyper_grid_domain@hyperparameter_list
    tuning_method <- object@tuning_strategy@tuning_method

    if (length(hyperparameter_list) > 0) {
      for (hyperparameter in names(hyperparameter_list)) {
        #Extract value for grid_searcho or bayesian_opt
        value <- hyperparameter_list[[hyperparameter]]

        #Extract distribution and parameters for random_search
        if(tuning_method == "random_search"){
        distribution_choice <- object@tuning_strategy@hyper_grid_domain@hyperparameter_list[[hyperparameter]]$distribution_choice  #Distribution Choice
        pars <- object@tuning_strategy@hyper_grid_domain@hyperparameter_list[[hyperparameter]]$pars  #Pars
        }

        #GLMET Logic
        if (object@ml_algorithm == "glmnet") {
          if(tuning_method != "random_search"){
            if (hyperparameter == "alpha" && (any(value < 0) || any(value > 1))) {
              stop("alpha should be set in interval [0, 1]")
            }
            if (hyperparameter == "lambda.min.ratio" && (any(value < 0) || any(value >= 1))) {
              stop("lambda.min.ratio should be set in interval [0, 1)")
            }
          } else {
            if (hyperparameter == "alpha"){
              if(distribution_choice == "uniform" && pars["max"] > 1) warning("max above upper range for alpha")
              if(distribution_choice == "uniform" && pars["min"] < 0) warning("min below lower range for alpha")
              if(distribution_choice == "normal" && (pars["mean"] + pars["sd"]/2 > 1)) warning("mean + sd/2 above upper range for alpha")
              if(distribution_choice == "normal" && (pars["mean"] - pars["sd"]/2 < 0)) warning("mean - sd/2 below lower range for alpha")
              if(distribution_choice == "lognormal" && (pars["meanlog"] + pars["sdlog"]/2 > 1)) warning("meanlog + sdlog/2 above upper range for alpha")
              if(distribution_choice == "lognormal" && (pars["meanlog"] - pars["sdlog"]/2 < 0)) warning("meanlog - sdlog/2 below lower range for alpha")
              if(distribution_choice == "constant" && (any(value$value < 0) || any(value$value > 1))) stop("alpha should be set in interval [0, 1]")
            }
            if (hyperparameter == "lambda.min.ratio"){
              if(distribution_choice == "uniform" && pars["max"] >= 1) warning("max above upper range for lambda.min.ratio")
              if(distribution_choice == "uniform" && pars["min"] < 0) warning("min below lower range for lambda.min.ratio")
              if(distribution_choice == "normal" && (pars["mean"] + pars["sd"]/2 >= 1)) warning("mean + sd/2 above upper range for lambda.min.ratio")
              if(distribution_choice == "normal" && (pars["mean"] - pars["sd"]/2 < 0)) warning("mean - sd/2 below lower range for lambda.min.ratio")
              if(distribution_choice == "lognormal" && (pars["meanlog"] + pars["sdlog"]/2 >= 1)) warning("meanlog + sdlog/2 above upper range for lambda.min.ratio")
              if(distribution_choice == "lognormal" && (pars["meanlog"] + pars["sdlog"]/2 < 0)) warning("meanlog - sdlog/2 below lower range for lambda.min.ratio")
              if(distribution_choice == "constant" && (any(value$value < 0) || any(value$value >= 1)))stop("lambda.min.ratio should be set in interval [0, 1)")
            }
          }
        }

        #RF Logic
        if (object@ml_algorithm == "rf") {
          if(tuning_method != "random_search"){
            if (hyperparameter == "num.trees" && tuning_method == "bayesian_opt" && (any(value <= 0) || any(!is.integer(value)))) {
              stop("num.trees should be a positive integer without decimals")
            }
            if (hyperparameter == "num.trees" && tuning_method == "grid_search" && (any(value <= 0) || any(!(value == floor(value))))) {
              stop("num.trees should be a positive integer without decimals")
            }
            if (hyperparameter == "mtry" && (any(value < 0) || any(value > 1))) {
              stop("mtry should be set in interval [0, 1]")
            }
            if (hyperparameter == "max.depth" && tuning_method == "bayesian_opt" && (any(value <= 0) || any(!is.integer(value)))) {
              stop("max.depth should be a positive integer without decimals")
            }
            if (hyperparameter == "max.depth" && tuning_method == "grid_search" && (any(value <= 0) || any(!(value == floor(value))))) {
              stop("max.depth should be a positive integer without decimals")
            }

          } else {
            if (hyperparameter == "num.trees"){
              if(distribution_choice != "constant" && any(!is.integer(pars))) stop("pars should be set as integers for num.trees")
              if(distribution_choice == "uniform" && pars["min"] < 0) warning("min below lower range for num.trees")
              if(distribution_choice == "normal" && (pars["mean"] - pars["sd"]/2 < 0)) warning("mean - sd/2 below lower range for num.trees")
              if(distribution_choice == "lognormal" && (pars["meanlog"] - pars["sdlog"]/2 < 0)) warning("meanlog - sdlog/2 below lower range for num.trees")
              if(distribution_choice == "constant" && (any(value$value < 0))) stop("num.trees should be positive")
            }
            if (hyperparameter == "mtry"){
              if(distribution_choice == "uniform" && pars["max"] > 1) warning("max above upper range for mtry")
              if(distribution_choice == "uniform" && pars["min"] < 0) warning("min below lower range for mtry")
              if(distribution_choice == "normal" && (pars["mean"] + pars["sd"]/2 > 1)) warning("mean + sd/2 above upper range for mtry")
              if(distribution_choice == "normal" && (pars["mean"] - pars["sd"]/2 < 0)) warning("mean - sd/2 below lower range for mtry")
              if(distribution_choice == "lognormal" && (pars["meanlog"] + pars["sdlog"]/2 > 1)) warning("meanlog + sdlog/2 above upper range for mtry")
              if(distribution_choice == "lognormal" && (pars["meanlog"] - pars["sdlog"]/2 < 0)) warning("meanlog - sdlog/2 below lower range for mtry")
              if(distribution_choice == "constant" && (any(value$value < 0) || any(value$value > 1))) stop("mtry should be set in interval [0, 1]")
            }
            if (hyperparameter == "max.depth"){
              if(distribution_choice != "constant" && any(!is.integer(pars))) stop("pars should be set as integers for max.depth")
              if(distribution_choice == "uniform" && pars["min"] < 0) warning("min below lower range for max.depth")
              if(distribution_choice == "normal" && (pars["mean"] - pars["sd"]/2 < 0)) warning("mean - sd/2 below lower range for max.depth")
              if(distribution_choice == "lognormal" && (pars["meanlog"] - pars["sdlog"]/2 < 0)) warning("meanlog - sdlog/2 below lower range for max.depth")
              if(distribution_choice == "constant" && (any(value$value < 0))) stop("max.depth should be positive")
            }
          }
        }

        #XGB Logic
        if (object@ml_algorithm == "xgb") {
          if(tuning_method != "random_search"){
            if (hyperparameter == "eta" && (any(value < 0) || any(value >= 1))) {
              stop("eta should be set in interval [0, 1)")
            }
            if (hyperparameter == "colsample_bytree" && (any(value < 0) || any(value > 1))) {
              stop("colsample_bytree should be set in interval [0, 1]")
            }
            if (hyperparameter == "subsample" && (any(value < 0) || any(value > 1))) {
              stop("subsample should be set in interval [0, 1]")
            }
            if (hyperparameter == "max_depth" && tuning_method == "grid_search" && (any(value <= 0) || any(!(value == floor(value))))) {
              stop("max_depth should be a positive integer without decimals")
            }
            if (hyperparameter == "max_depth" && tuning_method == "bayesian_opt" && (any(value <= 0) || any(!is.integer(value)))) {
              stop("max_depth should be a positive integer without decimals")
            }

          } else {
            if (hyperparameter == "eta"){
              if(distribution_choice == "uniform" && pars["max"] > 1) warning("max above upper range for eta")
              if(distribution_choice == "uniform" && pars["min"] < 0) warning("min below lower range for eta")
              if(distribution_choice == "normal" && (pars["mean"] + pars["sd"]/2 > 1)) warning("mean + sd/2 above upper range for eta")
              if(distribution_choice == "normal" && (pars["mean"] - pars["sd"]/2 < 0)) warning("mean - sd/2 below lower range for eta")
              if(distribution_choice == "lognormal" && (pars["meanlog"] + pars["sdlog"]/2 > 1)) warning("meanlog + sdlog/2 above upper range for eta")
              if(distribution_choice == "lognormal" && (pars["meanlog"] - pars["sdlog"]/2 < 0)) warning("meanlog - sdlog/2 below lower range for eta")
              if(distribution_choice == "constant" && (any(value$value < 0) || any(value$value > 1))) stop("eta should be set in interval [0, 1]")
            }
            if (hyperparameter == "colsample_bytree"){
              if(distribution_choice == "uniform" && pars["max"] > 1) warning("max above upper range for colsample_bytree")
              if(distribution_choice == "uniform" && pars["min"] < 0) warning("min below lower range for colsample_bytree")
              if(distribution_choice == "normal" && (pars["mean"] + pars["sd"]/2 > 1)) warning("mean + sd/2 above upper range for colsample_bytree")
              if(distribution_choice == "normal" && (pars["mean"] - pars["sd"]/2 < 0)) warning("mean - sd/2 below lower range for colsample_bytree")
              if(distribution_choice == "lognormal" && (pars["meanlog"] + pars["sdlog"]/2 > 1)) warning("meanlog + sdlog/2 above upper range for colsample_bytree")
              if(distribution_choice == "lognormal" && (pars["meanlog"] - pars["sdlog"]/2 < 0)) warning("meanlog - sdlog/2 below lower range for colsample_bytree")
              if(distribution_choice == "constant" && (any(value$value < 0) || any(value$value > 1))) stop("colsample_bytree should be set in interval [0, 1]")
            }
            if (hyperparameter == "subsample"){
              if(distribution_choice == "uniform" && pars["max"] > 1) warning("max above upper range for subsample")
              if(distribution_choice == "uniform" && pars["min"] < 0) warning("min below lower range for subsample")
              if(distribution_choice == "normal" && (pars["mean"] + pars["sd"]/2 > 1)) warning("mean + sd/2 above upper range for subsample")
              if(distribution_choice == "normal" && (pars["mean"] - pars["sd"]/2 < 0)) warning("mean - sd/2 below lower range for subsample")
              if(distribution_choice == "lognormal" && (pars["meanlog"] + pars["sdlog"]/2 > 1)) warning("meanlog + sdlog/2 above upper range for subsample")
              if(distribution_choice == "lognormal" && (pars["meanlog"] - pars["sdlog"]/2 < 0)) warning("meanlog - sdlog/2 below lower range for subsample")
              if(distribution_choice == "constant" && (any(value$value < 0) || any(value$value > 1))) stop("subsample should be set in interval [0, 1]")
            }
            if (hyperparameter == "max_depth"){
              if(distribution_choice != "constant" && any(!is.integer(pars))) warning("pars should be set as integers for max_depth")
              if(distribution_choice == "uniform" && pars["min"] < 0) warning("min below lower range for max_depth")
              if(distribution_choice == "normal" && (pars["mean"] - pars["sd"]/2 < 0)) warning("mean - sd/2 below lower range for max_depth")
              if(distribution_choice == "lognormal" && (pars["meanlog"] - pars["sdlog"]/2 < 0)) warning("meanlog - sdlog/2 below lower range for max_depth")
              if(distribution_choice == "constant" && (any(value$value < 0))) stop("max_depth should be positive")
            }
          }
        }


        #NN Logic
        if (object@ml_algorithm == "nn") {
          if(tuning_method != "random_search"){
            if (hyperparameter == "droprate" && (any(value < 0) || any(value >= 1))) {
              stop("droprate should be set in interval [0, 1)")
            }
            if (hyperparameter == "number_of_epochs" && tuning_method == "grid_search" && (any(value <= 0) || any(!(value == floor(value))))) {
              stop("number_of_epochs should be a positive integer without decimals")
            }
            if (hyperparameter == "number_of_epochs" && tuning_method == "bayesian_opt" && (any(value <= 0) || any(!is.integer(value)))) {
              stop("number_of_epochs should be a positive integer without decimals")
            }
            if (hyperparameter == "size_of_batch" && tuning_method == "grid_search" && (any(value <= 0) || any(!(value == floor(value))))) {
              stop("size_of_batch should be a positive integer without decimals")
            }
            if (hyperparameter == "size_of_batch" && tuning_method == "bayesian_opt" && (any(value <= 0) || any(!is.integer(value)))) {
              stop("size_of_batch should be a positive integer without decimals")
            }
          } else {
            if (hyperparameter == "droprate"){
              if(distribution_choice == "uniform" && pars["max"] > 1) warning("max above upper range for droprate")
              if(distribution_choice == "uniform" && pars["min"] < 0) warning("min below lower range for droprate")
              if(distribution_choice == "normal" && (pars["mean"] + pars["sd"]/2 > 1)) warning("mean + sd/2 above upper range for droprate")
              if(distribution_choice == "normal" && (pars["mean"] - pars["sd"]/2 < 0)) warning("mean - sd/2 below lower range for droprate")
              if(distribution_choice == "lognormal" && (pars["meanlog"] + pars["sdlog"]/2 > 1)) warning("meanlog + sdlog/2 above upper range for droprate")
              if(distribution_choice == "lognormal" && (pars["meanlog"] - pars["sdlog"]/2 < 0)) warning("meanlog - sdlog/2 below lower range for droprate")
              if(distribution_choice == "constant" && (any(value$value < 0) || any(value$value > 1))) stop("droprate should be set in interval [0, 1]")
            }
            if (hyperparameter == "number_of_epochs"){
              if(distribution_choice != "constant" && any(!is.integer(pars))) stop("pars should be set as integers for number_of_epochs")
              if(distribution_choice == "uniform" && pars["min"] < 0) warning("min below lower range for number_of_epochs")
              if(distribution_choice == "normal" && (pars["mean"] - pars["sd"]/2 < 0)) warning("mean - sd/2 below lower range for number_of_epochs")
              if(distribution_choice == "lognormal" && (pars["meanlog"] - pars["sdlog"]/2 < 0)) warning("meanlog - sdlog/2 below lower range for number_of_epochs")
              if(distribution_choice == "constant" && (any(value$value < 0))) stop("number_of_epochs should be positive")
            }
            if (hyperparameter == "size_of_batch"){
              if(distribution_choice != "constant" && any(!is.integer(pars))) stop("pars should be set as integers for size_of_batch")
              if(distribution_choice == "uniform" && pars["min"] < 0) warning("min below lower range for size_of_batch")
              if(distribution_choice == "normal" && (pars["mean"] - pars["sd"]/2 < 0)) warning("mean - sd/2 below lower range for size_of_batch")
              if(distribution_choice == "lognormal" && (pars["meanlog"] - pars["sdlog"]/2 < 0)) warning("meanlog - sdlog/2 below lower range for size_of_batch")
              if(distribution_choice == "constant" && (any(value$value < 0))) stop("size_of_batch should be positive")
            }
          }
        }
      }
    }
   }
    return(TRUE)
  }
)


#' @title ml_metabacktest_config Class
#' @description The ml_metabacktest_config class is designed to store and manage a collection of ml_backtest_config objects.
#' @slot configs A list of ml_backtest_config objects.
#' @export
setClass(
  "ml_metabacktest_config",
  slots = list(
    ml_backtest_configs = "list"
  ),
  validity = function(object) {
    if (!all(sapply(object@ml_backtest_configs, function(x) is(x, "ml_backtest_config")))) {
      return("All elements in 'configs' must be of class 'ml_backtest_config'.")
    }

    # Initialize an empty character vector to collect error messages
    errors <- character()

    # Check that all elements are ml_backtest_config objects
    if (!all(sapply(object@ml_backtest_configs, function(x) is(x, "ml_backtest_config")))) {
      errors <- c(errors, "All elements in 'ml_backtest_configs' must be of class 'ml_backtest_config'.")
    }

    # Check for identical objects in ml_backtest_configs
    num_configs <- length(object@ml_backtest_configs)
    for (i in 1:(num_configs - 1)) {
      for (j in (i + 1):num_configs) {
        if (identical(object@ml_backtest_configs[[i]], object@ml_backtest_configs[[j]])) {
          return("Duplicate objects found in 'ml_backtest_configs'. Each configuration must be unique.")
        }
      }
    }

    # Check for duplicate names in ml_backtest_configs
    config_names <- names(object@ml_backtest_configs)
    if (any(duplicated(config_names))) {
      return("Duplicate names found in 'ml_backtest_configs'. Each configuration must have a unique name.")
    }

    # Check that target_fwd_name matches across all configurations
    target_fwd_names <- sapply(object@ml_backtest_configs, function(x) x@target_fwd_name)
    if (length(unique(target_fwd_names)) > 1) {
      errors <- c(errors, "The 'target_fwd_name' must match across all 'ml_backtest_config' elements.")
    }

    # Loop over each ml_backtest_config in the list
    for (i in seq_along(object@ml_backtest_configs)) {
      config <- object@ml_backtest_configs[[i]]

      # Get ml_algorithm
      ml_algorithm <- config@ml_algorithm

      # If tuning_strategy is NULL, skip hyperparameter checks
      if (is.null(config@tuning_strategy)) {
        next
      }
      # Get hyperparameters_list names
      hyperparameters_list <- config@tuning_strategy@hyper_grid_domain@hyperparameter_list
      hyperparameters_names <- names(hyperparameters_list)

      # Expected hyperparameters for each algorithm
      expected_hyperparameters <- switch(ml_algorithm,
                                         "glmnet" = c("alpha", "lambda.min.ratio"),
                                         "rf" = c("mtry", "num.trees", "max.depth", "min.bucket"),
                                         "xgb" = c("min_child_weight", "max_depth", "subsample", "colsample_bytree", "eta", "alpha", "gamma", "nrounds"),
                                         "nn" = c("regularizer_l1", "regularizer_l2", "droprate", "lr", "size_of_batch", "number_of_epochs"),
                                         "ols" = character(0), # OLS does not require hyperparameters
                                         character(0) # default for unrecognized algorithms
      )

      # If ml_algorithm is not recognized and not 'ols', record an error
      if (length(expected_hyperparameters) == 0 && ml_algorithm != "ols") {
        errors <- c(errors, paste0("Unknown ml_algorithm '", ml_algorithm, "' in config ", i, "."))
        next
      }

      # For algorithms other than 'ols', perform hyperparameter checks
      if (ml_algorithm != "ols") {
        # Check for missing hyperparameters
        missing_hyperparameters <- setdiff(expected_hyperparameters, hyperparameters_names)
        if (length(missing_hyperparameters) > 0) {
          errors <- c(errors, paste0("In config ", i, ", missing hyperparameters for algorithm '", ml_algorithm, "': ",
                                     paste(missing_hyperparameters, collapse = ", "), "."))
        }

        # Check for unexpected hyperparameters
        extra_hyperparameters <- setdiff(hyperparameters_names, expected_hyperparameters)
        if (length(extra_hyperparameters) > 0) {
          errors <- c(errors, paste0("In config ", i, ", unexpected hyperparameters for algorithm '", ml_algorithm, "': ",
                                     paste(extra_hyperparameters, collapse = ", "), "."))
        }
      }
    }

    # If any errors were collected, return them
    if (length(errors) > 0) {
      return(paste(errors, collapse = "\n"))
    }

    return(TRUE)
  }
)


#' Define the `refit_ml_model` S4 Class
#'
#' This class represents a refitted machine learning model. It encapsulates the algorithm used, hyperparameters,
#' feature data, target variable, and the fitted model object.
#'
#' @slot ml_algorithm A character string specifying the machine learning algorithm used (e.g., "ols", "glmnet", "rf", "xgb", "nn").
#' @slot best_hyperparameters The chosen hyperparameters relevant to the specified machine learning algorithm.
#' @slot model The fitted model object, which varies based on the algorithm used.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{refit()}}{Refits the model based on the specified algorithm and hyperparameters.}
#'   \item{\code{predict(new_features)}}{Generates predictions using the fitted model on new feature data.}
#' }
#'
#' @export
setClass(
  "refit_ml_model",
  slots = list(
    model = "ANY",
    model_class = "character",
    ml_algorithm = "character",
    best_hyperparameters = "ANY",
    custom_objective = "ANY",
    huber_delta = "numeric",
    keras_architecture_parameters = "ANY"
  )
)



#' S4 Class for Time Series Walk-Forward Validation Results of Machine-Learning Models
#'
#' This S4 class encapsulates the results and parameters from performing walk-forward
#' validation on time series data using machine learning algorithms. It includes
#' information about the model, data, tuning process, and performance metrics.
#'
#' @slot oos_prediction_list A list containing out-of-sample predictions indexed by testing dates.
#' @slot oos_error_list A list of out-of-sample errors indexed by testing dates.
#' @slot oos_y_list A list containing the actual target values for the out-of-sample period, indexed by testing dates.
#' @slot oos_testing_eval_metrics A list of evaluation metrics for the out-of-sample testing samples.
#' @slot final_model The final refitted machine learning model with best hyperparameters found after tuning. Possibly a object of refit_ml_model S4 class.
#' @slot chosen_eval_metric_validation A list of data.frames with the chosen evaluation metric calculated for the hyperparameter grid.
#' @slot best_hyperparameters A data frame containing the best hyperparameters selected during tuning for each rebalancing period.
#' @slot validation_eval_metrics_hyper_choice All evaluation metrics calculated for the set of best hyperparameters.
#' @slot metadata A list containing metadata about the walk-forward validation process. It includes:
#' \itemize{
#'   \item \strong{ml_algorithm}: A character string specifying the machine learning algorithm used.
#'   \item \strong{custom_objective}: A character string indicating the custom loss function applied (e.g., "squared_error").
#'   \item \strong{dates_covered}: A vector of dates representing the time period covered by the analysis.
#'   \item \strong{n_dates}: An integer indicating the total number of dates in the covered period.
#'   \item \strong{training_sample_size}: An integer representing the size of the training samples used.
#'   \item \strong{validation_sample_size}: An integer indicating the size of the validation samples used.
#'   \item \strong{testing_sample_size}: An integer indicating the size of the testing samples used.
#'   \item \strong{dates_testing_sample}: A vector of dates corresponding to the testing samples.
#'   \item \strong{first_rebalance_date}: A date indicating the first date when the model was rebalanced.
#'   \item \strong{rebalance_dates}: A vector of dates when the model was rebalanced.
#'   \item \strong{split_method}: A character string indicating the method used for splitting the data (e.g., "expanding" or "rolling").
#'   \item \strong{ids}: A vector of identifiers from the features data frame.
#'   \item \strong{nobs}: An integer representing the total number of observations in the features data frame.
#'   \item \strong{tickers}: A vector of unique stock tickers from the features data frame.
#'   \item \strong{n_stocks}: An integer indicating the number of unique stocks in the features data frame.
#'   \item \strong{target_fwd_name}: A character string naming the target variable for forward prediction.
#'   \item \strong{target_fwd}: A vector of forward target values.
#'   \item \strong{target_workflow}: A description of the workflow used for the target variable.
#'   \item \strong{target_object}: A character string capturing the name of the target data frame passed to the function.
#'   \item \strong{features}: A character vector of feature names extracted from the features data frame.
#'   \item \strong{features_workflow}: A description of the workflow used for the features.
#'   \item \strong{features_object}: A character string capturing the name of the features data frame passed to the function.
#'   \item \strong{tuning_method}: A character string indicating the method used for hyperparameter tuning (e.g., "grid_search").
#'   \item \strong{n_iter}: An integer specifying the number of iterations for tuning methods that require it.
#'   \item \strong{k_iter}: An integer indicating the number of times to sample the evaluation function during tuning.
#'   \item \strong{acq}: A character string specifying the acquisition function used in Bayesian optimization.
#'   \item \strong{init_points}: An integer indicating the number of initial random points for Bayesian optimization.
#'   \item \strong{hyper_grid_domain_list}: A list containing hyperparameter definitions for tuning.
#'   \item \strong{chosen_eval_metric}: A character string representing the evaluation metric chosen for optimization.
#'   \item \strong{huber_delta}: A numeric value indicating the boundary for the Huber loss function.
#'   \item \strong{quantile_tau}: A numeric value representing the target quantile for quantile loss.
#'   \item \strong{early_stop}: A criteria indicating if early stopping was used during training.
#'   \item \strong{keras_architecture_parameters}: A list containing parameters for the Keras model architecture.
#'   \item \strong{completion_time}: The system time when the validation process was completed.
#'   \item \strong{elapsed_time}: A numeric value representing the total time taken for the validation process.
#'   \item \strong{parallel}: A logical value indicating whether the process was run in parallel (TRUE or FALSE).
#'   \item \strong{call}: The matched call used to create the S4 object, capturing the function call context.
#' }
#'
#'
#' @return An S4 object of class `ml_backtest_results` containing all the specified results and metadata.
#'
#'
#'@export
setClass(
  "ml_backtest_results",
  slots = list(
    oos_prediction_list = "list",
    oos_error_list = "list",
    oos_y_list = "list",
    oos_testing_eval_metrics = "data.frame",
    final_model = "refit_ml_model",
    chosen_eval_metric_validation = "ANY",
    best_hyperparameters = "ANY",
    validation_eval_metrics_hyper_choice = "ANY",
    metadata = "list"
  )
)

#' @title ml_metabacktest_results Class
#' @description The ml_metabacktest_results class is designed to store and manage a collection of `ml_backtest_results` objects.
#' @slot results A list of `ml_backtest_results` objects.
#' @export
setClass(
  "ml_metabacktest_results",
  slots = list(
    ml_backtest_results = "list"
  ),
  validity = function(object) {
    if (!all(sapply(object@ml_backtest_results, function(x) is(x, "ml_backtest_results")))) {
      return("All elements in 'results' must be of class 'ml_backtest_results'.")
    }
  })





#' Define the `portfolio_policies` S4 Class
#'
#' S4 class to represent a set of portfolio policies, including liquidity, turnover, concentration and signal selection constraints.
#'
#' @slot liquidity_constraint_policy A list that contains liquidity constraint policies.
#'   It must include a character element named `liquidity_floor_rule` and one or more rules,
#'   each represented as a list with `liquidity_classification` and `liquidity_cap`.
#' @slot signal_selection_policy A list describing signal selection policy.
#' @slot turnover_constraint_policy A list describing turnover constraint policy.
#' @slot concentration_constraint_policy A list describing concentration constraint policy.
#' @slot liquidity_floor_cutoffs A list for liquidity floor cutoffs.
setClass("portfolio_policies",
         slots = list(
           liquidity_constraint_policy = "list",
           signal_selection_policy = "list",
           turnover_constraint_policy = "list",
           concentration_constraint_policy = "list",
           liquidity_floor_cutoffs = "list"
         )
)


#################################################################

##########################
#########Methods#########
##########################


##########################################################


#' Accessor Methods for meta_dataframe
#'
#' These methods are used to access components of a `meta_dataframe` object.
#'
#' @param object An object of class `meta_dataframe`.
#' @return The respective slot of the `meta_dataframe` object.
#' @name meta_dataframe_accessors
#' @export
setGeneric("get_data", function(object) standardGeneric("get_data"))

#' @export
setMethod("get_data", "meta_dataframe", function(object) {
  return(object@data)
})

#' @export
setGeneric("get_workflow", function(object) standardGeneric("get_workflow"))

#' @export
setMethod("get_workflow", "meta_dataframe", function(object) {
  return(object@workflow)
})

#' @export
setMethod(
  "as.data.frame", "meta_dataframe", function(x) {
    as.data.frame(x@data)
  }
)


#' Accessor Methods for refit_ml_model
#'
#' These methods are used to access components of a `refit_ml_model` object.
#'
#' @param object An object of class `refit_ml_model`.
#' @return The respective slot of the `refit_ml_model` object.
#' @name refit_ml_model_accessors
#' @export
setGeneric("get_ml_algorithm", function(object) standardGeneric("get_ml_algorithm"))

#' @export
setMethod("get_ml_algorithm", "refit_ml_model", function(object) {
  return(object@ml_algorithm)
})

#' @export
setGeneric("get_best_hyperparameters", function(object) standardGeneric("get_best_hyperparameters"))

#' @export
setMethod("get_best_hyperparameters", "refit_ml_model", function(object) {
  return(object@best_hyperparameters)
})

#' @export
setGeneric("get_model", function(object) standardGeneric("get_model"))

#' @export
setMethod("get_model", "refit_ml_model", function(object) {
  return(object@model)
})

#' Accessor Methods for ml_backtest_results
#'
#' These methods are used to access various components of an `ml_backtest_results` object.
#'
#' @param object An object of class `ml_backtest_results`.
#' @return The respective slot of the `ml_backtest_results` object.
#' @name ml_backtest_results_accessors
#' @export
setGeneric("get_oos_prediction_list", function(object) standardGeneric("get_oos_prediction_list"))

#' @export
setMethod("get_oos_prediction_list", "ml_backtest_results", function(object) {
  return(object@oos_prediction_list)
})

#' @export
setGeneric("get_oos_error_list", function(object) standardGeneric("get_oos_error_list"))

#' @export
setMethod("get_oos_error_list", "ml_backtest_results", function(object) {
  return(object@oos_error_list)
})

#' @export
setGeneric("get_oos_y_list", function(object) standardGeneric("get_oos_y_list"))

#' @export
setMethod("get_oos_y_list", "ml_backtest_results", function(object) {
  return(object@oos_y_list)
})

#' @export
setGeneric("get_oos_testing_eval_metrics", function(object) standardGeneric("get_oos_testing_eval_metrics"))

#' @export
setMethod("get_oos_testing_eval_metrics", "ml_backtest_results", function(object) {
  return(object@oos_testing_eval_metrics)
})

#' @export
setGeneric("get_final_model", function(object) standardGeneric("get_final_model"))

#' @export
setMethod("get_final_model", "ml_backtest_results", function(object) {
  return(object@final_model)
})

#' @export
setGeneric("get_chosen_eval_metric_validation", function(object) standardGeneric("get_chosen_eval_metric_validation"))

#' @export
setMethod("get_chosen_eval_metric_validation", "ml_backtest_results", function(object) {
  return(object@chosen_eval_metric_validation)
})

#' @export
setMethod("get_best_hyperparameters", "ml_backtest_results", function(object) {
  return(object@best_hyperparameters)
})

#' @export
setGeneric("get_validation_eval_metrics_hyper_choice", function(object) standardGeneric("get_validation_eval_metrics_hyper_choice"))

#' @export
setMethod("get_validation_eval_metrics_hyper_choice", "ml_backtest_results", function(object) {
  return(object@validation_eval_metrics_hyper_choice)
})

#' @export
setGeneric("get_metadata", function(object) standardGeneric("get_metadata"))

#' @export
setMethod("get_metadata", "ml_backtest_results", function(object) {
  return(object@metadata)
})


#' @export
setMethod("as.list", "ml_backtest_results", function(x) {
  # Get the names of all slots
  slot_names <- slotNames(x)

  # Create a list to hold the extracted slots, ignoring NULL slots
  slot_list <- lapply(slot_names, function(slot) {
    value <- slot(x, slot)  # Extract the slot using the slot name
    if (!is.null(value)) {
      return(value)  # Return the value only if it's not NULL
    }
    return(NULL)  # Return NULL if the slot is NULL
  })

  # Filter out NULL entries
  non_null_indices <- which(!sapply(slot_list, is.null))
  slot_list <- slot_list[non_null_indices]

  # Set names for the list elements based on non-NULL slots
  names(slot_list) <- slot_names[non_null_indices]

  return(slot_list)
})


#' @title Get Hyperparameter Tuning Strategy
#' @description Accessor function to retrieve the hyperparameter tuning strategy from an ml_backtest_config object.
#'
#' @param object An ml_backtest_config object.
#'
#' @return The `tuning_strategy` slot of the `ml_backtest_config` object.
#' @export
setGeneric("get_tuning_strategy", function(object) {
  standardGeneric("get_tuning_strategy")
})

#' @rdname get_tuning_strategy
#' @export
setMethod("get_tuning_strategy", "ml_backtest_config", function(object) {
  return(object@tuning_strategy)
})


#' @rdname get_tuning_strategy
#' @export
setMethod("get_tuning_strategy", "ml_metabacktest_config", function(object) {
  return(lapply(object@ml_backtest_configs, function(config) get_tuning_strategy(config)))
})


#' @title Get Hyperparameter Grid Domain
#' @description Accessor function to retrieve the hyperparameter grid domain.
#'
#' @param object An ml_backtest_config or tuning_strategy object
#'
#' @return The `hyper_grid_domain` object stored in the `tuning_strategy`.
setGeneric("get_hyper_grid_domain", function(object) {
  standardGeneric("get_hyper_grid_domain")
})

#' @rdname get_hyper_grid_domain
setMethod("get_hyper_grid_domain", "ml_backtest_config", function(object) {
  if(is.null(object@tuning_strategy)){
    stop("tuning_strategy not avaiable.")
  } else {
    return(object@tuning_strategy@hyper_grid_domain)
  }
})

setMethod("get_hyper_grid_domain", "tuning_strategy", function(object) {
    return(object@hyper_grid_domain)
})

#' @rdname get_hyper_grid_domain
#' @export
setMethod("get_hyper_grid_domain", "ml_metabacktest_config", function(object) {
  return(lapply(object@ml_backtest_configs, function(config) {
    if(is.null(config@tuning_strategy)) {
      stop("tuning_strategy not available for one of the ml_backtest_config objects.")
    } else {
      return(config@tuning_strategy@hyper_grid_domain@hyperparameter_list)
    }
  }))
})


#' @title Convert Keras Architecture Parameters to List
#' @description Converts a `keras_architecture_parameters` object to a list.
#'
#' This method extracts the relevant attributes from a `keras_architecture_parameters`
#' object and returns them as a list, making it easier to work with the parameters
#' in a more general R context.
#'
#' @param x A `keras_architecture_parameters` object that contains the architecture parameters
#'          for a Keras model.
#'
#' @return A list containing the following elements:
#' \item{units}{The number of units in the layer.}
#' \item{n_layers}{The number of layers in the architecture.}
#' \item{activation}{The activation function used in the architecture.}
#' \item{nn_optimizer}{The optimizer used for training the neural network.}
#' \item{batch_norm_option}{Indicates if batch normalization is applied.}
#'
#' @export
setMethod("as.list", "keras_architecture_parameters", function(x) {
  list(
    units = x@units,
    n_layers = x@n_layers,
    activation = x@activation,
    nn_optimizer = x@nn_optimizer,
    batch_norm_option = x@batch_norm_option
  )
})




#' @title Accessor for Liquidity Constraint Policy
#' @description Retrieves the liquidity constraint policy from a `portfolio_policies` object.
#' @param portfolio_policies_obj A `portfolio_policies` object.
#' @return The liquidity constraint policy list.
#' @export
setGeneric("get_liquidity_constraint_policy", function(portfolio_policies_obj) {
  standardGeneric("get_liquidity_constraint_policy")
})

#' @export
setMethod("get_liquidity_constraint_policy", "portfolio_policies", function(portfolio_policies_obj) {
  return(portfolio_policies_obj@liquidity_constraint_policy)
})

#' @title Accessor for Signal Selection Policy
#' @description Retrieves the signal selection policy from a `portfolio_policies` object.
#' @param portfolio_policies_obj A `portfolio_policies` object.
#' @return The signal selection policy list.
#' @export
setGeneric("get_signal_selection_policy", function(portfolio_policies_obj) {
  standardGeneric("get_signal_selection_policy")
})

#' @export
setMethod("get_signal_selection_policy", "portfolio_policies", function(portfolio_policies_obj) {
  return(portfolio_policies_obj@signal_selection_policy)
})

#' @title Accessor for Turnover Constraint Policy
#' @description Retrieves the turnover constraint policy from a `portfolio_policies` object.
#' @param portfolio_policies_obj A `portfolio_policies` object.
#' @return The turnover constraint policy list.
#' @export
setGeneric("get_turnover_constraint_policy", function(portfolio_policies_obj) {
  standardGeneric("get_turnover_constraint_policy")
})

#' @export
setMethod("get_turnover_constraint_policy", "portfolio_policies", function(portfolio_policies_obj) {
  return(portfolio_policies_obj@turnover_constraint_policy)
})

#' @title Accessor for Concentration Constraint Policy
#' @description Retrieves the concentration constraint policy from a `portfolio_policies` object.
#' @param portfolio_policies_obj A `portfolio_policies` object.
#' @return The concentration constraint policy list.
#' @export
setGeneric("get_concentration_constraint_policy", function(portfolio_policies_obj) {
  standardGeneric("get_concentration_constraint_policy")
})

#' @export
setMethod("get_concentration_constraint_policy", "portfolio_policies", function(portfolio_policies_obj) {
  return(portfolio_policies_obj@concentration_constraint_policy)
})

#' @title Accessor for Liquidity Floor Cutoffs
#' @description Retrieves the liquidity floor cutoffs from a `portfolio_policies` object.
#' @param portfolio_policies_obj A `portfolio_policies` object.
#' @return The liquidity floor cutoffs list.
#' @export
setGeneric("get_liquidity_floor_cutoffs", function(portfolio_policies_obj) {
  standardGeneric("get_liquidity_floor_cutoffs")
})

#' @export
setMethod("get_liquidity_floor_cutoffs", "portfolio_policies", function(portfolio_policies_obj) {
  return(portfolio_policies_obj@liquidity_floor_cutoffs)
})




