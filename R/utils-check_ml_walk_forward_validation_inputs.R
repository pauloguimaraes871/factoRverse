#' Perform validation checks on inputs for ML workflow
#'
#' This function validates and checks various inputs required for a machine learning workflow.
#'
#' @param features_m_df A data frame or matrix containing features data.
#' @param target_m_df A data frame or matrix containing target variable data.
#' @param dates_m_vector A vector of dates corresponding to the data.
#' @param training_sample_size Numeric, size of the training sample.
#' @param target_fwd Numeric, forward looking target.
#' @param target_fwd_name Character, name of the forward looking target.
#' @param validation_sample_size Numeric, size of the validation sample.
#' @param rebalancing_months Numeric, number of months for rebalancing.
#' @param split_method Character, method of data splitting (currently only "expanding" is supported).
#' @param ml_algorithm Character, choice of machine learning algorithm ("ols", "glmnet", "rf", "xgb", "nn").
#' @param custom_objective Character, custom objective function for loss (for "xgb" and "nn" algorithms).
#' @param chosen_eval_metric Character, chosen evaluation metric ("rmse", "mae", "cp", "rss", "mphe", "mpe", "hr", "mape").
#' @param huber_delta Numeric, delta parameter for Huber loss (for "pseudo_huber_error" custom objective).
#' @param quantile_tau Numeric, tau parameter for quantile loss (for "quantile_error" custom objective).
#' @param hyper_grid_domain_list List, domain list of hyperparameters for tuning.
#' @param tuning_method Character, method of hyperparameter tuning ("random_search", "grid_search", "bayesian_opt").
#' @param n_iter number of iterations for tuning.
#' @param k_iter number of iterations for k-fold cross-validation.
#' @param acq Character, acquisition function for Bayesian optimization ("ucb", "ei", "poi").
#' @param init_points Numeric, number of initial points for Bayesian optimization.
#' @param early_stop Numeric, number of epochs for early stopping (for "xgb" and "nn" algorithms).
#' @param keras_architecture_parameters List, containing units (numeric), n_layers (numeric between 1 and 5), activation_function and nn_optimizer ("Adam" or "RMSProp")
#' @param show_plots Logical, whether to show diagnostic plots.
#' @param verbose Logical, whether to print verbose output.
#' @param parallel Logical, whether to use parallel computation.
#'
#' @return NULL. This function is used for validation and does not return a value; it stops on errors.
#'
#' @details
#' This function performs comprehensive validation checks on various inputs required for a machine learning workflow.
#' It validates data formats, correctness of hyperparameters, consistency of dates, and other specific requirements for
#' different machine learning algorithms.
#'
#' @examples
#' \dontrun{
#' check_inputs_ml_wf_val(features_m_df, target_m_df, dates_m_vector, training_sample_size, target_fwd,
#'                       target_fwd_name, validation_sample_size, rebalancing_months, split_method,
#'                       ml_algorithm, custom_objective, chosen_eval_metric, huber_delta, quantile_tau,
#'                       hyper_grid_domain_list, tuning_method, n_iter, k_iter, acq,
#'                       init_points, early_stop, units, n_layers, activation, batch_norm_option,
#'                       nn_optimizer, size_of_batch, max_number_of_epochs, show_plots, verbose, parallel)
#' }
#'
#' @export
#'
#' @references
#' For more information on machine learning algorithms and their usage, refer to appropriate documentation.
#'
#' @keywords internal validation machine-learning
#'
#'
check_inputs_ml_wf_val <- function(
    features_m_df, target_m_df, dates_m_vector, training_sample_size, target_fwd, target_fwd_name,
    validation_sample_size, rebalancing_months, split_method, ml_algorithm,
    custom_objective, chosen_eval_metric, huber_delta, quantile_tau,
    hyper_grid_domain_list, tuning_method, n_iter, k_iter, acq, init_points, early_stop,
    keras_architecture_parameters,
    show_plots, verbose, parallel
){

  ###Initial Checks###
  ################

  #Structures
  ############################################################################################

  #Check for correct format in features_m_df
      if(!(is.data.frame(features_m_df))){
        stop("features_m_df should be a data_frame.")
      }

      if(!all(c("id", "tickers", "dates") %in% colnames(features_m_df))){
        stop("features_m_df should have id, tickers and dates columns.")
      } else {}

      if(any(sapply(features_m_df[,-c(1:3)], function(x) any(is.na(as.numeric(as.character(x))))))
         ){
        stop("features_m_df should contain only numeric columns with non-NAs.")
      }

      suppressWarnings(
        if(any(!is.na(as.numeric(features_m_df$tickers)))){
        stop("tickers in features_m_df must be character.")
      })

       if(is.factor(features_m_df$dates)){
          warning("dates in target_m_df/features_m_df should preferably be of class Date.")
        }

      if(all(!is.factor(features_m_df$dates),
            any(!lubridate::is.Date(features_m_df$dates)) ||
            any(is.na(as.Date(features_m_df$dates, format = "%Y-%m-%d", tryFormats = c("%Y-%m-%d")))))){
        stop("dates in features_m_df must be a date object with format %Y-%m-%d.")
      }





  #Check for correct format in dates_m_vector
      if(!inherits(dates_m_vector, "Date")){
        stop("dates_m_vector must be a date object with format %Y-%m-%d")
      }

      #Check for correct format in dates_m_vector
      if(any(!lubridate::is.Date(dates_m_vector)) ||
         any(is.na(as.Date(dates_m_vector, format = "%Y-%m-%d", tryFormats = c("%Y-%m-%d"))))){
        stop("dates_m_vector must be a date object with format %Y-%m-%d")
      } else {}

  #Check structure of dates_m_vector

      if(length(dates_m_vector) <= target_fwd){
        stop("dates_m_vector should have more dates than target_fwd")
      } else {}

      if(!all(dates_m_vector == dates_m_vector[order(dates_m_vector)])){
        stop("dates_m_vector should be in ascending chronological order")
      } else {}

      if(!all(dates_m_vector == dates_m_vector[order(dates_m_vector)])){
        stop("dates_m_vector should be in ascending chronological order")
      } else {}

  #Check structure of dates_m_vector and features_m_df$dates
      if(!all(as.character(dates_m_vector) %in% unique(as.character(features_m_df$dates))) ||
         !all(unique(as.character(features_m_df$dates)) %in% as.character(dates_m_vector))){
        stop("all dates in dates_m_vector must have a correspondence in features_m_df")
      } else {}

      if(any(as.Date(dates_m_vector, format = "%Y-%m-%d") != as.Date(unique(features_m_df$dates), format = "%Y-%m-%d"))){
        stop("dates_m_vector and features_m_df$dates should have same order")
      }


  #Check for correct format in target_m_df
      if(!(is.matrix(target_m_df) | is.data.frame(target_m_df))){
        stop("target_m_df should be a data.frame or a matrix.")
      }

      if(!all(c("id", "tickers", "dates") %in% colnames(target_m_df))){
        stop("target_m_df should have id, tickers and dates columns.")
      } else {}

      suppressWarnings(
      if(
        any(!is.na(as.numeric(target_m_df$tickers)))){
        stop("tickers in target_m_df must be character.")
      }
      )

      if(is.factor(target_m_df$dates)){
        warning("dates in target_m_df/features_m_df should preferably be of class Date.")
      }

      if(all(!is.factor(target_m_df$dates),
             any(!lubridate::is.Date(target_m_df$dates)) ||
             any(is.na(as.Date(target_m_df$dates, format = "%Y-%m-%d", tryFormats = c("%Y-%m-%d")))))){
        stop("dates in target_m_df must be a date object with format %Y-%m-%d.")
      }

      dates_allowed_to_be_NA_in_target_m_df <- unique(target_m_df$dates)[(length(unique(target_m_df$dates)) - target_fwd + 1):length(unique(target_m_df$dates))]
      if(length(dates_allowed_to_be_NA_in_target_m_df) > target_fwd){
        stop("number of dates in target_m_df with NAs should be at most equal to target_fwd")
      }

      if(any(is.na(target_m_df[-which(target_m_df$dates %in% dates_allowed_to_be_NA_in_target_m_df),target_fwd_name]))){
         stop("target_m_df before target_fwd periods should contain only numeric columns with non-NAs.")
      }


  #Check structure of dates_m_vector and target_m_df$dates
      if(!all(as.character(dates_m_vector) %in% unique(as.character(target_m_df$dates))) ||
         !all(unique(as.character(target_m_df$dates)) %in% as.character(dates_m_vector))){
        stop("all dates in dates_m_vector must have a correspondence in target_m_df")
      } else {}




  #Check structure between target_m_df and feature_m_df
      if(nrow(target_m_df) != nrow(features_m_df)){
        stop("features_m_df and target_m_df must possess same number of rows.")
      }

      if(any(target_m_df$id != features_m_df$id)){
        stop("id in features_m_df and in target_m_df must match.")
      }

      if(any(target_m_df$tickers != features_m_df$tickers)){
        stop("tickers in features_m_df and in target_m_df must match.")
      }

      if(any(target_m_df$dates != features_m_df$dates)){
        stop("dates in features_m_df and in target_m_df must match.")
      }



  #Check structure of rebalancing_months
    if(!is.numeric(rebalancing_months)){
      stop("rebalancing_months should be numeric.")
    }

  #Check structure of target_fwd
    if(!(is.numeric(target_fwd))){
      stop("target_fwd should be numeric.")
    }

  #Check structure of target_fwd_name
    if(!(is.character(target_fwd_name))){
      stop("target_fwd_name should be character.")
    }

  #Check structure of training_sample_size and validation_sample_size
    if(!(is.numeric(training_sample_size))){
      stop("training_sample_size should be numeric.")
    }

    if(!(is.numeric(validation_sample_size))){
      stop("validation_sample_size should be numeric.")
    }

    if(ml_algorithm == "ols" & validation_sample_size != 0){
      stop("ols do not support validation split.")
    } else {}

  #Check structure of split_method
  if(split_method != "expanding"){
    stop("split_method should be expanding.")
  }


  #####################################################################################

  #Validation Schema
  #Check for correct choice in chosen_eval_metric
    if(!is.null(chosen_eval_metric)){
      if(!chosen_eval_metric %in% c("rmse", "mae", "cp", "rss", "mphe", "mpe", "hr", "mape")){
        stop("chosen_eval_metric choice not supported.")
      } else {}
    } else {}

  #Check for correct format of custom_eval/loss parameters
  if(quantile_tau <= 0 || quantile_tau >= 1){
    stop("quantile_tau should be > 0 and less than 1.")
  } else {}

  if(!is.numeric(huber_delta)){
    stop("huber_delta should be numeric.")
  } else {}



  #Check for correct hyperparameters names in hyper_grid_domain_list
      #GLMNET
      if(ml_algorithm == "glmnet" && !all(names(hyper_grid_domain_list) == c("alpha", "lambda.min.ratio"))){
        stop("hyperparameters do not match ml_algorithm choice")
      } else {}

      #RF
      if(ml_algorithm == "rf" && !all(names(hyper_grid_domain_list) == c("mtry", "num.trees", "max.depth", "min.bucket"))){
        stop("hyperparameters do not match ml_algorithm choice")
      } else {}

      #XGB
      if(ml_algorithm == "xgb" && !all(names(hyper_grid_domain_list) == c("min_child_weight", "max_depth", "subsample", "colsample_bytree",
                                                                          "eta", "alpha", "gamma", "nrounds"))){
        stop("hyperparameters do not match ml_algorithm choice")
      } else {}

      #NN
      if(ml_algorithm == "nn" && !all(names(hyper_grid_domain_list) == c("regularizer_l1", "regularizer_l2", "droprate", "lr", "size_of_batch", "number_of_epochs"))){
        stop("hyperparameters do not match ml_algorithm choice")
      } else {}



  #Check for valid format in tuning method
  if(ml_algorithm != "ols" && !tuning_method %in% c("random_search", "grid_search", "bayesian_opt")){
    stop("tuning_method should be one of random_search, grid_search or bayesian_opt.")
  } else {}

  #Check for correct format in case tuning method is grid_search
  if(ml_algorithm != "ols" && tuning_method == c("grid_search")){
    if(any(
      #Check if hyper_grid_domain_list is a list
      !(class(hyper_grid_domain_list) == "list"),
      #Check if hyper_grid_domain_list is a list of vectors
      !all(sapply(hyper_grid_domain_list, function(x) is.vector(x))),
      #Check if hyper_grid_domain_list contains numeric values
      !all(sapply(hyper_grid_domain_list, function(x) is.numeric(x)))
    )
    ){
      stop("hyper_grid_domain_list not in correct format for grid_search tuning.")
    } else {}
  } else {}

  if(all(ml_algorithm != "ols", tuning_method == "grid_search",!is.null(n_iter))){
    warning("When tuning_method is grid_search, hyperparameters are combined exhaustively. Ignoring n_iter value")
  }

  #Check for correct format in case tuning method is random_search
  if(ml_algorithm != "ols" && tuning_method == c("random_search")){


  tryCatch({
    if(any(
      #Check if hyper_grid_domain_list is a list
      !(class(hyper_grid_domain_list) == "list"),
      #Check if hyper_grid_domain_list is a list of lists
      !all(sapply(hyper_grid_domain_list, function(x) is.list(x))),
      #Check if every element contains data for distribution choice and pars
      !all(sapply(hyper_grid_domain_list, function(x) names(x) %in% c("distribution_choice", "pars", "value"))),
      #Check if distribution choices match allowed choices
      !all(sapply(hyper_grid_domain_list, function(x) all(x$distribution_choice %in% c("normal", "uniform", "lognormal", "constant")))),
      #Check if pars are numeric and not NA
      !all(sapply(hyper_grid_domain_list, function(x) all(is.numeric(x$pars) | is.numeric(x$value), !is.na(x$pars) | !is.na(x$value)))),
      #Check if pars are named
      !all(sapply(hyper_grid_domain_list, function(x) ifelse(x$distribution_choice != "constant", all(!is.null(names(x$pars))), any(names(x) %in% c("value"))))),
      #Check if pars match each possible distribution choice
      !all(sapply(hyper_grid_domain_list, function(x) ifelse(x$distribution_choice == "uniform", all(names(x$pars) == c("min", "max")),
                                                             ifelse(x$distribution_choice == "normal", all(names(x$pars) == c("mean", "sd")),
                                                                    ifelse(x$distribution_choice == "lognormal", all(names(x$pars) == c("meanlog", "sdlog")),
                                                                           is.numeric(x$value))))))
    )
    ){
      stop("hyper_grid_domain_list not in correct format for random_search tuning.")
    }

  }, error = function(e){
    stop("hyper_grid_domain_list not in correct format for random_search tuning.")
  })


    if(!is.numeric(n_iter)){
      stop("n_iter must be numeric.")
    }

  } else {}



  #Check for correct format in case tuning method is Bayesian Optimization
  if(ml_algorithm != "ols" && tuning_method == c("bayesian_opt")){
    if(any(
      #Check if hyper_grid_domain_list is a list
      !is.list(hyper_grid_domain_list),
      #Check if hyper_grid_domain_list elements have length of 2 (boundaries)
      !all(sapply(hyper_grid_domain_list, function(x) length(x) == 2)),
      #Check if hyper_grid_domain_list elements are vectors
      !all(sapply(hyper_grid_domain_list, function(x) is.vector(x))),
      #Check if hyper_grid_domain_list contains numeric values
      !all(sapply(hyper_grid_domain_list, function(x) is.numeric(x)))
    )
    ){
      stop("hyper_grid_domain_list not in correct format for bayesian_opt tuning.")
    } else {}

    if(!acq %in% c("ucb", "ei", "poi")){
      stop("acq should be one of ucb, ei or poi")
    } else {}
    if(any(!is.numeric(init_points), !is.numeric(n_iter), !is.numeric(k_iter))){
      stop("n_iter, k_iter and init_points must be numeric.")
    } else {}
    if(init_points <= length(hyper_grid_domain_list)){
      stop("init_points must be greater than number of hyperparameters")
    }
    if(n_iter < k_iter){
      stop("n_iter must be greater than k_iter")
    }





  } else {}



  #ML algorithms
  ################
    #Check for correct choice in ml_algorithm
        if(!ml_algorithm %in% c("ols", "glmnet", "rf", "xgb", "nn")){
          stop("ml_algorithm choice not supported.")
        } else {}

    #Check for correct choice in custom_objective
    if(all(!ml_algorithm %in% c("xgb", "nn") && custom_objective != "squared_error")){
      stop("Custom objective functions are only allowed for xgb or nn ml_algorithm choices")
    }

    if(!custom_objective %in% c("squared_error", "pseudo_huber_error", "absolute_error")){
        stop("Possible choices for custom_objective are squared_error, pseudo_huber_error and absolute_error")
      }


  #Check for correct choice in early_stop
  if(all(!is.null(early_stop), !ml_algorithm %in% c("xgb", "nn"))){
    stop("Early stop only allowed for xgb or nn ml_algorithm choices")
  }

  if(ml_algorithm == "nn"){

    if(is.data.frame(keras_architecture_parameters) || !is.list(keras_architecture_parameters) ||
       !all(names(keras_architecture_parameters) == c("units", "n_layers", "activation", "nn_optimizer", "batch_norm_option"))){
      stop("keras_architecture_parameters should be a list with units, n_layers, activation, nn_optimizer and batch_norm_option elements")
    }

    if(!all(is.numeric(keras_architecture_parameters$units))){
      stop("units should be numeric")
    }

    if(!keras_architecture_parameters$n_layers %in% c(1,2,3,4,5) || length(keras_architecture_parameters$n_layers) > 1){
      stop("n_layers should be an integer between 1 and 5.")
    }

    if(!all(keras_architecture_parameters$activation %in% c("relu", "sigmoid", "softmax", "softplus", "tanh", "leaky_relu"))){
      stop("activation should be one of relu, sigmoid, softmax, softplus, tanh or leaky_relu.")
    }

    if(length(keras_architecture_parameters$units) != keras_architecture_parameters$n_layers ||
       length(keras_architecture_parameters$activation) != keras_architecture_parameters$n_layers ||
       length(keras_architecture_parameters$batch_norm_option) != keras_architecture_parameters$n_layers
    ){
      stop("length of units, activation and batch_norm_option should match n_layers")
    }

    if(!keras_architecture_parameters$nn_optimizer %in% c("Adam", "RMSProp")){
      stop("nn_optimizer should be Adam or RMSProp.")
    }


    if(!all(is.logical(keras_architecture_parameters$batch_norm_option))){
      stop("batch_norm_option should be logical")
    }

    if(parallel == TRUE){
      warning("keras models have some limitations regarding parallel computations. Use with care.")
    }



  }

  ################



  #Hyper domain
  ##################
      #Check for correct domains in hyper_grid_domain_list

      #GLMNET
      ###############
      if(ml_algorithm == "glmnet"){
        #alpha
        ##########
        if(tuning_method == "random_search"){
          if(hyper_grid_domain_list$alpha$distribution_choice == "constant"){
            hyper_domain <- hyper_grid_domain_list$alpha$value
          } else {
            #in case of random
            hyper_domain <- range(hyper_grid_domain_list$alpha$pars)
          }
        } else {
          #bayesian opt or grid search
          hyper_domain <- hyper_grid_domain_list$alpha
        }
        #Check domain
        if(!all(0 <= hyper_domain, hyper_domain <= 1)){
          stop("alpha should be set in interval [0,1]")
        } else {}
        ##########

        #lambda.min.ratio
        ##########
        if(tuning_method == "random_search"){
          if(hyper_grid_domain_list$lambda.min.ratio$distribution_choice == "constant"){
            hyper_domain <- hyper_grid_domain_list$lambda.min.ratio$value
          } else {
            #in case of random
            hyper_domain <- range(hyper_grid_domain_list$lambda.min.ratio$pars)
          }
        } else {
          #bayesian opt or grid search
          hyper_domain <- hyper_grid_domain_list$lambda.min.ratio
        }
        #Check domain
        if(!all(0 <= hyper_domain, hyper_domain < 1)){
          stop("lambda.min.ratio should be set in interval [0,1)")
        } else {}
        ##########
      }
      ###############

      #RF
      ###############
      if(ml_algorithm == "rf"){
        #num.trees
        ##########
        if(tuning_method == "random_search"){
          if(hyper_grid_domain_list$num.trees$distribution_choice == "constant"){
            hyper_domain <- hyper_grid_domain_list$num.trees$value
          } else {
            #in case of random
            hyper_domain <- range(hyper_grid_domain_list$num.trees$pars)
          }
        } else {
          #bayesian opt or grid search
          hyper_domain <- hyper_grid_domain_list$num.trees
        }
        #Check domain
        if(tuning_method == "grid_search"){
          if(!all(hyper_domain == floor(hyper_domain))){
            stop("num.trees should have no decimals")
          }
        } else {
          if(!all(is.integer(hyper_domain))){
            stop("num.trees should be integer")
          } else {}
        }

        if(!all(hyper_domain > 0)){
          stop("num.trees should be positive")
        } else {}
        ##########

        #mtry
        ##########
        if(tuning_method == "random_search"){
          if(hyper_grid_domain_list$mtry$distribution_choice == "constant"){
            hyper_domain <- hyper_grid_domain_list$mtry$value
          } else {
            #in case of random
            hyper_domain <- range(hyper_grid_domain_list$mtry$pars)
          }
        } else {
          #bayesian opt or grid search
          hyper_domain <- hyper_grid_domain_list$mtry
        }
        #Check domain
        if(!all(0 <= hyper_domain, hyper_domain <= 1)){
          stop("mtry should be set in interval [0,1]")
        } else {}
        ##########

        #max.depth
        ##########
        if(tuning_method == "random_search"){
          if(hyper_grid_domain_list$max.depth$distribution_choice == "constant"){
            hyper_domain <- hyper_grid_domain_list$max.depth$value
          } else {
            #in case of random
            hyper_domain <- range(hyper_grid_domain_list$max.depth$pars)
          }
        } else {
          #bayesian opt or grid search
          hyper_domain <- hyper_grid_domain_list$max.depth
        }
        #Check domain
        if(tuning_method == "grid_search"){
          if(!all(hyper_domain == floor(hyper_domain))){
            stop("max.depth should have no decimals")
          }
        } else {
          if(!all(is.integer(hyper_domain))){
            stop("max.depth should be integer")
          } else {}
        }
        if(!all(hyper_domain > 0)){
          stop("max.depth should be positive")
        } else {}
        ##########

      }
      ###############

      #XGB
      ###############
      if(ml_algorithm == "xgb"){
        #eta
        ##########
        if(tuning_method == "random_search"){
          if(hyper_grid_domain_list$eta$distribution_choice == "constant"){
            hyper_domain <- hyper_grid_domain_list$eta$value
          } else {
            #in case of random
            hyper_domain <- range(hyper_grid_domain_list$eta$pars)
          }
        } else {
          #bayesian opt or grid search
          hyper_domain <- hyper_grid_domain_list$eta
        }
        #Check domain
        if(!all(0 <= hyper_domain, hyper_domain <= 1)){
          stop("eta should be set in interval [0,1]")
        } else {}
        ##########

        #max_depth
        ##########
        if(tuning_method == "random_search"){
          if(hyper_grid_domain_list$max_depth$distribution_choice == "constant"){
            hyper_domain <- hyper_grid_domain_list$max_depth$value
          } else {
            #in case of random
            hyper_domain <- range(hyper_grid_domain_list$max_depth$pars)
          }
        } else {
          #bayesian opt or grid search
          hyper_domain <- hyper_grid_domain_list$max_depth
        }
        #Check domain
        if(tuning_method == "grid_search"){
          if(!all(hyper_domain == floor(hyper_domain))){
            stop("max_depth should have no decimals")
          }
        } else {
          if(!all(is.integer(hyper_domain))){
            stop("max_depth should be integer")
          } else {}
        }
        if(!all(hyper_domain > 0)){
          stop("max_depth should be positive")
        } else {}
        ##########

        #colsample_bytree
        ##########
        if(tuning_method == "random_search"){
          if(hyper_grid_domain_list$colsample_bytree$distribution_choice == "constant"){
            hyper_domain <- hyper_grid_domain_list$colsample_bytree$value
          } else {
            #in case of random
            hyper_domain <- range(hyper_grid_domain_list$colsample_bytree$pars)
          }
        } else {
          #bayesian opt or grid search
          hyper_domain <- hyper_grid_domain_list$colsample_bytree
        }
        #Check domain
        if(!all(0 <= hyper_domain, hyper_domain <= 1)){
          stop("colsample_bytree should be set in interval [0,1]")
        } else {}
        ##########

        #subsample
        ##########
        if(tuning_method == "random_search"){
          if(hyper_grid_domain_list$subsample$distribution_choice == "constant"){
            hyper_domain <- hyper_grid_domain_list$subsample$value
          } else {
            #in case of random
            hyper_domain <- range(hyper_grid_domain_list$subsample$pars)
          }
        } else {
          #bayesian opt or grid search
          hyper_domain <- hyper_grid_domain_list$subsample
        }
        #Check domain
        if(!all(0 <= hyper_domain, hyper_domain <= 1)){
          stop("subsample should be set in interval [0,1]")
        } else {}
        ##########

      }
      ###############

      #NN
      ###############

      if(ml_algorithm == "nn"){
        #droprate
        ##########
        if(tuning_method == "random_search"){
          if(hyper_grid_domain_list$droprate$distribution_choice == "constant"){
            hyper_domain <- hyper_grid_domain_list$droprate$value
          } else {
            #in case of random
            hyper_domain <- range(hyper_grid_domain_list$droprate$pars)
          }
        } else {
          #bayesian opt or grid search
          hyper_domain <- hyper_grid_domain_list$droprate
        }
        #Check domain
        if(!all(0 <= hyper_domain, hyper_domain < 1)){
          stop("droprate should be set in interval [0,1)")
        } else {}
        ##########

        #number_of_epochs
        ##########
        if(tuning_method == "random_search"){
          if(hyper_grid_domain_list$number_of_epochs$distribution_choice == "constant"){
            hyper_domain <- hyper_grid_domain_list$number_of_epochs$value
          } else {
            #in case of random
            hyper_domain <- range(hyper_grid_domain_list$number_of_epochs$pars)
          }
        } else {
          #bayesian opt or grid search
          hyper_domain <- hyper_grid_domain_list$number_of_epochs
        }
        #Check domain
        if(tuning_method == "grid_search"){
          if(!all(hyper_domain == floor(hyper_domain))){
            stop("number_of_epochs should have no decimals")
          }
        } else {
          if(!all(is.integer(hyper_domain))){
            stop("number_of_epochs should be integer")
          } else {}
        }
        if(!all(hyper_domain > 0)){
          stop("number_of_epochs should be positive")
        } else {}
        ##########

        #size_of_batch
        ##########
        if(tuning_method == "random_search"){
          if(hyper_grid_domain_list$size_of_batch$distribution_choice == "constant"){
            hyper_domain <- hyper_grid_domain_list$size_of_batch$value
          } else {
            #in case of random
            hyper_domain <- range(hyper_grid_domain_list$size_of_batch$pars)
          }
        } else {
          #bayesian opt or grid search
          hyper_domain <- hyper_grid_domain_list$size_of_batch
        }
        #Check domain
        if(tuning_method == "grid_search"){
          if(!all(hyper_domain == floor(hyper_domain))){
            stop("size_of_batch should have no decimals")
          }
        } else {
          if(!all(is.integer(hyper_domain))){
            stop("size_of_batch should be integer")
          } else {}
        }
        if(!all(hyper_domain > 0)){
          stop("size_of_batch should be positive")
        } else {}
        ##########


      }
      ###############

}
