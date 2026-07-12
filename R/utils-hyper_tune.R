#' Perform Hyperparameter Tuning for Machine Learning Models
#'
#' This function tunes hyperparameters of machine learning models using grid search, random search, or Bayesian optimization. It evaluates different hyperparameter combinations and returns the best-performing set, selected by maximizing the `Score` column from [calculate_eval_metrics()].
#'
#' @param tuning_method A character string specifying the tuning method. Possible values are `"random_search"`, `"grid_search"`, or `"bayesian_opt"`.
#' @param ml_algorithm A character string indicating the machine learning algorithm to be used.
#' @param target_fwd_name A character string specifying the name of the target variable in forward prediction.
#' @param full_data_training_sample_clean A data frame containing the clean training sample data.
#' @param features_validation_sample A data frame with feature values for validation.
#' @param target_validation_sample A data frame with target values for validation.
#' @param eval_function A function that computes the evaluation metric for given hyperparameters.
#' @param custom_objective_translated An optional custom objective function to be used in tuning.
#' @param chosen_eval_metric_translated The evaluation metric to be used internally for model performance assessment.
#' @param early_stop An integer specifying the number of iterations with no improvement before stopping early.
#' @param chosen_eval_metric A character string specifying the evaluation metric to be used.
#' @param huber_delta A numeric value for the Huber loss delta parameter, applicable if using Huber loss.
#' @param quantile_tau A numeric value for the quantile parameter, applicable if using quantile loss.
#' @param hyper_grid_domain_list A list specifying the domain of hyperparameters to search over.
#' @param n_iter An integer specifying the number of iterations for grid or random search or Bayesian optimization.
#' @param init_points An integer specifying the number of initial random points for Bayesian optimization.
#' @param k_iter An integer specifying the number of iterations for scoring function sampling in Bayesian optimization.
#' @param acq A character string specifying the acquisition function for Bayesian optimization.
#' @param keras_architecture_parameters A list of parameters specifying the architecture of the Keras model.
#' @param parallel A logical indicating whether to evaluate candidates in parallel (future backend).
#' @param verbose A logical indicating whether to print timing and progress information.
#'
#' @return A list containing:
#' \item{chosen_eval_metric_validation_current_date}{A data frame of evaluation metrics for each set of hyperparameters tried.}
#' \item{optimal_hyper}{A named vector of the optimal hyperparameter values found.}
#' \item{validation_eval_metrics_hyper_choice_current_date}{A named vector of evaluation metrics corresponding to the optimal hyperparameter set.}
#'
#' @seealso [set_eval_function()], [calculate_eval_metrics()]
#'
#' @export
hyper_tune <- function(tuning_method, ml_algorithm, target_fwd_name,  #General Parameters
                       full_data_training_sample_clean, features_validation_sample, target_validation_sample, #Data
                       eval_function, custom_objective_translated, #Eval Function and custom obj
                       chosen_eval_metric_translated, early_stop, #Early Stop
                       chosen_eval_metric, huber_delta, quantile_tau,  #Chosen eval metric
                       hyper_grid_domain_list, n_iter, #Grid/Random Searches
                       init_points, k_iter, acq, #Bayesian Optimization
                       keras_architecture_parameters, #Keras Parameters
                       parallel, #Parallelization (default is true with future backend)
                       verbose){ #Verbose

  ###Hyperparameter tuning following grid or random search!
  if(tuning_method %in% c("random_search", "grid_search")){

    #Create expanded_hyper_grid_list
    expanded_hyper_grid_list <- create_expanded_hyper_grid_list(
        hyper_grid_domain_list = hyper_grid_domain_list,
        n_iter = n_iter,
        tuning_method = tuning_method,
        ml_algorithm = ml_algorithm
      )

    #Print start
      if(verbose){
        tictoc::tic(msg = crayon::green("Hyperparameter tuning finished"))
      }

      #Evalute eval metrics to all hyper values
      if(parallel){ #If running in parallel
        hyper_eval <- furrr::future_pmap(expanded_hyper_grid_list, #List of hyperparameters for search
                                         ~ eval_function( #function on which to apply the search
                                           ...,
                                           #Data
                                           full_data_training_sample_clean = full_data_training_sample_clean, #Full data training
                                           features_validation_sample = features_validation_sample, #Features validation
                                           target_validation_sample = target_validation_sample, #Target validation
                                           target_fwd_name = target_fwd_name, #Target fwd

                                           #General Parameters
                                           ml_algorithm = ml_algorithm, #ML Algo
                                           tuning_method = tuning_method, #Tuning method,

                                           #Eval Function Parameteres
                                           chosen_eval_metric = chosen_eval_metric, #Chosen Eval
                                           chosen_eval_metric_translated = chosen_eval_metric_translated, #Chosen Eval Metric for Internal Algo Usage
                                           huber_delta = huber_delta,
                                           quantile_tau = quantile_tau,

                                           #Early stop
                                           early_stop = early_stop, #Early halting

                                           #Custom Loss
                                           custom_objective_translated = custom_objective_translated,

                                           #Keras Network Parameters
                                           keras_architecture_parameters = keras_architecture_parameters,


                                           verbose = FALSE #verbose


                                           #Future implementation
                                           #Functions for custom eval and loss - XGB
                                           #mpe_xgb <- mpe_xgb, #Custom mpe
                                           #rss_xgb = rss_xgb, #Custom rss
                                           #cp_xgb = cp_xgb, #custom cp
                                           #pinball_loss_xgb = pinball_loss_xgb #Custom pinball loss

                                         ),
                                         .options = furrr::furrr_options(seed = TRUE),
                                         .progress = verbose
        )


      } else { #If not running in parallel
        hyper_eval <- purrr::pmap(expanded_hyper_grid_list, #List of hyperparameters for search
                                  eval_function, #function on which to apply the search

                                  #Data
                                  full_data_training_sample_clean = full_data_training_sample_clean, #Full data training
                                  features_validation_sample = features_validation_sample, #Features validation
                                  target_validation_sample = target_validation_sample, #Target validation
                                  target_fwd_name = target_fwd_name, #Target fwd

                                  #General Parameters
                                  ml_algorithm = ml_algorithm, #ML Algo
                                  tuning_method = tuning_method, #Tuning method,

                                  #Eval Function Parameteres
                                  chosen_eval_metric = chosen_eval_metric, #Chosen Eval
                                  chosen_eval_metric_translated = chosen_eval_metric_translated, #Chosen Eval Metric for Early Stop
                                  huber_delta = huber_delta,
                                  quantile_tau = quantile_tau,

                                  #Early stop
                                  early_stop = early_stop, #Early halting

                                  #Custom Loss
                                  custom_objective_translated = custom_objective_translated,

                                  #Keras Network Parameters
                                  keras_architecture_parameters = keras_architecture_parameters,


                                  verbose = FALSE #verbose



                                  #Future implementation
                                  #Functions for custom eval and loss - XGB
                                  #mpe_xgb <- mpe_xgb, #Custom mpe
                                  #rss_xgb = rss_xgb, #Custom rss
                                  #cp_xgb = cp_xgb, #custom cp
                                  #pinball_loss_xgb = pinball_loss_xgb #Custom pinball loss
        )

      }

      #Displays how much time it took for hyper_tuning
      if(verbose){
        tictoc::toc()
      }

      ##########
      #Fill best lambda
      try(expanded_hyper_grid_list$best_lam <- as.numeric(sapply(hyper_eval, function(x) x$best_lam)), silent = TRUE)
      #Fill early stop
      try(expanded_hyper_grid_list$best_iteration <- as.numeric(sapply(hyper_eval, function(x) x$best_iteration)), silent = TRUE) #Fill with best iteration if it's the case

      #Fill chosen_eval_metric_validation
      chosen_eval_metric_validation_current_date <- do.call(data.frame, expanded_hyper_grid_list) #Create data frame to store expanded hyper grid list

      chosen_eval_metric_validation_current_date$chosen_eval_metric <-
        as.numeric(sapply(hyper_eval, #Store a dataframe with rows for combination of hyperparameters and single column to chosen_eval_metric
                          function(x) x[1, which(names(x) == chosen_eval_metric)]))

      ##########


      ###Tune!
      ###Get optimal values
      ####################
      #Get reference
      optimal_hyper_ref <- which.max(sapply(hyper_eval, function(x) x$Score)) #Which position corresponds to the best?
      #Optimal Hyper Choice
      optimal_hyper <- sapply(expanded_hyper_grid_list, function(x) x[[optimal_hyper_ref]]) #Get best values

      #Values for eval_metrics for validation  (optimal hyper choice)
      validation_eval_metrics_hyper_choice_current_date <- hyper_eval[[optimal_hyper_ref]][
        c("Score", "rss", "cp", "rmse", "mae", "mphe", "mpe", "mape", "hr", "mb")
      ] #Place corresponding eval metrics


      ####################

    }


    ###Hyperparameter tuning following Bayesian Optimization!
    if(tuning_method == c("bayesian_opt")){

      ###ParBayesianOptimization is a Suggests dependency (archived on CRAN),
      ###so fail fast with installation guidance when it is missing.
      if (!requireNamespace("ParBayesianOptimization", quietly = TRUE)) {
        stop("The 'ParBayesianOptimization' package is required for tuning_method = 'bayesian_opt'. ",
             "Install it with remotes::install_github('AnotherSamWilson/ParBayesianOptimization').")
      }

      #Apply Bayes Optimization
      if(parallel){

        # Check if doRNG is available (required by doFuture::withDoRNG)
        if (!requireNamespace("doRNG", quietly = TRUE)) {
          stop("The 'doRNG' package is required for parallel Bayesian optimization. Please install it.")
        }

        #Bayesian Optimization
        bayes_opt <- doFuture::withDoRNG(
          ParBayesianOptimization::bayesOpt(
            #Passing variables to set_eval_function
            FUN = eval_function(

              #Data
              full_data_training_sample_clean = full_data_training_sample_clean, #Pass full_data_train
              features_validation_sample = features_validation_sample, #Pass feat_val
              target_validation_sample = target_validation_sample, #Pass target_val
              target_fwd_name = target_fwd_name, #Pass target_fwd

              #General Parameters
              ml_algorithm = ml_algorithm,
              tuning_method = tuning_method,

              #Eval Function Parameters
              chosen_eval_metric = chosen_eval_metric, #Chosen Eval
              chosen_eval_metric_translated = chosen_eval_metric_translated,
              huber_delta = huber_delta, #Huber delta for pseudo huber loss
              quantile_tau = quantile_tau, #Quantile tau for pinball loss

              #Early Stop
              early_stop = early_stop, #Halting criteria

              #Custom Loss
              custom_objective_translated = custom_objective_translated, #Custom objective

              #Keras Network Parameters
              keras_architecture_parameters = keras_architecture_parameters,

              verbose = FALSE

              #Future Implementation:
              #Functions for custom eval and loss - XGB
              #mpe_xgb <- mpe_xgb, #Custom mpe
              #rss_xgb = rss_xgb, #Custom rss
              #cp_xgb = cp_xgb, #custom cp
              #pinball_loss_xgb = pinball_loss_xgb #Custom pinball loss

            ),
            bounds = hyper_grid_domain_list, #Boundaries
            initPoints = init_points, #Number of randomly chosen points to sample the target function before B.O.
            acq = acq, #Acquisition function to be used
            iters.n = n_iter, #Number of times BO is to be repeated
            iters.k = k_iter, #Number of times to sample the scoring function at each epoch. If running in parallel, set iters.k to some multiple of the number of cores designated for the process
            verbose = verbose, #Display msgs?
            parallel = if(ml_algorithm == "nn") FALSE else parallel #Parallel?
          )
        )
      } else { #In case of not PARALLEL

        bayes_opt <-
          ParBayesianOptimization::bayesOpt(
            #Passing variables to set_eval_function
            FUN = eval_function(

              #Data
              full_data_training_sample_clean = full_data_training_sample_clean, #Pass full_data_train
              features_validation_sample = features_validation_sample, #Pass feat_val
              target_validation_sample = target_validation_sample, #Pass target_val
              target_fwd_name = target_fwd_name, #Pass target_fwd

              #General Parameters
              ml_algorithm = ml_algorithm,
              tuning_method = tuning_method,

              #Eval Function Parameters
              chosen_eval_metric = chosen_eval_metric, #Chosen Eval
              chosen_eval_metric_translated = chosen_eval_metric_translated,
              huber_delta = huber_delta, #Huber delta for pseudo huber loss
              quantile_tau = quantile_tau, #Quantile tau for pinball loss

              #Early Stop
              early_stop = early_stop, #Halting criteria

              #Custom Loss
              custom_objective_translated = custom_objective_translated, #Custom objective

              #Keras Network Parameters
              keras_architecture_parameters = keras_architecture_parameters,

              verbose = FALSE

              #Future Implementation:
              #Functions for custom eval and loss - XGB
              #mpe_xgb <- mpe_xgb, #Custom mpe
              #rss_xgb = rss_xgb, #Custom rss
              #cp_xgb = cp_xgb, #custom cp
              #pinball_loss_xgb = pinball_loss_xgb #Custom pinball loss

            ),
            bounds = hyper_grid_domain_list, #Boundaries
            initPoints = init_points, #Number of randomly chosen points to sample the target function before B.O.
            acq = acq, #Acquisition function to be used
            iters.n = n_iter, #Number of times BO is to be repeated
            iters.k = k_iter, #Number of times to sample the scoring function at each epoch. If running in parallel, set iters.k to some multiple of the number of cores designated for the process
            verbose = verbose, #Display msgs?
            parallel = parallel #Parallel?
          )

      }

      ###Store results
      #Hyperparameters
      chosen_eval_metric_validation_current_date <- #Get correct position for list
        as.data.frame(
          bayes_opt$scoreSummary[,which(colnames(bayes_opt$scoreSummary) %in% c(names(hyper_grid_domain_list), "best_lam", "best_iteration"))]
        ) #Create data frame to store combinations of hyperparameters tried

      #Chosen Eval metric
      chosen_eval_metric_validation_current_date$chosen_eval_metric <-
        as.numeric(bayes_opt$scoreSummary[,chosen_eval_metric]) #Store a dataframe with rows for combination of hyperparameters and single column to chosen_eval_metric

      #Create expanded hyper grid list
      expanded_hyper_grid_list <- list() #Create expanded hyper_grid_list as usual
      for (j in seq_len(ncol(dplyr::select(chosen_eval_metric_validation_current_date, -chosen_eval_metric)))){
        expanded_hyper_grid_list[[j]] <- #To each element, a column!
          dplyr::select(chosen_eval_metric_validation_current_date, -chosen_eval_metric)[,j]
      }


      #Get optimal values
      #Optimal Hyper Choice
      optimal_hyper <- unlist(ParBayesianOptimization::getBestPars(bayes_opt)) #Get best values

      #Add best lam
      try(optimal_hyper <- c(optimal_hyper,
                             best_lam = bayes_opt$scoreSummary$best_lam[which.max(bayes_opt$scoreSummary$Score)]),
          silent = TRUE
      )

      #Add best_iteration
      try(optimal_hyper <- c(optimal_hyper,
                             best_iteration = bayes_opt$scoreSummary$best_iteration[which.max(bayes_opt$scoreSummary$Score)]),
                             silent = TRUE
      )

      #Assign val eval of optimal hyper choice
      validation_eval_metrics_hyper_choice_current_date <-
        bayes_opt$scoreSummary[which.max(bayes_opt$scoreSummary$Score),
                               c("Score", "rss", "cp", "rmse", "mae", "mphe", "mpe", "mape", "hr", "mb")] #Take the row that maximizes the score


    }

  #Print Results
  if(verbose){
    cat(paste0("Chosen hyperparameters were: "))
    if(ml_algorithm != "glmnet") cat(paste0(names(hyper_grid_domain_list),":", round(optimal_hyper, 4), sep=" ")) else cat(paste0(c(names(hyper_grid_domain_list), "best_lam"),":", round(optimal_hyper, 4), sep=" "))
    cat("\n")
    cat(paste0("Validation eval_metrics for hyperparameters chosen were: "))
    cat(paste0(names(validation_eval_metrics_hyper_choice_current_date),":",
               round(validation_eval_metrics_hyper_choice_current_date,4), sep=" "))
    cat("\n")
  }

  return(list(
    chosen_eval_metric_validation_current_date = chosen_eval_metric_validation_current_date,
    optimal_hyper = optimal_hyper,
    validation_eval_metrics_hyper_choice_current_date = validation_eval_metrics_hyper_choice_current_date
  ))

}
