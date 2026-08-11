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

      #Separate searched hyperparameters from those held constant
      ###Only genuine ranges may reach bayesOpt. A constant expressed as a
      ###zero-width bound is not equivalent: ParBayesianOptimization scales
      ###candidates by (upper - lower), so such a bound becomes an all-NaN
      ###column silently, and gsPoints is pmax(100, length(bounds)^3), so the
      ###pinned hyperparameter would still cost a full dimension of search.
      hyper_grid_partition <- partition_hyper_grid_domain_list(hyper_grid_domain_list)
      searched_hyper_grid_domain_list <- hyper_grid_partition$searched
      fixed_hyperparameters <- hyper_grid_partition$fixed

      #Build the objective, re-inserting the fixed values on every call
      ###bayesOpt calls FUN with one argument per bound, so the fixed
      ###hyperparameters have to be spliced back in here; the learner is always
      ###invoked with the complete set it expects.
      base_tuning_objective <- eval_function(

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
      )

      tuning_objective <- if (length(fixed_hyperparameters) == 0) {
        ###No constants declared: the objective is exactly what it was before,
        ###so a domain of pure bounds takes an unchanged code path.
        base_tuning_objective
      } else {
        function(...) {
          do.call(base_tuning_objective, c(list(...), fixed_hyperparameters))
        }
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
            FUN = tuning_objective, #Objective, with any constants spliced back in
            bounds = searched_hyper_grid_domain_list, #Boundaries, searched hyperparameters only
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
            FUN = tuning_objective, #Objective, with any constants spliced back in
            bounds = searched_hyper_grid_domain_list, #Boundaries, searched hyperparameters only
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

      #Record hyperparameters held constant
      ###They never entered the score summary because they were not searched,
      ###but the tuning history is read by name downstream (the hyperparameter
      ###plots and summaries), so a pinned hyperparameter must still appear,
      ###at the value it was held at.
      if (length(fixed_hyperparameters) > 0) {
        for (fixed_name in names(fixed_hyperparameters)) {
          chosen_eval_metric_validation_current_date[[fixed_name]] <- fixed_hyperparameters[[fixed_name]]
        }
      }

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

      #Add hyperparameters held constant
      ###getBestPars() reports only the searched dimensions. Downstream,
      ###fit_sb_model() reads hyperparameters out of this vector by name, so a
      ###constant left out here would reach the learner as NA rather than as
      ###the value the user pinned.
      if (length(fixed_hyperparameters) > 0) {
        optimal_hyper <- c(optimal_hyper, unlist(fixed_hyperparameters))
      }

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


# partition_hyper_grid_domain_list ---------------------------------------

#' Split a Bayesian-Optimization Domain into Searched and Fixed Hyperparameters
#'
#' @description
#' Internal helper. Separates the entries of a `bayesian_opt` hyperparameter
#' domain into those the surrogate should search over and those the user pinned
#' to a constant.
#'
#' @details
#' A constant cannot be expressed to `ParBayesianOptimization` as a zero-width
#' bound. Candidates are rescaled by `(upper - lower)`, so `c(x, x)` produces an
#' all-`NaN` column with no error and no warning, and `gsPoints` defaults to
#' `pmax(100, length(bounds)^3)`, so the pinned hyperparameter still costs a
#' full dimension of search. Holding a hyperparameter constant therefore means
#' removing it from the surrogate's input space and re-inserting its value only
#' when the learner is called.
#'
#' Constants use the same shape `random_search` already accepts, a list with
#' `distribution_choice = "constant"` and a single numeric `value`, so one
#' configuration reads the same way under either tuning method.
#'
#' @param hyper_grid_domain_list A named list. Each entry is either a numeric
#'   vector of length 2 giving `c(lower, upper)` bounds, or a list with
#'   `distribution_choice = "constant"` and a single numeric `value`.
#'
#' @return A list with two elements:
#'   \item{searched}{Named list of length-2 numeric bounds, to be passed as
#'     `bounds`. Retains the order in which the hyperparameters were declared.}
#'   \item{fixed}{Named list of single numeric values, to be spliced into the
#'     learner call and appended to the optimal hyperparameters.}
#'
#' @keywords internal
partition_hyper_grid_domain_list <- function(hyper_grid_domain_list) {

  ##Validate the container itself
  if (!is.list(hyper_grid_domain_list) || length(hyper_grid_domain_list) == 0) {
    stop("hyper_grid_domain_list must be a non-empty list.")
  }
  if (is.null(names(hyper_grid_domain_list)) || any(names(hyper_grid_domain_list) == "")) {
    stop("Every entry of hyper_grid_domain_list must be named.")
  }

  ##Classify each entry
  is_fixed_entry <- vapply(
    hyper_grid_domain_list,
    function(entry) is.list(entry) && identical(entry$distribution_choice, "constant"),
    logical(1)
  )

  searched <- hyper_grid_domain_list[!is_fixed_entry]
  fixed_entries <- hyper_grid_domain_list[is_fixed_entry]

  ##Searched entries must be usable bounds
  for (hyperparameter_name in names(searched)) {
    bounds <- searched[[hyperparameter_name]]
    if (!is.numeric(bounds) || length(bounds) != 2 || any(!is.finite(bounds))) {
      stop("Hyperparameter '", hyperparameter_name,
           "' must be a finite numeric vector of length 2 giving c(lower, upper), ",
           "or a constant declared with distribution_choice = 'constant'.")
    }
    if (bounds[2] < bounds[1]) {
      stop("Hyperparameter '", hyperparameter_name,
           "' has upper bound below lower bound.")
    }
  }

  ##Fixed entries must carry a single usable value
  fixed <- lapply(names(fixed_entries), function(hyperparameter_name) {
    value <- fixed_entries[[hyperparameter_name]]$value
    if (!is.numeric(value) || length(value) != 1 || !is.finite(value)) {
      stop("Constant hyperparameter '", hyperparameter_name,
           "' must have a single finite numeric value.")
    }
    return(value)
  })
  names(fixed) <- names(fixed_entries)

  ##Flag zero-width ranges without changing what they do
  ###Collapsed bounds, c(x, x), were the only way to pin a hyperparameter before
  ###constants existed, and configurations still use them. They are pathological:
  ###candidates are rescaled by (upper - lower), so the column reaches the
  ###Gaussian process as all-NaN with no error of its own, while gsPoints still
  ###counts the dimension.
  ###
  ###They are deliberately NOT converted to constants here. Converting them
  ###would change the dimension the surrogate searches, hence the candidates it
  ###draws, hence the selected hyperparameters, for every existing configuration
  ###that uses the idiom. Behaviour is left exactly as it was and the user is
  ###told how to opt in, so results only change when they choose to migrate.
  is_collapsed_range <- vapply(searched, function(bounds) bounds[2] == bounds[1], logical(1))

  if (any(is_collapsed_range)) {
    collapsed_names <- names(searched)[is_collapsed_range]
    warning("Hyperparameter(s) ", paste(sprintf("'%s'", collapsed_names), collapse = ", "),
            " have a zero-width range and are still being passed to the Gaussian process, ",
            "which rescales candidates by (upper - lower) and therefore receives an ",
            "undefined (NaN) input for them, while they continue to count towards the ",
            "search dimension and gsPoints. To hold them genuinely constant, declare them ",
            "with distribution_choice = 'constant' and pars = <value>. Note that migrating ",
            "changes the search space and so will change tuning results.",
            call. = FALSE)
  }

  ##At least one hyperparameter must remain to search over
  if (length(searched) == 0) {
    stop("Every hyperparameter is held constant, leaving nothing for bayesian_opt to ",
         "search over. Give at least one hyperparameter a range, or use a tuning ",
         "method that does not optimize.")
  }

  return(list(searched = searched, fixed = fixed))
}
