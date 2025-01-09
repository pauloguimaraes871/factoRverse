#' Show Method for meta_dataframe Class
#'
#' This method displays a summary of the `meta_dataframe` object, including
#' sb_backtest_workflow information, number of signals, unique dates, unique tickers,
#' total observations, and the first few rows of the data.
#'
#' @param object An instance of the `meta_dataframe` class.
#'
#' @return The method returns the object invisibly.
#'
#' @export
setMethod("show", "meta_dataframe", function(object) {

  # Print a summary of the sb_backtest_workflow
  cat("Meta Dataframe Summary:\n")
  cat("=================================\n")
  cat("Meta Dataframe name: ", object@meta_dataframe_name, " \n\n")
  if(object@class == "target_m_df"){
  cat(" Targets:\n")
  } else {
  cat(" Signals:\n")
  }
  cat(paste(object@signals, collapse = ", "))
  if(object@class == "target_m_df"){
  cat("  \nNumber of targets:", ncol(object@data)-3, "\n")
  } else {
  cat("  \nNumber of signals:", ncol(object@data)-3, "\n")
  }
  cat(" \nDates:\n")
  print(unique(as.Date(object@data$dates)))
  cat("  Number of unique dates:", object@unique_dates, "\n")
  cat(" \nTickers:\n", unique(object@data$tickers), "\n")
  cat("  Number of unique tickers:", object@unique_tickers, "\n")
  cat("\nTotal Observations (n_obs):", object@n_obs, "\n")
  cat("  \nWorkflow:\n")
  if(length(object@workflow) == 0){
    cat("  No workflow set.\n")
  } else {
    print(object@workflow)
  }

  cat("=================================\n")

  # Print the first few rows of the data
  cat("\nFirst few rows of the data:\n")
  print(head(object@data))

  # Return the object invisibly
  invisible(object)
})


#' Show Method for meta_dataframe Class
#'
#' This method displays a summary of the `meta_dataframe` object, including
#' sb_backtest_workflow information, number of signals, unique dates, unique tickers,
#' total observations, and the first few rows of the data.
#'
#' @param object An instance of the `meta_dataframe` class.
#'
#' @return The method returns the object invisibly.
#'
#' @export
setMethod("show", "groups_m_df", function(object) {

  # Print a summary of the sb_backtest_workflow
  cat("Meta Dataframe Groups Summary:\n")
  cat("=================================\n")
  cat("Meta Dataframe name: ", object@meta_dataframe_name, " \n\n")
  cat(" Groups:\n")
  cat(paste(object@signals, collapse = ", "))
  cat("  \nNumber of groups:", ncol(object@data)-3, "\n")
  group_classifications <- object@data %>% dplyr::select(object@signals)
  for(i in 1:ncol(group_classifications)){
    cat(paste0(" \nGroup '", object@signals[i], "':\n"))
    print(unique(group_classifications[,i]))
  }
  cat(" \nDates:\n")
  print(unique(as.Date(object@data$dates)))
  cat("  Number of unique dates:", object@unique_dates, "\n")
  cat(" \nTickers:\n", unique(object@data$tickers), "\n")
  cat("  Number of unique tickers:", object@unique_tickers, "\n")
  cat("\nTotal Observations (n_obs):", object@n_obs, "\n")
  cat("  \nWorkflow:\n")
  if(length(object@workflow) == 0){
    cat("  No workflow set.\n")
  } else {
    print(object@workflow)
  }

  cat("=================================\n")

  # Print the first few rows of the data
  cat("\nFirst few rows of the data:\n")
  print(head(object@data))

  # Return the object invisibly
  invisible(object)
})

#' Show Method for signal_universe_m_df Class
#'
#' This method extends the parent \code{meta_dataframe} show method by displaying
#' additional elements from \code{ss_backtest_workflow} and \code{sb_backtest_workflow}.
#' It focuses on the key fields you specified:
#'   \itemize{
#'     \item \strong{ss_backtest_workflow}: active_returns, model_structure, market_factor_proxy,
#'           backtest_type, p_correction_method, theme_level_intercept (can be NULL),
#'           theme_level_slope (can be NULL), signals_object_name, signal_themes_object_name,
#'           priors_object_name, backtest_returns_object_name, rebalancing_months
#'     \item \strong{sb_backtest_workflow}: sb_algorithm, custom_objective, backtest_type,
#'           keras_architecture_parameters, tuning_method, chosen_eval_metric, huber_delta, quantile_tau
#'     \item Note: \code{sb_backtest_workflow} can be \code{NULL}.
#'   }
#'
#' @param object An instance of the \code{signal_universe_m_df} class.
#'
#' @return Returns the object invisibly.
#' @export
setMethod("show", "signal_universe_m_df", function(object) {
  # 1) Initial Info
  # Print a summary of the signal_universe_m_df
  cat("Signal Universe Summary:\n")
  cat("=================================\n")
  cat("Object name: ", object@meta_dataframe_name, " \n\n")
  cat(" Performance Metrics:\n")
  cat(paste(setdiff(object@signals, c("top_assets", "theme_ss_bench_weights", "theme_sb_bench_weights", "theme", "is_eligible")), collapse = ", "))
  cat("  \nNumber of performance metrics:", ncol(object@data)-3-5, "\n")
  cat(" \nDates:\n")
  print(unique(as.Date(object@data$dates)))
  cat("  Number of unique dates:", object@unique_dates, "\n")
  cat(" \nTickers (Signals):\n", unique(object@data$tickers), "\n")
  cat("  Number of unique tickers:", object@unique_tickers, "\n")
  cat("\nTotal Observations (n_obs):", object@n_obs, "\n")
  cat("  \nWorkflow:\n")
  if(length(object@workflow) == 0){
    cat("  No workflow set.\n")
  } else {
    print(object@workflow)
  }

  cat("=================================\n")


  # 2) Summarize ss_backtest_workflow
  ss_wf <- object@ss_backtest_workflow
  if (!is.null(ss_wf)) {
    cat("\n=========================\n")
    cat("Signal Selection Backtest Summary\n")
    cat("=========================\n")

    # Safely extract each element; if missing, use NA or "NULL"
    cat("\n Performance Metrics and CAPM Details:")
    cat("   \n Return Type:", if(ss_wf[["active_returns"]]) "Active" else "Raw")
    cat("   \n Model Structure:", ss_wf[["model_structure"]])
    cat("   \n Market Factor Proxy:", ss_wf[["market_factor_proxy"]] )
    cat("   \n P-Value Correction Method:", ss_wf[["p_correction_method"]])
    if(ss_wf[["enable_theme_representativeness"]]){
    cat("   \n Theme Representativeness Enable")
    }
    cat("   \n P-Value Correction Method:", ss_wf[["p_correction_method"]])

    if(!is.null(ss_wf[["theme_level_intercept"]])){
    cat("   \n Theme Level Intercept:", ss_wf[["theme_level_intercept"]])
    cat("   \n Theme Level Slope:", ss_wf[["theme_level_slope"]])
    }
    cat("\n\n------------------------\n")

    cat("\n Object Names:")
    cat("   \n signals_object_name:", ss_wf[["signals_object_name"]] %||% "NULL")
    cat("   \n signal_themes_object_name:", ss_wf[["signal_themes_object_name"]] %||% "NULL")
    cat("   \n priors_object_name:", ss_wf[["priors_object_name"]] %||% "NULL")
    cat("   \n backtest_returns_object_name:", ss_wf[["backtest_returns_object_name"]] %||% "NULL")

    cat("\n\n------------------------\n")

    cat("\n Training Information :")
    cat("   \n Rebalancing Months:", paste(ss_wf[["rebalancing_months"]] %||% "NULL", collapse = ", "))
    cat("   \n Initial Sample Size:", paste(ss_wf[["initial_sample_size"]] %||% "NULL", collapse = ", "))
    cat("   \n Data Availability Cutoff:", paste(ss_wf[["data_availability_cutoff"]] %||% "NULL", collapse = ", "))
    cat("\n")
  }

  # Print the first few rows of the data
  cat("\nFirst few rows of the data:\n")
  print(head(object@data))

  # Return invisibly
  invisible(object)
})

# A small helper for safely extracting list elements or returning a default if not found.
`%||%` <- function(x, default) {
  if (!is.null(x)) x else default
}


#' Print method for hyper_grid_domain
#'
#' This method prints the contents of a `hyper_grid_domain` object in a user-friendly format.
#'
#' @param object A `hyper_grid_domain` object to be printed.
#'
#' @export
#' Print Method for hyper_grid_domain
#'
#' This method provides a well-structured output for hyper_grid_domain objects.
#'
#' @param object An object of class hyper_grid_domain.
#'
#' @export
setMethod("show", "hyper_grid_domain", function(object) {

  cat("Hyperparameters Grid Domain:\n")
  cat("=================================\n")

  cat("Hyperparameters:\n")

  if (length(object@hyperparameter_list) == 0) {
    cat("  No hyperparameters set.\n")
  } else {
    for (name in names(object@hyperparameter_list)) {
      cat("  ", name, ":\n")
      hyperparam <- object@hyperparameter_list[[name]]
      if (is.list(hyperparam)) {
        if ("distribution_choice" %in% names(hyperparam)) {
          cat("    Distribution Choice:", hyperparam$distribution_choice, "\n")
          if (hyperparam$distribution_choice == "constant") {
            cat("    Value:", paste(hyperparam$value, collapse = ", "), "\n")
          } else {
            cat("    Parameters:", paste(names(hyperparam$pars), hyperparam$pars, sep = "=", collapse = ", "), "\n")
          }
        }
      } else {
        print(object@hyperparameter_list[[name]])
      }
    }
  }

  cat("=================================\n")
})


#' @title Show Method for `tuning_strategy`
#' @description Custom show method for displaying the general information of objects that extend `tuning_strategy`.
#' This method prints the tuning method, machine learning algorithm, validation sample size, split method, evaluation metric,
#' early stopping criteria, and the hyperparameter grid domain.
#' @param object An object of class `tuning_strategy` or its subclasses (`grid_search_strategy`, `random_search_strategy`, or `bayesian_opt_strategy`).
#' @return Printed information about the base properties of the object.
#' @examples
#' # Create a base tuning_strategy object
#' base_obj <- create_tuning_strategy(
#'   tuning_method = "grid_search",
#'   sb_algorithm = "rf",
#'   validation_sample_size = 1000,
#'   split_method = "expanding"
#' )
#' show(base_obj)
#' @export
setMethod("show", "tuning_strategy", function(object) {

  cat("------------------------------\n")
  cat("Tuning Method: ", object@tuning_method, "\n")
  cat("Validation Sample Size: ", object@validation_sample_size, "\n")
  cat("Evaluation Metric: ", object@chosen_eval_metric, "\n")

  if (!is.null(object@early_stop)) {
    cat("Early Stop Criteria: ", object@early_stop, "\n")
  } else {
    cat("Early Stop Criteria: Not provided\n")
  }



})


#' @title Show Method for `grid_search_strategy`
#' @description Custom show method for displaying information about objects of class `grid_search_strategy`.
#' This method will display the tuning method, machine learning algorithm, validation sample size,
#' and details about the hyperparameter grid.
#' @param object An object of class `grid_search_strategy`.
#' @return Printed information about the object.
#' @examples
#' # Create a grid_search_strategy object
#' grid_search_obj <- create_tuning_strategy(
#'   tuning_method = "grid_search",
#'   validation_sample_size = 1000,
#'   split_method = "expanding",
#'   chosen_eval_metric = "rmse"
#' )
#' show(grid_search_obj)
#' @export
setMethod("show", "grid_search_strategy", function(object) {
  cat("Grid Search Tuning Strategy\n")
  callNextMethod()  # Calls the base show method for common slots
  cat("Grid Search Specific Information:\n")
  cat("- Hyperparameter Grid:\n")
  if (length(object@hyper_grid_domain@hyperparameter_list) == 0) {
    cat("  No hyperparameters set.\n")
  } else {
    for (name in names(object@hyper_grid_domain@hyperparameter_list)) {
      cat("  ", name, ":\n")
      hyperparam <- object@hyper_grid_domain@hyperparameter_list[[name]]
      cat("    Values:", paste(hyperparam, collapse = ", "), "\n")
    }
  }
})


#' @title Show Method for `random_search_strategy`
#' @description Custom show method for displaying information about objects of class `random_search_strategy`.
#' This method will display the tuning method, machine learning algorithm, validation sample size,
#' and the number of iterations (`n_iter`) along with hyperparameter distributions.
#' @param object An object of class `random_search_strategy`.
#' @return Printed information about the object.
#' @examples
#' # Create a random_search_strategy object
#' random_search_obj <- create_tuning_strategy(
#'   tuning_method = "random_search",
#'   sb_algorithm = "rf",
#'   validation_sample_size = 1000,
#'   n_iter = 20
#' )
#' show(random_search_obj)
#' @export
setMethod("show", "random_search_strategy", function(object) {

  cat("Random Search Tuning Strategy\n")
  callNextMethod()  # Calls the base show method for common slots
  cat("Random Search Specific Information:\n")
  cat("- Number of Iterations (n_iter): ", object@n_iter, "\n")
  cat("- Hyperparameter Distribution:\n")
  if (length(object@hyper_grid_domain@hyperparameter_list) == 0) {
    cat("  No hyperparameters set.\n")
  } else {
    for (name in names(object@hyper_grid_domain@hyperparameter_list)) {
      cat("  ", name, ":\n")
      hyperparam <- object@hyper_grid_domain@hyperparameter_list[[name]]
      cat("    Distribution Choice:", hyperparam$distribution_choice, "\n")
      if (hyperparam$distribution_choice == "constant") {
        cat("    Value:", paste(hyperparam$value, collapse = ", "), "\n")
      } else {
        cat("    Parameters:", paste(names(hyperparam$pars), hyperparam$pars, sep = "=", collapse = ", "), "\n")
      }
    }
  }


})


#' @title Show Method for `bayesian_opt_strategy`
#' @description Custom show method for displaying information about objects of class `bayesian_opt_strategy`.
#' This method will display the tuning method, machine learning algorithm, validation sample size,
#' and details specific to Bayesian optimization such as `n_iter`, acquisition function (`acq`),
#' initial points, and hyperparameter bounds.
#' @param object An object of class `bayesian_opt_strategy`.
#' @return Printed information about the object.
#' @examples
#' # Create a bayesian_opt_strategy object
#' bayesian_opt_obj <- create_tuning_strategy(
#'   tuning_method = "bayesian_opt",
#'   sb_algorithm = "xgb",
#'   validation_sample_size = 1000,
#'   n_iter = 50,
#'   acq = "ei",
#'   init_points = 5,
#'   k_iter = 3
#' )
#' show(bayesian_opt_obj)
#' @export
setMethod("show", "bayesian_opt_strategy", function(object) {
  cat("Bayesian Optimization Tuning Strategy\n")
  callNextMethod()  # Calls the base show method for common slots
  cat("Bayesian Optimization Specific Information:\n")
  cat("- Number of Iterations (n_iter): ", object@n_iter, "\n")
  cat("- Acquisition Function (acq): ", object@acq, "\n")
  cat("- Initial Points: ", object@init_points, "\n")
  cat("- k_iter: ", object@k_iter, "\n")
  cat("- Hyperparameter Bounds:\n")
  if (length(object@hyper_grid_domain@hyperparameter_list) == 0) {
    cat("  No hyperparameters set.\n")
  } else {
    for (name in names(object@hyper_grid_domain@hyperparameter_list)) {
      cat("  ", name, ":\n")
      hyperparam <- object@hyper_grid_domain@hyperparameter_list[[name]]
      cat("    Bounds:", paste(hyperparam, collapse = ", "), "\n")
    }
  }


})



#' @title Print keras_architecture_parameters
#' @description Method to print an object of class `keras_architecture_parameters`.
#'
#' @param object An object of class `keras_architecture_parameters`.
#'
#' @export
setMethod("show", "keras_architecture_parameters", function(object) {
  cat("------------------------------\n")
  cat("Keras Architecture Parameters:\n")
  cat("------------------------------\n")
  cat("Number of Layers:", object@n_layers, "\n")
  cat("Units per Layer:", paste(object@units, collapse = ", "), "\n")
  cat("Activation Functions:", paste(object@activation, collapse = ", "), "\n")
  cat("Optimizer:", object@nn_optimizer, "\n")
  cat("Batch Normalization Options:", paste(object@batch_norm_option, collapse = ", "), "\n")
  cat("------------------------------\n")
})



#' @title Show SB Backtest Config
#' @description Prints the contents of an `sb_backtest_config` object, detailing the various parameters and their configurations.
#'
#' @param object An `sb_backtest_config` object to be displayed.
#'
#' @method show sb_backtest_config
#' @export
setMethod("show", "sb_backtest_config", function(object) {
  cat("==============================\n")
  cat("SB Backtest Configuration\n\n")

  # Display Main Information
  cat("------------------------------\n")
  cat("Main Information:\n")
  cat("------------------------------\n")
  cat("SB Algorithm:", object@sb_algorithm, "\n")
  cat("Config Name:", object@config_name, "\n")
  cat("Target Fwd Name:", object@target_fwd_name, "\n")

  cat("Training Scheme:\n")
  cat("  Training Sample Size:", object@training_sample_size, "\n")
  cat("  Rebalancing Months:", paste(object@rebalancing_months, collapse = " "), "\n")
  cat("  Split Method:", object@split_method, "\n")

  # Display Custom Objective Information
  cat("Objective Function:\n")

  if(!object@sb_algorithm %in% c("ew","rp"))  cat("  Custom Objective:", object@custom_objective, "\n")

  # Display Miscellaneous Parameters
  cat("  Function Parameters:")
  cat("  Huber Delta:", object@huber_delta)
  cat("  Quantile Tau:", object@quantile_tau, "\n")

  #Display ML stuff
  if(!object@sb_algorithm %in% c("ols", "ew", "sw", "rp", "mvo")){
    cat("------------------------------\n")

    # Display Keras Architecture Parameters Information
    if (object@sb_algorithm == "nn"){
      if (is.null(object@keras_architecture_parameters)) {
        cat("  No Keras architecture parameters set.\n\n")
      } else {
        cat("\n")
        show(object@keras_architecture_parameters)
      }
    }



    # Display Hyperparameter Tuning Information
    if(is.null(object@tuning_strategy)){
      cat("  No tuning strategy set.\n")
    } else {
      show(object@tuning_strategy)

      ## Check hyperparameters validity based on sb_algorithm
      hyperparameters_names <- names(object@tuning_strategy@hyper_grid_domain@hyperparameter_list)

      ### GLMNET
      expected_hyperparameters_glmnet <- c("alpha", "lambda.min.ratio")
      hyperparameters_missing <- expected_hyperparameters_glmnet[which(!expected_hyperparameters_glmnet %in% hyperparameters_names)]
      if(length(hyperparameters_missing) != 0 && object@sb_algorithm == "glmnet"){
        cat("\n")
        cat(paste("Hyperparameter(s) still not configured:\n"))
        cat(paste(hyperparameters_missing, collapse = ", "))
        cat("\n")
      }

      ### RF
      expected_hyperparameters_rf <- c("mtry", "num.trees", "max.depth", "min.bucket")
      hyperparameters_missing <- expected_hyperparameters_rf[which(!expected_hyperparameters_rf %in% hyperparameters_names)]
      if(length(hyperparameters_missing) != 0 && object@sb_algorithm == "rf"){
        cat("\n")
        cat(paste("Hyperparameter(s) still not configured:\n"))
        cat(paste(hyperparameters_missing, collapse = ", "))
        cat("\n")
      }

      ### XGB
      expected_hyperparameters_xgb <- c("min_child_weight", "max_depth", "subsample", "colsample_bytree", "eta", "alpha", "gamma", "nrounds")
      hyperparameters_missing <- expected_hyperparameters_xgb[which(!expected_hyperparameters_xgb %in% hyperparameters_names)]
      if(length(hyperparameters_missing) != 0 && object@sb_algorithm == "xgb"){
        cat("\n")
        cat(paste("Hyperparameter(s) still not configured:\n"))
        cat(paste(hyperparameters_missing, collapse = ", "))
        cat("\n")
      }

      ### NN
      expected_hyperparameters_nn <- c("regularizer_l1", "regularizer_l2", "droprate", "lr", "size_of_batch", "number_of_epochs")
      hyperparameters_missing <- expected_hyperparameters_nn[which(!expected_hyperparameters_nn %in% hyperparameters_names)]
      if(length(hyperparameters_missing) != 0 && object@sb_algorithm == "nn"){
        cat("\n")
        cat(paste("Hyperparameter(s) still not configured:\n"))
        cat(paste(hyperparameters_missing, collapse = ", "))
        cat("\n")
      }

    }

  }

  #Display signal port
  if(object@sb_algorithm %in% c("rp", "mvo")){
    if (!is.null(object@signal_port_parameters)) {
      cat("\n------------------------------\n")
      cat("Signal Portfolio Parameters:\n")
      cat("------------------------------\n")
      signal_port_parameters <- object@signal_port_parameters

      # Cov Est Method
        cat("Covariance Estimation Method:\n")
        cat("  Method: ")
        cat(signal_port_parameters@cov_est_method@cov_estimation_method, "\n")
        cat("  Sample Size:", signal_port_parameters@cov_est_method@cov_matrix_sample_size, "\n")
        cat("  Active Returns:", signal_port_parameters@cov_est_method@active_returns, "\n")
        if(signal_port_parameters@cov_est_method@active_returns){
          cat("  Cov Matrix Benchmark:", signal_port_parameters@cov_est_method@cov_matrix_benchmark, "\n")
        }

      cat("\n------------------------------\n")

      # RP parameters
      if (object@sb_algorithm == "rp"){
        cat("RP Parameters:\n")
        cat("  RP Method:", signal_port_parameters@rp_parameters@rp_method, "\n")

        cat("\n------------------------------\n")
      }

      # MVO parameters
      if (object@sb_algorithm == "mvo"){
        cat("MVO Parameters:\n")
        cat("  Optimization Method:", signal_port_parameters@mvo_parameters@opt_method, "\n")
        cat("  Random Ports Method:", signal_port_parameters@mvo_parameters@random_ports_method, "\n")
        cat("  n_random_ports:", signal_port_parameters@mvo_parameters@n_random_ports, "\n")
        cat("  Objective:", signal_port_parameters@mvo_parameters@opt_objective, "\n")

        cat("\n------------------------------\n")
      }

      # Concentration Constraint Policy
      if (!is.null(signal_port_parameters@concentration_constraint_policy) &&
          methods::is(signal_port_parameters@concentration_constraint_policy, "concentration_constraint_policy")) {
        cat("Concentration Constraint Policy:\n")
        cat("  Benchmark:", signal_port_parameters@concentration_constraint_policy@benchmark, "\n")
        if (!is.null(signal_port_parameters@concentration_constraint_policy@max_abs_active_individual_weight)){
          cat("  Max Abs Active Individual Weight:", signal_port_parameters@concentration_constraint_policy@max_abs_active_individual_weight, "\n")
        }
        if (!is.null(signal_port_parameters@concentration_constraint_policy@max_abs_active_group_weight)){
        cat("  Max Abs Active Group Weight:", signal_port_parameters@concentration_constraint_policy@max_abs_active_group_weight, "\n")
        }
      } else {
        cat("  (No concentration constraint policy set)\n")
      }
      cat("\n------------------------------\n")

    }
  }

  if(!is.null(object@ss_backtest_results)){
    cat("\n")
    cat("Printing Signal Selection Backtest:\n")
    cat("==============================\n")
    print(object@ss_backtest_results)
  }


  if(!is.null(object@ss_backtest_config)){
    cat("\n")
    cat("Printing Signal Selection Configuration:\n")
    cat("==============================\n")
    print(object@ss_backtest_config)
  }


  cat("\n=================================\n")
})



#' Show Method for sb_metabacktest_config Class
#'
#' Displays detailed information about each configuration in the `sb_metabacktest_config` object.
#'
#' @param object An `sb_metabacktest_config` object.
#' @return Invisibly returns `NULL`. This function is called for its side effect of displaying information.
#' @examples
#' # Assuming you have an sb_metabacktest_config object named meta_config
#' show(meta_config)
#'
#' @export
setMethod("show", "sb_metabacktest_config",
          function(object) {

            cat(crayon::yellow("SB Metabacktest Configuration\n"))
            cat("Config Name: ", object@config_name, "\n")

            cat("------------------------------\n")
            cat(crayon::cyan("Meta Learner Backtest Configuration:\n"))
            config <- object@meta_sb_backtest_config
            cat(sprintf("  sb_algorithm: %s\n", config@sb_algorithm))
            cat(sprintf("  config_name: %s\n", config@config_name))


            # For neural networks, display number of layers
            if (config@sb_algorithm == "nn" && !is.null(config@keras_architecture_parameters)) {
              n_layers <- length(config@keras_architecture_parameters@units)
              cat(sprintf("  n_layers: %s\n", n_layers))
            }

            cat(sprintf("  training_sample_size: %s\n", config@training_sample_size))
            cat(sprintf("  rebalancing_months: %s\n", paste(config@rebalancing_months, collapse = " ")))
            cat(sprintf("  custom_objective: %s\n", config@custom_objective))
            cat(sprintf("  huber_delta: %s\n", config@huber_delta))
            cat(sprintf("  quantile_tau: %s\n", config@quantile_tau))

            if (!is.null(config@tuning_strategy)) {
              cat("  Tuning_strategy:\n")
              cat(sprintf("    tuning_method: %s\n", config@tuning_strategy@tuning_method))
              cat(sprintf("    validation_sample_size: %s\n", config@tuning_strategy@validation_sample_size))
              cat(sprintf("    chosen_eval_metric: %s\n", config@tuning_strategy@chosen_eval_metric))
            } else {
              cat("  No tuning_strategy available\n")
            }
            cat("\n")


            cat("------------------------------\n")
            n_configs <- length(object@base_sb_backtest_configs)
            if (n_configs > 0) {
              cat(crayon::yellow("\nBase Backtest Configuration details:\n\n"))
              cat(sprintf("Number of base SB backtest configurations: %d\n", n_configs))

              # Define a color palette using crayon
              colors <- list(
                neon_cyan = crayon::cyan,
                neon_pink = crayon::magenta,
                neon_blue = crayon::blue,
                neon_purple = crayon::make_style("#8A2BE2"),
                neon_orange = crayon::red,
                neon_green = crayon::green
              )

              # Loop through configurations
              for (i in seq_along(object@base_sb_backtest_configs)) {
                config <- object@base_sb_backtest_configs[[i]]

                # Use a color from the palette
                color_func <- colors[[ (i - 1) %% length(colors) + 1 ]]

                # Color the backtest configuration header
                cat(color_func(sprintf("Base SB Backtest Configuration %d:\n", i)))
                cat(paste("Config name:", config@config_name), "\n")
                cat(sprintf("  sb_algorithm: %s\n", config@sb_algorithm))


                # For neural networks, display number of layers
                if (config@sb_algorithm == "nn" && !is.null(config@keras_architecture_parameters)) {
                  n_layers <- length(config@keras_architecture_parameters@units)
                  cat(sprintf("  n_layers: %s\n", n_layers))
                }

                cat(sprintf("  training_sample_size: %s\n", config@training_sample_size))
                cat(sprintf("  rebalancing_months: %s\n", paste(config@rebalancing_months, collapse = " ")))
                cat(sprintf("  custom_objective: %s\n", config@custom_objective))
                cat(sprintf("  huber_delta: %s\n", config@huber_delta))
                cat(sprintf("  quantile_tau: %s\n", config@quantile_tau))

                if (!is.null(config@tuning_strategy)) {
                  cat("  Tuning_strategy:\n")
                  cat(sprintf("    tuning_method: %s\n", config@tuning_strategy@tuning_method))
                  cat(sprintf("    validation_sample_size: %s\n", config@tuning_strategy@validation_sample_size))
                  cat(sprintf("    chosen_eval_metric: %s\n", config@tuning_strategy@chosen_eval_metric))
                } else {
                  cat("  No tuning_strategy available\n")
                }
                cat("\n")
              }
            }

            n_results <- length(object@base_sb_backtest_results)
            if (n_results > 0) {
              cat(crayon::yellow("\nBase Backtest Results details:\n\n"))
              cat(sprintf("Number of base SB backtest results: %d\n", n_results))

              # Define a color palette using crayon
              colors <- list(
                neon_cyan = crayon::cyan,
                neon_pink = crayon::magenta,
                neon_blue = crayon::blue,
                neon_purple = crayon::make_style("#8A2BE2"),
                neon_orange = crayon::red,
                neon_green = crayon::green
              )

              # Loop through results
              for (i in seq_along(object@base_sb_backtest_results)) {
                result <- object@base_sb_backtest_results[[i]]@sb_backtest_workflow

                # Use a color from the palette
                color_func <- colors[[ (i - 1) %% length(colors) + 1 ]]

                # Color the backtest configuration header
                cat(color_func(sprintf("Base SB Backtest Results %d:\n", i)))
                cat(paste("Config name:", result$config_name), "\n")
                cat(paste("Backtest identifier:", result$backtest_identifier), "\n")
                cat(sprintf("  sb_algorithm: %s\n", result$sb_algorithm))


                # For neural networks, display number of layers
                if (result$sb_algorithm == "nn" && !is.null(result$keras_architecture_parameters)) {
                  n_layers <- length(result$keras_architecture_parameters$units)
                  cat(sprintf("  n_layers: %s\n", n_layers))
                }

                cat(sprintf("  training_sample_size: %s\n", result$training_sample_size))
                cat(sprintf("  rebalancing_months: %s\n", paste(result$rebalancing_months, collapse = " ")))
                cat(sprintf("  custom_objective: %s\n", result$custom_objective))
                cat(sprintf("  huber_delta: %s\n", result$huber_delta))
                cat(sprintf("  quantile_tau: %s\n", result$quantile_tau))

                if (!is.null(result$tuning_method)) {
                  cat("  Tuning_strategy:\n")
                  cat(sprintf("    tuning_method: %s\n", result$tuning_method))
                  cat(sprintf("    validation_sample_size: %s\n", result$validation_sample_size))
                  cat(sprintf("    chosen_eval_metric: %s\n", result$chosen_eval_metric))
                } else {
                  cat("  No tuning_strategy available\n")
                }
                cat("\n")
              }

            }


            invisible(NULL)
          })

#' Show Method for sb_model Class
#'
#' This method provides a summary of the `sb_model` object, including
#' the machine learning algorithm used, best hyperparameters, custom objective,
#' Huber delta, Keras architecture parameters, and the model structure.
#'
#' @param object An instance of the `sb_model` class.
#'
#' @return The method returns the object invisibly.
#'
#' @export
setMethod("show", "sb_model", function(object) {
  cat("SB Model Summary:\n")

  cat("=================================\n")

  # Display the algorithm used
  cat("SB Algorithm: ", object@sb_algorithm, "\n")

  # Display the best hyperparameters if they exist
  cat("Best Hyperparameters: ")
  if (length(object@best_hyperparameters) > 0) {
    cat(object@best_hyperparameters)
  } else {
    cat("No hyperparameters available.\n")
  }

  # Display the custom objective if it exists
  if (!is.null(object@custom_objective)) {
    cat("Custom Objective: ")
    cat(object@custom_objective)
  } else {
    cat("No custom objective specified.\n")
  }

  # Display the Huber delta if it is set
  cat("\nHuber Delta: ", object@huber_delta, "\n")

  # Display Keras architecture parameters if they exist
  if (!is.null(object@keras_architecture_parameters)) {
    cat("Keras Architecture Parameters:\n")
    print(object@keras_architecture_parameters)
  } else {
    cat("No Keras architecture parameters specified.\n")
  }

  cat("=================================\n")

  # Display model structure or summary if available
  cat("Model Structure:\n\n")
  if (!is.null(object@model)) {
    print(object@model)
  } else {
    cat("No model object available.\n")
  }

  # Indicate that the object is displayed
  invisible(object)
})


#' Show Method for sb_backtest_results Class
#'
#' This method displays a detailed summary of the `sb_backtest_results` object,
#' including metadata on the machine learning workflow validation results,
#' algorithm details, sample sizes, stock information, features, tuning,
#' Keras architecture parameters, performance, and the original call.
#'
#' @param object An instance of the `sb_backtest_results` class.
#'
#' @return The method returns the object invisibly.
#'
#' @export
setMethod("show", "sb_backtest_results", function(object) {

  # Extract the sb_backtest_workflow
  sb_backtest_workflow <- object@sb_backtest_workflow

  # Create a neat display of the sb_backtest_workflow
  cat("SB Backtest Workflow Metadata\n")
  cat("Backtest Identifier: ", object@backtest_identifier, "\n")
  cat("=================================\n")

  # Display Algorithm Information
  cat("Algorithm Information:\n")
  cat(" Config Name:", sb_backtest_workflow$config_name, "\n")
  cat("  SB Algorithm:", sb_backtest_workflow$sb_algorithm, "\n")
  if(!sb_backtest_workflow$sb_algorithm %in% c("ew_ensemble", "optimal_ensemble")){
    cat("  Custom Objective:", sb_backtest_workflow$custom_objective, "\n")
    if(sb_backtest_workflow$custom_objective == "pseudo_huber_error") cat("  Custom Huber Delta:", sb_backtest_workflow$huber_delta, "\n")
  }
  if(sb_backtest_workflow$sb_algorithm == "opt_ensemble"){
    cat("  Ensemble Eval Metric:", sb_backtest_workflow$chosen_eval_metric, "\n")
    cat("  Ensemble Huber Delta:", sb_backtest_workflow$huber_delta, "\n")
    cat("  Ensemble Quantile Tau:", sb_backtest_workflow$quantile_tau, "\n")
  }

  cat("  Backtest Type:", sb_backtest_workflow$backtest_type, "\n")
  if(sb_backtest_workflow$backtest_type == "meta_learner"){
    cat("    Base-Learner Config Names:", sb_backtest_workflow$config_name_bl, "\n")
    cat("    Base-Learner Algorithms:", sb_backtest_workflow$sb_algorithm_bl, "\n")
  }
  if(sb_backtest_workflow$sb_algorithm == "nn"){
    # Display Keras Information
    show(create_keras_architecture(
      nn_optimizer = sb_backtest_workflow$keras_architecture_parameters$nn_optimizer,
      units = sb_backtest_workflow$keras_architecture_parameters$units,
      activation = sb_backtest_workflow$keras_architecture_parameters$activation,
      batch_norm_option = sb_backtest_workflow$keras_architecture_parameters$batch_norm_option)
    )
  }


  cat("=================================\n")

  # Display Date Information
  cat("Date Information:\n")
  cat("  Range of Dates Covered:", paste(c(min(sb_backtest_workflow$dates_covered),max(sb_backtest_workflow$dates_covered)), sep = "-"), "\n")
  cat("  Number of Dates:", sb_backtest_workflow$n_dates, "\n")
  cat("  First Rebalance Date:", paste(sb_backtest_workflow$first_rebalance_date), "\n")
  cat("  Rebalance Dates:", paste(sb_backtest_workflow$rebalance_dates, collapse = ", "), "\n")
  if(!sb_backtest_workflow$sb_algorithm %in% c("ew_ensemble")) cat("  Split Method:", sb_backtest_workflow$split_method, "\n")

  if(sb_backtest_workflow$backtest_type == "meta_learner"){
    cat("-------------------------------\n")
    cat("  Base Learners Date Information:\n")
    cat("    Range of Dates Covered:", paste(c(as.Date(min(sb_backtest_workflow$dates_covered_bl)),
                                             as.Date(max(sb_backtest_workflow$dates_covered_bl))), sep ="-"), "\n")
    cat("    Number of Dates:", sb_backtest_workflow$n_dates_bl, "\n")
    cat("-------------------------------\n")
  }

  cat("=================================\n")

  # Display Sample Sizes
  cat("Sample Sizes:\n")
  if(!sb_backtest_workflow$sb_algorithm %in% c("ew_ensemble")) cat("  Training Sample Size:", sb_backtest_workflow$training_sample_size, "\n")
  if(!sb_backtest_workflow$sb_algorithm %in% c("ew_ensemble", "optimal_ensemble")) cat("  Validation Sample Size:", sb_backtest_workflow$validation_sample_size, "\n")
  cat("  Testing Sample Size:", sb_backtest_workflow$testing_sample_size, "\n")
  cat("  Range of Dates in Testing Sample:", paste(c(min(sb_backtest_workflow$dates_testing_sample), max(sb_backtest_workflow$dates_testing_sample)), sep ="-"), "\n")

  if(sb_backtest_workflow$backtest_type == "meta_learner"){
    cat("-------------------------------\n")
    cat("  Base Learners Sample Sizes:\n")
    cat("    Training Sample Size:", sb_backtest_workflow$training_sample_size_bl, "\n")
    cat("    Validation Sample Size:", sb_backtest_workflow$validation_sample_size_bl, "\n")
    cat("    Testing Sample Size:", sb_backtest_workflow$testing_sample_size_bl, "\n")
    cat("    Range of Dates in Testing Sample:", paste(c(as.Date(min(sb_backtest_workflow$dates_testing_sample_bl)),
                                                        as.Date(max(sb_backtest_workflow$dates_testing_sample_bl))), sep="-"), "\n")
    cat("    Rebalance Dates:", paste(as.Date(sb_backtest_workflow$rebalance_dates_bl), collapse = ", "), "\n")

    cat("-------------------------------\n")
  }

  cat("=================================\n")

  # Display Stocks Information
  cat("Stocks Information:\n")
  cat("Number of Stocks:", sb_backtest_workflow$n_stocks, "\n")
  cat("Number of Observations:", sb_backtest_workflow$nobs, "\n")
  #cat("Tickers:", paste(sb_backtest_workflow$tickers, collapse = ", "), "\n")

  cat("=================================\n")

  # Display Target Information
  cat("Target Information:\n")
  cat("  Forward Target Name:", sb_backtest_workflow$target_fwd_name, "\n")
  cat("  Target Forward:", sb_backtest_workflow$target_fwd, "\n")
  cat("  Target Object:", sb_backtest_workflow$target_object, "\n")
  cat("  Target Workflow:\n")
  if(is.null(sb_backtest_workflow$target_workflow)){
    cat("    No Target Workflow\n")
  } else {
    print(sb_backtest_workflow$target_workflow)
    cat("\n")
  }
  cat("\n")


  cat("=================================\n")

  # Display Features Information
  cat("Features Information:\n")
  cat("Features:", paste(sb_backtest_workflow$features, collapse = ", "), "\n")
  if(!sb_backtest_workflow$sb_algorithm %in% c("ew_ensemble", "optimal_ensemble")){
    cat("  Features Workflow:\n")
    if(is.null(sb_backtest_workflow$features_workflow)){
      cat("    No Features Workflow\n")
    } else {
      print(sb_backtest_workflow$features_workflow)
      cat("\n")
    }
  }
  cat("\n")

  cat("Top 5 most important features at final rebalancing:", paste(object@final_feature_importance_m_d_ref@data %>%
                                                             dplyr::slice_max(order_by = normalized_importance, n = 5, with_ties = FALSE) %>% dplyr::pull(tickers),
                                                             collapse = ", "), "\n")

  cat("Bottom 5 least important features at final rebalancing:", paste(object@final_feature_importance_m_d_ref@data %>%
                                                                   dplyr::slice_min(order_by = normalized_importance, n = 5, with_ties = FALSE) %>% dplyr::pull(tickers),
                                                                   collapse = ", "), "\n")

  if(!sb_backtest_workflow$sb_algorithm %in% c("ew_ensemble", "optimal_ensemble")) cat("Features Object:", sb_backtest_workflow$features_object, "\n")

  cat("Feature Selection Backtest Information:\n")
  if(!is.null(object@ss_backtest_results)){
    cat("\n")
    cat("=================================\n")
    print(object@ss_backtest_results)
  } else {
    cat("  No feature selection backtest results available.\n")
  }


  cat("=================================\n")

  # Display Tuning Information
  if(!sb_backtest_workflow$sb_algorithm %in% c("ols", "ew_ensemble", "optimal_ensemble", "ew", "sw", "rp", "mvo")){
    cat("Tuning Information:\n")
    cat("  Tuning Method:", sb_backtest_workflow$tuning_method, "\n")
    if(sb_backtest_workflow$tuning_method == "random_search" || sb_backtest_workflow$tuning_method == "bayesian_opt"){
      cat("  Number of Iterations:", sb_backtest_workflow$n_iter, "\n")
      if(sb_backtest_workflow$tuning_method == "bayesian_opt"){
        cat("  Number of Samples of Eval Function:", sb_backtest_workflow$k_iter, "\n")
        cat("  Acquisition Function:", sb_backtest_workflow$acq, "\n")
        cat("  Number of Points to Init Process:", sb_backtest_workflow$init_points, "\n")
      }
    }
    cat("  Hyperparameter Grid Domain List:", paste(names(sb_backtest_workflow$hyper_grid_domain_list), collapse = ", "), "\n")
    cat("  Chosen Evaluation Metric:", sb_backtest_workflow$chosen_eval_metric, "\n")
    cat("  Huber Delta:", sb_backtest_workflow$huber_delta, "\n")
    cat("  Quantile Tau:", sb_backtest_workflow$quantile_tau, "\n")
    cat("  Early Stop:", sb_backtest_workflow$early_stop, "\n")

    cat("=================================\n")
  }


  # Display Performance Information
  if(!sb_backtest_workflow$sb_algorithm %in% c("ew_ensemble", "optimal_ensemble")){
  cat("Performance Information:\n")
   cat("  Completion Time:", sb_backtest_workflow$completion_time, "\n")
   cat("  Elapsed Time:", sb_backtest_workflow$elapsed_time, "seconds\n")
   cat("  Parallel Processing:", sb_backtest_workflow$parallel, "\n")
   cat("=================================\n")
  }

  # Display Call Information
  cat("Call:\n")
  cat("  Function Call:\n")
  print(sb_backtest_workflow$call)
  cat("\n")

  cat("  Call Timestamp:\n")
  print(sb_backtest_workflow$timestamps, quotes = FALSE)
  cat("\n")

  cat("=========================================\n")
})



#' @title Show Method for sb_metabacktest_results Class
#' @description Displays the contents of an `sb_metabacktest_results` object,
#' including consolidated and time series evaluation metrics.
#'
#' @param object An object of class `sb_metabacktest_results`.
#' @export
setMethod("show", "sb_metabacktest_results", function(object) {

  # Extract the meta_learner_sb_backtest_workflow from the meta learner object
  meta_learner_sb_backtest_workflow <- object@meta_sb_backtest_results_list[[1]]@sb_backtest_workflow

  # Create a neat display of the meta_learner_sb_backtest_workflow
  cat(crayon::cyan("Meta Learner Workflow Metadata\n"))
  cat("Backtest Identifier: ", object@backtest_identifier, "\n")
  cat("=================================\n")

  # Display Algorithm Information
  cat("Algorithm Information:\n")
  cat(" Config Name:", meta_learner_sb_backtest_workflow$config_name, "\n")
  cat("  SB Algorithm:", meta_learner_sb_backtest_workflow$sb_algorithm, "\n")
  if(!meta_learner_sb_backtest_workflow$sb_algorithm %in% c("ew_ensemble")) cat("  Custom Objective:", meta_learner_sb_backtest_workflow$custom_objective, "\n")
  cat("  Backtest Type:", meta_learner_sb_backtest_workflow$backtest_type, "\n")
  if(meta_learner_sb_backtest_workflow$backtest_type == "meta_learner"){
    cat("    Base-Learner Config Names:", meta_learner_sb_backtest_workflow$config_name_bl, "\n")
    cat("    Base-Learner Algorithms:", meta_learner_sb_backtest_workflow$sb_algorithm_bl, "\n")
  }
  if(meta_learner_sb_backtest_workflow$sb_algorithm == "nn"){
    # Display Keras Information
    show(create_keras_architecture(
      nn_optimizer = meta_learner_sb_backtest_workflow$keras_architecture_parameters$nn_optimizer,
      units = meta_learner_sb_backtest_workflow$keras_architecture_parameters$units,
      activation = meta_learner_sb_backtest_workflow$keras_architecture_parameters$activation,
      batch_norm_option = meta_learner_sb_backtest_workflow$keras_architecture_parameters$batch_norm_option)
    )
  }


  cat("=================================\n")

  # Display Date Information
  cat("Date Information:\n")
  cat("  Range of Dates Covered:", paste(c(min(meta_learner_sb_backtest_workflow$dates_covered),max(meta_learner_sb_backtest_workflow$dates_covered)), sep = "-"), "\n")
  cat("  Number of Dates:", meta_learner_sb_backtest_workflow$n_dates, "\n")
  cat("  First Rebalance Date:", paste(meta_learner_sb_backtest_workflow$first_rebalance_date), "\n")
  cat("  Rebalance Dates:", paste(meta_learner_sb_backtest_workflow$rebalance_dates, collapse = ", "), "\n")
  if(!meta_learner_sb_backtest_workflow$sb_algorithm %in% c("ew_ensemble")) cat("  Split Method:", meta_learner_sb_backtest_workflow$split_method, "\n")

  if(meta_learner_sb_backtest_workflow$backtest_type == "meta_learner"){
    cat("-------------------------------\n")
    cat("  Base Learners Date Information:\n")
    cat("    Range of Dates Covered:", paste(c(as.Date(min(meta_learner_sb_backtest_workflow$dates_covered_bl)),
                                               as.Date(max(meta_learner_sb_backtest_workflow$dates_covered_bl))), sep ="-"), "\n")
    cat("    Number of Dates:", meta_learner_sb_backtest_workflow$n_dates_bl, "\n")
    cat("-------------------------------\n")
  }

  cat("=================================\n")

  # Display Sample Sizes
  cat("Sample Sizes:\n")
  if(!meta_learner_sb_backtest_workflow$sb_algorithm %in% c("ew_ensemble")) cat("  Training Sample Size:", meta_learner_sb_backtest_workflow$training_sample_size, "\n")
  if(!meta_learner_sb_backtest_workflow$sb_algorithm %in% c("ew_ensemble", "optimal_ensemble")) cat("  Validation Sample Size:", meta_learner_sb_backtest_workflow$validation_sample_size, "\n")
  cat("  Testing Sample Size:", meta_learner_sb_backtest_workflow$testing_sample_size, "\n")
  cat("  Range of Dates in Testing Sample:", paste(c(min(meta_learner_sb_backtest_workflow$dates_testing_sample), max(meta_learner_sb_backtest_workflow$dates_testing_sample)), sep ="-"), "\n")

  if(meta_learner_sb_backtest_workflow$backtest_type == "meta_learner"){
    cat("-------------------------------\n")
    cat("  Base Learners Sample Sizes:\n")
    cat("    Training Sample Size:", meta_learner_sb_backtest_workflow$training_sample_size_bl, "\n")
    cat("    Validation Sample Size:", meta_learner_sb_backtest_workflow$validation_sample_size_bl, "\n")
    cat("    Testing Sample Size:", meta_learner_sb_backtest_workflow$testing_sample_size_bl, "\n")
    cat("    Range of Dates in Testing Sample:", paste(c(as.Date(min(meta_learner_sb_backtest_workflow$dates_testing_sample_bl)),
                                                         as.Date(max(meta_learner_sb_backtest_workflow$dates_testing_sample_bl))), sep="-"), "\n")
    cat("    Rebalance Dates:", paste(as.Date(meta_learner_sb_backtest_workflow$rebalance_dates_bl), collapse = ", "), "\n")

    cat("-------------------------------\n")
  }

  cat("=================================\n")

  # Display Stocks Information
  cat("Stocks Information:\n")
  cat("Number of Stocks:", meta_learner_sb_backtest_workflow$n_stocks, "\n")
  cat("Number of Observations:", meta_learner_sb_backtest_workflow$nobs, "\n")
  #cat("Tickers:", paste(meta_learner_sb_backtest_workflow$tickers, collapse = ", "), "\n")

  cat("=================================\n")

  # Display Target Information
  cat("Target Information:\n")
  cat("  Forward Target Name:", meta_learner_sb_backtest_workflow$target_fwd_name, "\n")
  cat("  Target Forward:", meta_learner_sb_backtest_workflow$target_fwd, "\n")
  cat("  Target Object:", meta_learner_sb_backtest_workflow$target_object, "\n")
  cat("  Target Workflow:\n")
  print(meta_learner_sb_backtest_workflow$target_workflow)
  cat("\n")


  cat("=================================\n")

  # Display Features Information
  cat("Features Information:\n")
  cat("Features:", paste(meta_learner_sb_backtest_workflow$features, collapse = ", "), "\n")
  if(!meta_learner_sb_backtest_workflow$sb_algorithm %in% c("ew_ensemble", "optimal_ensemble")){
    cat("  Features Workflow:\n")
    print(meta_learner_sb_backtest_workflow$features_workflow)
    cat("\n")
  }
  if(!meta_learner_sb_backtest_workflow$sb_algorithm %in% c("ew_ensemble", "optimal_ensemble")) cat("Features Object:", meta_learner_sb_backtest_workflow$features_object, "\n")

  cat("=================================\n")

  # Display Tuning Information
  if(!meta_learner_sb_backtest_workflow$sb_algorithm %in% c("ols", "ew_ensemble", "optimal_ensemble")){
    cat("Tuning Information:\n")
    cat("  Tuning Method:", meta_learner_sb_backtest_workflow$tuning_method, "\n")
    if(meta_learner_sb_backtest_workflow$tuning_method == "random_search" || meta_learner_sb_backtest_workflow$tuning_method == "bayesian_opt"){
      cat("  Number of Iterations:", meta_learner_sb_backtest_workflow$n_iter, "\n")
      if(meta_learner_sb_backtest_workflow$tuning_method == "bayesian_opt"){
        cat("  Number of Samples of Eval Function:", meta_learner_sb_backtest_workflow$k_iter, "\n")
        cat("  Acquisition Function:", meta_learner_sb_backtest_workflow$acq, "\n")
        cat("  Number of Points to Init Process:", meta_learner_sb_backtest_workflow$init_points, "\n")
      }
    }
    cat("  Hyperparameter Grid Domain List:", paste(names(meta_learner_sb_backtest_workflow$hyper_grid_domain_list), collapse = ", "), "\n")
    cat("  Chosen Evaluation Metric:", meta_learner_sb_backtest_workflow$chosen_eval_metric, "\n")
    cat("  Huber Delta:", meta_learner_sb_backtest_workflow$huber_delta, "\n")
    cat("  Quantile Tau:", meta_learner_sb_backtest_workflow$quantile_tau, "\n")
    cat("  Early Stop:", meta_learner_sb_backtest_workflow$early_stop, "\n")

    cat("=================================\n")
  }


  # Display Performance Information
  if(!meta_learner_sb_backtest_workflow$sb_algorithm %in% c("ew_ensemble", "optimal_ensemble")){
    cat("Performance Information:\n")
    cat("  Completion Time:", meta_learner_sb_backtest_workflow$completion_time, "\n")
    cat("  Elapsed Time:", meta_learner_sb_backtest_workflow$elapsed_time, "seconds\n")
    cat("  Parallel Processing:", meta_learner_sb_backtest_workflow$parallel, "\n")
    cat("=================================\n")
  }

  # Display Call Information
  cat("Call:\n")
  cat("  Function Call:\n")
  print(meta_learner_sb_backtest_workflow$call)
  cat("\n")

  cat("  Call Timestamp:\n")
  print(meta_learner_sb_backtest_workflow$timestamps, quotes = FALSE)
  cat("\n")

  cat("=========================================\n")

  # Create a neat display for EW
  cat(crayon::magenta("\n\nEW Ensemble Workflow Metadata\n"))
  ew_sb_backtest_workflow <- object@meta_sb_backtest_results_list[[2]]@sb_backtest_workflow

  cat("Backtest Identifier: ", ew_sb_backtest_workflow$backtest_identifier, "\n")
  cat("=================================\n")

  #Algo Info
  cat("Algorithm Information:\n")
  cat("  Config Name:", ew_sb_backtest_workflow$config_name, "\n")
  cat("-------------------------------\n")

  # Display Date Information
  cat("Date Information:\n")
  cat("  Range of Dates Covered:", paste(c(min(ew_sb_backtest_workflow$dates_covered),max(ew_sb_backtest_workflow$dates_covered)), sep = "-"), "\n")
  cat("  Number of Dates:", ew_sb_backtest_workflow$n_dates, "\n")
  cat("  First Rebalance Date:", paste(ew_sb_backtest_workflow$first_rebalance_date), "\n")
  cat("  Rebalance Dates:", paste(ew_sb_backtest_workflow$rebalance_dates, collapse = ", "), "\n")

  cat("-------------------------------\n")
  # Display Sample Sizes
  cat("Sample Sizes:\n")
  cat("  Testing Sample Size:", ew_sb_backtest_workflow$testing_sample_size, "\n")
  cat("  Range of Dates in Testing Sample:", paste(c(min(ew_sb_backtest_workflow$dates_testing_sample), max(ew_sb_backtest_workflow$dates_testing_sample)), sep ="-"), "\n")

  cat("=================================\n")

  # Create a neat display for Opt
  cat(crayon::magenta("\n\nOptimal Ensemble Workflow Metadata\n"))
  opt_sb_backtest_workflow <- object@meta_sb_backtest_results_list[[3]]@sb_backtest_workflow

  cat("Backtest Identifier: ", opt_sb_backtest_workflow$backtest_identifier, "\n")
  cat("=================================\n")

  #Algo Info
  cat("Algorithm Information:\n")
  cat("  Config Name:", opt_sb_backtest_workflow$config_name, "\n")
  cat("  Ensemble Eval Metric:", opt_sb_backtest_workflow$chosen_eval_metric, "\n")
  cat("  Ensemble Huber Delta:", opt_sb_backtest_workflow$huber_delta, "\n")
  cat("  Ensemble Quantile Tau:", opt_sb_backtest_workflow$quantile_tau, "\n")
  cat("-------------------------------\n")

  cat("Date Information:\n")
  cat("  Range of Dates Covered:", paste(c(min(opt_sb_backtest_workflow$dates_covered),max(opt_sb_backtest_workflow$dates_covered)), sep = "-"), "\n")
  cat("  Number of Dates:", opt_sb_backtest_workflow$n_dates, "\n")
  cat("  First Rebalance Date:", paste(opt_sb_backtest_workflow$first_rebalance_date), "\n")
  cat("  Rebalance Dates:", paste(opt_sb_backtest_workflow$rebalance_dates, collapse = ", "), "\n")

  cat("-------------------------------\n")
  # Display Sample Sizes
  cat("Sample Sizes:\n")
  cat("  Testing Sample Size:", opt_sb_backtest_workflow$testing_sample_size, "\n")
  cat("  Range of Dates in Testing Sample:", paste(c(min(opt_sb_backtest_workflow$dates_testing_sample), max(opt_sb_backtest_workflow$dates_testing_sample)), sep ="-"), "\n")

  cat("=================================\n")




})

#' @title Show Signal Selection Backtest Config
#' @description Prints the contents of an `ss_backtest_config` object, detailing the various parameters and their configurations.
#'
#' @param object An `ss_backtest_config` object to be displayed.
#' @method show ss_backtest_config
#' @export
setMethod("show", "ss_backtest_config", function(object) {
  cat("==============================\n")
  cat("Signal Selection Backtest Configuration\n")
  cat("==============================\n\n")

  # Display Main Information
  cat("------------------------------\n")
  cat("Main Information:\n")
  cat("------------------------------\n")
  cat("Config Name:", object@config_name, "\n\n")


  # Display Backtest Parameters
  cat("------------------------------\n")
  cat("Backtest Parameters:\n")
  cat("------------------------------\n")
  cat("Data Availability Cutoff:", object@data_availability_cutoff, "\n")
  cat("Initial Sample Size:", object@initial_sample_size, "\n")
  cat("Rebalancing Months:", object@rebalancing_months, "\n")
  cat("Active Returns:", object@active_returns, "\n")
  cat("Split Method:", object@split_method, "\n")
  cat("Chosen Signals and Positions:\n")
  print(object@chosen_signals_and_positions, quote = FALSE)



  # Display Alpha Test Strategy
  cat("------------------------------\n")

  if (!is.null(object@alpha_test_strategy)) {
    show(object@alpha_test_strategy)
  } else {
    cat("  No Alpha Test Strategy set.\n")
  }

  cat("\n==============================\n")
})


#' @title Show Alpha Test Strategy
#' @description Prints the contents of an `alpha_test_strategy` object, detailing the various parameters and their configurations.
#' @param object An `alpha_test_strategy` object to be displayed.
#' @method show alpha_test_strategy
#' @export
setMethod("show", "alpha_test_strategy", function(object) {

  # Display Main Information
  cat("------------------------------\n")
  cat("Main Information:\n")
  cat("------------------------------\n")
  cat("Signal Significance Threshold:", object@signal_significance_threshold, "\n")
  cat("P Correction Method:", object@p_correction_method, "\n")
  cat("Market Factor Proxy:", object@market_factor_proxy, "\n")
  cat("Enable Theme Representativeness:", object@enable_theme_representativeness, "\n\n")


    #Display Model Structure
  cat("------------------------------\n")
  cat("Model Structure:\n")
  cat("------------------------------\n")
  cat("Model Structure:", object@model_structure, "\n")
  if(!is.null(object@theme_level_intercept)){
    cat("Theme-Level Intercept:", object@theme_level_intercept, "\n")
  }
  if(!is.null(object@theme_level_slope)){
    cat("Theme-Level Slope:", object@theme_level_slope, "\n")
  }

  # Display lmer Control Parameters
  if (!is.null(object@lmer_control)) {
    cat("\nlmer Control Parameters:\n")
    cat("------------------------------\n")
    for (param_name in names(object@lmer_control)) {
      cat("  ", param_name, ": ", object@lmer_control[[param_name]], "\n")
    }
  } else {
    cat("\nNo lmer Control Parameters set.\n")
  }

})


#' @title Show Frequentist Alpha Test Strategy
#' @description Prints the contents of a `frequentist_alpha_test_strategy` object, detailing the various parameters and their configurations.
#' @param object A `frequentist_alpha_test_strategy` object to be displayed.
#' @method show frequentist_alpha_test_strategy
#' @export
setMethod("show", "frequentist_alpha_test_strategy", function(object) {
  cat("==============================\n")
  cat("Frequentist Alpha Test Strategy Configuration\n")
  cat("==============================\n")

  # Call the parent class show method to display common information
  callNextMethod()

})


#' @title Show Bayesian Alpha Test Strategy
#' @description Prints the contents of a `bayesian_alpha_test_strategy` object, detailing the various parameters and their configurations.
#' @param object A `bayesian_alpha_test_strategy` object to be displayed.
#' @method show bayesian_alpha_test_strategy
#' @export
setMethod("show", "bayesian_alpha_test_strategy", function(object) {
  cat("==============================\n")
  cat("Bayesian Alpha Test Strategy Configuration\n")
  cat("==============================\n")

  # Call the parent class show method to display common information
  callNextMethod()

  # Display Bayesian Model Parameters
  cat("\n------------------------------\n")
  cat("Bayesian Model Parameters:\n")
  cat("------------------------------\n")

  bayesian_params <- object@bayesian_model_parameters
  if (!is.null(bayesian_params)) {
    show(bayesian_params)
  } else {
    cat("No Bayesian Model Parameters set.\n")
  }

})

#' @title Show Bayesian Model Parameters
#' @description Prints the contents of a `bayesian_model_parameters` object, detailing the various parameters and configurations.
#' @param object A `bayesian_model_parameters` object to be displayed.
#' @method show bayesian_model_parameters
#' @export
setMethod("show", "bayesian_model_parameters", function(object) {

  # Display Prior Derivation Control
  if (!is.null(object@prior_derivation_control)) {
    cat("\nPrior Derivation Control:\n")
    cat("------------------------------\n")
    for (param_name in names(object@prior_derivation_control)) {
      cat("  ", param_name, ": ", object@prior_derivation_control[[param_name]], "\n")
    }
  } else {
    cat("\nNo Prior Derivation Control set.\n")
  }

  # Display brms Control Parameters
  if (!is.null(object@brms_control)) {
    cat("\nbrms Control Parameters:\n")
    cat("------------------------------\n")
    for (param_name in names(object@brms_control)) {
      cat("  ", param_name, ": ", object@brms_control[[param_name]], "\n")
    }
  } else {
    cat("\nNo brms Control Parameters set.\n")
  }

  # Display User Priors
  if (!is.null(object@user_priors)) {
    cat("\nUser Priors:\n")
    cat("------------------------------\n")
    print(object@user_priors)
  } else {
    cat("\nNo User Priors set.\n")
  }
})

#' Show Method for ss_backtest_results Class
#'
#' This method displays a detailed summary of the `ss_backtest_results` object,
#' including metadata on the signal selection backtest results,
#' configuration details, date information, signals information,
#' p-value correction methods, Bayesian model parameters (if applicable),
#' performance metrics, and the original call.
#'
#' @param object An instance of the `ss_backtest_results` class.
#'
#' @return The method returns the object invisibly.
#'
#' @export
setMethod("show", "ss_backtest_results", function(object) {

  # Extract the ss_backtest_workflow
  ss_backtest_workflow <- object@ss_backtest_workflow

  # Create a neat display of the ss_backtest_workflow
  cat("Signal Selection Backtest Workflow Metadata\n")
  cat("Backtest Identifier: ", object@backtest_identifier, "\n")
  cat("=========================================\n")

  # Display Backtest Configuration Information
  cat("Backtest Configuration:\n")
  cat("  Config Name: ", ss_backtest_workflow$config_name, "\n")
  cat("  Backtest Type: ", ss_backtest_workflow$backtest_type, "\n")
  cat("  Active Returns: ", ss_backtest_workflow$active_returns, "\n")
  cat("  Alpha Test Strategy Parameters:\n")
  cat("    Model Structure: ", ss_backtest_workflow$model_structure, "\n")
  if(!ss_backtest_workflow$model_structure == "no_pooled"){
  cat("    Theme-Level Intercept: ", ss_backtest_workflow$theme_level_intercept, "\n")
  cat("    Theme-Level Slope: ", ss_backtest_workflow$theme_level_slope, "\n")
  }
  cat("    Market Factor Proxy: ", ss_backtest_workflow$market_factor_proxy, "\n")
  cat("    Data Availability Cutoff: ", ss_backtest_workflow$data_availability_cutoff, "\n")
  cat("    Signal Significance Threshold: ", ss_backtest_workflow$signal_significance_threshold, "\n")
  cat("    Enable Theme Representativeness: ", ss_backtest_workflow$enable_theme_representativeness, "\n")
  cat("    lmer Control Parameters:\n")
  if(is.null(ss_backtest_workflow$lmer_control)) {
    cat("      No lmer Control set\n")
  } else {
    print(ss_backtest_workflow$lmer_control)
  }


  cat("=========================================\n")

  # Display P-Value Correction Method
  cat("P-Value Correction Method:\n")
  cat("  Method: ", ss_backtest_workflow$p_correction_method, "\n")
  if (ss_backtest_workflow$p_correction_method == "bayesian") {
    # Display Bayesian Model Parameters
    cat("Bayesian Model Parameters:\n")
    cat("  User Priors: ", ifelse(is.null(ss_backtest_workflow$user_priors), "NULL", "Provided"), "\n")
    cat("  brms Control Parameters:\n")
    print(ss_backtest_workflow$brms_control)
    cat("  Prior Derivation Control Parameters:\n")
    print(ss_backtest_workflow$prior_derivation_control)
  }
  cat("=========================================\n")

  # Display Date Information
  cat("Date Information:\n")
  cat("  Dates Covered: ", paste(as.character(range(ss_backtest_workflow$dates_covered)), collapse = " - "), "\n")
  cat("  Number of Dates: ", ss_backtest_workflow$n_dates, "\n")
  cat("  Initial Sample Size: ", ss_backtest_workflow$initial_sample_size, "\n")
  cat("  Rebalancing Months: ", paste(ss_backtest_workflow$rebalancing_months, collapse = ", "), "\n")
  cat("  Rebalance Dates: ", paste(as.character(ss_backtest_workflow$rebalance_dates), collapse = ", "), "\n")
  cat("  Number of Rebalance Months: ", ss_backtest_workflow$n_rebalance_months, "\n")
  cat("  First Rebalance Date: ", as.character(ss_backtest_workflow$first_rebalance_date), "\n")
  cat("  Split Method: ", ss_backtest_workflow$split_method, "\n")
  cat("=========================================\n")

  # Display Signals Information
  cat("Signals Information:\n")
  cat("  Chosen Signals and Positions:\n")
  print(ss_backtest_workflow$chosen_signals_and_positions)
  cat("  Number of Signals: ", ss_backtest_workflow$n_signals, "\n")
  cat("  Selected Signals with Corrected Positions:\n")
  cat("    ", paste(ss_backtest_workflow$selected_signals_corrected_positions, collapse = ", "), "\n")
  if (!is.null(ss_backtest_workflow$signals_workflow)) {
    cat("  Signals Workflow:\n")
    print(ss_backtest_workflow$signals_workflow)
  }
  if (!is.null(ss_backtest_workflow$signals_object)) {
    cat("  Signals Object Name: ", ss_backtest_workflow$signals_object, "\n")
  }
  cat("=========================================\n")

  # Display Signal Themes Information
  if (!is.null(ss_backtest_workflow$signal_themes_workflow)) {
    cat("Signal Themes Information:\n")
    cat("  Signal Themes Workflow:\n")
    print(ss_backtest_workflow$signal_themes_workflow)
    cat("  Signal Themes Object Name: ", ss_backtest_workflow$signal_themes_object, "\n")
    cat("=========================================\n")
  }

  # Display Priors Information
  if (ss_backtest_workflow$p_correction_method == "bayesian") {
    cat("Priors Information:\n")
    if (!is.null(ss_backtest_workflow$priors_workflow)) {
      cat("  Priors Workflow:\n")
      print(ss_backtest_workflow$priors_workflow)
      cat("  Priors Object Name: ", ss_backtest_workflow$priors_object, "\n")
    } else {
      cat("  No priors workflow information available.\n")
    }
    cat("=========================================\n")
  }

  # Display Winsorization Information
  cat("Winsorization Parameters:\n")
  cat("  Lower Quantile Winsorization: ", ss_backtest_workflow$lower_quantile_winsorization, "\n")
  cat("  Upper Quantile Winsorization: ", ss_backtest_workflow$upper_quantile_winsorization, "\n")
  cat("=========================================\n")

  # Display Performance Information
  cat("Performance Information:\n")
  cat("  Elapsed Time: ", ss_backtest_workflow$elapsed_time[["elapsed"]], " seconds\n")
  if (!is.null(ss_backtest_workflow$timestamps)) {
    cat("  Timestamps:\n")
    print(ss_backtest_workflow$timestamps, quote = FALSE)
  }
  cat("=========================================\n")

  # Display Call Information
  if (!is.null(ss_backtest_workflow$call)) {
    cat("Call:\n")
    cat("  Function Call:\n")
    print(ss_backtest_workflow$call)
    cat("=========================================\n")
  }
})


#############################################

#' @title Show Portfolio Policies
#' @description Prints the contents of a `port_backtest_config` object, detailing
#' the various policies and their configurations.
#'
#' @param object A `port_backtest_config` object to be displayed.
#'
#' @method show port_backtest_config
#' @export
setMethod("show", "port_backtest_config", function(object) {
  cat("Portfolio Policie:\n")

  # Display Signal Selection Policy
  cat("\nSignal Selection Policy:\n")
  if (length(object@signal_selection_policy) == 0) {
    cat("  No signal selection policy set.\n")
  } else {
    cat("  Main info\n")
    # Show chosen signals with their respective positions
    chosen_signals_with_positions <- paste0(
      object@signal_selection_policy$chosen_signals,
      " (",
      object@signal_selection_policy$signal_positions,
      ")",
      collapse = ", "
    )
    cat("  Chosen Signals:", chosen_signals_with_positions, "\n")
    cat("  Blending Method:", object@signal_selection_policy$signal_blending_method, "\n")

    if (object@signal_selection_policy$signal_blending_method %in% c("SW", "MTO")) {
      cat("  Chosen Signal-Blending Metric:", object@signal_selection_policy$chosen_sb_metric, "\n")
    }

    cat("\n")
    cat("  Signal Eligibility Criteria\n")
    cat("  Alpha Significance Threshold:", object@signal_selection_policy$signal_significance_threshold, "\n")
    cat("  Min Number of Periods to Include Signal:", object@signal_selection_policy$data_availability_cutoff, "\n")
    cat("  Multiple Testing Adjustment:", object@signal_selection_policy$p_correction_method, "\n")

    if (object@signal_selection_policy$p_correction_method == "bayesian") {
      cat("\n")
      cat("  Bayesian Adjustment Criteria\n")
      cat("  Priors Type:", object@signal_selection_policy$priors_type, "\n")
      if(object@signal_selection_policy$priors_type %in% c("all", "mean")){
        cat("  Dataset Used to Inform Priors:", object@signal_selection_policy$priors_informative_data, "\n")
      }

    }

    if (object@signal_selection_policy$signal_blending_method == "MTO") {
      cat("\n")
      cat("  Signal Blending Restrictions:\n")
      cat("  Multisignal Portfolio Benchmark:", object@signal_selection_policy$sb_benchmark_weighting, "\n")
      cat("  Max Abs Active Individual Weight:", object@signal_selection_policy$max_abs_active_individual_weight, "\n")
      cat("  Max Abs Active Group Weight:", object@signal_selection_policy$max_abs_active_group_weight, "\n")
    }
  }

  cat("\n=================================\n")

  # Display Liquidity Constraint Policy
  cat("\nLiquidity Constraint Policy:\n")
  if (length(object@liquidity_constraint_policy) == 0) {
    cat("  No liquidity constraint policy set.\n")
  } else {
    if (!is.null(object@liquidity_constraint_policy$liquidity_floor_rule)) {
      cat("  Liquidity Floor Rule:", object@liquidity_constraint_policy$liquidity_floor_rule, "\n")
    } else {
      cat("  No liquidity floor rule set.\n")
    }
    if (length(object@liquidity_constraint_policy) > 1) {
      cat("  Liquidity Cap Rules:\n")
      for (rule in names(object@liquidity_constraint_policy)[-1]) {
        liquidity_rule <- object@liquidity_constraint_policy[[rule]]
        cat("    - Liquidity Classification:", liquidity_rule$liquidity_classification,
            "| Liquidity Cap:", liquidity_rule$liquidity_cap, "\n")
      }
    } else {
      cat("  No liquidity cap rules set.\n")
    }
  }

  cat("\n=================================\n")

  # Display Turnover Constraint Policy
  cat("\nTurnover Constraint Policy:\n")
  if (length(object@turnover_constraint_policy) == 0) {
    cat("  No turnover constraint policy set.\n")
  } else {
    cat("  Number of Turnover Rules:", length(object@turnover_constraint_policy), "\n")
    for (rule in object@turnover_constraint_policy) {
      cat("    - Liquidity Classification:", rule$liquidity_classification,
          "| Turnover Cap:", rule$turnover_cap,
          "| Top Stock Quantile Buffer Rule:", rule$top_stock_quantile_buffer, "\n")
    }
  }

  cat("\n=================================\n")

  # Display Concentration Constraint Policy
  cat("\nConcentration Constraint Policy:\n")
  if (length(object@concentration_constraint_policy) == 0) {
    cat("  No concentration constraint policy set.\n")
  } else {
    cat("  Benchmark:", object@concentration_constraint_policy$benchmark, "\n")
    cat("  Max Absolute Active Individual Weight:", object@concentration_constraint_policy$max_abs_active_individual_weight, "\n")
    cat("  Max Absolute Active Group Weights:\n")
    if (!is.null(object@concentration_constraint_policy$max_abs_active_group_weight)) {
      for (name in names(object@concentration_constraint_policy$max_abs_active_group_weight)) {
        cat("    -", name, ":", object@concentration_constraint_policy$max_abs_active_group_weight[[name]], "\n")
      }
    }
  }

  cat("\n=================================\n")

  # Display Liquidity Floor Cutoffs
  cat("\nLiquidity Floor Cutoffs:\n")
  if (length(object@liquidity_floor_cutoffs) == 0) {
    cat("  No liquidity floor cutoffs set.\n")
  } else {
    for (classification in names(object@liquidity_floor_cutoffs)) {
      cat("  Classification:", classification, "\n")
      for (metric in names(object@liquidity_floor_cutoffs[[classification]])) {
        cat("    - Metric:", metric, "| Value:", object@liquidity_floor_cutoffs[[classification]][[metric]], "\n")
      }
    }
  }

  cat("\n")
  cat("=================================\n")
})


#' @title Show a \code{port} object
#' @description
#' Provides a concise summary of a \code{port} object, including its subclass,
#' portfolio name, construction method, eligible assets, weights, covariance/correlation
#' matrices, and key parameters for MVO or RP methods.
#'
#' @param object An instance of class \code{port} or one of its subclasses (e.g.,
#'   \code{signal_port}, \code{signal_blend_stock_port}, \code{single_signal_stock_port}).
#'
#' @return Returns the \code{object} invisibly.
#'
#' @seealso \linkS4class{port}
#' @importFrom methods setMethod show
#' @export
setMethod(
  f = "show",
  signature = "port",
  definition = function(object) {

    # 1) Class Identification
    # Check if object is one of the subclasses
    subclass <- if (is(object, "signal_port")) {
      "signal_port"
    } else if (is(object, "stock_port")) {
      "stock_port"
    }  else {
      "port"
    }

    # 2) Print Header
    if(subclass == "port"){
      cat("Portfolio Summary:\n")
    }
    if(subclass == "signal_port"){
      cat("Signal Portfolio Summary:\n")
    }
    if(subclass == "stock_port"){
      cat("Stock Portfolio Summary:\n")
      if(object@type == "signal_blend") cat("\n Signal-Blend")
      if(object@type == "single_signal") cat("\n Single Signal")
    }


    cat("=================================\n")
    cat("Class:                ", subclass, "\n")
    if(subclass == "stock_port"){
    cat("Type:                 ", object@type, "\n")
    }
    cat("Portfolio Name:       ", object@port_name, "\n")
    cat("Method:               ", object@port_construction_method, "\n")
    if(subclass == "signal_port"){
    cat("Heuristic SB Metric:  ", object@heuristic_sb_metric, "\n")
    }
    cat("Eligible Assets:      ", paste(object@eligible_assets, collapse = ", "), "\n")
    cat("Number of Assets:     ", length(object@eligible_assets), "\n")

    if(!is.null(object@exp_ret_score)){
    port_exp_ret_score <- object@weights %*% object@exp_ret_score
    cat("Port Expected Return: ", round(port_exp_ret_score, 3), "\n")
    }
    if(!is.null(object@covariance_matrix)){
    cov_matrix <- object@covariance_matrix
    port_exp_risk <- sqrt(object@weights %*% cov_matrix %*% object@weights)
    cat("Port Expected Risk:   ", round(port_exp_risk, 3), "\n")
    }
    if(!is.null(object@exp_ret_score) && !is.null(object@covariance_matrix)){
    port_sharpe_ratio <- port_exp_ret_score / port_exp_risk
    cat("Port Expected Sharpe: ", round(port_sharpe_ratio, 3), "\n")
    }


    # 3) Weights and Return Scores
    cat("\nWeights (first few shown):\n")
    weights_vector <- object@weights
    names(weights_vector) <- object@eligible_assets
    print(utils::head(round(weights_vector, 3), n = 10))

    if (!is.null(object@exp_ret_score)) {
      cat("\nExpected Return Score (first few):\n")
      exp_ret_score_vector <- object@exp_ret_score
      names(exp_ret_score_vector) <- object@eligible_assets
      print(utils::head(round(exp_ret_score_vector, 2), n = 10))
    } else {
      cat("\nNo expected return score provided.\n\n")
    }

    # 4) Correlation Matrix
    if (!is.null(object@correlation_matrix)) {
      cat("\nCorrelation Matrix (first few rows shown):\n")
      print(utils::head(round(object@correlation_matrix, 2), n = 10))
      cat("\n")
      cat("Relative Risk Contribution (first few):\n")
      rel_risk_contr_vector <- object@rel_risk_contr
      names(rel_risk_contr_vector) <- object@eligible_assets
      print(utils::head(round(rel_risk_contr_vector, 3), n = 10))
    } else {
      cat("No correlation matrix.\n")
    }


    # 5) Additional Fields (if they exist)
    if (!is.null(object@ind_max_weights)) {
      ind_constraints_df <- data.frame(assets = object@eligible_assets,
                                    ind_max_weights = object@ind_max_weights, ind_min_weights = object@ind_min_weights)

      cat("\nIndividual Max and Min Weights (first few):\n")
      print(utils::head(ind_constraints_df, 10))
    }
    if (!is.null(object@groups)) {
      cat("\nGroups Provided:\n")
      print(object@groups %>% dplyr::select(tickers, theme))
    } else {
      cat("\nNo groups specified.\n")
    }

    # Wrap up
    cat("\n=================================\n")

    # Return invisibly
    invisible(object)
  }
)
