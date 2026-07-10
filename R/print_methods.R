#meta_dataframe-----------------------------
#' Show Method for meta_dataframe Class
#'
#' This method displays a summary of the `meta_dataframe` object, including its name and current date, the
#' signal (or target) columns and their count, the unique dates and tickers, the total number of observations,
#' and the first few rows of the data.
#'
#' @param object An instance of the `meta_dataframe` class.
#'
#' @return The method returns the object invisibly.
#' @importFrom methods show
#' @export
setMethod("show", "meta_dataframe", function(object) {

  # Print a summary of the sb_backtest_workflow
  cat("Meta Dataframe Show Method:\n")
  cat("=================================\n")
  cat("Meta Dataframe name: ", object@meta_dataframe_name, " \n")
  cat("Current date :", paste(as.Date(object@current_date)), " \n\n")
  if(object@class == "target_m_df"){
    cat(" Targets:\n")
  } else {
    cat("Signals:\n")
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

  cat("=================================\n")

  # Print the first few rows of the data
  cat("\nFirst few rows of the data:\n")
  print(utils::head(object@data))

  # Return the object invisibly
  invisible(object)
})


#' Show Method for groups_m_df Class
#'
#' This method displays a summary of the `groups_m_df` object, including its name, the grouping columns and
#' their count, the distinct classifications within each grouping column, the unique dates and tickers, the
#' total number of observations, and the first few rows of the data.
#'
#' @param object An instance of the `groups_m_df` class.
#'
#' @return The method returns the object invisibly.
#'
#' @export
setMethod("show", "groups_m_df", function(object) {

  # Print a summary of the sb_backtest_workflow
  cat("Meta Dataframe Groups Show Method:\n")
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

  cat("=================================\n")

  # Print the first few rows of the data
  cat("\nFirst few rows of the data:\n")
  print(utils::head(object@data))

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
  cat("Signal Universe Show Method:\n")
  cat("=================================\n")
  cat("Object name: ", object@meta_dataframe_name, " \n\n")
  cat(" Performance Metrics:\n")
  cat(paste(setdiff(object@signals, c("pre_eligible_assets", "theme_ss_bench_weights", "theme_sb_bench_weights", "theme", "is_eligible")), collapse = ", "))
  cat("  \nNumber of performance metrics:", ncol(object@data)-3-5, "\n")
  cat(" \nDates:\n")
  print(unique(as.Date(object@data$dates)))
  cat("  Number of unique dates:", object@unique_dates, "\n")
  cat(" \nTickers (Signals):\n", unique(object@data$tickers), "\n")
  cat("  Number of unique tickers:", object@unique_tickers, "\n")
  cat("\nTotal Observations (n_obs):", object@n_obs, "\n")

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
    cat("\n")
  }

  # Print the first few rows of the data
  cat("\nFirst few rows of the data:\n")
  print(utils::head(object@data))

  # Return invisibly
  invisible(object)
})

# A small helper for safely extracting list elements or returning a default if not found.
`%||%` <- function(x, default) {
  if (!is.null(x)) x else default
}


#tickers_catalog-----------------------------
#' Print method for tickers_catalog
#'
#' @description Displays key information about a `tickers_catalog` object: its source meta_dataframe name and
#' current reference date, the total number of tickers, the untraded / delisted / old / listed classifications,
#' the delisting tolerance (`n_days_tolerance`), and the first few rows of the `catalog` slot.
#'
#' @param object An instance of the \code{tickers_catalog} class.
#'
#' @return Called for its side effect of printing a summary of the catalog.
#' @export
setMethod("show", "tickers_catalog", function(object) {
  cat("\nTickers Catalog Object\n")
  cat("----------------------\n")
  cat(" Metadataframe Obj:", object@meta_dataframe_name, "\n")
  cat(" Current date reference:", paste(as.Date(object@current_date)), "\n\n")
  cat(" Total tickers:", nrow(object@catalog), "\n")
  cat(" Untraded tickers:", object@untraded, "\n")
  cat(" Delisted tickers:", object@delisted, "\n")
  cat(" Old tickers:", object@old, "\n")
  cat(" Listed tickers:", object@listed, "\n")

  cat(" Number of days of tolerance:", object@n_days_tolerance, "\n")

  #Short first few rows of the catalog
  cat("\nFirst few rows of the catalog:\n")
  print(utils::head(object@catalog))
})


#meta_xts------------------------------------------
#' @title Show method for meta_xts
#' @description
#' Shows a summary of the \code{meta_xts} object, including metadata and
#' the first few rows of the underlying \code{xts} data.
#'
#' @param object An object of class \code{meta_xts}.
#'
#' @return Returns the object invisibly after printing its summary.
#'
#'
#' @export
setMethod("show", "meta_xts", function(object) {

  # Print a summary of the meta_xts object
  cat("meta_xts Show Method:\n")
  cat("=================================\n")
  cat("Name:", object@meta_xts_name, "\n")
  cat("Number of dates (n_dates):", object@n_dates, "\n")
  cat("Frequency:", object@frequency, "\n")
  cat("Source(s):", paste(unique(object@source), collapse = ", "), "\n")


  cat("=================================\n")

  # Print the first few rows of the data slot
  cat("\nFirst few rows of 'data' (xts):\n")
  print(utils::head(object@data))

  # Return the object invisibly
  invisible(object)
})

#' @title Show method for returns_meta_xts
#' @description
#' Shows a summary of the \code{returns_meta_xts} object, including metadata
#' and the first few rows of the underlying \code{xts} data.
#'
#' @param object An object of class \code{returns_meta_xts}.
#'
#' @return Returns the object invisibly after printing its summary.
#'
#'
#' @export
setMethod("show", "returns_meta_xts", function(object) {

  # Print a summary of the returns_meta_xts object
  cat("returns_meta_xts Show Method:\n")
  cat("=================================\n")
  cat("Name:", object@meta_xts_name, "\n")
  cat("Number of dates (n_dates):", object@n_dates, "\n")
  cat("Frequency:", object@frequency, "\n")
  cat("Source(s):", paste(unique(object@source), collapse = ", "), "\n")

  cat("\nAssets Info:\n")
  cat("  asset_type:", object@asset_type, "\n")
  cat("  assets:", paste(object@assets, collapse = ", "), "\n")
  cat("  number of assets (n_assets):", object@n_assets, "\n")

  cat("=================================\n")

  # Print the first few rows of the data slot
  cat("\nFirst few rows of 'data' (xts):\n")
  print(utils::head(object@data))

  # Return the object invisibly
  invisible(object)
})

#' @title Show method for metrics_meta_xts
#' @description
#' Shows a summary of the \code{metrics_meta_xts} object, including metadata
#' and the first few rows of the underlying \code{xts} data.
#'
#' @param object An object of class \code{metrics_meta_xts}.
#'
#' @return Returns the object invisibly after printing its summary.
#'
#'
#' @export
setMethod("show", "metrics_meta_xts", function(object) {

  # Print a summary of the metrics_meta_xts object
  cat("metrics_meta_xts Show Method:\n")
  cat("=================================\n")
  cat("Name:", object@meta_xts_name, "\n")
  cat("Number of dates (n_dates):", object@n_dates, "\n")
  cat("Frequency:", object@frequency, "\n")
  cat("Source(s):", paste(unique(object@source), collapse = ", "), "\n")


  cat("\nSeries Info:\n")
  cat("  Metric Name: ", object@metric_name, "\n")
  cat("  series:", paste(object@series, collapse = ", "), "\n")
  cat("  number of series:", object@n_series, "\n")

  cat("=================================\n")

  # Print the first few rows of the data slot
  cat("\nFirst few rows of 'data' (xts):\n")
  print(utils::head(object@data))

  # Return the object invisibly
  invisible(object)
})





#hyper_grid_domain------------------------------------------
#' Print method for hyper_grid_domain
#'
#' This method prints the contents of a `hyper_grid_domain` object in a user-friendly format.
#'
#' @param object A `hyper_grid_domain` object to be printed.
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


#tuning_strategy------------------------------------------
#' @title Show Method for `tuning_strategy`
#' @description Custom show method for displaying the general information of objects that extend `tuning_strategy`.
#' This method prints the tuning method, machine learning algorithm, validation sample size, split method, evaluation metric,
#' early stopping criteria, and the hyperparameter grid domain.
#' @param object An object of class `tuning_strategy` or its subclasses (`grid_search_strategy`, `random_search_strategy`, or `bayesian_opt_strategy`).
#' @return Printed information about the base properties of the object.
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
#' @export
setMethod("show", "grid_search_strategy", function(object) {
  cat("Grid Search Tuning Strategy\n")
  methods::callNextMethod()  # Calls the base show method for common slots
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
#' @export
setMethod("show", "random_search_strategy", function(object) {

  cat("Random Search Tuning Strategy\n")
  methods::callNextMethod()  # Calls the base show method for common slots
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
#' @export
setMethod("show", "bayesian_opt_strategy", function(object) {
  cat("Bayesian Optimization Tuning Strategy\n")
  methods::callNextMethod()  # Calls the base show method for common slots
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


#keras_architecture_parameters------------------------------------------------
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


#sb_backtest_config------------------------------------------------
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

  if(!object@sb_algorithm %in% c("ew","rp","custom_weights"))  cat("  Custom Objective:", object@custom_objective, "\n")

  # Display Miscellaneous Parameters
  cat("  Function Parameters:")
  cat("  Huber Delta:", object@huber_delta)
  cat("  Quantile Tau:", object@quantile_tau, "\n")

  #Display ML stuff
  if(!object@sb_algorithm %in% c("ols", "ew", "sw", "rp", "hrp", "mvo", "mmaf")){
    cat("------------------------------\n")

    # Display Keras Architecture Parameters Information
    if (object@sb_algorithm == "nn"){
      if (is.null(object@keras_architecture_parameters)) {
        cat("  No Keras architecture parameters set.\n\n")
      } else {
        cat("\n")
        methods::show(object@keras_architecture_parameters)
      }
    }



    # Display Hyperparameter Tuning Information
    if(is.null(object@tuning_strategy)){
      cat("  No tuning strategy set.\n")
    } else {
      methods::show(object@tuning_strategy)

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
  if(object@sb_algorithm %in% c("rp", "hrp", "mvo", "mmaf")){
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

      # RP parameters
      if (object@sb_algorithm == "rp"){
        show(signal_port_parameters@rp_parameters)
      }

      # HRP Parameters
      if (object@sb_algorithm == "hrp"){
        show(signal_port_parameters@hrp_parameters)
      }

      # MVO parameters
      if (object@sb_algorithm == "mvo"){
        show(signal_port_parameters@mvo_parameters)
      }

      # MMAF parameters
      if (object@sb_algorithm == "mmaf"){
        show(signal_port_parameters@mmaf_parameters)
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

    }
  }

  cat("------------------------------\n")
  if (!is.null(object@chosen_signals_and_positions)){
    cat("Chosen Signals and Positions:\n")
    print(object@chosen_signals_and_positions, quote = FALSE)
  } else {
    cat("Chosen Signals and Positions derived through SS Backtest \n")
  }

  cat("\n=================================\n")
})


#sb_metabacktest_config------------------------------------------------
#' Show Method for sb_metabacktest_config Class
#'
#' Displays detailed information about each configuration in the `sb_metabacktest_config` object.
#'
#' @param object An `sb_metabacktest_config` object.
#' @return Invisibly returns `NULL`. This function is called for its side effect of displaying information.
#' @export
setMethod("show", "sb_metabacktest_config",
          function(object) {

            cat(crayon::yellow("SB Metabacktest Configuration\n"))
            cat("Config Name: ", object@config_name, "\n")

            cat("------------------------------\n")
            cat(crayon::cyan("Meta Backtesting Scheme:"))
            cat("\n")
            cat(sprintf("  features_passthrough: %s\n", object@features_passthrough))
            cat(sprintf("  winsorize_base_predictions: %s\n", object@winsorize_base_predictions))
            cat(sprintf("  normalize_base_predictions: %s\n", object@normalize_base_predictions))
            cat("\n")

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
              cat("  Meta Learner Tuning Strategy:\n")
              cat(sprintf("    tuning_method: %s\n", config@tuning_strategy@tuning_method))
              cat(sprintf("    validation_sample_size: %s\n", config@tuning_strategy@validation_sample_size))
              cat(sprintf("    chosen_eval_metric: %s\n", config@tuning_strategy@chosen_eval_metric))
            } else {
              cat("  No Meta Learner Tuning Strategy available\n")
            }
            cat("\n")



            invisible(NULL)
          })

#sb_model------------------------------------------------
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
  cat("Best Hyperparameters: \n")
  if (length(object@best_hyperparameters) > 0) {
    print(object@best_hyperparameters %>% round(5))
  } else {
    cat("\nNo hyperparameters available.\n")
  }

  # Display the custom objective if it exists
  if (!is.null(object@custom_objective)) {
    cat("Custom Objective: ")
    cat(object@custom_objective)
  } else {
    cat("\nNo custom objective specified.\n")
  }

  # Display eligible signals
  if (!is.null(object@eligible_signals)) {
    cat("Eligible Signals: ")
    cat(object@eligible_signals)
  } else {
    cat("\nNo eligible signals specified.\n")
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


#sb_backtest_results------------------------------------------------
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
  sb_backtest_workflow <- object@sb_backtest_workflow[[length(object@sb_backtest_workflow)]]

  # Create a neat display of the sb_backtest_workflow
  cat("SB Backtest Workflow Metadata\n")
  cat("Backtest Identifier: ", object@backtest_identifier, "\n")
  cat("=================================\n")

  # Display Algorithm Information
  cat("Algorithm Information:\n")
  cat(" Config Name:", sb_backtest_workflow$config_name, "\n")
  cat("  SB Algorithm:", sb_backtest_workflow$sb_algorithm, "\n")
  cat("  Custom Objective:", sb_backtest_workflow$custom_objective, "\n")
  if(sb_backtest_workflow$custom_objective == "pseudo_huber_error") cat("  Custom Huber Delta:", sb_backtest_workflow$huber_delta, "\n")

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
    methods::show(create_keras_architecture(
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
  cat("  Split Method:", sb_backtest_workflow$split_method, "\n")

  if(sb_backtest_workflow$backtest_type == "meta_learner"){
    cat("-------------------------------\n")
    cat("  Base Learners Date Information:\n")
    cat("    Range of Dates Covered:", paste(c(as.Date(min(unlist(sb_backtest_workflow$dates_covered_bl))),
                                               as.Date(max(unlist(sb_backtest_workflow$dates_covered_bl))), sep ="-"), "\n"))
    cat("    Number of Dates:", sb_backtest_workflow$n_dates_bl, "\n")
    cat("-------------------------------\n")
  }

  cat("=================================\n")

  # Display Sample Sizes
  cat("Sample Sizes:\n")
  cat("  Training Sample Size:", sb_backtest_workflow$training_sample_size, "\n")
  cat("  Validation Sample Size:", sb_backtest_workflow$validation_sample_size, "\n")
  cat("  Testing Sample Size:", sb_backtest_workflow$testing_sample_size, "\n")
  cat("  Range of Dates in Testing Sample:", paste(
    if(length(sb_backtest_workflow$dates_testing_sample) == 1){
      as.Date(min(sb_backtest_workflow$dates_testing_sample))
    } else {
      c(min(sb_backtest_workflow$dates_testing_sample), max(sb_backtest_workflow$dates_testing_sample))
    }, sep ="-"), "\n")
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
  cat("\n")


  cat("=================================\n")

  # Display Features Information
  cat("Features Information:\n")
  cat("  Chosen Signals and Positions:\n")
  print(sb_backtest_workflow$chosen_signals_and_positions)
  cat("Features:", paste(sb_backtest_workflow$features, collapse = ", "), "\n")
  cat("\n")

  if (!is.null(object@final_feature_importance_m_d_ref)){
  cat("\nTop 5 most important features at final rebalancing:", paste(object@final_feature_importance_m_d_ref@data %>%
                                                                     dplyr::slice_max(order_by = normalized_importance, n = 5, with_ties = FALSE) %>% dplyr::pull(tickers),
                                                                   collapse = ", "), "\n")

  cat("\nBottom 5 least important features at final rebalancing:", paste(object@final_feature_importance_m_d_ref@data %>%
                                                                         dplyr::slice_min(order_by = normalized_importance, n = 5, with_ties = FALSE) %>% dplyr::pull(tickers),
                                                                       collapse = ", "), "\n")
  }

  cat("Features Object:", sb_backtest_workflow$features_object, "\n")


  cat("=================================\n")

  # Display Tuning Information
  if(!sb_backtest_workflow$sb_algorithm %in% c("ols", "ew", "sw", "rp", "hrp", "mvo", "mmaf", "custom_weights")){
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
  cat("Performance Information:\n")
  cat("  Completion Time:", sb_backtest_workflow$completion_time, "\n")
  cat("  Elapsed Time:", sb_backtest_workflow$elapsed_time, "seconds\n")
  cat("  Parallel Processing:", sb_backtest_workflow$parallel, "\n")
  cat("=================================\n")

  cat("  Call Timestamp:\n")
  print(sb_backtest_workflow$timestamps, quotes = FALSE)
  cat("\n")

  cat("=========================================\n")
})


#sb_metabacktest_results------------------------------------------------
#' @title Show Method for sb_metabacktest_results Class
#' @description Displays the contents of an `sb_metabacktest_results` object,
#' including consolidated and time series evaluation metrics.
#'
#' @param object An object of class `sb_metabacktest_results`.
#' @export
setMethod("show", "sb_metabacktest_results", function(object) {

  # Extract the meta_learner_sb_backtest_workflow from the meta learner object
  meta_learner_sb_backtest_workflow <- object@meta_sb_backtest_results@sb_backtest_workflow[[length(object@meta_sb_backtest_results@sb_backtest_workflow)]]

  # Create a neat display of the meta_learner_sb_backtest_workflow
  cat(crayon::cyan("Meta Learner Workflow Metadata\n"))
  cat("Backtest Identifier: ", object@backtest_identifier, "\n")
  cat("=================================\n")

  # Display Algorithm Information
  cat("Algorithm Information:\n")
  cat(" Config Name:", meta_learner_sb_backtest_workflow$config_name, "\n")
  cat("  SB Algorithm:", meta_learner_sb_backtest_workflow$sb_algorithm, "\n")
  cat("  Custom Objective:", meta_learner_sb_backtest_workflow$custom_objective, "\n")
  if(meta_learner_sb_backtest_workflow$backtest_type == "meta_learner"){
    cat("    Base-Learner Config Names:", meta_learner_sb_backtest_workflow$config_name_bl, "\n")
    cat("    Base-Learner Algorithms:", meta_learner_sb_backtest_workflow$sb_algorithm_bl, "\n")
  }
  if(meta_learner_sb_backtest_workflow$sb_algorithm == "nn"){
    # Display Keras Information
    methods::show(create_keras_architecture(
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
  cat("  Split Method:", meta_learner_sb_backtest_workflow$split_method, "\n")

  if(meta_learner_sb_backtest_workflow$backtest_type == "meta_learner"){
    cat("-------------------------------\n")
    cat("  Base Learners Date Information:\n")
    cat("    Range of Dates Covered:", paste(c(as.Date(min(unlist(meta_learner_sb_backtest_workflow$dates_covered_bl))),
                                               as.Date(max(unlist(meta_learner_sb_backtest_workflow$dates_covered_bl)))), sep ="-"), "\n")
    cat("    Number of Dates:", meta_learner_sb_backtest_workflow$n_dates_bl, "\n")
    cat("-------------------------------\n")
  }

  cat("=================================\n")

  # Display Sample Sizes
  cat("Sample Sizes:\n")

  cat("  Training Sample Size:", meta_learner_sb_backtest_workflow$training_sample_size, "\n")
  if(!meta_learner_sb_backtest_workflow$sb_algorithm %in% c("ols", "ew", "sw", "rp", "mvo", "custom_weights")){
    cat("  Validation Sample Size:", meta_learner_sb_backtest_workflow$validation_sample_size, "\n")
  }
  cat("  Testing Sample Size:", meta_learner_sb_backtest_workflow$testing_sample_size, "\n")
  cat("  Range of Dates in Testing Sample:", paste(
    if(length(meta_learner_sb_backtest_workflow$dates_testing_sample) == 1){
      as.Date(min(meta_learner_sb_backtest_workflow$dates_testing_sample))
    } else {
      c(min(meta_learner_sb_backtest_workflow$dates_testing_sample), max(meta_learner_sb_backtest_workflow$dates_testing_sample))
    }, sep ="-"), "\n")
  cat("\n")

  if(meta_learner_sb_backtest_workflow$backtest_type == "meta_learner"){
    cat("-------------------------------\n")
    cat("  Base Learners Sample Sizes:\n")
    cat("    Training Sample Size:", meta_learner_sb_backtest_workflow$training_sample_size_bl, "\n")
    cat("    Validation Sample Size:", meta_learner_sb_backtest_workflow$validation_sample_size_bl, "\n")
    cat("    Testing Sample Size:", meta_learner_sb_backtest_workflow$testing_sample_size_bl, "\n")
    cat("    Range of Dates in Testing Sample:", paste(c(as.Date(min(unlist(meta_learner_sb_backtest_workflow$dates_testing_sample_bl))),
                                                         as.Date(max(unlist(meta_learner_sb_backtest_workflow$dates_testing_sample_bl)))), sep="-"), "\n")
    cat("    Rebalance Dates:", paste(as.Date(unlist(meta_learner_sb_backtest_workflow$rebalance_dates_bl)), collapse = ", "), "\n")

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


  cat("=================================\n")

  # Display Features Information
  cat("Features Information:\n")
  cat("Chosen Signals and Positions: \n")
  print(meta_learner_sb_backtest_workflow$chosen_signals_and_positions)
  cat("\n")
  cat("Features:", paste(meta_learner_sb_backtest_workflow$features, collapse = ", "), "\n")
  cat("Features Object:", meta_learner_sb_backtest_workflow$features_object_name, "\n\n")

  cat("Top 5 most important features at final rebalancing:", paste(object@meta_sb_backtest_results@final_feature_importance_m_d_ref@data %>%
                                                                     dplyr::slice_max(order_by = normalized_importance, n = 5, with_ties = FALSE) %>% dplyr::pull(tickers),
                                                                   collapse = ", "), "\n\n")

  cat("Bottom 5 least important features at final rebalancing:", paste(object@meta_sb_backtest_results@final_feature_importance_m_d_ref@data %>%
                                                                         dplyr::slice_min(order_by = normalized_importance, n = 5, with_ties = FALSE) %>% dplyr::pull(tickers),
                                                                       collapse = ", "), "\n\n")


  cat("=================================\n")

  # Display Tuning Information
  if(!meta_learner_sb_backtest_workflow$sb_algorithm %in% c("ols", "ew", "sw", "rp", "mvo", "custom_weights")){
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
  cat("Performance Information:\n")
  cat("  Completion Time:", meta_learner_sb_backtest_workflow$completion_time, "\n")
  cat("  Elapsed Time:", meta_learner_sb_backtest_workflow$elapsed_time, "seconds\n")
  cat("  Parallel Processing:", meta_learner_sb_backtest_workflow$parallel, "\n")
  cat("=================================\n")


  # Display Call Information
  cat("  Call Timestamp:\n")
  print(meta_learner_sb_backtest_workflow$timestamps, quotes = FALSE)
  cat("\n")

  cat("=========================================\n")



})

#ss_backtest_config------------------------------------------------
#' @title Show Signal Selection Backtest Config
#' @description Prints the contents of an `ss_backtest_config` object: config name; backtest parameters
#' (initial sample size, rebalancing months, active returns, split method, chosen signals and positions); and,
#' if set, the `alpha_test_strategy` (delegated to its own `show` method).
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
  cat("Initial Sample Size:", object@initial_sample_size, "\n")
  cat("Rebalancing Months:", object@rebalancing_months, "\n")
  cat("Active Returns:", object@active_returns, "\n")
  cat("Split Method:", object@split_method, "\n")
  cat("Chosen Signals and Positions:\n")
  print(object@chosen_signals_and_positions, quote = FALSE)



  # Display Alpha Test Strategy
  cat("------------------------------\n")

  if (!is.null(object@alpha_test_strategy)) {
    methods::show(object@alpha_test_strategy)
  } else {
    cat("  No Alpha Test Strategy set.\n")
  }

  cat("\n==============================\n")
})

#alpha_test_strategy------------------------------------------------
#' @title Show Alpha Test Strategy
#' @description Prints the contents of an `alpha_test_strategy` object: main information (signal significance
#' threshold, p-value correction method, market factor proxy, theme representativeness); model structure
#' (model structure, and theme-level intercept/slope if set); and `lmer_control` parameters, if set.
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
#' @description Prints the contents of a `frequentist_alpha_test_strategy` object: a header, followed by the
#' inherited `alpha_test_strategy` display (main information, model structure, and `lmer_control`).
#' @param object A `frequentist_alpha_test_strategy` object to be displayed.
#' @method show frequentist_alpha_test_strategy
#' @export
setMethod("show", "frequentist_alpha_test_strategy", function(object) {
  cat("==============================\n")
  cat("Frequentist Alpha Test Strategy Configuration\n")
  cat("==============================\n")

  # Call the parent class show method to display common information
  methods::callNextMethod()

})


#' @title Show Bayesian Alpha Test Strategy
#' @description Prints the contents of a `bayesian_alpha_test_strategy` object: a header, followed by the
#' inherited `alpha_test_strategy` display, plus a `bayesian_model_parameters` section (delegated to its own
#' `show` method), or a "No Bayesian Model Parameters set" message if `NULL`.
#' @param object A `bayesian_alpha_test_strategy` object to be displayed.
#' @method show bayesian_alpha_test_strategy
#' @export
setMethod("show", "bayesian_alpha_test_strategy", function(object) {
  cat("==============================\n")
  cat("Bayesian Alpha Test Strategy Configuration\n")
  cat("==============================\n")

  # Call the parent class show method to display common information
  methods::callNextMethod()

  # Display Bayesian Model Parameters
  cat("\n------------------------------\n")
  cat("Bayesian Model Parameters:\n")
  cat("------------------------------\n")

  bayesian_params <- object@bayesian_model_parameters
  if (!is.null(bayesian_params)) {
    methods::show(bayesian_params)
  } else {
    cat("No Bayesian Model Parameters set.\n")
  }

})

#' @title Show Bayesian Model Parameters
#' @description Prints the contents of a `bayesian_model_parameters` object: prior derivation control
#' (e.g. `half_t_df`), `brms` control parameters (e.g. `chains`, `iter`, `warmup`, `thin`, `seed`, `adapt_delta`),
#' and user priors (if set) — each section shown, or reported as not set, individually.
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

#ss_backtest_results------------------------------------------------
#' Show Method for ss_backtest_results Class
#'
#' This method displays a detailed summary of the most recent batch in the `ss_backtest_results` object's
#' `ss_backtest_workflow`, including: backtest configuration (config name, alpha test strategy parameters,
#' lmer control), p-value correction method (with Bayesian model parameters, if applicable), date/rebalancing
#' information, signals information, signal themes and priors information (if applicable), winsorization
#' parameters, and execution performance (elapsed time and timestamps).
#'
#' @param object An instance of the `ss_backtest_results` class.
#'
#' @return The method returns the object invisibly.
#'
#' @export
setMethod("show", "ss_backtest_results", function(object) {

  # Extract the most recent ss_backtest_workflow
  ss_backtest_workflow <- object@ss_backtest_workflow[[length(object@ss_backtest_workflow)]]

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
  if (!is.null(ss_backtest_workflow$signals_object)) {
    cat("  Signals Object Name: ", ss_backtest_workflow$signals_object, "\n")
  }
  cat("=========================================\n")

  # Display Signal Themes Information
  if (!is.null(ss_backtest_workflow$signal_themes_workflow)) {
    cat("Signal Themes Information:\n")
    cat("  Signal Themes Object Name: ", ss_backtest_workflow$signal_themes_object, "\n")
    cat("=========================================\n")
  }

  # Display Priors Information
  if (ss_backtest_workflow$p_correction_method == "bayesian") {
    cat("Priors Information:\n")
    if (!is.null(ss_backtest_workflow$priors_workflow)) {
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


})


#port_backtest_config--------------------------

#' @title Show Port Backtest Config
#' @description Prints the contents of a `port_backtest_config` object, detailing its various
#' parameters and configurations for stock-level portfolio backtesting.
#'
#' @param object A `port_backtest_config` object to be displayed.
#'
#' @method show port_backtest_config
#' @export
setMethod("show", "port_backtest_config", function(object) {
  cat("==============================\n")
  cat("Port Backtest Configuration\n\n")

  # Main Information
  cat("------------------------------\n")
  cat("Main Information:\n")
  cat("------------------------------\n")
  cat("Config Name: ", object@config_name, "\n")
  cat("Portfolio Construction Method: ", object@port_construction_method, "\n")
  cat("Initial Buffer Period: ", object@initial_buffer_period, "\n")
  cat("Rebalancing Months: ", paste(object@rebalancing_months, collapse = " "), "\n")
  cat("Selected Benchmark: ", object@selected_benchmark, "\n")
  cat("Main Liquidity Metric: ", object@main_liquidity_metric, "\n\n")

  # Expected Return Score
  if (!is.null(object@chosen_score_metric_and_position)){
    cat("------------------------------\n")
    cat("Expected Return Score:\n")
    cat("------------------------------\n")

    cat("Chosen Score Metric: ", names(object@chosen_score_metric_and_position), "\n")
    cat("Chosen Score Position: ", object@chosen_score_metric_and_position, "\n")
  }
  if (is.null(object@chosen_score_metric_and_position) && object@port_construction_method != "custom_weights"){
    cat("------------------------------\n")
    cat("Expected Return Score:\n")
    cat("------------------------------\n")
    cat("Derived through SB OOS Predictions\n")
  }
  cat("Eligibility Quantile Range: ", paste(object@eligibility_quantile_range, collapse = " - "), "\n")
  cat("Min Eligible Assets Fallback: ")
  if(is.null(object@min_eligible_assets_fallback)) cat("Not available.\n") else cat(object@min_eligible_assets_fallback, "\n")

  # Scaler
  if (!is.null(object@chosen_scaler)){
    cat("Chosen Scaler: ", object@chosen_scaler, "\n")
    cat("Scaler Shrinkage: ", object@scaler_shrinkage, "\n")
    cat("Use raw score for eligibility: ", object@use_raw_for_eligibility, "\n")
  } else {
    cat("Scaler not applied.\n")
  }

  # Covariance Estimation
  cat("------------------------------\n")
  methods::show(object@cov_est_method)
  cat("\n")

  # Portfolio-specific parameters
  if(object@port_construction_method == "mvo"){
    cat("------------------------------\n")
    methods::show(object@mvo_parameters)
    cat("\n")
  }
  if(object@port_construction_method == "rp"){
    cat("------------------------------\n")
    methods::show(object@rp_parameters)
    cat("\n")
  }
  if(object@port_construction_method == "hrp"){
    cat("------------------------------\n")
    methods::show(object@hrp_parameters)
    cat("\n")
  }
  if(object@port_construction_method == "mmaf"){
    cat("------------------------------\n")
    methods::show(object@mmaf_parameters)
    cat("\n")
  }

  # Constraint Policies
  if(!is.null(object@liquidity_constraint_policy)){
    cat("------------------------------\n")
    methods::show(object@liquidity_constraint_policy)
    cat("\n")
  }
  if(!is.null(object@turnover_constraint_policy)){
    cat("------------------------------\n")
    methods::show(object@turnover_constraint_policy)
    cat("\n")
  }
  if(!is.null(object@concentration_constraint_policy)){
    cat("------------------------------\n")
    methods::show(object@concentration_constraint_policy)
    cat("\n")
  }

  # Transaction Costs
  if(!is.null(object@transaction_costs_parameters)){
    cat("------------------------------\n")
    methods::show(object@transaction_costs_parameters)
    cat("\n")
  }

  # Liquidity Floor Cutoffs
  if(!is.null(object@liquidity_floor_cutoffs)){
    cat("------------------------------\n")
    methods::show(object@liquidity_floor_cutoffs)
    cat("\n")
  }

  cat("=================================\n")
})


#port----------------------------------------
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
#' @export
setMethod(
  f = "show",
  signature = "port",
  definition = function(object) {

    # 1) Class Identification
    # Check if object is one of the subclasses
    subclass <- if (methods::is(object, "signal_port")) {
      "signal_port"
    } else if (methods::is(object, "stock_port")) {
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
    if(subclass == "signal_port" && !is.null(object@heuristic_sb_metric)){
      cat("Heuristic SB Metric:  ", object@heuristic_sb_metric, "\n")
    }
    cat("Eligible Assets:      ", paste(object@eligible_assets, collapse = ", "), "\n")
    cat("Number of Assets:     ", length(object@eligible_assets), "\n")
    cat("---------------------------------\n")

    #### 2) Classic ex-ante metrics
      ##### First choose group col
      chosen_group_col <- object@group_col

      ##### Now choose bench_weights col
      weights_kind_default  <- "regular"
      bench_col_default     <- NULL
      if ("universe_m_d_ref" %in% methods::slotNames(object) &&
          !is.null(object@universe_m_d_ref) &&
          !is.null(object@universe_m_d_ref@data)) {
        universe_m_d_ref <- object@universe_m_d_ref@data
        if (is.data.frame(universe_m_d_ref)) {
          bench_cols <- grep("_bench_weights$", names(universe_m_d_ref), value = TRUE)
          if (length(bench_cols) >= 1L) {
            weights_kind_default <- "active"
            bench_col_default    <- bench_cols[1L]  # if multiple, use the first
          }
        }
      }

      ##### Port Stats
      port_stats  <- object@port_stats
      active_mode <- any(stringr::str_detect(names(port_stats), "act"))

        if (!active_mode) {
          cat("\nRisk x Return:\n")
          cat(" Port Expected Return: ", round(as.numeric(port_stats$exp_ret), 3), "\n")
          cat(" Port Expected Risk:   ", round(as.numeric(port_stats$risk), 3), "\n")
          cat(" Port Expected Sharpe: ", round(as.numeric(port_stats$sharpe), 3), "\n")

          cat("\nConcentration & Breadth:\n")
          cat("  HHI (weights):             ", round(as.numeric(port_stats$hhi_weights), 4), "\n")
          cat("  Effective N (1/HHI):       ", round(as.numeric(port_stats$n_eff_weights), 2), "\n")
          cat("  Entropy Eff. N (exp H):    ", round(as.numeric(port_stats$entropy_effective_n), 2), "\n")
          cat("  Gini (weights):            ", round(as.numeric(port_stats$gini_weights), 3), "\n")
          cat("  Top-5 weight:              ", round(as.numeric(port_stats$top_5_concentration), 3), "\n")
          cat("  Top-10 weight:             ", round(as.numeric(port_stats$top_10_concentration), 3), "\n")
          cat("  Top-25 weight:             ", round(as.numeric(port_stats$top_25_concentration), 3), "\n")
          if (!is.na(port_stats$diversification_ratio)) {
            cat("  Diversification Ratio:      ", round(as.numeric(port_stats$diversification_ratio), 3), "\n")
          }
          if (!is.na(port_stats$wavg_pairwise_corr)) {
            cat("  Wtd Avg Pairwise Corr:      ", round(as.numeric(port_stats$wavg_pairwise_corr), 3), "\n")
          }
          ##### Risk-contribution metrics
          if (!is.na(port_stats$hhi_rrc) || !is.na(port_stats$n_eff_rrc) || !is.na(port_stats$rrc_dist_to_erc)) {
            cat("  HHI (risk contrib):         ", round(as.numeric(port_stats$hhi_rrc), 4), "\n")
            cat("  Eff. N (risk contrib):      ", round(as.numeric(port_stats$n_eff_rrc), 2), "\n")
            cat("  RRC distance to ERC (L2):   ", round(as.numeric(port_stats$rrc_dist_to_erc), 4), "\n")
          }

        } else {
          cat("\nRisk x Return:\n")
          cat(" Port Expected Act. Return: ", round(as.numeric(port_stats$act_exp_ret), 3), "\n")
          cat(" Port Expected TE:   ", round(as.numeric(port_stats$act_risk), 3), "\n")
          cat(" Port Expected IR: ", round(as.numeric(port_stats$info_ratio), 3), "\n")

          cat("\nConcentration & Breadth:\n")
          cat("  HHI (act. weights):        ", round(as.numeric(port_stats$act_hhi_weights), 4), "\n")
          cat("  Effective N (act. 1/HHI):  ", round(as.numeric(port_stats$act_n_eff_weights), 2), "\n")
          cat("  Entropy Eff. N (act. exp H):", round(as.numeric(port_stats$act_entropy_effective_n), 2), "\n")
          cat("  Gini (act. weights):       ", round(as.numeric(port_stats$act_gini_weights), 3), "\n")
          cat("  Top-5 act. weight:         ", round(as.numeric(port_stats$act_top_5_concentration), 3), "\n")
          cat("  Top-10 act. weight:        ", round(as.numeric(port_stats$act_top_10_concentration), 3), "\n")
          cat("  Top-25 act. weight:        ", round(as.numeric(port_stats$act_top_25_concentration), 3), "\n")
          if (!is.na(port_stats$act_diversification_ratio)) {
            cat("  Diversification Ratio (act.):", round(as.numeric(port_stats$act_diversification_ratio), 3), "\n")
          }
          if (!is.na(port_stats$act_wavg_pairwise_corr)) {
            cat("  Wtd Avg Pairwise Corr (act.):", round(as.numeric(port_stats$act_wavg_pairwise_corr), 3), "\n")
          }
          ##### Risk-contribution metrics
          if (!is.na(port_stats$act_hhi_rrc) || !is.na(port_stats$act_n_eff_rrc) || !is.na(port_stats$act_rrc_dist_to_erc)) {
            cat("  HHI (risk contrib):         ", round(as.numeric(port_stats$act_hhi_rrc), 4), "\n")
            cat("  Eff. N (risk contrib):      ", round(as.numeric(port_stats$act_n_eff_rrc), 2), "\n")
            cat("  RRC distance to ERC (L2):   ", round(as.numeric(port_stats$act_rrc_dist_to_erc), 4), "\n")
          }
        }


    # 3) Weights and Return Scores
    cat("---------------------------------\n")
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
      cat("\nGroups Provided (First 25):\n")
      print(utils::head(dplyr::select(object@groups, -id, -dates), 25))

      # Group/sector concentration
        #### Group metrics
        if (!is.null(chosen_group_col)) {
          cat("\nGroup Concentration (", chosen_group_col, "):\n", sep = "")
          if (!active_mode){
            cat("  HHI (groups):               ", round(as.numeric(port_stats$group_hhi_weights), 4), "\n")
            cat("  Effective N (groups):       ", round(as.numeric(port_stats$group_entropy_effective_n), 2), "\n")
            cat("  Top 3 Group Weight:         ", round(as.numeric(port_stats$group_top_3_concentration), 3), "\n")
            cat("  Number of Groups:           ", as.integer(port_stats$n_groups), "\n")
          } else {
            cat("  HHI (act. groups):          ", round(as.numeric(port_stats$act_group_hhi_weights), 4), "\n")
            cat("  Effective N (act. groups):  ", round(as.numeric(port_stats$act_group_entropy_effective_n), 2), "\n")
            cat("  Top Group Weight (act.):    ", round(as.numeric(port_stats$act_group_top_3_concentration), 3), "\n")
            cat("  Number of Groups:           ", as.integer(port_stats$n_groups), "\n")
          }

          if (!is.null(object@group_cov_matrix)) {
            cat("Group Correlation Matrix (first few rows shown):\n")
            print(utils::head(round(stats::cov2cor(object@group_cov_matrix), 2), n = 10))
          } else {
            cat("No group covariance matrix provided.\n")
          }
        } else if (is.data.frame(object@groups)) {
          cat("\nGroup Concentration:   Multiple or no group columns detected; run summary(object, \"Group Concentration by Column\") for a full table.\n")
        }
    } else {
      cat("\nNo groups specified.\n")
    }



    # Wrap up
    cat("\n=================================\n")

    # Return invisibly
    invisible(object)
  }
)

#cov_est_method--------------------------
#' @title Show Covariance Estimation Method
#' @description Displays the configuration of a `cov_est_method` object, including the estimation method,
#' the sample size used for the covariance matrix, whether active returns are used, and the benchmark used.
#' @param object A `cov_est_method` object.
#' @method show cov_est_method
#' @export
setMethod("show", "cov_est_method", function(object) {
  cat("\nCovariance Estimation Method Configuration:\n")
  cat("--------------------------------------------\n")
  cat("Covariance Estimation Method: ", object@cov_estimation_method, "\n")
  cat("Covariance Matrix Sample Size: ", object@cov_matrix_sample_size, "\n")
  cat("Active Returns: ", object@active_returns, "\n")
  cat("Covariance Matrix Benchmark: ", object@cov_matrix_benchmark, "\n")
})

#mvo_parameters--------------------------
#' @title Show MVO Parameters
#' @description Displays the mean-variance optimization parameters contained in a `mvo_parameters` object.
#' @param object A `mvo_parameters` object.
#' @method show mvo_parameters
#' @export
methods::setMethod("show", "mvo_parameters", function(object) {
  .print_mvo_parameters(object, hide_title = FALSE)
})

.print_mvo_parameters <- function(object, hide_title = FALSE) {
  if (!hide_title){
    cat("\nMean-Variance Optimization Parameters:\n")
  }
  cat(" Optimization Method: ", object@opt_method, "\n")
  cat(" Random Ports Method: ", object@random_ports_method, "\n")
  cat(" Number of Random Ports: ", object@n_random_ports, "\n")
  cat(" Optimization Objective: ", object@opt_objective, "\n")
  cat(" Ridge Penalty: ", ifelse(is.null(object@ridge_pen), "None", object@ridge_pen), "\n")
  cat(" Number of resamples: ", object@n_resamples, "\n")
  if (object@n_resamples > 0) {
    cat(" Exp Return Score Jitter: ", object@exp_ret_score_jitter, "\n")
    cat(" Covariance Eigenvalues Jitter: ", object@cov_eigval_jitter, "\n")
  }
}


#rp_parameters--------------------------
#' @title Show Risk-Parity Parameters
#' @description Displays the risk-parity configuration contained in a `rp_parameters` object.
#' @param object A `rp_parameters` object.
#' @method show rp_parameters
#' @export
methods::setMethod("show", "rp_parameters", function(object) {
  .print_rp_parameters(object, hide_title = FALSE)
})

.print_rp_parameters <- function(object, hide_title = FALSE) {
  if (!hide_title){
    cat("\nRisk-Parity Parameters:\n")
  }
  cat(" Risk-Parity Method: ", object@rp_method, "\n")
  cat(" Expected Return Tilt: ", object@exp_ret_score_tilt, "\n")
  cat(" Tilt Eta: ", ifelse(is.null(object@exp_ret_score_tilt_eta), "None", object@exp_ret_score_tilt_eta), "\n")
}

#hrp_parameters-------------------------------------------
#' @title Show HRP Parameters
#' @description Displays the hierarchical risk parity configuration contained in a `hrp_parameters` object.
#' @param object A `hrp_parameters` object.
#' @method show hrp_parameters
#' @export
methods::setMethod("show", "hrp_parameters", function(object) {
  .print_hrp_parameters(object, hide_title = FALSE)
})

.print_hrp_parameters <- function(object, hide_title = FALSE) {
  if (!hide_title){
    cat("\nHierarchical Risk-Parity Parameters:\n")
  }
  cat(" Linkage Method: ", object@linkage, "\n")
  cat(" Expected Return Tilt: ", object@exp_ret_score_tilt, "\n")
  cat(" Tilt Eta: ", ifelse(is.null(object@exp_ret_score_tilt_eta), "None", object@exp_ret_score_tilt_eta), "\n")
}


#mmaf_parameters-------------------------------------------
#' @title Show MMAF Parameters
#' @description Displays the Micro-Macro Allocation Framework configuration contained in a `mmaf_parameters` object.
#' @param object A `mmaf_parameters` object.
#' @method show mmaf_parameters
#' @export
setMethod("show", "mmaf_parameters", function(object) {
  cat("\nMicro-Macro Allocation Framework (MMAF) Parameters:\n")
  cat("MMAF Method: ", object@mmaf_method, "\n")
  cat("Group Column: ", object@mmaf_group_col, "\n")

  if (object@mmaf_method == "top_down") {
    cat("Top-Down Proxy Portfolio Method: ", object@top_down_proxy_port_method, "\n")
  } else {
    cat("Top-Down Proxy Portfolio Method: None (Bottom-Up MMAF)\n")
  }

  cat("\nMicro Portfolio Configuration:\n")
  cat(" Construction Method: ", object@micro_port_config@port_construction_method, "\n")
  if (!is.null(object@micro_port_config@mvo_parameters)){
    cat(" MVO Parameters:\n")
    .print_mvo_parameters(object@micro_port_config@mvo_parameters, hide_title = TRUE)
  }
  if (!is.null(object@micro_port_config@rp_parameters)){
    cat(" RP Parameters:\n")
    .print_rp_parameters(object@micro_port_config@rp_parameters, hide_title = TRUE)
  }
  if (!is.null(object@micro_port_config@hrp_parameters)){
    cat(" HRP Parameters:\n")
    .print_hrp_parameters(object@micro_port_config@hrp_parameters, hide_title = TRUE)
  }
  cat("\nMacro Portfolio Configuration:\n")
  cat(" Construction Method: ", object@macro_port_config@port_construction_method, "\n")
  if (!is.null(object@macro_port_config@mvo_parameters)){
    cat(" MVO Parameters:\n")
    .print_mvo_parameters(object@macro_port_config@mvo_parameters, hide_title = TRUE)
  }
  if (!is.null(object@macro_port_config@rp_parameters)){
    cat(" RP Parameters:\n")
    .print_rp_parameters(object@macro_port_config@rp_parameters, hide_title = TRUE)
  }
  if (!is.null(object@macro_port_config@hrp_parameters)){
    cat(" HRP Parameters:\n")
    .print_hrp_parameters(object@macro_port_config@hrp_parameters, hide_title = TRUE)
  }
})


#concentration_constraint_policy--------------------------
#' @title Show Concentration Constraint Policy
#' @description Prints the contents of a `concentration_constraint_policy` object,
#' detailing the benchmark, the maximum absolute active weight for individual assets,
#' and the maximum absolute active group weights.
#' @param object A `concentration_constraint_policy` object.
#' @method show concentration_constraint_policy
#' @export
setMethod("show", "concentration_constraint_policy", function(object) {
  cat("\nConcentration Constraint Policy:\n")
  cat("------------------------------\n")
  cat("Benchmark: ", object@benchmark, "\n")
  cat("Max Abs Active Individual Weight: ", object@max_abs_active_individual_weight, "\n")

  cat("Max Abs Active Group Weight:\n")
  if (!is.null(object@max_abs_active_group_weight)) {
    if (!is.null(names(object@max_abs_active_group_weight))) {
      for (grp in names(object@max_abs_active_group_weight)) {
        cat("   ", grp, ": ", object@max_abs_active_group_weight[[grp]], "\n")
      }
    } else {
      cat("   ", object@max_abs_active_group_weight, "\n")
    }
  } else {
    cat("   Not set.\n")
  }
})

#liquidity_constraint_policy--------------------------
#' @title Show Liquidity Constraint Policy
#' @description Prints the contents of a `liquidity_constraint_policy` object,
#' including the liquidity floor rule and liquidity cap rules.
#' @param object A `liquidity_constraint_policy` object.
#' @method show liquidity_constraint_policy
#' @export
setMethod("show", "liquidity_constraint_policy", function(object) {
  cat("\nLiquidity Constraint Policy:\n")
  cat("------------------------------\n")
  cat("Liquidity Floor Rule: ", object@liquidity_floor_rule, "\n")

  cat("Liquidity Cap Rules:\n")
  if (!is.null(object@liquidity_cap_rules)) {
    if (!is.null(names(object@liquidity_cap_rules))) {
      for (rule in names(object@liquidity_cap_rules)) {
        cat("   ", rule, ": ", object@liquidity_cap_rules[[rule]], "\n")
      }
    } else {
      cat("   ", object@liquidity_cap_rules, "\n")
    }
  } else {
    cat("   Not set.\n")
  }
})

#turnover_constraint_policy--------------------------
#' @title Show Turnover Constraint Policy
#' @description Prints the contents of a `turnover_constraint_policy` object,
#' including the quantile range buffer and the turnover cap rules.
#' @param object A `turnover_constraint_policy` object.
#' @method show turnover_constraint_policy
#' @export
setMethod("show", "turnover_constraint_policy", function(object) {
  cat("\nTurnover Constraint Policy:\n")
  cat("------------------------------\n")
  cat("Quantile Range Buffer: ", object@quantile_range_buffer, "\n")

  cat("Turnover Cap Rules:\n")
  if (!is.null(object@turnover_cap_rules)) {
    if (!is.null(names(object@turnover_cap_rules))) {
      for (rule in names(object@turnover_cap_rules)) {
        cat("   ", rule, ": ", object@turnover_cap_rules[[rule]], "\n")
      }
    } else {
      cat("   ", object@turnover_cap_rules, "\n")
    }
  } else {
    cat("   Not set.\n")
  }
})

#transaction_costs_parameters--------------------------
#' @title Show Transaction Cost Parameters
#' @description Prints the contents of a `transaction_costs_parameters` object,
#' including direct transaction cost, strategy AUM, alpha, and lambda.
#' @param object A `transaction_costs_parameters` object.
#' @method show transaction_costs_parameters
#' @export
setMethod("show", "transaction_costs_parameters", function(object) {
  cat("\nTransaction Cost Parameters:\n")
  cat("------------------------------\n")
  cat("Direct Transaction Cost: ", object@direct_transaction_cost, "\n")
  cat("Strategy AUM: ", object@strategy_aum, "\n")
  cat("Alpha: ", object@alpha, "\n")
  cat("Lambda: ", object@lambda, "\n")
})

#port_backtest_results--------------------------
#' @title Show Port Backtest Results
#' @description Displays a detailed summary of the `port_backtest_results` object, including the backtest
#' identifier, configuration (config name, construction method, chosen score/position, selected benchmark),
#' date information, stock-universe size, performance information (portfolio-return means, plus custom
#' portfolio-metric means when a `port_metrics_m_xts` is available), and the final stock portfolio.
#'
#' @param object An instance of the `port_backtest_results` class.
#'
#' @return The object is returned invisibly.
#'
#' @export
setMethod("show", "port_backtest_results", function(object) {
  workflow <- object@port_backtest_workflow[[length(object@port_backtest_workflow)]]

  cat("==============================\n")
  cat("Portfolio Backtest Results\n")
  cat("Backtest Identifier: ", object@backtest_identifier, "\n")
  cat("==============================\n\n")

  # Display configuration information
  cat("Configuration:\n")
  cat("  Config Name: ", workflow$config_name, "\n")
  cat("  Portfolio Construction Method: ", object@port_construction_method, "\n")
  cat("  Chosen Score & Position: ", workflow$chosen_score_metric_and_position, "\n")
  cat("  Selected Benchmark: ", ifelse(is.null(workflow$selected_benchmark), "None", workflow$selected_benchmark), "\n\n")

  # Display date information
  cat("Date Information:\n")
  cat("  Dates Covered: ", paste(range(workflow$dates_covered), collapse = " - "), "\n")
  cat("  Number of Dates: ", workflow$n_dates, "\n")
  cat("  First Rebalance Date: ", paste(as.Date(workflow$first_rebalance_date), "\n"))
  cat("  Rebalance Dates: ", paste(workflow$rebalance_dates, collapse = ", "), "\n")
  cat("  Last Rebalance Date: ", paste(as.Date(workflow$last_rebalance_date), "\n\n"))

  # Display sample/stock universe information
  cat("Stock Universe:\n")
  cat("  Number of Stocks: ", workflow$n_stocks, "\n")
  cat("  Number of Observations: ", workflow$nobs, "\n\n")

  # Display performance information
  cat("Performance Information:\n")
  if (!is.null(object@port_metrics_m_xts)) {
    cat("  Portfolio Metrics Means: \n")
    print(object@port_metrics_m_xts@data %>% sapply(function(x) round(mean(x), 2)))
  } else {
    cat("  Portfolio Metrics: Not available\n")
  }
  cat("  Portfolio Returns Means: \n")
  print(object@port_returns_m_xts@data %>% sapply(function(x) round(mean(x), 2)))

  # Display final stock portfolio information
  cat("\nFinal Stock Portfolio:\n")
  methods::show(object@final_stock_port)
  cat("\n")


  invisible(object)
})

#port_backtest_cohort--------------------------
#' @title Show Method for port_backtest_cohort Class
#' @description Displays a detailed summary of a `port_backtest_cohort` object.
#' It focuses on the configuration settings contained in the common
#' port_backtest_workflow slot (e.g., selected_benchmark, port_construction_method,
#' object names, dates, and more).
#'
#' @param object An object of class `port_backtest_cohort`.
#' @export
setMethod("show", "port_backtest_cohort", function(object) {

  # Header
  cat(crayon::cyan("Portfolio Backtest Cohort Summary\n"))
  cat("Cohort Name:", object@cohort_name, "\n")
  cat("========================================\n")

  # Display Workflow Configuration
  cat("Cohort Common Information:\n")
  backtest_workflow_common <- object@backtest_workflow_common
  cat("  Selected Benchmark: ", backtest_workflow_common$selected_benchmark, "\n")
  cat("  Dates Covered: ", paste0(as.Date(min(backtest_workflow_common$dates_covered)), "-", as.Date(max(backtest_workflow_common$dates_covered)), "\n"))
  cat("  Backtested Dates: ", paste0(as.Date(min(backtest_workflow_common$dates_backtest)), "-", as.Date(max(backtest_workflow_common$dates_backtest)), "\n"))
  cat("  Initial Buffer Period: ", backtest_workflow_common$initial_buffer_period, "\n\n")

  # Display Objects
  cat("Objects Names:\n")
  cat("  Signals Object Name: ", backtest_workflow_common$signals_object_name, "\n")
  cat("  Fwd Returns Object Name: ", backtest_workflow_common$fwd_returns_object_name, "\n")
  cat("  Stock Groups Object Name: ", backtest_workflow_common$stock_groups_object_name, "\n")
  cat("  Benchmark Returns Object Name: ", backtest_workflow_common$benchmark_returns_object_name, "\n")
  cat("  Daily Assets Returns Object Name: ", backtest_workflow_common$daily_assets_returns_object_name, "\n")
  cat("  Daily Bench Returns Object Name: ", backtest_workflow_common$daily_bench_returns_object_name, "\n")
  cat("  Liquidity Object Name: ", backtest_workflow_common$liquidity_object_name, "\n")
  cat("  Volatility Object Name: ", backtest_workflow_common$volatility_object_name, "\n")
  cat("  Benchmark Weights Object Name: ", backtest_workflow_common$volatility_object_name, "\n")

  ## Portfolios Details
  cat(crayon::yellow("\nPortfolio Backtest Results details:\n"))
  cat("Number of Backtests: ", length(object@port_backtest_results_list ), "\n")

  # Define a color palette using crayon
  colors <- list(
    neon_cyan = crayon::cyan,
    neon_pink = crayon::magenta,
    neon_blue = crayon::blue,
    neon_purple = crayon::make_style("#8A2BE2"),
    neon_orange = crayon::red,
    neon_green = crayon::green,
    neon_yellow = crayon::yellow,
    neon_red = crayon::make_style("#FF4500"),
    neon_silver = crayon::make_style("#C0C0C0"),
    neon_gold = crayon::make_style("#FFD700"),
    neon_teal = crayon::make_style("#008080")
  )

  # Loop through Backtests
  for (i in seq_along(object@port_backtest_results_list)) {
    port_backtest <- object@port_backtest_results_list[[i]]
    port_backtest_workflow <- port_backtest@port_backtest_workflow[[length(port_backtest@port_backtest_workflow)]] # Get the last workflow
    port_backtest_config <- port_backtest@port_backtest_config

    # Use a color from the palette
    color_func <- colors[[ (i - 1) %% length(colors) + 1 ]]

    # Color the backtest configuration header
    cat("------------------------------\n")
    cat(color_func(sprintf("Port Backtest Results %d:\n", i)))
    cat(paste("Backtest Identifier:", port_backtest@backtest_identifier), "\n")
    cat(sprintf("  port_construction_method: %s\n", port_backtest_workflow$port_construction_method))
    if (!is.null(port_backtest_workflow$chosen_score_metric_and_position)){
      cat(sprintf("  chosen_score_metric_and_position: %s\n",
                  paste0(names(port_backtest_workflow$chosen_score_metric_and_position)," - ",port_backtest_workflow$chosen_score_metric_and_position))
      )
    } else {
      cat(sprintf("  oos_predictions_object_name: %s\n", port_backtest_workflow$oos_predictions_object_name))
   }
    cat(sprintf("  eligibility_quantile_range: %s\n", paste0(min(port_backtest_workflow$eligibility_quantile_range),"-",max(port_backtest_workflow$eligibility_quantile_range))))
    cat(sprintf("  min_eligible_assets_fallback: %s\n", port_backtest_workflow$min_eligible_assets_fallback))

    # Covariance Estimation

    methods::show(port_backtest_config@cov_est_method)
    cat("\n")

    # Portfolio-specific parameters
    if(port_backtest_config@port_construction_method == "mvo"){
      methods::show(port_backtest_config@mvo_parameters)
      cat("\n")
    }
    if(port_backtest_config@port_construction_method == "rp"){
      methods::show(port_backtest_config@rp_parameters)
      cat("\n")
    }
    if(port_backtest_config@port_construction_method == "hrp"){
      methods::show(port_backtest_config@hrp_parameters)
      cat("\n")
    }
    if(port_backtest_config@port_construction_method == "mmaf"){
      methods::show(port_backtest_config@mmaf_parameters)
      cat("\n")
    }

    # Constraint Policies
    if(!is.null(port_backtest_config@liquidity_constraint_policy)){
      methods::show(port_backtest_config@liquidity_constraint_policy)
      cat("\n")
    }
    if(!is.null(port_backtest_config@turnover_constraint_policy)){
      methods::show(port_backtest_config@turnover_constraint_policy)
      cat("\n")
    }
    if(!is.null(port_backtest_config@concentration_constraint_policy)){
      methods::show(port_backtest_config@concentration_constraint_policy)
      cat("\n")
    }

    cat("\n")


  }
})



