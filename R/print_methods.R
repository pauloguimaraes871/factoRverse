#' Show Method for meta_dataframe Class
#'
#' This method displays a summary of the `meta_dataframe` object, including
#' metadata information, number of signals, unique dates, unique tickers,
#' total observations, and the first few rows of the data.
#'
#' @param object An instance of the `meta_dataframe` class.
#'
#' @return The method returns the object invisibly.
#'
#' @export
setMethod("show", "meta_dataframe", function(object) {

  # Print a summary of the metadata
  cat("Metadata:\n")
  cat("=================================\n")
  cat("  Number of signals:", ncol(object@data)-3, "\n")
  cat("  Unique Dates:", object@unique_dates, "\n")
  cat("  Unique Tickers:", object@unique_tickers, "\n")
  cat("  Total Observations (n_obs):", object@n_obs, "\n")
  cat("  Workflow:\n")
  print(object@workflow)
  cat("  Signals:\n")
  print(object@signals)

  cat("=================================\n")

  # Print the first few rows of the data
  cat("\nFirst few rows of the data:\n")
  print(head(object@data))

  # Return the object invisibly
  invisible(object)
})

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
  cat("Chosen tuning_method:\n")
  cat("  ", object@tuning_method, "\n\n")

  cat("Chosen ml_algorithm:\n")
  cat("  ", object@ml_algorithm, "\n\n")

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
        if (object@tuning_method == "bayesian_opt") {
          cat("    Bounds:", paste(hyperparam, collapse = ", "), "\n")
        } else {
          cat("    Values:", paste(hyperparam, collapse = ", "), "\n")
        }
      }
    }
  }

  cat("=================================\n")
})


#' @title Print keras_architecture_parameters
#' @description Method to print an object of class `keras_architecture_parameters`.
#'
#' @param object An object of class `keras_architecture_parameters`.
#'
#' @export
setMethod("show", "keras_architecture_parameters", function(object) {
  cat("Keras Architecture Parameters:\n")
  cat("=================================\n")
  cat("Number of Layers:", object@n_layers, "\n")
  cat("Units per Layer:", paste(object@units, collapse = ", "), "\n")
  cat("Activation Functions:", paste(object@activation, collapse = ", "), "\n")
  cat("Optimizer:", object@nn_optimizer, "\n")
  cat("Batch Normalization Options:", paste(object@batch_norm_option, collapse = ", "), "\n")
  cat("=================================\n")
})


#' Show Method for refit_ml_model Class
#'
#' This method provides a summary of the `refit_ml_model` object, including
#' the machine learning algorithm used, best hyperparameters, custom objective,
#' Huber delta, Keras architecture parameters, and the model structure.
#'
#' @param object An instance of the `refit_ml_model` class.
#'
#' @return The method returns the object invisibly.
#'
#' @export
setMethod("show", "refit_ml_model", function(object) {
  cat("Refit ML Model Summary:\n")

  cat("=================================\n")

  # Display the algorithm used
  cat("  Model Algorithm: ", object@ml_algorithm, "\n")

  # Display the best hyperparameters if they exist
  cat("  Best Hyperparameters:\n")
  if (length(object@best_hyperparameters) > 0) {
    print(object@best_hyperparameters)
  } else {
    cat("    No hyperparameters available.\n")
  }

  # Display the custom objective if it exists
  if (!is.null(object@custom_objective)) {
    cat("  Custom Objective:\n")
    print(object@custom_objective)
  } else {
    cat("  No custom objective specified.\n")
  }

  # Display the Huber delta if it is set
  cat("  Huber Delta: ", object@huber_delta, "\n")

  # Display Keras architecture parameters if they exist
  if (!is.null(object@keras_architecture_parameters)) {
    cat("  Keras Architecture Parameters:\n")
    print(object@keras_architecture_parameters)
  } else {
    cat("  No Keras architecture parameters specified.\n")
  }

  cat("=================================\n")

  # Display model structure or summary if available
  cat("  Model Structure:\n")
  if (!is.null(object@model)) {
    print(object@model)
  } else {
    cat("  No model object available.\n")
  }

  # Indicate that the object is displayed
  invisible(object)
})


#' Show Method for ml_wf_val_results Class
#'
#' This method displays a detailed summary of the `ml_wf_val_results` object,
#' including metadata on the machine learning workflow validation results,
#' algorithm details, sample sizes, stock information, features, tuning,
#' Keras architecture parameters, performance, and the original call.
#'
#' @param object An instance of the `ml_wf_val_results` class.
#'
#' @return The method returns the object invisibly.
#'
#' @export
setMethod("show", "ml_wf_val_results", function(object) {

  # Extract the metadata
  metadata <- object@metadata

  # Create a neat display of the metadata
  cat("ML Workflow Validation Results Metadata\n")
  cat("=================================\n")

  # Display Algorithm Information
  cat("\nAlgorithm Information:\n")
  cat("  ML Algorithm:", metadata$ml_algorithm, "\n")
  cat("  Custom Objective:", metadata$custom_objective, "\n")

  cat("=================================\n")

  # Display Date Information
  cat("\nDate Information:\n")
  cat("  Dates Covered:", paste(metadata$dates_covered, collapse = ", "), "\n")
  cat("  Number of Dates:", metadata$n_dates, "\n")
  cat("  First Rebalance Date:", as.Date(metadata$first_rebalance_date), "\n")
  cat("  Rebalance Dates:", paste(metadata$rebalance_dates, collapse = ", "), "\n")
  cat("  Split Method:", metadata$split_method, "\n")

  cat("=================================\n")

  # Display Sample Sizes
  cat("\nSample Sizes:\n")
  cat("  Training Sample Size:", metadata$training_sample_size, "\n")
  cat("  Validation Sample Size:", metadata$validation_sample_size, "\n")
  cat("  Testing Sample Size:", metadata$testing_sample_size, "\n")
  cat("  Dates in Testing Sample:", paste(metadata$dates_testing_sample, collapse = ", "), "\n")

  cat("=================================\n")

  # Display Stocks Information
  cat("\nStocks Information:\n")
  cat("  Number of Observations:", metadata$nobs, "\n")
  cat("  Tickers:", paste(metadata$tickers, collapse = ", "), "\n")
  cat("  Number of Stocks:", metadata$n_stocks, "\n")

  cat("=================================\n")

  # Display Target Information
  cat("\nTarget Information:\n")
  cat("  Target Forward Name:", metadata$target_fwd_name, "\n")
  cat("  Target Forward:", metadata$target_fwd, "\n")
  cat("  Target Workflow:", metadata$target_workflow, "\n")
  cat("  Target Object:", metadata$target_object, "\n")

  cat("=================================\n")

  # Display Features Information
  cat("\nFeatures Information:\n")
  cat("  Features:", paste(metadata$features, collapse = ", "), "\n")
  cat("  Features Workflow:", metadata$features_workflow, "\n")
  cat("  Features Object:", metadata$features_object, "\n")

  cat("=================================\n")

  # Display Tuning Information
  cat("\nTuning Information:\n")
  cat("  Tuning Method:", metadata$tuning_method, "\n")
  cat("  Number of Iterations:", metadata$n_iter, "\n")
  cat("  Number of K-Folds:", metadata$k_iter, "\n")
  cat("  Acquisition Function:", metadata$acq, "\n")
  cat("  Initial Points:", metadata$init_points, "\n")
  cat("  Hyperparameter Grid Domain List:", paste(names(metadata$hyper_grid_domain_list), collapse = ", "), "\n")
  cat("  Chosen Evaluation Metric:", metadata$chosen_eval_metric, "\n")
  cat("  Huber Delta:", metadata$huber_delta, "\n")
  cat("  Quantile Tau:", metadata$quantile_tau, "\n")
  cat("  Early Stop:", metadata$early_stop, "\n")

  cat("=================================\n")

  # Display Keras Information
  cat("\nKeras Architecture Parameters:\n")
  print(metadata$keras_architecture_parameters)

  cat("=================================\n")

  # Display Performance Information
  cat("\nPerformance Information:\n")
  cat("  Completion Time:", metadata$completion_time, "\n")
  cat("  Elapsed Time:", metadata$elapsed_time, "seconds\n")
  cat("  Parallel Processing:", metadata$parallel, "\n")

  # Display Call Information
  cat("\nCall:\n")
  print(metadata$call)

  cat("=========================================\n")
})
#############################################


#' @title Show Portfolio Policies
#' @description Prints the contents of a `portfolio_policies` object, detailing
#' the various policies and their configurations.
#'
#' @param object A `portfolio_policies` object to be displayed.
#'
#' @method show portfolio_policies
#' @export
setMethod("show", "portfolio_policies", function(object) {
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

