# Define the print method for meta_dataframe
############################################
setMethod("show", "meta_dataframe", function(object) {
  # Extract the class name
  cat("meta_dataframe object\n")

  # Print a summary of the metadata
  cat("Metadata:\n")
  cat("  Number of signals:", ncol(object@data)-3, "\n")
  cat("  Unique Dates:", object@unique_dates, "\n")
  cat("  Unique Tickers:", object@unique_tickers, "\n")
  cat("  Total Observations (n_obs):", object@n_obs, "\n")
  cat("  Workflow:\n")
  print(object@workflow)
  cat("  Signals:\n")
  print(object@signals)

  # Print the first few rows of the data
  cat("\nFirst few rows of the data:\n")
  print(head(object@data))

  # Return the object invisibly
  invisible(object)
})
############################################


# Define the print method for refit_ml_model
############################################
setMethod("show", "refit_ml_model", function(object) {
  cat("Refit ML Model Summary:\n")

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
############################################

# Define the print method for ml_wf_val_results
#############################################
# Define the show method for the ml_wf_val_results class
setMethod("show", "ml_wf_val_results", function(object) {

  # Extract the metadata
  metadata <- object@metadata

  # Create a neat display of the metadata
  cat("=== ML Workflow Validation Results Metadata ===\n")

  # Display Algorithm Information
  cat("\nAlgorithm Information:\n")
  cat("  ML Algorithm:", metadata$ml_algorithm, "\n")
  cat("  Custom Objective:", metadata$custom_objective, "\n")

  # Display Date Information
  cat("\nDate Information:\n")
  cat("  Dates Covered:", paste(metadata$dates_covered, collapse = ", "), "\n")
  cat("  Number of Dates:", metadata$n_dates, "\n")
  cat("  First Rebalance Date:", metadata$first_rebalance_date, "\n")
  cat("  Rebalance Dates:", paste(metadata$rebalance_dates, collapse = ", "), "\n")
  cat("  Split Method:", metadata$split_method, "\n")

  # Display Sample Sizes
  cat("\nSample Sizes:\n")
  cat("  Training Sample Size:", metadata$training_sample_size, "\n")
  cat("  Validation Sample Size:", metadata$validation_sample_size, "\n")
  cat("  Testing Sample Size:", metadata$testing_sample_size, "\n")
  cat("  Dates in Testing Sample:", paste(metadata$dates_testing_sample, collapse = ", "), "\n")

  # Display Stocks Information
  cat("\nStocks Information:\n")
  cat("  Number of Observations:", metadata$nobs, "\n")
  cat("  Tickers:", paste(metadata$tickers, collapse = ", "), "\n")
  cat("  Number of Stocks:", metadata$n_stocks, "\n")

  # Display Target Information
  cat("\nTarget Information:\n")
  cat("  Target Forward Name:", metadata$target_fwd_name, "\n")
  cat("  Target Forward:", metadata$target_fwd, "\n")
  cat("  Target Workflow:", metadata$target_workflow, "\n")
  cat("  Target Object:", metadata$target_object, "\n")

  # Display Features Information
  cat("\nFeatures Information:\n")
  cat("  Features:", paste(metadata$features, collapse = ", "), "\n")
  cat("  Features Workflow:", metadata$features_workflow, "\n")
  cat("  Features Object:", metadata$features_object, "\n")

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

  # Display Keras Information
  cat("\nKeras Architecture Parameters:\n")
  print(metadata$keras_architecture_parameters)

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
