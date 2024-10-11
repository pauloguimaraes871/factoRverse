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


# Define the print method for meta_dataframe
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
