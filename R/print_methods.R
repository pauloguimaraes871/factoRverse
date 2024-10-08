# Define the print method for meta_dataframe
############################################
setMethod("print", "meta_dataframe", function(x, ...) {
  # Extract the class name
  cat("meta_dataframe object\n")

  # Print a summary of the metadata
  cat("Metadata:\n")
  cat("  Number of signals:", ncol(x@data)-3, "\n")
  cat("  Unique Dates:", x@unique_dates, "\n")
  cat("  Unique Tickers:", x@unique_tickers, "\n")
  cat("  Total Observations (n_obs):", x@n_obs, "\n")
  cat("  Workflow:\n")
  print(x@workflow)
  cat("  Signals:\n")
  print(x@signals)

  # Print the first few rows of the data
  cat("\nFirst few rows of the data:\n")
  print(head(x@data))

  # Return the object invisibly
  invisible(x)
})
############################################


