#' Summary Method for meta_dataframe Class
#'
#' This method provides a detailed summary of the `meta_dataframe` object,
#' including basic statistics of the data frame, information about missing
#' values, descriptive statistics for numeric columns, and counts for
#' categorical columns.
#'
#' @param object An instance of the `meta_dataframe` class.
#'
#' @return The method prints a summary to the console, including:
#'   - Number of observations and columns.
#'   - Data types of each column.
#'   - Missing values count per column.
#'   - Descriptive statistics for numeric columns.
#'   - Counts of unique values for categorical columns.
#'   - Summary statistics for tickers and date range.
#'   - Overall data frame summary.
#'
#' @export
setMethod("summary", "meta_dataframe", function(object) {
  # Basic Data Frame Summary
  data_summary <- summary(object@data)

  # Additional Information
  n_obs <- nrow(object@data)
  n_cols <- ncol(object@data)
  col_types <- sapply(object@data, class)

  # Check for missing values in each column
  missing_values <- sapply(object@data, function(col) sum(is.na(col)))

  # Descriptive statistics for numeric columns
  numeric_summary <- lapply(object@data, function(col) {
    if (is.numeric(col)) {
      return(c(
        Min = min(col, na.rm = TRUE),
        Max = max(col, na.rm = TRUE),
        Mean = mean(col, na.rm = TRUE),
        Median = median(col, na.rm = TRUE),
        SD = sd(col, na.rm = TRUE),
        NAs = sum(is.na(col)),
        Quantiles = quantile(col, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
      ))
    } else {
      return(NULL)
    }
  })

  # Count occurrences for categorical columns
  categorical_summary <- lapply(object@data, function(col) {
    if (is.factor(col) || is.character(col)) {
      return(table(col, useNA = "ifany"))
    } else {
      return(NULL)
    }
  })

  # Create a cleaner output for numeric summary
  numeric_summary <- Filter(Negate(is.null), numeric_summary)
  categorical_summary <- Filter(Negate(is.null), categorical_summary)

  # Extract specific column summaries for your data structure
  id_summary <- unique(object@data$id)
  ticker_summary <- table(object@data$tickers)
  date_range <- range(object@data$dates, na.rm = TRUE)

  # Print the summary
  cat("Summary of meta_dataframe:\n")
  cat("Number of observations:", n_obs, "\n")
  cat("Number of columns:", n_cols, "\n")
  cat("Column types:\n")
  print(col_types)

  cat("\nMissing values in each column:\n")
  print(missing_values)

  cat("\nSummary of numeric columns:\n")
  print(numeric_summary)

  cat("\nSummary of categorical columns:\n")
  print(categorical_summary)

  cat("\nTicker Summary (Counts):\n")
  print(ticker_summary)

  cat("\nDate Range:\n")
  print(date_range)

  cat("\nOverall data summary:\n")
  print(data_summary)

})



#' Summary Method for ml_metabacktest_config Class
#'
#' Provides aggregated statistics about the `ml_metabacktest_config` object, including counts and ranges for various parameters.
#'
#' @param object An `ml_metabacktest_config` object.
#' @param ... Additional arguments (not used).
#' @return Invisibly returns `NULL`. This function is called for its side effect of displaying information.
#' @examples
#' # Assuming you have an ml_metabacktest_config object named meta_config
#' summary(meta_config)
#'
#' @export
setMethod("summary", "ml_metabacktest_config",
          function(object, ...) {
            cat("Summary of 'ml_metabacktest_config' object\n")
            cat("Target variable:", unique(sapply(object@ml_backtest_configs, function(x) x@target_fwd_name)), "\n")
            n_configs <- length(object@ml_backtest_configs)
            cat(sprintf("Number of backtest configurations: %d\n", n_configs))
            if (n_configs > 0) {
              cat("\nAggregated statistics:\n")

              # Counts per ml_algorithm
              ml_algorithms <- sapply(object@ml_backtest_configs, function(x) x@ml_algorithm)
              algorithm_counts <- table(ml_algorithms)
              cat("Counts of configurations per ml_algorithm:\n")
              print(algorithm_counts)
              cat("\n")

              # For neural networks, count by number of layers
              nn_configs <- object@ml_backtest_configs[ml_algorithms == "nn"]
              if (length(nn_configs) > 0) {
                n_layers_list <- sapply(nn_configs, function(x) {
                  if (!is.null(x@keras_architecture_parameters@units)) {
                    length(x@keras_architecture_parameters@units)
                  } else {
                    NA
                  }
                })
                n_layers_list <- na.omit(n_layers_list)
                if (length(n_layers_list) > 0) {
                  n_layers_counts <- table(n_layers_list)
                  cat("Counts of nn configurations by n_layers:\n")
                  print(n_layers_counts)
                  cat("\n")
                }
              }

              # Counts per tuning_method
              tuning_methods <- sapply(object@ml_backtest_configs, function(x) {
                if (!is.null(x@tuning_strategy) && !is.null(x@tuning_strategy@tuning_method)) {
                  x@tuning_strategy@tuning_method
                } else {
                  NA
                }
              })
              tuning_methods <- na.omit(tuning_methods)
              if (length(tuning_methods) > 0) {
                tuning_method_counts <- table(tuning_methods)
                cat("Counts of configurations per tuning_method:\n")
                print(tuning_method_counts)
                cat("\n")
              }

              # Counts per custom_objective
              custom_objectives <- sapply(object@ml_backtest_configs, function(x) x@custom_objective)
              custom_objective_counts <- table(custom_objectives)
              cat("Counts of configurations per custom_objective:\n")
              print(custom_objective_counts)
              cat("\n")

              # Counts per chosen_eval_metric
              chosen_eval_metrics <- sapply(object@ml_backtest_configs, function(x) {
                if (!is.null(x@tuning_strategy) && !is.null(x@tuning_strategy@chosen_eval_metric)) {
                  x@tuning_strategy@chosen_eval_metric
                } else {
                  NA
                }
              })
              chosen_eval_metrics <- na.omit(chosen_eval_metrics)
              if (length(chosen_eval_metrics) > 0) {
                chosen_eval_metric_counts <- table(chosen_eval_metrics)
                cat("Counts of configurations per chosen_eval_metric:\n")
                print(chosen_eval_metric_counts)
                cat("\n")
              }

              # Quantitative metrics
              # validation_sample_size
              validation_sizes <- sapply(object@ml_backtest_configs, function(x) {
                if (!is.null(x@tuning_strategy) && !is.null(x@tuning_strategy@validation_sample_size)) {
                  x@tuning_strategy@validation_sample_size
                } else {
                  NA
                }
              })
              validation_sizes <- na.omit(validation_sizes)
              if (length(validation_sizes) > 0) {
                unique_validation_sizes <- unique(validation_sizes)
                if (length(unique_validation_sizes) == 1) {
                  cat(sprintf("validation_sample_size: %s\n", unique_validation_sizes))
                } else {
                  cat(sprintf("Range of validation_sample_size: %s - %s\n",
                              min(validation_sizes), max(validation_sizes)))
                }
              }

              # early_stop
              early_stops <- sapply(object@ml_backtest_configs, function(x) {
                if (!is.null(x@tuning_strategy) && !is.null(x@tuning_strategy@early_stop)) {
                  x@tuning_strategy@early_stop
                } else {
                  NA
                }
              })
              early_stops <- na.omit(early_stops)
              if (length(early_stops) > 0) {
                unique_early_stops <- unique(early_stops)
                if (length(unique_early_stops) == 1) {
                  cat(sprintf("Unique early_stop value: %s\n", unique_early_stops))
                } else {
                  cat(sprintf("Range of early_stop values: %s - %s\n",
                              min(unique_early_stops), max(unique_early_stops)))
                }
              }

              # quantile_tau
              quantile_taus <- sapply(object@ml_backtest_configs, function(x) x@quantile_tau)
              quantile_taus <- na.omit(quantile_taus)
              if (length(quantile_taus) > 0) {
                unique_quantile_taus <- unique(quantile_taus)
                if (length(unique_quantile_taus) == 1) {
                  cat(sprintf("Unique quantile_tau value: %s\n", unique_quantile_taus))
                } else {
                  cat(sprintf("Range of quantile_tau values: %s - %s\n",
                              min(quantile_taus), max(quantile_taus)))
                }
              }

              # huber_delta
              huber_deltas <- sapply(object@ml_backtest_configs, function(x) x@huber_delta)
              huber_deltas <- na.omit(huber_deltas)
              if (length(huber_deltas) > 0) {
                unique_huber_deltas <- unique(huber_deltas)
                if (length(unique_huber_deltas) == 1) {
                  cat(sprintf("Unique huber_delta value: %s\n", unique_huber_deltas))
                } else {
                  cat(sprintf("Range of huber_delta values: %s - %s\n",
                              min(huber_deltas), max(huber_deltas)))
                }
              }
            }
            invisible(NULL)
          })





#' Summary Method for ml_backtest_results Class
#'
#' This method provides a comprehensive summary of the `ml_backtest_results`
#' object, including statistics for out-of-sample predictions, errors,
#' actual values, evaluation metrics, and hyperparameters.
#'
#' @param object An instance of the `ml_backtest_results` class.
#'
#' @return The method prints a summary to the console, including:
#'   - Main information about the ML algorithm, final model, and objective function.
#'   - Summary statistics (mean, median, SD, min, max) for out-of-sample predictions, errors, actual values, and evaluation metrics.
#'   - Chosen evaluation metric validation summaries.
#'   - Best hyperparameters and validation evaluation metrics hyper choice.
#'
#'
#' @export
setMethod("summary", "ml_backtest_results", function(object) {

  # Helper function to summarize a list of vectors or data frames
  summarize_list <- function(lst) {
    summaries <- lapply(names(lst), function(name) {
      x <- lst[[name]]
      if (is.vector(x) || is.data.frame(x)) {
        summary_stats <- data.frame(
          Date = name,  # Add the date as the first column
          Mean = mean(x, na.rm = TRUE),
          Median = median(x, na.rm = TRUE),
          SD = sd(x, na.rm = TRUE),
          Min = min(x, na.rm = TRUE),
          Max = max(x, na.rm = TRUE)
        )
        return(summary_stats)
      } else {
        return(NULL)
      }
    })
    # Combine all summaries into one data frame and remove NULLs
    summaries <- do.call(rbind, summaries)
    return(summaries)
  }


  # Extracting summary statistics for each component
  prediction_summary <- summarize_list(object@oos_prediction_list)
  error_summary <- summarize_list(object@oos_error_list)
  y_summary <- summarize_list(object@oos_y_list)
  metrics_summary <- summarize_list(object@oos_testing_eval_metrics)

  # Combine summaries into a single data frame for output
  summary_table <- list(
    OOS_Predictions = prediction_summary,
    OOS_Errors = error_summary,
    OOS_Y = y_summary,
    OOS_Testing_Eval_Metrics = metrics_summary
  )

  # Main information
  cat("\n ML Algorithm:", object@metadata$ml_algorithm, "\n")
  cat("Final Model Object Class:", class(object@final_model@model_class), "\n")
  cat("Objective Function:", object@metadata$custom_objective, "\n")
  cat("Chosen Evaluation Metric:", object@metadata$chosen_eval_metric, "\n")
  cat("Testing Sample Dates:", format(as.Date(object@metadata$dates_testing_sample), "%d-%m-%Y"), "\n")
  cat("Rebalancing Dates:", format(as.Date(object@metadata$rebalance_dates), "%d-%m-%Y"), "\n")

  # Print each summary table
  for (name in names(summary_table)) {
    cat(paste("\n", name, "Summary:\n"))
    if (!is.null(summary_table[[name]])) {
      print(knitr::kable(summary_table[[name]],
                         caption = paste("Summary of", name),
                         align = 'c',  # Center-align all columns
                         format = "markdown"))  # Use markdown for formatting
    } else {
      cat("No data available.\n")
    }
  }

  # Summarize chosen evaluation metric validation
  cat("\nChosen Evaluation Metric Validation:\n")
  if (!is.null(object@chosen_eval_metric_validation) && length(object@chosen_eval_metric_validation) > 0) {
    validation_summaries <- lapply(object@chosen_eval_metric_validation, function(x) {
      data.frame(
        Mean = mean(x, na.rm = TRUE),
        Median = median(x, na.rm = TRUE),
        SD = sd(x, na.rm = TRUE),
        Min = min(x, na.rm = TRUE),
        Max = max(x, na.rm = TRUE)
      )
    })
    names(validation_summaries) <- names(object@chosen_eval_metric_validation)
    for (name in names(validation_summaries)) {
      cat(paste("\n", name, "Summary:\n"))
      print(knitr::kable(validation_summaries[[name]],
                         caption = paste("Summary of", name),
                         align = 'c',
                         format = "markdown"))
    }
  } else {
    cat("Not specified or empty.\n")
  }

  # Summarize best hyperparameters
  cat("\nBest Hyperparameters:\n")
  if (!is.null(object@best_hyperparameters)) {
    print(knitr::kable(object@best_hyperparameters,
                       caption = "Best Hyperparameters",
                       align = 'c',
                       format = "markdown"))
  } else {
    cat("Not specified.\n")
  }

  # Summarize validation evaluation metrics hyper choice
  cat("\nValidation Eval Metrics Hyper Choice:\n")
  if (!is.null(object@validation_eval_metrics_hyper_choice)) {
    print(knitr::kable(object@validation_eval_metrics_hyper_choice,
                       caption = "Validation Eval Metrics Hyper Choice",
                       align = 'c',
                       format = "markdown"))
  } else {
    cat("Not specified.\n")
  }

  invisible(object)  # Return the object invisibly
})
