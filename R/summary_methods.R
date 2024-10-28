#' Summary Method for meta_dataframe Class
#'
#' This method provides a detailed summary of the `meta_dataframe` object,
#' including tables summarizing numeric and categorical columns.
#' The tables and plots are styled accordingly, using the specified colors.
#'
#' @param object An instance of the `meta_dataframe` class.
#'
#' @return The method displays:
#'   - A summary table of numeric columns with specified formatting.
#'   - A frequency table for the 'tickers' column.
#'   - Plots for remaining categorical variables (excluding 'id' and 'tickers').
#'
#' @export
setMethod("summary", "meta_dataframe", function(object) {

  # Define colors based on the provided code
  deep_navy <- "#000033"                  # Deep Navy for data rows
  black <- "#000000"                      # Black for headers and 'Average' row
  white <- "#FFFFFF"                      # White text
  vibrant_purple <- "#6A0DAD"             # Vibrant Purple
  teal_blue <- "#00BFFF"                  # Teal Blue
  soft_pink <- "#FF69B4"                  # Soft Pink
  bright_yellow_orange <- "#FFA500"       # Bright Yellow-Orange
  blue_bg <- "#001f3f"                    # Blue background for plots
  cluster_colors <- c(vibrant_purple, teal_blue, soft_pink, bright_yellow_orange)

  # Exclude 'id', 'tickers', and 'dates' columns
  data_numeric <- object@data[, !(names(object@data) %in% c("id", "tickers", "dates")), drop = FALSE]

  # Identify numeric columns
  numeric_cols <- sapply(data_numeric, is.numeric)
  numeric_data <- data_numeric[, numeric_cols, drop = FALSE]

  # Identify categorical columns (excluding 'id', 'tickers', and 'dates')
  categorical_cols <- sapply(object@data, function(col) is.factor(col) || is.character(col))
  categorical_data <- object@data[, categorical_cols, drop = FALSE]
  categorical_data <- categorical_data[, !(names(categorical_data) %in% c("id", "tickers")), drop = FALSE]

  # Summary for numeric columns
  if (ncol(numeric_data) > 0) {
    numeric_summary <- data.frame(
      Variable = names(numeric_data),
      NAs = sapply(numeric_data, function(col) sum(is.na(col))),
      Min = sapply(numeric_data, function(col) min(col, na.rm = TRUE)),
      `1st Quartile` = sapply(numeric_data, function(col) quantile(col, 0.25, na.rm = TRUE)),
      Median = sapply(numeric_data, function(col) median(col, na.rm = TRUE)),
      Mean = sapply(numeric_data, function(col) mean(col, na.rm = TRUE)),
      `3rd Quartile` = sapply(numeric_data, function(col) quantile(col, 0.75, na.rm = TRUE)),
      Max = sapply(numeric_data, function(col) max(col, na.rm = TRUE)),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    # Include a final "Average" row with variable-wise averages
    average_row <- data.frame(
      Variable = "Average",
      NAs = mean(numeric_summary$NAs),
      Min = mean(as.numeric(numeric_summary$Min), na.rm = TRUE),
      `1st Quartile` = mean(as.numeric(numeric_summary$`1st Quartile`), na.rm = TRUE),
      Median = mean(as.numeric(numeric_summary$Median), na.rm = TRUE),
      Mean = mean(as.numeric(numeric_summary$Mean), na.rm = TRUE),
      `3rd Quartile` = mean(as.numeric(numeric_summary$`3rd Quartile`), na.rm = TRUE),
      Max = mean(as.numeric(numeric_summary$Max), na.rm = TRUE),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    numeric_summary <- rbind(numeric_summary, average_row)

    # Format numeric values (except 'NAs') in scientific notation with two digits after the decimal point
    numeric_columns <- names(numeric_summary)[!names(numeric_summary) %in% c("Variable", "NAs")]
    numeric_summary[numeric_columns] <- lapply(numeric_summary[numeric_columns], function(x) {
      formatC(as.numeric(x), format = "e", digits = 2)
    })
    # Round 'NAs' column to two decimal places
    numeric_summary$NAs <- round(as.numeric(numeric_summary$NAs), 2)

    # Format the numeric summary table
    numeric_summary_formatted <- DT::datatable(
      numeric_summary,
      rownames = FALSE,
      extensions = c('FixedColumns', 'Scroller'),
      options = list(
        scrollX = TRUE,
        scrollY = 400,
        scroller = TRUE,
        fixedColumns = list(leftColumns = 1),
        dom = 't',
        ordering = FALSE
      ),
      class = 'cell-border stripe',
      caption = htmltools::tags$caption(
        style = paste0('caption-side: top; text-align: center; color: ', white, '; font-weight: bold; font-size: 18px;'),
        "Summary of Numeric Variables"
      )
    )

    # Apply overall styling for data rows
    numeric_summary_formatted <- numeric_summary_formatted %>%
      DT::formatStyle(
        columns = names(numeric_summary),
        backgroundColor = deep_navy,
        color = white
      )

    # Apply styling to the 'Average' row
    numeric_summary_formatted <- numeric_summary_formatted %>%
      DT::formatStyle(
        'Variable',
        target = 'row',
        backgroundColor = DT::styleEqual("Average", black),
        color = DT::styleEqual("Average", white),
        fontWeight = DT::styleEqual("Average", "bold")
      )

    # Apply styling to the header via CSS
    css_styles_numeric <- paste0("
    table.dataTable thead th {
      background-color: ", black, " !important;
      color: ", white, " !important;
    }
    table.dataTable tbody tr:last-child {
      background-color: ", black, " !important;  /* Black for 'Average' row */
      color: ", white, " !important;
    }
    .dataTable {
      background-color: ", deep_navy, " !important;
      color: ", white, ";
    }
    table.dataTable tbody tr {
      background-color: ", deep_navy, " !important;  /* Deep Navy */
    }
    table.dataTable tbody td {
      border-color: #333333 !important;
    }
    ")

    # Add CSS to the numeric summary table
    numeric_summary_formatted <- htmlwidgets::prependContent(
      numeric_summary_formatted,
      htmltools::tags$style(css_styles_numeric)
    )

    # Display the numeric summary table
    cat("\n")
    print(numeric_summary_formatted)
  } else {
    cat("\nNo numeric columns to summarize.\n")
  }

  # Frequency table for 'tickers'
  if ("tickers" %in% names(object@data)) {
    tickers_data <- object@data$tickers
    freq_table <- as.data.frame(table(tickers_data, useNA = "ifany"))
    names(freq_table) <- c("Tickers", "Frequency")

    # Format the frequency table
    freq_table_formatted <- DT::datatable(
      freq_table,
      rownames = FALSE,
      extensions = c('FixedColumns', 'Scroller'),
      options = list(
        scrollX = TRUE,
        scrollY = 400,
        scroller = TRUE,
        fixedColumns = list(leftColumns = 1),
        dom = 't',
        ordering = FALSE
      ),
      class = 'cell-border stripe',
      caption = htmltools::tags$caption(
        style = paste0('caption-side: top; text-align: center; color: ', white, '; font-weight: bold; font-size: 18px;'),
        "Summary of Tickers"
      )
    ) %>%
      DT::formatStyle(
        columns = names(freq_table),
        backgroundColor = deep_navy,
        color = white,
        fontWeight = 'bold'
      )

    # Apply styling to the header via CSS
    css_styles_tickers <- paste0("
    table.dataTable thead th {
      background-color: ", black, " !important;
      color: ", white, " !important;
    }
    .dataTable {
      background-color: ", deep_navy, " !important;
      color: ", white, ";
    }
    table.dataTable tbody tr {
      background-color: ", deep_navy, " !important;  /* Deep Navy */
    }
    table.dataTable tbody td {
      border-color: #333333 !important;
    }
    ")

    # Add CSS to the frequency table
    freq_table_formatted <- htmlwidgets::prependContent(
      freq_table_formatted,
      htmltools::tags$style(css_styles_tickers)
    )

    # Display the frequency table
    cat("\n")
    print(freq_table_formatted)
  }

  # Plots for remaining categorical variables (excluding 'id' and 'tickers')
  if (!is.null(categorical_data) && ncol(categorical_data) > 0) {
    for (col_name in names(categorical_data)) {
      col_data <- categorical_data[[col_name]]
      freq_table <- as.data.frame(table(col_data, useNA = "ifany"))
      names(freq_table) <- c("Category", "Frequency")

      # Create a bar plot using the specified colors
      plot <- ggplot2::ggplot(freq_table, ggplot2::aes(x = reorder(Category, -Frequency), y = Frequency)) +
        ggplot2::geom_bar(stat = "identity", fill = vibrant_purple, color = white, size = 0.5) +
        ggplot2::geom_text(ggplot2::aes(label = Frequency), vjust = -0.5, color = white, size = 4) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.background = ggplot2::element_rect(fill = blue_bg, color = NA),    # Deep Navy background outside plot area
          panel.background = ggplot2::element_rect(fill = blue_bg, color = NA),   # White background inside plot area
          panel.grid.major = ggplot2::element_blank(),                              # Remove major grid lines
          panel.grid.minor = ggplot2::element_blank(),                              # Remove minor grid lines
          axis.text = ggplot2::element_text(color = white),
          axis.title = ggplot2::element_text(color = white),
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
          plot.title = ggplot2::element_text(size = 16, face = "bold", color = white, hjust = 0.5),
          plot.caption = ggplot2::element_text(color = white),
          legend.position = "none"
        ) +
        ggplot2::labs(
          title = paste("Frequency Plot for", col_name),
          x = col_name,
          y = "Frequency"
        ) +
        ggplot2::expand_limits(y = max(freq_table$Frequency) * 1.1)  # Add more space above bars

      # Display the plot
      cat("\n")
      print(plot)
    }
  } else {
    cat("\nNo additional categorical columns to plot.\n")
  }
})





#' Summary Method for ml_metabacktest_config Class
#'
#' Produces an interactive table summarizing the counts of configurations by `ml_algorithm` and other parameters using the `DT` package.
#' Neural networks (`nn`) are grouped by the number of hidden layers, resulting in rows like `nn_1`, `nn_2`, etc.
#' The table supports horizontal scrolling with the first column frozen and includes visual enhancements using specified colors.
#' Underscores in column headers are replaced with spaces.
#'
#' @param object An `ml_metabacktest_config` object.
#' @param ... Additional arguments (not used).
#' @return Invisibly returns a `DT` table object. This function is called for its side effect of displaying the table.
#' @examples
#' # Assuming you have an ml_metabacktest_config object named meta_config
#' summary(meta_config)
#'
#' @export
setMethod("summary", "ml_metabacktest_config",
          function(object, ...) {


            cat("Summary of 'ml_metabacktest_config' object\n")
            target_var <- unique(sapply(object@ml_backtest_configs, function(x) x@target_fwd_name))
            cat("Target variable:", target_var, "\n")
            n_configs <- length(object@ml_backtest_configs)
            cat(sprintf("Number of backtest configurations: %d\n\n", n_configs))

            if (n_configs > 0) {
              # Collect unique ml_algorithms, handling 'nn' separately
              ml_algorithms <- unique(sapply(object@ml_backtest_configs, function(x) {
                if (x@ml_algorithm == "nn") {
                  # Get number of hidden layers
                  num_hidden_layers <- length(x@keras_architecture_parameters@units)
                  return(paste0("nn_", num_hidden_layers))
                } else {
                  return(x@ml_algorithm)
                }
              }))

              # Collect possible options
              tuning_methods <- unique(unlist(sapply(object@ml_backtest_configs, function(x) {
                if (!is.null(x@tuning_strategy)) x@tuning_strategy@tuning_method else NA
              })))
              custom_objectives <- unique(sapply(object@ml_backtest_configs, function(x) x@custom_objective))
              chosen_eval_metrics <- unique(unlist(sapply(object@ml_backtest_configs, function(x) {
                if (!is.null(x@tuning_strategy)) x@tuning_strategy@chosen_eval_metric else NA
              })))

              # Remove NA values
              tuning_methods <- na.omit(tuning_methods)
              chosen_eval_metrics <- na.omit(chosen_eval_metrics)

              # Create all column names without prefixes
              count_col_names <- c(tuning_methods, custom_objectives, chosen_eval_metrics)
              quant_param_names <- c("huber_delta", "quantile_tau", "validation_sample_size")
              total_count_col <- "Algo_Count"
              col_names <- c("ml_algorithm", count_col_names, quant_param_names, total_count_col)

              # Create display names by replacing underscores with spaces
              display_col_names <- gsub("_", " ", col_names)

              # Initialize data frame
              summary_df <- data.frame(matrix("", nrow = length(ml_algorithms) + 1, ncol = length(col_names)),
                                       stringsAsFactors = FALSE)
              names(summary_df) <- col_names
              summary_df$ml_algorithm <- c(ml_algorithms, "Total")

              # Function to get range or single value
              get_range_or_value <- function(values) {
                values <- unique(na.omit(values))
                if (length(values) == 1) {
                  return(as.character(values))
                } else if (length(values) > 1) {
                  return(paste0(min(values), "-", max(values)))
                } else {
                  return(NA)
                }
              }

              # Populate counts and quantitative parameters
              total_counts <- numeric(length(ml_algorithms))
              for (i in seq_along(ml_algorithms)) {
                algo <- ml_algorithms[i]
                # Filter configs based on ml_algorithm and, for 'nn', the number of hidden layers
                configs <- object@ml_backtest_configs[sapply(object@ml_backtest_configs, function(x) {
                  if (x@ml_algorithm == "nn") {
                    num_hidden_layers <- length(x@keras_architecture_parameters@units)
                    paste0("nn_", num_hidden_layers) == algo
                  } else {
                    x@ml_algorithm == algo
                  }
                })]

                # Total count per ml_algorithm
                total_counts[i] <- length(configs)
                summary_df[i, total_count_col] <- total_counts[i]

                # Counts for tuning_methods
                tm_counts <- table(sapply(configs, function(x) {
                  if (!is.null(x@tuning_strategy)) x@tuning_strategy@tuning_method else NA
                }))
                for (tm in names(tm_counts)) {
                  summary_df[i, tm] <- as.numeric(tm_counts[tm])
                }

                # Counts for custom_objectives
                co_counts <- table(sapply(configs, function(x) x@custom_objective))
                for (co in names(co_counts)) {
                  summary_df[i, co] <- as.numeric(co_counts[co])
                }

                # Counts for chosen_eval_metrics
                cem_counts <- table(sapply(configs, function(x) {
                  if (!is.null(x@tuning_strategy)) x@tuning_strategy@chosen_eval_metric else NA
                }))
                for (cem in names(cem_counts)) {
                  summary_df[i, cem] <- as.numeric(cem_counts[cem])
                }

                # Quantitative parameters
                # huber_delta
                huber_deltas <- sapply(configs, function(x) x@huber_delta)
                summary_df[i, "huber_delta"] <- get_range_or_value(huber_deltas)

                # quantile_tau
                quantile_taus <- sapply(configs, function(x) x@quantile_tau)
                summary_df[i, "quantile_tau"] <- get_range_or_value(quantile_taus)

                # validation_sample_size
                val_sample_sizes <- sapply(configs, function(x) {
                  if (!is.null(x@tuning_strategy)) x@tuning_strategy@validation_sample_size else NA
                })
                summary_df[i, "validation_sample_size"] <- get_range_or_value(val_sample_sizes)
              }

              # Calculate totals for counts
              count_columns <- count_col_names
              for (col_name in count_columns) {
                counts <- as.numeric(summary_df[1:length(ml_algorithms), col_name])
                counts[is.na(counts)] <- 0
                total <- sum(counts)
                summary_df[length(ml_algorithms) + 1, col_name] <- total
              }

              # Total count for Total row
              summary_df[length(ml_algorithms) + 1, total_count_col] <- sum(total_counts)

              # For quantitative parameters in total row, display overall range or value
              # huber_delta
              all_huber_deltas <- sapply(object@ml_backtest_configs, function(x) x@huber_delta)
              summary_df[length(ml_algorithms) + 1, "huber_delta"] <- get_range_or_value(all_huber_deltas)

              # quantile_tau
              all_quantile_taus <- sapply(object@ml_backtest_configs, function(x) x@quantile_tau)
              summary_df[length(ml_algorithms) + 1, "quantile_tau"] <- get_range_or_value(all_quantile_taus)

              # validation_sample_size
              all_val_sample_sizes <- sapply(object@ml_backtest_configs, function(x) {
                if (!is.null(x@tuning_strategy)) x@tuning_strategy@validation_sample_size else NA
              })
              summary_df[length(ml_algorithms) + 1, "validation_sample_size"] <- get_range_or_value(all_val_sample_sizes)

              # Replace empty strings with NA
              summary_df[summary_df == ""] <- NA

              # Replace NAs with zeros in counts columns
              for (col_name in c(count_columns, total_count_col)) {
                summary_df[[col_name]][is.na(summary_df[[col_name]])] <- 0
                summary_df[[col_name]] <- as.numeric(summary_df[[col_name]])
              }

              # Convert quantitative parameters to character
              for (col_name in quant_param_names) {
                summary_df[[col_name]] <- as.character(summary_df[[col_name]])
              }

              # Create cluster indices
              col_index <- 2  # ml_algorithm is at index 1
              cluster_indices <- list()

              # Tuning Methods
              if(length(tuning_methods) > 0) {
                start_tm <- col_index
                end_tm <- col_index + length(tuning_methods) -1
                cluster_indices$tuning_methods <- start_tm:end_tm
                col_index <- end_tm +1
              }

              # Custom Objectives
              if(length(custom_objectives) >0) {
                start_co <- col_index
                end_co <- col_index + length(custom_objectives) -1
                cluster_indices$custom_objectives <- start_co:end_co
                col_index <- end_co +1
              }

              # Chosen Eval Metrics
              if(length(chosen_eval_metrics) >0) {
                start_cem <- col_index
                end_cem <- col_index + length(chosen_eval_metrics) -1
                cluster_indices$chosen_eval_metrics <- start_cem:end_cem
                col_index <- end_cem +1
              }

              # Quantitative Parameters
              if(length(quant_param_names) >0) {
                start_qp <- col_index
                end_qp <- col_index + length(quant_param_names) -1
                cluster_indices$quantitative_parameters <- start_qp:end_qp
                col_index <- end_qp +1
              }

              # Total Count Column
              cluster_indices$total_count <- col_index

              # Assign class names to columns via columnDefs
              columnDefs <- list()
              def_index <- 1

              # Colors for clusters (specified colors)
              cluster_colors <- c("#6A0DAD", "#00BFFF", "#FF69B4", "#FFA500")  # Vibrant Purple, Teal Blue, Soft Pink, Bright Yellow-Orange
              cluster_names <- c("cluster1", "cluster2", "cluster3", "cluster4")
              cluster_end_names <- c("cluster1_end", "cluster2_end", "cluster3_end", "cluster4_end")
              cluster_list <- list(cluster_indices$tuning_methods, cluster_indices$custom_objectives,
                                   cluster_indices$chosen_eval_metrics, cluster_indices$quantitative_parameters)

              for(i in seq_along(cluster_list)) {
                indices <- cluster_list[[i]]
                if(!is.null(indices)) {
                  # Adjust for zero-based indexing
                  indices_zero_based <- indices -1
                  # Assign className to columns
                  columnDefs[[def_index]] <- list(targets = indices_zero_based, className = cluster_names[i])
                  def_index <- def_index +1
                  # Assign end className to last column
                  columnDefs[[def_index]] <- list(targets = max(indices_zero_based), className = cluster_end_names[i])
                  def_index <- def_index +1
                }
              }

              # Style the Total_Count column
              total_count_index <- cluster_indices$total_count - 1  # Zero-based index
              columnDefs[[def_index]] <- list(targets = total_count_index, className = "total_count_col")

              # Create the datatable
              dt_table <- DT::datatable(
                summary_df,
                rownames = FALSE,
                colnames = display_col_names,  # Use display names with spaces
                extensions = c('FixedColumns', 'Scroller'),
                options = list(
                  scrollX = TRUE,
                  scrollY = 400,
                  scroller = TRUE,
                  fixedColumns = list(leftColumns = 1),
                  columnDefs = columnDefs,
                  dom = 't',
                  ordering = FALSE
                ),
                class = 'cell-border stripe'
              )

              # Apply formatting
              dt_table <- dt_table %>%
                DT::formatStyle(
                  columns = 'ml_algorithm',
                  fontWeight = 'bold',
                  backgroundColor = '#000033',  # Deep Navy
                  color = '#FFFFFF'
                ) %>%
                DT::formatStyle(
                  columns = names(summary_df),
                  valueColumns = 'ml_algorithm',
                  backgroundColor = DT::styleEqual("Total", "#000000"),  # Black for 'Total' row
                  color = DT::styleEqual("Total", "#FFFFFF")
                )

              # Define CSS styles
              css_styles <- paste0("
              .dataTable {
                background-color: #000033 !important;  /* Deep Navy */
                color: #FFFFFF;
              }
              table.dataTable tbody tr {
                background-color: #000033 !important;  /* Deep Navy */
              }
              table.dataTable tbody td {
                border-color: #333333 !important;
              }
              .cluster1 {
                background-color: rgba(106, 13, 173, 0.2);  /* Vibrant Purple */
              }
              .cluster2 {
                background-color: rgba(0, 191, 255, 0.2);   /* Teal Blue */
              }
              .cluster3 {
                background-color: rgba(255, 105, 180, 0.2);   /* Soft Pink */
              }
              .cluster4 {
                background-color: rgba(255, 165, 0, 0.2);     /* Bright Yellow-Orange */
              }
              .cluster1_end, .cluster2_end, .cluster3_end, .cluster4_end, .total_count_col {
                border-right: 2px solid #FFFFFF !important;
              }
              .total_count_col {
                background-color: #000033 !important;  /* Deep Navy */
                color: #FFFFFF !important;
              }
              thead {
                background-color: #000000 !important;
                color: #FFFFFF !important;
              }
              /* Style for the 'Total' row */
              table.dataTable tbody tr:last-child {
                background-color: #000000 !important;  /* Black */
                color: #FFFFFF !important;
              }
              ")

              # Add CSS to the datatable
              dt_table <- htmlwidgets::prependContent(dt_table, htmltools::tags$style(css_styles))

              # Print the table
              print(dt_table)

            } else {
              invisible(NULL)
            }
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
