#' @title Summary Method for meta_dataframe Class
#' @description Provides a detailed summary of a `meta_dataframe` object.
#' Users can select which summary to display by specifying the `summary_id` parameter,
#' either by name or by number.
#' The summary includes tables and plots styled accordingly.
#'
#' @param object An instance of the `meta_dataframe` class.
#' @param summary_id A character string or numeric value specifying which summary to display.
#'   - By name: Options are:
#'     - `"Numeric Summary Table"`
#'     - `"Tickers Frequency Table"`
#'     - `"Categorical Variables Plots"`
#'   - By number: Provide a number corresponding to the summary (as listed when `summary_id` is `NULL`).
#'   If `NULL` (default), the method lists available summaries.
#' @return Invisibly returns the input `object`.
#' @export
setMethod("summary", "meta_dataframe", function(object, summary_id = NULL) {

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

  # List of available summaries
  available_summaries <- c(
    "Numeric Summary Table",
    "Tickers Frequency Table",
    "Categorical Variables Plots"
  )

  # Display Main Information always
  cat("\nMeta Dataframe Summary\n")
  cat("Number of Rows:", nrow(object@data), "\n")
  cat("Number of Columns:", ncol(object@data), "\n")
  cat("Columns:", paste(names(object@data), collapse = ", "), "\n")

  if (is.null(summary_id)) {
    cat("\nAvailable summaries to display:\n")
    for (i in seq_along(available_summaries)) {
      cat(paste0(i, ": ", available_summaries[i], "\n"))
    }
    cat("\nPlease specify the 'summary_id' parameter to display a summary.\n")
    cat("You can select a summary either by name or by number.\n")
    return(invisible(object))
  }

  # Determine if summary_id is numeric (index) or character (name)
  if (is.numeric(summary_id)) {
    if (summary_id >= 1 && summary_id <= length(available_summaries)) {
      summary_name <- available_summaries[summary_id]
    } else {
      stop("Invalid summary number. Please select a number between 1 and ", length(available_summaries), ".")
    }
  } else if (is.character(summary_id)) {
    if (summary_id %in% available_summaries) {
      summary_name <- summary_id
    } else {
      stop("Invalid 'summary_id' specified. Available options are:\n",
           paste(available_summaries, collapse = ", "))
    }
  } else {
    stop("'summary_id' must be either a string or a number corresponding to the summary.")
  }

  # Exclude 'id', 'tickers', and 'dates' columns
  data_numeric <- object@data[, !(names(object@data) %in% c("id", "tickers", "dates")), drop = FALSE]

  # Identify numeric columns
  numeric_cols <- sapply(data_numeric, is.numeric)
  numeric_data <- data_numeric[, numeric_cols, drop = FALSE]

  # Identify categorical columns (excluding 'id', 'tickers', and 'dates')
  categorical_cols <- sapply(object@data, function(col) is.factor(col) || is.character(col))
  categorical_data <- object@data[, categorical_cols, drop = FALSE]
  categorical_data <- categorical_data[, !(names(categorical_data) %in% c("id", "tickers", "dates")), drop = FALSE]

  # Function to display the numeric summary table
  display_numeric_summary <- function() {
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

      # Round numeric values
      numeric_columns <- names(numeric_summary)[!names(numeric_summary) %in% c("Variable", "NAs")]
      numeric_summary[numeric_columns] <- lapply(numeric_summary[numeric_columns], function(x) {
        round(as.numeric(x), 4)
      })
      # Round 'NAs' column to two decimal places
      numeric_summary$NAs <- round(as.numeric(numeric_summary$NAs), 2)

      # Format the numeric summary table with scroll bars and title
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
          style = paste0('caption-side: top; text-align: center; color: ', white, '; background-color: ', black, '; padding: 10px; margin-bottom: 10px; font-size: 18px; font-weight: bold;'),
          'Summary of Numeric Variables'
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
      print(numeric_summary_formatted)
    } else {
      cat("\nNo numeric columns to summarize.\n")
    }
  }

  # Function to display the tickers frequency table
  display_tickers_frequency <- function() {
    if ("tickers" %in% names(object@data)) {
      tickers_data <- object@data$tickers
      freq_table <- as.data.frame(table(tickers_data, useNA = "ifany"))
      names(freq_table) <- c("Tickers", "Frequency")

      # Format the frequency table with scroll bars and title
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
          style = paste0('caption-side: top; text-align: center; color: ', white, '; background-color: ', black, '; padding: 10px; margin-bottom: 10px; font-size: 18px; font-weight: bold;'),
          'Summary of Tickers'
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
      print(freq_table_formatted)
    } else {
      cat("\nNo 'tickers' column found in the data.\n")
    }
  }

  # Function to display plots for categorical variables
  display_categorical_plots <- function() {
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
            panel.background = ggplot2::element_rect(fill = blue_bg, color = NA),   # Blue background inside plot area
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
        print(plot)
      }
    } else {
      cat("\nNo additional categorical columns to plot.\n")
    }
  }

  # Display the selected summary
  if (summary_name == "Numeric Summary Table") {
    display_numeric_summary()
  } else if (summary_name == "Tickers Frequency Table") {
    display_tickers_frequency()
  } else if (summary_name == "Categorical Variables Plots") {
    display_categorical_plots()
  }

  invisible(object)
})





#' Summary Method for ml_metabacktest_config Class
#'
#' Produces an interactive table summarizing the counts of configurations by `ml_algorithm` and other parameters using the `DT` package.
#' Neural networks (`nn`) are grouped by the number of hidden layers, resulting in rows like `nn_1`, `nn_2`, etc.
#' The table supports horizontal and vertical scrolling with the first column frozen and includes visual enhancements using specified colors.
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
            n_configs <- length(object@ml_backtest_configs)
            cat(sprintf("Number of backtest configurations: %d\n\n", n_configs))
            training_sample_size <- unique(sapply(object@ml_backtest_configs, function(x) x@training_sample_size))
            cat(paste("Training sample size:", max(training_sample_size), "\n"))
            cat(paste("Training sample size (OLS):", min(training_sample_size), "\n"))
            rebalancing_months <- unique(sapply(object@ml_backtest_configs, function(x) x@rebalancing_months))
            cat(paste("Rebalancing months:", paste(rebalancing_months, collapse = ", "), "\n"))

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

              # Create the datatable with caption for the title
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
                class = 'cell-border stripe',
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: center; color: #FFFFFF; background-color: #000000; padding: 10px; margin-bottom: 10px; font-size: 18px; font-weight: bold;',
                  'Summary of ML Metabacktesting Configuration'
                )
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



#' @title Summary Method for ml_backtest_results Class
#' @description Provides a detailed summary of an `ml_backtest_results` object.
#' Users can select which summary table to display by specifying the `summary_id` parameter,
#' either by name or by number.
#' The summary includes interactive tables styled using the `DT` package.
#'
#' @param object An object of class `ml_backtest_results`.
#' @param summary_id A character string or numeric value specifying which table to display.
#'   - By name: Options are:
#'     - `"OOS_Predictions"`
#'     - `"OOS_Errors"`
#'     - `"OOS_Y"`
#'     - `"OOS_Testing_Eval_Metrics"`
#'     - `"Chosen_Eval_Metric_Validation"`
#'   - By number: Provide a number corresponding to the table (as listed when `summary_id` is `NULL`).
#'   If `NULL` (default), the method lists available tables.
#' @return Invisibly returns the input `object`.
#' @importFrom methods setMethod
#' @export
setMethod("summary", "ml_backtest_results", function(object, summary_id = NULL) {

  # Define colors for styling
  deep_navy <- "#000033"   # Deep Navy for data rows
  black <- "#000000"       # Black for headers
  white <- "#FFFFFF"       # White text

  # List of available tables (excluding 'Main_Information')
  available_tables <- c(
    "OOS_Predictions",
    "OOS_Errors",
    "OOS_Y",
    "OOS_Testing_Eval_Metrics",
    "Chosen_Eval_Metric_Validation"
  )

  # Display Main Information always
  cat("\nML Algorithm:", object@ml_backtest_workflow$ml_algorithm, "\n")
  cat("Final Model Object Class:", object@final_model@model_class, "\n")
  cat("Custom Objective:", object@ml_backtest_workflow$custom_objective, "\n")
  cat("Chosen Evaluation Metric:", object@ml_backtest_workflow$chosen_eval_metric, "\n")
  cat("Testing Sample Dates:", format(as.Date(object@ml_backtest_workflow$dates_testing_sample), "%d-%m-%Y"), "\n")
  cat("Rebalancing Dates:", format(as.Date(object@ml_backtest_workflow$rebalance_dates), "%d-%m-%Y"), "\n")

  if (is.null(summary_id)) {
    cat("\nAvailable tables to display:\n")
    for (i in seq_along(available_tables)) {
      cat(paste0(i, ": ", available_tables[i], "\n"))
    }
    cat("\nPlease specify the 'summary_id' parameter to display a table.\n")
    cat("You can select a table either by name or by number.\n")
    return(invisible(object))
  }

  # Determine if summary_id is numeric (index) or character (name)
  if (is.numeric(summary_id)) {
    if (summary_id >= 1 && summary_id <= length(available_tables)) {
      table_name <- available_tables[summary_id]
    } else {
      stop("Invalid table number. Please select a number between 1 and ", length(available_tables), ".")
    }
  } else if (is.character(summary_id)) {
    if (summary_id %in% available_tables) {
      table_name <- summary_id
    } else {
      stop("Invalid 'summary_id' specified. Available options are:\n",
           paste(available_tables, collapse = ", "))
    }
  } else {
    stop("'summary_id' must be either a string or a number corresponding to the table.")
  }

  # Function to round numeric columns
  round_numeric_columns <- function(df, digits = 4) {
    numeric_cols <- sapply(df, is.numeric)
    df[numeric_cols] <- lapply(df[numeric_cols], round, digits = digits)
    return(df)
  }

  # Function to create and display a styled datatable
  display_table <- function(data_df, title) {
    if (is.null(data_df) || nrow(data_df) == 0) {
      cat("No data available for", title, "\n")
      return()
    }

    # Round numeric columns
    data_df <- round_numeric_columns(data_df, digits = 4)

    # Format the table
    data_dt <- DT::datatable(
      data_df,
      rownames = FALSE,
      extensions = c('FixedColumns'),
      options = list(
        scrollX = TRUE,
        scrollY = '400px',
        scrollCollapse = TRUE,
        fixedColumns = list(leftColumns = 1),
        dom = 't',
        ordering = FALSE
      ),
      class = 'cell-border stripe'
    )

    # Apply overall styling
    data_dt <- data_dt %>%
      DT::formatStyle(
        columns = names(data_df),
        backgroundColor = deep_navy,
        color = white
      )

    # Apply styling to the header via CSS
    css_styles <- paste0("
    table.dataTable thead th {
      background-color: ", black, " !important;
      color: ", white, " !important;
    }
    .dataTable {
      background-color: ", deep_navy, " !important;
      color: ", white, ";
    }
    table.dataTable tbody tr {
      background-color: ", deep_navy, " !important;
    }
    table.dataTable tbody td {
      border-color: #333333 !important;
    }
    .dataTables_wrapper {
      overflow-x: auto !important;
      overflow-y: auto !important;
    }
    ")

    # Add CSS to the table
    data_dt <- htmlwidgets::prependContent(
      data_dt,
      htmltools::tags$style(css_styles)
    )

    # Add title above the table
    data_dt <- htmlwidgets::prependContent(
      data_dt,
      htmltools::tags$h3(
        style = paste0("color: ", white, "; background-color: ", black, "; padding: 10px; margin-bottom: 10px;"),
        title
      )
    )

    # Display the table
    print(data_dt)
  }

  # Prepare data based on the selected table
  if (table_name %in% c("OOS_Predictions", "OOS_Errors", "OOS_Y", "OOS_Testing_Eval_Metrics")) {
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
    summary_list <- list(
      OOS_Predictions = object@oos_prediction_list,
      OOS_Errors = object@oos_error_list,
      OOS_Y = object@oos_y_list,
      OOS_Testing_Eval_Metrics = object@oos_testing_eval_metrics
    )

    data_list <- summarize_list(summary_list[[table_name]])

    # Display the table
    display_table(data_list, paste("Summary of", table_name))

  } else if (table_name == "Chosen_Eval_Metric_Validation") {
    # Summarize chosen evaluation metric validation
    if (!is.null(object@chosen_eval_metric_validation) && length(object@chosen_eval_metric_validation) > 0) {
      # Combine all data frames in the list into one data frame
      chosen_eval_metric_validation_list <- object@chosen_eval_metric_validation
      chosen_eval_metric_validation_df <- do.call(rbind, chosen_eval_metric_validation_list)

      # Ensure hyperparameter columns are correctly identified
      hyperparameter_columns <- setdiff(names(chosen_eval_metric_validation_df), c("chosen_eval_metric", "Date"))

      # Transform hyperparameter columns into categorical variables by binning
      for (hyperparam in hyperparameter_columns) {
        tryCatch({
          # Bin numerical hyperparameters into quantiles (deciles)
          if (is.numeric(chosen_eval_metric_validation_df[[hyperparam]])) {
            chosen_eval_metric_validation_df[[hyperparam]] <- as.factor(
              cut(chosen_eval_metric_validation_df[[hyperparam]],
                  breaks = unique(stats::quantile(chosen_eval_metric_validation_df[[hyperparam]], probs = seq(0, 1, by = 0.1), na.rm = TRUE)),
                  include.lowest = TRUE)
            )
          } else {
            # If not numeric, convert to factor directly
            chosen_eval_metric_validation_df[[hyperparam]] <- as.factor(chosen_eval_metric_validation_df[[hyperparam]])
          }
        }, error = function(e) {
          message(paste("Only one unique value identified for", hyperparam))
          chosen_eval_metric_validation_df[[hyperparam]] <- as.factor(chosen_eval_metric_validation_df[[hyperparam]])
        })
      }

      # Create concatenation of hyperparameters to define groups
      ml_algorithm <- object@ml_backtest_workflow$ml_algorithm

      if (ml_algorithm == "glmnet") {
        chosen_eval_metric_validation_df$concatenation <- paste(
          chosen_eval_metric_validation_df$alpha,
          chosen_eval_metric_validation_df$lambda.min.ratio,
          sep = "_"
        )
      } else if (ml_algorithm == "rf") {
        chosen_eval_metric_validation_df$concatenation <- paste(
          chosen_eval_metric_validation_df$mtry,
          chosen_eval_metric_validation_df$num.trees,
          chosen_eval_metric_validation_df$max.depth,
          chosen_eval_metric_validation_df$min.bucket,
          sep = "_"
        )
      } else if (ml_algorithm == "xgb") {
        chosen_eval_metric_validation_df$concatenation <- paste(
          chosen_eval_metric_validation_df$min_child_weight,
          chosen_eval_metric_validation_df$max_depth,
          chosen_eval_metric_validation_df$subsample,
          chosen_eval_metric_validation_df$colsample_bytree,
          chosen_eval_metric_validation_df$eta,
          chosen_eval_metric_validation_df$alpha,
          chosen_eval_metric_validation_df$gamma,
          chosen_eval_metric_validation_df$nrounds,
          sep = "_"
        )
      } else if (ml_algorithm == "nn") {
        chosen_eval_metric_validation_df$concatenation <- paste(
          chosen_eval_metric_validation_df$regularizer_l1,
          chosen_eval_metric_validation_df$regularizer_l2,
          chosen_eval_metric_validation_df$droprate,
          chosen_eval_metric_validation_df$lr,
          sep = "_"
        )
      } else {
        # For other algorithms, concatenate all hyperparameters
        chosen_eval_metric_validation_df$concatenation <- do.call(paste, c(chosen_eval_metric_validation_df[hyperparameter_columns], sep = "_"))
      }

      # Summarize the chosen evaluation metric by hyperparameter combinations
      chosen_eval_metric_validation_summary <- chosen_eval_metric_validation_df %>%
        dplyr::group_by(concatenation) %>%
        dplyr::summarise(
          mean_chosen_eval_metric = mean(chosen_eval_metric, na.rm = TRUE),
          median_chosen_eval_metric = median(chosen_eval_metric, na.rm = TRUE),
          q25 = stats::quantile(chosen_eval_metric, 0.25, na.rm = TRUE),
          q75 = stats::quantile(chosen_eval_metric, 0.75, na.rm = TRUE),
          max = max(chosen_eval_metric, na.rm = TRUE),
          min = min(chosen_eval_metric, na.rm = TRUE),
          .groups = 'drop'
        )

      # Display the table
      display_table(chosen_eval_metric_validation_summary, "Chosen Evaluation Metric Summary")


    } else {
      cat("Not specified or empty.\n")
    }
  }

  invisible(object)  # Return the object invisibly
})




#' @title Summary Method for ml_metabacktest_results Class
#' @description Provides a detailed summary of an `ml_metabacktest_results` object.
#' Users can select which summary table to display by specifying the `summary_id` parameter,
#' either by name or by number.
#' The summary includes interactive tables styled using the `DT` package.
#'
#' @param object An object of class `ml_metabacktest_results`.
#' @param summary_id A character string or numeric value specifying which table to display.
#'   - By name: Options are:
#'     - `"Time Series OOS Testing Metrics Summary"`
#'     - `"Time Series Validation Metrics Summary"`
#'   - By number: Provide a number corresponding to the table (as listed when `summary_id` is `NULL`).
#'   If `NULL` (default), the method lists available summaries.
#'
#' @return Invisibly returns the input `object`.
#' @importFrom methods setMethod
#' @export
setMethod("summary", "ml_metabacktest_results", function(object, summary_id = NULL) {


  # Define colors for styling
  deep_navy <- "#000033"   # Deep Navy for data rows
  black <- "#000000"       # Black for headers
  white <- "#FFFFFF"       # White text
  faint_blue <- "#003366"  # Faint Blue for grid lines

  # List of available summaries
  available_summaries <- c(
    "Time Series OOS Testing Metrics Summary",
    "Time Series Validation Metrics Summary"
  )

  # Display Main Information
  cat("\n--- ML Metabacktest Results Summary ---\n")
  cat("\nNumber of ML Backtest Results:", length(object@ml_backtest_results), "\n")
  cat("\nConsolidated OOS Testing Metrics (Head):\n")
  print(utils::head(object@consolidated_oos_testing_metrics))
  cat("\nMean Validation Metrics (Head):\n")
  print(utils::head(object@mean_validation_metrics))


  # Handle summary_id == NULL
  if (is.null(summary_id)) {
    cat("\nAvailable summaries to display:\n")
    for (i in seq_along(available_summaries)) {
      cat(paste0(i, ": ", available_summaries[i], "\n"))
    }
    cat("\nPlease specify the 'summary_id' parameter to display a summary table.\n")
    cat("You can select a summary either by name or by number.\n")
    return(invisible(object))
  }

  # Determine if summary_id is numeric (index) or character (name)
  if (is.numeric(summary_id)) {
    if (summary_id >= 1 && summary_id <= length(available_summaries)) {
      summary_name <- available_summaries[summary_id]
    } else {
      stop("Invalid summary number. Please select a number between 1 and ", length(available_summaries), ".")
    }
  } else if (is.character(summary_id)) {
    if (summary_id %in% available_summaries) {
      summary_name <- summary_id
    } else {
      stop("Invalid 'summary_id' specified. Available options are:\n",
           paste(available_summaries, collapse = ", "))
    }
  } else {
    stop("'summary_id' must be either a string or a number corresponding to the summary.")
  }

  # Function to round numeric columns
  round_numeric_columns <- function(df, digits = 4) {
    numeric_cols <- sapply(df, is.numeric)
    df[numeric_cols] <- lapply(df[numeric_cols], round, digits = digits)
    return(df)
  }

  # Function to create and display a styled datatable
  display_table <- function(data_df, title) {
    if (is.null(data_df) || nrow(data_df) == 0) {
      cat("No data available for", title, "\n")
      return()
    }

    # Round numeric columns
    data_df <- round_numeric_columns(data_df, digits = 4)

    # Format the table
    data_dt <- DT::datatable(
      data_df,
      rownames = FALSE,
      extensions = c('FixedColumns'),
      options = list(
        scrollX = TRUE,
        scrollY = '400px',
        scrollCollapse = TRUE,
        fixedColumns = list(leftColumns = 1),
        dom = 't',
        ordering = FALSE
      ),
      class = 'cell-border stripe'
    )

    # Apply overall styling
    data_dt <- DT::formatStyle(
      data_dt,
      columns = names(data_df),
      backgroundColor = deep_navy,
      color = white
    )

    # Apply styling to the header via CSS
    css_styles <- paste0("
    table.dataTable thead th {
      background-color: ", black, " !important;
      color: ", white, " !important;
    }
    .dataTable {
      background-color: ", deep_navy, " !important;
      color: ", white, ";
    }
    table.dataTable tbody tr {
      background-color: ", deep_navy, " !important;
    }
    table.dataTable tbody td {
      border-color: #333333 !important;
    }
    .dataTables_wrapper {
      overflow-x: auto !important;
      overflow-y: auto !important;
    }
    ")

    # Add CSS to the table
    data_dt <- htmlwidgets::prependContent(
      data_dt,
      htmltools::tags$style(css_styles)
    )

    # Add title above the table
    data_dt <- htmlwidgets::prependContent(
      data_dt,
      htmltools::tags$h3(
        style = paste0("color: ", white, "; background-color: ", black, "; padding: 10px; margin-bottom: 10px;"),
        title
      )
    )

    # Display the table
    print(data_dt)
  }

  # Function to summarize time series metrics
  summarize_time_series_metrics <- function(metrics_list) {
    # Initialize an empty data frame
    summary_df <- data.frame(
      Metric = character(),
      Mean = numeric(),
      Median = numeric(),
      SD = numeric(),
      Min = numeric(),
      Max = numeric(),
      stringsAsFactors = FALSE
    )

    # Iterate over each metric's data frame
    for (metric_name in names(metrics_list)) {
      metric_df <- metrics_list[[metric_name]]

      # Check if metric_df is a data.frame
      if (!is.data.frame(metric_df)) {
        warning(paste("Metric", metric_name, "is not a data.frame. Skipping."))
        next
      }

      # Identify the value column (assuming it's the first non-Date column)
      value_columns <- setdiff(names(metric_df), "Date")
      if (length(value_columns) == 0) {
        warning(paste("No value columns found for metric", metric_name, ". Skipping."))
        next
      }

      # If multiple value columns exist, select the first numeric one
      value_col <- value_columns[which(sapply(metric_df[value_columns], is.numeric))[1]]
      if (is.na(value_col)) {
        warning(paste("No numeric value column found for metric", metric_name, ". Skipping."))
        next
      }

      values <- metric_df[[value_col]]

      # Compute summary statistics
      metric_summary <- data.frame(
        Metric = metric_name,
        Mean = mean(values, na.rm = TRUE),
        Median = median(values, na.rm = TRUE),
        SD = sd(values, na.rm = TRUE),
        Min = min(values, na.rm = TRUE),
        Max = max(values, na.rm = TRUE),
        stringsAsFactors = FALSE
      )

      # Append to summary_df
      summary_df <- rbind(summary_df, metric_summary)
    }

    return(summary_df)
  }

  # Generate and display the selected summary
  if (summary_name == "Time Series OOS Testing Metrics Summary") {
    # Summarize time_series_oos_testing_metrics
    summary_data <- summarize_time_series_metrics(object@time_series_oos_testing_metrics)

    # Display the table
    display_table(summary_data, "Time Series OOS Testing Metrics Summary")

  } else if (summary_name == "Time Series Validation Metrics Summary") {
    # Summarize time_series_validation_metrics
    summary_data <- summarize_time_series_metrics(object@time_series_validation_metrics)

    # Display the table
    display_table(summary_data, "Time Series Validation Metrics Summary")
  }

  invisible(object)  # Return the object invisibly
})


