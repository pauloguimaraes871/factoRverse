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

  # Prompt user for date filtering if a 'dates' column exists
  if ("dates" %in% names(object@data)) {
    filter_date_choice <- readline(prompt = "\nDo you want to filter by a specific date? (yes/no): ")
    if (tolower(trimws(filter_date_choice)) %in% c("yes", "y")) {
      date_input <- readline(prompt = "Enter the date (YYYY-MM-DD): ")
      date_input_parsed <- as.Date(date_input)
      if (is.na(date_input_parsed)) {
        stop("Invalid date format. Please enter date in YYYY-MM-DD format.")
      }
      original_row_count <- base::nrow(object@data)
      object@data <- object@data[as.Date(object@data$dates) == date_input_parsed, , drop = FALSE]
      filtered_row_count <- base::nrow(object@data)
      if (filtered_row_count == 0) {
        stop("No data available for the specified date.")
      } else {
        cat("Data filtered from", original_row_count, "to", filtered_row_count, "rows for date", date_input, "\n")
      }
    }
  } else {
    cat("\nNo 'dates' column in the data. Skipping date filtering.\n")
  }

  # List of available summaries
  available_summaries <- c(
    "Numeric Summary Table",
    "Tickers Frequency Table",
    "Categorical Variables Plots"
  )

  # Display Main Information always
  cat("\nMeta Dataframe Summary\n")
  cat("Meta Dataframe Name:", object@meta_dataframe_name, "\n")
  cat("Number of Rows:", nrow(object@data), "\n")
  cat("Number of Columns:", ncol(object@data), "\n")
  cat("Columns:", paste(names(object@data), collapse = ", "), "\n")

  if (is.null(summary_id)) {
    cat("\nPlease choose a summary to display:\n")
    for (i in seq_along(available_summaries)) {
      cat(paste0(i, ": ", available_summaries[i], "\n"))
    }
    selection <- readline(prompt = "Enter the number of your choice: ")
    summary_id <- as.numeric(selection)
    if (is.na(summary_id) || summary_id < 1 || summary_id > length(available_summaries)) {
      stop("Invalid selection.")
    }
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
          paste0(object@meta_dataframe_name, ': Summary of Numeric Variables')
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
          paste0(object@meta_dataframe_name, ': Summary of Tickers')
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
            title = paste(object@meta_dataframe_name, ": Frequency Plot for", col_name),
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


#' Summary Method for tickers_catalog Class
#'
#' Provides a detailed summary of a `tickers_catalog` object, allowing users to view
#' key statistics and filter the catalog slot interactively.
#'
#' @param object An instance of the `tickers_catalog` class.
#' @param summary_id A character string or numeric value specifying which summary to display.
#'   - By name: Options are:
#'     - "Catalog Overview"
#'   - By number: Provide a number corresponding to the summary.
#'   If `NULL` (default), the method lists available summaries.
#'
#' @return Invisibly returns the input `object`.
#' @export
setMethod("summary", "tickers_catalog", function(object, summary_id = NULL) {

  # Define colors
  deep_navy <- "#000033"
  black <- "#000000"
  white <- "#FFFFFF"

  # List of available summaries
  available_summaries <- c("Catalog Overview", "Yearly Summary")

  if (is.null(summary_id)) {
    cat("\nPlease choose a summary to display:\n")
    for (i in seq_along(available_summaries)) {
      cat(paste0(i, ": ", available_summaries[i], "\n"))
    }
    selection <- readline(prompt = "Enter the number of your choice: ")
    summary_id <- as.numeric(selection)
    if (is.na(summary_id) || summary_id < 1 || summary_id > length(available_summaries)) {
      stop("Invalid selection.")
    }
  }

  if (is.numeric(summary_id)) {
    if (summary_id == 1) {
      summary_name <- "Catalog Overview"
    } else if (summary_id == 2) {
      summary_name <- "Yearly Summary"
    } else {
      stop("Invalid summary number.")
    }
  } else if (is.character(summary_id)) {
    if (summary_id %in% available_summaries) {
      summary_name <- summary_id
    } else {
      stop("Invalid 'summary_id' specified. Available options are: ", paste(available_summaries, collapse = ", "))
    }
  } else {
    stop("'summary_id' must be either a string or a number corresponding to the summary.")
  }

  # Display catalog slot using DT with black column headers and search bar
  if (summary_name == "Catalog Overview") {
    catalog_table <- DT::datatable(
      object@catalog,
      rownames = FALSE,
      extensions = c('FixedColumns', 'Scroller'),
      options = list(
        scrollX = TRUE,
        scrollY = 400,
        scroller = TRUE,
        fixedColumns = list(leftColumns = 1),
        dom = 't<"top"f>',  # Adds a search bar
        ordering = FALSE,
        headerCallback = DT::JS("function(thead, data, start, end, display) {",
                                "  $(thead).find('th').css({'background-color': '#000000', 'color': 'white', 'font-weight': 'bold'});",
                                "}")
      ),
      class = 'cell-border stripe',
      caption = htmltools::tags$caption(
        style = paste0('caption-side: top; text-align: center; color: ', white,
                       '; background-color: ', black,
                       '; padding: 10px; margin-bottom: 10px; font-size: 18px; font-weight: bold;'),
        paste0(object@meta_dataframe_name, ': Catalog Overview')
      )
    ) %>%
      DT::formatStyle(
        columns = names(object@catalog),
        backgroundColor = deep_navy,
        color = white
      ) %>%
      DT::formatStyle(
        columns = names(object@catalog),
        target = "cell",
        backgroundColor = deep_navy,
        color = white,
        fontWeight = "bold"
      )

    print(catalog_table)
  }

  # Display summary of first & last quotes by year
  if (summary_name == "Yearly Summary") {

    # Extract year from date columns
    object@catalog <- dplyr::mutate(object@catalog,
                                    year_first = lubridate::year(object@catalog$tickers_first_quote),
                                    year_last = lubridate::year(object@catalog$tickers_last_quote))

    # Count occurrences of first quotes by year
    first_quotes_summary <- object@catalog %>%
      dplyr::filter(!is.na(year_first)) %>%
      dplyr::count(year_first, name = "first_quote_count")

    # Count occurrences of last quotes by year
    last_quotes_summary <- object@catalog %>%
      dplyr::filter(!is.na(year_last)) %>%
      dplyr::count(year_last, name = "last_quote_count")

    # Full join to ensure all years are present
    summary_table <- dplyr::full_join(first_quotes_summary, last_quotes_summary,
                                      by = c("year_first" = "year_last")) %>%
      dplyr::rename(year = year_first) %>%
      dplyr::mutate(across(everything(), ~ tidyr::replace_na(.x, 0))) %>%
      dplyr::arrange(year)

    # Convert 'year' to character before adding total row
    summary_table <- summary_table %>%
      dplyr::mutate(year = as.character(year))  # Convert year to character

    # Add a total row
    total_row <- data.frame(
      year = "Total",  # Now matches character type
      first_quote_count = sum(summary_table$first_quote_count),
      last_quote_count = sum(summary_table$last_quote_count)
    )

    # Combine with summary table
    summary_table <- dplyr::bind_rows(summary_table, total_row)


    summary_dt <- DT::datatable(
      summary_table,
      rownames = FALSE,
      extensions = c('FixedColumns', 'Scroller'),
      options = list(
        scrollX = TRUE,
        scrollY = 400,
        scroller = TRUE,
        fixedColumns = list(leftColumns = 1),
        dom = 't<"top"f>',  # Adds a search bar
        ordering = FALSE,
        headerCallback = DT::JS("function(thead, data, start, end, display) {",
                                "  $(thead).find('th').css({'background-color': '#000000', 'color': 'white', 'font-weight': 'bold'});",
                                "}")
      ),
      class = 'cell-border stripe',
      caption = htmltools::tags$caption(
        style = paste0('caption-side: top; text-align: center; color: ', white,
                       '; background-color: ', black,
                       '; padding: 10px; margin-bottom: 10px; font-size: 18px; font-weight: bold;'),
        "Summary of First & Last Quotes by Year"
      )
    ) %>%
      DT::formatStyle(
        columns = names(summary_table),
        backgroundColor = deep_navy,
        color = white
      ) %>%
      DT::formatStyle(
        columns = names(summary_table),
        target = "cell",
        backgroundColor = deep_navy,
        color = white,
        fontWeight = "bold"
      )

    print(summary_dt)
  }

  invisible(object)
})


#' @title Summary Method for meta_xts Class
#' @description Provides summary statistics for meta_xts objects, including numeric summaries,
#' yearly trends, series frequency tables, and (for returns_meta_xts) a performance metrics table.
#'
#' @param object An S4 object of class `meta_xts`, possibly a subclass like `returns_meta_xts`.
#' @param summary_id A numeric or character specifying which summary to display.
#'   If NULL, a menu is shown.
#' @param benchmark_returns_m_xts An optional \code{xts} object with benchmark returns (for active performance).
#' @param active_returns Logical. If TRUE, computes active returns relative to the benchmark.
#' @param ... Further arguments (not used).
#'
#' @return Invisibly returns the \code{object}, printing a styled table or prompt in the Viewer.
#'
#' @examples
#' \dontrun{
#' # If x is a generic meta_xts:
#' summary(x)  # numeric summary, yearly summary, or series frequency table
#'
#' # If x is a returns_meta_xts:
#' summary(x)  # includes the new 'Performance Metrics Table' if you choose it
#' }
#'
#' @export
setMethod("summary", "meta_xts", function(object, summary_id = NULL, benchmark_returns_m_xts = NULL, active_returns = FALSE, ...) {

  # Colors
  deep_navy <- "#000033"        # Deep Navy for data rows
  black <- "#000000"            # Black for headers and the title bar
  white <- "#FFFFFF"            # White text
  blue_bg <- "#001f3f"          # Blue background (if needed)

  # Base summaries
  base_summaries <- c(
    "Numeric Summary Table",
    "Numeric Summary Table by Year",
    "Series Frequency Table"
  )

  # If object is 'returns_meta_xts', add a new option:
  if (methods::is(object, "returns_meta_xts")) {
    available_summaries <- c(base_summaries, "Performance Metrics Table")
    if (!is.null(benchmark_returns_m_xts) && (!methods::is(benchmark_returns_m_xts, "returns_meta_xts"))){
      stop("The 'benchmark_returns_m_xts' argument must be a 'returns_meta_xts' object.")
    }
  } else {
    available_summaries <- base_summaries
  }


  # Display info about the meta_xts
  cat("\nMeta xts Summary\n")
  cat("Meta xts Name:", object@meta_xts_name, "\n")
  cat("Number of Rows:", nrow(object@data), "\n")
  cat("Number of Columns:", ncol(object@data), "\n")
  cat("Columns:", paste(names(object@data), collapse = ", "), "\n")

  # If no summary_id, prompt user
  if (is.null(summary_id)) {
    cat("\nPlease choose a summary to display:\n")
    for (i in seq_along(available_summaries)) {
      cat(paste0(i, ": ", available_summaries[i], "\n"))
    }
    selection <- readline(prompt = "Enter the number of your choice: ")
    summary_id <- as.numeric(selection)
    if (is.na(summary_id) || summary_id < 1 || summary_id > length(available_summaries)) {
      stop("Invalid selection.")
    }
  }

  # Prompt for active_returns
  if (summary_id == 4) {
    active_returns <- readline(prompt = "Compute active returns relative to the benchmark? (yes/no): ")
    if (active_returns == "yes") {
      if (is.null(benchmark_returns_m_xts)){
        stop("You must provide a 'benchmark_returns_m_xts' object to compute active returns.")
      }
      if (ncol(benchmark_returns_m_xts@data) > 1){
        stop("The 'benchmark_returns_m_xts' object must have only one column.")
      }
      active_returns <- TRUE
    } else {
      active_returns <- FALSE
    }
  }

  # Resolve summary name
  if (is.numeric(summary_id)) {
    if (summary_id >= 1 && summary_id <= length(available_summaries)) {
      summary_name <- available_summaries[summary_id]
    } else {
      stop("Invalid summary number. Must be between 1 and ", length(available_summaries), ".")
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


  # Convert xts -> data frame, add year for numeric summaries
  df_data <- as.data.frame(object@data)
  df_data$dates <- as.Date(zoo::index(object@data))
  df_data$year <- lubridate::year(df_data$dates)

  # Only numeric columns for numeric summaries
  numeric_cols <- sapply(df_data, is.numeric)
  numeric_data <- df_data[, numeric_cols, drop = FALSE]

  # Numeric Summary Table
  display_numeric_summary <- function() {
    if (ncol(numeric_data) == 0) {
      cat("\nNo numeric columns to summarize.\n")
      return()
    }

    # Build summary
    numeric_summary <- data.frame(
      Variable = names(numeric_data),
      NAs = sapply(numeric_data, function(col) sum(is.na(col))),
      Min = sapply(numeric_data, function(col) round(min(col, na.rm = TRUE), 4)),
      `1st Quartile` = sapply(numeric_data, function(col) round(quantile(col, 0.25, na.rm = TRUE), 4)),
      Median = sapply(numeric_data, function(col) round(median(col, na.rm = TRUE), 4)),
      Mean = sapply(numeric_data, function(col) round(mean(col, na.rm = TRUE), 4)),
      `3rd Quartile` = sapply(numeric_data, function(col) round(quantile(col, 0.75, na.rm = TRUE), 4)),
      Max = sapply(numeric_data, function(col) round(max(col, na.rm = TRUE), 4)),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    # Format the table with scroll bars and a title
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
        style = paste0(
          'caption-side: top; text-align: center; color: ', white,
          '; background-color: ', black,
          '; padding: 10px; margin-bottom: 10px; font-size: 18px; font-weight: bold;'
        ),
        # meta_xts_name + " : Summary of Numeric Variables"
        paste0(object@meta_xts_name, ": Summary of Numeric Variables")
      )
    ) %>%
      # Apply overall styling for data rows
      DT::formatStyle(
        columns = names(numeric_summary),
        backgroundColor = deep_navy,
        color = white
      )

    # Additional CSS for black headers, etc.
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
        background-color: ", deep_navy, " !important;  /* Deep Navy */
      }
      table.dataTable tbody td {
        border-color: #333333 !important;
      }
    ")

    # Prepend the extra CSS
    numeric_summary_formatted <- htmlwidgets::prependContent(
      numeric_summary_formatted,
      htmltools::tags$style(css_styles)
    )

    print(numeric_summary_formatted)
  }

  # Numeric Summary Table by Year (User Statistic)
  display_numeric_summary_by_year <- function() {
    if (ncol(numeric_data) == 0) {
      cat("\nNo numeric columns to summarize.\n")
      return()
    }

    cat("\nChoose a statistic to calculate (mean, median, sd, min, max):\n")
    stat_choice <- readline(prompt = "Enter your choice: ")
    valid_stats <- c("mean", "median", "sd", "min", "max")
    if (!stat_choice %in% valid_stats) {
      stop("Invalid statistic. Choose from: mean, median, sd, min, max.")
    }

    summary_by_year <- numeric_data %>%
      dplyr::mutate(year = df_data$year) %>%
      dplyr::group_by(year) %>%
      dplyr::summarize(dplyr::across(dplyr::everything(), match.fun(stat_choice), na.rm = TRUE)) %>%
      dplyr::ungroup()

    # Round except 'year'
    for (nm in names(summary_by_year)) {
      if (nm != "year") {
        summary_by_year[[nm]] <- round(summary_by_year[[nm]], 4)
      }
    }

    # Create the table
    summary_by_year_formatted <- DT::datatable(
      summary_by_year,
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
        style = paste0(
          'caption-side: top; text-align: center; color: ', white,
          '; background-color: ', black,
          '; padding: 10px; margin-bottom: 10px; font-size: 18px; font-weight: bold;'
        ),
        paste0(object@meta_xts_name, ": Numeric Summary by Year (", stat_choice, ")")
      )
    ) %>%
      DT::formatStyle(
        columns = names(summary_by_year),
        backgroundColor = deep_navy,
        color = white
      )

    # Extra CSS for black headers, etc.
    css_styles_by_year <- paste0("
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

    summary_by_year_formatted <- htmlwidgets::prependContent(
      summary_by_year_formatted,
      htmltools::tags$style(css_styles_by_year)
    )

    print(summary_by_year_formatted)
  }

  # Series Frequency Table
  display_series_frequency_table <- function() {

    # Summarize the number of non-NA obs per date
    freq_table <- df_data %>%
      dplyr::rowwise() %>%
      dplyr::mutate(Non_NA_Count = sum(!is.na(dplyr::c_across(-c(dates, year))))) %>%
      dplyr::ungroup() %>%
      dplyr::select(dates, Non_NA_Count)

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
        style = paste0(
          'caption-side: top; text-align: center; color: ', white,
          '; background-color: ', black,
          '; padding: 10px; margin-bottom: 10px; font-size: 18px; font-weight: bold;'
        ),
        paste0(object@meta_xts_name, ": Series Frequency Table")
      )
    ) %>%
      DT::formatStyle(
        columns = names(freq_table),
        backgroundColor = deep_navy,
        color = white
      )

    css_styles_freq <- paste0("
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

    freq_table_formatted <- htmlwidgets::prependContent(
      freq_table_formatted,
      htmltools::tags$style(css_styles_freq)
    )

    print(freq_table_formatted)
  }

  # Performance Metrics Table (for returns_meta_xts)
  display_performance_metrics_table <- function() {
    # Check if the object is actually returns_meta_xts
    if (!methods::is(object, "returns_meta_xts")) {
      cat("\nThis summary is only for returns_meta_xts objects.\n")
      return()
    }

    # "Wrap" create_performance_m_df
    performance_df <- create_performance_m_df(
      selected_backtest_returns_corrected_positions_m_xts_upd_ref = object@data,
      selected_market_factor_proxy_m_xts_upd_ref = if (is.null(benchmark_returns_m_xts)) NULL else benchmark_returns_m_xts@data,
      active_returns = active_returns,
      verbose = TRUE
    )

    # Convert to wide format with tickers as columns, ignoring 'id' and 'dates'
    # Rows = metrics, Columns = tickers
    performance_df_wide <- performance_df %>%
      dplyr::select(-id, -dates) %>%
      tidyr::pivot_longer(
        cols = -tickers,
        names_to = "metric",
        values_to = "value"
      ) %>%
      tidyr::pivot_wider(
        names_from = tickers,
        values_from = value
      ) %>%
      dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4)))

    # Build a DT exactly like the other tables (no finalize_table):
    table_title <- if (active_returns) {
      "Active Performance Metrics Table"
    } else {
      "Performance Metrics Table"
    }

    performance_dt <- DT::datatable(
      performance_df_wide,
      rownames = FALSE,
      extensions = c("FixedColumns", "Scroller"),
      options = list(
        scrollX = TRUE,
        scrollY = 400,
        scroller = TRUE,
        fixedColumns = list(leftColumns = 1),
        dom = "t",
        ordering = FALSE
      ),
      class = "cell-border stripe",
      caption = htmltools::tags$caption(
        style = paste0(
          'caption-side: top; text-align: center; color: ', white,
          '; background-color: ', black,
          '; padding: 10px; margin-bottom: 10px; font-size: 18px; font-weight: bold;'
        ),
        paste0(object@meta_xts_name, ": ", table_title)
      )
    ) %>%
      DT::formatStyle(
        columns = names(performance_df_wide),
        backgroundColor = deep_navy,
        color = white
      )

    # Same black header & navy background style
    css_styles_perf <- paste0("
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
    ")

    performance_dt <- htmlwidgets::prependContent(
      performance_dt,
      htmltools::tags$style(css_styles_perf)
    )


    print(performance_dt)
  }


  # Dispatcher
  if (summary_name == "Numeric Summary Table") {
    display_numeric_summary()
  } else if (summary_name == "Numeric Summary Table by Year") {
    display_numeric_summary_by_year()
  } else if (summary_name == "Series Frequency Table") {
    display_series_frequency_table()
  } else if (summary_name == "Performance Metrics Table") {
    display_performance_metrics_table()
  }

  invisible(object)
})


#' Summary Method for sb_metabacktest_config Class
#'
#' Produces an interactive table summarizing the counts of configurations by `sb_algorithm` and other parameters using the `DT` package.
#' Neural networks (`nn`) are grouped by the number of hidden layers, resulting in rows like `nn_1`, `nn_2`, etc.
#'
#' The table supports horizontal and vertical scrolling with the first column frozen and includes visual enhancements using specified colors.
#' Underscores in column headers are replaced with spaces.
#'
#' This version also summarizes information from any associated \code{ss_backtest_config} or \code{ss_backtest_workflow}:
#' \itemize{
#'   \item \code{model_structure}
#'   \item \code{p_correction_method}
#' }
#' For \code{ss_backtest_workflow}, we look at \code{sb_backtest_config@@ss_backtest_results@@ss_backtest_workflow}.
#'
#' @param object An `sb_metabacktest_config` object.
#' @param ... Additional arguments (not used).
#'
#' @return Invisibly returns a `DT` table object. This function is primarily called for its side effect of displaying the table.
#'
#' @examples
#' # Assuming you have an sb_metabacktest_config object named meta_config:
#' summary(meta_config)
#'
#' @export
setMethod("summary", "sb_metabacktest_config",
          function(object, ...) {

            cat("Summary of 'sb_metabacktest_config' object\n")
            cat("------------------------------\n")

            n_configs <- length(object@base_sb_backtest_configs)
            cat(sprintf("Number of Base SB backtest configurations: %d\n\n", n_configs))

            # Basic info
            training_sample_size <- unique(sapply(object@base_sb_backtest_configs, function(x) x@training_sample_size))
            cat(paste("Training sample size:", paste(training_sample_size, collapse = ","), "\n"))
            cat(paste("Training sample size (OLS):", paste(training_sample_size, collapse = ","), "\n"))

            rebalancing_months <- unique(sapply(object@base_sb_backtest_configs, function(x) x@rebalancing_months))
            cat(paste("Rebalancing months:", paste(rebalancing_months, collapse = ", "), "\n"))

            # If there are no configs, just return
            if (n_configs == 0) {
              invisible(NULL)
            } else {
              ###################################################################
              # 1) Collect unique sb_algorithms, grouping nn by # of hidden layers
              ###################################################################
              sb_algorithms <- unique(sapply(object@base_sb_backtest_configs, function(x) {
                if (x@sb_algorithm == "nn") {
                  num_hidden_layers <- length(x@keras_architecture_parameters@units)
                  paste0("nn_", num_hidden_layers)
                } else {
                  x@sb_algorithm
                }
              }))

              ###################################################################
              # 2) Collect possible options: tuning methods, custom objectives, etc.
              ###################################################################
              # 2a) Tuning Methods
              tuning_methods <- unique(unlist(sapply(object@base_sb_backtest_configs, function(x) {
                if (!is.null(x@tuning_strategy)) {
                  x@tuning_strategy@tuning_method
                } else {
                  NA
                }
              })))
              tuning_methods <- na.omit(tuning_methods)

              # 2b) Custom Objectives
              custom_objectives <- unique(sapply(object@base_sb_backtest_configs, function(x) x@custom_objective))
              custom_objectives <- na.omit(custom_objectives)

              # 2c) Chosen Eval Metrics
              chosen_eval_metrics <- unique(unlist(sapply(object@base_sb_backtest_configs, function(x) {
                if (!is.null(x@tuning_strategy)) {
                  x@tuning_strategy@chosen_eval_metric
                } else {
                  NA
                }
              })))
              chosen_eval_metrics <- na.omit(chosen_eval_metrics)

              ###################################################################
              # 3) Summarize info from any embedded ss_backtest_config or
              #    ss_backtest_workflow (found in x@ss_backtest_results).
              #
              # We look for:
              #   model_structure
              #   p_correction_method
              #
              # If there's no SS config or workflow, skip.
              ###################################################################
              # 3a) model_structures
              model_structures <- unique(unlist(sapply(object@base_sb_backtest_configs, function(x) {
                if (!is.null(x@ss_backtest_config)) {
                  # scenario (1)
                  x@ss_backtest_config@alpha_test_strategy@model_structure
                } else if (!is.null(x@ss_backtest_results) &&
                           !is.null(x@ss_backtest_results@ss_backtest_workflow)) {
                  # scenario (2) => check the workflow list
                  x@ss_backtest_results@ss_backtest_workflow$model_structure
                } else {
                  NA
                }
              })))
              model_structures <- na.omit(model_structures)

              # 3b) p_correction_methods
              p_correction_methods <- unique(unlist(sapply(object@base_sb_backtest_configs, function(x) {
                if (!is.null(x@ss_backtest_config)) {
                  x@ss_backtest_config@alpha_test_strategy@p_correction_method
                } else if (!is.null(x@ss_backtest_results) &&
                           !is.null(x@ss_backtest_results@ss_backtest_workflow)) {
                  x@ss_backtest_results@ss_backtest_workflow$p_correction_method
                } else {
                  NA
                }
              })))
              p_correction_methods <- na.omit(p_correction_methods)

              ###################################################################
              # 4) Define final columns to summarize, ordering them so that
              #    SS columns appear *after* validation_sample_size.
              ###################################################################
              # Categorical "counts" first:
              count_col_names <- c(
                tuning_methods,
                custom_objectives,
                chosen_eval_metrics
              )
              # Then numeric/quant param columns:
              quant_param_names <- c("huber_delta", "quantile_tau", "validation_sample_size")
              # Then SS columns (after validation_sample_size):
              ss_col_names <- c(
                model_structures,
                p_correction_methods
              )
              total_count_col <- "Algo_Count"

              # So final order is:
              col_names <- c("sb_algorithm", count_col_names, quant_param_names, ss_col_names, total_count_col)
              display_col_names <- gsub("_", " ", col_names)

              # Prepare a data.frame with one row per sb_algorithm + a "Total" row
              summary_df <- data.frame(
                matrix("", nrow = length(sb_algorithms) + 1, ncol = length(col_names)),
                stringsAsFactors = FALSE
              )
              names(summary_df) <- col_names
              summary_df$sb_algorithm <- c(sb_algorithms, "Total")

              # Helper to unify numeric ranges or single values
              get_range_or_value <- function(values) {
                values <- unique(na.omit(values))
                if (length(values) == 1) {
                  as.character(values)
                } else if (length(values) > 1) {
                  paste0(min(values), "-", max(values))
                } else {
                  NA
                }
              }

              ###################################################################
              # 5) Populate summary_df row by row for each sb_algorithm
              ###################################################################
              total_counts <- numeric(length(sb_algorithms))

              for (i in seq_along(sb_algorithms)) {
                algo <- sb_algorithms[i]
                # Filter the base_sb_backtest_configs for this algo
                configs <- object@base_sb_backtest_configs[sapply(object@base_sb_backtest_configs, function(x) {
                  if (x@sb_algorithm == "nn") {
                    num_hidden_layers <- length(x@keras_architecture_parameters@units)
                    paste0("nn_", num_hidden_layers) == algo
                  } else {
                    x@sb_algorithm == algo
                  }
                })]

                total_counts[i] <- length(configs)
                summary_df[i, total_count_col] <- total_counts[i]

                #------------------- Tuning Methods --------------------#
                tm_counts <- table(sapply(configs, function(x) {
                  if (!is.null(x@tuning_strategy)) x@tuning_strategy@tuning_method else NA
                }))
                for (tm in names(tm_counts)) {
                  summary_df[i, tm] <- as.numeric(tm_counts[tm])
                }

                #------------------- Custom Objectives -----------------#
                co_counts <- table(sapply(configs, function(x) x@custom_objective))
                for (co in names(co_counts)) {
                  summary_df[i, co] <- as.numeric(co_counts[co])
                }

                #------------------- Chosen Eval Metrics --------------#
                cem_counts <- table(sapply(configs, function(x) {
                  if (!is.null(x@tuning_strategy)) {
                    x@tuning_strategy@chosen_eval_metric
                  } else {
                    NA
                  }
                }))
                for (cem in names(cem_counts)) {
                  summary_df[i, cem] <- as.numeric(cem_counts[cem])
                }

                #------------------- Quantitative Parameters ----------#
                # huber_delta
                huber_deltas <- sapply(configs, function(x) x@huber_delta)
                summary_df[i, "huber_delta"] <- get_range_or_value(huber_deltas)

                # quantile_tau
                quantile_taus <- sapply(configs, function(x) x@quantile_tau)
                summary_df[i, "quantile_tau"] <- get_range_or_value(quantile_taus)

                # validation_sample_size
                val_sample_sizes <- sapply(configs, function(x) {
                  if (!is.null(x@tuning_strategy)) {
                    x@tuning_strategy@validation_sample_size
                  } else {
                    NA
                  }
                })
                summary_df[i, "validation_sample_size"] <- get_range_or_value(val_sample_sizes)

                #------------------- SS Summaries (after val_sample) --#
                # model_structure
                ms_counts <- table(sapply(configs, function(x) {
                  if (!is.null(x@ss_backtest_config)) {
                    x@ss_backtest_config@alpha_test_strategy@model_structure
                  } else if (!is.null(x@ss_backtest_results) &&
                             !is.null(x@ss_backtest_results@ss_backtest_workflow)) {
                    x@ss_backtest_results@ss_backtest_workflow$model_structure
                  } else {
                    NA
                  }
                }))
                for (ms in names(ms_counts)) {
                  summary_df[i, ms] <- as.numeric(ms_counts[ms])
                }

                # p_correction_method
                pc_counts <- table(sapply(configs, function(x) {
                  if (!is.null(x@ss_backtest_config)) {
                    x@ss_backtest_config@alpha_test_strategy@p_correction_method
                  } else if (!is.null(x@ss_backtest_results) &&
                             !is.null(x@ss_backtest_results@ss_backtest_workflow)) {
                    x@ss_backtest_results@ss_backtest_workflow$p_correction_method
                  } else {
                    NA
                  }
                }))
                for (pc in names(pc_counts)) {
                  summary_df[i, pc] <- as.numeric(pc_counts[pc])
                }
              }

              ###################################################################
              # 6) Fill in the "Total" row for all categorical columns
              ###################################################################
              count_columns <- c(
                tuning_methods,
                custom_objectives,
                chosen_eval_metrics,
                model_structures,
                p_correction_methods
              )
              for (col_name in count_columns) {
                col_vals <- as.numeric(summary_df[1:length(sb_algorithms), col_name])
                col_vals[is.na(col_vals)] <- 0
                summary_df[length(sb_algorithms) + 1, col_name] <- sum(col_vals)
              }
              summary_df[length(sb_algorithms) + 1, total_count_col] <- sum(total_counts)

              ###################################################################
              # 7) Summation row for numeric ranges
              ###################################################################
              # huber_delta
              all_huber_deltas <- sapply(object@base_sb_backtest_configs, function(x) x@huber_delta)
              summary_df[length(sb_algorithms) + 1, "huber_delta"] <- get_range_or_value(all_huber_deltas)

              # quantile_tau
              all_quantile_taus <- sapply(object@base_sb_backtest_configs, function(x) x@quantile_tau)
              summary_df[length(sb_algorithms) + 1, "quantile_tau"] <- get_range_or_value(all_quantile_taus)

              # validation_sample_size
              all_val_sample_sizes <- sapply(object@base_sb_backtest_configs, function(x) {
                if (!is.null(x@tuning_strategy)) x@tuning_strategy@validation_sample_size else NA
              })
              summary_df[length(sb_algorithms) + 1, "validation_sample_size"] <-
                get_range_or_value(all_val_sample_sizes)

              # Replace empty strings with NA
              summary_df[summary_df == ""] <- NA

              # Replace NAs with 0 in all "count" columns
              for (col_name in c(count_columns, total_count_col)) {
                summary_df[[col_name]][is.na(summary_df[[col_name]])] <- 0
                summary_df[[col_name]] <- as.numeric(summary_df[[col_name]])
              }

              # Convert numeric param columns to character
              for (col_name in quant_param_names) {
                summary_df[[col_name]] <- as.character(summary_df[[col_name]])
              }

              ###################################################################
              # 8) DataTable styling
              ###################################################################
              columnDefs <- list()
              def_index <- 1

              # Helper to get zero-based indices for a given set of names
              get_zero_based_indices <- function(the_names) {
                pos <- match(the_names, col_names)
                pos <- pos[!is.na(pos)]
                pos - 1L  # zero-based for DT
              }

              # Tuning + custom obj + eval metrics
              tm_co_cem_idx <- get_zero_based_indices(c(tuning_methods, custom_objectives, chosen_eval_metrics))
              if (length(tm_co_cem_idx) > 0) {
                # color cluster #1
                columnDefs[[def_index]] <- list(targets = tm_co_cem_idx, className = "cluster1")
                def_index <- def_index + 1
                # rightmost boundary
                columnDefs[[def_index]] <- list(targets = max(tm_co_cem_idx), className = "cluster1_end")
                def_index <- def_index + 1
              }

              # Quant param indices
              quant_idx <- get_zero_based_indices(quant_param_names)
              if (length(quant_idx) > 0) {
                # color cluster #2
                columnDefs[[def_index]] <- list(targets = quant_idx, className = "cluster2")
                def_index <- def_index + 1
                # boundary
                columnDefs[[def_index]] <- list(targets = max(quant_idx), className = "cluster2_end")
                def_index <- def_index + 1
              }

              # SS columns
              ss_idx <- get_zero_based_indices(c(model_structures, p_correction_methods))
              if (length(ss_idx) > 0) {
                # color cluster #3
                columnDefs[[def_index]] <- list(targets = ss_idx, className = "cluster3")
                def_index <- def_index + 1
                # boundary
                columnDefs[[def_index]] <- list(targets = max(ss_idx), className = "cluster3_end")
                def_index <- def_index + 1
              }

              # total_count col
              total_count_0 <- match(total_count_col, col_names) - 1
              columnDefs[[def_index]] <- list(targets = total_count_0, className = "total_count_col")

              # Build the DT
              dt_table <- DT::datatable(
                summary_df,
                rownames = FALSE,
                colnames = display_col_names,
                extensions = c("FixedColumns", "Scroller"),
                options = list(
                  scrollX = TRUE,
                  scrollY = 400,
                  scroller = TRUE,
                  fixedColumns = list(leftColumns = 1),
                  columnDefs = columnDefs,
                  dom = "t",
                  ordering = FALSE
                ),
                class = "cell-border stripe",
                caption = htmltools::tags$caption(
                  style = "caption-side: top; text-align: center; color: #FFFFFF; background-color: #000000;
                           padding: 10px; margin-bottom: 10px; font-size: 18px; font-weight: bold;",
                  "Summary of Base SB Backtesting Configurations"
                )
              )

              # Format styles
              dt_table <- dt_table %>%
                DT::formatStyle(
                  columns = "sb_algorithm",
                  fontWeight = "bold",
                  backgroundColor = "#000033",
                  color = "#FFFFFF"
                ) %>%
                DT::formatStyle(
                  columns = names(summary_df),
                  valueColumns = "sb_algorithm",
                  backgroundColor = DT::styleEqual("Total", "#000000"),
                  color = DT::styleEqual("Total", "#FFFFFF")
                )

              # Custom CSS
              css_styles <- paste0("
                .dataTable {
                  background-color: #000033 !important;
                  color: #FFFFFF;
                }
                table.dataTable tbody tr {
                  background-color: #000033 !important;
                }
                table.dataTable tbody td {
                  border-color: #333333 !important;
                }
                /* Clusters */
                .cluster1 {
                  background-color: rgba(106, 13, 173, 0.2);
                }
                .cluster1_end {
                  border-right: 2px solid #FFFFFF !important;
                }
                .cluster2 {
                  background-color: rgba(0, 191, 255, 0.2);
                }
                .cluster2_end {
                  border-right: 2px solid #FFFFFF !important;
                }
                .cluster3 {
                  background-color: rgba(255, 105, 180, 0.2);
                }
                .cluster3_end {
                  border-right: 2px solid #FFFFFF !important;
                }
                /* Total col */
                .total_count_col {
                  border-right: 2px solid #FFFFFF !important;
                  background-color: #000033 !important;
                  color: #FFFFFF !important;
                }
                thead {
                  background-color: #000000 !important;
                  color: #FFFFFF !important;
                }
                table.dataTable tbody tr:last-child {
                  background-color: #000000 !important;
                  color: #FFFFFF !important;
                }
              ")
              dt_table <- htmlwidgets::prependContent(dt_table, htmltools::tags$style(css_styles))

              # Print the DT
              print(dt_table)

              invisible(TRUE)
            }
          })



#' @title Summary Method for sb_backtest_results Class
#' @description Provides a detailed summary of an `sb_backtest_results` object.
#' Users can select which summary table to display by specifying the `summary_id` parameter,
#' either by name or by number.
#' The summary includes interactive tables styled using the `DT` package and leverages
#' the summary method of the `meta_dataframe` class for the `oos_sb_outputs_m_df` component.
#'
#' @param object An object of class `sb_backtest_results`.
#' @param summary_id A character string or numeric value specifying which table to display.
#'   - By name: Options are:
#'     - `"OOS_SB_Outputs_Summary"` (Delegates to `meta_dataframe` summary)
#'     - `"OOS_Testing_Eval_Metrics"`
#'     - `"Chosen_Eval_Metric_Validation"`
#'   - By number: Provide a number corresponding to the table (as listed when `summary_id` is `NULL`).
#'   If `NULL` (default), the method lists available tables.
#' @return Invisibly returns the input `object`.
#' @importFrom methods setMethod
#' @export
setMethod("summary", "sb_backtest_results", function(object, summary_id = NULL) {

  # Define colors for styling
  deep_navy <- "#000033"   # Deep Navy for data rows
  black <- "#000000"       # Black for headers
  white <- "#FFFFFF"       # White for headers and text

  # List of available tables
  available_tables <- c(
    "OOS_SB_Outputs_Summary",        # Delegates to meta_dataframe summary
    "OOS_Testing_Eval_Metrics",
    "Chosen_Eval_Metric_Validation",
    "Feature_Importance"
  )

  # Display Main Information always
  sb_backtest_workflow <- object@sb_backtest_workflow[[length(object@sb_backtest_workflow)]]
  cat("Backtest Identifier:", object@backtest_identifier, "\n")
  cat("SB Algorithm:", sb_backtest_workflow$sb_algorithm, "\n")
  cat("Final Model Object Class:", object@final_sb_model@model_class, "\n")
  cat("Custom Objective:", sb_backtest_workflow$custom_objective, "\n")
  if(!sb_backtest_workflow$sb_algorithm %in% c("ols", "sw", "ew", "rp", "mvo", "custom_weights")) cat("Chosen Evaluation Metric:", sb_backtest_workflow$chosen_eval_metric, "\n")
  cat("Testing Sample Dates:", format(as.Date(sb_backtest_workflow$dates_testing_sample), "%d-%m-%Y"), "\n")
  cat("Rebalancing Dates:", format(as.Date(sb_backtest_workflow$rebalance_dates), "%d-%m-%Y"), "\n")

  if (is.null(summary_id)) {
    cat("\nPlease choose a table to display:\n")
    for (i in seq_along(available_tables)) {
      cat(paste0(i, ": ", available_tables[i], "\n"))
    }
    selection <- readline(prompt = "Enter the number of your choice: ")
    summary_id <- as.numeric(selection)
    if (is.na(summary_id) || summary_id < 1 || summary_id > length(available_tables)) {
      stop("Invalid selection.")
    }
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
  if (table_name == "OOS_SB_Outputs_Summary") {
    # Delegate to the meta_dataframe summary method
    if (!is.null(object@oos_sb_outputs_m_df)) {
      cat("\nDisplaying Summary for oos_sb_outputs_m_df:\n")
      summary(object@oos_sb_outputs_m_df)
    } else {
      cat("oos_sb_outputs_m_df is not specified.\n")
    }

  } else if (table_name == "OOS_Testing_Eval_Metrics") {
    # Existing logic for OOS_Testing_Eval_Metrics
    if (!is.null(object@oos_testing_eval_metrics_m_xts) && length(object@oos_testing_eval_metrics_m_xts) > 0) {
      # Convert to data.frame
      oos_testing_metrics <- object@oos_testing_eval_metrics_m_xts@data %>% as.data.frame() %>%
        dplyr::mutate(Date = zoo::index(object@oos_testing_eval_metrics_m_xts@data)) %>% tidyr::pivot_longer(cols = -Date, names_to = "Metric", values_to = "value")

      # Summarize evaluation metrics
      oos_testing_summary <- oos_testing_metrics %>%
        dplyr::group_by(Metric) %>%
        dplyr::summarise(
          Mean = mean(value, na.rm = TRUE),
          Median = median(value, na.rm = TRUE),
          SD = sd(value, na.rm = TRUE),
          Min = min(value, na.rm = TRUE),
          Max = max(value, na.rm = TRUE),
          .groups = 'drop'
        )

      # Display the table
      display_table(oos_testing_summary, "OOS Testing Evaluation Metrics Summary")

    } else {
      cat("OOS Testing Evaluation Metrics not specified or empty.\n")
    }

  } else if (table_name == "Chosen_Eval_Metric_Validation") {
    # Existing logic for Chosen_Eval_Metric_Validation
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
      sb_algorithm <- object@sb_backtest_workflow$sb_algorithm

      if (sb_algorithm == "glmnet") {
        chosen_eval_metric_validation_df$hyperparameters_combination <- paste(
          chosen_eval_metric_validation_df$alpha,
          chosen_eval_metric_validation_df$lambda.min.ratio,
          sep = "_"
        )
      } else if (sb_algorithm == "rf") {
        chosen_eval_metric_validation_df$hyperparameters_combination <- paste(
          chosen_eval_metric_validation_df$mtry,
          chosen_eval_metric_validation_df$num.trees,
          chosen_eval_metric_validation_df$max.depth,
          chosen_eval_metric_validation_df$min.bucket,
          sep = "_"
        )
      } else if (sb_algorithm == "xgb") {
        chosen_eval_metric_validation_df$hyperparameters_combination <- paste(
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
      } else if (sb_algorithm == "nn") {
        chosen_eval_metric_validation_df$hyperparameters_combination <- paste(
          chosen_eval_metric_validation_df$regularizer_l1,
          chosen_eval_metric_validation_df$regularizer_l2,
          chosen_eval_metric_validation_df$droprate,
          chosen_eval_metric_validation_df$lr,
          sep = "_"
        )
      } else {
        # For other algorithms, concatenate all hyperparameters
        chosen_eval_metric_validation_df$hyperparameters_combination <- do.call(paste, c(chosen_eval_metric_validation_df[hyperparameter_columns], sep = "_"))
      }

      # Summarize the chosen evaluation metric by hyperparameter combinations
      chosen_eval_metric_validation_summary <- chosen_eval_metric_validation_df %>%
        dplyr::group_by(hyperparameters_combination) %>%
        dplyr::summarise(
          Mean_Chosen_Eval_Metric = mean(chosen_eval_metric, na.rm = TRUE),
          Median_Chosen_Eval_Metric = median(chosen_eval_metric, na.rm = TRUE),
          Q25 = stats::quantile(chosen_eval_metric, 0.25, na.rm = TRUE),
          Q75 = stats::quantile(chosen_eval_metric, 0.75, na.rm = TRUE),
          Max = max(chosen_eval_metric, na.rm = TRUE),
          Min = min(chosen_eval_metric, na.rm = TRUE),
          .groups = 'drop'
        )

      # Display the table
      display_table(chosen_eval_metric_validation_summary, "Chosen Evaluation Metric Validation Summary")

    } else {
      cat("Chosen Evaluation Metric Validation not specified or empty.\n")
    }
  } else if (table_name == "Feature_Importance"){
    # Delegate to the meta_dataframe summary method
    if (!is.null(object@feature_importance_m_df)) {
      cat("\nDisplaying Summary for feature_importance_m_df:\n")

      #Summarize
      feature_importance_summary <- object@feature_importance_m_df@data %>% dplyr::group_by(tickers) %>%
        dplyr::summarise(
          Mean_Feature_Imp = mean(normalized_importance, na.rm = TRUE),
          Median_Feature_Imp = median(normalized_importance, na.rm = TRUE),
          Q25 = stats::quantile(normalized_importance, 0.25, na.rm = TRUE),
          Q75 = stats::quantile(normalized_importance, 0.75, na.rm = TRUE),
          Max = max(normalized_importance, na.rm = TRUE),
          Min = min(normalized_importance, na.rm = TRUE),
          .groups = 'drop'
        )


      #Get data for last model
      last_model_feature_importance <- object@final_feature_importance_m_d_ref@data %>%
        dplyr::select(tickers, normalized_importance) %>%
        dplyr::rename(Final_Feature_Imp = "normalized_importance")

      feature_importance_summary <- dplyr::left_join(feature_importance_summary, last_model_feature_importance, by = "tickers")

      # Display the table
      display_table(feature_importance_summary, "Feature Importance Summary")


    } else {
      cat("feature_importance_m_df is not specified.\n")
    }

  }

  invisible(object)  # Return the object invisibly
})

#' @title Summary Method for sb_metabacktest_results Class
#' @description Provides a detailed summary of an `sb_metabacktest_results` object.
#' Users can select which summary table to display by specifying the `summary_id` parameter,
#' either by name or by number.
#' The summary includes interactive tables styled using the `DT` package.
#'
#' @param object An object of class `sb_metabacktest_results`.
#' @param summary_id A character string or numeric value specifying which table to display.
#'   - By name: Options are:
#'     - `"Consolidated_OOS_Testing_Metrics"`
#'     - `"Mean_Validation_Metrics"`
#'     - `"Time_Series_OOS_Testing_Metrics"`
#'     - `"Time_Series_Validation_Metrics"`
#'     - `"Base_Learners_OOS_Predictions"`
#'   - By number: Provide a number corresponding to the table (as listed when `summary_id` is `NULL`).
#'   If `NULL` (default), the method lists available tables.
#' @return Invisibly returns the input `object`.
#' @importFrom methods setMethod
#' @export
setMethod("summary", "sb_metabacktest_results", function(object, summary_id = NULL, which_backtest_results = NULL) {

  # Define colors for styling
  deep_navy <- "#000033"   # Deep Navy for data rows
  black <- "#000000"       # Black for headers
  white <- "#FFFFFF"       # White text

  # Ask if intention is to summarize underlying sb_backtest_results
  available_objects <- c("sb_metabacktest_results", "meta_learner_sb_backtest_results", "base_learners_sb_backtest_results")
  if (is.null(which_backtest_results)) {
    cat("Which object results do you want to summarize?\n")
    for (i in seq_along(available_objects)) {
      cat(paste0(i, ": ", available_objects[i], "\n"))
    }
    selection <- readline(prompt = "Enter the number of your choice: ")
    which_backtest_results <- as.numeric(selection)
    if (is.na(which_backtest_results) || which_backtest_results < 1) {
      stop("Invalid selection.")
    }
  }

    ## Call Appropriate Method
    if (available_objects[which_backtest_results] == "meta_learner_sb_backtest_results"){
      summary(object@meta_sb_backtest_results)
      return()
    }
    if (available_objects[which_backtest_results] == "base_learners_sb_backtest_results"){
      cat("Which base learner do you want?\n")
      available_base_learners <- names(object@base_sb_backtest_results_list)
      for (i in seq_along(available_base_learners)) {
        cat(paste0(i, ": ", available_base_learners[i], "\n"))
      }
      selection <- readline(prompt = "Enter the number of your choice: ")
      chosen_base_learner <- as.numeric(selection)
      if (is.na(chosen_base_learner) || chosen_base_learner < 1) {
        stop("Invalid selection.")
      }
      summary(object@base_sb_backtest_results_list[[chosen_base_learner]])
      return()
    }


  # List of available tables
  available_tables <- c(
    "Combined_OOS_Testing_Metrics",
    "Mean_Validation_Metrics",
    "Time_Series_OOS_Testing_Metrics",
    "Time_Series_Validation_Metrics",
    "Base_Learners_OOS_Predictions"
  )

  # Display Main Information always
  cat("Backtest Identifier:", object@backtest_identifier, "\n")
  cat("\nBase Learners Algorithms:\n")
  base_learner_algorithms <- sapply(object@base_sb_backtest_results_list, function(x) x@sb_backtest_workflow$sb_algorithm)
  cat(paste0("- ", base_learner_algorithms), sep = "\n")
  cat("\nMeta Learners Algorithm:\n")
  meta_learner_algorithm <- object@meta_sb_backtest_results@sb_backtest_workflow$sb_algorithm
  cat(meta_learner_algorithm,"\n")

  if (is.null(summary_id)) {
    cat("\nPlease choose a table to display:\n")
    for (i in seq_along(available_tables)) {
      cat(paste0(i, ": ", available_tables[i], "\n"))
    }
    selection <- readline(prompt = "Enter the number of your choice: ")
    summary_id <- as.numeric(selection)
    if (is.na(summary_id) || summary_id < 1 || summary_id > length(available_tables)) {
      stop("Invalid selection.")
    }
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
  display_table <- function(data_df, title, legend = NULL) {
    if (is.null(data_df) || nrow(data_df) == 0) {
      cat("No data available for", title, "\n")
      return()
    }

    # Round numeric columns
    data_df <- round_numeric_columns(data_df, digits = 4)

    # If a legend is provided, replace the 'Backtest' column values with numeric labels
    if (!is.null(legend)) {
      # Ensure 'Backtest' column exists
      if ("Backtest" %in% names(data_df)) {
        # Replace 'Backtest' values with numbers
        data_df$Backtest <- legend$labels[data_df$Backtest]
      }
    }

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

    # Display the legend if provided
    if (!is.null(legend) && !legend$printed) {
      legend_text <- paste(legend$labels, ": ", legend$backtest_ids, collapse = "\n")
      cat("\nLegend:\n")
      cat(legend_text, "\n\n")
      legend$printed <- TRUE  # Mark legend as printed
    }

    # Display the table
    print(data_dt)
  }

  # Prepare data based on the selected table
  if (table_name == "Combined_OOS_Testing_Metrics") {
    # Extract the data
    consolidated_metrics <- object@combined_oos_testing_metrics

    # Collect all unique sb_tickers identifiers from both tables
    all_backtests <- unique(unlist(lapply(consolidated_metrics, function(df) df$sb_backtest)))
    labels <- seq_along(all_backtests)
    legend <- list(
      backtest_ids = all_backtests,
      labels = setNames(labels, all_backtests),
      printed = FALSE  # Track if legend has been printed
    )

    # Display both full periods and common dates metrics
    for (metric_name in names(consolidated_metrics)) {
      data_df <- consolidated_metrics[[metric_name]]

      # Replace 'sb_backtest' values with labels
      data_df$sb_backtest <- legend$labels[data_df$sb_backtest]

      display_table(data_df, paste("Combined OOS Testing Metrics -", metric_name), legend)
    }

  } else if (table_name == "Mean_Validation_Metrics") {
    data_df <- object@mean_validation_metrics

    # Replace long sb_backtest identifiers with labels
    if ("sb_backtest" %in% names(data_df)) {
      all_backtests <- unique(data_df$sb_backtest)
      labels <- seq_along(all_backtests)
      legend <- list(
        backtest_ids = all_backtests,
        labels = setNames(labels, all_backtests),
        printed = FALSE
      )
      data_df$sb_backtest <- legend$labels[data_df$sb_backtest]
    } else {
      legend <- NULL
    }

    display_table(data_df, "Mean Validation Metrics", legend)

  } else if (table_name == "Time_Series_OOS_Testing_Metrics") {
    # This is a list of data frames for each metric over time
    metrics_list <- object@time_series_oos_testing_metrics

    # Collect all unique backtest identifiers from all metrics
    all_backtests <- unique(unlist(lapply(metrics_list, function(xts) names(xts@data))))
    labels <- seq_along(all_backtests)
    legend <- list(
      backtest_ids = all_backtests,
      labels = setNames(labels, all_backtests),
      printed = FALSE
    )

    # Display the legend once
    cat("\nLegend:\n")
    legend_text <- paste(labels, ": ", all_backtests, collapse = "\n")
    cat(legend_text, "\n\n")

    for (metric_name in names(metrics_list)) {
      data_df <- metrics_list[[metric_name]]@data %>% as.data.frame()
      data_df$dates <- zoo::index(metrics_list[[metric_name]]@data)
      data_df <- data_df[, c("dates", setdiff(names(data_df), "dates"))]

      # Replace Backtest column names with labels
      colnames(data_df)[-1] <- legend$labels[colnames(data_df)[-1]]

      display_table(data_df, paste("Time Series OOS Testing Metrics -", metric_name))
    }

  } else if (table_name == "Time_Series_Validation_Metrics") {
    # Similarly handle time series validation metrics
    metrics_list <- object@time_series_validation_metrics

    # Collect all unique Backtest identifiers from all metrics
    all_backtests <- unique(unlist(lapply(metrics_list, function(xts) names(xts@data))))
    labels <- seq_along(all_backtests)
    legend <- list(
      backtest_ids = all_backtests,
      labels = setNames(labels, all_backtests),
      printed = FALSE
    )

    # Display the legend once
    cat("\nLegend:\n")
    legend_text <- paste(labels, ": ", all_backtests, collapse = "\n")
    cat(legend_text, "\n\n")

    for (metric_name in names(metrics_list)) {
      data_df <- metrics_list[[metric_name]]@data %>% as.data.frame()
      data_df$dates <- zoo::index(metrics_list[[metric_name]]@data) %>% as.Date()
      data_df <- data_df[, c("dates", setdiff(names(data_df), "dates"))]

      # Replace Backtest column names with labels
      colnames(data_df)[-1] <- legend$labels[colnames(data_df)[-1]]

      display_table(data_df, paste("Time Series Validation Metrics -", metric_name))
    }

  } else if (table_name == "Base_Learners_OOS_Predictions") {

    # base_learners_oos_predictions_meta_dataframe is of class meta_dataframe
    summary(object@base_learners_oos_predictions_m_df)
  }

  invisible(object)  # Return the object invisibly
})


#' @title Summary Method for ss_backtest_results Class
#' @description Provides a detailed summary of an `ss_backtest_results` object.
#' Users can select which summary table to display by specifying the `summary_id` parameter.
#' The summary includes interactive tables styled using the `DT` package.
#'
#' @param object An object of class `ss_backtest_results`.
#' @param summary_id A character string or numeric value specifying which table to display.
#' @return Invisibly returns the input `object`.
#' @export
setMethod("summary", "ss_backtest_results", function(object, summary_id = NULL) {
  # Define colors
  deep_navy <- "#000033"
  black <- "#000000"
  white <- "#FFFFFF"

  # Available tables
  available_tables <- c(
    "Eligibility_Count",
    "Theme_Eligibility_Proportion",
    "Eligibility_Over_Time",
    "Metric_Rate_of_Change",
    "Metrics_By_Theme",
    "Metrics_By_Eligibility",
    "Top_Signals",
    "Top_Themes"
  )

  # Print summary header
  cat("==============================\n")
  cat("Signal Selection Backtest Results Summary\n")
  cat("==============================\n\n")
  cat("Backtest Config Name:", object@backtest_identifier, "\n")

  # If no summary_id provided, prompt the user to select
  if (is.null(summary_id)) {
    cat("\nPlease choose a table to display:\n")
    for (i in seq_along(available_tables)) {
      cat(paste0(i, ": ", available_tables[i], "\n"))
    }
    selection <- readline(prompt = "Enter the number of your choice: ")
    summary_id <- as.numeric(selection)
    if (is.na(summary_id) || summary_id < 1 || summary_id > length(available_tables)) {
      stop("Invalid selection.")
    }
  }

  # Resolve summary_id to table name
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

  # Helper: Round numeric columns
  round_numeric_columns <- function(df, digits = 4) {
    numeric_cols <- sapply(df, is.numeric)
    df[numeric_cols] <- lapply(df[numeric_cols], round, digits = digits)
    return(df)
  }

  # Helper: Display data table with styling
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


  # Data extraction
  signal_universe_df <- object@signal_universe_m_df@data %>% dplyr::filter(theme != "forced")
  final_signal_universe_df <- object@final_signal_universe_m_d_ref@data %>% dplyr::filter(theme != "forced")

  # Table logic
  if (table_name == "Eligibility_Count") {
    eligibility_count <- dplyr::group_by(signal_universe_df, tickers, theme) %>%
      dplyr::summarise(
        times_eligible = sum(is_eligible),
        total_periods = dplyr::n(),
        proportion_eligible = times_eligible / total_periods,
        .groups = 'drop'
      )
    display_table(eligibility_count, "Eligibility Count by Ticker and Theme")

  } else if (table_name == "Theme_Eligibility_Proportion") {
    theme_eligibility_total <- dplyr::group_by(signal_universe_df, theme) %>%
      dplyr::summarise(
        total_periods = dplyr::n_distinct(dates),
        total_eligible = sum(is_eligible),
        max_possible = dplyr::n_distinct(tickers) * total_periods,
        overall_proportion_eligible = total_eligible / max_possible,
        .groups = 'drop'
      )
    display_table(theme_eligibility_total, "Proportion of Eligible Tickers by Theme")

  } else if (table_name == "Eligibility_Over_Time") {
    eligibility_over_time <- dplyr::group_by(signal_universe_df, dates) %>%
      dplyr::summarise(
        total_tickers = dplyr::n_distinct(tickers),
        eligible_tickers = sum(is_eligible),
        proportion_eligible = eligible_tickers / total_tickers,
        .groups = 'drop'
      )
    display_table(eligibility_over_time, "Eligibility Over Time")

  } else if (table_name == "Metric_Rate_of_Change") {
    first_date <- min(signal_universe_df$dates)
    last_date <- max(signal_universe_df$dates)

    data_first <- dplyr::filter(signal_universe_df, dates == first_date)
    data_last <- dplyr::filter(signal_universe_df, dates == last_date)

    metric_columns <- setdiff(names(signal_universe_df), c("id", "tickers", "dates", "theme", "is_eligible"))
    metric_columns <- metric_columns[sapply(signal_universe_df[metric_columns], is.numeric)]

    data_merged <- dplyr::inner_join(
      data_first[, c("tickers", metric_columns)],
      data_last[, c("tickers", metric_columns)],
      by = "tickers",
      suffix = c("_first", "_last")
    )

    for (metric in metric_columns) {
      data_merged[[paste0(metric, "_rate_of_change")]] <- (data_merged[[paste0(metric, "_last")]] - data_merged[[paste0(metric, "_first")]]) / abs(data_merged[[paste0(metric, "_first")]])
    }

    rate_of_change_columns <- grep("_rate_of_change$", names(data_merged), value = TRUE)
    rate_of_change_df <- data_merged[, c("tickers", rate_of_change_columns)]
    display_table(rate_of_change_df, "Rate of Change of Metrics between First and Last Periods")

  } else if (table_name == "Metrics_By_Theme") {
    known_columns <- c("id", "tickers", "dates", "theme", "is_eligible")
    metric_columns <- setdiff(names(final_signal_universe_df), known_columns)
    metrics_by_theme <- final_signal_universe_df %>%
      dplyr::group_by(theme) %>%
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(metric_columns),
          list(mean = ~mean(. , na.rm = TRUE), sd = ~stats::sd(. , na.rm = TRUE)),
          .names = "{.col}_{.fn}"
        ),
        .groups = 'drop'
      )
    display_table(metrics_by_theme, "Metrics Summary by Theme")

  } else if (table_name == "Metrics_By_Eligibility") {
    # Summarise metrics by eligibility (two categories)
    known_columns <- c("id", "tickers", "dates", "theme", "is_eligible")
    metric_columns <- setdiff(names(final_signal_universe_df), known_columns)
    metrics_by_eligibility <- final_signal_universe_df %>%
      dplyr::group_by(is_eligible) %>%
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(metric_columns),
          list(mean = ~mean(. , na.rm = TRUE), sd = ~stats::sd(. , na.rm = TRUE)),
          .names = "{.col}_{.fn}"
        ),
        .groups = 'drop'
      )
    display_table(metrics_by_eligibility, "Metrics Summary by Eligibility")

  } else if (table_name == "Top_Signals") {
    ss_backtest_workflow <- object@ss_backtest_workflow[[length(object@ss_backtest_workflow)]]
    # Top signals by avg_ir, including alpha_t_stat
    if(ss_backtest_workflow$active_returns){
      if(ss_backtest_workflow$p_correction_method == "bayesian"){
        #Bayesian
        top_signals <- final_signal_universe_df %>%
          dplyr::group_by(tickers) %>%
          dplyr::summarise(
            avg_ir = mean(info_ratio, na.rm = TRUE),
            avg_alpha = mean(individual_alpha, na.rm = TRUE),
            alpha_t_stat = mean(alpha_t_stat, na.rm = TRUE),
            avg_posterior_alpha = mean(posterior_individual_alpha, na.rm = TRUE),
            avg_alpha_t_stat = mean(posterior_alpha_t_stat, na.rm = TRUE),
            .groups = 'drop'
          ) %>%
          dplyr::arrange(dplyr::desc(avg_ir)) %>%
          dplyr::slice_head(n = 5)
        display_table(top_signals, "Top Signals by Average Information Ratio")
      } else {
        #Frequentist
        if(ss_backtest_workflow$model_structure == "no_pooled"){
          ###No Pooled
          top_signals <- final_signal_universe_df %>%
            dplyr::group_by(tickers) %>%
            dplyr::summarise(
              avg_ir = mean(info_ratio, na.rm = TRUE),
              avg_alpha = mean(alpha, na.rm = TRUE),
              alpha_t_stat = mean(alpha_t_stat, na.rm = TRUE),
              .groups = 'drop'
            ) %>%
            dplyr::arrange(dplyr::desc(avg_ir)) %>%
            dplyr::slice_head(n = 5)
          display_table(top_signals, "Top Signals by Average Information Ratio")
        } else {
          ###Partial Pooled
          top_signals <- final_signal_universe_df %>%
            dplyr::group_by(tickers) %>%
            dplyr::summarise(
              avg_ir = mean(info_ratio, na.rm = TRUE),
              avg_alpha = mean(individual_alpha, na.rm = TRUE),
              alpha_t_stat = mean(alpha_t_stat, na.rm = TRUE),
              .groups = 'drop'
            ) %>%
            dplyr::arrange(dplyr::desc(avg_ir)) %>%
            dplyr::slice_head(n = 5)
          display_table(top_signals, "Top Signals by Average Information Ratio")
        }

      }

    } else {
      if(ss_backtest_workflow$p_correction_method == "bayesian"){
        #Bayesian
        top_signals <- final_signal_universe_df %>%
          dplyr::group_by(tickers) %>%
          dplyr::summarise(
            avg_sharpe = mean(sharpe_ratio, na.rm = TRUE),
            avg_alpha = mean(individual_alpha, na.rm = TRUE),
            alpha_t_stat = mean(alpha_t_stat, na.rm = TRUE),
            avg_posterior_alpha = mean(posterior_individual_alpha, na.rm = TRUE),
            avg_alpha_t_stat = mean(posterior_alpha_t_stat, na.rm = TRUE),
            .groups = 'drop'
          ) %>%
          dplyr::arrange(dplyr::desc(avg_sharpe)) %>%
          dplyr::slice_head(n = 5)
        display_table(top_signals, "Top Signals by Average Sharpe Ratio")
      } else {
        #Frequentist
        if(ss_backtest_workflow$model_structure == "no_pooled"){
          ###No Pooled
          top_signals <- final_signal_universe_df %>%
            dplyr::group_by(tickers) %>%
            dplyr::summarise(
              avg_sharpe = mean(sharpe_ratio, na.rm = TRUE),
              avg_alpha = mean(alpha, na.rm = TRUE),
              alpha_t_stat = mean(alpha_t_stat, na.rm = TRUE),
              .groups = 'drop'
            ) %>%
            dplyr::arrange(dplyr::desc(avg_sharpe)) %>%
            dplyr::slice_head(n = 5)
          display_table(top_signals, "Top Signals by Average Sharpe Ratio")
        } else {
          ###Partial Pooled
          top_signals <- final_signal_universe_df %>%
            dplyr::group_by(tickers) %>%
            dplyr::summarise(
              avg_sharpe = mean(sharpe_ratio, na.rm = TRUE),
              avg_alpha = mean(individual_alpha, na.rm = TRUE),
              alpha_t_stat = mean(alpha_t_stat, na.rm = TRUE),
              .groups = 'drop'
            ) %>%
            dplyr::arrange(dplyr::desc(avg_sharpe)) %>%
            dplyr::slice_head(n = 5)
          display_table(top_signals, "Top Signals by Average Sharpe Ratio")
        }
      }
    }

  } else if (table_name == "Top_Themes") {
    ss_backtest_workflow <- object@ss_backtest_workflow[[length(object@ss_backtest_workflow)]]
    # Top signals by avg_ir, including alpha_t_stat
    if(ss_backtest_workflow$active_returns){
      if(ss_backtest_workflow$p_correction_method == "bayesian"){
        #Bayesian
        top_signals <- final_signal_universe_df %>%
          dplyr::group_by(theme) %>%
          dplyr::summarise(
            avg_ir = mean(info_ratio, na.rm = TRUE),
            avg_alpha = mean(individual_alpha, na.rm = TRUE),
            alpha_t_stat = mean(alpha_t_stat, na.rm = TRUE),
            avg_posterior_alpha = mean(posterior_individual_alpha, na.rm = TRUE),
            avg_alpha_t_stat = mean(posterior_alpha_t_stat, na.rm = TRUE),
            .groups = 'drop'
          ) %>%
          dplyr::arrange(dplyr::desc(avg_ir)) %>%
          dplyr::slice_head(n = 5)
        display_table(top_signals, "Top Themes by Average Information Ratio")
      } else {
        #Frequentist
        if(ss_backtest_workflow$model_structure == "no_pooled"){
          ###No Pooled
          top_signals <- final_signal_universe_df %>%
            dplyr::group_by(theme) %>%
            dplyr::summarise(
              avg_ir = mean(info_ratio, na.rm = TRUE),
              avg_alpha = mean(alpha, na.rm = TRUE),
              alpha_t_stat = mean(alpha_t_stat, na.rm = TRUE),
              .groups = 'drop'
            ) %>%
            dplyr::arrange(dplyr::desc(avg_ir)) %>%
            dplyr::slice_head(n = 5)
          display_table(top_signals, "Top Themes by Average Information Ratio")
        } else {
          ###Partial Pooled
          top_signals <- final_signal_universe_df %>%
            dplyr::group_by(theme) %>%
            dplyr::summarise(
              avg_ir = mean(info_ratio, na.rm = TRUE),
              avg_alpha = mean(individual_alpha, na.rm = TRUE),
              alpha_t_stat = mean(alpha_t_stat, na.rm = TRUE),
              .groups = 'drop'
            ) %>%
            dplyr::arrange(dplyr::desc(avg_ir)) %>%
            dplyr::slice_head(n = 5)
          display_table(top_signals, "Top Themes by Average Information Ratio")
        }

      }

    } else {
      if(ss_backtest_workflow$p_correction_method == "bayesian"){
        #Bayesian
        top_signals <- final_signal_universe_df %>%
          dplyr::group_by(theme) %>%
          dplyr::summarise(
            avg_sharpe = mean(sharpe_ratio, na.rm = TRUE),
            avg_alpha = mean(individual_alpha, na.rm = TRUE),
            alpha_t_stat = mean(alpha_t_stat, na.rm = TRUE),
            avg_posterior_alpha = mean(posterior_individual_alpha, na.rm = TRUE),
            avg_alpha_t_stat = mean(posterior_alpha_t_stat, na.rm = TRUE),
            .groups = 'drop'
          ) %>%
          dplyr::arrange(dplyr::desc(avg_sharpe)) %>%
          dplyr::slice_head(n = 5)
        display_table(top_signals, "Top Themes by Average Sharpe Ratio")
      } else {
        #Frequentist
        if(ss_backtest_workflow$model_structure == "no_pooled"){
          ###No Pooled
          top_signals <- final_signal_universe_df %>%
            dplyr::group_by(theme) %>%
            dplyr::summarise(
              avg_sharpe = mean(sharpe_ratio, na.rm = TRUE),
              avg_alpha = mean(alpha, na.rm = TRUE),
              alpha_t_stat = mean(alpha_t_stat, na.rm = TRUE),
              .groups = 'drop'
            ) %>%
            dplyr::arrange(dplyr::desc(avg_sharpe)) %>%
            dplyr::slice_head(n = 5)
          display_table(top_signals, "Top Themes by Average Sharpe Ratio")
        } else {
          ###Partial Pooled
          top_signals <- final_signal_universe_df %>%
            dplyr::group_by(theme) %>%
            dplyr::summarise(
              avg_sharpe = mean(sharpe_ratio, na.rm = TRUE),
              avg_alpha = mean(individual_alpha, na.rm = TRUE),
              alpha_t_stat = mean(alpha_t_stat, na.rm = TRUE),
              .groups = 'drop'
            ) %>%
            dplyr::arrange(dplyr::desc(avg_sharpe)) %>%
            dplyr::slice_head(n = 5)
          display_table(top_signals, "Top Themes by Average Sharpe Ratio")
        }
      }
    }
  } else {
    stop("Unknown table name.")
  }

  invisible(object)
})


#' @title Summary Method for port_backtest_results Class
#' @description Provides a detailed summary of `port_backtest_results` object.
#' Users can select which summary table to display by specifying the `summary_id` parameter.
#' The summary includes interactive tables styled using the `DT` package.
#'
#' @param object An object of class `port_backtest_results`.
#' @param summary_id A character string or numeric value specifying which table to display.
#' @return Invisibly returns the input `object`.
#' @export
setMethod("summary", "port_backtest_results", function(object, summary_id = NULL) {
  # Define colors
  deep_navy <- "#000033"
  black <- "#000000"
  white <- "#FFFFFF"

  # Available tables
  available_tables <- c(
    "Returns Summary",
    "Costs Summary",
    "Metrics Summary",
    "Stock Universe Summary",
    "Final Stock Universe Summary",
    "Transactions Log"
  )

  # Print summary header
  cat("==============================\n")
  cat("Port Backtest Results Summary\n")
  cat("==============================\n\n")
  cat("Backtest Config Name:", object@backtest_identifier, "\n")

  # If no summary_id provided, prompt the user to select
  if (is.null(summary_id)) {
    cat("\nPlease choose a table to display:\n")
    for (i in seq_along(available_tables)) {
      cat(paste0(i, ": ", available_tables[i], "\n"))
    }
    selection <- readline(prompt = "Enter the number of your choice: ")
    summary_id <- as.numeric(selection)
    if (is.na(summary_id) || summary_id < 1 || summary_id > length(available_tables)) {
      stop("Invalid selection.")
    }
  }

  # Resolve summary_id to table name
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

  # Get objects
  port_returns_m_xts <- object@port_returns_m_xts
  port_costs_m_xts <- object@port_costs_m_xts
  port_metrics_m_xts <- object@port_metrics_m_xts
  port_weights_m_df <- object@port_weights_m_df
  stock_universe_m_df <- object@stock_universe_m_df
  final_stock_universe_m_d_ref <- object@final_stock_universe_m_d_ref
  transactions_log <- object@transactions_log@data

  # Display the selected table

  if (table_name == "Returns Summary"){

    if("selected_bench_return" %in% colnames(port_returns_m_xts@data)){
      bench_returns_m_xts <- port_returns_m_xts
      bench_returns_m_xts@data <- bench_returns_m_xts@data[, "selected_bench_return"]
    } else {
      bench_returns_m_xts <- NULL
    }

    port_returns_m_xts <- port_returns_m_xts
    port_returns_m_xts@data <- port_returns_m_xts@data[, c("raw_return", "net_return", "raw_active_return", "net_active_return")]

    summary(port_returns_m_xts, benchmark_returns_m_xts = bench_returns_m_xts)

  } else if (table_name == "Costs Summary"){

    summary(port_costs_m_xts)

  } else if (table_name == "Metrics Summary"){

    if(!is.null(port_metrics_m_xts)){
      summary(port_metrics_m_xts)
    } else {
      cat("No metrics available.")
    }

  } else if (table_name == "Stock Universe Summary"){

    summary(stock_universe_m_df)

  } else if (table_name == "Final Stock Universe Summary"){

    summary(final_stock_universe_m_d_ref)

  } else if (table_name == "Transactions Log"){

    available_dates <- names(transactions_log)

    if (length(available_dates) == 0) {
      stop("The transactions_log object does not contain any dated entries.")
    }

      cat("Available dates in the transactions log:\n")
      for (i in seq_along(available_dates)) {
        cat(paste0(i, ": ", available_dates[i], "\n"))
      }
      selection <- readline(prompt = "Enter the number of the date you want to summarize: ")
      selection <- as.numeric(selection)
      if (is.na(selection) || selection < 1 || selection > length(available_dates)) {
        stop("Invalid selection.")
      }
      date <- available_dates[selection]

    df <- transactions_log[[date]]
    df <- df %>%
      # Round all numeric columns to 2 decimals
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(., 3))) %>%
      # Keep only rows where relative_order_size is greater than 0
      dplyr::filter(relative_order_size > 0) %>%
      dplyr::arrange(desc(total_cost))

    if (length(df) == 0) {
      stop("No transactions found for the selected date.")
    }

    cat("\nSummary for date:", date, "\n")

    # Define color palette (using your stylistics)
    deep_navy <- "#000033"                  # Deep Navy for data rows
    black <- "#000000"                      # Black for headers and captions
    white <- "#FFFFFF"                      # White text
    vibrant_purple <- "#6A0DAD"             # Vibrant Purple
    teal_blue <- "#00BFFF"                  # Teal Blue
    soft_pink <- "#FF69B4"                  # Soft Pink
    bright_yellow_orange <- "#FFA500"       # Bright Yellow-Orange
    blue_bg <- "#001f3f"                    # Blue background for plots

    caption_text <- paste0(date, ": Transactions Log")

    # Create a DT datatable with similar styling as in your example
    dt_obj <- DT::datatable(
      df,
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
        style = paste0('caption-side: top; text-align: center; color: ', white,
                       '; background-color: ', black, '; padding: 10px; margin-bottom: 10px; font-size: 18px; font-weight: bold;'),
        caption_text
      )
    ) %>% DT::formatStyle(
      columns = names(df),
      backgroundColor = deep_navy,
      color = white
    )

    # Apply additional CSS styling
    css_styles <- paste0("
      table.dataTable thead th {
        background-color: ", black, " !important;
        color: ", white, " !important;
      }
      table.dataTable tbody tr {
        background-color: ", deep_navy, " !important;
      }
      table.dataTable tbody td {
        border-color: #333333 !important;
      }
      .dataTable {
        background-color: ", deep_navy, " !important;
        color: ", white, ";
      }
  ")

    dt_obj <- htmlwidgets::prependContent(dt_obj, htmltools::tags$style(css_styles))

    print(dt_obj)
    invisible(dt_obj)


  }

})

#' @title Summary Method for port_backtest_cohort Class
#' @description Provides a detailed summary of `port_backtest_cohort` object.
#' Users can select which summary table to display by specifying the `summary_id` parameter.
#' The summary includes interactive tables styled using the `DT` package.
#'
#' @param object An object of class `port_backtest_cohort`.
#' @param summary_id A character string or numeric value specifying which table to display.
#' @return Invisibly returns the input `object`.
#' @export
setMethod("summary", "port_backtest_cohort", function(object, summary_id = NULL) {
  # Define colors
  deep_navy <- "#000033"
  black <- "#000000"
  white <- "#FFFFFF"

  # Available tables
  available_tables <- c(
    "Raw Returns Summary",
    "Net Returns Summary",
    "Raw Active Returns Summary",
    "Net Active Returns Summary",
    "Direct Cost Summary",
    "Market Impact Cost Summary",
    "Total Cost Summary",
    "Turnover Summary",
    "Weights Summary",
    "Metrics Summary"
  )

  # Print summary header
  cat("==============================\n")
  cat("Port Backtest Cohort Summary\n")
  cat("==============================\n\n")
  cat("Cohort Name:", object@cohort_name, "\n")

  # If no summary_id provided, prompt the user to select
  if (is.null(summary_id)) {
    cat("\nPlease choose a table to display:\n")
    for (i in seq_along(available_tables)) {
      cat(paste0(i, ": ", available_tables[i], "\n"))
    }
    selection <- readline(prompt = "Enter the number of your choice: ")
    summary_id <- as.numeric(selection)
    if (is.na(summary_id) || summary_id < 1 || summary_id > length(available_tables)) {
      stop("Invalid selection.")
    }
  }

  # Resolve summary_id to table name
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

  # Get objects
  port_weights_m_df <- object@port_weights_m_df
  port_costs_m_xts_list <- object@port_costs_m_xts_list
  port_returns_m_xts_list <- object@port_returns_m_xts_list
  port_metrics_m_xts_list <- object@port_metrics_m_xts_list
  port_backtest_results_list <- object@port_backtest_results_list

  # Print selected table
  if (table_name == "Raw Returns Summary"){

    raw_returns_m_xts <- port_returns_m_xts_list$raw_returns_m_xts

    if (!is.null(object@backtest_workflow_common$selected_benchmark)){
      bench_returns_m_xts <- raw_returns_m_xts
      bench_returns_m_xts@data <- bench_returns_m_xts@data[, "selected_bench_return"]
      raw_returns_m_xts@data <- raw_returns_m_xts@data[, -which(colnames(raw_returns_m_xts@data) == "selected_bench_return")]
    } else {
      bench_returns_m_xts <- NULL
    }

    summary(raw_returns_m_xts, benchmark_returns_m_xts = bench_returns_m_xts)


  }

  if (table_name == "Net Returns Summary"){

    net_returns_m_xts <- port_returns_m_xts_list$net_returns_m_xts

    if (!is.null(object@backtest_workflow_common$selected_benchmark)){
      bench_returns_m_xts <- net_returns_m_xts
      bench_returns_m_xts@data <- bench_returns_m_xts@data[, "selected_bench_return"]
      net_returns_m_xts@data <- net_returns_m_xts@data[, -which(colnames(net_returns_m_xts@data) == "selected_bench_return")]
    } else {
      bench_returns_m_xts <- NULL
    }

    summary(net_returns_m_xts, benchmark_returns_m_xts = bench_returns_m_xts)

  }

  if (table_name == "Raw Active Returns Summary"){

    if (is.null(object@backtest_workflow_common$selected_benchmark)){
      stop("There is no benchmark available for selected plot.")
    }

    raw_active_returns_m_xts <- port_returns_m_xts_list$raw_active_returns_m_xts

    summary(raw_active_returns_m_xts)

  }

  if (table_name == "Net Active Returns Summary"){

    if (is.null(object@backtest_workflow_common$selected_benchmark)){
      stop("There is no benchmark available for selected plot.")
    }

    net_active_returns_m_xts <- port_returns_m_xts_list$net_active_returns_m_xts

    summary(net_active_returns_m_xts)

  }

  if (table_name == "Direct Cost Summary"){

    direct_cost_m_xts <- port_costs_m_xts_list$direct_cost_m_xts
    summary(direct_cost_m_xts)

  }

  if (table_name == "Market Impact Cost Summary"){

    market_impact_cost_m_xts <- port_costs_m_xts_list$market_impact_cost_m_xts
    summary(market_impact_cost_m_xts)

  }

  if (table_name == "Total Cost Summary"){

    total_cost_m_xts <- port_costs_m_xts_list$total_cost_m_xts
    summary(total_cost_m_xts)

  }

  if (table_name == "Turnover Summary"){

    turnover_m_xts <- port_costs_m_xts_list$turnover_m_xts
    summary(turnover_m_xts)

  }

  if (table_name == "Weights Summary"){

    summary(port_weights_m_df)

  }

  if (table_name == "Metrics Summary"){

    #Promp the user regarding the metrics to plot
    available_metrics <- names(port_metrics_m_xts_list) %>% stringr::str_remove("_m_xts")

    selected_metric <- readline(prompt = paste("Please select the metric to plot from the following list: ",
                                               paste(available_metrics, collapse = ", "), ": "))

    if (!selected_metric %in% available_metrics){
      stop("Invalid metric selected")
    }

    metric_m_xts <- port_metrics_m_xts_list[[paste0(selected_metric, "_m_xts")]]

    summary(metric_m_xts)

  }

})






