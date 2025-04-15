#' @title Plot Method for `meta_dataframe` Objects
#'
#' @description
#' This method creates a variety of diagnostic and summary plots from a `meta_dataframe` object,
#' including time series plots, cross-sectional plots, histograms, boxplots, composition plots,
#' regression plots, density plots, tile heatmaps, correlograms, and radar charts.
#'
#' It supports both univariate and bivariate statistics (e.g., correlation, regression),
#' faceting by clustering variables, and interactive filtering by tickers, dates, or other custom criteria.
#'
#' @param x A `meta_dataframe` object to be plotted.
#' @param type A character string specifying the plot type. Valid options include:
#'   `"cross_sectional"`, `"time_series"`, `"histogram"`, `"boxplot"`, `"composition"`, `"regression"`,
#'   `"density2d"`, `"correlogram"`, `"radar"`, `"waterfall"`, `"tile_heatmap"`, `"frequency"`.
#' @param clustering_variables A character vector specifying one or more columns to group by (e.g., `"dates"`, `"tickers"`).
#'   Used to produce faceted plots or grouping within plots.
#' @param variable A character vector specifying the main numeric variable(s) to analyze or visualize.
#' @param tickers Character vector of tickers to include. Use `"all"` (default) to include all tickers.
#' @param dates Either a single date, a vector of dates, or `"all"` to include all.
#' @param calc_stat Character string specifying the calculation to apply. Supported options include:
#'   `"mean"`, `"sd"`, `"median"`, `"min"`, `"max"`, `"sum"`, `"n"`, `"q05"`, `"q10"`, `"q25"`, `"q75"`, `"q90"`, `"q95"`,
#'   and bivariate metrics such as `"cor"`, `"beta"`, `"beta_tstat"`, `"alpha"`, `"alpha_tstat"`.
#' @param custom_filter A character or character vector specifying the column(s) to filter on (e.g., `sector`, `theme`).
#' @param filter_values A list or vector of values corresponding to `custom_filter` columns.
#' @param dep_y Optional. A character string indicating the dependent variable to be used in bivariate statistics
#'   (e.g., `"cor"`, `"beta"`, `"alpha"`). Required when `calc_stat` involves regression or correlation.
#' @param numeric_aggregation A character string defining how to discretize numeric `clustering_variables`.
#'   Options include: `"decile"`, `"quartile"`, `"tercile"`, `"median"`. Default is `"decile"`.
#'
#' @return A `ggplot` object or base R plot (in the case of radar plots), visualizing the requested output.
#'         The plot is printed as a side effect.
#'
#' @details
#' This method performs internal filtering and summarization before rendering visual output.
#' - **Univariate plots** (e.g., histogram, boxplot, time series) are based on the `variable` argument.
#' - **Bivariate plots** (e.g., regression, correlation) require both `variable` and `dep_y`.
#' - **Tile heatmaps** classify values across time and grouping dimensions using quantiles (e.g., deciles).
#'
#' The plot aesthetics and background colors follow a customized neon/blue theme consistent with the `factoRverse` package style.
#'
#' @export

setMethod(
  "plot",
  signature(x = "meta_dataframe", y = "missing"),
  function(x, type = NULL, clustering_variables = NULL, variable = NULL, tickers = "all", dates = "all", calc_stat = NULL,
           custom_filter = NULL, filter_values = NULL, dep_y = NULL, numeric_aggregation = "decile") {

    #Check for packages
    if (!requireNamespace("gridExtra", quietly = TRUE) || !requireNamespace("scales", quietly = TRUE)) {
      stop("Packages 'gridExtra' and 'scales' are required to generate plots. Please install them using install.packages().")
    }


    # Prompt for 'type' if not specified
    if (is.null(type)) {
      if(inherits(x, "groups_m_df")){
      available_types <- c("frequency", "composition", "tile_heatmap")
      } else {
      available_types <- c("cross_sectional", "time_series", "histogram", "boxplot", "composition",
                           "regression", "density2d", "correlogram", "radar", "waterfall", "tile_heatmap")
      }
      cat("\nPlease choose a plot type:\n")
      for (i in seq_along(available_types)) {
        cat(paste0(i, ": ", available_types[i], "\n"))
      }
      selection <- readline(prompt = "Enter the number of your choice: ")
      selection <- as.numeric(selection)
      if (is.na(selection) || selection < 1 || selection > length(available_types)) {
        stop("Invalid selection.")
      }
      type <- available_types[selection]
    }

    if(inherits(x, "groups_m_df") && !type %in% c("frequency", "composition", "tile_heatmap")){
      stop("Only composition and tile_heatmaps are avaiable for groups_m_df objects.")
    }

    # Prompt for 'variables' if not specified
    if (is.null(variable)) {
      available_variables <- setdiff(names(x@data), c("id", "tickers", "dates"))
      cat("\nPlease choose variable(s) (separated by commas, or press Enter to skip):\n")
      for (i in seq_along(available_variables)) {
        cat(paste0(i, ": ", available_variables[i], "\n"))
      }
      selection <- readline(prompt = "Enter your choices (e.g., 1,2): ")
      if (nzchar(selection)) {
        indices <- as.numeric(strsplit(selection, ",")[[1]])
        if (any(is.na(indices)) || any(indices < 1 | indices > length(available_variables))) {
          stop("Invalid selection for variable.")
        }
        variable <- available_variables[indices]
      }
    }


    #Check number of variables provided
    if(length(variable) == 0){
      stop("Please select at least one variable.")
    }
    if(length(variable) > 1 && !type %in% c("density2d", "correlogram", "radar", "waterfall")){
      if(!type == "regression"){
        stop("Currently, only one variable can be selected for the chosen plot type.")
      } else {
        stop("For regression, set only one variable (the independent variable) and then set dep_y (the dependent variable).")
      }
    }
    if(length(variable) < 3 && type == "radar"){
      stop("For radar plots, select at least three variables.")
    }


    # Prompt for 'clustering_variables' if not specified
    if (is.null(clustering_variables) && !type %in% c("composition")) {
      available_clustering_vars <- setdiff(names(x@data), c("id", variable))
      cat("\nPlease choose clustering variables (separated by commas, or press Enter to skip):\n")
      for (i in seq_along(available_clustering_vars)) {
        cat(paste0(i, ": ", available_clustering_vars[i], "\n"))
      }
      selection <- readline(prompt = "Enter your choices (e.g., 1,2): ")
      if (nzchar(selection)) {
        indices <- as.numeric(strsplit(selection, ",")[[1]])
        if (any(is.na(indices)) || any(indices < 1 | indices > length(available_clustering_vars))) {
          stop("Invalid selection for clustering variables.")
        }
        clustering_variables <- available_clustering_vars[indices]
      }
    }

    # Prompt for 'tickers' if not specified
    if (identical(tickers, "all")) {
      cat("\nDo you want to include all tickers or select specific ones? (Type 'all' or enter tickers separated by commas):\n")
      selection <- readline(prompt = "Enter your choice: ")
      if (nzchar(selection) && selection != "all") {
        tickers <- strsplit(selection, ",")[[1]]
      }
    }

    # Prompt for 'dates' if not specified
    if (identical(dates, "all")) {
      cat("\nDo you want to include all dates or select a date range? (Type 'all' or enter range in 'YYYY-MM-DD,YYYY-MM-DD' format):\n")
      selection <- readline(prompt = "Enter your choice: ")
      if (nzchar(selection) && selection != "all") {
        date_range <- strsplit(selection, ",")[[1]]
        if (length(date_range) > 2 || any(!grepl("^\\d{4}-\\d{2}-\\d{2}$", date_range))) {
          stop("Invalid date range format. Use 'YYYY-MM-DD,YYYY-MM-DD'.")
        }
        if(length(dates) == 1){
          dates <- as.Date(date_range)
        } else {
          dates <- seq.Date(as.Date(date_range[1]), as.Date(date_range[2]), by = "day")
        }
      }
    }

    # Prompt for 'calc_stat' if not specified
    if (is.null(calc_stat) && type %in% c("cross_sectional", "time_series", "waterfall")) {
      available_stats <- c("mean", "sd", "median", "min", "max", "sum", "n", "q05", "q10", "q25", "q75", "q90", "q95", "cor", "beta", "beta_tstat", "alpha", "alpha_tstat")
      cat("\nPlease choose a calculation statistic (calc_stat):\n")
      for (i in seq_along(available_stats)) {
        cat(paste0(i, ": ", available_stats[i], "\n"))
      }
      selection <- readline(prompt = "Enter the number of your choice: ")
      selection <- as.numeric(selection)
      if (is.na(selection) || selection < 1 || selection > length(available_stats)) {
        stop("Invalid selection for calc_stat.")
      }
      calc_stat <- available_stats[selection]
    } else {
      calc_stat <- if(type %in% c("waterfall", "signal_evolution")) "sum" else "mean"
    }

    # Prompt for 'dep_y' if applicable
    if (is.null(dep_y) && (calc_stat %in% c("cor", "beta", "beta_tstat", "alpha", "alpha_tstat") || type == "regression")) {
      available_variables <- setdiff(names(x@data), c("id", "tickers", "dates", variable))
      cat("\nPlease choose a dependent variable (dep_y):\n")
      for (i in seq_along(available_variables)) {
        cat(paste0(i, ": ", available_variables[i], "\n"))
      }
      selection <- readline(prompt = "Enter the number of your choice: ")
      selection <- as.numeric(selection)
      if (is.na(selection) || selection < 1 || selection > length(available_variables)) {
        stop("Invalid selection for dep_y.")
      }
      dep_y <- available_variables[selection]
    }

    # Set default clustering_variables based on plot type
    if (is.null(clustering_variables)) {
      if (type == "cross_sectional"){
        clustering_variables <- "tickers"
      }
      if (type == "time_series"){
        clustering_variables <- "dates"
      }
    }

    #Treatment for composition plot
    if(type == "composition"){
      if(!is.null(clustering_variables)){
        stop("Composition plot does not support clustering variables.")
        if(calc_stat != "mean"){
          stop("Composition plot does not support custom calc_stat.")
        }
      }
    }

    #Check if variable and dep_y are set as character
    if(any(!is.character(variable)) && !type %in% c("tile_heatmap")){
      stop("variable must be set as character values describing colnames in the data.frame.")
    }

    if(!all(variable %in% colnames(x@data))){
      stop("variable must be present in the data.frame.")
    }

    #Check for dep_y adequacy
    if(!is.null(dep_y)){
      if(any(!is.character(dep_y))){
        stop("dep_y must be set as character values describing colnames in the data.frame.")
      }

      if(!all(dep_y %in% colnames(x@data))){
        stop("dep_y must be present in the data.frame.")
      }

      if((type == "composition" || type == "histogram" || type == "density2d")){
        stop("dep_y is not supported for this plot type.")
      }

    }

    # Define colors for plotting
    deep_navy <- "#000033"
    black <- "#000000"
    white <- "#FFFFFF"
    vibrant_purple <- "#6A0DAD"
    neon_green <- "#39FF14"
    neon_orange <- "#FF5F1F"
    blue_bg <- "#001f3f"
    neon_yellow <- "#FFDC00"
    neon_pink <- "#FF007F"
    cyan <- "#7FDBFF"
    neon_red <- "#FF4136"

    df <- x@data
    date_range_text <- ""
    tickers_text <- ""

    # Define statistic function based on user choice
    FUN <- switch(calc_stat,
                  mean = function(x) mean(x, na.rm = TRUE),
                  sd = function(x) sd(x, na.rm = TRUE),
                  median = function(x) median(x, na.rm = TRUE),
                  min = function(x) min(x, na.rm = TRUE),
                  max = function(x) max(x, na.rm = TRUE),
                  sum = function(x) sum(x, na.rm = TRUE),
                  n = function(x) length(x),
                  q05 = function(x) quantile(x, 0.05, na.rm = TRUE),
                  q10 = function(x) quantile(x, 0.10, na.rm = TRUE),
                  q25 = function(x) quantile(x, 0.25, na.rm = TRUE),
                  q75 = function(x) quantile(x, 0.75, na.rm = TRUE),
                  q90 = function(x) quantile(x, 0.90, na.rm = TRUE),
                  q95 = function(x) quantile(x, 0.95, na.rm = TRUE),
                  cor = function(x, dep_y) {
                    if (missing(dep_y)) stop("dep_y is required for correlation calculation")
                    stats::cor(x, dep_y, use = "complete.obs")
                  },
                  beta = function(x, dep_y) {
                    if (missing(dep_y)) stop("dep_y is required for beta calculation")
                    stats::lm(dep_y ~ x)$coefficients[2]
                  },
                  beta_tstat = function(x, dep_y) {
                    if (missing(dep_y)) stop("dep_y is required for beta t-stat calculation")
                    summary(stats::lm(dep_y ~ x))$coefficients[2, 3]
                  },
                  alpha = function(x, dep_y) {
                    if (missing(dep_y)) stop("dep_y is required for alpha calculation")
                    stats::lm(y ~ x)$coefficients[1]
                  },
                  alpha_tstat = function(x, dep_y) {
                    if (missing(dep_y)) stop("dep_y is required for alpha t-stat calculation")
                    summary(stats::lm(dep_y ~ x))$coefficients[1, 3]
                  },
                  stop("Invalid function")
    )

    # Filter based on tickers
    if (!identical(tickers, "all")) {
      tickers_clean <- gsub(pattern = '[\"[:space:]]', replacement = '', x = tickers)
      df <- df %>% dplyr::filter(tickers %in% tickers_clean)
      tickers_text <- if (length(tickers) > 10) "> 10 tickers" else paste("Tickers:", paste(tickers, collapse = ", "))
    } else {
      tickers_text <- "All tickers"
    }

    # Filter based on dates
    if (!identical(dates, "all")) {
      df <- df %>% dplyr::filter(dates %in% !!dates)
    }
    date_range_text <- paste("Dates:", paste(unique(range(dates)), collapse = " - "))

    # Apply custom filters if specified
    if (!is.null(custom_filter) && !is.null(filter_values)) {
      if (is.character(custom_filter) && is.list(filter_values) && length(custom_filter) == length(filter_values)) {
        df <- purrr::reduce2(custom_filter, filter_values, .init = df, ~ .x %>% dplyr::filter(!!rlang::sym(.y) %in% .z))
      } else if (is.character(custom_filter) && is.vector(filter_values)) {
        df <- df %>% dplyr::filter(!!rlang::sym(custom_filter) %in% filter_values)
      } else {
        stop("custom_filter and filter_values must be compatible: custom_filter should be a vector of column names, and filter_values a list or vector.")
      }
    }

    # Stop if no data after filtering
    if (nrow(df) == 0) stop("No data available for the specified filters.")

    # Identify numeric clustering variables and create decile-based factors if needed
    # Set number of bins and labels based on chosen aggregation
    bins <- switch(numeric_aggregation,
                   decile = 10,
                   quartile = 4,
                   tercile = 3,
                   median = 2,
                   stop("Invalid numeric_aggregation. Choose from 'decile', 'quartile', 'tercile', or 'median'."))

    # Pre-compute labels once
    label_list <- switch(numeric_aggregation,
                         decile = sprintf("d%02d", 1:10),
                         quartile = sprintf("q%02d", 1:4),
                         tercile = sprintf("t%02d", 1:3),
                         median = c("below_median", "above_median"))

    numeric_clustering_variables <- purrr::keep(clustering_variables, ~ is.numeric(df[[.x]]))

    if (length(numeric_clustering_variables) > 0) {

      # Check if any date has fewer observations than the number of bins
      insufficient_obs <- df %>%
        dplyr::group_by(dates) %>%
        dplyr::summarise(n_obs = dplyr::n(), .groups = 'drop') %>%
        dplyr::filter(n_obs < bins)

      if (nrow(insufficient_obs) > 0) {
        # Create a descriptive error message listing problematic dates and their observation counts
        error_message <- paste0(
          "Not enough observations to create '", numeric_aggregation, "' bins on the following date(s): ",
          paste(insufficient_obs$dates, collapse = ", "),
          ".\nRequested: ", bins, " bins. Available: ", insufficient_obs$n_obs,
          " observation(s) each.\nConsider using a less granular 'numeric_aggregation' or providing more data."
        )
        stop(error_message, call. = FALSE)
      }

      # Mutate to create factor-based categorization with proper labels (date-wise)
      df <- df %>%
        dplyr::group_by(dates) %>%
        dplyr::mutate(
          dplyr::across(
            dplyr::all_of(numeric_clustering_variables),
            ~ {
              var_name <- dplyr::cur_column()
              var_labels <- paste0(label_list, "_", var_name)
              factor_val <- dplyr::ntile(., bins)
              factor(factor_val, levels = seq_len(bins), labels = var_labels)
            },
            .names = "{numeric_aggregation}_{col}"
          )
        ) %>%
        dplyr::ungroup()

      # Update clustering variables to include the new categorized columns
      clustering_variables <- purrr::map_chr(clustering_variables, ~ if (.x %in% numeric_clustering_variables) {
        paste0(numeric_aggregation, "_", .x)
      } else .x)
    }

    # Logic for cross-sectional plot
    if (type == "cross_sectional") {
      if (is.null(variable)) stop("Please specify the 'variable' argument for cross-sectional plot.")

      # Group by clustering variable and calculate the statistic
      if (calc_stat %in% c("cor", "beta", "beta_tstat", "alpha", "alpha_tstat")) {
        if (is.null(y)) stop(paste(calc_stat, "requires a secondary variable 'y'."))
        df_fun <- df %>%
          dplyr::group_by(dplyr::across(dplyr::all_of(clustering_variables))) %>%
          dplyr::summarize(Calc_Stat = FUN(!!rlang::sym(variable), !!rlang::sym(y))) %>%
          dplyr::ungroup()
      } else {
        df_fun <- df %>%
          dplyr::group_by(dplyr::across(dplyr::all_of(clustering_variables))) %>%
          dplyr::summarize(Calc_Stat = FUN(!!rlang::sym(variable))) %>%
          dplyr::ungroup()
      }

      # Remove rows with NaN in the Calc_Stat column
      df_fun <- df_fun %>% dplyr::filter(!is.nan(Calc_Stat))

      # Stop if no data remains after filtering
      if (nrow(df_fun) == 0) stop("No data available after removing NaN values from Calc_Stat.")

      # Cross-sectional bar plot
      p <-
        ggplot2::ggplot(df_fun, ggplot2::aes(x = interaction(!!!rlang::syms(clustering_variables)), y = Calc_Stat)) +
        ggplot2::geom_bar(stat = "identity", fill = vibrant_purple, color = black) +
        ggplot2::geom_text(ggplot2::aes(label = scales::scientific(Calc_Stat, digits = 2)), vjust = -0.5, color = white) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.background = ggplot2::element_rect(fill = blue_bg, color = NA),
          panel.background = ggplot2::element_rect(fill = blue_bg, color = NA),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          axis.text = ggplot2::element_text(color = white),
          axis.title = ggplot2::element_text(color = white),
          plot.title = ggplot2::element_text(color = white, size = 16, face = "bold"),
          plot.subtitle = ggplot2::element_text(color = white, size = 8, face = "italic")
        ) +
        ggplot2::labs(
          title = paste("Cross-section of", variable, "by", paste(clustering_variables, collapse = ", ")),
          subtitle = paste(date_range_text, ifelse(tickers_text != "", paste("|", tickers_text), "")),
          x = paste(clustering_variables, collapse = ", "),
          y = paste(calc_stat, variable)
        )

      print(p)

    }

    #Time series
    if (type == "time_series") {
      # Ensure 'dates' is included in clustering_variables for time series
      if (is.null(variable)) stop("Please specify the 'variable' argument for the time series plot.")

      clustering_variables <- c("dates", setdiff(clustering_variables, "dates"))

      # Calculate the statistic based on user-specified function
      if (calc_stat %in% c("cor", "beta", "beta_tstat", "alpha", "alpha_tstat")) {
        if (is.null(y)) stop(paste(calc_stat, "requires a secondary variable 'y'."))
        df_fun <- df %>%
          dplyr::group_by(dplyr::across(dplyr::all_of(clustering_variables))) %>%
          dplyr::summarize(Calc_Stat = FUN(!!rlang::sym(variable), !!rlang::sym(y)), .groups = "drop")
      } else {
        df_fun <- df %>%
          dplyr::group_by(dplyr::across(dplyr::all_of(clustering_variables))) %>%
          dplyr::summarize(Calc_Stat = FUN(!!rlang::sym(variable)), .groups = "drop")
      }

      # Remove rows with NaN in the Calc_Stat column
      df_fun <- df_fun %>% dplyr::filter(!is.nan(Calc_Stat))

      # Stop if no data remains after filtering
      if (nrow(df_fun) == 0) stop("No data available after removing NaN values from Calc_Stat.")

      # If clustering variables include more than just 'dates', create separate columns for each unique combination
      if (length(clustering_variables) > 1) {

        # Pivot wider to have one column per unique group of clustering variables, excluding 'dates'
        df_wide <- df_fun %>%
          tidyr::pivot_wider(names_from = setdiff(clustering_variables, "dates"),
                             values_from = Calc_Stat,
                             names_sep = "_")

        # Choose a color palette (e.g., "Set3" for distinct colors) or any other palette with enough colors
        num_series <- ncol(df_wide) - 1  # Number of unique time series (columns in df_wide excluding 'dates')
        color_palette <- suppressWarnings(RColorBrewer::brewer.pal(min(num_series, 12), "Set3"))  # Adjust palette size to number of series

        # If more than 12 series, extend the palette by repeating colors (or use a larger palette if available)
        if (num_series > 12) {
          color_palette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))(num_series)
        }

        # Pivot df_wide to long format for easier plotting with legend
        df_long <- df_wide %>%
          tidyr::pivot_longer(cols = -dates, names_to = "Group", values_to = "Calc_Stat")

        # Plot each time series with a unique color and include legend
        custom_title <- if(all(clustering_variables == c("dates", "tickers"))){
          paste("Time Series of", variable, "by", paste(setdiff(clustering_variables, "dates"), collapse = ", "))
        } else {
          paste("Time Series of", calc_stat, variable, "by", paste(setdiff(clustering_variables, "dates"), collapse = ", "))
        }

        p <-
          ggplot2::ggplot(df_long, ggplot2::aes(x = dates, y = Calc_Stat, color = Group, group = Group)) +
          ggplot2::geom_line(size = 1) +
          ggplot2::scale_y_continuous(labels = scales::scientific) +
          ggplot2::scale_color_manual(values = color_palette) +  # Apply custom color palette
          ggplot2::theme_minimal() +
          ggplot2::theme(
            plot.background = ggplot2::element_rect(fill = deep_navy, color = NA),
            panel.background = ggplot2::element_rect(fill = deep_navy, color = NA),
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            axis.text = ggplot2::element_text(color = white),
            axis.title = ggplot2::element_text(color = white),
            plot.title = ggplot2::element_text(color = white, size = 16, face = "bold"),
            plot.subtitle = ggplot2::element_text(color = white, size = 8, face = "italic"),
            legend.position = "bottom",
            legend.title = ggplot2::element_text(color = white),
            legend.text = ggplot2::element_text(color = white)
          ) +
          ggplot2::labs(
            title = custom_title,
            subtitle = date_range_text,
            x = "Date",
            y = paste(calc_stat, variable),
            color = paste(setdiff(clustering_variables, "dates"), collapse = ", ")
          )

        print(p)
      } else {
        # Single time series line plot for Calc_Stat over dates
        p <-
          ggplot2::ggplot(df_fun, ggplot2::aes(x = dates, y = Calc_Stat)) +
          ggplot2::geom_line(size = 1, color = vibrant_purple) +
          ggplot2::scale_y_continuous(labels = scales::scientific) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            plot.background = ggplot2::element_rect(fill = blue_bg, color = NA),
            panel.background = ggplot2::element_rect(fill = blue_bg, color = NA),
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            axis.text = ggplot2::element_text(color = white),
            axis.title = ggplot2::element_text(color = white),
            plot.title = ggplot2::element_text(color = white, size = 16, face = "bold"),
            plot.subtitle = ggplot2::element_text(color = white, size = 8, face = "italic")
          ) +
          ggplot2::labs(
            title = paste("Time Series of", calc_stat, variable),
            subtitle = paste(date_range_text, ifelse(tickers_text != "", paste("|", tickers_text), "")),
            x = "Date",
            y = paste(calc_stat, variable),
            color = if (!identical(clustering_variables, "tickers")) paste(clustering_variables, collapse = ", ") else NULL
          )
        print(p)
      }
    }

    # histogram Plot
    if (type == "histogram") {
      if (is.null(variable)) stop("Please specify the 'variable' argument for histogram plot.")

      # Filter out rows with NA in the specified variable
      df <- dplyr::filter(df, !is.na(!!rlang::sym(variable)))

      # Check if clustering_variables is empty or NULL, in which case we create a single plot
      if (is.null(clustering_variables) || length(clustering_variables) == 0) {
        # Single histogram plot with classic histogram
        p <-
          ggplot2::ggplot(df, ggplot2::aes_string(x = variable)) +
          ggplot2::geom_histogram(bins = 30, fill = vibrant_purple, color = black, alpha = 0.7) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            plot.background = ggplot2::element_rect(fill = blue_bg, color = NA),
            panel.background = ggplot2::element_rect(fill = blue_bg, color = NA),
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            axis.text = ggplot2::element_text(color = white),
            axis.title = ggplot2::element_text(color = white),
            plot.title = ggplot2::element_text(color = white, size = 16, face = "bold"),
            plot.subtitle = ggplot2::element_text(color = white, size = 8, face = "italic")
          ) +
          ggplot2::labs(
            title = paste("histogram of", variable),
            subtitle = paste(date_range_text, ifelse(tickers_text != "", paste("|", tickers_text), "")),
            x = variable,
            y = "Frequency"
          )
        print(p)
      } else {

        # Generate color palette based on the number of unique categories
        num_categories <- length(unique(df[[clustering_variables]]))
        base_palette <- supressWarnings(RColorBrewer::brewer.pal(min(num_categories, 12), "Set3"))
        color_palette <- if (num_categories > 12) {
          grDevices::colorRampPalette(base_palette)(num_categories)
        } else {
          base_palette
        }

        # histogram plot faceted by clustering variables with classic histogram
        p <-
          ggplot2::ggplot(df, ggplot2::aes_string(x = variable, fill = clustering_variables)) +
          ggplot2::geom_histogram(bins = 30, color = black, alpha = 0.7, position = "identity") +
          ggplot2::facet_wrap(ggplot2::vars(interaction(!!!rlang::syms(clustering_variables))), scales = "free") +
          ggplot2::scale_fill_manual(values = color_palette) +  # Apply custom color palette
          ggplot2::theme_minimal() +
          ggplot2::theme(
            plot.background = ggplot2::element_rect(fill = blue_bg, color = NA),
            panel.background = ggplot2::element_rect(fill = blue_bg, color = NA),
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            axis.text = ggplot2::element_text(color = white),
            axis.title = ggplot2::element_text(color = white),
            plot.title = ggplot2::element_text(color = white, size = 16, face = "bold"),
            plot.subtitle = ggplot2::element_text(color = white, size = 8, face = "italic"),
            strip.text = ggplot2::element_text(color = white),  # Set facet label color to white
            legend.position = "none" #Exclude legend
          ) +
          ggplot2::labs(
            title = paste("histogram of", variable, "by", paste(clustering_variables, collapse = ", ")),
            subtitle = paste(date_range_text, ifelse(tickers_text != "", paste("|", tickers_text), "")),
            x = variable,
            y = "Frequency"
          )
        print(p)
      }
    }

    # Boxplot
    if (type == "boxplot") {
      if (is.null(variable)) stop("Please specify the 'variable' argument for the boxplot.")
      if (is.null(clustering_variables) || length(clustering_variables) == 0) {
        stop("Please specify 'clustering_variables' for the boxplot.")
      }

      # Filter out rows with NA in the specified variable
      df <- df %>% dplyr::filter(!is.na(!!rlang::sym(variable)))

      # Generate color palette based on the number of unique categories
      num_categories <- length(unique(df[[clustering_variables]]))
      base_palette <- RColorBrewer::brewer.pal(min(num_categories, 12), "Set3")
      color_palette <- if (num_categories > 12) {
        grDevices::colorRampPalette(base_palette)(num_categories)
      } else {
        base_palette
      }

      # Create the boxplot
      p <- ggplot2::ggplot(df, ggplot2::aes_string(x = clustering_variables, y = variable, fill = clustering_variables)) +
        ggplot2::geom_boxplot() +
        ggplot2::scale_fill_manual(values = color_palette) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.background = ggplot2::element_rect(fill = blue_bg, color = NA),
          panel.background = ggplot2::element_rect(fill = blue_bg, color = NA),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          axis.text = ggplot2::element_text(color = white),
          axis.title = ggplot2::element_text(color = white),
          plot.title = ggplot2::element_text(color = white, size = 16, face = "bold"),
          plot.subtitle = ggplot2::element_text(color = white, size = 8, face = "italic"),
          legend.position = "none"
        ) +
        ggplot2::labs(
          title = paste("Boxplot of", variable, "by", paste(clustering_variables, collapse = ", ")),
          subtitle = paste(date_range_text, ifelse(tickers_text != "", paste("|", tickers_text), "")),
          x = paste(clustering_variables, collapse = ", "),
          y = variable
        )
      print(p)
    }


    # Composition plot
    if (type == "composition") {
      if (is.null(variable)) stop("Please specify the 'variable' argument for the composition plot.")

      # Ensure 'dates' is included in clustering_variables for time series composition
      clustering_variables <- c("dates", setdiff(clustering_variables, "dates"))

      # Filter out rows with NA in the specified variable
      df <- df %>% dplyr::filter(!is.na(!!rlang::sym(variable)))

      # Check if variable is numeric, and if so, create deciles or other aggregation
      if (is.numeric(df[[variable]])) {
        # Set number of bins and labels based on chosen aggregation
        bins <- switch(numeric_aggregation,
                       decile = 10,
                       quartile = 4,
                       tercile = 3,
                       median = 2,
                       stop("Invalid numeric_aggregation. Choose from 'decile', 'quartile', 'tercile', or 'median'."))

        # Create decile-based factor for the variable
        df <- df %>%
          dplyr::mutate(
            Composition_Group = factor(
              dplyr::ntile(!!rlang::sym(variable), bins),
              labels = switch(numeric_aggregation,
                              decile = sprintf("D%02d", 1:10),
                              quartile = sprintf("Q%02d", 1:4),
                              tercile = sprintf("T%02d", 1:3),
                              median = c("Below Median", "Above Median"))
            )
          )
      } else {
        # If variable is not numeric, use it directly for composition
        df <- df %>% dplyr::mutate(Composition_Group = as.factor(!!rlang::sym(variable)))
      }

      # Aggregate the data by dates and Composition_Group to get the counts per group over time
      df_fun <- df %>%
        dplyr::group_by(dates, Composition_Group) %>%
        dplyr::summarize(Count = dplyr::n(), .groups = "drop")

      # Generate color palette based on the number of unique categories
      num_categories <- length(unique(df_fun$Composition_Group))
      base_palette <- RColorBrewer::brewer.pal(min(num_categories, 3), "Set3")
      color_palette <- if (num_categories > 3) {
        grDevices::colorRampPalette(base_palette)(num_categories)
      } else {
        base_palette
      }

      # Create the composition area plot
      p <-
        ggplot2::ggplot(df_fun, ggplot2::aes(x = dates, y = Count, fill = Composition_Group)) +
        ggplot2::geom_area(position = "fill", alpha = 0.7) +
        ggplot2::scale_fill_manual(values = color_palette) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.background = ggplot2::element_rect(fill = blue_bg, color = NA),
          panel.background = ggplot2::element_rect(fill = blue_bg, color = NA),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          axis.text = ggplot2::element_text(color = white),
          axis.title = ggplot2::element_text(color = white),
          plot.title = ggplot2::element_text(color = white, size = 16, face = "bold"),
          plot.subtitle = ggplot2::element_text(color = white, size = 8, face = "italic"),
          legend.position = "bottom",
          legend.title = ggplot2::element_text(color = white),
          legend.text = ggplot2::element_text(color = white)
        ) +
        ggplot2::labs(
          title = paste("Composition of", variable, "by", paste(clustering_variables, collapse = ", ")),
          subtitle = paste(date_range_text, ifelse(tickers_text != "", paste("|", tickers_text), "")),
          x = "Date",
          y = "Proportion",
          fill = "Composition Group"
        )

      print(p)
    }

    # Regression plot
    if (type == "regression") {
      # Ensure that y is provided
      if (missing(dep_y)) stop("Please provide a dependent variable (dep_y) for the regression plot.")


      # Check if clustering variables are provided
      if (is.null(clustering_variables)) {
        # Single regression line
        p <- ggplot2::ggplot(df, ggplot2::aes_string(x = variable, y = dep_y)) +
          ggplot2::geom_point(color = neon_green, alpha = 0.8) +
          ggplot2::geom_smooth(method = "lm", se = FALSE, color = vibrant_purple, size = 1.2) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            plot.background = ggplot2::element_rect(fill = blue_bg, color = NA),
            panel.background = ggplot2::element_rect(fill = blue_bg, color = NA),
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            axis.text = ggplot2::element_text(color = white),
            axis.title = ggplot2::element_text(color = white, face = "bold"),
            plot.title = ggplot2::element_text(color = white, size = 18, face = "bold", hjust = 0), # Left-aligned title
            plot.subtitle = ggplot2::element_text(color = white, size = 10, face = "italic", hjust = 0) # Left-aligned subtitle
          ) +
          ggplot2::labs(
            title = paste("Regression of", variable, "on", dep_y),
            subtitle = paste(date_range_text, ifelse(tickers_text != "", paste("|", tickers_text), "")),
            x = variable,
            y = dep_y
          )
      } else {
        if (clustering_variables == "dates") df$dates <- as.factor(df$dates)
        # Grouped regression lines
        p <- ggplot2::ggplot(df, ggplot2::aes_string(x = variable, y = dep_y, color = clustering_variables)) +
          ggplot2::geom_point(alpha = 0.8) +
          ggplot2::geom_smooth(method = "lm", se = FALSE, size = 1.2) +
          ggplot2::scale_color_manual(values = grDevices::colorRampPalette(c(neon_green, vibrant_purple, neon_orange))(length(unique(df[[clustering_variables]])))) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            plot.background = ggplot2::element_rect(fill = blue_bg, color = NA),
            panel.background = ggplot2::element_rect(fill = blue_bg, color = NA),
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            axis.text = ggplot2::element_text(color = white),
            axis.title = ggplot2::element_text(color = white, face = "bold"),
            plot.title = ggplot2::element_text(color = white, size = 18, face = "bold", hjust = 0),
            plot.subtitle = ggplot2::element_text(color = white, size = 10, face = "italic", hjust = 0),
            legend.position = "bottom",
            legend.title = ggplot2::element_text(color = white),
            legend.text = ggplot2::element_text(color = white)
          ) +
          ggplot2::labs(
            title = paste("Regression of", variable, "on", dep_y, "by", paste(clustering_variables, collapse = ", ")),
            subtitle = paste(date_range_text, ifelse(tickers_text != "", paste("|", tickers_text), "")),
            x = variable,
            y = dep_y,
            color = clustering_variables
          )
      }

      print(p)
    }

    # 2d density
    if (type == "density2d") {

      #Check for the number of variables provided
      if (length(variable) != 2 && type == "density2d"){
        stop("For a 2d density plot, two variables must be provided.")
      } else {
        x_1 <- variable[1]
        x_2 <- variable[2]
      }

      # Check if clustering variables are provided for faceted density plots
      if (is.null(clustering_variables)) {
        # Single 2D density plot without faceting
        p <- ggplot2::ggplot(df, ggplot2::aes(x = !!rlang::sym(x_1), y = !!rlang::sym(x_2))) +
          ggplot2::geom_point(color = neon_green, alpha = 0.6) +
          ggplot2::geom_density_2d_filled(alpha = 0.7, contour_var = "ndensity") +
          ggplot2::scale_fill_viridis_d(option = "plasma") +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            plot.background = ggplot2::element_rect(fill = blue_bg, color = NA),
            panel.background = ggplot2::element_rect(fill = blue_bg, color = NA),
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            axis.text = ggplot2::element_text(color = white),
            axis.title = ggplot2::element_text(color = white, face = "bold"),
            plot.title = ggplot2::element_text(color = white, size = 18, face = "bold", hjust = 0),
            plot.subtitle = ggplot2::element_text(color = white, size = 10, face = "italic", hjust = 0),
            legend.position = "bottom",                     # Place legend at the bottom
            legend.title = ggplot2::element_text(color = white),
            legend.text = ggplot2::element_text(color = white)  # Set legend text color to white
          ) +
          ggplot2::labs(
            title = paste("2D Density Plot of", x_1, "and", x_2),
            subtitle = paste(date_range_text, ifelse(tickers_text != "", paste("|", tickers_text), "")),
            x = x_1,
            y = x_2
          )
      } else {
        # Faceted 2D density plot by each category in clustering_variables
        p <- ggplot2::ggplot(df, ggplot2::aes(x = !!rlang::sym(x_1), y = !!rlang::sym(x_2))) +
          ggplot2::geom_point(color = neon_green, alpha = 0.6) +
          ggplot2::geom_density_2d_filled(alpha = 0.7, contour_var = "ndensity") +
          ggplot2::facet_wrap(ggplot2::vars(!!!rlang::syms(clustering_variables)), scales = "free") +
          ggplot2::scale_fill_viridis_d(option = "plasma") +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            plot.background = ggplot2::element_rect(fill = blue_bg, color = NA),
            panel.background = ggplot2::element_rect(fill = blue_bg, color = NA),
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            axis.text = ggplot2::element_text(color = white),
            axis.title = ggplot2::element_text(color = white, face = "bold"),
            plot.title = ggplot2::element_text(color = white, size = 18, face = "bold", hjust = 0),
            plot.subtitle = ggplot2::element_text(color = white, size = 10, face = "italic", hjust = 0),
            legend.position = "bottom",                       # Place legend at the bottom
            legend.title = ggplot2::element_text(color = white),
            legend.text = ggplot2::element_text(color = white), # Set legend text color to white
            strip.text = ggplot2::element_text(color = white)  # Facet label text color
          ) +
          ggplot2::labs(
            title = paste("2D Density Plot of", x_1, "and", x_2, "by", paste(clustering_variables, collapse = ", ")),
            subtitle = paste(date_range_text, ifelse(tickers_text != "", paste("|", tickers_text), "")),
            x = x_1,
            y = x_2
          )
      }

      print(p)
    }

    #Correlogram
    if (type == "correlogram") {
      # Check if variables are provided
      if (is.null(variable) || length(variable) < 2) {
        stop("Please provide at least two variables in 'variable' for the correlogram.")
      }

      # Check for clustering variables and adapt accordingly
      if (!is.null(clustering_variables)) {
        unique_clusters <- unique(df[[clustering_variables]])

        # Initialize a list to store correlation data
        corr_data_all <- data.frame()

        # Loop over each unique value of clustering variable
        for (cluster in unique_clusters) {
          df_cluster <- dplyr::filter(df, !!rlang::sym(clustering_variables) == cluster)

          # Check if df_cluster contains the specified variables
          if (all(variable %in% colnames(df_cluster)) && nrow(df_cluster) > 0) {
            # Calculate correlation matrix for the specific cluster
            corr_matrix <- stats::cor(df_cluster[variable], use = "pairwise.complete.obs")
            corr_data <- as.data.frame(as.table(corr_matrix)) %>%
              dplyr::rename(Var1 = Var1, Var2 = Var2, Correlation = Freq) %>%
              dplyr::filter(as.numeric(factor(Var1)) < as.numeric(factor(Var2))) %>%
              dplyr::mutate(Cluster = as.factor(cluster))  # Add cluster label

            # Combine data for all clusters
            corr_data_all <- rbind(corr_data_all, corr_data)
          } else {
            message(paste("Skipping cluster", cluster, "- No data available for the specified variables"))
          }
        }

        # Plotting with facet_wrap for each cluster
        if (nrow(corr_data_all) > 0) {
          p <- ggplot2::ggplot(data = corr_data_all) +
            ggplot2::geom_tile(
              ggplot2::aes(x = Var1, y = Var2, fill = Correlation),
              color = "white"
            ) +
            ggplot2::geom_text(
              ggplot2::aes(x = Var1, y = Var2, label = sprintf("%.2f", Correlation)),
              color = "white", size = 4
            ) +
            ggplot2::scale_fill_gradient2(
              low = "#D68EEB", mid = "#8A03C9", high = "#000033", midpoint = 0,
              limits = c(-1, 1), guide = ggplot2::guide_colorbar(title = "Correlation")
            ) +
            ggplot2::theme_minimal() +
            ggplot2::theme(
              plot.background = ggplot2::element_rect(fill = blue_bg, color = NA),
              panel.background = ggplot2::element_rect(fill = blue_bg, color = NA),
              panel.grid = ggplot2::element_blank(),
              axis.text = ggplot2::element_text(color = white),
              axis.title = ggplot2::element_text(color = white, face = "bold"),
              plot.title = ggplot2::element_text(color = white, size = 20, face = "bold", hjust = 0),
              plot.subtitle = ggplot2::element_text(color = white, size = 12, face = "italic"),
              legend.position = "bottom",
              legend.title = ggplot2::element_text(color = white),
              legend.text = ggplot2::element_text(color = white),
              strip.text = ggplot2::element_text(color = white, size = 9, face = "bold")  # Smaller titles for each facet
            ) +
            ggplot2::labs(
              title = "Correlogram",
              subtitle = paste(date_range_text, ifelse(tickers_text != "", paste("|", tickers_text), "")),
              x = NULL, y = NULL
            ) +
            ggplot2::facet_wrap(~ Cluster, scales = "free", labeller = ggplot2::labeller(Cluster = stats::setNames(unique_clusters, unique_clusters)))  # Custom labels for each cluster

          print(p)
        } else {
          stop("No valid data available for any of the clusters.")
        }

      } else {
        # Single correlogram code
        corr_matrix <- stats::cor(df[variable], use = "pairwise.complete.obs")
        corr_data <- as.data.frame(as.table(corr_matrix)) %>%
          dplyr::rename(Var1 = Var1, Var2 = Var2, Correlation = Freq) %>%
          dplyr::filter(as.numeric(factor(Var1)) < as.numeric(factor(Var2)))

        # Plotting
        p <- ggplot2::ggplot(data = corr_data) +
          ggplot2::geom_tile(
            ggplot2::aes(x = Var1, y = Var2, fill = Correlation),
            color = "white"
          ) +
          ggplot2::geom_text(
            ggplot2::aes(x = Var1, y = Var2, label = sprintf("%.2f", Correlation)),
            color = "white", size = 4
          ) +
          ggplot2::scale_fill_gradient2(
            low = "#D68EEB", mid = "#8A03C9", high = "#000033", midpoint = 0,
            limits = c(-1, 1), guide = ggplot2::guide_colorbar(title = "Correlation")
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            plot.background = ggplot2::element_rect(fill = blue_bg, color = NA),
            panel.background = ggplot2::element_rect(fill = blue_bg, color = NA),
            panel.grid = ggplot2::element_blank(),
            axis.text = ggplot2::element_text(color = white),
            axis.title = ggplot2::element_text(color = white, face = "bold"),
            plot.title = ggplot2::element_text(color = white, size = 18, face = "bold", hjust = 0),
            plot.subtitle = ggplot2::element_text(color = white, size = 12, face = "italic"),
            legend.position = "bottom",
            legend.title = ggplot2::element_text(color = white),
            legend.text = ggplot2::element_text(color = white)
          ) +
          ggplot2::labs(
            title = "Correlogram",
            subtitle = paste(date_range_text, ifelse(tickers_text != "", paste("|", tickers_text), "")),
            x = NULL, y = NULL
          )

        print(p)
      }
    }

    # Radar Plot
    if (type == "radar") {
      if (is.null(variable) || length(variable) < 1) {
        stop("Please specify at least one variable for the radar plot.")
      }

      # Ensure clustering variables are set appropriately
      if (is.null(clustering_variables)) {
        stop("clustering_variables can't be missing when type = 'radar'.")  # Default value if not provided
      }

      # Save current graphical parameters
      old_par <- graphics::par(no.readonly = TRUE)

      # Set global graphical parameters for your plot
      graphics::par(fg = "white", col.axis = "white", col.lab = "white",
                    col.main = "white", col.sub = "white", bg = "#001f3f")

      # Prepare the data for the radar plot
      df_radar <- df %>%
        dplyr::select(!!rlang::sym(clustering_variables), !!!rlang::syms(variable)) %>%
        dplyr::group_by(!!rlang::sym(clustering_variables)) %>%
        dplyr::summarize(dplyr::across(dplyr::where(is.numeric), ~ mean(., na.rm = TRUE)), .groups = "drop")  # Calculate mean, ignoring NAs

      # Dynamically filter out rows with NA or NaN values for specified variables
      na_filter <- purrr::reduce(variable, function(acc, var) {
        acc & !is.na(df_radar[[var]]) & !is.nan(df_radar[[var]])
      }, .init = TRUE)

      df_radar <- df_radar[na_filter, ]  # Apply the filter

      # Check if there's data available for the radar plot
      if (nrow(df_radar) == 0) {
        stop("No data available for the specified clustering variables and variable.")
      }

      # Normalize the data
      df_radar <- df_radar %>%
        dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ ( . - min(., na.rm = TRUE)) / ( max(., na.rm = TRUE) - min(., na.rm = TRUE))))  # Normalization

      # Ensure the first column is treated as a factor for the radar chart
      df_radar[[clustering_variables]] <- as.character(df_radar[[clustering_variables]])

      # Prepare the data frame for the radar chart
      max_row <- as.data.frame(t(rep(1, length(variable))))  # Row of maximum values
      min_row <- as.data.frame(t(rep(0, length(variable))))  # Row of minimum values
      actual_data <- df_radar %>% dplyr::select(!!!rlang::syms(variable))  # Actual data for the radar plot

      # Set column names for max_row and min_row to match the actual data
      colnames(max_row) <- variable
      colnames(min_row) <- variable

      # Combine into one data frame
      df_radar_chart <- rbind(max_row, min_row, actual_data)

      # Set the colors for each row (example: generate colors)
      num_rows <- nrow(actual_data)
      colors <- scales::hue_pal()(num_rows)  # Generate distinct colors for each row

      # Set background color
      graphics::par(bg = "#001f3f")  # Set background color to blue_bg

      # Create the radar plot using fmsb
      if (!requireNamespace("fmsb", quietly = TRUE)) {
        stop("The 'fmsb' package is required for radar plots. Please install it.")
      }

      fmsb::radarchart(df_radar_chart, axistype = 1,
                       pcol = colors, pfcol = scales::alpha(colors, 0.5), plwd = 2, plty = 1,
                       axislabcol = "white"
                       # Set the main title color to white
      )

      # Title
      graphics::mtext(side = 3, line = 2.5, adj = 0, cex = 1.25,  # Align to the left
                      paste("Radar Plot of", paste(variable, collapse = ", "), "by", clustering_variables),
                      font = 2, col = "white")

      # Subtitle
      graphics::mtext(side = 3, line = 1, adj = 0, cex = 0.75,  # Align to the left
                      paste(date_range_text, ifelse(tickers_text != "", paste("|", tickers_text), "")),
                      font = 2, col = "white")


      # Add legend with white text color at the bottom
      graphics::legend("bottomleft", legend = df_radar[[clustering_variables]], col = colors, pch = 16, bty = "n", text.col = "white")

      # Restore the original graphical parameters
      graphics::par(old_par)

    }



    # Waterfall Plot
    if (type == "waterfall") {

      # Check variables
      if (is.null(variable) || length(variable) < 2) {
        stop("Please specify at least two numeric variables for the waterfall plot.")
      }

      # Check all variables are present and numeric
      if (!all(variable %in% names(df))) {
        stop("Some specified variables not found in the dataframe.")
      }

      # Check numeric
      if (any(!sapply(variable, function(v) is.numeric(df[[v]])))) {
        stop("All variables must be numeric for the waterfall plot.")
      }

      # Check clustering_variables
      if (is.null(clustering_variables) || length(clustering_variables) == 0) {
        clustering_variables <- "tickers"
      }

      # If calc_stat is not specified, use sum by default
      if (is.null(calc_stat)) {
        calc_stat <- "sum"
      }

      # Define the function for calc_stat
      FUN <- switch(calc_stat,
                    mean = function(x) mean(x, na.rm = TRUE),
                    sd = function(x) sd(x, na.rm = TRUE),
                    median = function(x) median(x, na.rm = TRUE),
                    min = function(x) min(x, na.rm = TRUE),
                    max = function(x) max(x, na.rm = TRUE),
                    sum = function(x) sum(x, na.rm = TRUE),
                    stop("Invalid calc_stat for waterfall plot. Please use sum, mean, sd, median, min, or max.")
      )

      # Aggregate data by clustering variables and apply FUN to each variable
      agg_data <- df %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(clustering_variables))) %>%
        dplyr::summarize(dplyr::across(dplyr::all_of(variable), FUN), .groups = "drop")

      if (nrow(agg_data) == 0) {
        stop("No data available for the specified filters.")
      }

      # Pivot from wide to long so that each chosen variable is now a row called "Component"
      agg_long <- agg_data %>%
        tidyr::pivot_longer(cols = dplyr::all_of(variable), names_to = "Component", values_to = "Value")

      # Make Component a factor with the order given by the user
      agg_long <- agg_long %>%
        dplyr::mutate(Component = factor(Component, levels = variable))

      # Now compute the cumulative sums by each cluster combination, in the order of "Component"
      # Group by clustering_variables (the facets), and then arrange by Component to ensure correct order
      agg_long <- agg_long %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(clustering_variables))) %>%
        dplyr::arrange(Component, .by_group = TRUE) %>%
        dplyr::mutate(
          Cumulative_Start = dplyr::lag(cumsum(Value), default = 0),
          Cumulative_End = Cumulative_Start + Value,
          Sign = ifelse(Value >= 0, "Positive", "Negative")
        ) %>%
        dplyr::ungroup()

      # Choose colors for positive and negative contributions
      pos_color <- neon_green
      neg_color <- neon_orange

      # If we have more than one clustering variable, we facet by them
      # If we have just one, it's still a single facet but shows a single plot
      # The x-axis now is Component
      main_plot <- ggplot2::ggplot(agg_long, ggplot2::aes(x = Component, ymin = Cumulative_Start, ymax = Cumulative_End, fill = Sign)) +
        ggplot2::geom_rect(
          xmin = as.numeric(agg_long$Component) - 0.4,
          xmax = as.numeric(agg_long$Component) + 0.4,
          color = black
        ) +
        ggplot2::scale_fill_manual(values = c(Positive = pos_color, Negative = neg_color)) +
        ggplot2::geom_hline(yintercept = 0, color = white, linetype = "dashed") +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.background = ggplot2::element_rect(fill = blue_bg, color = NA),
          panel.background = ggplot2::element_rect(fill = blue_bg, color = NA),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          axis.text = ggplot2::element_text(color = white),
          axis.title = ggplot2::element_text(color = white),
          plot.title = ggplot2::element_text(color = white, size = 16, face = "bold"),
          plot.subtitle = ggplot2::element_text(color = white, size = 8, face = "italic"),
          legend.position = "bottom",
          legend.title = ggplot2::element_text(color = white),
          legend.text = ggplot2::element_text(color = white),
          strip.text = ggplot2::element_text(color = white)
        ) +
        ggplot2::labs(
          title = paste("Waterfall Plot of", paste(variable, collapse = ", "), "Components"),
          subtitle = paste(date_range_text, ifelse(tickers_text != "", paste("|", tickers_text), "")),
          x = "Components",
          y = paste(calc_stat, "of", paste(variable, collapse = ", ")),
          fill = "Contribution"
        )

      # If more than one clustering variable, facet by them
      # If exactly one clustering variable, it will just show one facet (one plot)
      if (length(clustering_variables) > 1) {
        facet_clusters <- clustering_variables
        main_plot <- main_plot + ggplot2::facet_wrap(facets = facet_clusters, scales = "free_x")
      } else if (length(clustering_variables) == 1) {
        # One clustering variable means multiple categories produce multiple rows.
        main_plot <- main_plot + ggplot2::facet_wrap(facets = clustering_variables, scales = "free_x")
      }
      print(main_plot)
    }

    # Tile Heatmap
    if (type == "tile_heatmap") {

      # Ensure 'variable' is specified and is numeric
      if (is.null(variable) || length(variable) != 1) {
        stop("Please specify exactly one numeric 'variable' for the tile heatmap plot.")
      }

      # Determine y-axis variable based on clustering_variables
      if (!is.null(clustering_variables) && length(clustering_variables) > 0) {
        # Check if clustering_variables exist in the dataframe
        missing_vars <- setdiff(clustering_variables, names(df))
        if (length(missing_vars) > 0) {
          stop("The following clustering_variables are missing from the dataframe: ", paste(missing_vars, collapse = ", "))
        }
        y_vars <- clustering_variables
      } else {
        y_vars <- "tickers"
        if (!"tickers" %in% names(df)) {
          stop("'tickers' column is not present in the dataframe, and clustering_variables is NULL.")
        }
      }

      # Create an interaction term if multiple clustering variables are provided
      if (length(y_vars) > 1) {
        df <- df %>%
          dplyr::mutate(Cluster = interaction(!!!rlang::syms(y_vars), sep = "_"))
      } else {
        df <- df %>%
          dplyr::mutate(Cluster = !!rlang::sym(y_vars))
      }

      # Group by Cluster and Dates, then apply calc_stat on variable if numeric
      if(is.numeric(df[[variable]])){
        df_summary <- df %>%
          dplyr::group_by(Cluster, dates) %>%
          dplyr::summarize(
            Calc_Stat = FUN(!!rlang::sym(variable)),
            .groups = "drop"
          )

        # Check if there are valid rows to process
        if (nrow(df_summary) == 0) stop("No data available after filtering or summarization.")

        # Define the number of bins and labels based on numeric_aggregation
        bins <- switch(numeric_aggregation,
                       decile = 10,
                       quartile = 4,
                       tercile = 3,
                       median = 2,
                       stop("Invalid numeric_aggregation. Choose from 'decile', 'quartile', 'tercile', or 'median'."))

        label_list <- switch(numeric_aggregation,
                             decile = sprintf("D%02d", 1:10),
                             quartile = sprintf("Q%02d", 1:4),
                             tercile = sprintf("T%02d", 1:3),
                             median = c("Below_Median", "Above_Median"))


        # Categorize each Cluster by Calc_Stat for each date
        df_summary <- df_summary %>%
          dplyr::group_by(dates) %>%
          dplyr::mutate(
            bin = dplyr::ntile(Calc_Stat,
                               switch(numeric_aggregation,
                                      decile = 10,
                                      quartile = 4,
                                      tercile = 3,
                                      median = 2)),
            bin_label = dplyr::case_when(
              numeric_aggregation == "decile" ~ sprintf("D%02d", bin),
              numeric_aggregation == "quartile" ~ sprintf("Q%02d", bin),
              numeric_aggregation == "tercile" ~ sprintf("T%02d", bin),
              numeric_aggregation == "median" ~ ifelse(bin == 1, "Below_Median", "Above_Median")
            )
          ) %>%
          dplyr::ungroup()

        # Color palette for bins
        color_palette <- c(vibrant_purple, cyan, neon_green, neon_yellow, neon_yellow, neon_yellow, neon_orange, neon_red, neon_pink)

        # Create the plot
        p <- ggplot2::ggplot(df_summary, ggplot2::aes(x = dates, y = Cluster, fill = bin_label)) +
          ggplot2::geom_tile(color = white) +
          ggplot2::scale_fill_manual(
            values = stats::setNames(color_palette[1:bins], label_list),
            na.value = "grey50"
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            plot.background = ggplot2::element_rect(fill = blue_bg, color = NA),
            panel.background = ggplot2::element_rect(fill = blue_bg, color = NA),
            axis.text.x = ggplot2::element_text(color = white, angle = 45, hjust = 1),
            axis.text.y = ggplot2::element_text(color = white),
            axis.title = ggplot2::element_text(color = white),
            plot.title = ggplot2::element_text(color = white, size = 16, face = "bold"),
            plot.subtitle = ggplot2::element_text(color = white, size = 12, face = "italic"),
            legend.position = "bottom",
            legend.title = ggplot2::element_text(color = white),
            legend.text = ggplot2::element_text(color = white),
            panel.grid = ggplot2::element_blank()
          ) +
          ggplot2::labs(
            title = paste("Tile Heatmap of", variable," by dates and", clustering_variables),
            subtitle = paste(date_range_text, ifelse(tickers_text != "", paste("|", tickers_text), "")),
            x = "Dates",
            y = clustering_variables,
            fill = variable
          )
      } else {
        #Treatment for character
        df_summary <- df %>% dplyr::select(Cluster, dates, !!rlang::sym(variable)) %>% dplyr::rename(bin_label = !!rlang::sym(variable))
        # Color palette for bins
        color_palette <- c(cyan, neon_green, vibrant_purple, neon_pink, vibrant_purple)
        unique_labels <- unique(df_summary$bin_label)
        theme_colors <- stats::setNames(color_palette[seq_along(unique_labels)], unique_labels)

        # Create the plot
        p <-
          ggplot2::ggplot(df_summary, ggplot2::aes(x = dates, y = Cluster, fill = bin_label)) +
          ggplot2::geom_tile(color = "white") +
          ggplot2::scale_fill_manual(
            values = theme_colors,
            na.value = "grey50"
          ) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            plot.background = ggplot2::element_rect(fill = "#001f3f", color = NA),
            panel.background = ggplot2::element_rect(fill = "#001f3f", color = NA),
            axis.text.x = ggplot2::element_text(color = "white", angle = 45, hjust = 1),
            axis.text.y = ggplot2::element_text(color = "white"),
            axis.title = ggplot2::element_text(color = "white"),
            plot.title = ggplot2::element_text(color = "white", size = 16, face = "bold"),
            plot.subtitle = ggplot2::element_text(color = "white", size = 12, face = "italic"),
            legend.position = "bottom",
            legend.title = ggplot2::element_text(color = "white"),
            legend.text = ggplot2::element_text(color = "white"),
            panel.grid = ggplot2::element_blank()
          ) +
          ggplot2::labs(
            title = paste("Tile Heatmap of", variable, "by dates and", clustering_variables),
            subtitle = paste(date_range_text, ifelse(tickers_text != "", paste("|", tickers_text), "")),
            x = "Dates",
            y = clustering_variables,
            fill = variable
          )
      }
      print(p)
    }

    # Frequency
    if (type == "frequency") {

      # Identify categorical columns (excluding 'id', 'tickers', and 'dates')
      categorical_cols <- sapply(df, function(col) is.factor(col) || is.character(col))
      categorical_data <- df[, categorical_cols, drop = FALSE]
      categorical_data <- categorical_data[, !(names(categorical_data) %in% c("id", "tickers", "dates")), drop = FALSE]

      if (!is.null(categorical_data) && ncol(categorical_data) > 0) {
        for (col_name in names(categorical_data)) {
          col_data <- categorical_data[[col_name]]
          freq_table <- as.data.frame(table(col_data, useNA = "ifany"))
          names(freq_table) <- c("Category", "Frequency")

          # Create a bar plot using the specified colors
          plot <- ggplot2::ggplot(freq_table, ggplot2::aes(x = stats::reorder(Category, -Frequency), y = Frequency)) +
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
              title = paste(x@meta_dataframe_name, ": Frequency Plot for", col_name),
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

  }

)

#' @title Plot Method for meta_xts
#' @description
#' Plots time series or performance summaries from a `meta_xts` (or subclass) object using `ggplot2`.
#' Offers interactive prompts for series selection, cumulative return computation, faceting, and more.
#'
#' @details
#' The function behaves differently depending on whether the object is a subclass like `returns_meta_xts`.
#' It includes:
#' \enumerate{
#'   \item Interactive column selection.
#'   \item Optional performance metric visualization for returns objects.
#'   \item Optional cumulative return computation.
#'   \item Option to facet plots by year.
#'   \item Option to overlay horizontal mean lines.
#'   \item Option to add vertical reference lines.
#'   \item Neon color palette and dark theme styling.
#' }
#'
#' @param x A `meta_xts` or subclass object to be plotted.
#' @param y Not used. Included for S4 method compatibility.
#' @param clustering_list Optional named list assigning column names to cluster groups. Used to vary linetype by group.
#' @param facet_by_year Logical. Whether to facet plots by calendar year. If `NULL`, user is prompted interactively.
#' @param add_overall_means Logical. Whether to add dashed horizontal lines representing series-wide means.
#' Only applies if `cumulative = FALSE`. If `NULL`, user is prompted.
#' @param vertical_lines Optional vector of Dates or POSIXct timestamps at which to draw vertical dashed lines.
#' @param cumulative Logical. If `TRUE` and object is of class `returns_meta_xts`, plots cumulative returns (in %). If `NULL`, prompts the user.
#' @param plot_perf_metric Logical. For `returns_meta_xts` only. If `TRUE`, plots bar chart of selected performance metric instead of time series. If `NULL`, user is prompted.
#' @param benchmark_returns_m_xts Optional `meta_xts` object (single-column) with benchmark returns. Required if `active_returns = TRUE`.
#' @param active_returns Logical or `"yes"/"no"` string. Whether to compute active returns relative to the benchmark when `plot_perf_metric = TRUE`.
#' @param ... Currently unused.
#'
#' @return Invisibly returns the generated `ggplot` object.
#'
#' @seealso [create_performance_m_df()]
#'
#' @rdname plot_meta_xts
#' @export
setMethod("plot", signature = c(x = "meta_xts", y = "missing"),
          function(x, y, clustering_list = NULL, facet_by_year = NULL,
                   add_overall_means = NULL, vertical_lines = NULL, cumulative = NULL,
                   plot_perf_metric = NULL, benchmark_returns_m_xts = NULL, active_returns = FALSE, ...) {

            #Check for packages
            if (!requireNamespace("gridExtra", quietly = TRUE) || !requireNamespace("scales", quietly = TRUE)) {
              stop("Packages 'gridExtra' and 'scales' are required to generate plots. Please install them using install.packages().")
            }


            #Prompt for column selection
            # Get available column names
            cols <- colnames(x@data)

            # Display available options to the user
            numbered_cols <- paste0(seq_along(cols), ": ", cols)
            message("Available columns:\n", crayon::white(paste(numbered_cols, collapse = "\n")))

            # Prompt the user interactively
            input <- readline(prompt = "Enter column names or numbers separated by commas or 'all' for all columns: ")

            # Remove any leading/trailing whitespace
            input <- base::trimws(input)

            # If user types 'all', return all columns
            if (tolower(input) %in% c("all", "")) {
              selected_cols <- cols
            } else {
              # Split the input by commas and trim whitespace on each token
              tokens <- unlist(base::strsplit(input, split = ","))
              tokens <- base::trimws(tokens)

              # Initialize vector to store the selected column names
              selected_cols <- character(0)

              for (token in tokens) {
                # Try converting token to numeric. If it is numeric, use it as an index.
                num_token <- suppressWarnings(as.numeric(token))

                if (!is.na(num_token)) {
                  # Check if the index is within the valid range.
                  if (num_token < 1 || num_token > base::length(cols)) {
                    stop("The column index ", token, " is out of range. Available indices are 1 to ", base::length(cols), ".")
                  }
                  selected_cols <- c(selected_cols, cols[num_token])
                } else {
                  # If not numeric, assume token is a column name.
                  if (!(token %in% cols)) {
                    stop("The column name '", token, "' does not exist in the data.")
                  }
                  selected_cols <- c(selected_cols, token)
                }
              }
            }

           #Filter xts
            x@data <- x@data[, selected_cols]

            # If x is returns_meta_xts, decide if user wants to plot performance metric
            is_returns_xts <- methods::is(x, "returns_meta_xts")

            if (is_returns_xts) {
              if (is.null(plot_perf_metric) && length(unique(lubridate::year(zoo::index(x@data)))) > 1) {
                resp_perf <- readline(prompt = "Do you want to plot a performance metric? (yes/no): ")
                plot_perf_metric <- tolower(resp_perf) == "yes"
              }
            } else {
              # If not returns_meta_xts, ignore
              plot_perf_metric <- FALSE
            }

            if (plot_perf_metric) {
              #Check benchmark_returns_m_xts
              if (!is.null(benchmark_returns_m_xts) && !methods::is(benchmark_returns_m_xts, "meta_xts")) {
                stop("benchmark_returns_m_xts must be a meta_xts object.")
              }

              # Prompt for active_returns
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


              # Generate performance data
              performance_df <- create_performance_m_df(
                selected_backtest_returns_corrected_positions_m_xts_upd_ref = x@data,
                selected_market_factor_proxy_m_xts_upd_ref = if (is.null(benchmark_returns_m_xts)) NULL else benchmark_returns_m_xts@data,
                active_returns = active_returns,
                verbose = TRUE
              )

              # Identify the unique tickers
              unique_tickers <- unique(performance_df$tickers)
              # Create a map from ticker -> integer
              ticker_map <- stats::setNames(seq_along(unique_tickers), unique_tickers)
              # Replace each ticker in performance_df with its numeric code
              performance_df$tickers <- ticker_map[performance_df$tickers]

              # Pivot to wide (tickers = columns), ignoring 'id'/'dates'
              perf_wide <- performance_df %>%
                dplyr::select(-id, -dates) %>%
                tidyr::pivot_longer(
                  cols = -tickers,
                  names_to = "metric",
                  values_to = "value"
                ) %>%
                tidyr::pivot_wider(
                  names_from = tickers,
                  values_from = value
                )

              # Prompt user to pick a metric
              all_metrics <- unique(perf_wide$metric)
              cat("\nAvailable performance metrics:\n")
              for (i in seq_along(all_metrics)) {
                cat(i, ":", all_metrics[i], "\n")
              }
              sel <- readline(prompt = "Enter the number of your choice: ")
              sel_num <- as.numeric(sel)
              if (is.na(sel_num) || sel_num < 1 || sel_num > length(all_metrics)) {
                stop("Invalid selection.")
              }
              chosen_metric <- all_metrics[sel_num]

              # Subset data for that single metric
              metric_data <- perf_wide %>%
                dplyr::filter(metric == chosen_metric) %>%
                dplyr::select(-metric)

              # Convert to numeric vector with ticker columns
              #    One row, multiple columns
              # Ticker columns might be in alphabetical or original order
              # Gather them into a data.frame for ggplot
              metric_data_long <- metric_data %>%
                tidyr::pivot_longer(
                  cols = -character(0),
                  names_to = "ticker",
                  values_to = "val"
                )

              # Round data to 4 decimals
              metric_data_long$val <- round(metric_data_long$val, 4)

              # Use the same neon palette
              neon_colors <- c(
                "#00BFFF", "#FF1493", "#FFFF00", "#8A2BE2",
                "#FF4500", "#39FF14", "#FF69B4", "#32CD32", "#FFA500"
              )
              # If there are more tickers than colors, repeat as needed
              color_mapping <- rep(neon_colors, length.out = nrow(metric_data_long))

              # ggplot bar chart
              #    x = ticker, y = val
              p <- ggplot2::ggplot(metric_data_long, ggplot2::aes(x = ticker, y = val, fill = ticker)) +
                ggplot2::geom_bar(stat = "identity", color = "#FFFFFF") +
                ggplot2::scale_fill_manual(values = rep(neon_colors, length.out = length(unique(metric_data_long$ticker)))) +
                ggplot2::theme_minimal() +
                ggplot2::labs(
                  title = paste("Performance Metric:", chosen_metric),
                  x = "Series",
                  y = chosen_metric,
                  fill = "Series"
                ) +
                ggplot2::theme(
                  plot.background  = ggplot2::element_rect(fill = "#001f3f", color = NA),
                  panel.background = ggplot2::element_rect(fill = "#001f3f", color = NA),
                  plot.title       = ggplot2::element_text(color = "#FFFFFF", size = 16, face = "bold"),
                  axis.text        = ggplot2::element_text(color = "#FFFFFF"),
                  axis.title       = ggplot2::element_text(color = "#FFFFFF"),
                  legend.position  = "none",
                  panel.grid.major = ggplot2::element_line(color = "#4d4d4d", size = 0.2),
                  panel.grid.minor = ggplot2::element_line(color = "#4d4d4d", size = 0.1)
                )

              # Print plot
              print(p)
              # Print the mapping for reference
              cat("Legend:\n")
              for (original_name in names(ticker_map)) {
                cat(ticker_map[original_name], ":", original_name, "\n")
              }
              return(invisible(p))



            } else {

              # If x is returns_meta_xts, ask about cumulative if not specified
              if (methods::is(x, "returns_meta_xts")) {
                # If user didn't specify 'cumulative', prompt them
                if (is.null(cumulative)) {
                  response_cum <- readline(prompt = "Do you want to plot cumulative returns? (yes/no): ")
                  cumulative <- tolower(response_cum) == "yes"
                }
              } else {
                # If x is not returns_meta_xts, ignore 'cumulative' (force it to FALSE)
                cumulative <- FALSE
              }

              # Ask user if they want to facet by year if the argument is not provided
              if (is.null(facet_by_year) && is.null(vertical_lines)) {
                response_facet <- readline(prompt = "Do you want to facet the plot by year? (yes/no): ")
                facet_by_year <- tolower(response_facet) == "yes"
              } else {
                facet_by_year <- FALSE
              }

              # Ask user if they want to add overall mean lines if the argument is not provided
              if (is.null(add_overall_means) && !cumulative) {
                response_means <- readline(prompt = "Do you want to add overall mean lines? (yes/no): ")
                add_overall_means <- tolower(response_means) == "yes"
              } else {
                add_overall_means <- FALSE
              }


              # Define the neon color palette
              neon_colors <- c(
                "#00BFFF",  # Neon Blue
                "#FF1493",  # Neon Pink
                "#FFFF00",  # Neon Yellow
                "#8A2BE2",  # Neon Purple
                "#FF4500",  # Neon Orange
                "#39FF14",  # Neon Green
                "#FF69B4",  # Hot Pink
                "#32CD32",  # Lime Green
                "#FFA500"   # Bright Orange
              )

              blue_bg <- "#001f3f"
              faint_blue <- "#003366"
              white <- "#FFFFFF"
              vertical_line_color <- "#CCCC00"  # Dark Neon Yellow

              # Extract slots
              frequency <- x@frequency
              metric_name <- x@metric_name
              main_xts <- x@data

              # Convert xts -> data frame -> long format
              df_data <- as.data.frame(main_xts)
              df_data$dates <- as.Date(zoo::index(main_xts))
              df_data$year <- lubridate::year(df_data$dates)

              # Sort by date to ensure correct plotting order
              long_data <- long_data[order(long_data$dates), ]

              # Validate user-specified dates for vertical lines
              if (!is.null(vertical_lines)) {
                if (!all(class(vertical_lines) %in% c("Date", "POSIXt"))) {
                  stop("The 'vertical_lines' argument must be a Date or POSIXct vector.")
                }
                vertical_lines <- as.Date(vertical_lines)  # Convert to Date if not already
              }

              # Remove missing values and single-observation series
              series_counts <- long_data %>%
                dplyr::group_by(series) %>%
                dplyr::summarize(non_na_count = sum(!is.na(value)), .groups = "drop")

              multi_obs_series <- series_counts %>%
                dplyr::filter(non_na_count > 1) %>%
                dplyr::pull(series)

              plot_data_multi <- dplyr::filter(long_data, series %in% multi_obs_series & !is.na(value))

              # If cumulative = TRUE and x is returns_meta_xts, compute cumulative returns
              if (cumulative && methods::is(x, "returns_meta_xts")) {
                # Transform each series by cumulative product
                plot_data_multi <- plot_data_multi %>%
                  dplyr::group_by(series) %>%
                  dplyr::arrange(dates) %>%
                  dplyr::mutate(value = (cumprod(1 + value/100) - 1)*100) %>%
                  dplyr::ungroup()
                # Also rename the y-label so user sees it's cumulative
                metric_name <- paste0("cumulative ", metric_name)
              }

              # Compute overall mean per series if requested
              if (add_overall_means) {
                overall_means <- plot_data_multi %>%
                  dplyr::group_by(series) %>%
                  dplyr::summarize(overall_mean = mean(value, na.rm = TRUE), .groups = "drop") %>%
                  dplyr::mutate(series = as.factor(series))
              }

              # Apply clustering if provided
              if (!is.null(clustering_list)) {
                plot_data_multi$group <- NA
                for (grp in names(clustering_list)) {
                  assigned_series <- clustering_list[[grp]]
                  plot_data_multi$group[plot_data_multi$series %in% assigned_series] <- grp
                }

                unique_groups <- unique(plot_data_multi$group)

                if (length(unique_groups) == 1) {
                  warning("Only one group detected, plotting without grouping.")
                  plot_data_multi$group <- plot_data_multi$series
                  linetype_legend_title <- NULL
                } else {
                  linetype_legend_title <- "Group"
                }
              } else {
                plot_data_multi$group <- plot_data_multi$series
                linetype_legend_title <- NULL
              }

              # Assign colors to series
              unique_series <- unique(plot_data_multi$series)
              color_mapping <- stats::setNames(rep(neon_colors, length.out = length(unique_series)), unique_series)

              # Create a mapping from unique series to numbers
              series_mapping <- stats::setNames(seq_along(unique(plot_data_multi$series)), unique(plot_data_multi$series))
              plot_data_multi$series <- factor(series_mapping[plot_data_multi$series])
              color_mapping <- color_mapping[names(series_mapping)]
              names(color_mapping) <- series_mapping

              # Create the ggplot object
              plot_obj <- ggplot2::ggplot(plot_data_multi, ggplot2::aes(
                x = dates,
                y = value,
                color = series,
                group = series
              )) +
                ggplot2::geom_line(size = 0.8, na.rm = TRUE) +
                ggplot2::labs(
                  title = paste("Time series of", metric_name, "for", frequency, "frequency"),
                  x = "Date",
                  y = metric_name,
                  color = "Series Label",
                  linetype = linetype_legend_title
                ) +
                ggplot2::scale_color_manual(values = color_mapping) +
                ggplot2::theme_minimal() +
                ggplot2::theme(
                  plot.background  = ggplot2::element_rect(fill = blue_bg, color = NA),
                  panel.background = ggplot2::element_rect(fill = blue_bg, color = NA),
                  plot.title       = ggplot2::element_text(color = white, size = 16, face = "bold"),
                  axis.text        = ggplot2::element_text(color = white),
                  axis.title       = ggplot2::element_text(color = white),
                  legend.title     = ggplot2::element_text(color = white),
                  legend.text      = ggplot2::element_text(color = white),
                  panel.grid.major = ggplot2::element_line(color = faint_blue, size = 0.2),
                  panel.grid.minor = ggplot2::element_line(color = faint_blue, size = 0.1)
                ) +
                ggplot2::scale_x_date(labels = scales::date_format("%b-%y"))

              # Add faceting if enabled
              if (facet_by_year) {
                plot_obj <- plot_obj +
                  ggplot2::facet_wrap(~year, scales = "free") +
                  ggplot2::theme(
                    strip.text = ggplot2::element_text(color = white, face = "bold"),
                    legend.position = "right"
                  )
              }

              # Add overall mean lines if requested
              if (add_overall_means) {
                overall_means <- plot_data_multi %>%
                  dplyr::group_by(series) %>%
                  dplyr::summarize(overall_mean = mean(value, na.rm = TRUE), .groups = "drop")

                # Match the factor levels so geom_hline uses the same color mapping
                # 'series' in plot_data_multi is a factor with custom numeric labels, e.g. 1,2,3,...
                # Make sure overall_means$series has the exact same factor levels
                overall_means$series <- factor(overall_means$series,
                                               levels = levels(plot_data_multi$series))

                # Then add dashed lines colored by 'series'
                plot_obj <- plot_obj +
                  ggplot2::geom_hline(data = overall_means,
                                      ggplot2::aes(yintercept = overall_mean, color = series),
                                      linetype = "dashed", show.legend = FALSE)
              }

              # Add vertical dashed lines at specified timestamps
              if (!is.null(vertical_lines)) {
                # Create a data frame for annotations
                vline_data <- data.frame(
                  x = vertical_lines,
                  y = max(plot_data_multi$value, na.rm = TRUE) * 0.95,
                  label = format(vertical_lines, "%Y-%m-%d")
                )

                # Add vertical lines and their labels using geom_vline and geom_text
                plot_obj <- plot_obj +
                  ggplot2::geom_vline(xintercept = as.numeric(vertical_lines),
                                      linetype = "dashed", color = vertical_line_color, size = 0.5) +
                  ggplot2::geom_text(data = vline_data,
                                     ggplot2::aes(x = x, y = y, label = label),
                                     inherit.aes = FALSE,
                                     color = vertical_line_color,
                                     angle = 90,
                                     vjust = 0.5,
                                     size = 1)
              }

              # Print the legend mapping Backtest labels to identifiers
              cat("\nLegend:\n")
              for (i in seq_along(series_mapping)) {
                cat(paste(series_mapping[i], ":", names(series_mapping)[i], "\n"))
              }

              print(plot_obj)
              invisible(plot_obj)
            }
          })




################################

#' @title Plot Method for `grid_search_strategy`
#' @description Plot the values selected for each hyperparameter in `hyper_grid_domain` for grid search strategy.
#' @param x An object of class `grid_search_strategy`.
#' @param y Unused. Included for consistency with the generic `plot` method.
#' @return A `ggplot` object visualizing the hyperparameter grid and possible limits.
#' @export
setMethod("plot", signature(x = "grid_search_strategy", y = "missing"), function(x, y) {

  #Check for packages
  if (!requireNamespace("gridExtra", quietly = TRUE) || !requireNamespace("scales", quietly = TRUE)) {
    stop("Packages 'gridExtra' and 'scales' are required to generate plots. Please install them using install.packages().")
  }

  # Define colors for plotting
  deep_navy <- "#000033"
  black <- "#000000"
  white <- "#FFFFFF"
  vibrant_purple <- "#6A0DAD"
  neon_green <- "#39FF14"
  neon_orange <- "#FF5F1F"
  blue_bg <- "#001f3f"
  light_gray <- "#B0B0B0"  # Lighter shade for grid lines
  neon_cyan <- "#00FFFF"

  # Extract the hyperparameters and grid values
  hyper_list <- x@hyper_grid_domain@hyperparameter_list
  if (length(hyper_list) == 0) {
    stop("No hyperparameters found in the grid domain.")
  }

  # Define the predefined min and max ranges for certain hyperparameters
  predefined_limits <- list(
    "alpha" = c(0, 1),
    "lambda.min.ratio" = c(0, 1),
    "mtry" = c(0, 1),
    "eta" = c(0, 1),
    "colsample_bytree" = c(0, 1),
    "subsample" = c(0, 1),
    "droprate" = c(0, 1)
  )

  # Define transformations for specific hyperparameters
  transformation_rules <- list(
    "num.trees" = function(x) x / 100,
    "max.depth" = function(x) x / 10,
    "min.bucket" = function(x) x / 10,
    "min_child_weight" = function(x) x / 10,
    "gamma" = function(x) x / 10,
    "nrounds" = function(x) x / 100,
    "regularizer_l1" = function(x) x / 10,
    "regularizer_l2" = function(x) x / 10,
    "size_of_batch" = function(x) log(x, base = 2),
    "number_of_epochs" = function(x) x / 100
  )

  # Define labels for each transformation
  transformation_labels <- list(
    "num.trees" = "/10\u00b2",           # ²
    "max.depth" = "/10",
    "min.bucket" = "/10",
    "min_child_weight" = "/10",
    "gamma" = "/10",
    "nrounds" = "/10\u00b2",             # ²
    "regularizer_l1" = "/10",
    "regularizer_l2" = "/10",
    "size_of_batch" = "(log\u2082)",     # ₂
    "number_of_epochs" = "/10\u00b2"     # ²
  )

  # Apply transformations to hyperparameters if applicable
  transformed_hyper_list <- list()
  for (hp_name in names(hyper_list)) {
    if (hp_name %in% names(transformation_rules)) {
      # Apply the transformation
      transformed_hyper_list[[hp_name]] <- sapply(hyper_list[[hp_name]], transformation_rules[[hp_name]])
      # Update the hyperparameter name to reflect the transformation
      names(transformed_hyper_list)[names(transformed_hyper_list) == hp_name] <- paste0(hp_name, transformation_labels[[hp_name]])
    } else {
      transformed_hyper_list[[hp_name]] <- hyper_list[[hp_name]]  # No transformation
    }
  }

  # Prepare data for actual grid values
  plot_data <- data.frame(
    Hyperparameter = rep(names(transformed_hyper_list), sapply(transformed_hyper_list, length)),
    Value = unlist(transformed_hyper_list)
  )

  # Prepare data for predefined limits
  predefined_data <- data.frame(
    Hyperparameter = character(),
    MinAllowed = numeric(),
    MaxAllowed = numeric()
  )

  # Loop through each hyperparameter and check if predefined limits exist
  for (hp_name in names(hyper_list)) {
    if (hp_name %in% names(predefined_limits)) {
      predefined_data <- rbind(predefined_data, data.frame(
        Hyperparameter = if (hp_name %in% names(transformation_labels)) {
          paste0(hp_name, transformation_labels[[hp_name]])
        } else {
          hp_name
        },
        MinAllowed = predefined_limits[[hp_name]][1],
        MaxAllowed = predefined_limits[[hp_name]][2]
      ))
    }
  }

  # Create the plot with hyperparameters on the y-axis
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Value, y = Hyperparameter, color = Hyperparameter)) +
    ggplot2::geom_point(size = 4, colour = neon_green, shape = 4) +  # Points for actual grid values in white
    ggplot2::geom_point(data = predefined_data, ggplot2::aes(x = MinAllowed, y = Hyperparameter), shape = 124, size = 5, color = neon_cyan) +  # Min always closed
    ggplot2::geom_point(data = predefined_data, ggplot2::aes(x = MaxAllowed, y = Hyperparameter), shape = 124, size = 5, color = neon_cyan) +  # Max always closed
    ggplot2::labs(title = "Grid Search: Hyperparameter Grid Values",
                  y = "Hyperparameter",
                  x = "Selected Values") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = blue_bg, color = NA),
      panel.background = ggplot2::element_rect(fill = blue_bg, color = NA),
      plot.title = ggplot2::element_text(color = white, size = 16, face = "bold"),
      axis.text = ggplot2::element_text(color = white),
      axis.title = ggplot2::element_text(color = white),
      legend.position = "bottom",  # Adjust legend position if needed
      legend.title = ggplot2::element_text(color = white),
      legend.text = ggplot2::element_text(color = white),
      panel.grid.major = ggplot2::element_line(color = light_gray),  # Set major grid lines to light gray
      panel.grid.minor = ggplot2::element_line(color = light_gray)   # Set minor grid lines to light gray
    ) +
    ggplot2::scale_color_manual(values = c(vibrant_purple, neon_green, neon_orange, deep_navy)) +  # Use defined colors
    ggplot2::guides(fill = ggplot2::guide_none(), shape = ggplot2::guide_none())  # Remove legend for color and shape

  print(p)
})


#' @title Plot Method for `random_search_strategy`
#' @description Generate a sample for each hyperparameter and plot the histogram for random search.
#' @param x An object of class `random_search_strategy`.
#' @param y Unused. Included for consistency with the generic `plot` method.
#' @return A `ggplot` object visualizing the hyperparameter histograms with possible limits.
#' @export
setMethod("plot", signature(x = "random_search_strategy", y = "missing"), function(x, y) {

  #Check for packages
  if (!requireNamespace("gridExtra", quietly = TRUE) || !requireNamespace("scales", quietly = TRUE)) {
    stop("Packages 'gridExtra' and 'scales' are required to generate plots. Please install them using install.packages().")
  }

  # Define colors for plotting
  deep_navy <- "#000033"
  black <- "#000000"
  white <- "#FFFFFF"
  vibrant_purple <- "#6A0DAD"
  neon_green <- "#39FF14"
  neon_orange <- "#FF5F1F"
  blue_bg <- "#001f3f"
  light_gray <- "#B0B0B0"  # Lighter shade for grid lines
  neon_blue <- "#00BFFF"
  neon_pink <- "#FF1493"   # Added a light pink color
  neon_cyan <- "#00FFFF"
  neon_yellow <- "#FFFF00"
  neon_purple <- "#8A2BE2"

  # Extract n_iter from the object
  n_iter <- x@n_iter

  # Extract the hyperparameters and histograms
  hyper_list <- x@hyper_grid_domain@hyperparameter_list
  if (length(hyper_list) == 0) {
    stop("No hyperparameters found in the grid domain.")
  }

  # Define the predefined min and max ranges for certain hyperparameters
  predefined_limits <- list(
    "alpha" = c(0, 1),
    "lambda.min.ratio" = c(0, 1),
    "mtry" = c(0, 1),
    "eta" = c(0, 1),
    "colsample_bytree" = c(0, 1),
    "subsample" = c(0, 1),
    "droprate" = c(0, 1)
  )

  # Define transformations for specific hyperparameters
  transformation_rules <- list(
    "num.trees" = function(x) x / 100,
    "max.depth" = function(x) x / 10,
    "min.bucket" = function(x) x / 10,
    "min_child_weight" = function(x) x / 10,
    "gamma" = function(x) x / 10,
    "nrounds" = function(x) x / 100,
    "regularizer_l1" = function(x) x / 10,
    "regularizer_l2" = function(x) x / 10,
    "size_of_batch" = function(x) log(x, base = 2),
    "number_of_epochs" = function(x) x / 100
  )

  # Define labels for each transformation
  transformation_labels <- list(
    "num.trees" = "/10\\u00b2",
    "max.depth" = "/10",
    "min.bucket" = "/10",
    "min_child_weight" = "/10",
    "gamma" = "/10",
    "nrounds" = "/10\\u00b2",
    "regularizer_l1" = "/10",
    "regularizer_l2" = "/10",
    "size_of_batch" = "(log\\u2082)",
    "number_of_epochs" = "/10\\u00b2"
  )

  # Apply transformations to hyperparameters if applicable
  transformed_hyper_list <- list()
  for (hp_name in names(hyper_list)) {
    if (hp_name %in% names(transformation_rules)) {
      # Apply the transformation
      transformed_hyper_list[[hp_name]] <- hyper_list[[hp_name]]
      transformed_hyper_list[[hp_name]]$pars <- sapply(hyper_list[[hp_name]]$pars, transformation_rules[[hp_name]])
      # Update the hyperparameter name to reflect the transformation
      names(transformed_hyper_list)[names(transformed_hyper_list) == hp_name] <- paste0(hp_name, transformation_labels[[hp_name]])
    } else {
      transformed_hyper_list[[hp_name]] <- hyper_list[[hp_name]]  # No transformation
    }
  }

  # Prepare a data frame to store the samples
  plot_data <- data.frame(
    Hyperparameter = character(),
    Value = numeric(),
    histogram = character()
  )

  # Prepare a data frame for predefined limits
  predefined_data <- data.frame(
    Hyperparameter = character(),
    MinAllowed = numeric(),
    MaxAllowed = numeric()
  )

  # Loop through each hyperparameter to generate samples and check predefined limits
  for (hp_name in names(transformed_hyper_list)) {
    dist_choice <- transformed_hyper_list[[hp_name]]$distribution_choice
    if (dist_choice == "uniform") {
      pars <- transformed_hyper_list[[hp_name]]$pars
      samples <- stats::runif(n_iter, min = pars["min"], max = pars["max"])
    } else if (dist_choice == "normal") {
      pars <- transformed_hyper_list[[hp_name]]$pars
      samples <- stats::rnorm(n_iter, mean = pars["mean"], sd = pars["sd"])
    } else if (dist_choice == "lognormal") {
      pars <- transformed_hyper_list[[hp_name]]$pars
      samples <- stats::rlnorm(n_iter, meanlog = pars["meanlog"], sdlog = pars["sdlog"])
    } else if (dist_choice == "constant") {
      samples <- rep(transformed_hyper_list[[hp_name]]$value, n_iter)  # Constant value
    } else {
      stop(paste("Unsupported histogram:", dist_choice))
    }

    # Add the samples to the plot_data
    plot_data <- rbind(plot_data, data.frame(
      Hyperparameter = rep(hp_name, length(samples)),
      Value = samples,
      histogram = rep(dist_choice, length(samples))
    ))

    # If predefined limits exist, add them to predefined_data
    if (hp_name %in% names(predefined_limits)) {
      predefined_data <- rbind(predefined_data, data.frame(
        Hyperparameter = hp_name,
        MinAllowed = predefined_limits[[hp_name]][1],
        MaxAllowed = predefined_limits[[hp_name]][2]
      ))
    }
  }

  # Create the plot with hyperparameters on the y-axis
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Value, y = Hyperparameter, fill = Hyperparameter)) +
    ggplot2::geom_violin(data = plot_data[plot_data$histogram != "constant", ], alpha = 0.7, color = neon_cyan) +  # Violin for non-constant histograms
    ggplot2::geom_point(data = plot_data[plot_data$histogram == "constant", ], size = 3, color = neon_purple, shape = 4) +  # Points for constant in white
    ggplot2::geom_point(data = predefined_data, ggplot2::aes(x = MinAllowed, y = Hyperparameter), shape = 124, size = 5, color = neon_green) +  # Min always closed
    ggplot2::geom_point(data = predefined_data, ggplot2::aes(x = MaxAllowed, y = Hyperparameter), shape = 124, size = 5, color = neon_green) +  # Max always closed
    ggplot2::labs(title = "Random Search: Hyperparameter histograms",
                  y = "Hyperparameter",
                  x = "Sampled Values") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = blue_bg, color = NA),
      panel.background = ggplot2::element_rect(fill = blue_bg, color = NA),
      plot.title = ggplot2::element_text(color = white, size = 16, face = "bold"),
      axis.text = ggplot2::element_text(color = white),
      axis.title = ggplot2::element_text(color = white),
      legend.position = "bottom",  # Adjust legend position if needed
      legend.title = ggplot2::element_text(color = white),
      legend.text = ggplot2::element_text(color = white),
      panel.grid.major = ggplot2::element_line(color = light_gray),  # Set major grid lines to light gray
      panel.grid.minor = ggplot2::element_line(color = light_gray)   # Set minor grid lines to light gray
    ) +
    ggplot2::scale_fill_manual(values = c(neon_cyan, neon_blue, white, vibrant_purple, neon_green, neon_orange, neon_pink, neon_yellow,
                                          neon_purple)) +  # Use defined colors
    ggplot2::guides(fill = ggplot2::guide_none())  # Remove legend for fill and shape

  print(p)
})


#' @title Plot Method for `bayesian_opt_strategy`
#' @description Plot the bounds for each hyperparameter in `bayesian_opt_strategy`.
#' @param x An object of class `bayesian_opt_strategy`.
#' @param y Unused. Included for consistency with the generic `plot` method.
#' @param ... Additional arguments passed to the plotting method (currently unused).
#' @return A `ggplot` object visualizing the bounds.
#' @export
setMethod("plot", signature(x = "bayesian_opt_strategy", y = "missing"), function(x, y, ...) {

  #Check for packages
  if (!requireNamespace("gridExtra", quietly = TRUE) || !requireNamespace("scales", quietly = TRUE)) {
    stop("Packages 'gridExtra' and 'scales' are required to generate plots. Please install them using install.packages().")
  }


  # Define neon colors for plotting
  neon_blue <- "#00BFFF"
  neon_pink <- "#FF1493"
  neon_yellow <- "#FFFF00"
  neon_purple <- "#8A2BE2"
  neon_orange <- "#FF4500"
  neon_green <- "#39FF14"
  blue_bg <- "#001f3f"
  light_gray <- "#B0B0B0"
  black <- "#000000"
  white <- "#FFFFFF"



  # Extract hyper_grid_domain and the hyperparameter list
  hyper_grid <- x@hyper_grid_domain
  hyper_list <- hyper_grid@hyperparameter_list

  if (length(hyper_list) == 0) {
    stop("No hyperparameters found in the grid domain.")
  }

  # Define the predefined min and max ranges for certain hyperparameters
  predefined_limits <- list(
    "alpha" = c(0, 1),
    "lambda.min.ratio" = c(0, 1),
    "mtry" = c(0, 1),
    "eta" = c(0, 1),
    "colsample_bytree" = c(0, 1),
    "subsample" = c(0, 1),
    "droprate" = c(0, 1)
  )

  # Define transformations for specific hyperparameters
  transformation_rules <- list(
    "num.trees" = function(x) x / 100,
    "max.depth" = function(x) x / 10,
    "min.bucket" = function(x) x / 10,
    "min_child_weight" = function(x) x / 10,
    "gamma" = function(x) x / 10,
    "nrounds" = function(x) x / 100,
    "regularizer_l1" = function(x) x / 10,
    "regularizer_l2" = function(x) x / 10,
    "size_of_batch" = function(x) log(x, base = 2),
    "number_of_epochs" = function(x) x / 100
  )

  # Define labels for each transformation
  transformation_labels <- list(
    "num.trees" = "/10\u00b2",           # ² → \u00b2
    "max.depth" = "/10",
    "min.bucket" = "/10",
    "min_child_weight" = "/10",
    "gamma" = "/10",
    "nrounds" = "/10\u00b2",             # ² → \u00b2
    "regularizer_l1" = "/10",
    "regularizer_l2" = "/10",
    "size_of_batch" = "(log\u2082)",     # ₂ → \u2082
    "number_of_epochs" = "/10\u00b2"     # ² → \u00b2
  )


  # Apply transformations to hyperparameters if applicable
  transformed_hyper_list <- list()
  for (hp_name in names(hyper_list)) {
    if (hp_name %in% names(transformation_rules)) {
      # Apply the transformation
      transformed_hyper_list[[hp_name]] <- sapply(hyper_list[[hp_name]], transformation_rules[[hp_name]])
      # Update the hyperparameter name to reflect the transformation
      names(transformed_hyper_list)[names(transformed_hyper_list) == hp_name] <- paste0(hp_name, transformation_labels[[hp_name]])
    } else {
      transformed_hyper_list[[hp_name]] <- hyper_list[[hp_name]]  # No transformation
    }
  }

  # Prepare the data frame for plotting
  plot_data <- do.call(rbind, lapply(names(transformed_hyper_list), function(hp) {
    bounds <- transformed_hyper_list[[hp]]

    # Check if there are predefined limits
    if (hp %in% names(predefined_limits)) {
      limits <- predefined_limits[[hp]]
      min_val <- limits[1]
      max_val <- limits[2]

      data.frame(
        hyperparameter = hp,
        lower = bounds[1],
        upper = bounds[2],
        min_val = min_val,
        max_val = max_val
      )
    } else {
      data.frame(
        hyperparameter = hp,
        lower = bounds[1],
        upper = bounds[2],
        min_val = NA,
        max_val = NA
      )
    }
  }))

  # Create the ggplot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(y = hyperparameter)) +
    # Add segments for bounds with neon blue lines
    ggplot2::geom_segment(ggplot2::aes(x = lower, xend = upper, yend = hyperparameter), color = neon_blue, size = 0.8) +
    # Add points for bounds
    ggplot2::geom_point(ggplot2::aes(x = lower), size = 3, color = neon_green, shape = 16) +  # Closed for lower bounds
    ggplot2::geom_point(ggplot2::aes(x = upper), size = 3, color = neon_green, shape = 16) +  # Closed for upper bounds
    # Add predefined limits (min and max)
    ggplot2::geom_point(data = plot_data[!is.na(plot_data$min_val), ], ggplot2::aes(x = min_val, y = hyperparameter), size = 5, color = neon_pink, shape = 124) +  # Neon pink for predefined min
    ggplot2::geom_point(data = plot_data[!is.na(plot_data$max_val), ], ggplot2::aes(x = max_val, y = hyperparameter), size = 5, color = neon_pink, shape = 124) +  # Neon pink for predefined max
    # Adjust plot labels
    ggplot2::labs(
      title = "Bayesian Optimization: Hyperparameter Bounds",
      x = "Bounded Intervals",
      y = "Hyperparameter"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = blue_bg, color = NA),
      panel.background = ggplot2::element_rect(fill = blue_bg, color = NA),
      plot.title = ggplot2::element_text(color = white, size = 16, face = "bold"),
      axis.text = ggplot2::element_text(color = white),
      axis.title = ggplot2::element_text(color = white),
      legend.position = "bottom",  # Adjust legend position if needed
      legend.title = ggplot2::element_text(color = white),
      legend.text = ggplot2::element_text(color = white),
      panel.grid.major = ggplot2::element_line(color = light_gray),  # Set major grid lines to light gray
      panel.grid.minor = ggplot2::element_line(color = light_gray)   # Set minor grid lines to light gray
    )

  print(p)
})


#' @title Plot Method for `sb_backtest_config`
#' @description Calls the appropriate plot method for `tuning_strategy`.
#' @param x An object of class `sb_backtest_config`.
#' @param y Unused. Included for consistency with the generic `plot` method.
#' @return A `ggplot` object visualizing the hyperparameter histograms with possible limits.
#' @export
setMethod("plot", signature(x = "sb_backtest_config", y = "missing"), function(x, y){

  #Check for packages
  if (!requireNamespace("gridExtra", quietly = TRUE) || !requireNamespace("scales", quietly = TRUE)) {
    stop("Packages 'gridExtra' and 'scales' are required to generate plots. Please install them using install.packages().")
  }

  if(!x@sb_algorithm %in% c("ols", "sw", "ew", "rp", "mvo", "custom_weights")){
    plot(x@tuning_strategy)
  } else {
    message("Plot method not avaiable for `ols`, `sw`, `ew`, `rp` or `mvo` sb_algorithm.")
  }

})


#' @title Plot Method for `sb_metabacktest_config`
#' @description Allows the user to plot either the base learners or the meta learner configurations.
#' If `base_sb_backtest_results` is provided, it extracts the configurations using `get_sb_backtest_config`.
#'
#' @param x An object of class `sb_metabacktest_config`.
#' @param y Unused. Included for consistency with the generic `plot` method.
#' @param plot_id A character string or numeric value specifying which plot to display.
#'   If `NULL`, the user is prompted to select one from an interactive menu.
#' @param ... Additional arguments (currently unused).
#'
#' @return A combined `ggplot` object visualizing the hyperparameter histograms for the selected configurations.
#' @export
setMethod("plot", signature(x = "sb_metabacktest_config", y = "missing"), function(x, y, plot_id = NULL, ...) {

  #Check for packages
  if (!requireNamespace("gridExtra", quietly = TRUE) || !requireNamespace("scales", quietly = TRUE)) {
    stop("Packages 'gridExtra' and 'scales' are required to generate plots. Please install them using install.packages().")
  }

  # List of available plots
  available_plots <- c(
    "Combined and Consolidated OOS Testing Metrics - All Dates",
    "Combined and Averaged OOS Testing Metrics - Common Dates",
    "Time Series OOS Testing Metrics",
    "Mean Validation Metrics Comparison",
    "Time Series Validation Metrics",
    "Prediction Error Correlation",
    "Base and Meta Learners Hyperparameters"
  )

  # Display the available plots and prompt the user if plot_id is NULL
  if (is.null(plot_id)) {
    cat("\nPlease choose a plot to display:\n")
    for (i in seq_along(available_plots)) {
      cat(paste0(i, ": ", available_plots[i], "\n"))
    }
    selection <- readline(prompt = "Enter the number of your choice: ")
    plot_id <- as.numeric(selection)
    if (is.na(plot_id) || plot_id < 1 || plot_id > length(available_plots)) {
      stop("Invalid selection.")
    }
  }

  # Determine if plot_id is numeric (index) or character (name)
  if (is.numeric(plot_id)) {
    if (plot_id >= 1 && plot_id <= length(available_plots)) {
      plot_name <- available_plots[plot_id]
    } else {
      stop("Invalid plot number. Please select a number between 1 and ", length(available_plots), ".")
    }
  } else if (is.character(plot_id)) {
    if (plot_id %in% available_plots) {
      plot_name <- plot_id
    } else {
      stop("Invalid 'plot_id' specified. Available options are:\n",
           paste(available_plots, collapse = ", "))
    }
  } else {
    stop("'plot_id' must be either a string or a number corresponding to the plot.")
  }


  if (plot_name %in% available_plots[c(1:6)]){

    if (is.null(x@base_sb_backtest_results)){
      stop("The selected plot depends on the existence of base sb backtest results")
    }

    #Get base sb backtest results
    all_sb_backtest_results <- x@base_sb_backtest_results
    base_sb_names <- sapply(all_sb_backtest_results, function(x) x@backtest_identifier)

    #Consolidate them as in create_sb_metabacktest_results
    results <- consolidate_sb_metabacktest_results(
      all_sb_backtest_results = all_sb_backtest_results,
      meta_sb_name = NULL,
      base_sb_names = base_sb_names
    )

    #Call inner plot method
    plot_consolidated_sb_backtest_results(
      combined_metrics = list(all_dates_oos_testing_metrics = results$all_dates_oos_testing_metrics,
                              common_dates_oos_testing_metrics = results$common_dates_oos_testing_metrics),
      mean_validation_metrics = results$mean_validation_metrics,
      time_series_oos_testing_metrics = results$time_series_oos_testing_metrics,
      time_series_validation_metrics = results$time_series_validation_metrics,
      base_learners = all_sb_backtest_results,
      plot_name = plot_name
    )


    #Plot base and meta learners hyperparameters
  } else if (plot_name == "Base and Meta Learners Hyperparameters") {
    cat("\nPlease choose what to plot:\n")
    cat("1: Base Learners\n")
    cat("2: Meta Learner\n")
    cat("3: Both\n")
    selection <- readline(prompt = "Enter the number of your choice: ")
    selection <- as.numeric(selection)
    if (selection == 1) {
      which <- "base"
    } else if (selection == 2) {
      which <- "meta"
    } else if (selection == 3) {
      which <- "both"
    } else {
      stop("Invalid selection.")
    }

    # Initialize a list to hold plots
    plot_list <- list()

    # Plot base learners if requested
    if (which %in% c("base", "both")) {
      # Determine whether to use configs or results
      if (!is.null(x@base_sb_backtest_configs)) {
        base_configs <- x@base_sb_backtest_configs
      } else if (!is.null(x@base_sb_backtest_results)) {
        # Use get_sb_backtest_config to extract configs from results
        base_configs <- lapply(x@base_sb_backtest_results, get_sb_backtest_config)
      } else {
        stop("No base_sb_backtest_configs or base_sb_backtest_results found in the object.")
      }

      # Loop through each sb_backtest_config in the base_configs
      for (config in base_configs) {
        # Check if the algorithm is not OLS
        if (config@sb_algorithm != "ols") {
          # Create the plot using the existing plot method for sb_backtest_config
          p <- plot(config)  # Call the existing plot method

          # Store the plot in the list and add a centered subtitle
          plot_list[[length(plot_list) + 1]] <- p +
            ggplot2::ggtitle(paste("Algorithm:", config@sb_algorithm,
                                   if(config@sb_algorithm == "nn") paste("with", length(config@keras_architecture_parameters@units), "layers"),
                                   "  | Custom Obj:", config@custom_objective,
                                   "\nTuning Strategy:", config@tuning_strategy@tuning_method,
                                   "  |  Chosen Eval Metric:", config@tuning_strategy@chosen_eval_metric,
                                   "  |  Val Sample Size:", config@tuning_strategy@validation_sample_size)
            ) +  # Combined title
            ggplot2::theme(
              plot.title = ggplot2::element_text(size = 10),
              plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "white", size = 9),
              panel.border = ggplot2::element_rect(color = "white", fill = NA, size = 1)  # Add white border
            )
        } else {
          message("Skipping plotting for `ols` sb_algorithm in one of the configs.")
        }
      }
    }

    # Plot meta learner if requested
    if (which %in% c("meta", "both")) {
      meta_config <- x@meta_sb_backtest_config
      # Check if the algorithm is not OLS
      if (meta_config@sb_algorithm != "ols") {
        # Create the plot using the existing plot method for sb_backtest_config
        p <- plot(meta_config)  # Call the existing plot method

        # Store the plot in the list and add a centered subtitle
        plot_list[[length(plot_list) + 1]] <- p +
          ggplot2::ggtitle(paste("Meta Learner - Algorithm:", meta_config@sb_algorithm,
                                 if(meta_config@sb_algorithm == "nn") paste("with", length(meta_config@keras_architecture_parameters@units), "layers"),
                                 "  | Custom Obj:", meta_config@custom_objective,
                                 "\nTuning Strategy:", meta_config@tuning_strategy@tuning_method,
                                 "  |  Chosen Eval Metric:", meta_config@tuning_strategy@chosen_eval_metric,
                                 "  |  Val Sample Size:", meta_config@tuning_strategy@validation_sample_size)
          ) +  # Combined title
          ggplot2::theme(
            plot.title = ggplot2::element_text(size = 10),
            plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "white", size = 9),
            panel.border = ggplot2::element_rect(color = "white", fill = NA, size = 1)  # Add white border
          )
      } else {
        message("Skipping plotting for `ols` sb_algorithm in the meta learner config.")
      }
    }

    # Check if there are any plots to display
    if (length(plot_list) > 0) {
      # Create an empty plot for the main title with the blue background
      title_text <- switch(which,
                           "base" = "SB Metabacktest Base Learner Tuning Strategies",
                           "meta" = "SB Metabacktest Meta Learner Tuning Strategy",
                           "both" = "SB Metabacktest Tuning Strategies"
      )

      title_plot <- ggplot2::ggplot() +
        ggplot2::theme_void() +  # No grid lines or axes
        ggplot2::annotate("text", x = 0.5, y = 0.5,  # Centered position
                          label = title_text,
                          size = 6.25, color = "white", fontface = "bold", hjust = 0.5) +  # Centered title
        ggplot2::theme(plot.background = ggplot2::element_rect(fill = "#001f3f"))  # Set background to blue_bg

      # Determine the number of columns in the layout
      ncol_layout <- ifelse(length(plot_list) > 1, 2, 1)

      # Combine all individual plots into a single grid layout
      combined_plot <- gridExtra::grid.arrange(
        title_plot,
        do.call(gridExtra::arrangeGrob, c(plot_list, ncol = ncol_layout)),
        nrow = 2,
        heights = c(0.1, 0.9)  # Adjust height ratios for title and plots
      )

      # Display the combined plot
      grid::grid.newpage()
      grid::grid.draw(combined_plot)

    }


  } else {
    stop("No valid plots available to display.")
  }

})




#' @title Plot Method for 'sb_model' Objects
#' @description This method dispatches plotting to the underlying model stored in the \code{model} slot of a \code{sb_model} object.
#'
#' @param x An object of class \code{sb_model}.
#' @param type Currently unused. Included for compatibility with other plot methods.
#' @param ... Additional arguments passed to the plot method of the underlying model.
#'
#' @export
setMethod(
  "plot",
  signature(x = "sb_model", y = "missing"),
  function(x, type = NULL, ...) {

    plot(x@model)

  }
)


# Define the plot method for sb_backtest_results
################################
#' Plot Signal Blending Walk-Forward Validation Results
#'
#' This method generates various plots to visualize the performance of machine learning models using walk-forward validation metrics.
#' Users can select which plot to display by specifying the `plot_id` parameter,
#' either by name or by number.
#'
#' @param x An object of class \code{sb_backtest_results} containing the results of the walk-forward validation.
#' @param plot_id A character string or numeric value specifying which plot to display.
#'   - By name: Options are:
#'     - `"Chosen Evaluation Metric Over Time"`
#'     - `"Test vs Validation Chosen Evaluation Metric Over Time"`
#'     - `"Best Hyperparameters Over Time"`
#'     - `"Hyperparameters vs Error"`
#'     - `"All Evaluation Metrics Over Time"`
#'     - `"Consolidated OOS Testing Metrics"`
#'     - `"Average Validation Metrics"`
#'     - `"Signal Portfolio"`
#'   - By number: Provide a number corresponding to the plot (as listed when `plot_id` is `NULL`).
#'   If `NULL` (default), the method lists available plots.
#' @param features_m_df A \code{meta_dataframe} containing features used in the backtest. Required for plots like `"Explain Prediction"`.
#'
#' @return Invisibly returns the input \code{x}.
#' @export

setMethod("plot", "sb_backtest_results", function(x, plot_id = NULL, features_m_df = NULL) {

  #Check for packages
  if (!requireNamespace("gridExtra", quietly = TRUE) || !requireNamespace("scales", quietly = TRUE)) {
    stop("Packages 'gridExtra' and 'scales' are required to generate plots. Please install them using install.packages().")
  }

  # List of available plots
  sb_backtest_workflow <- x@sb_backtest_workflow[[length(x@sb_backtest_workflow)]]
  if(sb_backtest_workflow$sb_algorithm %in% c("glmnet", "rf", "xgb", "nn")){
    available_plots <- c(
      "Chosen Evaluation Metric Over Time",
      "Test vs Validation Chosen Evaluation Metric Over Time",
      "Best Hyperparameters Over Time",
      "Hyperparameters vs Error",
      "All Evaluation Metrics Over Time",
      "Consolidated OOS Testing Metrics",
      "OOS Predictions, Errors and Targets",
      "Consolidated OOS Testing Metrics vs Average Validation",
      "Final Signal-Blending Model",
      "Time-Series Feature Importance by Signal",
      "Average Time-Series Feature Importance by Theme",
      "Compare Feature Importance Side-by-Side by Signal",
      "Compare Feature Importance Side-by-Side by Theme",
      "Feature Importance Box-Plot by Signal",
      "Feature Importance Box-Plot by Theme",
      "Feature Importance Heatmap by Signal",
      "Feature Importance Heatmap by Theme",
      "Explain Prediction"
    )
  }

  if(sb_backtest_workflow$sb_algorithm %in% c("ols", "ew_ensemble", "optimal_ensemble", "ew", "sw", "rp", "mvo", "custom_weights")){
    available_plots <- c(
      "All Evaluation Metrics Over Time",
      "Consolidated OOS Testing Metrics",
      "OOS Predictions, Errors and Targets",
      "Final Signal-Blending Model",
      "Time-Series Feature Importance by Signal",
      "Average Time-Series Feature Importance by Theme",
      "Compare Feature Importance Side-by-Side by Signal",
      "Compare Feature Importance Side-by-Side by Theme",
      "Feature Importance Box-Plot by Signal",
      "Feature Importance Box-Plot by Theme",
      "Feature Importance Heatmap by Signal",
      "Feature Importance Heatmap by Theme",
      "Explain Prediction"
    )
  }


  if (is.null(plot_id)) {
    cat("\nPlease choose a plot to display:\n")
    for (i in seq_along(available_plots)) {
      cat(paste0(i, ": ", available_plots[i], "\n"))
    }
    selection <- readline(prompt = "Enter the number of your choice: ")
    plot_id <- as.numeric(selection)
    if (is.na(plot_id) || plot_id < 1 || plot_id > length(available_plots)) {
      stop("Invalid selection.")
    }
  }

  # Determine if plot_id is numeric (index) or character (name)
  if (is.numeric(plot_id)) {
    if (plot_id >= 1 && plot_id <= length(available_plots)) {
      plot_name <- available_plots[plot_id]
    } else {
      stop("Invalid plot number. Please select a number between 1 and ", length(available_plots), ".")
    }
  } else if (is.character(plot_id)) {
    if (plot_id %in% available_plots) {
      plot_name <- plot_id
    } else {
      stop("Invalid 'plot_id' specified. Available options are:\n",
           paste(available_plots, collapse = ", "))
    }
  } else {
    stop("'plot_id' must be either a string or a number corresponding to the plot.")
  }

  # Extract relevant data from the S4 object
  oos_testing_eval_metrics <- if (!is.null(x@oos_testing_eval_metrics_m_xts)) x@oos_testing_eval_metrics_m_xts@data %>% as.data.frame() else NULL
  validation_eval_metrics_hyper_choice <- if (!is.null(x@validation_eval_metrics_hyper_choice_m_xts)) x@validation_eval_metrics_hyper_choice_m_xts@data %>% as.data.frame() else NULL
  consolidated_eval_metrics <- x@consolidated_eval_metrics
  hyper_choice_df <- if (!is.null(x@best_hyperparameters_m_xts)) x@best_hyperparameters_m_xts@data %>% as.data.frame() else NULL
  chosen_eval_metric <- sb_backtest_workflow$chosen_eval_metric
  chosen_eval_metric_validation <- x@chosen_eval_metric_validation
  sb_algorithm <- sb_backtest_workflow$sb_algorithm
  rebalance_dates <- sb_backtest_workflow$rebalance_dates

  # Define color palette
  neon_blue <- "#00BFFF"
  neon_pink <- "#FF1493"
  neon_yellow <- "#FFFF00"
  neon_purple <- "#8A2BE2"
  neon_orange <- "#FF4500"
  neon_green <- "#39FF14"
  blue_bg <- "#001f3f"
  faint_blue <- "#003366"
  light_gray <- "#B0B0B0"
  black <- "#000000"
  white <- "#FFFFFF"
  neon_hot_pink <- "#FF69B4"      # Hot Pink
  neon_lime_green <- "#32CD32"    # Lime Green
  neon_bright_orange <- "#FFA500" # Bright Orange

  # Extended neon palette with 9 colors
  extended_neon_palette <- c(
    neon_blue, neon_pink, neon_yellow, neon_green, neon_orange, neon_purple,
    neon_hot_pink, neon_lime_green, neon_bright_orange
  )

  # Define global variables to pass R CMD check
  dates <- value <- year <- overall_mean <- yearly_mean <- quantile <- concatenation <- median <- lambda.min.ratio <- alpha <- q75 <- median_chosen_eval_metric <-
    q25 <- variable_mean <- x_coord <- y_coord <- label <- variable <- mtry <- num.trees <- min.bucket <- max.depth <- eta <- max_depth <- colsample_bytree <-
    lr <- droprate <- regularizer_l2 <- regularizer_l1 <- min_child_weight <- subsample <- gamma <- nrounds <- NULL

  # Get 'consolidated' row
  consolidated_oos_testing_metrics <- consolidated_eval_metrics %>% dplyr::select(metric, cons_oos)

  # Get 'average' row from validation_eval_metrics_hyper_choice before time series plots
  if (length(validation_eval_metrics_hyper_choice) > 0) {
    average_validation_metrics <-  consolidated_eval_metrics %>% dplyr::select(metric, avg_val)
  } else {
    average_validation_metrics <- NULL
  }

  # Prepare data for plotting

  # Some treatments to oos_testing_eval_metrics
  # Change colnames
  if (!is.null(oos_testing_eval_metrics)){
    colnames(oos_testing_eval_metrics) <- paste("oos_testing_", colnames(oos_testing_eval_metrics), sep = "")
    # Add dates column
    oos_testing_eval_metrics <- oos_testing_eval_metrics %>% dplyr::mutate(dates = rownames(oos_testing_eval_metrics))
    oos_testing_eval_metrics$dates <- as.Date(oos_testing_eval_metrics$dates, format = "%Y-%m-%d") # Coerce to dates
  }

  if (!sb_algorithm %in% c("ols", "rp", "sw", "ew", "mvo", "custom_weights")) {
    # Some treatments to validation_eval_metrics_hyper_choice
    # Change colnames
    colnames(validation_eval_metrics_hyper_choice) <- paste("validation_", colnames(validation_eval_metrics_hyper_choice), sep = "")
    # Add dates column
    validation_eval_metrics_hyper_choice <- validation_eval_metrics_hyper_choice %>% dplyr::mutate(dates = rownames(validation_eval_metrics_hyper_choice))
    validation_eval_metrics_hyper_choice$dates <- as.Date(validation_eval_metrics_hyper_choice$dates, format = "%Y-%m-%d") # Coerce to dates

    # Join test and validation
    if (!is.null(oos_testing_eval_metrics)){
      oos_testing_and_validation <- dplyr::left_join(oos_testing_eval_metrics, validation_eval_metrics_hyper_choice, by = 'dates')
      # Melt
      oos_testing_and_validation <- oos_testing_and_validation %>%
        tidyr::pivot_longer(
          cols = -dates,
          names_to = "variable",
          values_to = "value"
        )

      oos_testing_and_validation$dates <- as.Date(oos_testing_and_validation$dates, format = "%Y-%m-%d")

      # OOS test data
      oos_testing_data <-  oos_testing_and_validation %>%
        dplyr::filter(stringr::str_detect(variable, "oos_testing_")) %>% # Filter only OOS test
        dplyr::mutate(year = lubridate::year(dates)) %>% # Using lubridate::year() to extract year
        dplyr::group_by(variable) %>% # Group by variable
        dplyr::mutate(variable_mean = mean(value, na.rm = TRUE)) %>% # Take variable mean
        dplyr::ungroup() # Ungroup

      # Chosen eval metric - test data
      chosen_eval_testing_data <- oos_testing_and_validation %>%
        dplyr::filter(variable == paste("oos_testing_", chosen_eval_metric, sep = "")) %>% # Filter only OOS test chosen eval metric
        dplyr::mutate(year = lubridate::year(dates)) %>% # Using lubridate::year() to extract year
        dplyr::mutate(overall_mean = mean(value, na.rm = TRUE)) %>% # Add overall mean
        dplyr::group_by(year) %>% # Group by year
        dplyr::mutate(yearly_mean = mean(value, na.rm = TRUE)) %>% # Take yearly mean
        dplyr::ungroup() # Ungroup

      # Validation data
      validation_data <- oos_testing_and_validation %>%
        dplyr::filter(stringr::str_detect(variable, "validation_")) %>% # Filter only validation
        dplyr::mutate(year = lubridate::year(dates)) %>% # Using lubridate::year() to extract year
        dplyr::filter(!is.na(value)) # Filter out NAs

      # Chosen eval metric - validation data
      chosen_eval_validation_data <- oos_testing_and_validation %>%
        dplyr::filter(variable == paste("validation_", chosen_eval_metric, sep = "")) %>% # Filter only chosen validation metric
        dplyr::filter(!is.na(value)) # Filter out NAs

    } else {
      if (plot_name %in% c("Chosen Evaluation Metric Over Time", "Test vs Validation Chosen Evaluation Metric Over Time",
                           "All Evaluation Metrics Over Time", "Consolidated OOS Testing Metrics")){
        stop("Chosen evaluation metric over time plot requires a non NULL oos_testing_eval_metrics.")
      }
    }
  }

  # Initialize plots list
  plots_list <- list()

  # Now generate the selected plot
  if (plot_name == "Chosen Evaluation Metric Over Time") {
    # PLOT 1: Test chosen evaluation metric over time
    plots_list$chosen_eval_metric_over_time <- ggplot2::ggplot(
      chosen_eval_testing_data,
      ggplot2::aes(x = dates, y = value, color = paste(chosen_eval_metric))
    ) +
      ggplot2::geom_line(color = neon_blue, alpha = 0.7) +  # Neon cyan line
      ggplot2::geom_point(color = neon_green) +  # Neon green points
      ggplot2::labs(x = "Date", y = chosen_eval_metric) +
      ggplot2::ggtitle(paste("Test", chosen_eval_metric, "over time")) +
      ggplot2::facet_wrap(~year, scales = "free") +
      ggplot2::scale_x_date(labels = scales::date_format("%b-%y")) +
      ggplot2::geom_hline(
        data = chosen_eval_testing_data %>%
          dplyr::select(year, overall_mean, yearly_mean) %>%
          dplyr::distinct(),
        ggplot2::aes(yintercept = overall_mean),
        color = neon_pink,
        linetype = "dashed"
      ) +
      ggplot2::geom_hline(
        data = chosen_eval_testing_data %>%
          dplyr::select(year, overall_mean, yearly_mean) %>%
          dplyr::distinct(),
        ggplot2::aes(yintercept = yearly_mean),
        color = neon_purple,
        linetype = "dashed"
      ) +
      #ggplot2::scale_color_manual(values = c("Metric" = neon_blue)) +
      ggplot2::guides(color = ggplot2::guide_legend(title = "")) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = blue_bg, color = NA),
        panel.background = ggplot2::element_rect(fill = blue_bg, color = NA),
        plot.title = ggplot2::element_text(color = white, size = 16, face = "bold"),
        axis.text = ggplot2::element_text(color = white),
        axis.title = ggplot2::element_text(color = white),
        strip.text = ggplot2::element_text(color = white, face = "bold"),  # White facet labels
        legend.position = "bottom",
        legend.title = ggplot2::element_text(color = white),
        legend.text = ggplot2::element_text(color = white),
        panel.grid.major = ggplot2::element_line(color = faint_blue, size = 0.2),  # Blended blue for major grid
        panel.grid.minor = ggplot2::element_line(color = faint_blue, size = 0.1)   # Blended blue for minor grid
      )

    print(plots_list$chosen_eval_metric_over_time)

  } else if (plot_name == "Test vs Validation Chosen Evaluation Metric Over Time") {
    # PLOT 2: Test vs Validation chosen eval metric over time
    plots_list$test_vs_val_chosen_eval_metric_over_time <- ggplot2::ggplot() +
      ggplot2::geom_line(
        data = chosen_eval_testing_data,
        ggplot2::aes(x = dates, y = value, color = "Test"),
        alpha = 0.5
      ) +
      ggplot2::geom_point(
        data = chosen_eval_testing_data,
        ggplot2::aes(x = dates, y = value, color = "Test"),
        size = 2,
        color = neon_green
      ) +
      ggplot2::geom_point(
        data = chosen_eval_validation_data,
        ggplot2::aes(x = dates, y = value, color = "Validation"),
        size = 3
      ) +
      ggplot2::geom_text(
        data = chosen_eval_validation_data,
        ggplot2::aes(x = dates, y = value, label = dates),
        vjust = -0.5, hjust = 0, size = 3, color = neon_yellow
      ) +  # Display dates next to validation points
      ggplot2::labs(x = "Date", y = chosen_eval_metric, color = "") +
      ggplot2::ggtitle(paste("Test and Validation", chosen_eval_metric, "over time")) +
      ggplot2::scale_color_manual(values = c("Test" = neon_blue, "Validation" = neon_yellow)) +
      ggplot2::geom_vline(
        data = chosen_eval_validation_data,
        ggplot2::aes(xintercept = dates),
        linetype = "dashed",
        color = neon_orange
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = blue_bg, color = NA),
        panel.background = ggplot2::element_rect(fill = blue_bg, color = NA),
        plot.title = ggplot2::element_text(color = white, size = 16, face = "bold"),
        axis.text = ggplot2::element_text(color = white),
        axis.title = ggplot2::element_text(color = white),
        strip.text = ggplot2::element_text(color = white, face = "bold"),
        legend.position = "bottom",
        legend.title = ggplot2::element_text(color = white),
        legend.text = ggplot2::element_text(color = white),
        panel.grid.major = ggplot2::element_line(color = faint_blue, size = 0.2),  # Blended grid lines
        panel.grid.minor = ggplot2::element_line(color = faint_blue, size = 0.1)
      )

    print(plots_list$test_vs_val_chosen_eval_metric_over_time)

  } else if (plot_name == "Best Hyperparameters Over Time") {
    # PLOT 3: Best Hyperparameters over time
    plots_list$best_hyper_over_time <- ggplot2::ggplot(
      hyper_choice_df %>%
        dplyr::mutate(dates = as.Date(rownames(hyper_choice_df), format = "%Y-%m-%d")) %>%
        tidyr::pivot_longer(
          cols = -dates,
          names_to = "variable",
          values_to = "value"
        ),
      ggplot2::aes(x = dates, y = value, color = variable)
    ) +
      ggplot2::geom_line(alpha = 0.5) +
      ggplot2::geom_point(color = neon_green) +  # Neon green points
      ggplot2::geom_text(
        ggplot2::aes(label = round(value, 2)),
        vjust = -0.5, size = 3, color = white
      ) +  # Display values next to points
      ggplot2::labs(x = "Date", y = "Best Hyperparameter") +
      ggplot2::ggtitle("Best Hyperparameters Over Time") +
      ggplot2::facet_wrap(~variable, scales = "free") +  # Create subplots for each hyperparameter
      ggplot2::scale_x_date(labels = scales::date_format("%b-%y")) +
      ggplot2::scale_color_manual(values = extended_neon_palette) +
      ggplot2::guides(color = ggplot2::guide_legend(title = "")) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = blue_bg, color = NA),
        panel.background = ggplot2::element_rect(fill = blue_bg, color = NA),
        plot.title = ggplot2::element_text(color = white, size = 16, face = "bold"),
        axis.text = ggplot2::element_text(color = white),
        axis.title = ggplot2::element_text(color = white),
        strip.text = ggplot2::element_text(color = white, face = "bold"),  # White facet labels
        legend.position = "bottom",
        legend.title = ggplot2::element_text(color = white),
        legend.text = ggplot2::element_text(color = white),
        panel.grid.major = ggplot2::element_line(color = faint_blue, size = 0.2),  # Blended grid lines
        panel.grid.minor = ggplot2::element_line(color = faint_blue, size = 0.1)
      )

    # Print the plot
    suppressMessages(
      print(plots_list$best_hyper_over_time)
    )

  } else if (plot_name == "Hyperparameters vs Error") {
    # PLOT 4: Hyperparameters vs Error
    plots_list$hyper_vs_error <- NULL  # Initialize

    # Transform the list into a big rbinded data frame
    chosen_eval_metric_validation_df <- do.call(rbind, chosen_eval_metric_validation)

    # For each column of hyperparameters, turn into categories
    hyper_cols <- setdiff(names(chosen_eval_metric_validation_df), "chosen_eval_metric")
    for (j in hyper_cols) {
      tryCatch({
        chosen_eval_metric_validation_df[, j] <- as.factor( # As category
          cut(
            chosen_eval_metric_validation_df[, j],
            breaks = unique(stats::quantile(chosen_eval_metric_validation_df[, j], probs = seq(0, 1, by = 0.1), na.rm = TRUE)),
            include.lowest = TRUE
          )
        )
      }, error = function(e) {
        message(paste("Only one unique value identified for", j))
        chosen_eval_metric_validation_df[, j] <- chosen_eval_metric_validation_df[, j]
      })
    }

    # Concatenation based on algorithm
    if (sb_algorithm == "glmnet") {
      chosen_eval_metric_validation_df$concatenation <- paste(
        chosen_eval_metric_validation_df$alpha,
        chosen_eval_metric_validation_df$lambda.min.ratio,
        sep = "_"
      )
    } else if (sb_algorithm == "rf") {
      chosen_eval_metric_validation_df$concatenation <- paste(
        chosen_eval_metric_validation_df$mtry,
        chosen_eval_metric_validation_df$num.trees,
        chosen_eval_metric_validation_df$max.depth,
        chosen_eval_metric_validation_df$min.bucket,
        sep = "_"
      )
    } else if (sb_algorithm == "xgb") {
      chosen_eval_metric_validation_df$concatenation <- paste(
        chosen_eval_metric_validation_df$eta,
        chosen_eval_metric_validation_df$gamma,
        sep = "_"
      )
    } else if (sb_algorithm == "nn") {
      chosen_eval_metric_validation_df$concatenation <- paste(
        chosen_eval_metric_validation_df$lr,
        chosen_eval_metric_validation_df$droprate,
        sep = "_"
      )
    } else {
      # For other algorithms, concatenate all hyperparameters
      chosen_eval_metric_validation_df$concatenation <- apply(
        chosen_eval_metric_validation_df[, hyper_cols],
        1,
        paste,
        collapse = "_"
      )
    }

    # Summarize main quantiles
    chosen_eval_metric_validation_summary <- as.data.frame(
      chosen_eval_metric_validation_df %>%
        dplyr::group_by(concatenation) %>% # Take by group of hyper combinations
        dplyr::summarise(
          median_chosen_eval_metric = stats::median(chosen_eval_metric, na.rm = TRUE), # Q50
          q25 = stats::quantile(chosen_eval_metric, 0.25, na.rm = TRUE),              # Q25
          q75 = stats::quantile(chosen_eval_metric, 0.75, na.rm = TRUE),              # Q75
          max = max(chosen_eval_metric, na.rm = TRUE),                               # Q100
          min = min(chosen_eval_metric, na.rm = TRUE),                               # Q0
          .groups = 'drop'
        )
    )
    # Join with summary
    chosen_eval_metric_validation_df <- chosen_eval_metric_validation_df %>%
      dplyr::left_join(chosen_eval_metric_validation_summary, by = "concatenation")

    # Take last hyper tuning
    total_rows <- nrow(chosen_eval_metric_validation_df)
    tuning_period <- nrow(chosen_eval_metric_validation[[length(chosen_eval_metric_validation)]])
    start_row <- max(total_rows - tuning_period + 1, 1)
    chosen_eval_metric_validation_last_tuning <- chosen_eval_metric_validation_df[start_row:total_rows, ]

    # Generate Hyperparameters vs Error plot based on algorithm
    if (sb_algorithm == "glmnet") {
      # Ensure there are enough colors
      num_alphas <- length(unique(chosen_eval_metric_validation_last_tuning$alpha))
      fill_colors <- extended_neon_palette[1:num_alphas]

      plots_list$hyper_vs_error <- ggplot2::ggplot(
        chosen_eval_metric_validation_last_tuning,
        ggplot2::aes(x = lambda.min.ratio, y = chosen_eval_metric, fill = as.factor(alpha))
      ) +
        ggplot2::geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
        ggplot2::facet_grid(rows = ggplot2::vars(alpha)) +
        ggplot2::geom_point(
          ggplot2::aes(y = max),
          color = neon_yellow,
          size = 2
        ) +
        ggplot2::geom_point(
          ggplot2::aes(y = q75),
          color = neon_orange,
          size = 2
        ) +
        ggplot2::geom_point(
          ggplot2::aes(y = median_chosen_eval_metric),
          color = neon_green,
          size = 2
        ) +
        ggplot2::geom_point(
          ggplot2::aes(y = q25),
          color = neon_purple,
          size = 2
        ) +
        ggplot2::geom_point(
          ggplot2::aes(y = min),
          color = neon_blue,
          size = 2
        ) +
        ggplot2::ggtitle(paste("Validation", chosen_eval_metric, "by alpha and lambda.min.ratio")) +
        ggplot2::scale_fill_manual(values = fill_colors) +
        ggplot2::guides(fill = ggplot2::guide_legend(title = NULL)) +  # Remove legend title
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.background = ggplot2::element_rect(fill = blue_bg, color = NA),
          panel.background = ggplot2::element_rect(fill = blue_bg, color = NA),
          plot.title = ggplot2::element_text(color = white, size = 16, face = "bold"),
          axis.text = ggplot2::element_text(color = white),
          axis.title = ggplot2::element_text(color = white),
          strip.text = ggplot2::element_text(color = white, face = "bold"),
          legend.position = "bottom",
          legend.title = ggplot2::element_text(color = white),
          legend.text = ggplot2::element_text(color = white),
          panel.grid.major = ggplot2::element_line(color = faint_blue, size = 0.2),
          panel.grid.minor = ggplot2::element_line(color = faint_blue, size = 0.1),
          plot.caption = ggplot2::element_text(color = white, hjust = 0)
        ) +
        ggplot2::labs(caption = "Dots represent quantiles (min, Q25, Q50, Q75, max) of all rebalancing periods.")

      print(plots_list$hyper_vs_error)
    }

    # RF case
    if (sb_algorithm == "rf") {
      # Ensure there are enough colors
      num_mtrys <- length(unique(chosen_eval_metric_validation_last_tuning$mtry))
      fill_colors <- extended_neon_palette[1:num_mtrys]

      plots_list$hyper_vs_error <- ggplot2::ggplot(
        chosen_eval_metric_validation_last_tuning,
        ggplot2::aes(x = mtry, y = chosen_eval_metric, fill = as.factor(mtry))
      ) +
        ggplot2::geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
        ggplot2::facet_grid(rows = ggplot2::vars(max.depth), cols = ggplot2::vars(min.bucket)) +
        ggplot2::geom_point(
          ggplot2::aes(y = max),
          color = neon_yellow,
          size = 2
        ) +
        ggplot2::geom_point(
          ggplot2::aes(y = q75),
          color = neon_orange,
          size = 2
        ) +
        ggplot2::geom_point(
          ggplot2::aes(y = median_chosen_eval_metric),
          color = neon_green,
          size = 2
        ) +
        ggplot2::geom_point(
          ggplot2::aes(y = q25),
          color = neon_purple,
          size = 2
        ) +
        ggplot2::geom_point(
          ggplot2::aes(y = min),
          color = neon_pink,
          size = 2
        ) +
        ggplot2::ggtitle(paste("Validation", chosen_eval_metric, "by max.depth and min.bucket")) +
        ggplot2::scale_fill_manual(values = fill_colors) +
        ggplot2::guides(fill = ggplot2::guide_legend(title = NULL)) +  # Remove legend title
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.background = ggplot2::element_rect(fill = blue_bg, color = NA),
          panel.background = ggplot2::element_rect(fill = blue_bg, color = NA),
          plot.title = ggplot2::element_text(color = white, size = 16, face = "bold"),
          axis.text = ggplot2::element_text(color = white),
          axis.title = ggplot2::element_text(color = white),
          strip.text = ggplot2::element_text(color = white, face = "bold"),
          legend.position = "bottom",
          legend.title = ggplot2::element_text(color = white),
          legend.text = ggplot2::element_text(color = white),
          panel.grid.major = ggplot2::element_line(color = faint_blue, size = 0.2),
          panel.grid.minor = ggplot2::element_line(color = faint_blue, size = 0.1),
          plot.caption = ggplot2::element_text(color = white, hjust = 0)
        ) +
        ggplot2::labs(caption = "Dots represent quantiles (min, Q25, Q50, Q75, max) of all rebalancing periods.")

      print(plots_list$hyper_vs_error)
    }

    # XGB case
    if (sb_algorithm == "xgb") {
      # Ensure there are enough colors
      num_etas <- length(unique(chosen_eval_metric_validation_last_tuning$eta))
      fill_colors <- extended_neon_palette[1:num_etas]

      plots_list$hyper_vs_error <- ggplot2::ggplot(
        chosen_eval_metric_validation_last_tuning,
        ggplot2::aes(x = eta, y = chosen_eval_metric, fill = as.factor(eta))
      ) +
        ggplot2::geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
        ggplot2::facet_grid(rows = ggplot2::vars(max_depth), cols = ggplot2::vars(colsample_bytree)) +
        ggplot2::geom_point(
          ggplot2::aes(y = max),
          color = neon_yellow,
          size = 2
        ) +
        ggplot2::geom_point(
          ggplot2::aes(y = q75),
          color = neon_orange,
          size = 2
        ) +
        ggplot2::geom_point(
          ggplot2::aes(y = median_chosen_eval_metric),
          color = neon_green,
          size = 2
        ) +
        ggplot2::geom_point(
          ggplot2::aes(y = q25),
          color = neon_purple,
          size = 2
        ) +
        ggplot2::geom_point(
          ggplot2::aes(y = min),
          color = neon_blue,
          size = 2
        ) +
        ggplot2::ggtitle(paste("Validation", chosen_eval_metric, "by max_depth and colsample_bytree")) +
        ggplot2::scale_fill_manual(values = fill_colors) +
        ggplot2::guides(fill = ggplot2::guide_legend(title = NULL)) +  # Remove legend title
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.background = ggplot2::element_rect(fill = blue_bg, color = NA),
          panel.background = ggplot2::element_rect(fill = blue_bg, color = NA),
          plot.title = ggplot2::element_text(color = white, size = 16, face = "bold"),
          axis.text = ggplot2::element_text(color = white),
          axis.title = ggplot2::element_text(color = white),
          strip.text = ggplot2::element_text(color = white, face = "bold"),
          legend.position = "bottom",
          legend.title = ggplot2::element_text(color = white),
          legend.text = ggplot2::element_text(color = white),
          panel.grid.major = ggplot2::element_line(color = faint_blue, size = 0.2),
          panel.grid.minor = ggplot2::element_line(color = faint_blue, size = 0.1),
          plot.caption = ggplot2::element_text(color = white, hjust = 0)
        ) +
        ggplot2::labs(caption = "Dots represent quantiles (min, Q25, Q50, Q75, max) of all rebalancing periods.")

      print(plots_list$hyper_vs_error)
    }

    # NN case
    if (sb_algorithm == "nn") {
      # Ensure there are enough colors
      num_lrs <- length(unique(chosen_eval_metric_validation_last_tuning$lr))
      fill_colors <- extended_neon_palette[1:num_lrs]

      plots_list$hyper_vs_error <- ggplot2::ggplot(
        chosen_eval_metric_validation_last_tuning,
        ggplot2::aes(x = lr, y = chosen_eval_metric, fill = as.factor(lr))
      ) +
        ggplot2::geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
        ggplot2::facet_grid(rows = ggplot2::vars(droprate), cols = ggplot2::vars(regularizer_l1)) +
        ggplot2::geom_point(
          ggplot2::aes(y = max),
          color = neon_yellow,
          size = 2
        ) +
        ggplot2::geom_point(
          ggplot2::aes(y = q75),
          color = neon_orange,
          size = 2
        ) +
        ggplot2::geom_point(
          ggplot2::aes(y = median_chosen_eval_metric),
          color = neon_green,
          size = 2
        ) +
        ggplot2::geom_point(
          ggplot2::aes(y = q25),
          color = neon_purple,
          size = 2
        ) +
        ggplot2::geom_point(
          ggplot2::aes(y = min),
          color = neon_blue,
          size = 2
        ) +
        ggplot2::ggtitle(paste("Validation", chosen_eval_metric, "by droprate and regularizer_l1")) +
        ggplot2::scale_fill_manual(values = fill_colors) +
        ggplot2::guides(fill = ggplot2::guide_legend(title = NULL)) +  # Remove legend title
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.background = ggplot2::element_rect(fill = blue_bg, color = NA),
          panel.background = ggplot2::element_rect(fill = blue_bg, color = NA),
          plot.title = ggplot2::element_text(color = white, size = 16, face = "bold"),
          axis.text = ggplot2::element_text(color = white),
          axis.title = ggplot2::element_text(color = white),
          strip.text = ggplot2::element_text(color = white, face = "bold"),
          legend.position = "bottom",
          legend.title = ggplot2::element_text(color = white),
          legend.text = ggplot2::element_text(color = white),
          panel.grid.major = ggplot2::element_line(color = faint_blue, size = 0.2),
          panel.grid.minor = ggplot2::element_line(color = faint_blue, size = 0.1),
          plot.caption = ggplot2::element_text(color = white, hjust = 0)
        ) +
        ggplot2::labs(caption = "Dots represent quantiles (min, Q25, Q50, Q75, max) of all rebalancing periods.")

      print(plots_list$hyper_vs_error)
    }

  } else if (plot_name == "All Evaluation Metrics Over Time") {
    # PLOT 5: All Evaluation Metrics Over Time
    plots_list$all_eval_metrics_over_time <- ggplot2::ggplot(
      oos_testing_eval_metrics %>%
        tidyr::pivot_longer(
          cols = -dates,
          names_to = "variable",
          values_to = "value"
        ) %>%
        dplyr::mutate(variable = gsub("^oos_testing_", "", variable)) %>%  # Remove prefix
        dplyr::filter(!is.na(value)),
      ggplot2::aes(x = dates, y = value, color = variable)
    ) +
      ggplot2::geom_line(alpha = 0.5, show.legend = FALSE) +
      ggplot2::geom_point(size = 2, show.legend = FALSE) +
      ggplot2::labs(x = "Date", y = "Metric") +
      ggplot2::ggtitle("All Evaluation Metrics Over Time") +
      ggplot2::facet_wrap(~variable, scales = "free") +  # Create subplots for each metric with simplified titles
      ggplot2::scale_x_date(labels = scales::date_format("%b-%y")) +
      ggplot2::scale_color_manual(values = extended_neon_palette) +
      ggplot2::geom_vline(xintercept = as.numeric(rebalance_dates), color = neon_yellow, linetype = "dashed") +
      ggplot2::geom_text(
        data = data.frame(x = rebalance_dates, y = -Inf, label = rebalance_dates),
        ggplot2::aes(x = x, y = y, label = label),
        vjust = -0.5, hjust = -0.5, size = 3, color = white
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = blue_bg, color = NA),
        panel.background = ggplot2::element_rect(fill = blue_bg, color = NA),
        plot.title = ggplot2::element_text(color = white, size = 16, face = "bold"),
        axis.text = ggplot2::element_text(color = white),
        axis.title = ggplot2::element_text(color = white),
        strip.text = ggplot2::element_text(color = white, face = "bold"),
        legend.position = "none",
        panel.grid.major = ggplot2::element_line(color = faint_blue, size = 0.2),
        panel.grid.minor = ggplot2::element_line(color = faint_blue, size = 0.1)
      )

    # Add horizontal lines for variable means
    # Create a summary data frame for variable means
    summary_data <- oos_testing_eval_metrics %>%
      tidyr::pivot_longer(
        cols = -dates,
        names_to = "variable",
        values_to = "value"
      ) %>%
      dplyr::mutate(variable = gsub("^oos_testing_", "", variable)) %>%
      dplyr::group_by(variable) %>%
      dplyr::summarise(variable_mean = mean(value, na.rm = TRUE))

    plots_list$all_eval_metrics_over_time <- plots_list$all_eval_metrics_over_time +
      ggplot2::geom_hline(
        data = summary_data,
        ggplot2::aes(yintercept = variable_mean, color = variable),
        linetype = "dashed",
        show.legend = FALSE
      )

    print(plots_list$all_eval_metrics_over_time)

  } else if (plot_name == "Consolidated OOS Testing Metrics") {

    #Palette
    my_colors <- c(
      neon_blue,
      neon_pink,
      neon_yellow,
      neon_purple,
      neon_orange,
      neon_green,
      neon_hot_pink,
      neon_lime_green,
      neon_bright_orange
    )

    # Add an ID column
    consolidated_oos_testing_metrics$id <- "Consolidated OOS"

    # Melt the consolidated data
    consolidated_data <- tidyr::pivot_longer(
      data = consolidated_oos_testing_metrics,
      cols = -c(metric, id),
      names_to = "Discard_Column",
      values_to = "Value"
    )
    # Remove the unneeded column
    consolidated_data$Discard_Column <- NULL

    # Rename columns for clarity
    colnames(consolidated_data) <- c("Metric", "Type", "Value")

    # Convert Type to a factor with only "Consolidated OOS"
    consolidated_data$Metric <- factor(
      consolidated_data$Metric,
      levels = unique(consolidated_data$Metric)
    )

    # Create the facetted bar chart using only consolidated_data
    plots_list$consolidated_oos_faceted <- ggplot2::ggplot(
      consolidated_data,
      # Notice we use fill = Metric so each metric gets a different color
      ggplot2::aes(x = Type, y = Value, fill = Metric)
    ) +
      ggplot2::geom_bar(stat = "identity", alpha = 0.9) +
      ggplot2::facet_wrap(
        ~ Metric,
        scales = "free_y",
        strip.position = "bottom"
      ) +
      ggplot2::labs(
        title = "Consolidated OOS Testing Metrics",
        x = NULL,
        y = "Metric Value",
        fill = "Metric"
      ) +
      # Manual color scale with your neon palette
      ggplot2::scale_fill_manual(values = my_colors) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        strip.placement = "outside",  # Put metric labels below the facets
        plot.background = ggplot2::element_rect(fill = blue_bg, color = NA),
        panel.background = ggplot2::element_rect(fill = blue_bg, color = NA),
        plot.title = ggplot2::element_text(color = white, size = 16, face = "bold", hjust = 0.5),
        axis.text = ggplot2::element_text(color = white),
        axis.title = ggplot2::element_text(color = white),
        legend.title = ggplot2::element_text(color = white),
        legend.text = ggplot2::element_text(color = white),
        legend.position = "bottom",
        strip.text = ggplot2::element_text(color = white, face = "bold"),
        panel.grid.major = ggplot2::element_line(color = faint_blue, size = 0.2),
        panel.grid.minor = ggplot2::element_line(color = faint_blue, size = 0.1)
      )

    print(plots_list$consolidated_oos_faceted)


  } else if (plot_name == "OOS Predictions, Errors and Targets"){

    #Ask if the user wants to plot a regression-scatterplot
    resp_scat <- readline(prompt = "Do you want to plot a regression scatterplot? (yes/no): ")

    if (resp_scat %in% c("yes", "y")){
      dep_y <- readline(prompt = "Which variable do you want to be the dependent one? Choose between error, target and pred:")

      ##Check
      if (!dep_y %in% c("error", "target", "pred")){
        stop("Please choose between error, target and pred.")
      }
      #Plot scatter
      plot(x@oos_sb_outputs_m_df, dep_y = dep_y, type = "regression")
    } else {
      #Plot others
      plot(x@oos_sb_outputs_m_df, dep_y = NULL)
    }


  } else if (plot_name == "Consolidated OOS Testing Metrics vs Average Validation") {
    # PLOT 6: Compare Average Validation Metrics with Consolidated OOS Testing Metrics

    # Check if both metrics are available
    if (is.null(consolidated_oos_testing_metrics) || is.null(average_validation_metrics)) {
      cat("Both consolidated OOS testing metrics and average validation metrics are required for this plot.\n")
    } else {
      # Add an id column to prevent the melt warning
      consolidated_oos_testing_metrics$id <- "Consolidated OOS"
      average_validation_metrics$id <- "Average Validation"


      # Melt both data frames to long format with specified id.vars
      consolidated_data <- tidyr::pivot_longer(
        data = consolidated_oos_testing_metrics,
        cols = -c(metric, id),
        names_to = "Discard_Column",
        values_to = "Value"
      )

      average_data <- tidyr::pivot_longer(
        data = average_validation_metrics,
        cols = -c(metric, id),
        names_to = "Discard_Column",
        values_to = "Value"
      )


      consolidated_data$Discard_Column <- NULL
      average_data$Discard_Column <- NULL

      # Bind the two data frames together
      plot_data <- dplyr::bind_rows(consolidated_data, average_data)
      colnames(plot_data) <- c("Metric", "Type", "Value")

      # Create the facetted bar chart
      plots_list$comparison_avg_val_oos_faceted <- ggplot2::ggplot(
        plot_data,
        ggplot2::aes(x = Type, y = Value, fill = Type)
      ) +
        ggplot2::geom_bar(
          stat = "identity",
          position = ggplot2::position_dodge(width = 0.8),
          alpha = 0.8
        ) +
        # Facet by Metric, with the strip label on the bottom:
        ggplot2::facet_wrap(
          ~ Metric,
          scales = "free_y",
          strip.position = "bottom"  # put the metric label beneath each plot
        ) +
        ggplot2::labs(
          title = "Consolidated OOS Testing Metrics vs Average Validation",
          # We remove the x-axis label to avoid duplication with the facet label
          x = NULL,
          y = "Metric Value",
          fill = "Metric Type"
        ) +
        ggplot2::scale_fill_manual(
          # Adjust to your custom color palette:
          values = extended_neon_palette[1:2]
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          # Ensure the facet labels are placed outside the panel:
          strip.placement = "outside",

          # Keep your existing background and text coloring:
          plot.background = ggplot2::element_rect(fill = blue_bg, color = NA),
          panel.background = ggplot2::element_rect(fill = blue_bg, color = NA),
          plot.title = ggplot2::element_text(
            color = white,
            size = 16,
            face = "bold",
            hjust = 0.5
          ),
          axis.text = ggplot2::element_text(color = white),
          axis.title = ggplot2::element_text(color = white),
          legend.title = ggplot2::element_text(color = white),
          legend.text = ggplot2::element_text(color = white),
          legend.position = "bottom",
          strip.text = ggplot2::element_text(color = white, face = "bold"),
          panel.grid.major = ggplot2::element_line(color = faint_blue, size = 0.2),
          panel.grid.minor = ggplot2::element_line(color = faint_blue, size = 0.1)
        )

      print(plots_list$comparison_avg_val_oos_faceted)
    }
  } else if (plot_name == "Final Signal-Blending Model"){

    tryCatch(
      {
      plot(x@final_sb_model@model)
      }, error = function(e){
        warning("Error when plotting subjacent final signal-blending model. The following message was displayed by subjacent function: \n",
            e$message, "\n")
      })

  #Feature Importance Plots
  ########################
  } else if (plot_name == "Time-Series Feature Importance by Signal"){

    plot_type <- "time_series"
    clustering_variables <- "tickers"
    calc_stat <- "mean"
    variable <- "normalized_importance"

    plot(x@feature_importance_m_df, variable = variable, type = plot_type, clustering_variables = clustering_variables)

  } else if (plot_name == "Average Time-Series Feature Importance by Theme"){

    if(!"theme" %in% colnames(x@feature_importance_m_df@data)){
      stop("The feature importance data does not contain a 'theme' column. Please review the signal selection process to ensure a 'theme' classification is provided. \n")
    }

    plot_type <- "time_series"
    clustering_variables <- "theme"
    calc_stat <- "mean"
    variable <- "normalized_importance"

    plot(x@feature_importance_m_df, variable = variable, type = plot_type, clustering_variables = clustering_variables)

  } else if (plot_name == "Compare Feature Importance Side-by-Side by Signal"){

    plot_type <- "cross_sectional"
    clustering_variables <- "tickers"
    calc_stat <- "mean"
    variable <- "normalized_importance"

    plot(x@final_feature_importance_m_d_ref, variable = variable, type = plot_type, clustering_variables = clustering_variables, calc_stat = calc_stat)

  } else if (plot_name == "Compare Feature Importance Side-by-Side by Theme"){

    if(!"theme" %in% colnames(x@feature_importance_m_df@data)){
      stop("The feature importance data does not contain a 'theme' column. Please review the signal selection process to ensure a 'theme' classification is provided. \n")
    }

    plot_type <- "cross_sectional"
    clustering_variables <- "theme"
    calc_stat <- "mean"
    variable <- "normalized_importance"

    plot(x@final_feature_importance_m_d_ref, variable = variable, type = plot_type, clustering_variables = clustering_variables, calc_stat = calc_stat)

  } else if (plot_name == "Feature Importance Box-Plot by Signal"){

    plot_type <- "boxplot"
    clustering_variables <- "tickers"
    variable <- "normalized_importance"

    plot(x@feature_importance_m_df, variable = variable, type = plot_type, clustering_variables = clustering_variables)

  } else if (plot_name == "Feature Importance Box-Plot by Theme"){

    if(!"theme" %in% colnames(x@feature_importance_m_df@data)){
      stop("The feature importance data does not contain a 'theme' column. Please review the signal selection process to ensure a 'theme' classification is provided. \n")
    }

    plot_type <- "boxplot"
    clustering_variables <- "theme"
    variable <- "normalized_importance"

    plot(x@final_feature_importance_m_d_ref, variable = variable, type = plot_type, clustering_variables = clustering_variables)

  } else if (plot_name == "Feature Importance Heatmap by Signal"){

    plot_type <- "tile_heatmap"
    clustering_variables <- "tickers"
    variable <- "normalized_importance"
    calc_stat <- "mean"

    plot(x@feature_importance_m_df, variable = variable, type = plot_type, clustering_variables = clustering_variables, calc_stat = calc_stat)

  } else if (plot_name == "Feature Importance Heatmap by Theme"){

    if(!"theme" %in% colnames(x@feature_importance_m_df@data)){
      stop("The feature importance data does not contain a 'theme' column. Please review the signal selection process to ensure a 'theme' classification is provided. \n")
    }

    plot_type <- "tile_heatmap"
    clustering_variables <- "theme"
    variable <- "normalized_importance"
    calc_stat <- "mean"

    plot(x@feature_importance_m_df, variable = variable, type = plot_type, clustering_variables = clustering_variables, calc_stat = calc_stat)

  } else if (plot_name == "Explain Prediction"){

    #Check for features_m_df
    if (is.null(features_m_df)){
      stop("The 'features_m_df' argument is required for this plot. Please provide a meta dataframe with the features used to implement the backtest \n")
    } else {
      if (!is_meta_dataframe(features_m_df)){
        stop("The 'features_m_df' argument must be a meta dataframe. Please review the documentation for more information \n")
      }
    }

    # Prompt for 'ticker'
      cat("Which ticker predictions do you want to explain? (Please select just one) \n")
      selection <- readline(prompt = "Enter your choice: ")
      if (length(selection) == 1 && nzchar(selection)) {
        selected_ticker <- selection
      } else {
        stop("Invalid ticker selection. Please select just one ticker. \n")
      }

    # Prompt for 'date'
     cat("Which date? (Please select just one in format YYYY-MM-DD) \n")
     selection <- readline(prompt = "Enter your choice: ")
     if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", selection)) {
       stop("Invalid date format. Use 'YYYY-MM-DD'.")
     }
     if (length(selection) == 1 && nzchar(selection)) {
       selected_date <- as.Date(selection)
     } else {
       stop("Invalid date selection. Please select just one date. \n")
     }

     #Explain
     explain_prediction(x, features_m_df = features_m_df, selected_ticker = selected_ticker, selected_date = selected_date)

  }

  invisible(x)
})



#' @title Plot Method for sb_metabacktest_results Class
#' @description Generates various plots to visualize the performance and metrics of meta-learning backtest results.
#' Users can select which plot to display by specifying the `plot_id` parameter,
#' either by name or by number.
#' The plots include comparisons between base learners and meta learners over time.
#'
#' @param x An object of class \code{sb_metabacktest_results} containing the results of the meta-learning backtests.
#' @param plot_id A character string or numeric value specifying which plot to display.
#'   - By name: Options are:
#'     - `"Consolidated OOS Testing Metrics Comparison"`
#'     - `"Time Series OOS Testing Metrics"`
#'     - `"Mean Validation Metrics Comparison"`
#'     - `"Time Series Validation Metrics"`
#'     - `"Base Learners vs Meta Learners Over Time"`
#'   - By number: Provide a number corresponding to the plot (as listed when `plot_id` is `NULL`).
#'   If `NULL` (default), the method lists available plots.
#' @return Invisibly returns the input \code{x}.
#' @export
setMethod("plot", "sb_metabacktest_results", function(x, plot_id = NULL) {

  #Check for packages
  if (!requireNamespace("gridExtra", quietly = TRUE) || !requireNamespace("scales", quietly = TRUE)) {
    stop("Packages 'gridExtra' and 'scales' are required to generate plots. Please install them using install.packages().")
  }

  # List of available plots
  available_plots <- c(
    "Combined and Consolidated OOS Testing Metrics - All Dates",
    "Combined and Averaged OOS Testing Metrics - Common Dates",
    "Time Series OOS Testing Metrics",
    "Mean Validation Metrics Comparison",
    "Time Series Validation Metrics",
    "Prediction Error Correlation",
    "Base Learners vs Meta Learners Over Time",
    "Hierarchical Feature Importance"
  )

  # Display the available plots and prompt the user if plot_id is NULL
  if (is.null(plot_id)) {
    cat("\nPlease choose a plot to display:\n")
    for (i in seq_along(available_plots)) {
      cat(paste0(i, ": ", available_plots[i], "\n"))
    }
    selection <- readline(prompt = "Enter the number of your choice: ")
    plot_id <- as.numeric(selection)
    if (is.na(plot_id) || plot_id < 1 || plot_id > length(available_plots)) {
      stop("Invalid selection.")
    }
  }

  # Determine if plot_id is numeric (index) or character (name)
  if (is.numeric(plot_id)) {
    if (plot_id >= 1 && plot_id <= length(available_plots)) {
      plot_name <- available_plots[plot_id]
    } else {
      stop("Invalid plot number. Please select a number between 1 and ", length(available_plots), ".")
    }
  } else if (is.character(plot_id)) {
    if (plot_id %in% available_plots) {
      plot_name <- plot_id
    } else {
      stop("Invalid 'plot_id' specified. Available options are:\n",
           paste(available_plots, collapse = ", "))
    }
  } else {
    stop("'plot_id' must be either a string or a number corresponding to the plot.")
  }

  # Define color palette
  neon_blue <- "#00BFFF"
  neon_pink <- "#FF1493"
  neon_yellow <- "#FFFF00"
  neon_purple <- "#8A2BE2"
  neon_orange <- "#FF4500"
  neon_green <- "#39FF14"
  blue_bg <- "#001f3f"
  faint_blue <- "#003366"
  black <- "#000000"
  white <- "#FFFFFF"
  neon_hot_pink <- "#FF69B4"      # Hot Pink
  neon_lime_green <- "#32CD32"    # Lime Green
  neon_bright_orange <- "#FFA500" # Bright Orange

  # Extended neon palette with 9 colors
  extended_neon_palette <- c(
    neon_blue, neon_pink, neon_yellow, neon_green, neon_orange, neon_purple,
    neon_hot_pink, neon_lime_green, neon_bright_orange
  )

  # Extract data from the sb_metabacktest_results object
  combined_metrics <- x@combined_oos_testing_metrics
  time_series_oos_testing_metrics <- x@time_series_oos_testing_metrics
  mean_validation_metrics <- x@mean_validation_metrics
  time_series_validation_metrics <- x@time_series_validation_metrics
  base_learners <- x@base_sb_backtest_results_list
  meta_learner <- x@meta_sb_backtest_results

  if (plot_name %in% available_plots[c(1:6)]){

    #Call method to general consolidate sb_backtest_results plots
    plot_consolidated_sb_backtest_results(combined_metrics = combined_metrics, mean_validation_metrics = mean_validation_metrics,
                                          time_series_oos_testing_metrics = time_series_oos_testing_metrics, time_series_validation_metrics = time_series_validation_metrics,
                                          base_learners = base_learners,
                                          plot_name = plot_name
                                          )

  } else if (plot_name == "Base Learners vs Meta Learners Over Time") {
    # Plot comparison of base learners and meta learners over time for a selected metric

    # Prompt user to select a metric
    available_metrics <- names(time_series_oos_testing_metrics)
    cat("\nAvailable metrics:\n")
    for (i in seq_along(available_metrics)) {
      cat(paste0(i, ": ", available_metrics[i], "\n"))
    }
    metric_selection <- readline(prompt = "Please select a metric by number: ")
    metric_selection <- as.numeric(metric_selection)
    if (is.na(metric_selection) || metric_selection < 1 || metric_selection > length(available_metrics)) {
      stop("Invalid metric selection.")
    }
    metric_name <- available_metrics[metric_selection]

    # Prepare data
    meta_xts <- time_series_oos_testing_metrics[[metric_name]]

    if (is.null(meta_xts@data)) {
      cat("Metric", metric_name, "not found in time series OOS testing metrics.\n")
      return(invisible(x))
    }

    base_learner_ids <- sapply(base_learners, function(bl) bl@backtest_identifier) #Get bae learner ids
    meta_learner_id <- meta_learner@backtest_identifier #Get meta learner id

    clustering_list <- list("meta_learner" = meta_learner_id, "base_learners" = base_learner_ids)

    # Use meta_xts plot method
    plot(x = meta_xts, clustering_list = clustering_list)

  }  else if (plot_name == "Hierarchical Feature Importance"){

    # Prompt User for Number of Top Features**
    num_features <- as.integer(readline(prompt = "Enter the number of top features to display: "))

    # Ensure valid input
    if (is.na(num_features) || num_features <= 0) {
      stop("Invalid input. Please enter a positive integer.")
    }

    # **1. Extract Meta-Learner Feature Importances**
    meta_importance_df <- x@meta_sb_backtest_results@final_feature_importance_m_d_ref@data

    # **NEW: Filter Meta-Learner Features Based on Importance**
    meta_importance_df <- meta_importance_df %>%
      dplyr::slice_max(order_by = abs(importance), n = num_features)

    # **2. Extract Base Learners' Feature Importances**
    # Get the list of base learners
    base_learners_feature_imp_list <- x@base_sb_backtest_results_list
    base_model_ids <- names(base_learners_feature_imp_list)

    # If base_model_ids are NULL or empty, assign default names
    if (is.null(base_model_ids) || any(base_model_ids == "")) {
      base_model_ids <- paste0("BaseLearner", seq_along(base_learners_feature_imp_list))
      names(base_learners_feature_imp_list) <- base_model_ids
    }

    # Filter Each Base Learner's Feature Importance
    base_learners_feature_imp <- lapply(base_learners_feature_imp_list, function(bl) {
      bl@final_feature_importance_m_d_ref@data %>%
        dplyr::slice_max(order_by = abs(importance), n = num_features)
    })

    # **3. Normalize Base Models' Relative Importances**
    base_importances_sum <- sapply(base_learners_feature_imp, function(df) {
      sum(abs(df$importance))
    })

    total_importance_sum <- sum(base_importances_sum)
    base_importances_norm <- base_importances_sum / total_importance_sum

    # **4. Prepare Links from Features to Base Learners**
    links_base <- mapply(
      FUN = function(base_df, base_model_id) {
        base_df %>%
          dplyr::mutate(
            from = tickers,  # Feature name
            to = base_model_id,  # Base learner ID
            value = importance  # Importance value
          ) %>%
          dplyr::select(from, to, value)
      },
      base_df = base_learners_feature_imp,
      base_model_id = base_model_ids,
      SIMPLIFY = FALSE
    ) %>%
      dplyr::bind_rows()

    # **5. Prepare Links from Features Directly to Meta-Learner**
    links_direct <- meta_importance_df %>%
      dplyr::mutate(
        from = tickers,  # Feature name
        to = "meta_learner",  # Meta-learner node
        value = importance  # Importance value
      ) %>%
      dplyr::select(from, to, value)

    # **6. Prepare Links from Base Learners to Meta-Learner**
    links_meta <- data.frame(
      from = base_model_ids,  # Base learner IDs
      to = "meta_learner",  # Meta-learner node
      value = base_importances_norm,  # Normalized importance values
      stringsAsFactors = FALSE
    )

    # **7. Combine All Links**
    links <- dplyr::bind_rows(links_base, links_direct, links_meta)

    # **8. Define Nodes for the Hierarchical Tree**
    features <- unique(c(links_base$from, links_direct$from))
    features <- setdiff(features, base_model_ids)
    base_learners <- base_model_ids
    meta_learner <- "meta_learner"

    nodes <- data.frame(
      name = unique(c(features, base_learners, meta_learner)),
      stringsAsFactors = FALSE
    )

    # **10. Categorize Nodes into Types**
    nodes <- nodes %>%
      dplyr::mutate(type = dplyr::case_when(
        name %in% features ~ "Feature",
        name %in% base_learners ~ "Base Learner",
        name == meta_learner ~ "Meta Learner",
        TRUE ~ "Other"
      ))

    # **11. Define Custom Vibrant Neon Color Palette**
    type_colors <- c(
      "Feature" = "#39FF14",      # Neon Green
      "Base Learner" = "#FF5F1F", # Neon Orange
      "Meta Learner" = "#FFDC00", # Neon Yellow
      "Other" = "#FF007F"          # Neon Pink
    )

    # **12. Create the Graph Object with igraph**
    graph <- igraph::graph_from_data_frame(d = links, vertices = nodes, directed = TRUE)

    # Extract unique base model IDs
    base_model_ids <- unique(igraph::V(graph)$name[grepl("^c:", igraph::V(graph)$name)])

    # Create a mapping (model_1, model_2, ...)
    base_model_mapping <- stats::setNames(paste0("model_", seq_along(base_model_ids)), base_model_ids)

    # Rename the nodes in the graph
    igraph::V(graph)$name <- ifelse(igraph::V(graph)$name %in% names(base_model_mapping),
                                    base_model_mapping[igraph::V(graph)$name],
                                    igraph::V(graph)$name)

    # Print the mapping correspondence
    cat("\nLegend:\n")
    for (i in seq_along(base_model_mapping)) {
      cat(paste0(base_model_mapping[i], ": ", names(base_model_mapping)[i], "\n"))
    }

    # **13. Adjust Layout to Position Meta-Learner Below Base Models**
    layout <- ggraph::create_layout(graph, layout = "tree")
    layout$y <- ifelse(layout$name == "meta_learner", min(layout$y) - 1, layout$y)

    # **14. Generate Graph Plot**
    p <- ggraph::ggraph(layout) +
      ggraph::geom_edge_link(
        ggplot2::aes(width = value, color = value, alpha = value),
        arrow = ggplot2::arrow(length = grid::unit(4, 'mm')),
        end_cap = ggraph::circle(3, 'mm')
      ) +
      ggraph::geom_node_point(ggplot2::aes(color = type), size = 5) +
      ggraph::geom_node_text(
        ggplot2::aes(label = name),
        color = "white",
        hjust = -0.1,
        vjust = 0.5,
        size = 3,
        fontface = "bold"
      ) +
      ggraph::scale_edge_width(range = c(0.5, 3)) +
      ggplot2::scale_color_manual(values = type_colors) +
      ggraph::scale_edge_color_continuous(
        low = "red",
        high = "blue",
        name = "Importance",
        guide = ggraph::guide_edge_colorbar()
      ) +
      ggraph::scale_edge_alpha_continuous(range = c(0.0001, 0.8)) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.position = "bottom",
        plot.margin = ggplot2::margin(5, 40, 5, 5),
        plot.background = ggplot2::element_rect(fill = "#001f3f", color = NA),
        panel.background = ggplot2::element_rect(fill = "#001f3f", color = NA),
        panel.grid = ggplot2::element_blank(),
        axis.text = ggplot2::element_blank(),
        axis.title = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        plot.title = ggplot2::element_text(color = "white", size = 16, face = "bold"),
        plot.subtitle = ggplot2::element_text(color = "white", size = 12, face = "italic"),
        legend.text = ggplot2::element_text(color = "white"),
        legend.title = ggplot2::element_text(color = "white")
      ) +
      ggplot2::labs(
        title = "Hierarchical Feature Importance",
        subtitle = x@meta_sb_backtest_results@backtest_identifier,
        edge_width = "Importance",
        edge_color = "Importance",
        edge_alpha = "Importance",
        color = "Node Type"
      )

    print(p)


  }

  invisible(x)
})


#' @title Plot Priors from ss_backtest_config
#' @description Plots the distribution curves for each prior in the `bayesian_model_parameters` inside a `ss_backtest_config` object.
#' @param x An object of class `ss_backtest_config` with a `bayesian_alpha_test_strategy` alpha test strategy.
#' @param y Not used. Included for S4 method compatibility.
#' @param ... Additional arguments (currently unused).
#' @rdname plot_ss_backtest_config
#' @export
setMethod("plot", "ss_backtest_config", function(x, ...) {

  #Check for packages
  if (!requireNamespace("gridExtra", quietly = TRUE) || !requireNamespace("scales", quietly = TRUE)) {
    stop("Packages 'gridExtra' and 'scales' are required to generate plots. Please install them using install.packages().")
  }

  # Validate input object
  if (!inherits(x@alpha_test_strategy, "bayesian_alpha_test_strategy")) {
    stop("This plot method is only applicable to 'bayesian_alpha_test_strategy' subclass.")
  }

  # Extract user priors
  user_priors <- x@alpha_test_strategy@bayesian_model_parameters@user_priors
  if (is.null(user_priors) || nrow(user_priors) == 0) {
    cat("No user priors available to plot.\n")
    return(invisible(NULL))
  }

  # Convert priors to data frame
  priors_df <- as.data.frame(user_priors, stringsAsFactors = FALSE)

  # Define color palette
  class_colors <- c(
    b = "#FF1493",  # Neon Pink
    sd = "#FFFF00", # Neon Yellow
    sigma = "#8A2BE2" # Neon Purple
  )

  # Helper function to parse prior strings
  parse_prior_string <- function(prior_str) {
    prior_str <- gsub(" ", "", prior_str)
    regex <- "^([a-zA-Z_]+)\\((.*)\\)$"
    matches <- regmatches(prior_str, regexec(regex, prior_str))[[1]]

    if (length(matches) < 3) return(NULL)

    dist_name <- matches[2]
    params_str <- matches[3]
    params <- strsplit(params_str, ",")[[1]]
    param_list <- as.numeric(gsub(".*=", "", params))

    list(dist = dist_name, params = param_list)
  }

  # Helper function to generate plot data
  generate_plot_data <- function(dist_name, params) {
    x_seq <- seq(-3, 3, length.out = 1000)
    density <- switch(
      dist_name,
      "normal" = stats::dnorm(x_seq, mean = params[1], sd = params[2]),
      "student_t" = stats::dt((x_seq - params[2]) / params[3], df = params[1]) / params[3],
      "cauchy" = stats::dcauchy(x_seq, location = params[1], scale = params[2]),
      "lognormal" = stats::dlnorm(x_seq, meanlog = params[1], sdlog = params[2]),
      "beta" = stats::dbeta(x_seq, shape1 = params[1], shape2 = params[2]),
      "exponential" = stats::dexp(x_seq, rate = params[1]),
      NULL
    )
    if (is.null(density)) return(NULL)
    data.frame(x = x_seq, density = density)
  }

  # Prepare an empty list to store plots
  plot_list <- list()

  common_x_range <- seq(-3, 3, length.out = 1000)

  # Iterate over each prior
  for (i in seq_len(nrow(priors_df))) {
    prior_row <- priors_df[i, ]

    prior_str <- prior_row$prior
    class_str <- prior_row$class
    coef_str <- prior_row$coef
    group_str <- prior_row$group

    # Create identification string by concatenating class, coef, and group
    id_components <- c(
      paste0("class: ", class_str),
      if (!is.na(coef_str) && coef_str != "") paste0("coef: ", coef_str) else NULL,
      if (!is.na(group_str) && group_str != "") paste0("group: ", group_str) else NULL
    )
    id_str <- paste(id_components, collapse = ", ")

    # Parse the prior string to extract distribution and parameters
    parsed_prior <- parse_prior_string(prior_str)
    if (is.null(parsed_prior)) {
      message(sprintf("Could not parse prior: '%s'. Skipping.", prior_str))
      next
    }

    dist_name <- parsed_prior$dist
    params <- parsed_prior$params

    # Handle 'lkj' distribution separately
    if (dist_name == "lkj") {
      # Provide an informative message
      message(sprintf("The 'lkj' prior (prior '%s' with %s) will be skipped.", prior_str, id_str))
      next
    }

    # Generate data for plotting based on distribution
    plot_data <- generate_plot_data(dist_name, params)
    if (is.null(plot_data)) {
      message(sprintf("Unsupported distribution '%s' for prior '%s' with %s. Skipping.", dist_name, prior_str, id_str))
      next
    }

    # Ensure the x-axis matches the common range
    plot_data <- merge(data.frame(x = common_x_range), plot_data, by = "x", all.x = TRUE)
    plot_data$density[is.na(plot_data$density)] <- 0

    # Create the plot with cyberpunk style
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = density)) +
      ggplot2::geom_line(color = class_colors[class_str], size = 1) +
      ggplot2::labs(
        title = paste0(prior_str, "\n", id_str),
        x = "Value", y = "Density"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = "#001f3f", color = NA),
        panel.background = ggplot2::element_rect(fill = "#001f3f", color = NA),
        plot.title = ggplot2::element_text(color = "#FFFFFF", size = 14, face = "bold"),
        axis.text = ggplot2::element_text(color = "#FFFFFF"),
        axis.title = ggplot2::element_text(color = "#FFFFFF"),
        legend.position = "none",
        panel.grid.major = ggplot2::element_line(color = "#003366", size = 0.2),
        panel.grid.minor = ggplot2::element_line(color = "#003366", size = 0.1)
      )

    # Store the plot in the list
    plot_list[[length(plot_list) + 1]] <- p
  }

  # Arrange and display the plots
  if (length(plot_list) > 0) {
    gridExtra::grid.arrange(grobs = plot_list, ncol = 1)
  } else {
    cat("No valid priors to plot.\n")
  }

  invisible(NULL)
})




#' @title Plot Bayesian Alpha Test Strategy Priors
#' @description Plots the prior distributions defined in a `bayesian_alpha_test_strategy` object.
#' @param x A `bayesian_alpha_test_strategy` object.
#' @param y Ignored. Included for S3 compatibility.
#' @param ... Additional arguments (currently unused).
#' @method plot bayesian_alpha_test_strategy
#' @export
setMethod("plot", "bayesian_alpha_test_strategy", function(x, ...) {

  #Check for packages
  if (!requireNamespace("gridExtra", quietly = TRUE) || !requireNamespace("scales", quietly = TRUE)) {
    stop("Packages 'gridExtra' and 'scales' are required to generate plots. Please install them using install.packages().")
  }

  if (is.null(x@bayesian_model_parameters)) {
    cat("No Bayesian model parameters to plot.\n")
    return(invisible(NULL))
  }

  # Call the plot method of bayesian_model_parameters
  plot(x@bayesian_model_parameters, ...)
})


#' @title Plot Method for ss_backtest_results Class
#' @description Generates various plots to visualize metrics from the `ss_backtest_results` object.
#' Users can select which plot to display by specifying the `plot_id` parameter.
#' @param x An object of class `ss_backtest_results`.
#' @param plot_id A character string or numeric value specifying which plot to display.
#' @return Invisibly returns the input object.
#' @export
setMethod("plot", "ss_backtest_results", function(x, plot_id = NULL) {

  #Check for packages
  if (!requireNamespace("gridExtra", quietly = TRUE) || !requireNamespace("scales", quietly = TRUE)) {
    stop("Packages 'gridExtra' and 'scales' are required to generate plots. Please install them using install.packages().")
  }

  # Define colors for plotting
  deep_navy <- "#000033"
  black <- "#000000"
  white <- "#FFFFFF"
  vibrant_purple <- "#6A0DAD"
  neon_green <- "#39FF14"
  neon_orange <- "#FF5F1F"
  blue_bg <- "#001f3f"
  neon_yellow <- "#FFDC00"
  neon_pink <- "#FF007F"
  cyan <- "#7FDBFF"

  # Ensure Contribution_Type uses "Positive" and "Negative" for consistency
  pos_color <- neon_green    # Define positive contribution color
  neg_color <- "red"         # Define negative contribution color

  # SS Workflow
  ss_backtest_workflow <- x@ss_backtest_workflow[[length(x@ss_backtest_workflow)]]

  # List of available plots
  if(ss_backtest_workflow$p_correction_method == "bayesian"){
  available_plots <- c(
    "Time-Series Metrics by Signal",
    "Average Time-Series Metrics by Theme",
    "Compare Metrics Side-by-Side by Signals",
    "Compare Metrics Side-by-Side by Theme",
    "Box-Plot by Theme",
    "Box-Plot by Eligibility",
    "Waterfall Plot by Signal",
    "Waterfall Plot by Theme",
    "Eligibility by Theme",
    "Posterior Individual Alphas",
    "Posterior Individual Betas",
    "Posterior Random Effects",
    "Waterfall Plot of Posterior Variance Components",
    "Posterior Regression Lines",
    "Waterfall Plot of Return Decomposition by Signal",
    "Posterior Individual Alpha Distributions by Theme and Signal"
  )
  } else {
    available_plots <- c(
      "Time-Series Metrics by Signal",
      "Average Time-Series Metrics by Theme",
      "Compare Metrics Side-by-Side by Signals",
      "Compare Metrics Side-by-Side by Theme",
      "Box-Plot by Theme",
      "Box-Plot by Eligibility",
      "Waterfall Plot by Signal",
      "Waterfall Plot by Theme",
      "Eligibility by Theme"
    )
  }

  if (is.null(plot_id)) {
    cat("\nPlease choose a plot to display:\n")
    for (i in seq_along(available_plots)) {
      cat(paste0(i, ": ", available_plots[i], "\n"))
    }
    selection <- readline(prompt = "Enter the number of your choice: ")
    plot_id <- as.numeric(selection)
    if (is.na(plot_id) || plot_id < 1 || plot_id > length(available_plots)) {
      stop("Invalid selection.")
    }
  }

  # Determine plot name from the selection
  if (is.numeric(plot_id)) {
    if (plot_id >= 1 && plot_id <= length(available_plots)) {
      plot_name <- available_plots[plot_id]
    } else {
      stop("Invalid plot number. Please select a valid plot.")
    }
  } else if (is.character(plot_id)) {
    if (plot_id %in% available_plots) {
      plot_name <- plot_id
    } else {
      stop("Invalid 'plot_id' specified. Available options are:\n",
           paste(available_plots, collapse = ", "))
    }
  } else {
    stop("'plot_id' must be either a string or a number corresponding to the plot.")
  }

  # Define the data sources
  signal_universe_m_df <- x@signal_universe_m_df
  signal_universe_m_df@data <- signal_universe_m_df@data %>% dplyr::filter(theme != "forced")
  final_signal_universe_m_d_ref <- x@final_signal_universe_m_d_ref
  final_signal_universe_m_d_ref@data <- final_signal_universe_m_d_ref@data %>% dplyr::filter(theme != "forced")

  if (x@p_correction_method == "bayesian"){
    #Reconstruct selected_signal_themes_m_d_ref
    selected_signal_themes_m_d_ref <- final_signal_universe_m_d_ref@data %>% dplyr::select(id, tickers, dates, theme)
    #Get brm_model
    brm_model <- x@bayesian_results$brm_model
    #Get theme-level parameters
    theme_level_intercept <- ss_backtest_workflow$theme_level_intercept
    theme_level_slope <- ss_backtest_workflow$theme_level_slope
    model_spec_theme_level <- paste0(theme_level_intercept, "_intercept_", theme_level_slope, "_slope")

    #priors
    if (is.null(x@bayesian_results$elected_priors)){
      elected_priors <- x@bayesian_results$brm_model$prior
    } else {
      elected_priors <- x@bayesian_results$elected_priors
    }
    #Extract tidy posteriores
    tidy_posteriors_list <- summarize_posteriors_draws(brm_model = brm_model,
                                                       selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref,
                                                       model_spec_theme_level = model_spec_theme_level)
  }

  # Plot 1: Time-Series Metrics by Ticker
  if (plot_name == "Time-Series Metrics by Signal") {

    plot_type <- "time_series"
    clustering_variables <- "tickers"
    calc_stat <- "mean"

    plot(signal_universe_m_df, type = plot_type, clustering_variables = clustering_variables)


  } else if (plot_name == "Average Time-Series Metrics by Theme") {
    # Plot 2: Average Time-Series Metrics by Theme

    plot_type <- "time_series"
    clustering_variables <- "theme"
    calc_stat <- "mean"

    plot(signal_universe_m_df, type = plot_type, clustering_variables = clustering_variables)


  } else if (plot_name == "Compare Metrics Side-by-Side by Signals") {
    # Plot 3: Compare Metrics Side-by-Side by Tickers

    plot_type <- "cross_sectional"
    clustering_variables <- "tickers"
    calc_stat <- "mean"

    plot(final_signal_universe_m_d_ref, type = plot_type, clustering_variables = clustering_variables, calc_stat = calc_stat)

  } else if (plot_name == "Compare Metrics Side-by-Side by Theme") {
    # Plot 4: Compare Metrics Side-by-Side by Theme

    plot_type <- "cross_sectional"
    clustering_variables <- "theme"
    calc_stat <- "mean"
    plot(final_signal_universe_m_d_ref, type = plot_type, clustering_variables = clustering_variables, calc_stat = calc_stat)


  } else if (plot_name == "Box-Plot by Theme") {
    # Plot 5: Box-Plot by Theme

    plot_type <- "boxplot"
    clustering_variables <- "theme"

    plot(final_signal_universe_m_d_ref, type = plot_type, clustering_variables = clustering_variables)

  } else if (plot_name == "Box-Plot by Eligibility") {
    # Plot 6: Box-Plot by Eligibility
    final_signal_universe_m_d_ref@data$eligibility <- ifelse(final_signal_universe_m_d_ref@data$is_eligible == 1, "elected", "not_elected")

    plot_type <- "boxplot"
    clustering_variables <- "eligibility"

    plot(final_signal_universe_m_d_ref, type = plot_type, clustering_variables = clustering_variables)

  } else if (plot_name == "Waterfall Plot by Signal") {

        # Plot 7: Waterfall Plot by Ticker
    if (ss_backtest_workflow$model_structure == "no_pooled"){
    final_signal_universe_m_d_ref@data <- final_signal_universe_m_d_ref@data %>%
      dplyr::mutate(mean_market_factor_proxy = mean(x@selected_market_factor_proxy_m_xts@data),
                    beta_x_mean_market_factor_proxy = beta * mean_market_factor_proxy,
                    residual = specific_risk)
    } else {
      final_signal_universe_m_d_ref@data <- final_signal_universe_m_d_ref@data %>%
        dplyr::mutate(mean_market_factor_proxy = mean(x@selected_market_factor_proxy_m_xts@data),
                      beta_x_mean_market_factor_proxy = individual_beta * mean_market_factor_proxy,
                      residual = specific_risk) %>%
        dplyr::rename(alpha = individual_alpha)
    }

    plot_type <- "waterfall"
    clustering_variables <- "tickers"
    variables <- c("alpha", "beta_x_mean_market_factor_proxy", "residual")
    calc_stat <- "mean"

    plot(final_signal_universe_m_d_ref, type = plot_type, clustering_variables = clustering_variables, variable = variables, calc_stat = calc_stat)


  } else if (plot_name == "Waterfall Plot by Theme") {
    # Plot 8: Waterfall Plot by Ticker
    if (ss_backtest_workflow$model_structure == "no_pooled"){
      final_signal_universe_m_d_ref@data <- final_signal_universe_m_d_ref@data %>%
        dplyr::mutate(mean_market_factor_proxy = mean(x@selected_market_factor_proxy_m_xts@data),
                      beta_x_mean_market_factor_proxy = beta * mean_market_factor_proxy,
                      residual = specific_risk)
    } else {
      final_signal_universe_m_d_ref@data <- final_signal_universe_m_d_ref@data %>%
        dplyr::mutate(mean_market_factor_proxy = mean(x@selected_market_factor_proxy_m_xts@data),
                      beta_x_mean_market_factor_proxy = individual_beta * mean_market_factor_proxy,
                      residual = specific_risk) %>%
        dplyr::rename(alpha = individual_alpha)
    }

    plot_type <- "waterfall"
    clustering_variables <- "theme"
    variables <- c("alpha", "beta_x_mean_market_factor_proxy", "residual")
    calc_stat <- "mean"

    plot(final_signal_universe_m_d_ref, type = plot_type, clustering_variables = clustering_variables, variable = variables, calc_stat = calc_stat)


  } else if (plot_name == "Eligibility by Theme") {
    #Plot 9
    signal_universe_m_df@data <- signal_universe_m_df@data %>%
      dplyr::mutate(eligibility = ifelse(is_eligible == 1, "elected", "not_elected"))

    plot_type <- "tile_heatmap"
    clustering_variables <- "tickers"
    variables <- "eligibility"
    calc_stat <- "mean"

    plot(signal_universe_m_df, type = plot_type, clustering_variables = clustering_variables, variable = variables, calc_stat = calc_stat)

  }

  ###############
  #Bayesian Plots
  ###############

  else if (plot_name == "Posterior Individual Alphas"){

    #Plot 10
    tidy_posterior_draws_intercept <- tidy_posteriors_list$tidy_posterior_draws_intercept
    # Prompt the user to select tickers
    available_tickers <- unique(tidy_posterior_draws_intercept$tickers)

    # Display available tickers with numbers
    cat("Available tickers:\n")
    for (i in seq_along(available_tickers)) {
      cat(i, ":", available_tickers[i], "\n")
    }

    # Ask user for input
    selected_numbers <- readline(prompt = "Enter the numbers corresponding to the tickers you want to plot, separated by commas: ")

    # Convert input to numeric and validate
    selected_numbers <- as.numeric(strsplit(selected_numbers, ",")[[1]])
    if (any(is.na(selected_numbers)) || !all(selected_numbers %in% seq_along(available_tickers))) {
      stop("Invalid input. Please enter valid numbers corresponding to the tickers.")
    }

    # Match the selected numbers to tickers
    selected_tickers <- available_tickers[selected_numbers]

    # Filter the data based on user selection
    filtered_data <- tidy_posterior_draws_intercept %>%
      dplyr::filter(tickers %in% selected_tickers)

    if (nrow(filtered_data) == 0) {
      stop("No matching tickers found. Please check your input.")
    }

    #Get theme column
    theme_ticker_key <- data.frame(tickers = paste0(selected_signal_themes_m_d_ref$theme, "_", selected_signal_themes_m_d_ref$tickers), theme = selected_signal_themes_m_d_ref$theme)
    filtered_data <- filtered_data %>%
      dplyr::left_join(theme_ticker_key, by = c("tickers" = "tickers"))

    # Extract themes from `elected_priors`
      ##Random Intercept or None
      if(model_spec_theme_level %in% c("random_intercept_fixed_slope", "fixed_intercept_fixed_slope")){
        prior_data <- elected_priors %>%
          dplyr::filter(class == "Intercept") %>%
          dplyr::rowwise() %>%
          dplyr::mutate(
            mean = as.numeric(stringr::str_extract(prior, "(?<=\\().+?(?=,)")), # Extract mean
            sd = as.numeric(stringr::str_extract(prior, "(?<=, ).+?(?=\\))"))   # Extract sd
          ) %>%
          dplyr::ungroup() %>%
          dplyr::select(mean, sd)

        prior_data <- data.frame(theme = unique(theme_ticker_key$theme), mean = prior_data$mean, sd = prior_data$sd)

      }

      ##Fixed intercepts or Fixed Intercepts and Slopes
      if(model_spec_theme_level %in% c("theme_specific_intercept_fixed_slope", "theme_specific_intercept_theme_specific_slope")){
      prior_data <- elected_priors %>%
        dplyr::filter(class == "b" & stringr::str_detect(coef, "^theme")) %>%
        dplyr::mutate(theme = stringr::str_remove(coef, "^theme")) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          mean = as.numeric(stringr::str_extract(prior, "(?<=\\().+?(?=,)")), # Extract mean
          sd = as.numeric(stringr::str_extract(prior, "(?<=, ).+?(?=\\))"))   # Extract sd
        ) %>%
        dplyr::ungroup() %>%
        dplyr::select(theme, mean, sd)
      }

    # Generate draws for each theme and convert to long format
    long_draws_data <- prior_data %>%
      dplyr::mutate(
        draws = purrr::map2(mean, stats::sd, ~ stats::rnorm(length(unique(filtered_data$.draw)), mean = .x, sd = .y))
      ) %>%
      tidyr::unnest(draws) %>%                # Expand the list column into long format
      dplyr::group_by(theme) %>%              # Group by theme
      dplyr::mutate(prior_draw = dplyr::row_number()) %>% # Add simulation ID
      dplyr::ungroup()

    # Insert priors in the filtered data
    filtered_data <- filtered_data %>%
      dplyr::left_join(dplyr::select(long_draws_data, theme, draws), by = "theme")

    # Sample the data for performance (adjust the sample size as needed)
    sampled_data <- filtered_data %>%
      dplyr::sample_n(min(10000, nrow(filtered_data)))

    # Transform data to long format with both variables
    long_plot_data <- sampled_data %>%
      tidyr::pivot_longer(
        cols = c(posterior_individual_alpha, draws),
        names_to = "variable",
        values_to = "value"
      )

    # Define custom colors for the variables
    custom_colors <- c(
      "posterior_individual_alpha" = "#39FF14",  # Neon green
      "draws" = "#FF5F1F"                        # Neon orange
    )

    # Create the plot
    p <- long_plot_data %>%
      ggplot2::ggplot(ggplot2::aes(x = value, y = tickers, fill = variable)) +
      ggdist::stat_halfeye(
        adjust = 0.5,
        width = 0.6,
        alpha = 0.7,
        position = ggplot2::position_identity()
      ) +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "#FFFFFF") + # White dashed line
      ggplot2::scale_fill_manual(values = custom_colors,
                                 labels = c("Prior Alpha", "Posterior Alpha")) + # Neon green for posterior, neon orange for draws
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = "#001f3f", color = NA), # Deep navy background
        panel.background = ggplot2::element_rect(fill = "#001f3f", color = NA),
        panel.grid.major = ggplot2::element_line(color = "#B0B0B0"), # Light gray grid lines
        panel.grid.minor = ggplot2::element_blank(), # No minor grid lines
        axis.text = ggplot2::element_text(color = "#FFFFFF"), # White axis text
        axis.title = ggplot2::element_text(color = "#FFFFFF"), # White axis title
        plot.title = ggplot2::element_text(color = "#FFFFFF", size = 16, face = "bold"), # White title
        plot.subtitle = ggplot2::element_text(color = "#FFFFFF", size = 10, face = "italic"), # White subtitle
        legend.position = "bottom", # Legend at the bottom
        legend.text = ggplot2::element_text(color = "#FFFFFF"), # White legend text
        legend.title = ggplot2::element_text(color = "#FFFFFF") # White legend title
      ) +
      ggplot2::labs(
        title = "Posterior and Prior Distributions per Ticker",
        subtitle = "Showing posterior and prior distributions of individual alpha",
        x = "Value",
        y = "Tickers",
        fill = "Variable"
      )

    print(p)

  } else if (plot_name == "Posterior Individual Betas"){

    #Plot 11
    tidy_posterior_draws_slope <- tidy_posteriors_list$tidy_posterior_draws_slope
    # Prompt the user to select tickers
    available_tickers <- unique(tidy_posterior_draws_slope$tickers)

    # Display available tickers with numbers
    cat("Available tickers:\n")
    for (i in seq_along(available_tickers)) {
      cat(i, ":", available_tickers[i], "\n")
    }

    # Ask user for input
    selected_numbers <- readline(prompt = "Enter the numbers corresponding to the tickers you want to plot, separated by commas: ")

    # Convert input to numeric and validate
    selected_numbers <- as.numeric(strsplit(selected_numbers, ",")[[1]])
    if (any(is.na(selected_numbers)) || !all(selected_numbers %in% seq_along(available_tickers))) {
      stop("Invalid input. Please enter valid numbers corresponding to the tickers.")
    }

    # Match the selected numbers to tickers
    selected_tickers <- available_tickers[selected_numbers]

    # Filter the data based on user selection
    filtered_data <- tidy_posterior_draws_slope %>%
      dplyr::filter(tickers %in% selected_tickers)

    if (nrow(filtered_data) == 0) {
      stop("No matching tickers found. Please check your input.")
    }

    #Get theme column
    theme_ticker_key <- data.frame(tickers = paste0(selected_signal_themes_m_d_ref$theme, "_", selected_signal_themes_m_d_ref$tickers), theme = selected_signal_themes_m_d_ref$theme)
    filtered_data <- filtered_data %>% dplyr::left_join(theme_ticker_key, by = c("tickers" = "tickers"))

    # Extract themes from `elected_priors`

    ##Random Intercept, Fixed Intercepts or None
    if(model_spec_theme_level %in% c("random_intercept_fixed_slope", "theme_specific_intercept_fixed_slope", "fixed_intercept_fixed_slope")){
      prior_data <- elected_priors %>%
        dplyr::filter(class == "b" & coef == "market_factor_proxy") %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          mean = as.numeric(stringr::str_extract(prior, "(?<=\\().+?(?=,)")), # Extract mean
          sd = as.numeric(stringr::str_extract(prior, "(?<=, ).+?(?=\\))"))   # Extract sd
        ) %>%
        dplyr::ungroup() %>%
        dplyr::select(mean, sd)

      prior_data <- data.frame(theme = unique(theme_ticker_key$theme), mean = prior_data$mean, sd = prior_data$sd)

    }

    ##Fixed Intercepts and Slopes
    if(model_spec_theme_level %in% c("theme_specific_intercept_theme_specific_slope")){
      prior_data <- elected_priors %>%
        dplyr::filter(class == "b" & stringr::str_detect(coef, "^theme.*:market_factor_proxy$")) %>%
        dplyr::mutate(theme = stringr::str_extract(coef, "(?<=^theme).*?(?=:)")) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          mean = as.numeric(stringr::str_extract(prior, "(?<=\\().+?(?=,)")), # Extract mean
          sd = as.numeric(stringr::str_extract(prior, "(?<=, ).+?(?=\\))"))   # Extract sd
        ) %>%
        dplyr::ungroup() %>%
        dplyr::select(theme, mean, sd)
    }

    # Generate draws for each theme and convert to long format
    long_draws_data <- prior_data %>%
      dplyr::mutate(
        draws = purrr::map2(mean, stats::sd, ~ stats::rnorm(length(unique(filtered_data$.draw)), mean = .x, sd = .y))
      ) %>%
      tidyr::unnest(draws) %>%                # Expand the list column into long format
      dplyr::group_by(theme) %>%              # Group by theme
      dplyr::mutate(prior_draw = dplyr::row_number()) %>% # Add simulation ID
      dplyr::ungroup()

    # Insert priors in the filtered data
    filtered_data <- filtered_data %>%
      dplyr::left_join(dplyr::select(long_draws_data, theme, draws), by = "theme")

    # Sample the data for performance (adjust the sample size as needed)
    sampled_data <- filtered_data %>%
      dplyr::sample_n(min(10000, nrow(filtered_data)))

    # Transform data to long format with both variables
    long_plot_data <- sampled_data %>%
      tidyr::pivot_longer(
        cols = c(posterior_individual_beta, draws),
        names_to = "variable",
        values_to = "value"
      )

    # Define custom colors for the variables
    custom_colors <- c(
      "posterior_individual_beta" = "#39FF14",  # Neon green
      "draws" = "#FF5F1F"                        # Neon orange
    )

    # Create the plot
    p <- long_plot_data %>%
      ggplot2::ggplot(ggplot2::aes(x = value, y = tickers, fill = variable)) +
      ggdist::stat_halfeye(
        adjust = 0.5,
        width = 0.6,
        alpha = 0.7,
        position = ggplot2::position_identity()
      ) +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "#FFFFFF") + # White dashed line
      ggplot2::scale_fill_manual(values = custom_colors,
                                 labels = c("Prior Beta", "Posterior Beta")) + # Neon green for posterior, neon orange for draws
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = "#001f3f", color = NA), # Deep navy background
        panel.background = ggplot2::element_rect(fill = "#001f3f", color = NA),
        panel.grid.major = ggplot2::element_line(color = "#B0B0B0"), # Light gray grid lines
        panel.grid.minor = ggplot2::element_blank(), # No minor grid lines
        axis.text = ggplot2::element_text(color = "#FFFFFF"), # White axis text
        axis.title = ggplot2::element_text(color = "#FFFFFF"), # White axis title
        plot.title = ggplot2::element_text(color = "#FFFFFF", size = 16, face = "bold"), # White title
        plot.subtitle = ggplot2::element_text(color = "#FFFFFF", size = 10, face = "italic"), # White subtitle
        legend.position = "bottom", # Legend at the bottom
        legend.text = ggplot2::element_text(color = "#FFFFFF"), # White legend text
        legend.title = ggplot2::element_text(color = "#FFFFFF") # White legend title
      ) +
      ggplot2::labs(
        title = "Posterior and Prior Distributions per Ticker",
        subtitle = "Showing posterior and prior distributions of individual beta",
        x = "Value",
        y = "Tickers",
        fill = "Variable"
      )

    print(p)

  } else if (plot_name == "Posterior Random Effects") {
    #Plot 12

    #Get tidy_posterior_draws_sw
    tidy_posterior_draws_sd <-  tidy_posteriors_list$tidy_posterior_draws_sd

    # Transform and plot the data
    p <- tidy_posterior_draws_sd %>%
      # Select relevant columns by excluding unwanted ones
      dplyr::select(-.chain, -.iteration, -.draw, -posterior_cor_r_alpha_beta, -posterior_sigma) %>%
      # Pivot to longer format
      tidyr::pivot_longer(
        cols = dplyr::everything(),
        names_to = "parameter",
        values_to = "sd_value"
      ) %>%
      # Create the plot
      ggplot2::ggplot(ggplot2::aes(y = parameter, x = sd_value, fill = parameter)) +
      ggdist::stat_halfeye(
        show.legend = FALSE,              # Hide legend if not needed
        adjust = 0.5,                     # Adjust the bandwidth of the density estimate
        width = 0.6,                      # Width of the half-eye plot
        alpha = 0.8                       # Transparency for better visibility
      ) +
      ggplot2::geom_vline(
        xintercept = 0,
        linetype = "dashed",
        color = "#FFFFFF",                # White dashed line for consistency
        size = 1
      ) +
      # Customize the theme to match other plots
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = "#001f3f", color = NA),    # Deep navy background
        panel.background = ggplot2::element_rect(fill = "#001f3f", color = NA),   # Deep navy panel
        panel.grid.major = ggplot2::element_line(color = "#B0B0B0"),             # Light gray major grid lines
        panel.grid.minor = ggplot2::element_blank(),                             # Remove minor grid lines
        axis.text = ggplot2::element_text(color = "#FFFFFF"),                    # White axis text
        axis.title = ggplot2::element_text(color = "#FFFFFF"),                   # White axis titles
        plot.title = ggplot2::element_text(color = "#FFFFFF", size = 16, face = "bold"),       # White bold title
        plot.subtitle = ggplot2::element_text(color = "#FFFFFF", size = 10, face = "italic"),  # White italic subtitle
        legend.position = "bottom",                                             # Position legend at the bottom
        legend.text = ggplot2::element_text(color = "#FFFFFF"),                # White legend text
        legend.title = ggplot2::element_text(color = "#FFFFFF")                # White legend title
      ) +
      # Add labels
      ggplot2::labs(
        title = "Posterior Distributions of Random Effects",
        subtitle = "Standard Deviations across Parameters",
        x = "Standard Deviation",
        y = "Parameter"
      )

    print(p)
  } else if (plot_name == "Waterfall Plot of Posterior Variance Components") {
    #Plot 13

    #Get tidy_posterior_draws_sw
    tidy_posterior_draws_sd <-  tidy_posteriors_list$tidy_posterior_draws_sd

    #Waterfall plot
    if(model_spec_theme_level == "random_intercept_fixed_slope"){
    #For random_intercept_fixed_slope, include var_theme_intercept
    variance_data <- tidy_posterior_draws_sd %>%
      dplyr::mutate(
        # Variance components
        var_theme_intercept = posterior_r_theme_alpha^2,
        var_tickers_intercept = posterior_r_tickers_alpha^2,
        var_tickers_slope = posterior_r_tickers_beta^2,
        var_sigma = posterior_sigma^2
      ) %>%
      dplyr::mutate(
        cov_tickers_intercept_slope = 2 * posterior_r_tickers_alpha * posterior_r_tickers_beta * posterior_cor_r_alpha_beta
      ) %>%
      dplyr::mutate(
        total_variance = var_theme_intercept +
          var_tickers_intercept +
          var_tickers_slope +
          var_sigma +
          cov_tickers_intercept_slope
      )

    #Get decomposition info
    waterfall_data <- variance_data %>%
      dplyr::select(
        .draw,
        var_theme_intercept,
        var_tickers_intercept,
        var_tickers_slope,
        var_sigma,
        cov_tickers_intercept_slope,
        total_variance
      ) %>%
      # Calculate mean contributions across draws
      dplyr::summarise(
        var_theme_intercept = mean(var_theme_intercept),
        var_tickers_intercept = mean(var_tickers_intercept),
        var_tickers_slope = mean(var_tickers_slope),
        var_sigma = mean(var_sigma),
        cov_tickers_intercept_slope = mean(cov_tickers_intercept_slope),
        total_variance = mean(total_variance)
      ) %>%
      # Reshape data to long format
      tidyr::pivot_longer(
        cols = -total_variance,
        names_to = "Component",
        values_to = "Variance"
      ) %>%
      dplyr::mutate(
        # Order components for plotting
        Component = factor(
          Component,
          levels = c(
            "var_theme_intercept",
            "var_tickers_intercept",
            "var_tickers_slope",
            "cov_tickers_intercept_slope",
            "var_sigma"
          ),
          labels = c(
            "Theme Intercept Variance",
            "Tickers Intercept Variance",
            "Tickers Slope Variance",
            "Tickers Intercept-Slope Covariance",
            "Residual Variance"
          )
        )
      )

    # Calculate cumulative variance contributions
    waterfall_data <- waterfall_data %>%
      dplyr::mutate(
        Cumulative = cumsum(Variance) - Variance[1],
        Start = dplyr::lag(Cumulative, default = 0),
        End = Cumulative,
        Contribution_Type = ifelse(Variance >= 0, "Positive", "Negative")
      )

    # Plot the waterfall
    p <- ggplot2::ggplot(waterfall_data, ggplot2::aes(
      x = Component,
      ymin = Start,
      ymax = End,
      fill = Contribution_Type
    )) +
      ggplot2::geom_rect(
        ggplot2::aes(
          xmin = as.numeric(Component) - 0.4,
          xmax = as.numeric(Component) + 0.4
        ),
        color = black
      ) +
      ggplot2::geom_text(
        ggplot2::aes(
          x = Component,
          y = End,
          label = round(Variance, 4)
        ),
        vjust = ifelse(waterfall_data$Variance >= 0, -0.5, 1.5),
        size = 3,
        color = white  # Ensure text is visible against the background
      ) +
      ggplot2::scale_fill_manual(
        values = c("Positive" = pos_color, "Negative" = neg_color)
      ) +
      ggplot2::labs(
        title = "Waterfall Plot of Variance Components",
        x = "Components",
        y = "Cumulative Variance",
        fill = "Contribution"  # Add fill legend title for clarity
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = blue_bg, color = NA),
        panel.background = ggplot2::element_rect(fill = blue_bg, color = NA),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        axis.text = ggplot2::element_text(color = white, angle = 45, hjust = 1),
        axis.title = ggplot2::element_text(color = white),
        plot.title = ggplot2::element_text(color = white, size = 16, face = "bold"),
        plot.subtitle = ggplot2::element_text(color = white, size = 8, face = "italic"),
        legend.position = "bottom",
        legend.title = ggplot2::element_text(color = white),
        legend.text = ggplot2::element_text(color = white),
        strip.text = ggplot2::element_text(color = white)
      )

    print(p)

    } else {
      ############################
      #Otherwise, include variance for theme
      ##########################
      variance_data <- tidy_posterior_draws_sd %>%
        dplyr::mutate(
          # Variance components
          var_tickers_intercept = posterior_r_tickers_alpha^2,
          var_tickers_slope = posterior_r_tickers_beta^2,
          var_sigma = posterior_sigma^2
        ) %>%
        dplyr::mutate(
          cov_tickers_intercept_slope = 2 * posterior_r_tickers_alpha * posterior_r_tickers_beta * posterior_cor_r_alpha_beta
        ) %>%
        dplyr::mutate(
          total_variance =
            var_tickers_intercept +
            var_tickers_slope +
            var_sigma +
            cov_tickers_intercept_slope
        )

      #Get decomposition info
      waterfall_data <- variance_data %>%
        dplyr::select(
          .draw,
          var_tickers_intercept,
          var_tickers_slope,
          var_sigma,
          cov_tickers_intercept_slope,
          total_variance
        ) %>%
        # Calculate mean contributions across draws
        dplyr::summarise(
          var_tickers_intercept = mean(var_tickers_intercept),
          var_tickers_slope = mean(var_tickers_slope),
          var_sigma = mean(var_sigma),
          cov_tickers_intercept_slope = mean(cov_tickers_intercept_slope),
          total_variance = mean(total_variance)
        ) %>%
        # Reshape data to long format
        tidyr::pivot_longer(
          cols = -total_variance,
          names_to = "Component",
          values_to = "Variance"
        ) %>%
        dplyr::mutate(
          # Order components for plotting
          Component = factor(
            Component,
            levels = c(
              "var_tickers_intercept",
              "var_tickers_slope",
              "cov_tickers_intercept_slope",
              "var_sigma"
            ),
            labels = c(
              "Tickers Intercept Variance",
              "Tickers Slope Variance",
              "Tickers Intercept-Slope Covariance",
              "Residual Variance"
            )
          )
        )

      # Calculate cumulative variance contributions
      waterfall_data <- waterfall_data %>%
        dplyr::mutate(
          Cumulative = cumsum(Variance) - Variance[1],
          Start = dplyr::lag(Cumulative, default = 0),
          End = Cumulative,
          Contribution_Type = ifelse(Variance >= 0, "Positive", "Negative")
        )

      # Plot the waterfall
      p <- ggplot2::ggplot(waterfall_data, ggplot2::aes(
        x = Component,
        ymin = Start,
        ymax = End,
        fill = Contribution_Type
      )) +
        ggplot2::geom_rect(
          ggplot2::aes(
            xmin = as.numeric(Component) - 0.4,
            xmax = as.numeric(Component) + 0.4
          ),
          color = black
        ) +
        ggplot2::geom_text(
          ggplot2::aes(
            x = Component,
            y = End,
            label = round(Variance, 4)
          ),
          vjust = ifelse(waterfall_data$Variance >= 0, -0.5, 1.5),
          size = 3,
          color = white  # Ensure text is visible against the background
        ) +
        ggplot2::scale_fill_manual(
          values = c("Positive" = pos_color, "Negative" = neg_color)
        ) +
        ggplot2::labs(
          title = "Waterfall Plot of Variance Components",
          x = "Components",
          y = "Cumulative Variance",
          fill = "Contribution"  # Add fill legend title for clarity
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.background = ggplot2::element_rect(fill = blue_bg, color = NA),
          panel.background = ggplot2::element_rect(fill = blue_bg, color = NA),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          axis.text = ggplot2::element_text(color = white, angle = 45, hjust = 1),
          axis.title = ggplot2::element_text(color = white),
          plot.title = ggplot2::element_text(color = white, size = 16, face = "bold"),
          plot.subtitle = ggplot2::element_text(color = white, size = 8, face = "italic"),
          legend.position = "bottom",
          legend.title = ggplot2::element_text(color = white),
          legend.text = ggplot2::element_text(color = white),
          strip.text = ggplot2::element_text(color = white)
        )

      print(p)
    }

  } else if (plot_name == "Posterior Regression Lines"){

    # Get alpha and beta
    tidy_posterior_draws_intercept <- tidy_posteriors_list$tidy_posterior_draws_intercept
    tidy_posterior_draws_slope <- tidy_posteriors_list$tidy_posterior_draws_slope

    # Prompt the user to select tickers
    available_tickers <- unique(tidy_posterior_draws_slope$tickers)

    # Display available tickers with numbers
    cat("Available tickers:\n")
    for (i in seq_along(available_tickers)) {
      cat(i, ":", available_tickers[i], "\n")
    }

    # Ask user for input
    selected_numbers <- readline(prompt = "Enter the numbers corresponding to the tickers you want to plot, separated by commas: ")

    # Convert input to numeric and validate
    selected_numbers <- as.numeric(strsplit(selected_numbers, ",")[[1]])
    if (any(is.na(selected_numbers)) || !all(selected_numbers %in% seq_along(available_tickers))) {
      stop("Invalid input. Please enter valid numbers corresponding to the tickers.")
    }

    # Match the selected numbers to tickers
    selected_tickers <- available_tickers[selected_numbers]
    num_selected <- length(selected_tickers)

    #Define colors
    neon_colors <- c(
      "#FF10F0",  # Neon Pink
      "#00FFFF",  # Neon Cyan
      "#39FF14",  # Neon Green
      "#FF00FF",  # Neon Magenta
      "#FF5F1F",  # Neon Orange
      "#FFFF00",  # Neon Yellow
      "#00FFF7",  # Neon Light Blue
      "#FF1493",  # Deep Pink
      "#ADFF2F",  # Green Yellow
      "#FF69B4",  # Hot Pink
      "#7FFF00",  # Chartreuse
      "#DC143C",  # Crimson
      "#00CED1",  # Dark Turquoise
      "#FF4500",  # Orange Red
      "#00FA9A"   # Medium Spring Green
    )

    # Join intercept and slope data, filter for selected tickers, and convert tickers to factor
    tidy_posterior_draws_intercept_and_slope <- dplyr::left_join(
      tidy_posterior_draws_intercept,
      tidy_posterior_draws_slope,
      by = c("tickers", ".draw")
    ) %>%
      dplyr::filter(tickers %in% selected_tickers) %>%
      # **Convert 'tickers' to a factor to ensure distinct colors**
      dplyr::mutate(tickers = as.factor(tickers))

    # Get market_factor_proxy
    x_values <- x@selected_market_factor_proxy_m_xts %>% as.vector()
    x_values <- sort(x_values)

    # Sample a subset of posterior draws to avoid overplotting
    set.seed(123)  # For reproducibility
    sampled_draws <- dplyr::group_by(tidy_posterior_draws_intercept_and_slope, tickers) %>%
      dplyr::sample_n(size = 25, replace = FALSE) %>%
      dplyr::ungroup()

    # Create a grid of sampled draws and x_values
    plot_data <- tidyr::crossing(
      sampled_draws %>% dplyr::select(tickers, posterior_individual_alpha, posterior_individual_beta, .draw),
      x = x_values
    ) %>%
      dplyr::mutate(y = posterior_individual_alpha + posterior_individual_beta * x)

    # Generate the plot
    if (num_selected > length(neon_colors)) {
      warning("Number of selected tickers exceeds the number of predefined colors. Colors will be recycled.")
    }

    regression_plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = y, group = .draw, color = tickers)) +
      ggplot2::geom_line(alpha = 0.6, size = 1) +  # Increased alpha and line size for better visibility
      ggplot2::scale_color_manual(values = neon_colors[1:num_selected]) +  # Assign neon colors
      ggplot2::labs(
        title = "Posterior Regression Lines by Signal",
        subtitle = "Using Posterior Individual Alpha and Beta",
        x = "Market Factor Proxy",
        y = "Predicted Outcome",
        color = "Ticker"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = "#001f3f", color = NA),  # Dark blue background
        panel.background = ggplot2::element_rect(fill = "#001f3f", color = NA),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        axis.text = ggplot2::element_text(color = "#FFFFFF", angle = 45, hjust = 1),  # White axis text
        axis.title = ggplot2::element_text(color = "#FFFFFF"),  # White axis titles
        plot.title = ggplot2::element_text(color = "#FFFFFF", size = 16, face = "bold"),
        plot.subtitle = ggplot2::element_text(color = "#FFFFFF", size = 10, face = "italic"),
        legend.position = "bottom",
        legend.title = ggplot2::element_text(color = "#FFFFFF"),
        legend.text = ggplot2::element_text(color = "#FFFFFF")
      )

    # Display the Plot
    print(regression_plot)

  } else if (plot_name == "Waterfall Plot of Return Decomposition by Signal"){

    # Get alpha and beta
    tidy_posterior_draws_intercept <- tidy_posteriors_list$tidy_posterior_draws_intercept
    tidy_posterior_draws_slope <- tidy_posteriors_list$tidy_posterior_draws_slope

    # Prompt the user to select tickers
    available_tickers <- unique(tidy_posterior_draws_slope$tickers)

    # Display available tickers with numbers
    cat("Available tickers:\n")
    for (i in seq_along(available_tickers)) {
      cat(i, ":", available_tickers[i], "\n")
    }

    # Ask user for input
    selected_numbers <- readline(prompt = "Enter the numbers corresponding to the tickers you want to plot, separated by commas: ")

    # Convert input to numeric and validate
    selected_numbers <- as.numeric(strsplit(selected_numbers, ",")[[1]])
    if (any(is.na(selected_numbers)) || !all(selected_numbers %in% seq_along(available_tickers))) {
      stop("Invalid input. Please enter valid numbers corresponding to the tickers.")
    }

    # Match the selected numbers to tickers
    selected_tickers <- available_tickers[selected_numbers]
    num_selected <- length(selected_tickers)

    #Get Expectations
    tidy_posterior_epred_draws <- tidy_posteriors_list$tidy_posterior_epred_draws %>% dplyr::mutate(tickers = `theme:tickers`)

    #Add Alpha and Beta to Expectations
    tidy_posterior_epred_draws_complete <- dplyr::left_join(tidy_posterior_epred_draws,
                                                            #Join with intercept
                                                            tidy_posterior_draws_intercept, by = c("tickers", ".draw")) %>%
                                                            #Join with slopes
                                           dplyr::left_join(tidy_posterior_draws_slope, by = c("tickers", ".draw")) %>%
                                           #Filter given tickers
                                           dplyr::filter(tickers %in% selected_tickers)

    #Waterfall plot
    # Prepare and compute contributions
    if(model_spec_theme_level == "random_intercept_fixed_slope"){
      waterfall_data <- tidy_posterior_epred_draws_complete %>%
        dplyr::mutate(
          # Component contributions
          `Theme Fixed Effect` = posterior_theme_alpha,
          `Theme Random Effect` = r_theme,
          `Ticker Random Intercept` = r_tickers_intercept,
          `Market Factor Fixed Effect` = posterior_theme_beta * market_factor_proxy,
          `Ticker Random Slope` = r_tickers_slope * market_factor_proxy
        ) %>%
        dplyr::select(
          .draw, tickers, return,
          `Theme Fixed Effect`,
          `Theme Random Effect`,
          `Ticker Random Intercept`,
          `Market Factor Fixed Effect`,
          `Ticker Random Slope`
        ) %>%
        tidyr::pivot_longer(
          cols = c(
            `Theme Fixed Effect`,
            `Theme Random Effect`,
            `Ticker Random Intercept`,
            `Market Factor Fixed Effect`,
            `Ticker Random Slope`
          ),
          names_to = "Component",
          values_to = "Contribution"
        ) %>%
        dplyr::group_by(tickers, Component) %>%
        dplyr::summarise(
          Mean_Contribution = mean(Contribution, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        dplyr::mutate(
          Component = factor(
            Component,
            levels = c(
              "Theme Fixed Effect",
              "Theme Random Effect",
              "Ticker Random Intercept",
              "Market Factor Fixed Effect",
              "Ticker Random Slope"
            )
          )
        ) %>%
        dplyr::arrange(Component) %>%
        dplyr::group_by(tickers) %>%
        dplyr::mutate(
          Start = dplyr::lag(cumsum(Mean_Contribution), default = 0),
          End = cumsum(Mean_Contribution),
          Contribution_Type = ifelse(Mean_Contribution >= 0, "Positive", "Negative")
        ) %>%
        dplyr::ungroup()

    } else { #This is for other model_spec_theme_level
      waterfall_data <- tidy_posterior_epred_draws_complete %>%
        dplyr::mutate(
          # Component contributions
          `Theme Fixed Effect` = posterior_theme_alpha,
          `Ticker Random Intercept` = r_tickers_intercept,
          `Market Factor Fixed Effect` = posterior_theme_beta * market_factor_proxy,
          `Ticker Random Slope` = r_tickers_slope * market_factor_proxy
        ) %>%
        dplyr::select(
          .draw, tickers, return,
          `Theme Fixed Effect`,
          `Ticker Random Intercept`,
          `Market Factor Fixed Effect`,
          `Ticker Random Slope`
        ) %>%
        tidyr::pivot_longer(
          cols = c(
            `Theme Fixed Effect`,
            `Ticker Random Intercept`,
            `Market Factor Fixed Effect`,
            `Ticker Random Slope`
          ),
          names_to = "Component",
          values_to = "Contribution"
        ) %>%
        dplyr::group_by(tickers, Component) %>%
        dplyr::summarise(
          Mean_Contribution = mean(Contribution, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        dplyr::mutate(
          Component = factor(
            Component,
            levels = c(
              "Theme Fixed Effect",
              "Ticker Random Intercept",
              "Market Factor Fixed Effect",
              "Ticker Random Slope"
            )
          )
        ) %>%
        dplyr::arrange(Component) %>%
        dplyr::group_by(tickers) %>%
        dplyr::mutate(
          Start = dplyr::lag(cumsum(Mean_Contribution), default = 0),
          End = cumsum(Mean_Contribution),
          Contribution_Type = ifelse(Mean_Contribution >= 0, "Positive", "Negative")
        ) %>%
        dplyr::ungroup()
    }

      # Create the waterfall plot per ticker
      p <- ggplot2::ggplot(waterfall_data, ggplot2::aes(
        x = Component,
        ymin = Start,
        ymax = End,
        fill = Contribution_Type
      )) +
        ggplot2::geom_rect(
          ggplot2::aes(
            xmin = as.numeric(Component) - 0.4,
            xmax = as.numeric(Component) + 0.4
          ),
          color = "black"
        ) +
        ggplot2::geom_text(
          ggplot2::aes(
            x = Component,
            y = End,
            label = round(Mean_Contribution, 4)
          ),
          vjust = ifelse(waterfall_data$Mean_Contribution >= 0, -0.5, 1.5),
          size = 3,
          color = "white"
        ) +
        ggplot2::scale_fill_manual(
          values = c("Positive" = "#39FF14", "Negative" = "#FF5F1F")
        ) +
        ggplot2::labs(
          title = "Waterfall Plot of Return Decomposition by Signal",
          x = "Components",
          y = "Cumulative Contribution"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.background = ggplot2::element_rect(fill = blue_bg, color = NA),
          panel.background = ggplot2::element_rect(fill = blue_bg, color = NA),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          axis.text = ggplot2::element_text(color = white, angle = 45, hjust = 1),
          axis.title = ggplot2::element_text(color = white),
          plot.title = ggplot2::element_text(color = white, size = 16, face = "bold"),
          plot.subtitle = ggplot2::element_text(color = white, size = 8, face = "italic"),
          legend.position = "bottom",
          legend.title = ggplot2::element_text(color = white),
          legend.text = ggplot2::element_text(color = white),
          strip.text = ggplot2::element_text(color = white)
        ) +
        ggplot2::facet_wrap(~ tickers, scales = "free_y")


      print(p)

  } else if (plot_name == "Posterior Individual Alpha Distributions by Theme and Ticker"){

    # Get alpha and beta
    tidy_posterior_draws_intercept <- tidy_posteriors_list$tidy_posterior_draws_intercept
    tidy_posterior_draws_slope <- tidy_posteriors_list$tidy_posterior_draws_slope

    # Prompt the user to select tickers
    available_tickers <- unique(tidy_posterior_draws_slope$tickers)

    # Display available tickers with numbers
    cat("Available tickers:\n")
    for (i in seq_along(available_tickers)) {
      cat(i, ":", available_tickers[i], "\n")
    }

    # Ask user for input
    selected_numbers <- readline(prompt = "Enter the numbers corresponding to the tickers you want to plot, separated by commas: ")

    # Convert input to numeric and validate
    selected_numbers <- as.numeric(strsplit(selected_numbers, ",")[[1]])
    if (any(is.na(selected_numbers)) || !all(selected_numbers %in% seq_along(available_tickers))) {
      stop("Invalid input. Please enter valid numbers corresponding to the tickers.")
    }

    # Match the selected numbers to tickers
    selected_tickers <- available_tickers[selected_numbers]

    # Prepare the data
    theme_ticker_key <- data.frame(tickers = paste0(selected_signal_themes_m_d_ref$theme, "_", selected_signal_themes_m_d_ref$tickers), theme = selected_signal_themes_m_d_ref$theme)

    plot_data <- tidy_posterior_draws_intercept %>%
      dplyr::ungroup() %>%  # Ensure the data is not grouped
      dplyr::select(tickers, posterior_individual_alpha) %>%
      dplyr::left_join(theme_ticker_key, by = "tickers") %>%
      dplyr::filter(tickers %in% selected_tickers)

    # Automatically generate distinct colors for tickers
    num_tickers <- length(unique(plot_data$tickers))
    palette <- suppressWarnings(RColorBrewer::brewer.pal(min(max(num_tickers, 3), 12), "Set3"))  # Adjust palette as needed
    ticker_colors <- stats::setNames(palette, unique(plot_data$tickers))

    # Create the boxplot with consistent aesthetics
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(
      x = theme,
      y = posterior_individual_alpha,
      fill = tickers
    )) +
      ggplot2::geom_boxplot(
        position = ggplot2::position_dodge(width = 0.8),
        outlier.shape = NA,
        color = black,  # Border color for boxes
        alpha = 0.8      # Transparency for boxes
      ) +
      ggplot2::scale_fill_manual(
        values = ticker_colors
      ) +
      ggplot2::labs(
        title = "Posterior Individual Alpha Distributions by Theme and Ticker",
        x = "Theme",
        y = "Posterior Individual Alpha",
        fill = "Ticker"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        # Set the overall plot background
        plot.background = ggplot2::element_rect(fill = blue_bg, color = NA),
        # Set the panel background
        panel.background = ggplot2::element_rect(fill = blue_bg, color = NA),
        # Remove major and minor grid lines
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        # Customize axis text
        axis.text.x = ggplot2::element_text(color = white, angle = 45, hjust = 1),
        axis.text.y = ggplot2::element_text(color = white),
        # Customize axis titles
        axis.title = ggplot2::element_text(color = white, size = 12, face = "bold"),
        # Customize plot title
        plot.title = ggplot2::element_text(color = white, size = 16, face = "bold"),
        # Customize legend
        legend.position = "bottom",
        legend.background = ggplot2::element_rect(fill = blue_bg, color = NA),
        legend.title = ggplot2::element_text(color = white, face = "bold"),
        legend.text = ggplot2::element_text(color = white),
        # Customize plot subtitle if present
        plot.subtitle = ggplot2::element_text(color = white, size = 10, face = "italic")
      )

    print(p)


  }

  invisible(x)
})



#' @title Plot Method for port_backtest_config: Faceted Liquidity Floor Cutoffs (Ordered Within Facets)
#'
#' @description Generates a faceted bar plot displaying liquidity floor cutoff metrics
#' from a \code{port_backtest_config} object. The liquidity floor cutoffs data is expected to
#' contain one grouping column (automatically identified as the first non-numeric column)
#' and one or more numeric columns. The data is pivoted into a long format and, within each metric facet,
#' the grouping variable is reordered (from left to right) based on its numeric value (smallest to largest).
#' All text in the plot (including facet titles) is displayed in white.
#'
#' @param x A \code{port_backtest_config} object containing liquidity floor cutoffs data in the
#'   \code{liquidity_floor_cutoffs} slot.
#' @param y Not used. Required for S4 method consistency.
#' @param ... Additional arguments (currently not used).
#'
#' @return A \code{ggplot} object representing the faceted liquidity floor cutoffs plot.
#' @export

setMethod(
  "plot",
  signature(x = "port_backtest_config", y = "missing"),
  function(x, ...) {

    #Check for packages
    if (!requireNamespace("gridExtra", quietly = TRUE) || !requireNamespace("scales", quietly = TRUE)) {
      stop("Packages 'gridExtra' and 'scales' are required to generate plots. Please install them using install.packages().")
    }

    # Check if liquidity_floor_cutoffs data is available
    if (is.null(x@liquidity_floor_cutoffs)) {
      stop("No liquidity floor cutoffs data available in this port_backtest_config object.")
    }

    df <- x@liquidity_floor_cutoffs

    # Identify the grouping column: first column that is not numeric
    non_numeric_cols <- names(df)[!sapply(df, is.numeric)]
    if (length(non_numeric_cols) == 0) {
      stop("Liquidity floor cutoffs data must contain at least one non-numeric column to be used as the grouping variable.")
    }
    group_col <- non_numeric_cols[1]

    # Identify numeric columns (other than the grouping column)
    numeric_cols <- setdiff(names(df), group_col)
    if (length(numeric_cols) == 0) {
      stop("No numeric columns found for plotting.")
    }

    # Pivot the data into long format
    df_long <- tidyr::pivot_longer(
      df,
      cols = numeric_cols,
      names_to = "Metric",
      values_to = "Value"
    )

    # For each Metric facet, reorder the grouping variable by Value.
    # Since each (Metric, group_col) pair is unique, we can arrange by Value and then
    # set new_group factor levels to the order within that group.
    df_long <- df_long %>%
      dplyr::group_by(Metric) %>%
      dplyr::arrange(Value, .by_group = TRUE) %>%
      dplyr::mutate(new_group = factor(.data[[group_col]], levels = unique(.data[[group_col]]))) %>%
      dplyr::ungroup()

    # Define custom colors following the stylistic guidelines
    deep_navy      <- "#000033"
    black          <- "#000000"
    white          <- "#FFFFFF"
    vibrant_purple <- "#6A0DAD"
    blue_bg        <- "#001f3f"

    # Create the faceted bar plot: each facet corresponds to a numeric metric.
    # Within each facet, the x-axis uses new_group which is ordered by Value.
    p <- ggplot2::ggplot(df_long, ggplot2::aes(x = new_group, y = Value)) +
      ggplot2::geom_bar(stat = "identity", fill = vibrant_purple, color = black) +
      ggplot2::geom_text(ggplot2::aes(label = scales::scientific(Value, digits = 2)),
                         vjust = -0.5, color = white, size = 3.5) +
      ggplot2::facet_wrap(ggplot2::vars(Metric), scales = "free_y") +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.background  = ggplot2::element_rect(fill = blue_bg, color = NA),
        panel.background = ggplot2::element_rect(fill = blue_bg, color = NA),
        axis.text        = ggplot2::element_text(color = white),
        axis.title       = ggplot2::element_text(color = white),
        plot.title       = ggplot2::element_text(color = white, size = 16, face = "bold"),
        plot.subtitle    = ggplot2::element_text(color = white, size = 10, face = "italic"),
        legend.position  = "bottom",
        legend.title     = ggplot2::element_text(color = white),
        legend.text      = ggplot2::element_text(color = white),
        strip.text       = ggplot2::element_text(color = white)  # Facet titles in white
      ) +
      ggplot2::labs(
        title    = "Liquidity Floor Cutoffs",
        subtitle = paste("Faceted bar plots by metric (grouped by", group_col, ")"),
        x        = group_col,
        y        = "Value"
      )

    print(p)
    invisible(p)
  }
)


#' Plot Method for 'port' Objects
#'
#' This method generates plots for a \code{port} object, depending on the specified \code{type}.
#'
#' The currently supported plot types are:
#' \enumerate{
#'  \item \code{"weights"} - A bar chart of the portfolio weights (or active weights).
#'  \item \code{"exp_ret_score"} - A bar chart of the portfolio's expected return scores.
#'  \item \code{"risk_return"} - A scatterplot of expected return score (y) vs. risk (x).
#'  \item \code{"correlation"} - A heatmap of the asset correlation matrix.
#'  \item \code{"relative_risk_contribution"} - A bar chart comparing weights and risk contributions.
#'  \item \code{"efficient_frontier"} - A scatterplot of random portfolios with Sharpe ratio coloring and the optimal portfolio highlighted.
#'  \item \code{"random_weights_distribution"} - A jitter plot of random portfolio weights with constraints.
#'  \item \code{"group_composition"} - A grouped bar chart comparing portfolio and benchmark weights across classification variables (e.g., sectors).
#' }
#'
#' This method is dispatched on signature \code{plot(x = "port", y = "missing")} and does not use the \code{y} argument.
#'
#' @param x An object of class \code{"port"}.
#' @param type A character string specifying the type of plot to generate. If \code{NULL}, the user will be prompted.
#' @param ... Additional arguments for future extensions (currently unused).
#'
#' @return A \code{ggplot} object (invisibly). The function also prints the plot.
#' @export
setMethod(
  "plot",
  signature(x = "port", y = "missing"),
  function(x, type = NULL, ...) {

    #Check for packages
    if (!requireNamespace("gridExtra", quietly = TRUE) || !requireNamespace("scales", quietly = TRUE)) {
      stop("Packages 'gridExtra' and 'scales' are required to generate plots. Please install them using install.packages().")
    }

    #------------------------------------------------------------------------------------------------
    # 0) Prompt for 'type' if not specified
    #------------------------------------------------------------------------------------------------
    if (is.null(type)) {
      available_types <- c(
        "Weights",
        "Expected Return Score",
        "Risk-Return Trade-off",
        "Correlation",
        "Relative Risk Contribution",
        "Efficient Frontier",
        "Random Weights Distribution",
        "Group Composition"
      )
      cat("\nPlease choose a plot type:\n")
      for (i in seq_along(available_types)) {
        cat(paste0(i, ": ", available_types[i], "\n"))
      }
      selection <- readline(prompt = "Enter the number of your choice: ")
      selection <- as.numeric(selection)
      if (is.na(selection) || selection < 1 || selection > length(available_types)) {
        stop("Invalid selection.")
      }
      type <- available_types[selection]
    }

    #------------------------------------------------------------------------------------------------
    # Extract needed slots for convenience
    #------------------------------------------------------------------------------------------------
    weights           <- x@weights
    exp_ret_score     <- x@exp_ret_score
    cov_mat           <- x@covariance_matrix
    corr_mat          <- x@correlation_matrix
    rel_risk_contr    <- x@rel_risk_contr
    random_port_wt    <- x@random_port_weights
    groups_df         <- x@groups
    universe_df       <- if (methods::is(x@universe_m_d_ref, "meta_dataframe")) x@universe_m_d_ref@data else NULL
    asset_names       <- x@eligible_assets
    port_name         <- x@port_name

    #------------------------------------------------------------------------------------------------
    # Color definitions
    #------------------------------------------------------------------------------------------------
    vibrant_purple <- "#6A0DAD"
    black          <- "#000000"
    white          <- "#FFFFFF"
    neon_green     <- "#39FF14"
    neon_orange    <- "#FF5F1F"
    neon_cyan       = "#00FFFF"
    neon_blue       = "#1F51FF"
    neon_yellow     = "#FFFF33"
    neon_red        = "#FF073A"
    neon_turquoise  = "#40E0D0"
    neon_lime       = "#BFFF00"
    neon_magenta    = "#FF00FF"
    neon_hotpink    = "#FF69B4"
    neon_pink      <- "#FF007F"
    neon_purple    <- "#7F00FF"
    blue_bg        <- "#001f3f"
    neon_chartreuse <- "#7FFF00"
    neon_indigo     <- "#4B0082"
    neon_gold       <- "#FFD700"
    neon_silver     <- "#C0C0C0"
    neon_coral      <- "#FF7F50"
    neon_violet     <- "#EE82EE"

    #------------------------------------------------------------------------------------------------
    # 1) weights - Bar chart
    #------------------------------------------------------------------------------------------------
    if (type == "Weights") {
      # 1) Basic checks
      if (length(weights) == 0) {
        stop("No weights found in 'x'.")
      }
      if (length(asset_names) != length(weights)) {
        stop("Mismatch between 'eligible_assets' and 'weights' length.")
      }

      # Build the main data frame (no filter yet; we will filter after user selection)
      df_pie <- data.frame(Asset = asset_names, Weight = weights)

      # If all zero or the sum is zero, there's nothing sensible to plot
      if (all(df_pie$Weight == 0) || sum(df_pie$Weight) == 0) {
        stop("Sum of weights is zero or all weights are zero. Nothing to plot.")
      }

      # 2) Prompt user if they want active weights
      cat("Do you want to compute active weights? (y/n): ")
      active_choice <- tolower(readline())
      active_weights_mode <- FALSE
      if (active_choice %in% c("y", "yes")) {
        active_weights_mode <- TRUE
      }

      if (active_weights_mode) {
        bench_cols <- grep("_bench_weights$", colnames(universe_df), value = TRUE)
        if (length(bench_cols) == 0) {
          stop("No columns matching '_bench_weights' found in 'universe_df'.")
        }

        bench_col <- NULL
        if (length(bench_cols) == 1) {
          bench_col <- bench_cols[1]
          message("Benchmark is: ", bench_col)
        } else {
          message("Multiple benchmarks found:")
          chosen <- utils::menu(bench_cols, title = "Select a benchmark to use for active weights:")
          if (chosen == 0) {
            stop("No benchmark selected. Aborting.")
          }
          bench_col <- bench_cols[chosen]
          message("Benchmark is: ", bench_col)
        }

        # Join and calculate Active_Weight = portfolio Weight - benchmark Weight
        df_pie <- dplyr::left_join(
          df_pie,
          universe_df %>% dplyr::select(tickers, !!bench_col),
          by = c("Asset" = "tickers")
        ) %>%
          dplyr::mutate(Active_Weight = .data$Weight - !!rlang::sym(bench_col)) %>%
          dplyr::select(.data$Asset, .data$Active_Weight) %>%
          dplyr::rename(Weight = .data$Active_Weight)
      }

      # 3) Let user choose how to subset assets: top x or by names/indices
      cat("\nHow do you want to choose which assets to display?\n")
      cat("1: Top x weights (by absolute value)\n")
      cat("2: Choose assets individually (by name or index)\n")
      choice_mode <- as.integer(readline(prompt = "Your choice: "))

      if (is.na(choice_mode) || !choice_mode %in% c(1,2)) {
        stop("Invalid choice for asset selection mode.")
      }

      if (choice_mode == 1) {
        # -- Option (i): Top x weights
        cat("How many assets do you want to show? ")
        n_choice <- as.integer(readline())
        if (is.na(n_choice) || n_choice < 1 || n_choice > length(df_pie$Asset)) {
          stop(paste0("Invalid number of assets. Must be between 1 and ", length(df_pie$Asset)))
        }
        # Reorder by abs weight and keep top n_choice
        df_pie <- df_pie %>%
          dplyr::arrange(dplyr::desc(abs(.data$Weight))) %>%
          dplyr::slice_head(n = n_choice)

      } else {
        # -- Option (ii): Choose assets by name or index
        cat("\nAssets:\n")
        for (i in seq_along(asset_names)) {
          cat(paste0(i, ": ", asset_names[i], "\n"))
        }
        cat("\nEnter 'all' for all assets,\nOR indices (e.g. '1,3'),\nOR names (e.g. 'IBM, AAPL'):\n")
        selection <- readline(prompt = "Your choice: ")
        selection <- trimws(selection)

        if (!nzchar(selection)) {
          stop("No selection provided.")
        }

        if (tolower(selection) == "all") {
          # keep everything
        } else {
          parts <- strsplit(selection, ",")[[1]]
          parts <- trimws(parts)
          # Check if numeric
          all_numeric <- suppressWarnings(!any(is.na(as.numeric(parts))))
          if (all_numeric) {
            indices <- as.numeric(parts)
            if (any(indices < 1 | indices > length(asset_names))) {
              stop("Some indices are out of range.")
            }
            chosen_assets <- asset_names[indices]
          } else {
            # Assume strings are asset names
            if (!all(parts %in% asset_names)) {
              stop("Some chosen assets are not in the set of available asset_names.")
            }
            chosen_assets <- parts
          }
          # Subset to only selected assets
          df_pie <- df_pie %>%
            dplyr::filter(.data$Asset %in% chosen_assets)
        }
      }

      # 4) Create proportions (for labeling) & assemble final data
      total_weight <- sum(df_pie$Weight)
      if (total_weight == 0) {
        stop("After selection, the sum of the weights is zero. Nothing to plot.")
      }

      df_pie <- df_pie %>%
        dplyr::mutate(
          prop = .data$Weight / total_weight,
          label = paste0(
            .data$Asset, "\n",
            scales::percent(.data$prop, accuracy = 0.1)  # negative sign if Weight<0
          )
        )

      # Create palette
      neon_pallete <- c(
        neon_green, neon_orange, neon_cyan, neon_blue,
        neon_yellow, neon_red, neon_turquoise, neon_lime,
        neon_magenta, neon_hotpink, neon_pink, neon_purple,
        vibrant_purple, black, white, blue_bg, neon_chartreuse,
        neon_indigo, neon_gold, neon_silver, neon_coral, neon_violet
      )

      # Plot title
      plot_title <- if (active_weights_mode) {
        paste("Portfolio Active Weights:", if (port_name == "") "not_identified" else port_name)
      } else {
        paste("Portfolio Weights:", if (port_name == "") "not_identified" else port_name)
      }

      # 5) Create bar plot - reorder x-axis by abs(Weight), but show actual sign
      p <- ggplot2::ggplot(
        df_pie,
        ggplot2::aes(
          x = stats::reorder(.data$Asset, -abs(.data$Weight)),  # order by abs
          y = .data$Weight,
          fill = .data$Asset
        )
      ) +
        ggplot2::geom_bar(stat = "identity", color = white, width = 0.7, size = 0.5) +
        ggplot2::scale_fill_manual(values = neon_pallete) +
        ggplot2::labs(
          title = plot_title,
          x     = "Assets",
          y     = if (active_weights_mode) "Active Weight (Port - Bench)" else "Weight"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.background  = ggplot2::element_rect(fill = blue_bg, color = NA),
          panel.background = ggplot2::element_rect(fill = blue_bg, color = NA),
          panel.grid.major = ggplot2::element_line(color = ggplot2::alpha("white", 0.2), size = 0.5),
          panel.grid.minor = ggplot2::element_line(color = ggplot2::alpha("white", 0.1), size = 0.3),
          panel.grid       = ggplot2::element_blank(),
          plot.title       = ggplot2::element_text(color = white, size = 16, face = "bold"),
          axis.text        = ggplot2::element_text(color = white),
          axis.title       = ggplot2::element_text(color = white),
          legend.position  = "none"
        )

      print(p)
      return(invisible(p))
    }


    #------------------------------------------------------------------------------------------------
    # 2) exp_ret_score - Bar chart
    #------------------------------------------------------------------------------------------------
    if (type == "Expected Return Score") {
      if (length(exp_ret_score) == 0)
        stop("No exp_ret_score found in 'x'.")
      if (length(asset_names) != length(exp_ret_score))
        stop("Mismatch between 'eligible_assets' and 'exp_ret_score' length.")

      # Create data frame
      df_bar <- data.frame(Asset = asset_names, ExpRet = exp_ret_score)

      # 1) Subset logic (top x or individual selections)
      cat("\nHow do you want to choose which assets to display?\n")
      cat("1: Top x by absolute ExpRet\n")
      cat("2: Choose assets individually (by name or index)\n")
      choice_mode <- as.integer(readline(prompt = "Your choice: "))

      if (is.na(choice_mode) || !choice_mode %in% c(1, 2)) {
        stop("Invalid choice for asset selection mode.")
      }

      if (choice_mode == 1) {
        # -- Option (i): Top x by absolute ExpRet
        cat("How many assets do you want to show? ")
        n_choice <- as.integer(readline())
        if (is.na(n_choice) || n_choice < 1 || n_choice > length(df_bar$Asset)) {
          stop(paste0("Invalid number of assets. Must be between 1 and ", length(df_bar$Asset)))
        }

        df_bar <- df_bar %>%
          dplyr::arrange(dplyr::desc(abs(.data$ExpRet))) %>%
          dplyr::slice_head(n = n_choice)

      } else {
        # -- Option (ii): Choose assets by name or index
        cat("\nAssets:\n")
        for (i in seq_along(asset_names)) {
          cat(paste0(i, ": ", asset_names[i], "\n"))
        }
        cat("\nEnter 'all' for all assets,\nOR indices (e.g. '1,3'),\nOR names (e.g. 'IBM, AAPL'):\n")
        selection <- readline(prompt = "Your choice: ")
        selection <- trimws(selection)

        if (!nzchar(selection)) {
          stop("No selection provided.")
        }

        if (tolower(selection) == "all") {
          # keep everything
        } else {
          parts <- strsplit(selection, ",")[[1]]
          parts <- trimws(parts)
          # Check if numeric
          all_numeric <- suppressWarnings(!any(is.na(as.numeric(parts))))
          if (all_numeric) {
            indices <- as.numeric(parts)
            if (any(indices < 1 | indices > length(asset_names))) {
              stop("Some indices are out of range.")
            }
            chosen_assets <- asset_names[indices]
          } else {
            # Assume strings are asset names
            if (!all(parts %in% asset_names)) {
              stop("Some chosen assets are not in the set of available asset_names.")
            }
            chosen_assets <- parts
          }
          df_bar <- df_bar %>% dplyr::filter(.data$Asset %in% chosen_assets)
        }
      }

      # If after selection we have no data, abort
      if (nrow(df_bar) == 0) {
        stop("No assets left after selection. Nothing to plot.")
      }

      # 2) Plot
      p <- ggplot2::ggplot(df_bar, ggplot2::aes(x = .data$Asset, y = .data$ExpRet, fill = .data$Asset)) +
        ggplot2::geom_bar(stat = "identity", color = black, alpha = 0.8) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.background  = ggplot2::element_rect(fill = blue_bg, color = NA),
          panel.background = ggplot2::element_rect(fill = blue_bg, color = NA),
          panel.grid.major = ggplot2::element_blank(),
          axis.text        = ggplot2::element_text(color = white),
          axis.title       = ggplot2::element_text(color = white),
          legend.position  = "none",
          plot.title       = ggplot2::element_text(color = white, size = 14, face = "bold")
        ) +
        ggplot2::labs(
          title = paste("Expected Return Scores:",
                        if (port_name == "") "not_identified" else port_name),
          x = "Assets",
          y = "Exp Ret Score"
        )

      print(p)
      return(invisible(p))
    }


    #------------------------------------------------------------------------------------------------
    # 3) risk_return - Scatterplot of risk (x) vs. exp_ret_score (y)
    #------------------------------------------------------------------------------------------------
    if (type == "Risk-Return Trade-off") {
      if (is.null(cov_mat))
        stop("No 'covariance_matrix' found in x for risk_return plot.")
      if (is.null(exp_ret_score))
        stop("No 'exp_ret_score' found in x for risk_return plot.")
      if (nrow(cov_mat) != length(asset_names))
        stop("Dimension mismatch between covariance_matrix and eligible_assets.")

      diag_risk <- diag(cov_mat)
      if (length(diag_risk) != length(asset_names))
        stop("Mismatch between diagonal of covariance_matrix and eligible_assets.")
      if (length(exp_ret_score) != length(asset_names))
        stop("Mismatch between exp_ret_score and eligible_assets.")

      df_scatter <- data.frame(
        Asset  = asset_names,
        Risk   = sqrt(diag_risk),
        ExpRet = exp_ret_score
      )

      # 1) Subset logic (top x or individual selections)
      cat("\nHow do you want to choose which assets to display?\n")
      cat("1: Top x by absolute ExpRet\n")
      cat("2: Choose assets individually (by name or index)\n")
      choice_mode <- as.integer(readline(prompt = "Your choice: "))

      if (is.na(choice_mode) || !choice_mode %in% c(1, 2)) {
        stop("Invalid choice for asset selection mode.")
      }

      if (choice_mode == 1) {
        cat("How many assets do you want to show? ")
        n_choice <- as.integer(readline())
        if (is.na(n_choice) || n_choice < 1 || n_choice > length(df_scatter$Asset)) {
          stop(paste0("Invalid number of assets. Must be between 1 and ", length(df_scatter$Asset)))
        }

        df_scatter <- df_scatter %>%
          dplyr::arrange(dplyr::desc(abs(.data$ExpRet))) %>%
          dplyr::slice_head(n = n_choice)

      } else {
        cat("\nAssets:\n")
        for (i in seq_along(asset_names)) {
          cat(paste0(i, ": ", asset_names[i], "\n"))
        }
        cat("\nEnter 'all' for all assets,\nOR indices (e.g. '1,3'),\nOR names (e.g. 'IBM, AAPL'):\n")
        selection <- readline(prompt = "Your choice: ")
        selection <- trimws(selection)

        if (!nzchar(selection)) {
          stop("No selection provided.")
        }

        if (tolower(selection) == "all") {
          # keep everything
        } else {
          parts <- strsplit(selection, ",")[[1]]
          parts <- trimws(parts)
          # Check if numeric
          all_numeric <- suppressWarnings(!any(is.na(as.numeric(parts))))
          if (all_numeric) {
            indices <- as.numeric(parts)
            if (any(indices < 1 | indices > length(asset_names))) {
              stop("Some indices are out of range.")
            }
            chosen_assets <- asset_names[indices]
          } else {
            if (!all(parts %in% asset_names)) {
              stop("Some chosen assets are not in the set of available asset_names.")
            }
            chosen_assets <- parts
          }
          df_scatter <- df_scatter %>% dplyr::filter(.data$Asset %in% chosen_assets)
        }
      }

      if (nrow(df_scatter) == 0) {
        stop("No assets left after selection. Nothing to plot.")
      }

      # 2) Plot
      p <- ggplot2::ggplot(df_scatter, ggplot2::aes(x = .data$Risk, y = .data$ExpRet, label = .data$Asset)) +
        ggplot2::geom_point(color = vibrant_purple, size = 3) +
        ggrepel::geom_text_repel(
          color = white,
          size = 3.5,
          box.padding = 0.3,
          segment.color = white
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.background  = ggplot2::element_rect(fill = blue_bg, color = NA),
          panel.background = ggplot2::element_rect(fill = blue_bg, color = NA),
          panel.grid.major = ggplot2::element_line(color = "grey20"),
          axis.text        = ggplot2::element_text(color = white),
          axis.title       = ggplot2::element_text(color = white),
          plot.title       = ggplot2::element_text(color = white, size = 14, face = "bold")
        ) +
        ggplot2::labs(
          title = paste("Risk vs. Expected Return Score:",
                        if (port_name == "") "not_identified" else port_name),
          x = "Expected Risk",
          y = "Expected Return Score"
        )

      print(p)
      return(invisible(p))
    }

    #------------------------------------------------------------------------------------------------
    # 4) correlation - heatmap of correlations
    #------------------------------------------------------------------------------------------------
    if (type == "Correlation") {
      if (is.null(corr_mat)) {
        if (is.null(cov_mat))
          stop("No covariance_matrix or correlation_matrix found in 'x'. Can't create correlation heatmap.")
        diag_sd  <- sqrt(diag(cov_mat))
        outer_sd <- diag_sd %*% t(diag_sd)
        corr_mat <- cov_mat / outer_sd
      }
      if (nrow(corr_mat) != length(asset_names))
        stop("Dimension mismatch between correlation_matrix and eligible_assets.")

      cat("Assets:\n")
      for (i in seq_along(asset_names)) {
        cat(paste0(i, ": ", asset_names[i], "\n"))
      }
      cat("\nEnter 'all' for all assets,\nOR indices (e.g. '1,3'),\nOR names (e.g. 'book_yield, roe_3m'):\n")
      selection <- readline(prompt = "Your choice: ")

      if (nzchar(selection) && tolower(selection) != "all") {
        parts <- strsplit(selection, ",")[[1]]
        parts <- trimws(parts)
        all_numeric <- suppressWarnings(!any(is.na(as.numeric(parts))))
        if (all_numeric) {
          indices <- as.numeric(parts)
          if (any(indices < 1 | indices > length(asset_names)))
            stop("Some indices are out of range.")
          assets_to_plot <- asset_names[indices]
        } else {
          if (!all(parts %in% asset_names))
            stop("Some chosen assets are not in the correlation matrix.")
          assets_to_plot <- parts
        }
      } else {
        assets_to_plot <- asset_names
      }

      sub_mat <- corr_mat[assets_to_plot, assets_to_plot, drop = FALSE]
      sub_mat[upper.tri(sub_mat, diag = FALSE)] <- NA

      df_cor          <- as.data.frame(sub_mat)
      df_cor$AssetRow <- rownames(df_cor)

      df_long <- df_cor %>%
        tidyr::pivot_longer(
          cols      = -AssetRow,
          names_to  = "AssetCol",
          values_to = "Correlation"
        )

      p <- ggplot2::ggplot(
        df_long,
        ggplot2::aes(
          x = factor(.data$AssetCol, levels = assets_to_plot),
          y = factor(.data$AssetRow, levels = rev(assets_to_plot)),
          fill = .data$Correlation
        )
      ) +
        ggplot2::geom_tile(color = "white") +

        # NEW: Text labels for each tile
        ggplot2::geom_text(
          ggplot2::aes(label = round(.data$Correlation, 2)),
          color = "black",  # or "white"
          na.rm = TRUE,
          size = 3
        ) +

        ggplot2::scale_fill_gradient2(
          low      = neon_pink,
          mid      = white,
          high     = neon_green,
          midpoint = 0,
          limits   = c(-1, 1)
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.background  = ggplot2::element_rect(fill = blue_bg, color = NA),
          panel.background = ggplot2::element_rect(fill = blue_bg, color = NA),
          axis.text.x      = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1, color = white),
          axis.text.y      = ggplot2::element_text(color = white),
          axis.title       = ggplot2::element_blank(),
          plot.title       = ggplot2::element_text(color = white, size = 14, face = "bold"),
          legend.position  = "right",
          legend.title     = ggplot2::element_text(color = white),
          legend.text      = ggplot2::element_text(color = white)
        ) +
        ggplot2::labs(
          title = paste("Correlation Heatmap:", if (port_name == "") "not_identified" else port_name)
        )

      print(p)
      return(invisible(p))
    }

    #------------------------------------------------------------------------------------------------
    # 5) relative_risk_contribution - bar chart
    #------------------------------------------------------------------------------------------------
    if (type == "Relative Risk Contribution") {
      if (is.null(rel_risk_contr))
        stop("No 'rel_risk_contr' found in 'x'. This plot requires relative risk contributions.")
      if (length(rel_risk_contr) != length(weights))
        stop("Mismatch between rel_risk_contr and weights lengths.")

      df_rrc <- data.frame(
        Asset  = asset_names,
        Weight = weights,
        RRC    = rel_risk_contr
      )

      # 1) Subset logic (top x or individual selections)
      cat("\nHow do you want to choose which assets to display?\n")
      cat("1: Top x by absolute RRC\n")
      cat("2: Choose assets individually (by name or index)\n")
      choice_mode <- as.integer(readline(prompt = "Your choice: "))

      if (is.na(choice_mode) || !choice_mode %in% c(1, 2)) {
        stop("Invalid choice for asset selection mode.")
      }

      if (choice_mode == 1) {
        cat("How many assets do you want to show? ")
        n_choice <- as.integer(readline())
        if (is.na(n_choice) || n_choice < 1 || n_choice > length(df_rrc$Asset)) {
          stop(paste0("Invalid number of assets. Must be between 1 and ", length(df_rrc$Asset)))
        }

        df_rrc <- df_rrc %>%
          dplyr::arrange(dplyr::desc(abs(.data$RRC))) %>%
          dplyr::slice_head(n = n_choice)

      } else {
        cat("\nAssets:\n")
        for (i in seq_along(asset_names)) {
          cat(paste0(i, ": ", asset_names[i], "\n"))
        }
        cat("\nEnter 'all' for all assets,\nOR indices (e.g. '1,3'),\nOR names (e.g. 'IBM, AAPL'):\n")
        selection <- readline(prompt = "Your choice: ")
        selection <- trimws(selection)

        if (!nzchar(selection)) {
          stop("No selection provided.")
        }

        if (tolower(selection) == "all") {
          # keep everything
        } else {
          parts <- strsplit(selection, ",")[[1]]
          parts <- trimws(parts)
          # Check if numeric
          all_numeric <- suppressWarnings(!any(is.na(as.numeric(parts))))
          if (all_numeric) {
            indices <- as.numeric(parts)
            if (any(indices < 1 | indices > length(asset_names))) {
              stop("Some indices are out of range.")
            }
            chosen_assets <- asset_names[indices]
          } else {
            if (!all(parts %in% asset_names)) {
              stop("Some chosen assets are not in the set of available asset_names.")
            }
            chosen_assets <- parts
          }
          df_rrc <- df_rrc %>% dplyr::filter(.data$Asset %in% chosen_assets)
        }
      }

      if (nrow(df_rrc) == 0) {
        stop("No assets left after selection. Nothing to plot.")
      }

      # 2) Pivot to long format & plot
      df_long <- tidyr::pivot_longer(
        df_rrc,
        cols = c("Weight", "RRC"),
        names_to = "Metric",
        values_to = "Value"
      )

      p <- ggplot2::ggplot(
        df_long,
        ggplot2::aes(x = .data$Asset, y = .data$Value, fill = .data$Metric)
      ) +
        ggplot2::geom_bar(position = "dodge", stat = "identity", color = black, alpha = 0.9) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.background  = ggplot2::element_rect(fill = blue_bg, color = NA),
          panel.background = ggplot2::element_rect(fill = blue_bg, color = NA),
          panel.grid.major = ggplot2::element_blank(),
          axis.text        = ggplot2::element_text(color = white),
          axis.title       = ggplot2::element_text(color = white),
          legend.title     = ggplot2::element_text(color = white),
          legend.text      = ggplot2::element_text(color = white),
          plot.title       = ggplot2::element_text(color = white, size = 14, face = "bold")
        ) +
        ggplot2::scale_fill_manual(values = c("Weight" = vibrant_purple, "RRC" = neon_green)) +
        ggplot2::labs(
          title = paste("Relative Risk Contribution:",
                        if (port_name == "") "not_identified" else port_name),
          x = "Asset",
          y = "Value",
          fill = "Metric"
        )

      print(p)
      return(invisible(p))
    }


    #------------------------------------------------------------------------------------------------
    # 6) efficient_frontier - scatterplot using random_port_weights
    #------------------------------------------------------------------------------------------------
    if (type == "Efficient Frontier") {
      if (is.null(random_port_wt))
        stop("No 'random_port_weights' found in 'x'. This plot requires random_port_weights.")
      if (is.null(cov_mat))
        stop("No covariance_matrix found in 'x'. This plot requires covariance_matrix.")
      if (is.null(exp_ret_score))
        stop("No exp_ret_score found in 'x'. This plot requires exp_ret_score.")
      if (length(random_port_wt$tickers) != length(asset_names)) {
        stop("random_port_weights does not have the same number of columns as eligible_assets.")
      }

      # Temporarily rename columns in random_port_wt to match asset_names
      rp_df <- random_port_wt[,-1]
      rownames(rp_df) <- asset_names

      # Calculate returns, risk, sharpe for each random portfolio
      rp_returns <- apply(rp_df, 2, function(w) sum(w * exp_ret_score))
      rp_risk    <- apply(rp_df, 2, function(w) sqrt(t(w) %*% cov_mat %*% w))
      rp_sharpe  <- rp_returns / rp_risk

      frontier_df <- data.frame(
        Return = rp_returns,
        Risk   = rp_risk,
        Sharpe = rp_sharpe
      )

      # Also compute risk/return for the "optimal" portfolio in x@weights
      opt_w      <- weights
      opt_return <- sum(opt_w * exp_ret_score)
      opt_risk   <- sqrt(t(opt_w) %*% cov_mat %*% opt_w)
      opt_sharpe <- opt_return / opt_risk

      # Create the scatterplot
      p <- ggplot2::ggplot(frontier_df, ggplot2::aes(x = .data$Risk, y = .data$Return, color = .data$Sharpe)) +
        ggplot2::geom_point(size = 3, alpha = 0.7) +
        ggplot2::scale_color_gradient(low = neon_pink, high = neon_green) +
        # Add the "optimal" portfolio point
        ggplot2::annotate(
          "point",
          x = opt_risk, y = opt_return,
          color = "red", size = 4, shape = 17
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.background  = ggplot2::element_rect(fill = blue_bg, color = NA),
          panel.background = ggplot2::element_rect(fill = blue_bg, color = NA),
          axis.text        = ggplot2::element_text(color = white),
          axis.title       = ggplot2::element_text(color = white),
          legend.title     = ggplot2::element_text(color = white),
          legend.text      = ggplot2::element_text(color = white),
          plot.title       = ggplot2::element_text(color = white, size = 14, face = "bold")
        ) +
        ggplot2::labs(
          title = paste("Efficient Frontier:", if (port_name == "") "not_identified" else port_name),
          x = "Expected Risk (Std Dev)",
          y = "Expected Return",
          color = "Sharpe"
        )

      print(p)
      return(invisible(p))
    }
    #------------------------------------------------------------------------------------------------
    # 7) weight distribution
    #------------------------------------------------------------------------------------------------
    if (type == "Random Weights Distribution") {
      # Extract relevant slots
      random_port_weights <- x@random_port_weights
      if (any(is.null(x@ind_max_weights), is.null(x@ind_min_weights))) {
        stop("Random Weights Distribution is only available when max_abs_active_individual_weight is set.")
      }
      ind_max_weights <- x@ind_max_weights
      ind_min_weights <- x@ind_min_weights
      asset_names     <- x@eligible_assets

      # Basic checks
      if (is.null(random_port_weights) || is.null(ind_max_weights) || is.null(ind_min_weights)) {
        stop("random_port_weights, ind_max_weights, and ind_min_weights must be provided.")
      }
      if (length(ind_max_weights) != length(asset_names) ||
          length(ind_min_weights) != length(asset_names)) {
        stop("Mismatch between constraints and eligible assets.")
      }

      # Prepare data for plotting
      # random_port_weights is assumed to have first column named 'tickers'
      # and subsequent columns with random weight draws
      weights_long <- as.data.frame(t(random_port_weights[, -1]))
      colnames(weights_long) <- random_port_weights$tickers

      weights_long <- tidyr::pivot_longer(
        weights_long,
        cols = dplyr::everything(),
        names_to = "assets",
        values_to = "weights"
      )

      constraints <- data.frame(
        assets          = asset_names,
        ind_max_weights = ind_max_weights,
        ind_min_weights = ind_min_weights
      )

      plot_data <- dplyr::left_join(weights_long, constraints, by = "assets")

      # Subset logic: Let user choose how to filter the displayed assets
      cat("\nHow do you want to choose which assets to display?\n")
      cat("1: Top x by average absolute random weight\n")
      cat("2: Choose assets individually (by name or index)\n")
      choice_mode <- as.integer(readline(prompt = "Your choice: "))

      if (is.na(choice_mode) || !choice_mode %in% c(1, 2)) {
        stop("Invalid choice for asset selection mode.")
      }

      if (choice_mode == 1) {
        # 1a) Calculate a ranking metric: for example, average absolute weight
        df_avg <- plot_data %>%
          dplyr::group_by(.data$assets) %>%
          dplyr::summarize(avg_abs_weight = mean(abs(.data$weights), na.rm = TRUE)) %>%
          dplyr::arrange(dplyr::desc(.data$avg_abs_weight))

        # 1b) Ask how many top assets user wants
        cat("How many assets do you want to show? ")
        n_choice <- as.integer(readline())
        if (is.na(n_choice) || n_choice < 1 || n_choice > nrow(df_avg)) {
          stop(paste0("Invalid number of assets. Must be between 1 and ", nrow(df_avg)))
        }

        # 1c) Keep only those top assets in both plot_data & constraints
        top_assets <- df_avg$assets[1:n_choice]
        plot_data  <- dplyr::filter(plot_data, .data$assets %in% top_assets)
        constraints <- dplyr::filter(constraints, .data$assets %in% top_assets)

      } else {
        # 2) User picks assets by name or index
        cat("\nAssets:\n")
        for (i in seq_along(asset_names)) {
          cat(paste0(i, ": ", asset_names[i], "\n"))
        }
        cat("\nEnter 'all' for all assets,\nOR indices (e.g. '1,3'),\nOR names (e.g. 'PETR4, VALE3'):\n")
        selection <- readline(prompt = "Your choice: ")
        selection <- trimws(selection)

        if (!nzchar(selection)) {
          stop("No selection provided.")
        }

        if (tolower(selection) == "all") {
          # keep everything
        } else {
          parts <- strsplit(selection, ",")[[1]]
          parts <- trimws(parts)
          # Check if numeric
          all_numeric <- suppressWarnings(!any(is.na(as.numeric(parts))))
          if (all_numeric) {
            indices <- as.numeric(parts)
            if (any(indices < 1 | indices > length(asset_names))) {
              stop("Some indices are out of range.")
            }
            chosen_assets <- asset_names[indices]
          } else {
            # Assume strings are asset names
            if (!all(parts %in% asset_names)) {
              stop("Some chosen assets are not in the set of available asset_names.")
            }
            chosen_assets <- parts
          }
          plot_data  <- dplyr::filter(plot_data, .data$assets %in% chosen_assets)
          constraints <- dplyr::filter(constraints, .data$assets %in% chosen_assets)
        }
      }

      # Final check
      if (nrow(plot_data) == 0) {
        stop("No assets left after selection. Nothing to plot.")
      }

      # Create the jitter plot
      p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$assets, y = .data$weights)) +
        ggplot2::geom_jitter(width = 0.2, color = "#6A0DAD", alpha = 0.5, size = 2) +
        # Show max constraint
        ggplot2::geom_point(
          data  = constraints,
          ggplot2::aes(x = .data$assets, y = .data$ind_max_weights),
          color = "#FF5F1F", size = 3, shape = 17
        ) +
        # Show min constraint
        ggplot2::geom_point(
          data  = constraints,
          ggplot2::aes(x = .data$assets, y = .data$ind_min_weights),
          color = "#39FF14", size = 3, shape = 17
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.background  = ggplot2::element_rect(fill = "#001f3f", color = NA),
          panel.background = ggplot2::element_rect(fill = "#001f3f", color = NA),
          panel.grid       = ggplot2::element_blank(),
          axis.text        = ggplot2::element_text(color = "white"),
          axis.title       = ggplot2::element_text(color = "white"),
          plot.title       = ggplot2::element_text(color = "white", size = 14, face = "bold")
        ) +
        ggplot2::labs(
          title = "Weight Distribution with Constraints",
          x     = "Assets",
          y     = "Weights"
        )

      print(p)
      return(invisible(p))
    }

    #------------------------------------------------------------------------------------------------
    # 8) group_composition - compare group weights to a benchmark if available
    #------------------------------------------------------------------------------------------------
    if (type == "Group Composition") {

      if (is.null(groups_df)){
        stop("No valid groups slot found. This plot requires group information.")
      }
      required_cols <- c("id", "tickers", "dates")
      if (!all(required_cols %in% colnames(groups_df))) {
        stop("groups slot must contain at least 'id','tickers','dates'.")
      }
      # Identify group columns
      group_cols <- setdiff(colnames(groups_df), required_cols)
      if (length(group_cols) == 0)
        stop("No group columns found in 'groups' data.")

      # Merge each eligible asset with its group(s)
      port_alloc    <- data.frame(tickers = asset_names, weights = weights)
      groups_merged <- dplyr::left_join(port_alloc, groups_df, by = "tickers")
      groups_merged <- dplyr::filter(groups_merged, !is.na(.data$id))

      # Ask for which group to plot
      if (length(group_cols) == 1){
        main_group_col <- group_cols[1]
      }
      if (length(group_cols) == 0){
        main_group_col <- NULL
      }
      if (length(group_cols) > 1){
        for (i in seq_along(group_cols)) {
          cat(paste0(i, ": ", group_cols[i], "\n"))
        }
        selection <- readline(prompt = "Please choose the number correspondig to the group classification of your choice: ")
        selection <- as.numeric(selection)
        if (is.na(selection) || length(selection) != 1) {
          stop("Invalid selection.")
        }
        main_group_col <- group_cols[selection]
        if (!main_group_col %in% group_cols){
          stop("Invalid group selected.")
        }
      }

      df_by_group <- groups_merged %>%
        dplyr::group_by(.data[[main_group_col]]) %>%
        dplyr::summarize(Portfolio_Weight = sum(.data$weights), .groups = "drop")

      # Attempt to detect a benchmark column in the universe df
      bench_cols <- grep("_bench_weights$", colnames(universe_df), value = TRUE)

      # Ask for which bench to plot
      if (length(bench_cols) == 1){
        bench_col <- bench_cols[1]
      }
      if (length(bench_cols) == 0){
        bench_col <- NULL
      }
      if (length(bench_cols) > 1){
        for (i in seq_along(bench_cols)) {
          cat(paste0(i, ": ", bench_cols[i], "\n"))
        }
        selection <- readline(prompt = "Please choose the number correspondig to the benchmark of your choice:: ")
        selection <- as.numeric(selection)
        if (is.na(selection) || length(selection) != 1) {
          stop("Invalid selection.")
        }
        bench_col <- bench_cols[selection]
        if (!bench_col %in% bench_cols){
          stop("Invalid benchmark selected.")
        }
      }


      if (!is.null(bench_col) && bench_col %in% colnames(universe_df)) {
        # Merge group info with the universe benchmark column
        bench_merged <- dplyr::left_join(groups_df, dplyr::select(universe_df, -group_cols), by = c("tickers"))

        df_bench <- bench_merged %>%
          dplyr::group_by(.data[[main_group_col]]) %>%
          dplyr::summarize(Benchmark_Weight = sum(.data[[bench_col]], na.rm = TRUE), .groups = "drop")

        df_compare <- dplyr::full_join(df_by_group, df_bench, by = main_group_col) %>%
          tidyr::replace_na(list(Portfolio_Weight = 0, Benchmark_Weight = 0))

        df_plot <- tidyr::pivot_longer(
          df_compare,
          cols = c("Portfolio_Weight", "Benchmark_Weight"),
          names_to = "Type",
          values_to = "Weight"
        )

        p <- ggplot2::ggplot(df_plot, ggplot2::aes_string(x = main_group_col, y = "Weight", fill = "Type")) +
          ggplot2::geom_bar(stat = "identity", position = "dodge", color = black) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            plot.background  = ggplot2::element_rect(fill = blue_bg, color = NA),
            panel.background = ggplot2::element_rect(fill = blue_bg, color = NA),
            panel.grid.major = ggplot2::element_blank(),
            axis.text        = ggplot2::element_text(color = white),
            axis.title       = ggplot2::element_text(color = white),
            legend.title     = ggplot2::element_text(color = white),
            legend.text      = ggplot2::element_text(color = white),
            plot.title = ggplot2::element_text(color = "white", size = 14, face = "bold")
          ) +
          ggplot2::labs(
            title = paste("Group Composition vs. Benchmark:", if (port_name == "") "not_identified" else port_name),
            x     = main_group_col,
            y     = "Weight",
            fill  = "Allocation"
          )

        print(p)
        return(invisible(p))

      } else {
        # No benchmark found, just the portfolio composition
        p <- ggplot2::ggplot(df_by_group, ggplot2::aes_string(x = main_group_col, y = "Portfolio_Weight", fill = main_group_col)) +
          ggplot2::geom_bar(stat = "identity", color = black) +
          ggplot2::theme_minimal() +
          ggplot2::theme(
            plot.background  = ggplot2::element_rect(fill = blue_bg, color = NA),
            panel.background = ggplot2::element_rect(fill = blue_bg, color = NA),
            panel.grid.major = ggplot2::element_blank(),
            axis.text        = ggplot2::element_text(color = white),
            axis.title       = ggplot2::element_text(color = white),
            legend.position  = "none"
          ) +
          ggplot2::labs(
            title = paste("Group Composition:", if (port_name == "") "not_identified" else port_name),
            x = main_group_col,
            y = "Weight"
          )

        print(p)
        return(invisible(p))
      }
    }

    #------------------------------------------------------------------------------------------------
    # If none of the above matched, show an error
    #------------------------------------------------------------------------------------------------
    stop(paste("Plot type", type, "not recognized."))
  }
)


#' @title Plot Method for port_backtest_results Class
#' @description Generates various plots to visualize metrics from the `port_backtest_results` object.
#' Users can select which plot to display by specifying the `plot_id` parameter.
#'
#' @param x An object of class `port_backtest_results`.
#' @param plot_id A character string or numeric value specifying which plot to display. If `NULL`, the user will be prompted.
#' @param vertical_lines Optional. A vector of `Date` objects indicating vertical lines to display in time-series plots (e.g., rebalance dates). If `NULL`, the user will be prompted.
#'
#' @return Invisibly returns the input object.
#' @export
setMethod("plot", "port_backtest_results", function(x, plot_id = NULL, vertical_lines = NULL) {

  #Check for packages
  if (!requireNamespace("gridExtra", quietly = TRUE) || !requireNamespace("scales", quietly = TRUE)) {
    stop("Packages 'gridExtra' and 'scales' are required to generate plots. Please install them using install.packages().")
  }

  # Define colors for plotting
  deep_navy <- "#000033"
  black <- "#000000"
  white <- "#FFFFFF"
  vibrant_purple <- "#6A0DAD"
  neon_green <- "#39FF14"
  neon_orange <- "#FF5F1F"
  blue_bg <- "#001f3f"
  neon_yellow <- "#FFDC00"
  neon_pink <- "#FF007F"
  cyan <- "#7FDBFF"

  # Ensure Contribution_Type uses "Positive" and "Negative" for consistency
  pos_color <- neon_green    # Define positive contribution color
  neg_color <- "red"         # Define negative contribution color

  # List of available plots
    available_plots <- c(
      "Time-Series Weights by Tickers",
      "Time-Series Weights by Stock Group",
      "Cross-Sectional Weights Statistic by Tickers",
      "Cross-Sectional Weights Statistic by Stock Group",
      "Tile Heatmap of Weights by Tickers",
      "Tile Heatmap of Weights by Stock Group",
      "Time-Series of Groups Composition",
      "Time-Series of Expected Return Score by Tickers",
      "Time-Series of Expected Return Score by Stock Group",
      "Time-Series of Expected Return Score by Eligibility",
      "Box-plot of Expected Return Score by Stock Group",
      "Box-plot of Expected Return Score by Eligibility",
      "Plot Subjacent Final Port",
      "Time-Series of Port Returns",
      "Cross-Sectional Performance Metric Plot",
      "Time-Series of Transaction Costs",
      "Time-Series of Port Metrics"
      )


  if (is.null(plot_id)) {
    cat("\nPlease choose a plot to display:\n")
    for (i in seq_along(available_plots)) {
      cat(paste0(i, ": ", available_plots[i], "\n"))
    }
    selection <- readline(prompt = "Enter the number of your choice: ")
    plot_id <- as.numeric(selection)
    if (is.na(plot_id) || plot_id < 1 || plot_id > length(available_plots)) {
      stop("Invalid selection.")
    }
  }

  # Determine plot name from the selection
  if (is.numeric(plot_id)) {
    if (plot_id >= 1 && plot_id <= length(available_plots)) {
      plot_name <- available_plots[plot_id]
    } else {
      stop("Invalid plot number. Please select a valid plot.")
    }
  } else if (is.character(plot_id)) {
    if (plot_id %in% available_plots) {
      plot_name <- plot_id
    } else {
      stop("Invalid 'plot_id' specified. Available options are:\n",
           paste(available_plots, collapse = ", "))
    }
  } else {
    stop("'plot_id' must be either a string or a number corresponding to the plot.")
  }

  # Define the data sources
  port_weights_m_df <- x@port_weights_m_df
  stock_universe_m_df <- x@stock_universe_m_df
  final_stock_universe_m_d_ref <- x@final_stock_universe_m_d_ref
  port_returns_m_xts <- x@port_returns_m_xts
  port_costs_m_xts <- x@port_costs_m_xts
  port_metrics_m_xts <- x@port_metrics_m_xts
  final_stock_port <- x@final_stock_port

  #Prompt the user if he wants to plot rebalance dates
  if (is.null(vertical_lines)){
    rebalance_dates <- readline(prompt = "Do you want to plot rebalance dates? (yes/no): ")
    if (rebalance_dates %in% c("y", "yes")) {
      vertical_lines <- as.Date(x@port_backtest_workflow$rebalance_dates)
    }
  }

  #Select Group if plot_name contains "Stock Group"
  if (grepl("Stock Group", plot_name)) {

    # Get character columns, excluding the first three columns
    char_cols <- names(which(sapply(stock_universe_m_df@data[,-c(1:3)], is.character)))

    #Stop if char_cols is empty
    if (length(char_cols) == 0) {
      stop("No character columns found in the stock universe data.")
    }

    # Display available columns with numbers
    cat(crayon::white("\nPlease choose a column to consider as group:\n"))
    for (i in seq_along(char_cols)) {
      cat(crayon::white(paste0(i, ": ", char_cols[i], "\n")))
    }

    # Prompt user input
    selection <- readline(prompt = crayon::white("Enter the column name or number: "))

    # Check if the input is numeric (choosing by index) or a name
    if (nzchar(selection)) {
      if (grepl("^[0-9]+$", selection)) {  # If input is numeric
        selection <- as.numeric(selection)
        if (selection < 1 || selection > length(char_cols)) {
          stop("Invalid selection. Please enter a valid number from the list.")
        }
        group_col <- char_cols[selection]
      } else {  # If input is a column name
        if (!(selection %in% char_cols)) {
          stop("Invalid selection. Please enter a valid column name from the list.")
        }
        group_col <- selection
      }
    } else {
      stop("No input provided. Please enter a valid selection.")
    }

    #Check if the column is in the dataframe
    if (!(group_col %in% names(stock_universe_m_df@data))){
      stop("The column entered is not available in the stock universe.")
    }

  }


  # Plot 1: Time-Series Metrics by Ticker
  if (plot_name == "Time-Series Weights by Tickers") {

    plot_type <- "time_series"
    clustering_variables <- "tickers"
    calc_stat <- "mean"

    plot(port_weights_m_df, type = plot_type, calc_stat = calc_stat, clustering_variables = clustering_variables)


  } else if (plot_name == "Time-Series Weights by Stock Group") {

    plot_type <- "time_series"
    clustering_variables <- group_col
    variable <- "weights"

    plot(stock_universe_m_df, type = plot_type, variable = variable, clustering_variables = clustering_variables)


  } else if (plot_name == "Cross-Sectional Weights Statistic by Tickers"){

    plot_type <- "cross_sectional"
    clustering_variables <- "tickers"

    plot(port_weights_m_df, type = plot_type, clustering_variables = clustering_variables)


  } else if (plot_name == "Cross-Sectional Weights Statistic by Stock Group"){

    plot_type <- "cross_sectional"
    clustering_variables <- group_col
    variable <- "weights"

    plot(stock_universe_m_df, type = plot_type, clustering_variables = clustering_variables, variable = variable)



  } else if (plot_name == "Tile Heatmap of Weights by Tickers"){

    plot_type <- "tile_heatmap"
    clustering_variables <- "tickers"

    plot(port_weights_m_df, type = plot_type, clustering_variables = clustering_variables)

  } else if (plot_name == "Tile Heatmap of Weights by Stock Group"){

    plot_type <- "tile_heatmap"
    clustering_variables <- group_col
    variable <- "weights"

    plot(stock_universe_m_df, type = plot_type, clustering_variables = clustering_variables, variable = variable)

  } else if (plot_name == "Time-Series of Groups Composition"){

    plot_type <- "composition"

    plot(stock_universe_m_df, type = plot_type, variable = group_col)


  } else if (plot_name == "Time-Series of Expected Return Score by Tickers"){

    plot_type <- "time_series"
    clustering_variables <- "tickers"
    variable <- "exp_ret_score"
    calc_stat <- "mean"

    plot(stock_universe_m_df, type = plot_type, clustering_variables = clustering_variables, variable = variable, calc_stat = calc_stat)


  } else if (plot_name == "Time-Series of Expected Return Score by Stock Group"){

    plot_type <- "time_series"
    clustering_variables <- group_col
    variable <- "exp_ret_score"
    calc_stat <- "mean"

    plot(stock_universe_m_df, type = plot_type, clustering_variables = clustering_variables, variable = variable, calc_stat = calc_stat)

  } else if (plot_name == "Time-Series of Expected Return Score by Eligibility"){

    stock_universe_m_df@data$eligibility <- ifelse(stock_universe_m_df@data$is_eligible == 1, "elected", "not_elected")

    plot_type <- "time_series"
    clustering_variables <- "eligibility"
    variable <- "exp_ret_score"
    calc_stat <- "mean"

    plot(stock_universe_m_df, type = plot_type, clustering_variables = clustering_variables, variable = variable, calc_stat = calc_stat)


  } else if (plot_name == "Box-plot of Expected Return Score by Stock Group"){

    plot_type <- "boxplot"
    clustering_variables <- group_col
    variable <- "exp_ret_score"
    calc_stat <- "mean"

    plot(stock_universe_m_df, type = plot_type, clustering_variables = clustering_variables, variable = variable, calc_stat = calc_stat)


  } else if (plot_name == "Box-plot of Expected Return Score by Eligibility"){

    stock_universe_m_df@data$eligibility <- ifelse(stock_universe_m_df@data$is_eligible == 1, "elected", "not_elected")

    plot_type <- "boxplot"
    clustering_variables <- "eligibility"
    variable <- "exp_ret_score"
    calc_stat <- "mean"

    plot(stock_universe_m_df, type = plot_type, clustering_variables = clustering_variables, variable = variable, calc_stat = calc_stat)


  } else if (plot_name == "Plot Subjacent Final Port"){

    plot(final_stock_port)


  } else if (plot_name == "Time-Series of Port Returns"){

    plot_perf_metric = FALSE
    if("selected_bench_return" %in% colnames(port_returns_m_xts@data)){
      bench_returns_m_xts <- port_returns_m_xts
      bench_returns_m_xts@data <- bench_returns_m_xts@data[, "selected_bench_return"]
    } else {
      bench_returns_m_xts <- NULL
    }

    port_returns_m_xts <- port_returns_m_xts
    port_returns_m_xts@data <- port_returns_m_xts@data[, c("raw_return", "net_return", "raw_active_return", "net_active_return")]

    #Add first date
    first_return_date <- zoo::index(port_returns_m_xts@data)[1]
      ##Port Returns
      port_returns_m_xts@data <- rbind(
        xts::xts(data.frame(raw_return = 0, net_return = 0, raw_active_return = 0, net_active_return = 0),
                 order.by = lubridate::add_with_rollback(first_return_date, months(-1))), #Add a first row with 0 values
        port_returns_m_xts@data
      )
      ##Bench Returns
      if(!is.null(bench_returns_m_xts)){
        bench_returns_m_xts@data <- rbind(
          xts::xts(data.frame(selected_bench_return = 0),
                   order.by = lubridate::add_with_rollback(first_return_date, months(-1))), #Add a first row with 0 values
          bench_returns_m_xts@data
        )
      }

    plot(port_returns_m_xts, benchmark_returns_m_xts = bench_returns_m_xts, cumulative = TRUE, plot_perf_metric = plot_perf_metric,
         vertical_lines = vertical_lines)

  } else if (plot_name == "Cross-Sectional Performance Metric Plot"){

    plot_perf_metric = TRUE
    if("selected_bench_return" %in% colnames(port_returns_m_xts@data)){
      bench_returns_m_xts <- port_returns_m_xts
      bench_returns_m_xts@data <- bench_returns_m_xts@data[, "selected_bench_return"]
    } else {
      bench_returns_m_xts <- NULL
    }

    port_returns_m_xts <- port_returns_m_xts
    port_returns_m_xts@data <- port_returns_m_xts@data[, c("raw_return", "net_return", "raw_active_return", "net_active_return")]

    plot(port_returns_m_xts,  benchmark_returns_m_xts = bench_returns_m_xts, plot_perf_metric = plot_perf_metric)

  } else if (plot_name == "Time-Series of Transaction Costs"){

    plot(port_costs_m_xts, vertical_lines = vertical_lines)

  } else if (plot_name == "Time-Series of Port Metrics"){

    if (!is.null(port_metrics_m_xts)){
      plot(port_metrics_m_xts, vertical_lines = vertical_lines)
    } else {
      stop("No port_metrics available to plot.")
    }

  } else {
    stop("The plot name provided is not valid. Please choose one of the following: ", paste(available_plots, collapse = ", "))
  }

})


#' @title Plot Method for port_backtest_cohort Class
#' @description Generates various plots to visualize metrics from a \code{port_backtest_cohort} object.
#' Users can select which plot to display by specifying the \code{plot_id} parameter.
#'
#' @param x An object of class \code{port_backtest_cohort}.
#' @param plot_id A character string or numeric value specifying which plot to display.
#' @param vertical_lines Optional. A named list or vector specifying vertical lines to add to time-series plots. Used for highlighting events.
#'
#' @return Invisibly returns the input object.
#' @export
setMethod("plot", "port_backtest_cohort", function(x, plot_id = NULL, vertical_lines = NULL) {

  #Check for packages
  if (!requireNamespace("gridExtra", quietly = TRUE) || !requireNamespace("scales", quietly = TRUE)) {
    stop("Packages 'gridExtra' and 'scales' are required to generate plots. Please install them using install.packages().")
  }

  # Define colors for plotting
  deep_navy <- "#000033"
  black <- "#000000"
  white <- "#FFFFFF"
  vibrant_purple <- "#6A0DAD"
  neon_green <- "#39FF14"
  neon_orange <- "#FF5F1F"
  blue_bg <- "#001f3f"
  neon_yellow <- "#FFDC00"
  neon_pink <- "#FF007F"
  cyan <- "#7FDBFF"

  # Ensure Contribution_Type uses "Positive" and "Negative" for consistency
  pos_color <- neon_green    # Define positive contribution color
  neg_color <- "red"         # Define negative contribution color

  # List of available plots
  available_plots <- c(
    "Time-Series of Raw Returns",
    "Time-Series of Net Returns",
    "Time-Series of Raw Active Returns",
    "Time-Series of Net Active Returns",
    "Cross-Sectional Raw Performance Metric Plot",
    "Cross-Sectional Net Performance Metric Plot",
    "Cross-Sectional Raw Active Performance Metric Plot",
    "Cross-Sectional Net Active Performance Metric Plot",
    "Time-Series of Direct Costs",
    "Time-Series of Market Impact Costs",
    "Time-Series of Total Costs",
    "Time-Series of Turnover",
    "Time-Series of Port Metrics",
    "Weights Correlogram",
    "Weights Radar")

  if (is.null(plot_id)) {
    cat("\nPlease choose a plot to display:\n")
    for (i in seq_along(available_plots)) {
      cat(paste0(i, ": ", available_plots[i], "\n"))
    }
    selection <- readline(prompt = "Enter the number of your choice: ")
    plot_id <- as.numeric(selection)
    if (is.na(plot_id) || plot_id < 1 || plot_id > length(available_plots)) {
      stop("Invalid selection.")
    }
  }

  # Determine plot name from the selection
  if (is.numeric(plot_id)) {
    if (plot_id >= 1 && plot_id <= length(available_plots)) {
      plot_name <- available_plots[plot_id]
    } else {
      stop("Invalid plot number. Please select a valid plot.")
    }
  } else if (is.character(plot_id)) {
    if (plot_id %in% available_plots) {
      plot_name <- plot_id
    } else {
      stop("Invalid 'plot_id' specified. Available options are:\n",
           paste(available_plots, collapse = ", "))
    }
  } else {
    stop("'plot_id' must be either a string or a number corresponding to the plot.")
  }

  # Define the data sources
  port_weights_m_df <- x@port_weights_m_df
  port_costs_m_xts_list <- x@port_costs_m_xts_list
  port_returns_m_xts_list <- x@port_returns_m_xts_list
  port_metrics_m_xts_list <- x@port_metrics_m_xts_list
  port_backtest_results_list <- x@port_backtest_results_list

  #Plot 1
  if (plot_name == "Time-Series of Raw Returns"){

    raw_returns_m_xts <- port_returns_m_xts_list$raw_returns_m_xts

    plot_perf_metric <- FALSE
    if (!is.null(x@backtest_workflow_common$selected_benchmark)){
      bench_returns_m_xts <- raw_returns_m_xts
      bench_returns_m_xts@data <- bench_returns_m_xts@data[, "selected_bench_return"]
      raw_returns_m_xts@data <- raw_returns_m_xts@data[, -which(colnames(raw_returns_m_xts@data) == "selected_bench_return")]
    } else {
      bench_returns_m_xts <- NULL
    }

    #Add first date
    first_return_date <- zoo::index(raw_returns_m_xts@data)[1]
    ##Port Returns
    init_df <- as.data.frame(matrix(0, ncol = ncol(raw_returns_m_xts@data), nrow = 1))
    colnames(init_df) <- colnames(raw_returns_m_xts@data)
    raw_returns_m_xts@data <- rbind(
      xts::xts(init_df, order.by = lubridate::add_with_rollback(first_return_date, months(-1))), #Add a first row with 0 values
      raw_returns_m_xts@data
    )
    ##Bench Returns
    if(!is.null(bench_returns_m_xts)){
      bench_returns_m_xts@data <- rbind(
        xts::xts(data.frame(selected_bench_return = 0),
                 order.by = lubridate::add_with_rollback(first_return_date, months(-1))), #Add a first row with 0 values
        bench_returns_m_xts@data
      )
    }

    plot(raw_returns_m_xts, benchmark_returns_m_xts = bench_returns_m_xts, cumulative = TRUE, plot_perf_metric = plot_perf_metric,
         vertical_lines = vertical_lines)

  }

  #Plot 2
  if (plot_name == "Time-Series of Net Returns"){

    net_returns_m_xts <- port_returns_m_xts_list$net_returns_m_xts

    plot_perf_metric <- FALSE
    if (!is.null(x@backtest_workflow_common$selected_benchmark)){
      bench_returns_m_xts <- net_returns_m_xts
      bench_returns_m_xts@data <- bench_returns_m_xts@data[, "selected_bench_return"]
      net_returns_m_xts@data <- net_returns_m_xts@data[, -which(colnames(net_returns_m_xts@data) == "selected_bench_return")]
    } else {
      bench_returns_m_xts <- NULL
    }

    #Add first date
    first_return_date <- zoo::index(net_returns_m_xts@data)[1]
    ##Port Returns
    init_df <- as.data.frame(matrix(0, ncol = ncol(net_returns_m_xts@data), nrow = 1))
    colnames(init_df) <- colnames(net_returns_m_xts@data)

    net_returns_m_xts@data <- rbind(
      xts::xts(init_df, order.by = lubridate::add_with_rollback(first_return_date, months(-1))), #Add a first row with 0 values

      net_returns_m_xts@data
    )
    ##Bench Returns
    if(!is.null(bench_returns_m_xts)){
      bench_returns_m_xts@data <- rbind(
        xts::xts(data.frame(selected_bench_return = 0),
                 order.by = lubridate::add_with_rollback(first_return_date, months(-1))), #Add a first row with 0 values
        bench_returns_m_xts@data
      )
    }

    plot(net_returns_m_xts, benchmark_returns_m_xts = bench_returns_m_xts, cumulative = TRUE, plot_perf_metric = plot_perf_metric,
         vertical_lines = vertical_lines)

  }

  #Plot 3
  if (plot_name == "Time-Series of Raw Active Returns"){

    if (is.null(x@backtest_workflow_common$selected_benchmark)){
      stop("This plot is only applicable to cohorts with benchmarks")
    }

    plot_perf_metric <- FALSE
    raw_active_returns_m_xts <- port_returns_m_xts_list$raw_active_returns_m_xts

    #Add first date
    first_return_date <- zoo::index(raw_active_returns_m_xts@data)[1]
    ##Port Returns
    init_df <- as.data.frame(matrix(0, ncol = ncol(raw_active_returns_m_xts@data), nrow = 1))
    colnames(init_df) <- colnames(raw_active_returns_m_xts@data)

    raw_active_returns_m_xts@data <- rbind(
      xts::xts(init_df, order.by = lubridate::add_with_rollback(first_return_date, months(-1))), #Add a first row with 0 values

      raw_active_returns_m_xts@data
    )

    plot(raw_active_returns_m_xts, cumulative = TRUE, plot_perf_metric = plot_perf_metric, vertical_lines = vertical_lines)

  }

  #Plot 4
  if (plot_name == "Time-Series of Net Active Returns"){

    if (is.null(x@backtest_workflow_common$selected_benchmark)){
      stop("This plot is only applicable to cohorts with benchmarks")
    }

    plot_perf_metric <- FALSE
    net_active_returns_m_xts <- port_returns_m_xts_list$net_active_returns_m_xts

    #Add first date
    first_return_date <- zoo::index(net_active_returns_m_xts@data)[1]
    ##Port Returns
    init_df <- as.data.frame(matrix(0, ncol = ncol(net_active_returns_m_xts@data), nrow = 1))
    colnames(init_df) <- colnames(net_active_returns_m_xts@data)

    net_active_returns_m_xts@data <- rbind(
      xts::xts(init_df, order.by = lubridate::add_with_rollback(first_return_date, months(-1))), #Add a first row with 0 values

      net_active_returns_m_xts@data
    )


    plot(net_active_returns_m_xts, cumulative = TRUE, plot_perf_metric = plot_perf_metric, vertical_lines = vertical_lines)

  }

  #Plot 5
  if (plot_name == "Cross-Sectional Raw Performance Metric Plot"){

    raw_returns_m_xts <- port_returns_m_xts_list$raw_returns_m_xts

    plot_perf_metric <- TRUE
    if (!is.null(x@backtest_workflow_common$selected_benchmark)){
      bench_returns_m_xts <- raw_returns_m_xts
      bench_returns_m_xts@data <- bench_returns_m_xts@data[, "selected_bench_return"]
      raw_returns_m_xts@data <- raw_returns_m_xts@data[, -which(colnames(raw_returns_m_xts@data) == "selected_bench_return")]
    } else {
      bench_returns_m_xts <- NULL
    }

    plot(raw_returns_m_xts, benchmark_returns_m_xts = bench_returns_m_xts, plot_perf_metric = plot_perf_metric)

  }

  #Plot 6
  if (plot_name == "Cross-Sectional Net Performance Metric Plot"){

    net_returns_m_xts <- port_returns_m_xts_list$net_returns_m_xts

    plot_perf_metric <- TRUE
    if (!is.null(x@backtest_workflow_common$selected_benchmark)){
      bench_returns_m_xts <- net_returns_m_xts
      bench_returns_m_xts@data <- bench_returns_m_xts@data[, "selected_bench_return"]
      net_returns_m_xts@data <- net_returns_m_xts@data[, -which(colnames(net_returns_m_xts@data) == "selected_bench_return")]
    } else {
      bench_returns_m_xts <- NULL
    }

    plot(net_returns_m_xts, benchmark_returns_m_xts = bench_returns_m_xts, plot_perf_metric = plot_perf_metric)

  }

  #Plot 7
  if (plot_name == "Cross-Sectional Raw Active Performance Metric Plot"){

    if (is.null(x@backtest_workflow_common$selected_benchmark)){
      stop("This plot is only applicable to cohorts with benchmarks")
    }

    active_returns <- port_returns_m_xts_list$raw_active_returns_m_xts
    plot_perf_metric <- TRUE

    plot(active_returns, plot_perf_metric = plot_perf_metric)

  }

  #Plot 8
  if (plot_name == "Cross-Sectional Net Active Performance Metric Plot"){

    if (is.null(x@backtest_workflow_common$selected_benchmark)){
      stop("This plot is only applicable to cohorts with benchmarks")
    }

    active_returns <- port_returns_m_xts_list$net_active_returns_m_xts
    plot_perf_metric <- TRUE

    plot(active_returns, plot_perf_metric = plot_perf_metric)

  }

  #Plot 9
  if (plot_name == "Time-Series of Direct Costs"){

    direct_cost_m_xts <- port_costs_m_xts_list$direct_cost_m_xts
    plot(direct_cost_m_xts, vertical_lines = vertical_lines)

  }

  #Plot 10
  if (plot_name == "Time-Series of Market Impact Costs"){

    market_impact_cost_m_xts <- port_costs_m_xts_list$market_impact_cost_m_xts
    plot(market_impact_cost_m_xts,  vertical_lines = vertical_lines)

  }

  #Plot 11
  if (plot_name == "Time-Series of Total Costs"){

    total_cost_m_xts <- port_costs_m_xts_list$total_cost_m_xts
    plot(total_cost_m_xts, vertical_lines = vertical_lines)

  }

  #Plot 12
  if (plot_name == "Time-Series of Turnover"){

    turnover_m_xts <- port_costs_m_xts_list$turnover_m_xts
    plot(turnover_m_xts, vertical_lines = vertical_lines)

  }
  #Plot 13
  if (plot_name == "Time-Series of Port Metrics"){

    #Promp the user regarding the metrics to plot
    available_metrics <- names(port_metrics_m_xts_list) %>% stringr::str_remove("_m_xts")

    selected_metric <- readline(prompt = paste("Please select the metric to plot from the following list: ",
                                               paste(available_metrics, collapse = ", "), ": "))

    if (!selected_metric %in% available_metrics){
      stop("Invalid metric selected")
    }

    metric_m_xts <- port_metrics_m_xts_list[[paste0(selected_metric, "_m_xts")]]

    plot(metric_m_xts,  vertical_lines = vertical_lines)

  }

  #Plot 14
  if (plot_name == "Weights Correlogram"){

    plot(port_weights_m_df, type = "correlogram")

  }

  #Plot 15
  if (plot_name == "Weights Radar"){

    port_weights_m_df@data <- port_weights_m_df@data %>% dplyr::select(-dplyr::any_of(c("bench_weights")))
    plot(port_weights_m_df, type = "radar")

  }











})


