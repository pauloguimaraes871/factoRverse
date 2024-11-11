#' Plot Method for Meta Dataframe with Custom Statistics and Filtering
#'
#' This method generates enhanced plots for a `meta_dataframe` object based on the specified plot type and chosen calculation statistic.
#'
#' @param x A `meta_dataframe` object containing the data to be plotted.
#' @param type A character string specifying the type of plot. Options are "cross_sectional" and "time_series".
#' @param clustering_variables Character vector. Variables used for grouping in cross-sectional or time series plot.
#' @param variable Character. The numeric variable for calculating the specified statistic in the plot.
#' @param tickers Character or vector of characters. Specific tickers to include; defaults to "all" for no filtering.
#' @param dates Date, single date, or date range. Specific date range to include; defaults to "all" for no filtering.
#' @param calc_stat Character. Specifies the statistic to calculate. Supported values: "mean", "sd", "median", "min", "max", "sum", "n",
#'   "q05", "q10", "q25", "q75", "q90", "q95", "cor", "beta", "beta_tstat", "alpha", "alpha_tstat".
#' @param custom_filter Character or vector of characters. Additional columns to filter by, present in `data`.
#' @param filter_values List or vector of values to filter by for `custom_filter`.
#' @param y Optional numeric variable, only required for bivariate statistics like "cor", "beta", "beta_tstat", "alpha", "alpha_tstat".
#' @return A ggplot object representing the requested plot.
#' @export
setMethod(
  "plot",
  signature(x = "meta_dataframe", y = "missing"),
  function(x, type = "cross_sectional", clustering_variables = NULL, variable = NULL, tickers = "all", dates = "all", calc_stat = "mean",
           custom_filter = NULL, filter_values = NULL, dep_y = NULL, numeric_aggregation = "decile") {

    # Prompt for 'type' if not specified
    if (is.null(type)) {
      available_types <- c("cross_sectional", "time_series", "distribution", "composition", "regression", "density2d", "correlogram", "radar")
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

    # Check for valid 'type'
    if (!type %in% c("cross_sectional", "time_series", "distribution", "composition", "regression", "density2d", "correlogram", "radar")) {
      stop("Invalid plot type. Choose from 'cross_sectional', 'time_series', 'distribution', 'composition', 'regression', 'density2d', 'correlogram', or 'radar'.")
    }

    # Prompt for 'variable' if not specified and required
    if (is.null(variable)) {
      required_for_types <- c("cross_sectional", "time_series", "distribution", "composition", "regression", "density2d", "correlogram", "radar")
      if (type %in% required_for_types) {
        available_variables <- setdiff(names(x@data), c("id", "tickers", "dates"))
        cat("\nPlease choose a variable:\n")
        for (i in seq_along(available_variables)) {
          cat(paste0(i, ": ", available_variables[i], "\n"))
        }
        selection <- readline(prompt = "Enter the number of your choice: ")
        selection <- as.numeric(selection)
        if (is.na(selection) || selection < 1 || selection > length(available_variables)) {
          stop("Invalid selection.")
        }
        variable <- available_variables[selection]
      }
    }

    # Prompt for 'calc_stat' if not specified
    if (is.null(calc_stat)) {
      available_stats <- c("mean", "sd", "median", "min", "max", "sum", "n",
                           "q05", "q10", "q25", "q75", "q90", "q95",
                           "cor", "beta", "beta_tstat", "alpha", "alpha_tstat")
      cat("\nPlease choose a calculation statistic (calc_stat):\n")
      for (i in seq_along(available_stats)) {
        cat(paste0(i, ": ", available_stats[i], "\n"))
      }
      selection <- readline(prompt = "Enter the number of your choice: ")
      selection <- as.numeric(selection)
      if (is.na(selection) || selection < 1 || selection > length(available_stats)) {
        stop("Invalid selection.")
      }
      calc_stat <- available_stats[selection]
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


    #Check for dep_y adequacy
    if((type == "composition" || type == "distribution" || type == "density2d") && !is.null(dep_y)){
      stop("dep_y is not supported for this plot type.")
    }

    # Define colors for plotting
    deep_navy <- "#000033"
    black <- "#000000"
    white <- "#FFFFFF"
    vibrant_purple <- "#6A0DAD"
    neon_green <- "#39FF14"
    neon_orange <- "#FF5F1F"
    blue_bg <- "#001f3f"

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
                    cor(x, dep_y, use = "complete.obs")
                  },
                  beta = function(x, dep_y) {
                    if (missing(dep_y)) stop("dep_y is required for beta calculation")
                    lm(dep_y ~ x)$coefficients[2]
                  },
                  beta_tstat = function(x, dep_y) {
                    if (missing(dep_y)) stop("dep_y is required for beta t-stat calculation")
                    summary(lm(dep_y ~ x))$coefficients[2, 3]
                  },
                  alpha = function(x, dep_y) {
                    if (missing(dep_y)) stop("dep_y is required for alpha calculation")
                    lm(y ~ x)$coefficients[1]
                  },
                  alpha_tstat = function(x, dep_y) {
                    if (missing(dep_y)) stop("dep_y is required for alpha t-stat calculation")
                    summary(lm(dep_y ~ x))$coefficients[1, 3]
                  },
                  stop("Invalid function")
    )

    # Filter based on `tickers`
    if (!identical(tickers, "all")) {
      df <- df %>% dplyr::filter(tickers %in% !!tickers)
      tickers_text <- if (length(tickers) > 10) "> 10 tickers" else paste("Tickers:", paste(tickers, collapse = ", "))
    } else {
      tickers_text <- "All tickers"
    }

    # Filter based on `dates`
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
        stop("`custom_filter` and `filter_values` must be compatible: `custom_filter` should be a vector of column names, and `filter_values` a list or vector.")
      }
    }

    # Stop if no data after filtering
    if (nrow(df) == 0) stop("No data available for the specified filters.")

    # Identify numeric clustering variables and create decile-based factors if needed
    numeric_clustering_variables <- purrr::keep(clustering_variables, ~ is.numeric(df[[.x]]))
    if (length(numeric_clustering_variables) > 0) {

      # Set number of bins and labels based on chosen aggregation
      bins <- switch(numeric_aggregation,
                     decile = 10,
                     quartile = 4,
                     tercile = 3,
                     median = 2,
                     stop("Invalid numeric_aggregation. Choose from 'decile', 'quartile', 'tercile', or 'median'."))

      # Mutate to create factor-based categorization with proper labels
      df <- df %>%
        dplyr::mutate(dplyr::across(
          dplyr::all_of(numeric_clustering_variables),
          ~ {
            labels <- switch(numeric_aggregation,
                             decile = sprintf("d%02d_%s", 1:10, dplyr::cur_column()),
                             quartile = sprintf("q%02d_%s", 1:4, dplyr::cur_column()),
                             tercile = sprintf("t%02d_%s", 1:3, dplyr::cur_column()),
                             median = c(paste0("below_median_", dplyr::cur_column()), paste0("above_median_", dplyr::cur_column())),
                             NULL)
            factor(dplyr::ntile(., bins), labels = labels)
          },
          .names = "{numeric_aggregation}_{col}"
        ))

      # Update clustering variables to include the new categorized columns
      clustering_variables <- purrr::map_chr(clustering_variables, ~ if (.x %in% numeric_clustering_variables) {
        paste0(numeric_aggregation, "_", .x)
      } else .x
      )
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
        color_palette <- RColorBrewer::brewer.pal(min(num_series, 12), "Set3")  # Adjust palette size to number of series

        # If more than 12 series, extend the palette by repeating colors (or use a larger palette if available)
        if (num_series > 12) {
          color_palette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))(num_series)
        }

        # Pivot df_wide to long format for easier plotting with legend
        df_long <- df_wide %>%
          tidyr::pivot_longer(cols = -dates, names_to = "Group", values_to = "Calc_Stat")

        # Plot each time series with a unique color and include legend
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
            title = paste("Time Series of", calc_stat, variable, "by", paste(setdiff(clustering_variables, "dates"), collapse = ", ")),
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

    # Distribution Plot
    if (type == "distribution") {
      if (is.null(variable)) stop("Please specify the 'variable' argument for distribution plot.")

      # Filter out rows with NA in the specified variable
      df <- dplyr::filter(df, !is.na(!!rlang::sym(variable)))

      # Check if clustering_variables is empty or NULL, in which case we create a single plot
      if (is.null(clustering_variables) || length(clustering_variables) == 0) {
        # Single distribution plot with classic histogram
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
            title = paste("Distribution of", variable),
            subtitle = paste(date_range_text, ifelse(tickers_text != "", paste("|", tickers_text), "")),
            x = variable,
            y = "Frequency"
          )
        print(p)
      } else {

        # Generate color palette based on the number of unique categories
        num_categories <- length(unique(df[[clustering_variables]]))
        base_palette <- RColorBrewer::brewer.pal(min(num_categories, 12), "Set3")
        color_palette <- if (num_categories > 12) {
          grDevices::colorRampPalette(base_palette)(num_categories)
        } else {
          base_palette
        }

        # Distribution plot faceted by clustering variables with classic histogram
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
            title = paste("Distribution of", variable, "by", paste(clustering_variables, collapse = ", ")),
            subtitle = paste(date_range_text, ifelse(tickers_text != "", paste("|", tickers_text), "")),
            x = variable,
            y = "Frequency"
          )
        print(p)
      }
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
      # Ensure that `y` is provided
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
            y = dep_y
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
            ggplot2::facet_wrap(~ Cluster, scales = "free", labeller = ggplot2::labeller(Cluster = setNames(unique_clusters, unique_clusters)))  # Custom labels for each cluster

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

      # Prepare the data for the radar plot
      df_radar <- df %>%
        dplyr::select(!!rlang::sym(clustering_variables), !!!rlang::syms(variable)) %>%
        dplyr::group_by(!!rlang::sym(clustering_variables)) %>%
        dplyr::summarize(across(where(is.numeric), ~ mean(., na.rm = TRUE)), .groups = "drop")  # Calculate mean, ignoring NAs

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
        dplyr::mutate(across(where(is.numeric), ~ ( . - min(., na.rm = TRUE)) / ( max(., na.rm = TRUE) - min(., na.rm = TRUE))))  # Normalization

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
      par(bg = "#001f3f")  # Set background color to blue_bg

      # Create the radar plot using fmsb
      fmsb::radarchart(df_radar_chart, axistype = 1,
                       pcol = colors, pfcol = scales::alpha(colors, 0.5), plwd = 2, plty = 1,
                       axislabcol = "white"
                        # Set the main title color to white

      )

      # Title
      mtext(side = 3, line = 2.5, adj = 0, cex = 1.25,  # Align to the left
            paste("Radar Plot of", paste(variable, collapse = ", "), "by", clustering_variables),
            font = 2, col = "white")

      # Subtitle
      mtext(side = 3, line = 1, adj = 0, cex = 0.75,  # Align to the left
            paste(date_range_text, ifelse(tickers_text != "", paste("|", tickers_text), "")),
            font = 2, col = "white")


      # Add legend with white text color at the bottom
      legend("bottomleft", legend = df_radar[[clustering_variables]], col = colors, pch = 16, bty = "n", text.col = "white")




    }


  }
)



################################

#' @title Plot Method for `grid_search_strategy`
#' @description Plot the values selected for each hyperparameter in `hyper_grid_domain` for grid search strategy.
#' @param x An object of class `grid_search_strategy`.
#' @param y Unused. Included for consistency with the generic `plot` method.
#' @return A `ggplot` object visualizing the hyperparameter grid and possible limits.
#' @export
setMethod("plot", signature(x = "grid_search_strategy", y = "missing"), function(x, y) {

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
    "num.trees" = "/10²",
    "max.depth" = "/10",
    "min.bucket" = "/10",
    "min_child_weight" = "/10",
    "gamma" = "/10",
    "nrounds" = "/10²",
    "regularizer_l1" = "/10",
    "regularizer_l2" = "/10",
    "size_of_batch" = "(log₂)",
    "number_of_epochs" = "/10²"
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
#' @description Generate a sample for each hyperparameter and plot the distribution for random search.
#' @param x An object of class `random_search_strategy`.
#' @param y Unused. Included for consistency with the generic `plot` method.
#' @return A `ggplot` object visualizing the hyperparameter distributions with possible limits.
#' @export
setMethod("plot", signature(x = "random_search_strategy", y = "missing"), function(x, y) {

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

  # Extract the hyperparameters and distributions
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
    "num.trees" = "/10²",
    "max.depth" = "/10",
    "min.bucket" = "/10",
    "min_child_weight" = "/10",
    "gamma" = "/10",
    "nrounds" = "/10²",
    "regularizer_l1" = "/10",
    "regularizer_l2" = "/10",
    "size_of_batch" = "(log₂)",
    "number_of_epochs" = "/10²"
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
    Distribution = character()
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
      samples <- runif(n_iter, min = pars["min"], max = pars["max"])
    } else if (dist_choice == "normal") {
      pars <- transformed_hyper_list[[hp_name]]$pars
      samples <- rnorm(n_iter, mean = pars["mean"], sd = pars["sd"])
    } else if (dist_choice == "lognormal") {
      pars <- transformed_hyper_list[[hp_name]]$pars
      samples <- rlnorm(n_iter, meanlog = pars["meanlog"], sdlog = pars["sdlog"])
    } else if (dist_choice == "constant") {
      samples <- rep(transformed_hyper_list[[hp_name]]$value, n_iter)  # Constant value
    } else {
      stop(paste("Unsupported distribution:", dist_choice))
    }

    # Add the samples to the plot_data
    plot_data <- rbind(plot_data, data.frame(
      Hyperparameter = rep(hp_name, length(samples)),
      Value = samples,
      Distribution = rep(dist_choice, length(samples))
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
    ggplot2::geom_violin(data = plot_data[plot_data$Distribution != "constant", ], alpha = 0.7, color = neon_cyan) +  # Violin for non-constant distributions
    ggplot2::geom_point(data = plot_data[plot_data$Distribution == "constant", ], size = 3, color = neon_purple, shape = 4) +  # Points for constant in white
    ggplot2::geom_point(data = predefined_data, ggplot2::aes(x = MinAllowed, y = Hyperparameter), shape = 124, size = 5, color = neon_green) +  # Min always closed
    ggplot2::geom_point(data = predefined_data, ggplot2::aes(x = MaxAllowed, y = Hyperparameter), shape = 124, size = 5, color = neon_green) +  # Max always closed
    ggplot2::labs(title = "Random Search: Hyperparameter Distributions",
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
#' @return A `ggplot` object visualizing the bounds.
#' @export
setMethod("plot", signature(x = "bayesian_opt_strategy", y = "missing"), function(x, y, ...) {


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
    "num.trees" = "/10²",
    "max.depth" = "/10",
    "min.bucket" = "/10",
    "min_child_weight" = "/10",
    "gamma" = "/10",
    "nrounds" = "/10²",
    "regularizer_l1" = "/10",
    "regularizer_l2" = "/10",
    "size_of_batch" = "(log₂)",
    "number_of_epochs" = "/10²"
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




#' @title Plot Method for `ml_backtest_config`
#' @description Calls the appropriate plot method for `tuning_strategy`.
#' @param x An object of class `ml_backtest_config`.
#' @param y Unused. Included for consistency with the generic `plot` method.
#' @return A `ggplot` object visualizing the hyperparameter distributions with possible limits.
#' @export
setMethod("plot", signature(x = "ml_backtest_config", y = "missing"), function(x, y){

  if(x@ml_algorithm != "ols"){
    plot(x@tuning_strategy)
  } else {
    message("Plot method not avaiable for `ols` ml_algorithm.")
  }

})


#' @title Plot Method for `ml_metabacktest_config`
#' @description Allows the user to plot either the base learners or the meta learner configurations.
#' If `base_ml_backtest_results` is provided, it extracts the configurations using `get_ml_backtest_config`.
#' @param x An object of class `ml_metabacktest_config`.
#' @param y Unused. Included for consistency with the generic `plot` method.
#' @param which A character string specifying which configurations to plot.
#'   - `"base"`: Plots the base learners (default).
#'   - `"meta"`: Plots the meta learner.
#'   - `"both"`: Plots both base learners and meta learner.
#'   If not specified, the function will prompt the user to choose.
#' @param ... Additional arguments (currently unused).
#' @return A combined `ggplot` object visualizing the hyperparameter distributions for the selected configurations.
#' @export
setMethod("plot", signature(x = "ml_metabacktest_config", y = "missing"), function(x, y, which = NULL, ...) {

  # If 'which' is not provided, prompt the user
  if (is.null(which)) {
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
  }

  # Initialize a list to hold plots
  plot_list <- list()

  # Plot base learners if requested
  if (which %in% c("base", "both")) {
    # Determine whether to use configs or results
    if (!is.null(x@base_ml_backtest_configs)) {
      base_configs <- x@base_ml_backtest_configs
    } else if (!is.null(x@base_ml_backtest_results)) {
      # Use get_ml_backtest_config to extract configs from results
      base_configs <- lapply(x@base_ml_backtest_results, get_ml_backtest_config)
    } else {
      stop("No base_ml_backtest_configs or base_ml_backtest_results found in the object.")
    }

    # Loop through each ml_backtest_config in the base_configs
    for (config in base_configs) {
      # Check if the algorithm is not OLS
      if (config@ml_algorithm != "ols") {
        # Create the plot using the existing plot method for ml_backtest_config
        p <- plot(config)  # Call the existing plot method

        # Store the plot in the list and add a centered subtitle
        plot_list[[length(plot_list) + 1]] <- p +
          ggplot2::ggtitle(paste("Algorithm:", config@ml_algorithm,
                                 if(config@ml_algorithm == "nn") paste("with", length(config@keras_architecture_parameters@units), "layers"),
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
        message("Skipping plotting for `ols` ml_algorithm in one of the configs.")
      }
    }
  }

  # Plot meta learner if requested
  if (which %in% c("meta", "both")) {
    meta_config <- x@meta_ml_backtest_config
    # Check if the algorithm is not OLS
    if (meta_config@ml_algorithm != "ols") {
      # Create the plot using the existing plot method for ml_backtest_config
      p <- plot(meta_config)  # Call the existing plot method

      # Store the plot in the list and add a centered subtitle
      plot_list[[length(plot_list) + 1]] <- p +
        ggplot2::ggtitle(paste("Meta Learner - Algorithm:", meta_config@ml_algorithm,
                               if(meta_config@ml_algorithm == "nn") paste("with", length(meta_config@keras_architecture_parameters@units), "layers"),
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
      message("Skipping plotting for `ols` ml_algorithm in the meta learner config.")
    }
  }

  # Check if there are any plots to display
  if (length(plot_list) > 0) {
    # Create an empty plot for the main title with the blue background
    title_text <- switch(which,
                         "base" = "ML Metabacktest Base Learner Tuning Strategies",
                         "meta" = "ML Metabacktest Meta Learner Tuning Strategy",
                         "both" = "ML Metabacktest Tuning Strategies"
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
  } else {
    stop("No valid plots available to display.")
  }
})





# Define the plot method for ml_backtest_results
################################
#' Plot Machine Learning Walk-Forward Validation Results
#'
#' This method generates various plots to visualize the performance of machine learning models using walk-forward validation metrics.
#' Users can select which plot to display by specifying the `plot_id` parameter,
#' either by name or by number.
#'
#' @param x An object of class \code{ml_backtest_results} containing the results of the walk-forward validation.
#' @param plot_id A character string or numeric value specifying which plot to display.
#'   - By name: Options are:
#'     - `"Chosen Evaluation Metric Over Time"`
#'     - `"Test vs Validation Chosen Evaluation Metric Over Time"`
#'     - `"Best Hyperparameters Over Time"`
#'     - `"Hyperparameters vs Error"`
#'     - `"All Evaluation Metrics Over Time"`
#'     - `"Consolidated OOS Testing Metrics"`
#'     - `"Average Validation Metrics"`
#'   - By number: Provide a number corresponding to the plot (as listed when `plot_id` is `NULL`).
#'   If `NULL` (default), the method lists available plots.
#'
#' @return Invisibly returns the input \code{x}.
#' @export
setMethod("plot", "ml_backtest_results", function(x, plot_id = NULL) {

  # List of available plots
  available_plots <- c(
    "Chosen Evaluation Metric Over Time",
    "Test vs Validation Chosen Evaluation Metric Over Time",
    "Best Hyperparameters Over Time",
    "Hyperparameters vs Error",
    "All Evaluation Metrics Over Time",
    "Average Validation vs Consolidated OOS Testing Metrics"
  )

  if(x@ml_backtest_workflow$ml_algorithm %in% c("ols", "ew_ensemble", "optimal_ensemble")){
    plot_id <- 5
    message("'All Evaluation Metrics Over Time' is the only available plot for OLS, EW Ensemble, and Optimal Ensemble. Plotting 'All Evaluation Metrics Over Time'...")
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
  oos_testing_eval_metrics <- x@oos_testing_eval_metrics
  validation_eval_metrics_hyper_choice <- x@validation_eval_metrics_hyper_choice
  hyper_choice_df <- x@best_hyperparameters
  chosen_eval_metric <- x@ml_backtest_workflow$chosen_eval_metric
  chosen_eval_metric_validation <- x@chosen_eval_metric_validation
  ml_algorithm <- x@ml_backtest_workflow$ml_algorithm
  rebalance_dates <- x@ml_backtest_workflow$rebalance_dates

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

  # Remove 'consolidated' row from oos_testing_eval_metrics before time series plots
  if ("consolidated" %in% rownames(oos_testing_eval_metrics)) {
    consolidated_oos_testing_metrics <- oos_testing_eval_metrics["consolidated", , drop = FALSE]
    oos_testing_eval_metrics <- oos_testing_eval_metrics[rownames(oos_testing_eval_metrics) != "consolidated", , drop = FALSE]
  } else {
    consolidated_oos_testing_metrics <- NULL
  }

  # Remove 'average' row from validation_eval_metrics_hyper_choice before time series plots
  if ("average" %in% rownames(validation_eval_metrics_hyper_choice)) {
    average_validation_metrics <- validation_eval_metrics_hyper_choice["average", , drop = FALSE]
    validation_eval_metrics_hyper_choice <- validation_eval_metrics_hyper_choice[rownames(validation_eval_metrics_hyper_choice) != "average", , drop = FALSE]
  } else {
    average_validation_metrics <- NULL
  }

  # Prepare data for plotting

  # Some treatments to oos_testing_eval_metrics
  # Change colnames
  colnames(oos_testing_eval_metrics) <- paste("oos_testing_", colnames(oos_testing_eval_metrics), sep = "")
  # Add dates column
  oos_testing_eval_metrics <- oos_testing_eval_metrics %>% dplyr::mutate(dates = rownames(oos_testing_eval_metrics))
  oos_testing_eval_metrics$dates <- as.Date(oos_testing_eval_metrics$dates, format = "%Y-%m-%d") # Coerce to dates

  if (!ml_algorithm %in% c("ols", "ew_ensemble", "optimal_ensemble")) {
    # Some treatments to validation_eval_metrics_hyper_choice
    # Change colnames
    colnames(validation_eval_metrics_hyper_choice) <- paste("validation_", colnames(validation_eval_metrics_hyper_choice), sep = "")
    # Add dates column
    validation_eval_metrics_hyper_choice <- validation_eval_metrics_hyper_choice %>% dplyr::mutate(dates = rownames(validation_eval_metrics_hyper_choice))
    validation_eval_metrics_hyper_choice$dates <- as.Date(validation_eval_metrics_hyper_choice$dates, format = "%Y-%m-%d") # Coerce to dates

    # Join test and validation
    oos_testing_and_validation <- dplyr::left_join(oos_testing_eval_metrics, validation_eval_metrics_hyper_choice, by = 'dates')

    # Melt
    oos_testing_and_validation <- oos_testing_and_validation %>% reshape::melt(id.vars = "dates")
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
        aes(yintercept = overall_mean),
        color = neon_pink,
        linetype = "dashed"
      ) +
      ggplot2::geom_hline(
        data = chosen_eval_testing_data %>%
          dplyr::select(year, overall_mean, yearly_mean) %>%
          dplyr::distinct(),
        aes(yintercept = yearly_mean),
        color = neon_purple,
        linetype = "dashed"
      ) +
      ggplot2::scale_color_manual(values = c("Metric" = neon_blue)) +
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
        reshape::melt(id.vars = "dates"),
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

    print(plots_list$best_hyper_over_time)

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
        chosen_eval_metric_validation_df$eta,
        chosen_eval_metric_validation_df$gamma,
        sep = "_"
      )
    } else if (ml_algorithm == "nn") {
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
    if (ml_algorithm == "glmnet") {
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
    if (ml_algorithm == "rf") {
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
    if (ml_algorithm == "xgb") {
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
    if (ml_algorithm == "nn") {
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
        reshape::melt(id.vars = "dates") %>%
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
      reshape::melt(id.vars = "dates") %>%
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

  } else if (plot_name == "Average Validation vs Consolidated OOS Testing Metrics") {
    # PLOT 6: Compare Average Validation Metrics with Consolidated OOS Testing Metrics

    # Check if both metrics are available
    if (is.null(consolidated_oos_testing_metrics) || is.null(average_validation_metrics)) {
      cat("Both consolidated OOS testing metrics and average validation metrics are required for this plot.\n")
    } else {
      # Add an id column to prevent the melt warning
      consolidated_oos_testing_metrics$id <- "Consolidated"
      average_validation_metrics$id <- "Average"

      # Melt both data frames to long format with specified id.vars
      consolidated_data <- reshape2::melt(
        consolidated_oos_testing_metrics,
        id.vars = "id",
        variable.name = "Metric",
        value.name = "OOS_Testing_Value"
      )
      consolidated_data$Metric <- gsub("^oos_testing_", "", consolidated_data$Metric)

      average_data <- reshape2::melt(
        average_validation_metrics,
        id.vars = "id",
        variable.name = "Metric",
        value.name = "Average_Validation_Value"
      )
      average_data$Metric <- gsub("^validation_", "", average_data$Metric)

      # Merge the two data frames on Metric
      combined_data <- merge(
        consolidated_data,
        average_data,
        by = "Metric",
        all = TRUE
      )

      # Reshape to long format for plotting
      plot_data <- combined_data %>%
        tidyr::pivot_longer(
          cols = c("OOS_Testing_Value", "Average_Validation_Value"),
          names_to = "Type",
          values_to = "Value"
        )

      # Replace Type names for clarity
      plot_data$Type <- dplyr::case_when(
        plot_data$Type == "OOS_Testing_Value" ~ "OOS Testing",
        plot_data$Type == "Average_Validation_Value" ~ "Average Validation",
        TRUE ~ plot_data$Type
      )

      # Create the combined plot
      plots_list$comparison_avg_val_oos <- ggplot2::ggplot(
        plot_data,
        ggplot2::aes(x = Metric, y = Value, fill = Type)
      ) +
        ggplot2::geom_bar(
          stat = "identity",
          position = ggplot2::position_dodge(width = 0.8),
          alpha = 0.8
        ) +
        ggplot2::labs(
          title = "Average Validation vs Consolidated OOS Testing Metrics",
          x = "Metric",
          y = "Metric Value",
          fill = "Metric Type"
        ) +
        ggplot2::scale_fill_manual(
          values = extended_neon_palette[1:2]  # Assuming two types
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
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
          legend.position = "bottom",  # Place legend below the plot
          panel.grid.major = ggplot2::element_line(
            color = faint_blue,
            size = 0.2
          ),
          panel.grid.minor = ggplot2::element_line(
            color = faint_blue,
            size = 0.1
          )
        )

      print(plots_list$comparison_avg_val_oos)
    }
  }


  invisible(x)
})



#' @title Plot Method for ml_metabacktest_results Class
#' @description Generates various plots to visualize the performance and metrics of meta-learning backtest results.
#' Users can select which plot to display by specifying the `plot_id` parameter,
#' either by name or by number.
#' The plots include comparisons between base learners and meta learners over time.
#'
#' @param x An object of class \code{ml_metabacktest_results} containing the results of the meta-learning backtests.
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
setMethod("plot", "ml_metabacktest_results", function(x, plot_id = NULL) {

  # List of available plots
  available_plots <- c(
    "Consolidated OOS Testing Metrics Comparison",
    "Time Series OOS Testing Metrics",
    "Mean Validation Metrics Comparison",
    "Time Series Validation Metrics",
    "Base Learners vs Meta Learners Over Time"
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

  # Extract data from the ml_metabacktest_results object
  consolidated_metrics <- x@consolidated_oos_testing_metrics
  time_series_oos_testing_metrics <- x@time_series_oos_testing_metrics
  mean_validation_metrics <- x@mean_validation_metrics
  time_series_validation_metrics <- x@time_series_validation_metrics
  base_learners <- x@base_ml_backtest_results_list
  meta_learners <- x@meta_ml_backtest_results_list

  # Initialize plots list
  plots_list <- list()
  # Now generate the selected plot
  if (plot_name == "Consolidated OOS Testing Metrics Comparison") {
    # Plot consolidated OOS testing metrics for all models (base and meta learners)

    # Prepare data
    # Extract full periods and common dates metrics
    full_periods_metrics <- consolidated_metrics$full_periods_oos_testing_metrics
    common_dates_metrics <- consolidated_metrics$common_dates_oos_testing_metrics

    # Combine the two data frames
    consolidated_data <- rbind(
      cbind(full_periods_metrics, Period = "Full Periods"),
      cbind(common_dates_metrics, Period = "Common Dates")
    )

    # Replace long Backtest identifiers with labels
    all_backtests <- unique(consolidated_data$Backtest)
    labels <- seq_along(all_backtests)
    legend <- data.frame(
      Backtest = all_backtests,
      Label = labels
    )
    consolidated_data$BacktestLabel <- legend$Label[match(consolidated_data$Backtest, legend$Backtest)]

    # Melt data for plotting
    plot_data <- reshape2::melt(
      consolidated_data,
      id.vars = c("Backtest", "BacktestLabel", "Period", "testing_dates_range", "chosen_eval_metric"),
      variable.name = "Metric",
      value.name = "Value"
    )

    # Exclude non-numeric columns
    plot_data <- plot_data[!plot_data$Metric %in% c("Backtest", "BacktestLabel", "Period", "testing_dates_range", "chosen_eval_metric"), ]

    # Create the plot
    plots_list$consolidated_oos_testing_metrics_comparison <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = as.factor(BacktestLabel), y = Value, fill = Metric)
    ) +
      ggplot2::geom_bar(stat = "identity", position = "dodge") +
      ggplot2::facet_wrap(~Period, scales = "free_y") +
      ggplot2::labs(
        title = "Consolidated OOS Testing Metrics Comparison",
        x = "Model (Backtest Label)",
        y = "Metric Value",
        fill = "Metric"
      ) +
      ggplot2::scale_fill_manual(values = extended_neon_palette) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = blue_bg, color = NA),
        panel.background = ggplot2::element_rect(fill = blue_bg, color = NA),
        plot.title = ggplot2::element_text(color = white, size = 16, face = "bold"),
        axis.text = ggplot2::element_text(color = white),
        axis.title = ggplot2::element_text(color = white),
        strip.text = ggplot2::element_text(color = white, face = "bold"),
        legend.title = ggplot2::element_text(color = white),
        legend.text = ggplot2::element_text(color = white),
        panel.grid.major = ggplot2::element_line(color = faint_blue, size = 0.2),
        panel.grid.minor = ggplot2::element_line(color = faint_blue, size = 0.1)
      )

    # Print the legend mapping Backtest labels to identifiers
    cat("\nLegend:\n")
    for (i in seq_along(labels)) {
      cat(paste(labels[i], ":", all_backtests[i], "\n"))
    }

    print(plots_list$consolidated_oos_testing_metrics_comparison)

  } else if (plot_name == "Time Series OOS Testing Metrics") {
    # Plot time series OOS testing metrics for each model

    # Prepare data
    # time_series_oos_testing_metrics is a list of data frames for each metric
    # We'll combine them into one data frame for plotting

    metrics_list <- time_series_oos_testing_metrics
    metric_names <- names(metrics_list)

    plot_data <- data.frame()

    for (metric_name in metric_names) {
      metric_df <- metrics_list[[metric_name]]
      metric_long <- reshape2::melt(
        as.data.frame(metric_df),
        id.vars = NULL,
        variable.name = "Backtest",
        value.name = "Value"
      )
      metric_long$Date <- as.Date(rownames(metric_df))
      metric_long$Metric <- metric_name
      plot_data <- rbind(plot_data, metric_long)
    }

    # Replace long Backtest identifiers with labels
    all_backtests <- unique(plot_data$Backtest)
    labels <- seq_along(all_backtests)
    legend <- data.frame(
      Backtest = all_backtests,
      Label = labels
    )
    plot_data$BacktestLabel <- legend$Label[match(plot_data$Backtest, legend$Backtest)]

    # Create the plot
    plots_list$time_series_oos_testing_metrics <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = Date, y = Value, color = as.factor(BacktestLabel))
    ) +
      ggplot2::geom_line() +
      ggplot2::facet_wrap(~Metric, scales = "free_y") +
      ggplot2::labs(
        title = "Time Series OOS Testing Metrics",
        x = "Date",
        y = "Metric Value",
        color = "Model (Backtest Label)"
      ) +
      ggplot2::scale_color_manual(values = extended_neon_palette) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = blue_bg, color = NA),
        panel.background = ggplot2::element_rect(fill = blue_bg, color = NA),
        plot.title = ggplot2::element_text(color = white, size = 16, face = "bold"),
        axis.text = ggplot2::element_text(color = white),
        axis.title = ggplot2::element_text(color = white),
        strip.text = ggplot2::element_text(color = white, face = "bold"),
        legend.title = ggplot2::element_text(color = white),
        legend.text = ggplot2::element_text(color = white),
        panel.grid.major = ggplot2::element_line(color = faint_blue, size = 0.2),
        panel.grid.minor = ggplot2::element_line(color = faint_blue, size = 0.1)
      )

    # Print the legend mapping Backtest labels to identifiers
    cat("\nLegend:\n")
    for (i in seq_along(labels)) {
      cat(paste(labels[i], ":", all_backtests[i], "\n"))
    }

    print(plots_list$time_series_oos_testing_metrics)

  } else if (plot_name == "Mean Validation Metrics Comparison") {
    # Plot mean validation metrics for each model

    # Prepare data
    data_df <- x@mean_validation_metrics

    # Replace long Backtest identifiers with labels
    if ("Backtest" %in% names(data_df)) {
      all_backtests <- unique(data_df$Backtest)
      labels <- seq_along(all_backtests)
      legend <- data.frame(
        Backtest = all_backtests,
        Label = labels
      )
      data_df$BacktestLabel <- legend$Label[match(data_df$Backtest, legend$Backtest)]
    } else {
      legend <- NULL
    }

    # Melt data for plotting
    plot_data <- reshape2::melt(
      data_df,
      id.vars = c("Backtest", "BacktestLabel", "chosen_eval_metric"),
      variable.name = "Metric",
      value.name = "Value"
    )

    # Exclude non-numeric columns
    plot_data <- plot_data[!plot_data$Metric %in% c("Backtest", "BacktestLabel", "chosen_eval_metric"), ]

    # Create the plot
    plots_list$mean_validation_metrics_comparison <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = as.factor(BacktestLabel), y = Value, fill = Metric)
    ) +
      ggplot2::geom_bar(stat = "identity", position = "dodge") +
      ggplot2::labs(
        title = "Mean Validation Metrics Comparison",
        x = "Model (Backtest Label)",
        y = "Metric Value",
        fill = "Metric"
      ) +
      ggplot2::scale_fill_manual(values = extended_neon_palette) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = blue_bg, color = NA),
        panel.background = ggplot2::element_rect(fill = blue_bg, color = NA),
        plot.title = ggplot2::element_text(color = white, size = 16, face = "bold"),
        axis.text = ggplot2::element_text(color = white),
        axis.title = ggplot2::element_text(color = white),
        strip.text = ggplot2::element_text(color = white, face = "bold"),
        legend.title = ggplot2::element_text(color = white),
        legend.text = ggplot2::element_text(color = white),
        panel.grid.major = ggplot2::element_line(color = faint_blue, size = 0.2),
        panel.grid.minor = ggplot2::element_line(color = faint_blue, size = 0.1)
      )

    # Print the legend mapping Backtest labels to identifiers
    if (!is.null(legend)) {
      cat("\nLegend:\n")
      for (i in seq_along(labels)) {
        cat(paste(labels[i], ":", all_backtests[i], "\n"))
      }
    }

    print(plots_list$mean_validation_metrics_comparison)

  } else if (plot_name == "Time Series Validation Metrics") {
    # Plot time series validation metrics for each model

    # Prepare data
    metrics_list <- time_series_validation_metrics
    metric_names <- names(metrics_list)

    plot_data <- data.frame()

    for (metric_name in metric_names) {
      metric_df <- metrics_list[[metric_name]]
      metric_long <- reshape2::melt(
        as.data.frame(metric_df),
        id.vars = NULL,
        variable.name = "Backtest",
        value.name = "Value"
      )
      metric_long$Date <- as.Date(rownames(metric_df))
      metric_long$Metric <- metric_name
      plot_data <- rbind(plot_data, metric_long)
    }

    # Replace long Backtest identifiers with labels
    all_backtests <- unique(plot_data$Backtest)
    labels <- seq_along(all_backtests)
    legend <- data.frame(
      Backtest = all_backtests,
      Label = labels
    )
    plot_data$BacktestLabel <- legend$Label[match(plot_data$Backtest, legend$Backtest)]

    # Create the plot
    plots_list$time_series_validation_metrics <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = Date, y = Value, color = as.factor(BacktestLabel))
    ) +
      ggplot2::geom_line() +
      ggplot2::facet_wrap(~Metric, scales = "free_y") +
      ggplot2::labs(
        title = "Time Series Validation Metrics",
        x = "Date",
        y = "Metric Value",
        color = "Model (Backtest Label)"
      ) +
      ggplot2::scale_color_manual(values = extended_neon_palette) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = blue_bg, color = NA),
        panel.background = ggplot2::element_rect(fill = blue_bg, color = NA),
        plot.title = ggplot2::element_text(color = white, size = 16, face = "bold"),
        axis.text = ggplot2::element_text(color = white),
        axis.title = ggplot2::element_text(color = white),
        strip.text = ggplot2::element_text(color = white, face = "bold"),
        legend.title = ggplot2::element_text(color = white),
        legend.text = ggplot2::element_text(color = white),
        panel.grid.major = ggplot2::element_line(color = faint_blue, size = 0.2),
        panel.grid.minor = ggplot2::element_line(color = faint_blue, size = 0.1)
      )

    # Print the legend mapping Backtest labels to identifiers
    cat("\nLegend:\n")
    for (i in seq_along(labels)) {
      cat(paste(labels[i], ":", all_backtests[i], "\n"))
    }

    print(plots_list$time_series_validation_metrics)

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
    metric_df <- time_series_oos_testing_metrics[[metric_name]]

    if (is.null(metric_df)) {
      cat("Metric", metric_name, "not found in time series OOS testing metrics.\n")
      return(invisible(x))
    }

    # Prepare the data
    plot_data <- reshape2::melt(
      as.data.frame(metric_df),
      id.vars = NULL,
      variable.name = "Backtest",
      value.name = "Value"
    )
    plot_data$Date <- as.Date(rownames(metric_df))

    # Add a column indicating whether the model is a base learner or meta learner
    base_learner_ids <- sapply(base_learners, function(bl) bl@backtest_identifier)
    meta_learner_ids <- sapply(meta_learners, function(ml) ml@backtest_identifier)
    plot_data$ModelType <- ifelse(
      plot_data$Backtest %in% base_learner_ids, "Base Learner",
      ifelse(plot_data$Backtest %in% meta_learner_ids, "Meta Learner", "Unknown")
    )

    # Replace long Backtest identifiers with labels
    all_backtests <- unique(plot_data$Backtest)
    labels <- seq_along(all_backtests)
    legend <- data.frame(
      Backtest = all_backtests,
      Label = labels
    )
    plot_data$BacktestLabel <- legend$Label[match(plot_data$Backtest, legend$Backtest)]

    # Create the plot
    plots_list$base_vs_meta_over_time <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = Date, y = Value, color = as.factor(BacktestLabel), linetype = ModelType)
    ) +
      ggplot2::geom_line() +
      ggplot2::labs(
        title = paste("Base Learners vs Meta Learners Over Time -", metric_name),
        x = "Date",
        y = metric_name,
        color = "Model (Backtest Label)",
        linetype = "Model Type"
      ) +
      ggplot2::scale_color_manual(values = extended_neon_palette) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = blue_bg, color = NA),
        panel.background = ggplot2::element_rect(fill = blue_bg, color = NA),
        plot.title = ggplot2::element_text(color = white, size = 16, face = "bold"),
        axis.text = ggplot2::element_text(color = white),
        axis.title = ggplot2::element_text(color = white),
        legend.title = ggplot2::element_text(color = white),
        legend.text = ggplot2::element_text(color = white),
        panel.grid.major = ggplot2::element_line(color = faint_blue, size = 0.2),
        panel.grid.minor = ggplot2::element_line(color = faint_blue, size = 0.1)
      )

    # Print the legend mapping Backtest labels to identifiers
    cat("\nLegend:\n")
    for (i in seq_along(labels)) {
      cat(paste(labels[i], ":", all_backtests[i], "\n"))
    }

    print(plots_list$base_vs_meta_over_time)
  }

  invisible(x)
})


