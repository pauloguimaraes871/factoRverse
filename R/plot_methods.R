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
  function(x, type = "cross_sectional", clustering_variables = NULL, variable, tickers = "all", dates = "all", calc_stat = "mean",
           custom_filter = NULL, filter_values = NULL, y = NULL, numeric_aggregation = "decile") {

    #Check for type
    if(!type %in% c("cross_sectional", "time_series", "distribution")){
      stop("Invalid plot type. Choose from 'cross_sectional', 'time_series' or distribution.")
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
                  cor = function(x, y) {
                    if (missing(y)) stop("y is required for correlation calculation")
                    cor(x, y, use = "complete.obs")
                  },
                  beta = function(x, y) {
                    if (missing(y)) stop("y is required for beta calculation")
                    lm(y ~ x)$coefficients[2]
                  },
                  beta_tstat = function(x, y) {
                    if (missing(y)) stop("y is required for beta t-stat calculation")
                    summary(lm(y ~ x))$coefficients[2, 3]
                  },
                  alpha = function(x, y) {
                    if (missing(y)) stop("y is required for alpha calculation")
                    lm(y ~ x)$coefficients[1]
                  },
                  alpha_tstat = function(x, y) {
                    if (missing(y)) stop("y is required for alpha t-stat calculation")
                    summary(lm(y ~ x))$coefficients[1, 3]
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
                             decile = sprintf("d%02d_%s", 1:10, cur_column()),
                             quartile = sprintf("q%02d_%s", 1:4, cur_column()),
                             tercile = sprintf("t%02d_%s", 1:3, cur_column()),
                             median = c(paste0("below_median_", cur_column()), paste0("above_median_", cur_column())),
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
    ggplot2::geom_point(size = 3, colour = "darkblue", shape = 4) +  # Points for actual grid values
    ggplot2::geom_point(data = predefined_data, ggplot2::aes(x = MinAllowed, y = Hyperparameter), shape = 124, size = 3, color = "black") +  # Min always closed
    ggplot2::geom_point(data = predefined_data, ggplot2::aes(x = MaxAllowed, y = Hyperparameter), shape = 124, size = 3, color = "black") +  # Max always closed
    ggplot2::labs(title = "Grid Search: Hyperparameter Grid Values",
                  y = "Hyperparameter",
                  x = "Selected Values") +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_brewer(palette = "Set3") +
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
    ggplot2::geom_violin(data = plot_data[plot_data$Distribution != "constant", ], alpha = 0.7) +  # Violin for non-constant distributions
    ggplot2::geom_point(data = plot_data[plot_data$Distribution == "constant", ], size = 3, color = "darkblue", shape = 4) +  # Points for constant
    ggplot2::geom_point(data = predefined_data, ggplot2::aes(x = MinAllowed, y = Hyperparameter), shape = 124, size = 3, color = "black") +  # Min always closed
    ggplot2::geom_point(data = predefined_data, ggplot2::aes(x = MaxAllowed, y = Hyperparameter), shape = 124, size = 3, color = "black") +  # Max always closed
    ggplot2::labs(title = "Random Search: Hyperparameter Distributions",
                  y = "Hyperparameter",
                  x = "Sampled Values") +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_brewer(palette = "Set3") +
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
    # Add segments for bounds with thinner lines
    ggplot2::geom_segment(ggplot2::aes(x = lower, xend = upper, yend = hyperparameter), color = "darkblue", size = 0.8) +
    # Add points for bounds
    ggplot2::geom_point(ggplot2::aes(x = lower), size = 3, color = "blue", shape = 16) +  # Closed for lower bounds
    ggplot2::geom_point(ggplot2::aes(x = upper), size = 3, color = "blue", shape = 16) +  # Closed for upper bounds
    # Add predefined limits (min and max)
    ggplot2::geom_point(data = plot_data[!is.na(plot_data$min_val), ], ggplot2::aes(x = min_val, y = hyperparameter), size = 3, color = "black", shape = 124) +  # Red for predefined min
    ggplot2::geom_point(data = plot_data[!is.na(plot_data$max_val), ], ggplot2::aes(x = max_val, y = hyperparameter), size = 3, color = "black", shape = 124) +  # Red for predefined max
    # Adjust plot labels
    ggplot2::labs(
      title = "Bayesian Optimization: Hyperparameter Bounds",
      x = "Value",
      y = "Hyperparameter"
    ) +
    ggplot2::theme_minimal()

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


# Define the plot method for ml_backtest_results
################################
#' Plot Machine Learning Walk-Forward Validation Results
#'
#' This method generates various plots to visualize the performance of machine learning models using walk-forward validation metrics.
#' It creates plots comparing out-of-sample (OOS) testing metrics, validation metrics, and hyperparameter performance over time.
#'
#' @param x An object of class \code{ml_backtest_results} containing the results of the walk-forward validation.
#'
#' @return The following plots:
#' \itemize{
#'   \item \code{chosen_val_metric_over_time}: Plot of the chosen evaluation metric over time for the test data, including overall and yearly means.
#'   \item \code{test_vs_val_chosen_eval_metric_over_time}: Plot comparing the chosen evaluation metric over time for both test and validation data.
#'   \item \code{best_hyper_over_time}: Plot of the best hyperparameter values over time, with separate facets for each hyperparameter.
#'   \item \code{hyper_vs_error}: Plot showing the performance of hyperparameter choices with respect to the chosen evaluation metric. The plot varies depending on the machine learning algorithm used.
#'   \item \code{all_eval_metrics_over_time}: Plot of all evaluation metrics over time, including dashed lines for variable means and vertical lines for rebalancing dates.
#' }
#'
#' @export
setMethod("plot", "ml_backtest_results", function(x) {

  # Extract relevant data from the S4 object
  oos_testing_eval_metrics <- x@oos_testing_eval_metrics
  validation_eval_metrics_hyper_choice <- x@validation_eval_metrics_hyper_choice
  hyper_choice_df <- x@best_hyperparameters
  chosen_eval_metric <- x@metadata$chosen_eval_metric
  chosen_eval_metric_validation <- x@chosen_eval_metric_validation
  ml_algorithm <- x@metadata$ml_algorithm
  rebalance_dates <- x@metadata$rebalance_dates


  #Define global variables to pass R Cmd Check
  dates <- value <- year <- overall_mean <- yearly_mean <- quantile <- concatenation <- median <- lambda.min.ratio <- alpha <- q75 <- median_chosen_eval_metric <-
    q25 <- variable_mean <- x <- y <- label <- variable <- mtry <- num.trees <- min.bucket <- max.depth <- eta <- max_depth <- colsample_bytree <-
    lr <- droprate <- regularizer_l2 <- regularizer_l2 <- NULL

  #Get object
  plots_list <- list()


  #Treatments to oos_testing and validation metrics
  #Some treatments to oos_testing_eval
  #Change colnames
  colnames(oos_testing_eval_metrics) <- paste("oos_testing_",colnames(oos_testing_eval_metrics), sep = "")
  #Add dates column
  oos_testing_eval_metrics <- oos_testing_eval_metrics %>% dplyr::mutate(dates = rownames(oos_testing_eval_metrics))
  oos_testing_eval_metrics$dates <- as.Date(oos_testing_eval_metrics$dates, format = "%Y-%m-%d") #Coerce to dates
  #Extract dates
  oos_testing_dates <- as.Date(oos_testing_eval_metrics$dates, format = "%Y-%m-%d")

  if(ml_algorithm != "ols"){
    #Some treatments to the validation_eval
    #Change colnames
    colnames(validation_eval_metrics_hyper_choice) <- paste("validation_",colnames(validation_eval_metrics_hyper_choice), sep = "")
    #Add dates column
    validation_eval_metrics_hyper_choice <- validation_eval_metrics_hyper_choice %>% dplyr::mutate(dates = rownames(validation_eval_metrics_hyper_choice))
    validation_eval_metrics_hyper_choice$dates <- as.Date(validation_eval_metrics_hyper_choice$dates, format = "%Y-%m-%d") #Coerce to dates
    #Extract dates
    validation_dates <- as.Date(validation_eval_metrics_hyper_choice$dates, format = "%Y-%m-%d")
    #Join test and validation
    oos_testing_and_validation <- dplyr::left_join(oos_testing_eval_metrics, validation_eval_metrics_hyper_choice, by = 'dates')

    #Melt
    oos_testing_and_validation <- oos_testing_and_validation %>% reshape::melt(id.vars="dates")
    oos_testing_and_validation$dates <- as.Date(oos_testing_and_validation$dates, format = "%Y-%m-%d")

    #OOS test data
    oos_testing_data <-  oos_testing_and_validation %>%
      dplyr::filter(stringr::str_detect(variable, "oos_testing_")) %>% #Filter only OOS test
      dplyr::mutate(year = lubridate::year(dates)) %>% # Using lubridate::year() to extract year
      dplyr::group_by(variable) %>% # Group by year
      dplyr::mutate(variable_mean = mean(value, na.rm = TRUE)) %>% # Take yearly mean
      dplyr::ungroup() # Ungroup

    #Chosen eval metric - test data
    chosen_eval_testing_data <- oos_testing_and_validation %>%
      dplyr::filter(variable == paste("oos_testing_", chosen_eval_metric, sep = "")) %>% #Filter only OOS test chosen eval metric
      dplyr::mutate(year = lubridate::year(dates)) %>% # Using lubridate::year() to extract year
      dplyr::mutate(overall_mean = mean(value, na.rm = TRUE)) %>% #Add overall mean
      dplyr::group_by(year) %>% #Group by year
      dplyr::mutate(yearly_mean = mean(value, na.rm = TRUE)) %>% #Take yearly mean
      dplyr::ungroup() #Ungroup

    #Validation data
    validation_data <- oos_testing_and_validation %>%
      dplyr::filter(stringr::str_detect(variable, "validation_")) %>% #Filter only validation
      dplyr::mutate(year = lubridate::year(dates)) %>% # Using lubridate::year() to extract year
      dplyr::filter(!is.na(value)) #Filter out nas

    #Chosen eval metric - validation data
    chosen_eval_validation_data <- oos_testing_and_validation %>%
      dplyr::filter(variable == paste("validation_", chosen_eval_metric, sep = "")) %>% #Filter only chosen validation metric
      dplyr::filter(!is.na(value)) #Filter out nas

    #PLOT 1 - Test chosen validation metric over time
    plots_list$chosen_val_metric_over_time <-
      ggplot2::ggplot(chosen_eval_testing_data,
                      ggplot2::aes(x = dates, y = value, color = paste(chosen_eval_metric))) +
      ggplot2::geom_line(alpha = 0.5, ggplot2::aes(group = 1)) + # Draw line
      ggplot2::geom_point() +  # Add points
      ggplot2::labs(x = "Date", y = chosen_eval_metric) + # Add labels
      ggplot2::theme_bw() + # Set minimal theme
      ggplot2::ggtitle(paste("Test", chosen_eval_metric, "over time")) +
      ggplot2::facet_wrap(~year, scales = "free") +  # Ensure free y-axis scales
      ggplot2::scale_x_date(labels = scales::date_format("%b-%y")) +  # Format x-axis labels
      ggplot2::geom_hline(ggplot2::aes(yintercept = overall_mean, color = "Overall Mean"), linetype = "dashed") + # Add dashed line for overall mean
      ggplot2::geom_hline(ggplot2::aes(yintercept = yearly_mean, color = "Yearly Mean"), linetype = "dashed") + # Add dashed line for yearly mean
      ggplot2::scale_color_manual(values = c("red", "black", "black"),
                                  breaks = c("Overall Mean", "Yearly Mean", "Metric"),
                                  labels = c("Overall Mean", "Yearly Mean", "Metric")) + # Define legend colors and labels
      ggplot2::guides(color = ggplot2::guide_legend(title = "")) + # Customize legend title
      ggplot2::theme(legend.position = "bottom") + # Move legend to bottom
      ggplot2::scale_y_continuous(limits = c(min(chosen_eval_testing_data$value), max(chosen_eval_testing_data$value))) # Set y-axis limits

      print(plots_list$chosen_val_metric_over_time)


    #PLOT 2 - Test vs Validation chosen eval metric over time
    plots_list$test_vs_val_chosen_eval_metric_over_time <-
      ggplot2::ggplot() +
      ggplot2::geom_line(data = chosen_eval_testing_data, ggplot2::aes(x = dates, y = value, color = "Test"), alpha = 0.5) +
      ggplot2::geom_point(data = chosen_eval_testing_data, ggplot2::aes(x = dates, y = value, color = "Test"), size = 2) +  # Add test data points
      ggplot2::geom_point(data = chosen_eval_validation_data, ggplot2::aes(x = dates, y = value, color = "Validation"), size = 2) +
      ggplot2::labs(x = "Date", y = chosen_eval_metric, color = "") +
      ggplot2::ggtitle(paste("Test and validation", chosen_eval_metric, "over time")) +
      ggplot2::scale_color_manual(values = c("Test" = "black", "Validation" = "blue")) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::geom_text(data = chosen_eval_validation_data, ggplot2::aes(x = dates, y = value, label = dates),
                         vjust = -1.5, hjust = 0, size = 3, color = "blue") +
      ggplot2::geom_vline(data = chosen_eval_validation_data, ggplot2::aes(xintercept = dates),
                          linetype = "dashed", color = "blue")

      print(plots_list$test_vs_val_chosen_eval_metric_over_time)


    #PLOT 3 - Best Hyperparameters over time
    plots_list$best_hyper_over_time <-
      ggplot2::ggplot(hyper_choice_df %>% dplyr::mutate(dates = as.Date(rownames(hyper_choice_df), format = "%Y-%m-%d")) %>% reshape::melt(id.vars="dates"),
                      ggplot2::aes(x = dates, y = value, color = variable)) +
      ggplot2::geom_line(alpha = 0.5) +
      ggplot2::geom_point() +
      ggplot2::geom_text(ggplot2::aes(label = round(value, 2)), vjust = -0.5, size = 3) +  # Add text labels for values
      ggplot2::labs(x = "Date", y = "Best hyperparameter") +
      ggplot2::theme_bw() +
      ggplot2::ggtitle("Hyper choice over time") +
      ggplot2::facet_wrap(~variable, scales = "free") +  # Create subplots for each group specified by the variable column
      ggplot2::scale_x_date(labels = scales::date_format("%b-%y")) +
      ggplot2::guides(color = ggplot2::guide_legend(title = "")) +
      ggplot2::theme(legend.position = "bottom")


      print(plots_list$best_hyper_over_time)


    #PLOT 4 - Hyperparameters vs Error
    #Transform the list in a big rbinded data frame
    chosen_eval_metric_validation_df <- do.call(rbind, chosen_eval_metric_validation)

    #For each column of hyperparameters, turn into categories
    for(j in 1:(ncol(chosen_eval_metric_validation_df)-1)){
      tryCatch({
        chosen_eval_metric_validation_df[,j] <- as.factor(#As category
          cut(chosen_eval_metric_validation_df[,j], #Cut is specially useful for random_search
              breaks=unique(stats::quantile(chosen_eval_metric_validation_df[,j], probs = seq(0,1,by=0.1))),
              include.lowest = TRUE))

      }, error = function(e) {
        message(paste("Only one unique value identified for", names(chosen_eval_metric_validation_df)[j]))
        chosen_eval_metric_validation_df[,j] <- chosen_eval_metric_validation_df[,j]
      })
    }

    #Concatenation
    if(ml_algorithm == "glmnet"){
      chosen_eval_metric_validation_df$concatenation <- paste(chosen_eval_metric_validation_df$alpha, chosen_eval_metric_validation_df$lambda.min.ratio)
    } else {}
    if(ml_algorithm == "rf"){
      chosen_eval_metric_validation_df$concatenation <- paste(chosen_eval_metric_validation_df$mtry, chosen_eval_metric_validation_df$num.trees,
                                                              chosen_eval_metric_validation_df$max.depth, chosen_eval_metric_validation_df$min.bucket)
    } else {}
    if(ml_algorithm == "xgb"){
      chosen_eval_metric_validation_df$concatenation <- paste(chosen_eval_metric_validation_df$min_child_weight,
                                                              chosen_eval_metric_validation_df$max_depth,
                                                              chosen_eval_metric_validation_df$subsample,
                                                              chosen_eval_metric_validation_df$colsample_bytree,
                                                              chosen_eval_metric_validation_df$eta,
                                                              chosen_eval_metric_validation_df$alpha,
                                                              chosen_eval_metric_validation_df$gamma,
                                                              chosen_eval_metric_validation_df$nrounds)
    } else {}
    if(ml_algorithm == "nn"){
      chosen_eval_metric_validation_df$concatenation <- paste(chosen_eval_metric_validation_df$regularizer_l1,
                                                              chosen_eval_metric_validation_df$regularizer_l2,
                                                              chosen_eval_metric_validation_df$droprate,
                                                              chosen_eval_metric_validation_df$lr)

    } else {}


    ###Summarize main quantiles
    chosen_eval_metric_validation_summary <- as.data.frame(chosen_eval_metric_validation_df %>%
                                                             dplyr::group_by(concatenation) %>% #Take by group of hyper combinations
                                                             dplyr::summarise(median_chosen_eval_metric = stats::median(chosen_eval_metric), #Q50
                                                                              q25 = stats::quantile(chosen_eval_metric, 0.25), #Q25
                                                                              q75 = stats::quantile(chosen_eval_metric, 0.75), #Q75
                                                                              max = stats::quantile(chosen_eval_metric, 1), #Q100
                                                                              min = stats::quantile(chosen_eval_metric, 0))) #Q0
    #Join with summary
    chosen_eval_metric_validation_df <- chosen_eval_metric_validation_df %>%
      dplyr::left_join(chosen_eval_metric_validation_summary, by = "concatenation")

    #Take last hyper tuning
    chosen_eval_metric_validation_last_tuning <- chosen_eval_metric_validation_df[ #Take rows from beginning to end of last hyper tuning
      (nrow(chosen_eval_metric_validation_df) - nrow(chosen_eval_metric_validation[[length(chosen_eval_metric_validation)]])):nrow(chosen_eval_metric_validation_df),]



    if(ml_algorithm  == "glmnet"){
      #Create beautiful Plot!
      plots_list$hyper_vs_error <-
        ggplot2::ggplot(chosen_eval_metric_validation_last_tuning, ggplot2::aes(x = lambda.min.ratio, y = chosen_eval_metric, fill = alpha)) +
        ggplot2::geom_bar(stat = "identity", position = "dodge") +
        ggplot2::theme_bw() +
        ggplot2::ggtitle(paste("Validation", chosen_eval_metric, "of last rebalancing facetted by alpha and lambda.min.ratio")) +
        ggplot2::facet_grid(rows = ggplot2::vars(alpha)) +
        ggplot2::geom_point(ggplot2::aes(y = max), color ="#8B0000", size=2, position = ggplot2::position_dodge(width=0.9)) +
        ggplot2::geom_point(ggplot2::aes(y = q75), color ="#B22222" , size=2, position = ggplot2::position_dodge(width=0.9)) +
        ggplot2::geom_point(ggplot2::aes(y = median_chosen_eval_metric), color ="#FF0000" , size=2, position = ggplot2::position_dodge(width=0.9)) +
        ggplot2::geom_point(ggplot2::aes(y = q25), color ="#FF6347", size=2, position = ggplot2::position_dodge(width=0.9)) +
        ggplot2::geom_point(ggplot2::aes(y = min), color ="#FFA07A", size=2, position = ggplot2::position_dodge(width=0.9)) +
        ggplot2::theme(legend.position = "bottom",
                       legend.box = "horizontal",
                       legend.title = ggplot2::element_blank(),
                       legend.margin = ggplot2::margin(5, 0, 10, 0),
                       legend.spacing = ggplot2::unit(0.2, "cm"),
                       legend.text = ggplot2::element_text(size = 10),
                       plot.margin = ggplot2::margin(2, 5, 5, 5),
                       plot.caption = ggplot2::element_text(hjust = 0)) +
        ggplot2::labs(caption = "Dots represent quantiles (min, Q25, Q50, Q75, max) of all rebalancing periods.")

        print(plots_list$hyper_vs_error)

    } else {} #end glmnet_specific

    if(ml_algorithm  == "rf"){
      #Create beautiful Plot!
      plots_list$hyper_vs_error <-
        ggplot2::ggplot(chosen_eval_metric_validation_last_tuning, ggplot2::aes(x = mtry, y = chosen_eval_metric, fill = mtry)) +
        ggplot2::geom_bar(stat = "identity", position = "dodge") +
        ggplot2::theme_bw() +
        ggplot2::ggtitle(paste("Validation", chosen_eval_metric, "of last rebalancing facetted by max.depth and min.bucket")) +
        ggplot2::facet_grid(rows = ggplot2::vars(max.depth), cols = ggplot2::vars(min.bucket)) +
        ggplot2::geom_point(ggplot2::aes(y = max), color ="#8B0000", size=2, position = ggplot2::position_dodge(width=0.9)) +
        ggplot2::geom_point(ggplot2::aes(y = q75), color ="#B22222" , size=2, position = ggplot2::position_dodge(width=0.9)) +
        ggplot2::geom_point(ggplot2::aes(y = median_chosen_eval_metric), color ="#FF0000" , size=2, position = ggplot2::position_dodge(width=0.9)) +
        ggplot2::geom_point(ggplot2::aes(y = q25), color ="#FF6347", size=2, position = ggplot2::position_dodge(width=0.9)) +
        ggplot2::geom_point(ggplot2::aes(y = min), color ="#FFA07A", size=2, position = ggplot2::position_dodge(width=0.9)) +
        ggplot2::theme(legend.position = "bottom",
                       legend.box = "horizontal",
                       legend.title = ggplot2::element_blank(),
                       legend.margin = ggplot2::margin(5, 0, 10, 0),
                       legend.spacing = ggplot2::unit(0.2, "cm"),
                       legend.text = ggplot2::element_text(size = 10),
                       plot.margin = ggplot2::margin(2, 5, 5, 5),
                       plot.caption = ggplot2::element_text(hjust = 0)) +
        ggplot2::labs(caption = "Dots represent quantiles (min, Q25, Q50, Q75, max) of all rebalancing periods.")

        print(plots_list$hyper_vs_error)


    } else {} #end rf_specific

    if(ml_algorithm  == "xgb"){
      #Create beautiful Plot!
      plots_list$hyper_vs_error <-
        ggplot2::ggplot(chosen_eval_metric_validation_last_tuning, ggplot2::aes(x = eta, y = chosen_eval_metric, fill = eta)) +
        ggplot2::geom_bar(stat = "identity", position = "dodge") +
        ggplot2::theme_bw() +
        ggplot2::ggtitle(paste("Validation", chosen_eval_metric, "of last rebalancing facetted by max_depth and colsample_bytree")) +
        ggplot2::facet_grid(rows = ggplot2::vars(max_depth), cols = ggplot2::vars(colsample_bytree)) +
        ggplot2::geom_point(ggplot2::aes(y = max), color ="#8B0000", size=2, position = ggplot2::position_dodge(width=0.9)) +
        ggplot2::geom_point(ggplot2::aes(y = q75), color ="#B22222" , size=2, position = ggplot2::position_dodge(width=0.9)) +
        ggplot2::geom_point(ggplot2::aes(y = median_chosen_eval_metric), color ="#FF0000" , size=2, position = ggplot2::position_dodge(width=0.9)) +
        ggplot2::geom_point(ggplot2::aes(y = q25), color ="#FF6347", size=2, position = ggplot2::position_dodge(width=0.9)) +
        ggplot2::geom_point(ggplot2::aes(y = min), color ="#FFA07A", size=2, position = ggplot2::position_dodge(width=0.9)) +
        ggplot2::theme(legend.position = "bottom",
                       legend.box = "horizontal",
                       legend.title = ggplot2::element_blank(),
                       legend.margin = ggplot2::margin(5, 0, 10, 0),
                       legend.spacing = ggplot2::unit(0.2, "cm"),
                       legend.text = ggplot2::element_text(size = 10),
                       plot.margin = ggplot2::margin(2, 5, 5, 5),
                       plot.caption = ggplot2::element_text(hjust = 0)) +
        ggplot2::labs(caption = "Dots represent quantiles (min, Q25, Q50, Q75, max) of all rebalancing periods.")


        print(plots_list$hyper_vs_error)


    } else {} #end xgb_specific

    if(ml_algorithm  == "nn"){
      #Create beautiful Plot!
      plots_list$hyper_vs_error <-
        ggplot2::ggplot(chosen_eval_metric_validation_last_tuning, ggplot2::aes(x = lr, y = chosen_eval_metric, fill = lr)) +
        ggplot2::geom_bar(stat = "identity", position = "dodge") +
        ggplot2::theme_bw() +
        ggplot2::ggtitle(paste("Validation", chosen_eval_metric, "of last rebalancing facetted by droprate and regularizer_l1")) +
        ggplot2::facet_grid(rows = ggplot2::vars(droprate), cols = ggplot2::vars(regularizer_l1)) +
        ggplot2::geom_point(ggplot2::aes(y = max), color ="#8B0000", size=2, position = ggplot2::position_dodge(width=0.9)) +
        ggplot2::geom_point(ggplot2::aes(y = q75), color ="#B22222" , size=2, position = ggplot2::position_dodge(width=0.9)) +
        ggplot2::geom_point(ggplot2::aes(y = median_chosen_eval_metric), color ="#FF0000" , size=2, position = ggplot2::position_dodge(width=0.9)) +
        ggplot2::geom_point(ggplot2::aes(y = q25), color ="#FF6347", size=2, position = ggplot2::position_dodge(width=0.9)) +
        ggplot2::geom_point(ggplot2::aes(y = min), color ="#FFA07A", size=2, position = ggplot2::position_dodge(width=0.9)) +
        ggplot2::theme(legend.position = "bottom",
                       legend.box = "horizontal",
                       legend.title = ggplot2::element_blank(),
                       legend.margin = ggplot2::margin(5, 0, 10, 0),
                       legend.spacing = ggplot2::unit(0.2, "cm"),
                       legend.text = ggplot2::element_text(size = 10),
                       plot.margin = ggplot2::margin(2, 5, 5, 5),
                       plot.caption = ggplot2::element_text(hjust = 0)) +
        ggplot2::labs(caption = "Dots represent quantiles (min, Q25, Q50, Q75, max) of all rebalancing periods.")


        print(plots_list$hyper_vs_error)


    } else {} #end xgb_specific

  } else {} #end ols_restrictions

  #PLOT 5 - All eval metrics over time
  plots_list$all_eval_metrics_over_time <-
    ggplot2::ggplot(oos_testing_eval_metrics %>% reshape::melt(id.vars="dates") %>%
                      dplyr::group_by(variable) %>% #group by variable
                      dplyr::mutate(variable_mean = mean(value, na.rm = TRUE)) %>% #Create new variable
                      dplyr::ungroup(),
                    ggplot2::aes(x = dates, y = value, color = variable)) +
    ggplot2::geom_line(alpha = 0.5) +
    ggplot2::geom_point() +
    ggplot2::labs(x = "Date", y = "Metric") +
    ggplot2::theme_light() +
    ggplot2::ggtitle("All eval metrics over time") +
    ggplot2::facet_wrap(~variable, scales = "free") +  # Create subplots for each group specified by the variable column
    ggplot2::scale_x_date(labels = scales::date_format("%b-%y")) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = variable_mean, color = variable), linetype = "dashed") + # Map color to variable
    ggplot2::guides(color = ggplot2::guide_legend(title = "Metric")) +
    ggplot2::geom_vline(xintercept = rebalance_dates, color = "blue", linetype = "dashed") +  # Add blue dashed lines at specific dates
    ggplot2::geom_text(data = data.frame(x = rebalance_dates, y = -Inf, label = rebalance_dates),
                       ggplot2::aes(x = x, y = y, label = label), vjust = -0.5, hjust = -0.5, size = 2, color = "black") +  # Display date labels below the plot
    ggplot2::theme(legend.position = "bottom")


    print(plots_list$all_eval_metrics_over_time)


})
