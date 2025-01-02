
#-----------------------------------------------------------------------
# meta_dataframe
#-----------------------------------------------------------------------

#' Create a meta_dataframe Object
#'
#' This generic function creates an object of class \code{meta_dataframe} from a provided \code{data.frame} or a structured \code{list}.
#' Specific behaviors are implemented via methods tailored to the class of the input data.
#'
#' @param data The input data. The class of this parameter determines which method is dispatched.
#' @param meta_dataframe_name A \code{character} string specifying the name of the resulting \code{meta_dataframe} object.
#' @param ... Additional arguments passed to methods.
#'
#' @return An object of class \code{meta_dataframe}.
#'
#' @details
#' The function dispatches to specific methods based on the class of the \code{data} argument. Currently, methods are implemented for:
#' \itemize{
#'   \item \code{data.frame}: Converts a single \code{data.frame} into a \code{meta_dataframe} object after validation.
#'   \item \code{list}: Converts a structured \code{list} of matrices, \code{data.frames}, or tibbles into a \code{meta_dataframe} object in panel data format.
#' }
#'
#' @examples
#' # Example using a data.frame
#' df <- data.frame(
#'   id = c("A-2024-01-01", "B-2024-02-01"),
#'   tickers = c("A", "B"),
#'   dates = as.Date(c("2024-01-01", "2024-02-01")),
#'   value = c(10, 20),
#'   stringsAsFactors = FALSE
#' )
#' meta_df1 <- create_meta_dataframe(data = df, meta_dataframe_name = "SingleDataFrame")
#'
#' # Example using a list of features
#' features_list <- list(
#'   matrix(1:9, nrow = 3, ncol = 3),
#'   matrix(11:19, nrow = 3, ncol = 3)
#' )
#' row_names <- c("A", "B", "C")
#' column_names <- c("2024-01-01", "2024-02-01", "2024-03-01")
#' features_names <- c("Feature1", "Feature2")
#'
#' data_input <- list(
#'   features_list = features_list,
#'   row_names = row_names,
#'   column_names = column_names,
#'   features_names = features_names
#' )
#' meta_df2 <- create_meta_dataframe(data = data_input, meta_dataframe_name = "PanelData")
#'
#' @export
setGeneric("create_meta_dataframe", function(data, meta_dataframe_name = "not_identified", ...) {
  standardGeneric("create_meta_dataframe")
})

#' @describeIn create_meta_dataframe Method for \code{data.frame} Signature
#'
#' This method creates an object of class \code{meta_dataframe} from a provided \code{data.frame}.
#' The data frame must meet specific requirements: it must contain 'id', 'tickers', and 'dates' columns,
#' where 'dates' must be of class \code{Date} and sorted in ascending order. The 'id' column should
#' be constructed as \code{paste0(tickers, "-", dates)}. The function also validates that there are no
#' missing dates, duplicated IDs, or NA values in the required columns.
#'
#' @param data A \code{data.frame} containing the data to be converted to a \code{meta_dataframe}.
#' @param meta_dataframe_name A \code{character} string specifying the name of the resulting \code{meta_dataframe} object.
#'
#' @return An object of class \code{meta_dataframe} if the input data frame meets all validation criteria.
#' The returned object includes metadata such as the number of unique dates, unique tickers, and
#' total number of observations.
#'
#' @details
#' \itemize{
#'   \item The 'id' column is expected to be in the format of \code{paste0(tickers, "-", dates)}.
#'   \item The 'dates' column must be of class \code{Date} and in ascending chronological order.
#'   \item The function checks for NA values in the 'id', 'tickers', and 'dates' columns.
#'   \item The function ensures that there are no gaps in the dates sequence and no duplicated IDs.
#'   \item The metadata includes the number of unique dates, unique tickers, and total observations.
#' }
#'
#' @examples
#' # Create a sample data frame
#' df <- data.frame(
#'   id = c("A-2024-01-01", "B-2024-02-01"),
#'   tickers = c("A", "B"),
#'   dates = as.Date(c("2024-01-01", "2024-02-01")),
#'   value = c(10, 20),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Create a meta_dataframe object
#' meta_df <- create_meta_dataframe(data = df, meta_dataframe_name = "SingleDataFrame")
#'
#' @exportMethod create_meta_dataframe
setMethod("create_meta_dataframe", signature(data = "data.frame", meta_dataframe_name = "ANY"),

          function(data, meta_dataframe_name = "not_identified", workflow = NULL, ss_backtest_workflow = NULL, sb_backtest_workflow = NULL, ...) {

            #Check for type argument
            type <- list(...)
            if(length(type) > 0){
              #Check if it is correct
              if(!type %in% c("generic", "signal_universe", "stock_universe", "oos_sb_outputs")){
                stop("type argument must be one of 'generic', 'signal_universe', 'stock_universe', or 'oos_sb_outputs'")
              }
            } else {
              type <- "generic"
            }

            if (!is.data.frame(data)) {
              stop("Input must be a data.frame")
            }

            required_columns <- c("id", "tickers", "dates")
            if (!all(required_columns %in% names(data))) {
              stop("Data must contain 'id', 'tickers', and 'dates' columns")
            }

            # Check for NA values in the required columns
            if (any(is.na(data[required_columns]))) {
              stop("Columns 'id', 'tickers', or 'dates' contain NA values")
            }

            #Check dates format
            if (!inherits(data$dates, "Date")) {
              stop("The 'dates' column must be of class 'Date'")
            }

            if (!all(diff(unique(data$dates)[order(unique(data$dates))]) >= 0)) {
              stop("Dates must be in ascending chronological order")
            }

            #Check tickers format
            if(any(!is.character(data$tickers))){
              stop("Tickers must be of class character")
            }

            # Check for NA values in remaining columns and report them
            remaining_columns <- setdiff(names(data), required_columns)
            na_remaining <- sapply(data[, remaining_columns], function(col) any(is.na(col)))
            if (any(na_remaining)) {
              warning("The following columns contain NA values: ",
                      paste(remaining_columns[na_remaining], collapse = ", "))
            }

            # Ensure the 'id' column matches paste0(tickers, "-", dates)
            expected_id <- paste0(data$tickers, "-", data$dates)
            if (!all(data$id == expected_id)) {
              stop("The 'id' column does not match 'tickers-dates')")
            }
            # Ensure no gaps in the dates sequence
            unique_dates <- unique(data$dates)
            full_dates <- seq(min(unique_dates), max(unique_dates), by = "month")
            missing_dates <- setdiff(full_dates, unique_dates)

            if (length(missing_dates) > 0 && type == "generic") {
              warning("There are gaps in the dates sequence. Missing dates: ", paste(as.Date(missing_dates), collapse = ", "))
            }

            if (any(duplicated(data$id))) {
              stop("ID column contains duplicated values")
            }

            # Check for NA values in remaining columns and report them
            remaining_columns <- setdiff(names(data), required_columns)
            na_remaining <- sapply(data[, remaining_columns], function(col) any(is.na(col)))
            if (any(duplicated(remaining_columns))) {
              stop("Column names for variables must be unique")
            }

            # Calculate metadata
            unique_dates_count <- length(unique(data$dates))
            unique_tickers_count <- length(unique(data$tickers))
            total_observations_count <- nrow(data)

            # Initialize workflow slot as an empty list
            if(type == "generic"){
              # Store metadata and column names
              return(
                new("meta_dataframe",
                  data = data,
                  workflow = workflow,
                  signals = names(data)[-c(1:3)],
                  unique_dates = unique_dates_count,
                  unique_tickers = unique_tickers_count,
                  n_obs = total_observations_count,
                  meta_dataframe_name = meta_dataframe_name)
              )
            }
            if(type == "signal_universe"){

              #Check for workflow
              if(is.null(ss_backtest_workflow)){
                stop("ss_backtest_workflow argument must be provided for signal_universe type")
              }

              # Store metadata and column names
              return(
              new("signal_universe_m_df",
                  data = data,
                  workflow = NULL,
                  signals = names(data)[-c(1:3)],
                  unique_dates = unique_dates_count,
                  unique_tickers = unique_tickers_count,
                  n_obs = total_observations_count,
                  meta_dataframe_name = meta_dataframe_name,
                  ss_backtest_workflow = ss_backtest_workflow,
                  sb_backtest_workflow = sb_backtest_workflow)
              )
            }

            if(type == "oos_sb_outputs"){
              return(
                new("oos_sb_outputs_m_df",
                    data = data,
                    workflow = NULL,
                    signals = names(data)[-c(1:3)],
                    unique_dates = unique_dates_count,
                    unique_tickers = unique_tickers_count,
                    n_obs = total_observations_count,
                    meta_dataframe_name = meta_dataframe_name)
              )
            }
          }
)


#' @describeIn create_meta_dataframe Method for \code{list} Signature
#'
#' This method converts a structured \code{list} of matrices, \code{data.frames}, or tibbles into a \code{meta_dataframe} object in panel data format.
#' The input list must contain specific components required to build the panel data structure.
#'
#' @param data A \code{list} containing the following components:
#'   \describe{
#'     \item{\code{features_list}}{A list of matrices, \code{data.frames}, or tibbles containing the features.}
#'     \item{\code{row_names}}{A vector of row names corresponding to entities in the panel data.}
#'     \item{\code{column_names}}{A vector of column names corresponding to time points or dates in the panel data.}
#'     \item{\code{features_names}}{A vector of names for each feature in the \code{features_list}.}
#'   }
#' @param meta_dataframe_name A \code{character} string specifying the name of the resulting \code{meta_dataframe} object.
#'
#' @return An object of class \code{meta_dataframe} containing the panel data and associated metadata.
#'
#' @details
#' This method performs the following operations:
#' \enumerate{
#'   \item Validates the structure and contents of the input list.
#'   \item Converts each feature set into a long (panel) format using \code{\link[reshape2]{melt}}.
#'   \item Merges all features into a single data frame, ensuring that each row represents a unique combination of entity and date.
#'   \item Calculates metadata such as the number of unique dates, unique entities (tickers), and total observations.
#'   \item Constructs a \code{meta_dataframe} S4 object encapsulating the transformed data and metadata.
#' }
#'
#' @examples
#' # Example input data
#' features_list <- list(
#'   matrix(1:9, nrow = 3, ncol = 3),
#'   matrix(11:19, nrow = 3, ncol = 3)
#' )
#' row_names <- c("A", "B", "C")
#' column_names <- c("2024-01-01", "2024-02-01", "2024-03-01")
#' features_names <- c("Feature1", "Feature2")
#'
#' # Structured input list
#' data_input <- list(
#'   features_list = features_list,
#'   row_names = row_names,
#'   column_names = column_names,
#'   features_names = features_names
#' )
#'
#' # Create the meta_dataframe object
#' meta_df <- create_meta_dataframe(data = data_input, meta_dataframe_name = "PanelData")
#'
#' # Inspect the meta_dataframe object
#' print(meta_df)
#'
#' @exportMethod create_meta_dataframe
setMethod("create_meta_dataframe", signature(data = "list", meta_dataframe_name = "ANY"),

          function(data, row_names, column_names, features_names, meta_dataframe_name = "not_identified"){

            features_list <- data

            # Check if features_list is a list of matrices, data frames, or tibbles
            if (!is.list(features_list) ||
                !all(sapply(features_list, function(x) is.data.frame(x) || is.matrix(x) || tibble::is_tibble(x))) ||
                length(unique(sapply(features_list, nrow))) != 1 ||
                length(unique(sapply(features_list, ncol))) != 1 ||
                length(row_names) != unique(sapply(features_list, nrow)) ||
                length(column_names) != unique(sapply(features_list, ncol)) ||
                length(features_names) != length(features_list)) {
              stop("Input must be a list of matrices, data frames or tibbles with the same dimensions.")
            }

            # Convert each feature in features_list to data frame
            features_list <- lapply(features_list, as.data.frame)

            #Initialize list
            panel_features <- list()

            #for every element in list
            for(l in 1:length(features_list)){
              #Tickers + Features
              features_df <- data.frame(row_names, features_list[[l]])
              colnames(features_df)[1] <- "tickers" #change col name
              colnames(features_df)[2:length(colnames(features_df))] <- as.character(column_names)

              #melt to panel format
              panel_matrix <- reshape2::melt(features_df, id.vars="tickers")
              colnames(panel_matrix)[2] <- "dates" #change name
              id <- paste(panel_matrix$tickers, panel_matrix$dates, sep = "-") #create new id
              panel_matrix <- cbind(id, panel_matrix) #append id

              #change col name to characteristic name
              colnames(panel_matrix)[4] <- features_names[l]
              panel_matrix <- panel_matrix[order(panel_matrix$id), ] #order alphabetically by id
              panel_features[[l]] <- panel_matrix #save in list
            }

            # Create new data frame to store panel data
            final_panel <- data.frame(id = panel_features[[1]]$id,
                                      tickers = panel_features[[1]]$tickers,
                                      dates = as.Date(panel_features[[1]]$dates),
                                      stringsAsFactors = FALSE)

            #Fill columns with characteristics
            for(l in 1:length(features_list)){
              final_panel[[features_names[l]]] <- panel_features[[l]][, 4] #append last column, which is the characteristic
            }

            # Calculate metadata
            unique_dates_count <- length(unique(final_panel$dates))
            unique_tickers_count <- length(unique(final_panel$tickers))
            total_observations_count <- nrow(final_panel)

            # Create meta_dataframe object
            final_panel <- new("meta_dataframe",
                               data = final_panel,
                               workflow = list(),
                               signals = features_names,
                               unique_dates = unique_dates_count,
                               unique_tickers = unique_tickers_count,
                               n_obs = total_observations_count,
                               meta_dataframe_name = meta_dataframe_name
            )

            return(final_panel)

          }

)



#-----------------------------------------------------------------------
# ss_backtest
#-----------------------------------------------------------------------

#' @title Create an ss_backtest_config Object
#' @description This function constructs an object of class `ss_backtest_config`, ensuring the proper initialization
#' and validation of its slots.
#' @param data_availability_cutoff A numeric indicating the minimum number of non-NA observations required for a backtest.
#' @param initial_sample_size A numeric indicating the minimum number of observations required to begin the backtest.
#' @param rebalancing_months A numeric indicating the number of months for rebalancing.
#' @param active_returns Logical, whether to calculate active returns when calculating performance metrics, except for CAPM (default is TRUE).
#' @param split_method A character string specifying the splitting method, either "expanding" (default) or "rolling".
#' @param enable_theme_representativeness Logical, whether to enable theme representativeness (default is TRUE).
#' @param alpha_test_strategy An `alpha_test_strategy` object defining the alpha test configuration.
#' @param config_name A character string naming the configuration.
#' @return An object of class `ss_backtest_config`.
#' @examples
#' # Example usage:
#' config <- create_ss_backtest_config(
#'   data_availability_cutoff = 100,
#'   initial_sample_size = 200,
#'   rebalancing_months = 6,
#'   alpha_test_strategy = alpha_test_strategy_obj,
#'   config_name = "ExampleConfig"
#' )
#' @export
create_ss_backtest_config <- function(
    data_availability_cutoff,
    initial_sample_size,
    rebalancing_months,
    active_returns = TRUE,
    split_method = "expanding",
    alpha_test_strategy = NULL,
    config_name = "not_identified"
) {
  # Input validation
  if (data_availability_cutoff < 0) {
    stop("data_availability_cutoff cannot be negative.")
  }
  if (initial_sample_size < 0) {
    stop("initial_sample_size cannot be negative.")
  }
  if (initial_sample_size < data_availability_cutoff) {
    stop("initial_sample_size must be greater than or equal to data_availability_cutoff.")
  }
  if (!split_method %in% c("expanding", "rolling")) {
    stop("split_method must be either 'expanding' or 'rolling'.")
  }

  # Create and return the object
  new("ss_backtest_config",
      data_availability_cutoff = data_availability_cutoff,
      initial_sample_size = initial_sample_size,
      rebalancing_months = rebalancing_months,
      active_returns = active_returns,
      split_method = split_method,
      alpha_test_strategy = alpha_test_strategy,
      config_name = config_name)
}

#' Add a `ss_backtest_config` or a `ss_backtest_results` to an existing `sb_backtest_config`.
#'
#' This generic function adds an existing `ss_backtest_config` or a `ss_backtest_results`
#' to an `sb_backtest_config` object or creates a new `ss_backtest_config` if none is provided (i.e., when ss_backtest_obj is `missing`).
#'
#' @param object A `sb_backtest_config` object to which a ss_backtest_obj will be added.
#' @param ss_backtest_object An object of class `ss_backtest_config`, `ss_backtest_results` or `missing`.
#' If `NULL`, additional parameters must be provided to create a new `ss_backtest_config`.
#' @param ... Additional arguments required to create a new `ss_backtest_config`, only needed when ss_backtest_obj is `missing`.
#' @return An updated `sb_backtest_config` object with the specified or newly created ss_backtest_obj.
#' @export
setGeneric("add_ss_backtest_obj", function(object, ss_backtest_obj, ...) {
  standardGeneric("add_ss_backtest_obj")
})

#' @describeIn add_ss_backtest_obj Add an existing ss_backtest_obj to the `sb_backtest_config`.
#'
#' This method adds a pre-existing `ss_backtest_config` to the `sb_backtest_config`.
#' It replaces any existing ss_backtest_obj in `sb_backtest_config`.
#'
#' @param object A `sb_backtest_config` object to which a ss_backtest_obj will be added.
#' @param ss_backtest_obj  An object of class `ss_backtest_config`.
#' @return The updated `sb_backtest_config` object with the provided `ss_backtest_obj`.
#' @export
setMethod("add_ss_backtest_obj", signature(object = "sb_backtest_config", ss_backtest_obj = "ss_backtest_config"),
          function(object, ss_backtest_obj) {

            #Place ss_backtest_object
            object@ss_backtest_config <- ss_backtest_obj

            # Validate the object explicitly
            validObject(object)

            return(object)
          })

#' @describeIn add_ss_backtest_obj Add an existing ss_backtest_obj to the `sb_backtest_config`.
#'
#' This method adds a pre-existing `ss_backtest_results` to the `sb_backtest_config`.
#' It replaces any existing ss_backtest_obj in `sb_backtest_config`.
#'
#' @param object A `sb_backtest_config` object to which a ss_backtest_obj will be added.
#' @param ss_backtest_obj  An object of class `sb_backtest_config`.
#' @return The updated `sb_backtest_config` object with the provided `ss_backtest_obj`.
#' @export
setMethod("add_ss_backtest_obj", signature(object = "sb_backtest_config", ss_backtest_obj = "ss_backtest_results"),
          function(object, ss_backtest_obj) {

            #Place ss_backtest_results
            object@ss_backtest_results <- ss_backtest_obj

            # Validate the object explicitly
            validObject(object)

            return(object)
          })

#' @describeIn add_ss_backtest_obj Add an existing ss_backtest_obj to the `sb_backtest_config`.
#'
#' This method creates a `ss_backtest_config` to add to the `sb_backtest_config`.
#'
#' @param object A `sb_backtest_config` object to which a ss_backtest_obj will be added.
#' @param
#' @return The updated `sb_backtest_config` object with the created `ss_backtest_config`.
#' @export
setMethod("add_ss_backtest_obj", signature(object = "sb_backtest_config", ss_backtest_obj = "missing"),
          function(object, data_availability_cutoff, initial_sample_size, rebalancing_months, active_returns = TRUE, split_method = "expanding",
                   alpha_test_strategy = NULL, config_name = "not_identified") {

            #Create an empty alpha_test_strategy
            if(is.null(alpha_test_strategy)){
              alpha_test_strategy <- create_alpha_test_strategy()
            }

            #create ss_backtest_config
            ss_backtest_config <- create_ss_backtest_config(data_availability_cutoff = data_availability_cutoff,
                                                            initial_sample_size = initial_sample_size,
                                                            rebalancing_months = rebalancing_months,
                                                            active_returns = active_returns,
                                                            split_method = split_method,
                                                            alpha_test_strategy = alpha_test_strategy,
                                                            config_name = config_name)

            #Include
            object@ss_backtest_config <- ss_backtest_config

            # Validate the object explicitly
            validObject(object)

            return(object)
          })


#-----------------------------------------------------------------------
# alpha_test_strategy
#-----------------------------------------------------------------------

#' @title Create an alpha_test_strategy object
#' @description A constructor function to create instances of alpha_test_strategy or its subclasses
#' (frequentist_alpha_test_strategy and bayesian_alpha_test_strategy).
#' @param signal_significance_threshold A numeric value indicating the hypothesis testing zero-alpha null-hypothesis rejection criteria.
#'   Must be between 0 and 1. Defaults to 0.05.
#' @param p_correction_method A character string specifying the p-value correction method.
#'   Options include `"none"`, `"bonferroni"`, `"holm"`, `"hochberg"`, `"hommel"`, `"BH"`, `"fdr"`, `"BY"`, and `"bayesian"`.
#' @param market_factor_proxy A character string indicating the market factor proxy to be used in the CAPM model.
#' @param bayesian_model_parameters (Optional) An object of class `bayesian_model_parameters`.
#'   Required when `p_correction_method` is `"bayesian"`.
#' @return An object of class `alpha_test_strategy`, `frequentist_alpha_test_strategy`, or `bayesian_alpha_test_strategy`.
#' @export
create_alpha_test_strategy <- function(
    model_structure = "no_pooled",
    theme_level_intercept = NULL,
    theme_level_slope = NULL,
    signal_significance_threshold = 0.05,
    p_correction_method = "none",
    market_factor_proxy,
    bayesian_model_parameters = NULL,
    enable_theme_representativeness = TRUE,
    lmer_control = NULL
) {

  # Validate input arguments
  if (!p_correction_method %in% c("none", "bonferroni", "holm", "hochberg", "hommel", "BH", "fdr", "BY", "bayesian")) {
    stop("Invalid p_correction_method. Must be one of: 'none', 'bonferroni', 'holm', 'hochberg', 'hommel', 'BH', 'fdr', 'BY', 'bayesian'.")
  }
  if (signal_significance_threshold < 0 || signal_significance_threshold > 1) {
    stop("signal_significance_threshold must be between 0 and 1.")
  }
  if (missing(market_factor_proxy) || !is.character(market_factor_proxy) || length(market_factor_proxy) != 1) {
    stop("market_factor_proxy must be a single character string.")
  }
  if(!model_structure %in% c("partial_pooled", "no_pooled")){
    stop("Currently, model_structure must be one of partial_pooled or no_pooled")
  }
  if(model_structure == "partial_pooled"){
    if (is.null(theme_level_intercept) || !theme_level_intercept %in% c("fixed", "random", "theme_specific")){
      stop("theme_level_intercept must be 'fixed', 'random' or 'theme_specific'")
    }
    if (is.null(theme_level_slope) || !theme_level_slope %in% c("fixed", "theme_specific")){
      stop("Currently, theme_level_slope can only be 'fixed' or 'theme_specific'")
    }
    avaiable_combinations <- c(c("random_intercept_fixed_slope"), #old random_intercept
                               c("theme_specific_intercept_fixed_slope"), #old fixed_intercepts
                               c("theme_specific_intercept_theme_specific_slope"), #old fixed_intercepts_fixed_slopes
                               c("fixed_intercept_fixed_slope")) #one none
    chosen_combination <- paste0(theme_level_intercept, "_intercept_", theme_level_slope, "_slope")

    if(!chosen_combination %in% avaiable_combinations){
      stop("Chosen combination of theme_level_intercept and theme_level_slope is currently not supported.")
    }
  } else {
    if(any(!is.null(theme_level_intercept), !is.null(theme_level_slope))){
      stop("Theme-level parameters are only avaiable for partial pooled models.")
    }
  }

  # Handle Bayesian subclass creation
  if (p_correction_method == "bayesian") {
    if(model_structure != "partial_pooled"){
      stop("Currently, only the 'partial_pooled' model structure is supported for Bayesian alpha testing.")
    }
    if (!is.null(bayesian_model_parameters) && !inherits(bayesian_model_parameters, "bayesian_model_parameters")) {
      stop("When p_correction_method is 'bayesian', bayesian_model_parameters must be a bayesian_model_parameters object.")
    }

    #Check if a bayesian_model_parametesr is being provided
    if(is.null(bayesian_model_parameters)){
      #If not create a generic one
      bayesian_model_parameters = new("bayesian_model_parameters",
                                      user_priors = NULL,
                                      prior_derivation_control = NULL,
                                      brms_control = NULL
      )
    }

    return(new("bayesian_alpha_test_strategy",
               signal_significance_threshold = signal_significance_threshold,
               p_correction_method = p_correction_method,
               model_structure = model_structure,
               theme_level_intercept = theme_level_intercept,
               theme_level_slope = theme_level_slope,
               market_factor_proxy = market_factor_proxy, #For a new bayesian class, create an uniformative bayesian_model_parameters
               enable_theme_representativeness = enable_theme_representativeness,
               bayesian_model_parameters = bayesian_model_parameters,
               lmer_control = lmer_control
    )
    )
  }

  # Handle Frequentist subclass creation
  if (p_correction_method %in% c("none", "bonferroni", "holm", "hochberg", "hommel", "BH", "fdr", "BY")) {
    return(new("frequentist_alpha_test_strategy",
               signal_significance_threshold = signal_significance_threshold,
               model_structure = model_structure,
               theme_level_intercept = theme_level_intercept,
               theme_level_slope = theme_level_slope,
               p_correction_method = p_correction_method,
               enable_theme_representativeness = enable_theme_representativeness,
               market_factor_proxy = market_factor_proxy,
               lmer_control = lmer_control))
  }

  # Default fallback (should not reach here due to prior validation)
  stop("Unexpected error in create_alpha_test_strategy. Check input parameters.")
}


#' @title Add Alpha Test Strategy to ss_backtest_config
#' @description This method allows you to add an `alpha_test_strategy` object to an `ss_backtest_config` object.
#' @param object An `ss_backtest_config` object.
#' @param alpha_test_strategy An `alpha_test_strategy` object to be added.
#' @return The updated `ss_backtest_config` object with the specified `alpha_test_strategy` added.
#' @examples
#' # Example usage
#' alpha_strategy <- new("frequentist_alpha_test_strategy",
#'                        signal_significance_threshold = 0.05,
#'                        p_correction_method = "holm",
#'                        market_factor_proxy = "S&P500")
#' config <- create_ss_backtest_config(
#'   data_availability_cutoff = 100,
#'   initial_sample_size = 200,
#'   rebalancing_months = 6,
#'   alpha_test_strategy = NULL,
#'   config_name = "ExampleConfig"
#' )
#' config <- add_alpha_test_strategy(config, alpha_strategy)
#' @export
setGeneric("add_alpha_test_strategy", function(object, alpha_test_strategy, ...) {
  standardGeneric("add_alpha_test_strategy")
})

#' @rdname add_alpha_test_strategy
#' @export
setMethod(
  "add_alpha_test_strategy",
  signature(object = "ss_backtest_config", alpha_test_strategy = "alpha_test_strategy"),
  function(object, alpha_test_strategy) {

    # Set the alpha_test_strategy slot
    object@alpha_test_strategy <- alpha_test_strategy

    # Return the updated object
    return(object)
  }
)

#' @rdname add_alpha_test_strategy
#' @export
setMethod(
  "add_alpha_test_strategy",
  signature(object = "ss_backtest_config", alpha_test_strategy = "missing"),
  function(object, signal_significance_threshold = 0.05, p_correction_method = "none", market_factor_proxy,
           model_structure = "partial_pooled", theme_level_intercept = NULL, theme_level_slope = NULL,
           enable_theme_representativeness = TRUE, bayesian_model_parameters = NULL, lmer_control = NULL) {

    alpha_test_strategy <- create_alpha_test_strategy(signal_significance_threshold = signal_significance_threshold,
                                                      p_correction_method = p_correction_method,
                                                      market_factor_proxy = market_factor_proxy,
                                                      model_structure = model_structure,
                                                      theme_level_intercept = theme_level_intercept,
                                                      theme_level_slope = theme_level_slope,
                                                      enable_theme_representativeness = enable_theme_representativeness,
                                                      bayesian_model_parameters = bayesian_model_parameters,
                                                      lmer_control = lmer_control
    )

    # Set the alpha_test_strategy slot
    object@alpha_test_strategy <- alpha_test_strategy

    # Return the updated object
    return(object)
  }
)

#-----------------------------------------------------------------------
# bayesian_model_parameters
#-----------------------------------------------------------------------

#' @title Create Bayesian Model Parameters
#' @description Constructor for an S4 object of class \code{bayesian_model_parameters}.
#'
#' @param user_priors An object of class \code{brmsprior}, or \code{NULL}.
#'   Structured according to the \code{model_spec_theme_level}.
#' @param prior_derivation_control A list of additional parameters for deriving priors when \code{priors_type} is
#'   \code{"informative_exogenous_dataset"}. Must be a list with the following elements:
#'   \itemize{
#'     \item \code{half_t_df}: Degrees of freedom for the half-t distribution applied to sd priors.
#'     \item \code{lmer_optimizer}: Optimizer to be used in \code{lme4::lmer} for deriving priors.
#'     \item \code{lmer_optimization_objective}: Criteria to be optimized in \code{lme4::lmer} for deriving priors,
#'           e.g. \code{"likelihood"} or \code{"REML"}.
#'   }
#' @param brms_control A list of parameters to be passed to \code{brms::brm} for MCMC sampling, including:
#'   \itemize{
#'     \item \code{chains}: Number of Markov chains (default is 4).
#'     \item \code{iter}: Number of iterations per chain (default is 2000).
#'     \item \code{warmup}: Number of warmup iterations per chain (default is \code{floor(iter / 2)}).
#'     \item \code{thin}: Thinning interval (default is 1).
#'     \item \code{seed}: Seed for reproducibility (default is \code{NA}).
#'     \item \code{adapt_delta}: Target acceptance probability for HMC (default is 0.80).
#'   }
#'
#' @return An S4 object of class \code{bayesian_model_parameters}.
#' @export
#'
#' @examples
#' # Create a minimal bayesian_model_parameters object with defaults:
#' bayes_params <- create_bayesian_model_parameters()
#'
#' # Create one with custom brms_control:
#' bayes_params_custom <- create_bayesian_model_parameters(
#'   brms_control = list(chains = 2, iter = 1500, adapt_delta = 0.9)
#' )
create_bayesian_model_parameters <- function(
    user_priors = NULL,
    prior_derivation_control = NULL,
    brms_control = list(
      chains = 4,
      iter = 2000,
      warmup = 1000,
      thin = 1,
      seed = NA,
      adapt_delta = 0.80
    )
) {

  # Build an S4 object with the supplied arguments.
  bayes_obj <- methods::new(
    "bayesian_model_parameters",
    user_priors = user_priors,
    prior_derivation_control = prior_derivation_control,
    brms_control = brms_control
  )

  # Trigger validation checks (as defined in the class).
  methods::validObject(bayes_obj)

  return(bayes_obj)
}


#' @title Add Bayesian Model Parameters
#' @description Generic function to add Bayesian model parameters.
#' @param object The object to which Bayesian model parameters will be added.
#' @param user_priors An optional object of class `brmsprior`.
#' @param prior_derivation_control An optional list containing prior derivation control parameters.
#' @param brms_control An optional list of parameters for `brms::brm`.
#' @return The updated object with the `bayesian_model_parameters` added.
#' @export
setGeneric("add_bayesian_model_parameters", function(object, user_priors = NULL, prior_derivation_control = NULL, brms_control = NULL) {
  standardGeneric("add_bayesian_model_parameters")
})

#' @rdname add_bayesian_model_parameters
#' @export
setMethod(
  "add_bayesian_model_parameters",
  signature(object = "bayesian_alpha_test_strategy"),
  function(object, user_priors = NULL, prior_derivation_control = NULL, brms_control = NULL) {

    # Ensure only one of `user_priors` or `prior_derivation_control` is provided
    if (!is.null(user_priors) && !is.null(prior_derivation_control)) {
      stop("Only one of 'user_priors' or 'prior_derivation_control' can be provided, not both.")
    }

    # Check `user_priors` argument
    if (!is.null(user_priors) && !inherits(user_priors, "brmsprior")) {
      stop("When provided, 'user_priors' must be an object of class 'brmsprior'.")
    }

    # Create `bayesian_model_parameters` object
    bayesian_params <- new("bayesian_model_parameters",
                           user_priors = user_priors,
                           prior_derivation_control = prior_derivation_control,
                           brms_control = brms_control)

    # Add `bayesian_model_parameters` to `bayesian_alpha_test_strategy`
    object@bayesian_model_parameters <- bayesian_params
    return(object)
  }
)

#' @rdname add_bayesian_model_parameters
#' @export
setMethod(
  "add_bayesian_model_parameters",
  signature(object = "ss_backtest_config"),
  function(object,
           user_priors = NULL, prior_derivation_control = NULL, brms_control = NULL) {
    # Check if `alpha_test_strategy` is set
    if (is.null(object@alpha_test_strategy)) {
      stop("The 'alpha_test_strategy' slot is not set in the ss_backtest_config object.")
    }

    # Ensure the `alpha_test_strategy` is of class `bayesian_alpha_test_strategy`
    if (!is(object@alpha_test_strategy, "bayesian_alpha_test_strategy")) {
      stop("The 'alpha_test_strategy' in the ss_backtest_config object is not of class 'bayesian_alpha_test_strategy'.")
    }

    # Call the method for 'bayesian_alpha_test_strategy' to add 'bayesian_model_parameters'
    object@alpha_test_strategy <- add_bayesian_model_parameters(
      object@alpha_test_strategy,
      user_priors = user_priors,
      prior_derivation_control = prior_derivation_control,
      brms_control = brms_control
    )

    # Return the updated 'ss_backtest_config' object
    return(object)
  }
)


#' @title Add Prior to Bayesian Alpha Test Strategy
#' @description Adds a prior to a `bayesian_alpha_test_strategy` object based on provided parameters.
#' @param object An object of class `bayesian_alpha_test_strategy`.
#' @param distribution_choice A vector of strings representing the distribution of the prior. See details for valid options.
#' @param pars A list of named numeric vectors. The names should match the expected parameter names for the chosen distribution.
#' @param class A vector of character strings representing the class of the prior. Should be one of 'Intercept', 'b', 'sd', 'sigma', or 'cor'.
#' @param coef A vector of character strings representing the coefficient name. Only applicable when `class` is 'b' or 'sd'.
#' @param group A vector of character strings representing the group name. Only applicable when `class` is 'b' or 'sd'.
#' @return An updated `bayesian_alpha_test_strategy` object with the added prior.
#' @export
setGeneric("add_brms_prior", function(object, ...) standardGeneric("add_brms_prior"))

#' @rdname add_brms_prior
#' @export
setMethod("add_brms_prior",
          signature(object = "bayesian_alpha_test_strategy"),
          function(object, distribution_choice, pars, class, coef = NULL, group = NULL) {

            # Ensure lengths match
            n <- length(distribution_choice)
            if (!all(lengths(list(distribution_choice, pars, class)) == n)) {
              stop("All arguments must have the same length.")
            }
            if (!is.null(coef) && length(coef) != n) {
              stop("`coef` must be NULL or have the same length as `distribution_choice`.")
            }
            if (!is.null(group) && length(group) != n) {
              stop("`group` must be NULL or have the same length as `distribution_choice`.")
            }

            # Replace NA values with empty strings for `coef` and `group`
            if (is.null(coef)) {
              coef <- rep("", n)
            } else {
              coef[is.na(coef)] <- ""
            }
            if (is.null(group)) {
              group <- rep("", n)
            } else {
              group[is.na(group)] <- ""
            }

            # Validate `class`
            if (!all(class %in% c("Intercept", "b", "sd", "sigma", "cor"))) {
              stop("Invalid `class` values. Must be one of 'Intercept', 'b', 'sd', 'sigma', or 'cor'.")
            }

            # Validate `pars` against `distribution_choice`
            valid_distributions <- list(
              normal = c("mean", "sd"),
              student_t = c("df", "mean", "sd"),
              cauchy = c("location", "scale"),
              lognormal = c("meanlog", "sdlog"),
              inv_gamma = c("shape", "scale"),
              lkj = c("eta")
            )
            mapply(function(dist, params) {
              if (!dist %in% names(valid_distributions)) {
                stop(sprintf("Invalid distribution: '%s'.", dist))
              }
              missing_params <- setdiff(valid_distributions[[dist]], names(params))
              if (length(missing_params) > 0) {
                stop(sprintf("Missing parameters for '%s': %s.", dist, paste(missing_params, collapse = ", ")))
              }
              TRUE
            }, distribution_choice, pars, SIMPLIFY = FALSE)

            # Corrected validation for group and coef restrictions
            if (any(!(class %in% c("b", "sd")) & group != "")) {
              stop("Group should only be specified for class 'b' or 'sd'.")
            }
            if (any(!(class %in% c("b", "sd")) & coef != "")) {
              stop("Coef should only be specified for class 'b' or 'sd'.")
            }

            # Validate against model specification
            theme_level_intercept <- object@theme_level_intercept
            theme_level_slope <- object@theme_level_slope

            chosen_combination <- paste0(theme_level_intercept, "_intercept", theme_level_slope, "_slope")

            invalid_priors <- switch(
              chosen_combination,
              "random_intercept_fixed_slope" = {
                sapply(seq_along(distribution_choice), function(i) {
                  grepl("^theme", coef[i]) || grepl("^theme.*:market_factor_proxy$", coef[i])
                })
              },
              "theme_specific_intercept_fixed_slope" = {
                sapply(seq_along(distribution_choice), function(i) {
                  class[i] == "Intercept" || grepl("^theme.*:market_factor_proxy$", coef[i])
                })
              },
              "theme_specific_intercept_theme_specific_slope" = {
                sapply(seq_along(distribution_choice), function(i) {
                  class[i] == "Intercept" || coef[i] == "market_factor_proxy"
                })
              },
              "fixed_intercept_fixed_slope" = {
                sapply(seq_along(distribution_choice), function(i) {
                  grepl("^theme", coef[i])
                })
              },
              stop("Invalid model structure")
            )

            if (any(invalid_priors)) {
              stop(sprintf("Some priors are invalid for the chosen model structure at theme_level: '%s'.", chosen_combination))
            }

            # Generate `brmsprior` object
            new_priors <- lapply(seq_along(distribution_choice), function(i) {
              brms::set_prior(
                paste0(distribution_choice[i], "(", paste(pars[[i]], collapse = ", "), ")"),
                class = class[i],
                coef = coef[i],
                group = group[i]
              )
            })

            # Combine with existing `user_priors`
            if (is.null(object@bayesian_model_parameters@user_priors)) {
              object@bayesian_model_parameters@user_priors <- do.call(rbind, new_priors)
            } else {
              object@bayesian_model_parameters@user_priors <- rbind(
                object@bayesian_model_parameters@user_priors,
                do.call(rbind, new_priors)
              )
            }

            validObject(object@bayesian_model_parameters)
            return(object)
          })

#' @rdname add_brms_prior
#' @export
#' @rdname add_brms_prior
#' @export
setMethod(
  "add_brms_prior",
  signature(object = "ss_backtest_config"),
  function(object, distribution_choice, pars, class, coef = NULL, group = NULL) {
    # Check if `alpha_test_strategy` is set
    if (is.null(object@alpha_test_strategy)) {
      stop("The 'alpha_test_strategy' slot is not set in the ss_backtest_config object.")
    }

    # Ensure the `alpha_test_strategy` is of class `bayesian_alpha_test_strategy`
    if (!is(object@alpha_test_strategy, "bayesian_alpha_test_strategy")) {
      stop("The 'alpha_test_strategy' in the ss_backtest_config object is not of class 'bayesian_alpha_test_strategy'.")
    }

    # Delegate the call to the 'alpha_test_strategy' object's method
    object@alpha_test_strategy <- add_brms_prior(
      object@alpha_test_strategy,
      distribution_choice = distribution_choice,
      pars = pars,
      class = class,
      coef = coef,
      group = group
    )

    # Return the updated 'ss_backtest_config' object
    return(object)
  }
)




#-----------------------------------------------------------------------
# tuning_strategy
#-----------------------------------------------------------------------

#' @title Hyperparameter Tuning Strategy Constructor
#' @description A constructor function to create a tuning_strategy object, based on the specified tuning method.
#' @param tuning_method Character string indicating the hyperparameter tuning method. Must be one of 'grid_search', 'random_search', or 'bayesian_opt'.
#' @param validation_sample_size Numeric value representing the size of the validation sample.
#' @param chosen_eval_metric Character or NULL, specifying the evaluation metric to be optimized.
#' @param early_stop ANY, optional argument for halting criteria.
#' @param n_iter Numeric, number of iterations for 'random_search' or 'bayesian_opt'.
#' @param acq Character string for the acquisition function (for 'bayesian_opt' only).
#' @param init_points Numeric, number of initial random points for Bayesian optimization (for 'bayesian_opt' only).
#' @param k_iter Numeric, number of samples to evaluate during Bayesian optimization (for 'bayesian_opt' only).
#' @return An object of class `grid_search_strategy`, `random_search_strategy`, or `bayesian_opt_strategy`, depending on the selected `tuning_method`.
#' @export
create_tuning_strategy <- function(tuning_method, validation_sample_size, chosen_eval_metric, hyper_grid_domain = NULL, early_stop = NULL,
                                   n_iter = NULL, acq = NULL, init_points = NULL, k_iter = NULL) {

  # Check if hyper_grid_domain is provided; if not, create an empty one
  if(!tuning_method %in% c("grid_search", "random_search", "bayesian_opt")){
    stop("tuning_method must be one of grid_search, random_search or bayesian_opt")
  }
  if (is.null(hyper_grid_domain)) {
    hyper_grid_domain <-  new("hyper_grid_domain", hyperparameter_list = list())
  }

  # Check the value of tuning_method and create the appropriate subclass
  if (tuning_method == "grid_search") {
    # Create a grid_search_strategy object
    return(new("grid_search_strategy",
               tuning_method = "grid_search",
               validation_sample_size = validation_sample_size,
               chosen_eval_metric = chosen_eval_metric,
               hyper_grid_domain = hyper_grid_domain,
               early_stop = early_stop))

  } else if (tuning_method == "random_search") {
    # Create a random_search_strategy object
    if (is.null(n_iter)) {
      stop("n_iter must be provided for random_search.")
    }
    return(new("random_search_strategy",
               tuning_method = "random_search",
               validation_sample_size = validation_sample_size,
               chosen_eval_metric = chosen_eval_metric,
               hyper_grid_domain = hyper_grid_domain,
               early_stop = early_stop,
               n_iter = n_iter))

  } else if (tuning_method == "bayesian_opt") {
    # Create a bayesian_opt_strategy object
    if (is.null(n_iter) || is.null(acq) || is.null(init_points) || is.null(k_iter)) {
      stop("n_iter, acq, init_points, and k_iter must be provided for bayesian_opt.")
    }
    return(new("bayesian_opt_strategy",
               tuning_method = "bayesian_opt",
               validation_sample_size = validation_sample_size,
               chosen_eval_metric = chosen_eval_metric,
               hyper_grid_domain = hyper_grid_domain,
               early_stop = early_stop,
               n_iter = n_iter,
               acq = acq,
               init_points = init_points,
               k_iter = k_iter))
  } else {
    stop("Invalid tuning_method. Choose from 'grid_search', 'random_search', or 'bayesian_opt'.")
  }
}

#' Add a `tuning_strategy` to an existing `sb_backtest_config`.
#'
#' This generic function adds an existing `tuning_strategy` object or creates a new one if none is provided
#'  (i.e., when tuning_strategy is `NULL`).
#'
#' @param object A `sb_backtest_config` object to which a ss_backtest_obj will be added.
#' @param tuning_strategy An object of class `tuning_strategy`, or `NULL`.
#' If `NULL`, additional parameters must be provided to create a new `tuning_strategy`.
#' @param ... Additional arguments required to create a new `tuning_strategy`, only needed when tuning_strategy is `NULL`.
#' @return An updated `sb_backtest_config` object with the specified or newly created `tuning_strategy`.
#' @export
setGeneric("add_tuning_strategy", function(object, tuning_strategy, ...) {
  standardGeneric("add_tuning_strategy")
})

#' @describeIn add_tuning_strategy Add an existing `tuning_strategy` to the `sb_backtest_config`.
#'
#' This method adds a pre-existing `tuning_strategy` to the `sb_backtest_config`. It replaces any existing tuning strategy in the experiment.
#'
#' @param object A `sb_backtest_config` object.
#' @param tuning_strategy An object of class `tuning_strategy` to be added to the `sb_backtest_config`.
#' @return The updated `sb_backtest_config` object with the provided `tuning_strategy`.
#' @export
setMethod("add_tuning_strategy", signature(object = "sb_backtest_config", tuning_strategy = "tuning_strategy"),
          function(object, tuning_strategy) {

            #Adjust validation sample size
            if(tuning_strategy@validation_sample_size < 1){
              tuning_strategy@validation_sample_size <- round(tuning_strategy@validation_sample_size * object@training_sample_size)
            }

            if(object@sb_algorithm != "ols"){
              object@tuning_strategy <- tuning_strategy
            } else {
              stop("OLS does not require tuning.")
            }

            #Give warning if validation_sample size is bigger than training sample size
            if(tuning_strategy@validation_sample_size > object@training_sample_size){
              message("Validation sample size is bigger than training sample size.")
            }

            # Validate the object explicitly
            validObject(object)

            return(object)
          })



#' @describeIn add_tuning_strategy Create and add a new `tuning_strategy` to the `sb_backtest_config`.
#'
#' This method is used when `tuning_strategy` is `NULL`. It creates a new tuning strategy based on the provided parameters and adds it to the `sb_backtest_config`.
#'
#' @param object A `sb_backtest_config` object.
#' @param tuning_strategy `NULL`, indicating that a new `tuning_strategy` should be created.
#' @param tuning_method Character string indicating the hyperparameter tuning method. Must be one of 'grid_search', 'random_search', or 'bayesian_opt'.
#' @param validation_sample_size Numeric value representing the size of the validation sample.
#' @param chosen_eval_metric Character or `NULL`, specifying the evaluation metric to be optimized.
#' @param early_stop Optional, stopping criteria for early termination. Can be of any type.
#' @param n_iter Numeric, number of iterations for 'random_search' or 'bayesian_opt'.
#' @param acq Character string specifying the acquisition function for Bayesian optimization (for 'bayesian_opt' only).
#' @param init_points Numeric, number of initial random points for Bayesian optimization (for 'bayesian_opt' only).
#' @param k_iter Numeric, number of samples to evaluate during Bayesian optimization (for 'bayesian_opt' only).
#' @return An updated `sb_backtest_config` object with a newly created `grid_search_strategy`, `random_search_strategy`, or `bayesian_opt_strategy`, depending on the selected `tuning_method`.
#' @export
setMethod("add_tuning_strategy", signature(object = "sb_backtest_config", tuning_strategy = "missing"),
          function(object, tuning_strategy = NULL, tuning_method, validation_sample_size, chosen_eval_metric = NULL, hyper_grid_domain = NULL, early_stop = NULL,
                   n_iter = NULL, acq = "ucb", init_points = NULL, k_iter = NULL) {

            #Custom fill of chosen eval metric in case of null
            if(is.null(chosen_eval_metric)){
              chosen_eval_metric <- switch(object@custom_objective,
                                           "pseudo_huber_error" = "mphe",
                                           "quantile_error" = "quantile_loss",
                                           "absolute_error" = "mae",
                                           "rmse"
              )
              message(paste("chosen_eval_metric set to", chosen_eval_metric, "according to custom_objective.\n"))

            }

            #Adjust validation sample size if decimal
            if(validation_sample_size < 1){
              validation_sample_size <- round(validation_sample_size * object@training_sample_size)
            }

            #Give warning if validation_sample size is bigger than training sample size
            if(validation_sample_size > object@training_sample_size){
              message("Validation sample size is bigger than training sample size.")
            }

            if(!object@sb_algorithm %in% c("ols", "sw", "ew", "rp", "mto")){

              # Create a new tuning_strategy object
              object@tuning_strategy <- create_tuning_strategy(tuning_method = tuning_method, validation_sample_size = validation_sample_size,
                                                               chosen_eval_metric = chosen_eval_metric, early_stop = early_stop, n_iter = n_iter, acq = acq,
                                                               init_points = init_points, k_iter = k_iter)
            } else {
              stop("OLS, SW, EW, RP and MTO do not require tuning.")
            }


            # Validate the object explicitly
            validObject(object)

            return(object)
          }
)

#-----------------------------------------------------------------------
# hyperparameters
#-----------------------------------------------------------------------


#' Add a Hyperparameter to a `hyper_grid_domain`, whether inside a `sb_backtest_config`, a `tuning_strategy` or on its own.
#'
#' This generic function adds a new hyperparameter to an existing `hyper_grid_domain` object. The function is overloaded to handle different types of hyperparameters for different machine learning algorithms.
#'
#' @param object A `tuning_strategy` or a `hyper_grid_domain` object.
#' @param hyperparameter A vector of characters indicating the name of the hyperparameter to be added. Options are:
#' \itemize{
#'  \item \strong{glmnet}: alpha, lambda.min.ratio
#'  \item \strong{rf}: mtry, num.trees, max.depth, min.bucket
#'  \item \strong{xgb}: min_child_weight, max_depth, subsample, colsample_bytree, eta, gamma, nrounds
#'  \item \strong{nn}: regularizer_l1, regularizer_l2, droprate, lr, size_of_batch, number_of_epochs
#' }
#'
#' @return A `hyper_grid_domain` object with the updated hyperparameters.
#'
#' @examples
#' # Create an initial tuning_strategy object for grid-search
#' grid_search_obj <- create_tuning_strategy(
#'   tuning_method = "grid_search",
#'   validation_sample_size = 1000,
#'   split_method = "expanding",
#'   chosen_eval_metric = "rmse"
#' )
#'
#' # Add hyperparameter for grid_search
#' grid_search_obj <- add_hyperparameter(
#'   grid_search_obj,
#'   hyperparameter = c("alpha", "lambda.min.ratio"),
#'   grid = list(c(0.2, 0.5), c(0.5, 0.2, 0.3))
#' )
#'
#' # Create an initial tuning_strategy object for random_search
#' random_search_obj <- create_tuning_strategy(
#'   tuning_method = "random_search",
#'   validation_sample_size = 1000,
#'   split_method = "rolling",
#'   chosen_eval_metric = "mae",
#'   n_iter = 20
#' )
#'
#' # Add hyperparameters for random_search
#' random_search_obj <- add_hyperparameter(
#'   random_search_obj,
#'   hyperparameter = c("min_child_weight", "max_depth", "subsample", "colsample_bytree", "eta", "alpha", "gamma", "nrounds"),
#'   distribution_choice = c("constant", "uniform", "uniform", "uniform", "uniform", "uniform", "constant", "uniform"),
#'   pars = list(3, c(min = 1L, max = 2L), c(min = 0.25, max = 0.50), c(min = 0.25, max = 0.50), c(min = 0.1, max = 0.2),  c(min = 2, max = 5), 0, c(min = 200L, max = 500L))
#' )
#'
#' # Create an initial tuning_strategy object for bayesian_opt
#' bayesian_opt_obj <- create_tuning_strategy(
#'   tuning_method = "bayesian_opt",
#'   validation_sample_size = 1000,
#'   split_method = "expanding",
#'   chosen_eval_metric = "mape",
#'   n_iter = 50,
#'   acq = "ei",
#'   init_points = 5,
#'   k_iter = 3
#' )
#'
#' # Add hyperparameters for bayesian_opt
#' bayesian_opt_obj <- add_hyperparameter(
#'   bayesian_opt_obj,
#'   hyperparameter = c("mtry", "num.trees", "max.depth", "min.bucket"),
#'   bounds = list(c(0,1), c(100L, 1000L), c(2L, 8L), c(1, 5))
#' )
#'
#' # Creating a hyper_grid_domain object for a xgb model
#' hyper_grid_xgb_random <- create_hyper_grid_domain(
#'  tuning_method = "random_search",
#'  sb_algorithm = "xgb"
#'  )
#'
#'  hyper_grid_xgb_random <- add_hyperparameter(
#'  hyperparameter = c("min_child_weight", "max_depth", "subsample", "colsample_bytree",
#'                     "eta", "alpha", "gamma", "nrounds"),
#'  distribution_choice = c("constant", "uniform", "uniform", "uniform",
#'                          "uniform", "uniform", "constant", "uniform"),
#'  pars = list(3, c(min = 1L, max = 2L), c(min = 0.25, max = 0.50),
#'            c(min = 0.25, max = 0.50), c(min = 0.1, max = 0.2),
#'            c(min = 2, max = 5), 0, c(min = 200L, max = 500L))
#'  )
#'
#' @export
setGeneric("add_hyperparameter", function(object, hyperparameter, ...) {
  standardGeneric("add_hyperparameter")
})

#' @describeIn add_hyperparameter Add hyperparameter to `hyper_grid_domain` object
#' @param hyperparameter A vector of characters indicating the name of the hyperparameter to be added. Options are:
#' \itemize{
#'  \item \strong{glmnet}: alpha, lambda.min.ratio
#'  \item \strong{rf}: mtry, num.trees, max.depth, min.bucket
#'  \item \strong{xgb}: min_child_weight, max_depth, subsample, colsample_bytree, eta, gamma, nrounds
#'  \item \strong{nn}: regularizer_l1, regularizer_l2, droprate, lr, size_of_batch, number_of_epochs
#' }
#' @param grid A numeric vector or list of numeric vectors for grid search values (only used for grid_search).
#' @param distribution_choice A character vector indicating the distribution to sample from (only used for random_search).
#' @param pars A numeric named vector or list of numeric named vectors specifying parameter values (only used for random_search).
#' @param bounds A vector of length 2 indicating minimum and maximum bounds for each hyperparameter (only used for bayesian_opt).
#' @param new_hyperparameter_list Used to pass new_hyperparameters_list after specific method at tuning_strategy level.
#'
setMethod("add_hyperparameter",
          signature(object = "hyper_grid_domain"),
          function(object, new_hyperparameter_list) {

            #Get names stored in new_hyperparameter_list
            hyperparameter <- names(new_hyperparameter_list)

            # Merge with existing hyperparameters in object
            if (length(object@hyperparameter_list) != 0) {
              old_hyperparameter_list <- object@hyperparameter_list

              #Take only those that have no substitute in new hyperparameters
              if(any(!names(old_hyperparameter_list) %in% hyperparameter)){
                old_hyperparameter_list_disjoint <- old_hyperparameter_list[which(!names(old_hyperparameter_list) %in% hyperparameter)] #Info
                old_hyperparameter_list_disjoint_names <- names(old_hyperparameter_list)[which(!names(old_hyperparameter_list) %in% hyperparameter)] #Names

                #Re-combine
                for(hyper in old_hyperparameter_list_disjoint_names){
                  new_hyperparameter_list[hyper] <- old_hyperparameter_list[hyper]
                }

                #Re-order
                new_hyperparameter_list <- new_hyperparameter_list[c(old_hyperparameter_list_disjoint_names, hyperparameter)]
              }
            }

            # Update the object
            object@hyperparameter_list <- new_hyperparameter_list

            # Validate the object explicitly
            validObject(object)

            return(object)
          })


#' @describeIn add_hyperparameter Add hyperparameter to `grid_search_strategy` object
#' @param hyperparameter A vector of characters indicating the name of the hyperparameter to be added. Options are:
#' \itemize{
#'  \item \strong{glmnet}: alpha, lambda.min.ratio
#'  \item \strong{rf}: mtry, num.trees, max.depth, min.bucket
#'  \item \strong{xgb}: min_child_weight, max_depth, subsample, colsample_bytree, eta, gamma, nrounds
#'  \item \strong{nn}: regularizer_l1, regularizer_l2, droprate, lr, size_of_batch, number_of_epochs
#' }
#' @param grid A numeric vector or list of numeric vectors for grid search values.
#' @export
setMethod("add_hyperparameter",
          signature(object = "grid_search_strategy"),
          function(object, hyperparameter, grid, ...) {

            if (!is.list(grid)) {
              grid <- list(grid)
            }

            if (!all(sapply(grid, function(x) is.numeric(x) && is.vector(x)))) {
              stop("Grid should contain only numeric data.")
            }

            if (length(hyperparameter) != length(grid)) {
              stop("All hyperparameters should have a grid.")
            }

            new_hyperparameter_list <- grid
            names(new_hyperparameter_list) <- hyperparameter


            #Extract the current object
            current_hyper_grid_domain <- object@hyper_grid_domain
            updated_hyper_grid_domain <- add_hyperparameter(current_hyper_grid_domain, new_hyperparameter_list = new_hyperparameter_list)

            # Update the object
            object@hyper_grid_domain <- updated_hyper_grid_domain

            # Validate the object explicitly
            validObject(object)

            return(object)
          })



#' @describeIn add_hyperparameter Add hyperparameter to `random_search_strategy` object
#' @param hyperparameter A vector of characters indicating the name of the hyperparameter to be added. Options are:
#' \itemize{
#'  \item \strong{glmnet}: alpha, lambda.min.ratio
#'  \item \strong{rf}: mtry, num.trees, max.depth, min.bucket
#'  \item \strong{xgb}: min_child_weight, max_depth, subsample, colsample_bytree, eta, gamma, nrounds
#'  \item \strong{nn}: regularizer_l1, regularizer_l2, droprate, lr, size_of_batch, number_of_epochs
#' }
#' @param distribution_choice A character vector indicating the distribution to sample from (only used for random_search).
#' @param pars A numeric named vector or list of numeric named vectors specifying parameter values (only used for random_search).
#' @export
setMethod("add_hyperparameter",
          signature(object = "random_search_strategy"),
          function(object, hyperparameter, distribution_choice, pars, ...) {

            # Logic for random_search
            if (!is.list(distribution_choice)) {
              distribution_choice <- as.list(distribution_choice) #As list
            }
            if (!is.list(pars)) {
              pars <- list(pars) #list
            }

            if (length(hyperparameter) != length(distribution_choice) || length(hyperparameter) != length(pars)) {
              stop("All hyperparameters should have matching distribution_choice and pars.")
            }

            valid_distributions <- c("uniform", "normal", "lognormal", "constant")
            if (!all(distribution_choice %in% valid_distributions)) {
              stop("Invalid distribution_choice. Choose from 'uniform', 'normal', 'lognormal', or 'constant'.")
            }

            for (i in seq_along(distribution_choice)) {
              dist <- distribution_choice[[i]]
              param <- pars[[i]]

              if (dist == "uniform" && (!all(c("min", "max") %in% names(param)))) {
                stop("For 'uniform', pars must contain 'min' and 'max'.")
              }
              if (dist == "normal" && (!all(c("mean", "sd") %in% names(param)))) {
                stop("For 'normal', pars must contain 'mean' and 'sd'.")
              }
              if (dist == "lognormal" && (!all(c("meanlog", "sdlog") %in% names(param)))) {
                stop("For 'lognormal', pars must contain 'meanlog' and 'sdlog'.")
              }
            }

            new_hyperparameter_list <- mapply(function(hp, dist, param) {
              if (dist == "constant") {
                list(distribution_choice = dist, value = unname(param))
              } else {
                list(distribution_choice = dist, pars = param)
              }
            }, hyperparameter, distribution_choice, pars, SIMPLIFY = FALSE)

            names(new_hyperparameter_list) <- hyperparameter


            #Extract the current object
            current_hyper_grid_domain <- object@hyper_grid_domain
            updated_hyper_grid_domain <- add_hyperparameter(current_hyper_grid_domain,
                                                            new_hyperparameter_list = new_hyperparameter_list)

            # Update the object
            object@hyper_grid_domain <- updated_hyper_grid_domain

            # Validate the object explicitly
            validObject(object)

            return(object)
          })


#' @describeIn add_hyperparameter Add hyperparameter to `bayesian_opt_strategy` object
#' @param hyperparameter A vector of characters indicating the name of the hyperparameter to be added. Options are:
#' \itemize{
#'  \item \strong{glmnet}: alpha, lambda.min.ratio
#'  \item \strong{rf}: mtry, num.trees, max.depth, min.bucket
#'  \item \strong{xgb}: min_child_weight, max_depth, subsample, colsample_bytree, eta, gamma, nrounds
#'  \item \strong{nn}: regularizer_l1, regularizer_l2, droprate, lr, size_of_batch, number_of_epochs
#' }
#' @param distribution_choice A character vector indicating the distribution to sample from (only used for random_search).
#' @param pars A numeric named vector or list of numeric named vectors specifying parameter values (only used for random_search).
#' @export
setMethod("add_hyperparameter",
          signature(object = "bayesian_opt_strategy"),
          function(object, hyperparameter, bounds, ...) {


            # Logic for bayesian_opt
            if (!is.list(bounds)) {
              bounds <- list(bounds)
            }

            if (!all(sapply(bounds, function(x) is.numeric(x) && length(x) == 2))) {
              stop("Each hyperparameter must have a bounds vector of length 2 (min and max).")
            }

            if (length(hyperparameter) != length(bounds)) {
              stop("All hyperparameters should have corresponding bounds.")
            }

            new_hyperparameter_list <- bounds
            names(new_hyperparameter_list) <- hyperparameter



            #Extract the current object
            current_hyper_grid_domain <- object@hyper_grid_domain
            updated_hyper_grid_domain <- add_hyperparameter(current_hyper_grid_domain, new_hyperparameter_list = new_hyperparameter_list)

            # Update the object
            object@hyper_grid_domain <- updated_hyper_grid_domain

            # Validate the object explicitly
            validObject(object)

            return(object)
          })

#' @describeIn add_hyperparameter Add Hyperparameter to `sb_backtest_config` object
#' @param hyperparameter A vector of characters indicating the name of the hyperparameter to be added.
#' @param grid A numeric vector or list of numeric vectors for grid search values (only used for grid_search).
#' @param distribution_choice A character vector indicating the distribution to sample from (only used for random_search).
#' @param pars A numeric named vector or list of numeric named vectors specifying parameter values (only used for random_search).
#' @param bounds A vector of length 2 indicating minimum and maximum bounds for each hyperparameter (only used for bayesian_opt).
#' @export
setMethod("add_hyperparameter",
          signature(object = "sb_backtest_config"),
          function(object, hyperparameter, grid = NULL, distribution_choice = NULL, pars = NULL, bounds = NULL) {

            #Extract object
            tuning_strategy <- object@tuning_strategy


            #Add hyperparamete
            updated_tuning_strategy <- add_hyperparameter(tuning_strategy, hyperparameter = hyperparameter,
                                                          grid = grid, distribution_choice = distribution_choice, pars = pars, bounds = bounds)

            # Update the object
            object@tuning_strategy <- updated_tuning_strategy

            # Validate the object explicitly
            validObject(object)


            return(object)
          })


#' Add a `hyper_grid_domain` Object
#'
#' This function adds a `hyper_grid_domain` S4 class to a `tuning_strategy` or a `sb_backtest_config`.
#' It allows users to add a `hyper_grid_domain` already built or extracted from other objects.
#'
#' @param object An object of class `tuning_strategy` or a `sb_backtest_config`.
#' @param hyper_grid_domain An object of class `hyper_grid_domain`.
#'
#' @return The appropriate object with the added `hyper_grid_domain`.
#'
#' @export
setGeneric("add_hyper_grid_domain", function(object, hyper_grid_domain) {
  standardGeneric("add_hyper_grid_domain")
})


#' @describeIn add_hyper_grid_domain Add `hyper_grid_domain` to `tuning_strategy` object
#' @param object An object of class `tuning_strategy`.
#' @param hyper_grid_domain An object of class `hyper_grid_domain`.
#' @export
setMethod("add_hyper_grid_domain",
          signature(object = "tuning_strategy", hyper_grid_domain = "hyper_grid_domain"),
          function(object, hyper_grid_domain) {

            #Add hyper_grid_domain
            object@hyper_grid_domain <- hyper_grid_domain

            # Validate the object explicitly
            validObject(object)

            return(object)
          })


#' @describeIn add_hyper_grid_domain Add `hyper_grid_domain` to `sb_backtest_config` object
#' @param object An object of class `sb_backtest_config`.
#' @param hyper_grid_domain An object of class `hyper_grid_domain`.
#' @export
setMethod("add_hyper_grid_domain",
          signature(object = "sb_backtest_config", hyper_grid_domain = "hyper_grid_domain"),
          function(object, hyper_grid_domain) {

            #Add hyper_grid_domain
            object@tuning_strategy@hyper_grid_domain <- hyper_grid_domain

            # Validate the object explicitly
            validObject(object)

            return(object)
          })


#-----------------------------------------------------------------------
# keras_architecture
#-----------------------------------------------------------------------

#' @title Create Keras Architecture
#' @description Constructor for creating an instance of `keras_architecture_parameters`.
#'
#' @param nn_optimizer A character string specifying the optimizer to use (e.g., "adam").
#' @param units A numeric value for the number of units in the new layer.
#' @param activation A character string specifying the activation function for the new layer (e.g., "relu").
#' @param batch_norm_option A character string indicating whether to apply batch normalization for the new layer (e.g., "yes").
#'
#' @return An object of class `keras_architecture_parameters`.
#'
#' @export
create_keras_architecture <- function(nn_optimizer, units = NULL, activation = NULL, batch_norm_option = NULL) {

  # Check nn_optimizer is valid
  valid_optimizers <- c("Adam", "RMSProp")
  if (!nn_optimizer %in% valid_optimizers) {
    stop("nn_optimizer should be Adam or RMSProp.")
  }

  #Check format
  if(!is.numeric(units)){
    stop("units should be a numeric value.")
  }
  if(!all(activation %in% c("relu", "sigmoid", "tanh", "softmax"))){
    stop("activation should be relu, sigmoid, tanh, or softmax.")
  }
  if(!all(is.logical(batch_norm_option))){
    stop("batch_norm_option should be a logical value.")
  }

  #Create new_keras_architecture
  new_keras_architecture_parameters <-
    new("keras_architecture_parameters",
        units = units,
        n_layers = length(units),
        activation = activation,
        nn_optimizer = nn_optimizer,
        batch_norm_option = batch_norm_option
    )


  return(new_keras_architecture_parameters)

}


#' @title Add Layer to Keras Architecture
#' @description Method to add a new layer to the Keras architecture.
#'
#' @param object An object of class `keras_architecture_parameters` or `sb_backtest_config`
#' @param units A numeric value for the number of units in the new layer.
#' @param activation A character string specifying the activation function for the new layer (e.g., "relu").
#' @param batch_norm_option A character string indicating whether to apply batch normalization for the new layer (e.g., "yes").
#'
#' @return An updated object of class `keras_architecture_parameters`.
#'
#' @export
setGeneric("add_keras_layer", function(object, units, activation, batch_norm_option) {
  standardGeneric("add_keras_layer")
})

#' @describeIn add_keras_layer Add a keras layer to an object of class `keras_architecture_parameters`
#' @param object An object of class `keras_architecture_parameters`
#' @export
setMethod(
  "add_keras_layer", "keras_architecture_parameters", function(object, units, activation, batch_norm_option) {

    # Check that units are numeric
    if (!all(is.numeric(units))) {
      stop("units should be numeric")
    }

    # Check activation functions
    valid_activations <- c("relu", "sigmoid", "softmax", "softplus", "tanh", "leaky_relu")
    if (!all(activation %in% valid_activations)) {
      stop("activation should be one of relu, sigmoid, softmax, softplus, tanh or leaky_relu.")
    }

    # Check batch_norm_option is logical
    if (!all(batch_norm_option %in% c(TRUE, FALSE))) {
      stop("batch_norm_option should be logical (TRUE or FALSE)")
    }

    #Check length
    if(length(units) != length(activation) || length(units) != length(batch_norm_option)){
      stop("units, activation and batch_norm_option should have matching length.")
    }

    # Update the layers
    object@units <- c(object@units, units)
    object@n_layers <- length(object@units)  # Update the number of layers
    object@activation <- c(object@activation, activation)
    object@batch_norm_option <- c(object@batch_norm_option, batch_norm_option)

    if(length(object@units) > 5){
      warning("factoRverse only supports up to 5 layers currently")
    }

    return(object)  # Return the updated object
  }
)


#' @describeIn add_keras_layer Add a keras layer to an object of class `sb_backtest_config`
#' @param object An object of class `sb_backtest_config`
#' @export
setMethod(
  "add_keras_layer", "sb_backtest_config", function(object, units, activation, batch_norm_option) {

    object <- add_keras_layer(object@keras_architecture_parameters, units = units, activation = activation, batch_norm_option)

    return(object)  # Return the updated object
  }
)


#' @title Add Keras Architecture
#' @description Method to add a `keras_architecture_parameters` to a `sb_backtest_config`.
#'
#' This function allows you to either directly add a pre-existing `keras_architecture_parameters` object or create one dynamically by passing additional arguments.
#' When `keras_architecture_parameters` is not provided, a new one will be created using the values for `nn_optimizer`, `units`, `activation`, and `batch_norm_option` passed via the `...` argument.
#'
#' @param object An object of class `sb_backtest_config`.
#' @param keras_architecture_parameters An object of class `keras_architecture_parameters` or missing if a new architecture is to be created.
#' @param ... Additional arguments used to create a new `keras_architecture_parameters` when `keras_architecture_parameters` is missing. These arguments must include:
#'   \itemize{
#'     \item \strong{nn_optimizer}: A character string specifying the optimizer to use (e.g., "adam").
#'     \item \strong{units}: A numeric value for the number of units in the new layer.
#'     \item \strong{activation}: A character string specifying the activation function for the new layer (e.g., "relu").
#'     \item \strong{batch_norm_option}: A character string indicating whether to apply batch normalization for the new layer (e.g., "yes").
#'   }
#'
#' @return An updated object of class `sb_backtest_config` with the `keras_architecture_parameters` added.
#' @export
setGeneric("add_keras_architecture", function(object, keras_architecture_parameters, ...) {
  standardGeneric("add_keras_architecture")
})

#' @describeIn add_keras_architecture Add existing `keras_architecture_parameters` object
#'
#' This method allows you to add an already existing `keras_architecture_parameters` object to an `sb_backtest_config`.
#'
#' @param object An object of class `sb_backtest_config`.
#' @param keras_architecture_parameters An existing object of class `keras_architecture_parameters`.
#' @return An updated `sb_backtest_config` object with the provided `keras_architecture_parameters`.
#' @export
setMethod(
  "add_keras_architecture",
  signature(object = "sb_backtest_config", keras_architecture_parameters = "keras_architecture_parameters"),
  function(object, keras_architecture_parameters) {

    object@keras_architecture_parameters <- keras_architecture_parameters

    return(object)  # Return the updated object
  }
)

#' @describeIn add_keras_architecture Dynamically create and add `keras_architecture_parameters` object
#'
#' This method creates a new `keras_architecture_parameters` object dynamically when `keras_architecture_parameters` is not provided.
#' The parameters required to create this object must be passed via `...` and include:
#'   \itemize{
#'     \item \strong{nn_optimizer}: A character string specifying the optimizer to use (e.g., "adam").
#'     \item \strong{units}: A numeric value for the number of units in the new layer.
#'     \item \strong{activation}: A character string specifying the activation function for the new layer (e.g., "relu").
#'     \item \strong{batch_norm_option}: A character string indicating whether to apply batch normalization for the new layer (e.g., "yes").
#'   }
#'
#' @param object An object of class `sb_backtest_config`.
#' @param keras_architecture_parameters Should be missing to dynamically create a new architecture.
#' @param ... Additional parameters used to create the `keras_architecture_parameters`, including:
#'   \itemize{
#'     \item \strong{nn_optimizer}: A character string specifying the optimizer to use (e.g., "adam").
#'     \item \strong{units}: A numeric value for the number of units in the new layer.
#'     \item \strong{activation}: A character string specifying the activation function for the new layer (e.g., "relu").
#'     \item \strong{batch_norm_option}: A character string indicating whether to apply batch normalization for the new layer (e.g., "yes").
#'   }
#' @return An updated `sb_backtest_config` object with the newly created `keras_architecture_parameters`.
#' @export
setMethod(
  "add_keras_architecture",
  signature(object = "sb_backtest_config", keras_architecture_parameters = "missing"),
  function(object, keras_architecture_parameters = NULL, ...) {

    #Extract args to build keras
    args <- list(...)

    # Ensure all required parameters are present
    if (!all(c("nn_optimizer", "units", "activation", "batch_norm_option") %in% names(args))) {
      stop("All required parameters (nn_optimizer, units, activation, batch_norm_option) must be provided.")
    }

    # Create the keras architecture parameters using the additional arguments
    keras_architecture_parameters <- create_keras_architecture(nn_optimizer = args$nn_optimizer,
                                                               units = args$units, activation = args$activation, batch_norm_option = args$batch_norm_option)

    # Assign the created keras architecture to the object
    object@keras_architecture_parameters <- keras_architecture_parameters

    return(object)  # Return the updated object
  }
)

#-----------------------------------------------------------------------
# cov_est_method
#-----------------------------------------------------------------------

#' @title Create Covariance Estimation Method
#' @description Constructor for creating an instance of `cov_est_method`.
#'
#' @param cov_estimation_method A character string representing the covariance estimation method. Must be one of 'sample', 'ewma', 'cc', 'pca1', 'pca2', 'shrink_id' or 'shrink_cc'.
#' @param cov_matrix_sample_size Number of periods to subset return sample when estimating the covariance matrix. A high number will provide
#' higher degrees of freedom, but old returns might not reflect current risk due to parameter shift. A low number will tend to expose estimation
#' to dimensionality curse.
#' @param active_returns logical. If TRUE, the covariance matrix will be estimated using active returns. If FALSE, the covariance matrix will be estimated using raw returns.
create_cov_est_method <- function(cov_estimation_method = "sample", cov_matrix_sample_size, active_returns = TRUE, cov_matrix_benchmark = NULL) {

  cov_est_method <- new("cov_est_method",
                        cov_estimation_method = cov_estimation_method,
                        cov_matrix_sample_size = cov_matrix_sample_size,
                        active_returns = active_returns,
                        cov_matrix_benchmark = cov_matrix_benchmark
                        )

}

#' @title Add a cov_est_method to a `sb_backtest_config` or `port_backtest_config` object.
#'
#' This function allows either directly add a pre-existing `cov_est_method` object or create one dynamically by passing additional arguments.
#' When `cov_est_method` is not provided, a new one will be created using the values for `cov_estimation_method`, `cov_matrix_sample_size`, `active_returns`, passed via the `...` argument.
#'
#' @param object An object of class `sb_backtest_config` or `port_backtest_config`.
#' @param cov_est_method An object of class `cov_est_method`, or missing if a new object is to be created.
#' @param ... Additional arguments used to create a new `cov_est_method` when `cov_est_method` is missing. These arguments must include:
#'   \itemize{
#'     \item \strong{cov_estimation_method}: A character string representing the covariance estimation method. Must be one of 'sample', 'ewma', 'cc', 'pca1', 'pca2', 'shrink_id' or 'shrink_cc'.
#'     \item \strong{cov_matrix_sample_size}: Number of periods to subset return sample when estimating the covariance matrix. A high number will provide
#' higher degrees of freedom, but old returns might not reflect current risk due to parameter shift. A low number will tend to expose estimation
#' to dimensionality curse.
#'     \item \strong{active_returns}: logical. If TRUE, the covariance matrix will be estimated using active returns. If FALSE, the covariance matrix will be estimated using raw returns.
#'   }
#'
#' @return An updated object of class `sb_backtest_config` with the `cov_est_method` added.
#' @export
setGeneric("add_cov_est_method", function(object, cov_est_method, ...) {
  standardGeneric("add_cov_est_method")
})

#' @describeIn add_cov_est_method Add existing `cov_est_method` object to a `sb_backtest_config` object.
#'
#' This method allows to add an already existing `cov_est_method` object to an `sb_backtest_config`.
#'
#' @param object An object of class `sb_backtest_config`.
#' @param cov_est_method An existing object of class `cov_est_method`.
#' @export
setMethod("add_cov_est_method", signature(object = "sb_backtest_config", cov_est_method = "cov_est_method"),
          function(object, cov_est_method, ...) {

            object@signal_port_parameters@cov_est_method <- cov_est_method

            return(object)
          }
)

#' @describeIn add_cov_est_method Create a `cov_est_method` object to a `sb_backtest_config` object.
#'
#' This method allows to dynamically create a `cov_est_method` object and add to `sb_backtest_config`.
#'
#' @param object An object of class `sb_backtest_config`.
#' @param cov_est_method An existing object of class `cov_est_method`.
#' @export
setMethod("add_cov_est_method", signature(object = "sb_backtest_config", cov_est_method = "missing"),
          function(object, cov_est_method, cov_estimation_method = "sample", cov_matrix_sample_size = 36, active_returns = TRUE, cov_matrix_benchmark = NULL ...) {

            object@signal_port_parameters@cov_est_method <- create_cov_est_method(cov_estimation_method = cov_estimation_method,
                                                                                  cov_matrix_sample_size = cov_matrix_sample_size,
                                                                                  active_returns = active_returns,
                                                                                  cov_matrix_benchmark = cov_matrix_benchmark
                                                                                  )

            return(object)
          }
)



#' @describeIn add_cov_est_method Add existing `cov_est_method` object to a `port_backtest_config` object.
#'
#' This method allows to add an already existing `cov_est_method` object to an `port_backtest_config`.
#'
#' @param object An object of class `port_backtest_config`.
#' @param cov_est_method An existing object of class `cov_est_method`.
#' @export
setMethod("add_cov_est_method", signature(object = "port_backtest_config", cov_est_method = "cov_est_method"),
          function(object, cov_est_method, ...) {

            object@cov_est_method <- cov_est_method

            return(object)
          }
)

#' @describeIn add_cov_est_method Create a `cov_est_method` object to a `port_backtest_config` object.
#'
#' This method allows to dynamically create a `cov_est_method` object and add to `port_backtest_config`.
#'
#' @param object An object of class `port_backtest_config`.
#' @param cov_est_method An existing object of class `cov_est_method`.
#' @export
setMethod("add_cov_est_method", signature(object = "port_backtest_config", cov_est_method = "missing"),
          function(object, cov_est_method, cov_estimation_method = "sample", cov_matrix_sample_size = 252, active_returns = TRUE, cov_matrix_benchmark = NULL, ...) {

            object@cov_est_method <- create_cov_est_method(cov_estimation_method = cov_estimation_method,
                                                           cov_matrix_sample_size = cov_matrix_sample_size,
                                                           active_returns = active_returns,
                                                           cov_matrix_benchmark = cov_matrix_benchmark
            )

            return(object)
          }
)


#-----------------------------------------------------------------------
# mvo_parameters
#-----------------------------------------------------------------------

#' @title Create MVO Parameters
#' @description Constructor function for creating an instance of `mvo_parameters`.
#'
#' @param opt_method A character indicating the optimization method.
#'   The only current available method is 'random'. In this case, \code{n_random_ports} are
#'   generated under the constraints defined in the `mvo_parameters` object and the one that
#'   optimizes the \code{opt_objective} will be selected.
#' @param random_ports_method A character string representing the method that will be
#'   passed to \code{PortfolioAnalytics::random_portfolios} to generate random portfolios.
#'   Options are 'sample', 'simplex' or 'grid'.
#' @param n_random_ports Number of random portfolios to generate. Only needed when
#'   \code{opt_method} is 'random'.
#' @param opt_objective A character indicating the optimization objective. Possible options
#'   are 'return', 'risk' or 'sharpe'.
#'
#' @return An S4 object of class `mvo_parameters`.
#' @export
#'
#' @examples
#' # Create an `mvo_parameters` object with default values:
#' mvo_params_default <- create_mvo_parameters()
#'
#' # Create an `mvo_parameters` object with custom values:
#' mvo_params_custom <- create_mvo_parameters(
#'   opt_method = "random",
#'   random_ports_method = "grid",
#'   n_random_ports = 500,
#'   opt_objective = "risk"
#' )
create_mvo_parameters <- function(opt_method = "random",
                                  random_ports_method = "sample",
                                  n_random_ports = 1000,
                                  opt_objective = "sharpe") {

  mvo_params <- methods::new("mvo_parameters",
                             opt_method = opt_method,
                             random_ports_method = random_ports_method,
                             n_random_ports = n_random_ports,
                             opt_objective = opt_objective)
  return(mvo_params)
}


#' @title Add mvo_parameters to a backtest config
#'
#' @description
#' This function allows either directly adding a pre-existing `mvo_parameters` object
#' or creating one dynamically by passing additional arguments.
#'
#' @param object An object of class `sb_backtest_config` or `port_backtest_config`.
#' @param mvo_params An object of class `mvo_parameters`, or missing if a new object is to be created.
#' @param ... Additional arguments used to create a new `mvo_parameters` object when `mvo_params` is missing.
#'   These arguments must include:
#'   \itemize{
#'     \item \strong{opt_method}: A character indicating the optimization method.
#'           The only current available method is 'random'.
#'     \item \strong{random_ports_method}: A character string representing the method to generate random portfolios.
#'           Options are 'sample', 'simplex' or 'grid'.
#'     \item \strong{n_random_ports}: Number of random portfolios to generate.
#'           Only needed when `opt_method` is 'random'.
#'     \item \strong{opt_objective}: A character indicating the optimization objective.
#'           Possible options are 'return', 'risk' or 'sharpe'.
#'   }
#'
#' @return An updated object of class `sb_backtest_config` or `port_backtest_config` with
#'   the `mvo_parameters` added.
#' @export
setGeneric("add_mvo_parameters", function(object, mvo_params, ...) {
  standardGeneric("add_mvo_parameters")
})


#' @describeIn add_mvo_parameters Add existing `mvo_parameters` object to a `sb_backtest_config` object.
#' @export
setMethod("add_mvo_parameters",
          signature(object = "sb_backtest_config", mvo_params = "mvo_parameters"),
          function(object, mvo_params, ...) {

            # Suppose you store mvo_parameters within signal_port_parameters:
            object@signal_port_parameters@mvo_parameters <- mvo_params

            return(object)
          }
)

#' @describeIn add_mvo_parameters Dynamically create a `mvo_parameters` object and add it to a `sb_backtest_config` object.
#' @export
setMethod("add_mvo_parameters",
          signature(object = "sb_backtest_config", mvo_params = "missing"),
          function(object,
                   mvo_params,
                   opt_method = "random",
                   random_ports_method = "sample",
                   n_random_ports = 1000,
                   opt_objective = "sharpe",
                   ...) {

            object@signal_port_parameters@mvo_parameters <- create_mvo_parameters(
              opt_method = opt_method,
              random_ports_method = random_ports_method,
              n_random_ports = n_random_ports,
              opt_objective = opt_objective
            )

            return(object)
          }
)



#' @describeIn add_mvo_parameters Add existing `mvo_parameters` object to a `port_backtest_config` object.
#' @export
setMethod("add_mvo_parameters",
          signature(object = "port_backtest_config", mvo_params = "mvo_parameters"),
          function(object, mvo_params, ...) {

            object@mvo_parameters <- mvo_params

            return(object)
          }
)

#' @describeIn add_mvo_parameters Dynamically create a `mvo_parameters` object and add it to a `port_backtest_config` object.
#' @export
setMethod("add_mvo_parameters",
          signature(object = "port_backtest_config", mvo_params = "missing"),
          function(object,
                   mvo_params,
                   opt_method = "random",
                   random_ports_method = "sample",
                   n_random_ports = 1000,
                   opt_objective = "sharpe",
                   ...) {

            object@mvo_parameters <- create_mvo_parameters(
              opt_method = opt_method,
              random_ports_method = random_ports_method,
              n_random_ports = n_random_ports,
              opt_objective = opt_objective
            )

            return(object)
          }
)





#-----------------------------------------------------------------------
# rp_parameters
#-----------------------------------------------------------------------

#' @title Create RP (Risk Parity) Parameters
#' @description Constructor function for creating an instance of `rp_parameters`.
#'
#' @param rp_method A character indicating the method to compute the risk-parity vanilla solution.
#'   It is passed to \code{riskParityPortfolio::riskParityPortfolio()} function as \code{method_init}.
#'   Default is \code{"cyclical-spinu"}.
#'
#' @return An S4 object of class `rp_parameters`.
#' @export
#'
#' @examples
#' # Create an `rp_parameters` object with default values:
#' rp_params_default <- create_rp_parameters()
#'
#' # Create an `rp_parameters` object with custom values:
#' rp_params_custom <- create_rp_parameters(rp_method = "gauss-seidel")
create_rp_parameters <- function(rp_method = "cyclical-spinu") {
  rp_params <- methods::new("rp_parameters",
                            rp_method = rp_method)
  return(rp_params)
}

#' @title Add rp_parameters to a backtest config
#'
#' @description
#' This function allows either directly adding a pre-existing `rp_parameters` object
#' or creating one dynamically by passing additional arguments.
#'
#' @param object An object of class `sb_backtest_config` or `port_backtest_config`.
#' @param rp_params An object of class `rp_parameters`, or missing if a new object is to be created.
#' @param ... Additional arguments used to create a new `rp_parameters` object when `rp_params` is missing.
#'   These arguments must include:
#'   \itemize{
#'     \item \strong{rp_method}: A character indicating the method to compute the risk-parity solution.
#'   }
#'
#' @return An updated object of class `sb_backtest_config` or `port_backtest_config` with
#'   the `rp_parameters` added.
#' @export
setGeneric("add_rp_parameters", function(object, rp_params, ...) {
  standardGeneric("add_rp_parameters")
})


#' @describeIn add_rp_parameters Add an existing `rp_parameters` object to a `sb_backtest_config` object.
#' @export
setMethod("add_rp_parameters",
          signature(object = "sb_backtest_config", rp_params = "rp_parameters"),
          function(object, rp_params, ...) {

            # Suppose you store rp_parameters within signal_port_parameters:
            object@signal_port_parameters@rp_parameters <- rp_params

            return(object)
          }
)

#' @describeIn add_rp_parameters Dynamically create a `rp_parameters` object and add it to a `sb_backtest_config` object.
#' @export
setMethod("add_rp_parameters",
          signature(object = "sb_backtest_config", rp_params = "missing"),
          function(object,
                   rp_params,
                   rp_method = "cyclical-spinu",
                   ...) {

            object@signal_port_parameters@rp_parameters <- create_rp_parameters(
              rp_method = rp_method
            )

            return(object)
          }
)

#' @describeIn add_rp_parameters Add an existing `rp_parameters` object to a `port_backtest_config` object.
#' @export
setMethod("add_rp_parameters",
          signature(object = "port_backtest_config", rp_params = "rp_parameters"),
          function(object, rp_params, ...) {

            object@rp_parameters <- rp_params

            return(object)
          }
)

#' @describeIn add_rp_parameters Dynamically create a `rp_parameters` object and add it to a `port_backtest_config` object.
#' @export
setMethod("add_rp_parameters",
          signature(object = "port_backtest_config", rp_params = "missing"),
          function(object,
                   rp_params,
                   rp_method = "cyclical-spinu",
                   ...) {

            object@rp_parameters <- create_rp_parameters(
              rp_method = rp_method
            )

            return(object)
          }
)





#-----------------------------------------------------------------------
# sb_backtest
#-----------------------------------------------------------------------


#' @title Create sb_backtest_config Object
#' @description Constructs an sb_backtest_config object.
#'
#' @param sb_algorithm Character string specifying the machine learning algorithm to be used ('glmnet', 'rf', 'xgb', 'nn', 'sw', 'ew', etc.).
#' @param target_fwd_name Name of the target variable in `target_m_df`.
#' @param training_sample_size Number of observations to include in each training sample.
#' @param rebalancing_months Months (numeric) when model should be rebalanced (refit).
#' @param split_method Character string indicating the data splitting method ('expanding' or 'rolling').
#' @param tuning_strategy An object of class tuning_strategy, specifying the strategy for tuning hyperparameters.
#' @param ss_backtest_config An object of class `ss_backtest_config`, specifying the single strategy backtest configuration.
#' @param ss_backtest_results An object of class `ss_backtest_results`, containing the results of the single strategy backtest.
#' @param custom_objective Character string specifying the custom objective function ('squared_error', 'pseudo_huber_error', 'absolute_error') or NULL.
#' @param keras_architecture_parameters An object of class `keras_architecture_parameters` providing parameters specific to keras-based neural networks.
#' @param signal_port_parameters An object of class `signal_port_parameters`, specifying the parameters for constructing signal portfolios (portfolio-blending).
#' @param quantile_tau Numeric value indicating the tau parameter used for quantile regression, between 0 and 1.
#' @param huber_delta Numeric value greater than 0, specifying the delta parameter for Huber loss function.
#' @param config_name Name of the backtest configuration.
#'
#' @return An sb_backtest_config object.
#' @export
create_sb_backtest_config <- function(sb_algorithm = "ols", target_fwd_name, tuning_strategy = NULL, training_sample_size, rebalancing_months, split_method = "expanding",
                                      ss_backtest_config = NULL, ss_backtest_results = NULL,
                                      custom_objective = "squared_error", keras_architecture_parameters = NULL, signal_port_parameters = NULL, quantile_tau = 0.5, huber_delta = 1,
                                      config_name = "not_identified") {

  ##Give custom warning related to quantile tau and huber delta
  if (!is.null(quantile_tau) && quantile_tau != 0.5) {
    message("changing quantile_tau impacts both chosen_eval_metric and custom_objective.")
  }
  if (!is.null(huber_delta) && huber_delta != 1) {
    message("changing huber_delta impacts both chosen_eval_metric and custom_objective.")
  }

  #Create default parameters for signal_port_parameters depending on sb_algo
  if(sb_algorithm %in% c("mvo", "rp") && is.null(signal_port_parameters)){
    cov_est_method <- create_cov_est_method(cov_estimation_method = "sample", cov_matrix_sample_size = 36, active_returns = TRUE)
    mvo_parameters <- if(sb_algorithm == "mvo") create_mvo_parameters(opt_method = "random", random_ports_method = "sample", n_random_ports = 1000, opt_objective = "sharpe") else NULL
    rp_parameters <- if(sb_algorithm == "rp") create_rp_parameters(rp_method = "cyclical-spinu") else NULL


    signal_port_parameters <- new("signal_port_parameters",
                                  cov_est_method = cov_est_method,
                                  mvo_parameters = mvo_parameters,
                                  rp_parameters = rp_parameters,
                                  concentration_constraint_policy = NULL)
  }

  # Create the sb_backtest_config object
  new("sb_backtest_config",
      sb_algorithm = sb_algorithm,
      target_fwd_name = target_fwd_name,
      training_sample_size = training_sample_size,
      rebalancing_months = rebalancing_months,
      split_method = split_method,
      ss_backtest_config = ss_backtest_config,
      ss_backtest_results = ss_backtest_results,
      tuning_strategy = tuning_strategy,
      custom_objective = custom_objective,
      keras_architecture_parameters = keras_architecture_parameters,
      signal_port_parameters = signal_port_parameters,
      quantile_tau = quantile_tau,
      huber_delta = huber_delta,
      config_name = config_name
  )
}



#' Create SB Meta Backtest Configuration
#'
#' The `create_sb_metabacktest_config` function creates an `sb_metabacktest_config` object by combining a `sb_backtest_config` for the meta-learner
#' and a group of `sb_backtest_config` objects for the base learners.
#' Those can be passed with our without a `ss_backtest_config` set. In this latter case, by passing
#' a list of `ss_backtest_config` objects, the function will generate all possible combinations of configurations for running multiple backtests,
#' possibly in parallel.
#' Alternatively, the user can provide a list of `sb_backtest_results` directly to be used by the function.
#'
#' @param meta_sb_backtest_config A `sb_backtest_config` with the configuration for the meta learner.
#' @param base_sb_backtest_configs A list of `sb_backtest_config` objects with `tuning_strategy` set to `NULL`.
#' @param base_sb_backtest_results A list of `sb_backtest_results` objects.
#' @param ss_backtest_configs A list of `ss_backtest_config` objects (optional).
#' @param ss_backtest_results A list of `ss_backtest_results` objects (optional)
#' @param config_name Name of the backtest configuration.
#' @param ... Additional arguments (not used).
#'
#' @return A `sb_metabacktest_config` object containing all viable combinations of configs.
#'
#' @examples
#' # Example usage:
#' # Assuming you have sb_backtest_config objects sb_config1, sb_config2
#' # and ss_backtest_config objects ss_config1, ss_config2
#'
#' # First method: Combine sb_configs and ss_configs
#' meta_config <- create_sb_metabacktest_config(
#'     meta_sb_backtest_config = sb_backtest_config,
#'     base_sb_backtest_configs = list(sb_config1, sb_config2),
#'     ss_backtest_configs = list(ss_config1, ss_config2)
#' )
#'
#' # Second method: Configs already have ss_backtest_config set
#' meta_config <- create_sb_metabacktest_config(
#'     meta_sb_backtest_config = sb_backtest_config,
#'     base_sb_backtest_configs = list(config1, config2)
#' )
#'
#' @seealso \code{\link{sb_backtest_config}}, \code{\link{ss_backtest_config}}, \code{\link{sb_metabacktest_config}}
#'
#' @export
setGeneric("create_sb_metabacktest_config", function(meta_sb_backtest_config, base_sb_backtest_configs, base_sb_backtest_results,
                                                     ss_backtest_configs, ss_backtest_results, ...) standardGeneric("create_sb_metabacktest_config"))


#' @describeIn create_sb_metabacktest_config Combine ss_backtest_configs and ss_backtest_configs
#'
#' This method accepts one or multiple `sb_backtest_config` and one or multiple `ss_backtest_config` objects.
#' It combines all possible configurations between the configs and strategies by using the `add_ss_backtest` method.
#'
#' @param meta_sb_backtest_config A `sb_backtest_config` with the configuration for the meta learner.
#' @param base_sb_backtest_configs A list of `sb_backtest_config` objects.
#' @param config_name Name of the backtest configuration.
#' @param ss_backtest_configs A list of `ss_backtest_config` objects.
#' @param ... Additional arguments (not used).
#'
#' @return An `sb_metabacktest_config` object containing all combinations of sb_configs and ss_configs
#'
#' @examples
#' # Assuming you have sb_backtest_config objects config1, config2 (with ss_backtest_config = NULL and ss_backtest_results = NULL)
#' # and tuning_strategy objects strategy1, strategy2
#' meta_config <- create_sb_metabacktest_config(
#'     sb_configs = list(sb_config1, sb_config2),
#'     ss_configs = list(ss_config1, ss_config2)
#' )
#'
#' @export
setMethod("create_sb_metabacktest_config",
          signature(meta_sb_backtest_config = "sb_backtest_config", base_sb_backtest_configs = "list", base_sb_backtest_results = "missing",
                    ss_backtest_configs = "list", ss_backtest_results = "missing"),
          function(meta_sb_backtest_config, base_sb_backtest_configs, ss_backtest_configs, config_name = "not_identified", ...) {

            # Check that all base_sb_backtest_configs are sb_backtest_config objects
            if (!all(sapply(base_sb_backtest_configs, function(x) is(x, "sb_backtest_config")))) {
              stop("All elements in 'base_sb_backtest_configs' must be 'sb_backtest_config' objects.")
            }

            # Check that all ss_backtest_config are ss_backtest_config objects
            if (!all(sapply(ss_backtest_configs, function(x) is(x, "ss_backtest_config")))) {
              stop("All elements in 'ss_backtest_configs' must be 'ss_backtest_config' objects.")
            }

            # Get config names
            sb_config_names <- as.character(sapply(base_sb_backtest_configs, function(x) x@config_name))
            ss_config_names <- as.character(sapply(ss_backtest_configs, function(x) x@config_name))

            combined_configs <- list()


            # Iterate through each sb and ss configurations:
            for (i in seq_along(base_sb_backtest_configs)) {
              sb_config <- base_sb_backtest_configs[[i]]

              for (j in seq_along(ss_backtest_configs)) {
                ss_config <- ss_backtest_configs[[j]]

                # Add the ss_backtest_config
                new_config <- add_ss_backtest_obj(sb_config, ss_config)

                # Add it to combined_configs
                  combined_name <- paste0(sb_config_names[i], "_", ss_config_names[j])
                  combined_configs[[combined_name]] <- new_config

              }
            }

           # Create the sb_metabacktest_config object
            meta_config <- new("sb_metabacktest_config", meta_sb_backtest_config = meta_sb_backtest_config,
                               base_sb_backtest_configs = combined_configs,
                               base_sb_backtest_results = NULL, config_name = config_name)

            # State the number of valid configurations produced
            cat(sprintf("Created %d valid configurations.\n", length(combined_configs)))

            return(meta_config)
          }
)

#' @describeIn create_sb_metabacktest_config Combine ss_backtest_configs and ss_backtest_results
#'
#' This method accepts one or multiple `sb_backtest_config` and one or multiple `ss_backtest_config` objects.
#' It combines all possible configurations between the configs and strategies by using the `add_ss_backtest` method.
#'
#' @param meta_sb_backtest_config A `sb_backtest_config` with the configuration for the meta learner.
#' @param base_sb_backtest_configs A list of `sb_backtest_config` objects.
#' @param config_name Name of the backtest configuration.
#' @param ss_backtest_results A list of `ss_backtest_results` objects.
#' @param ... Additional arguments (not used).
#'
#' @return An `sb_metabacktest_config` object containing all combinations of sb_configs and ss_configs
#'
#' @examples
#' # Assuming you have sb_backtest_config objects config1, config2 (with ss_backtest_config = NULL and ss_backtest_results = NULL)
#' # and tuning_strategy objects strategy1, strategy2
#' meta_config <- create_sb_metabacktest_config(
#'     sb_configs = list(sb_config1, sb_config2),
#'     ss_configs = list(ss_config1, ss_config2)
#' )
#'
#' @export
setMethod("create_sb_metabacktest_config",
          signature(meta_sb_backtest_config = "sb_backtest_config", base_sb_backtest_configs = "list", base_sb_backtest_results = "missing",
                    ss_backtest_configs = "missing", ss_backtest_results = "list"),
          function(meta_sb_backtest_config, base_sb_backtest_configs, ss_backtest_results, config_name = "not_identified", ...) {

            # Check that all base_sb_backtest_configs are sb_backtest_config objects
            if (!all(sapply(base_sb_backtest_configs, function(x) is(x, "sb_backtest_config")))) {
              stop("All elements in 'base_sb_backtest_configs' must be 'sb_backtest_config' objects.")
            }

            # Check that all ss_backtest_config are ss_backtest_results objects
            if (!all(sapply(ss_backtest_results, function(x) is(x, "ss_backtest_results")))) {
              stop("All elements in 'ss_backtest_results' must be 'ss_backtest_results' objects.")
            }

            # Get config names
            sb_config_names <- as.character(sapply(base_sb_backtest_configs, function(x) x@config_name))
            ss_results_names <- as.character(sapply(ss_backtest_results, function(x) x@backtest_identifier))

            combined_configs <- list()


            # Iterate through each sb and ss configurations:
            for (i in seq_along(base_sb_backtest_configs)) {
              sb_config <- base_sb_backtest_configs[[i]]

              for (j in seq_along(ss_backtest_results)) {
                ss_results <- ss_backtest_results[[j]]

                # Add the ss_backtest_results
                new_config <- add_ss_backtest_obj(sb_config, ss_results)

                # Add it to combined_configs
                combined_name <- paste0(sb_config_names[i], "_", ss_results_names[j])
                combined_configs[[combined_name]] <- new_config

              }
            }

            # Create the sb_metabacktest_config object
            meta_config <- new("sb_metabacktest_config", meta_sb_backtest_config = meta_sb_backtest_config,
                               base_sb_backtest_configs = combined_configs,
                               base_sb_backtest_results = NULL, config_name = config_name)

            # State the number of valid configurations produced
            cat(sprintf("Created %d valid configurations.\n", length(combined_configs)))

            return(meta_config)
          }
)


#' @describeIn create_sb_metabacktest_config Create meta config from sb_backtest_configs
#'
#' @param meta_sb_backtest_config A `sb_backtest_config` with the configuration for the meta learner.
#' @param base_sb_backtest_configs A list of `sb_backtest_config` objects with `ss_backtest_config` or `ss_backtest_result` not `NULL`.
#' @param ... Additional arguments (not used).
#'
#' @return A `sb_metabacktest_config` object containing the provided `sb_backtest_config` objects.
#' @export
setMethod("create_sb_metabacktest_config",
          signature(meta_sb_backtest_config = 'sb_backtest_config', base_sb_backtest_configs = "list", base_sb_backtest_results = "missing",
                    ss_backtest_configs = "missing", ss_backtest_results = "missing"),
          function(meta_sb_backtest_config, base_sb_backtest_configs, config_name = "not_identified", ...) {

            # Check that all configs are sb_backtest_config objects
            if (!all(sapply(base_sb_backtest_configs, function(x) is(x, "sb_backtest_config")))) {
              stop("All elements in 'base_sb_backtest_configs' must be 'sb_backtest_config' objects.")
            }

            # Check that ss_backtest_config or ss_backtest_results is not NULL
            if(any(sapply(base_sb_backtest_configs, function(x) is.null(x@ss_backtest_config) && is.null(x@ss_backtest_results)))) {
              stop("All 'sb_backtest_config' objects must have 'ss_backtest_config' or 'ss_backtest_results' not NULL.")
            }

            # Create the sb_metabacktest_config object
            meta_config <- new("sb_metabacktest_config", meta_sb_backtest_config = meta_sb_backtest_config, base_sb_backtest_configs = base_sb_backtest_configs,
                               base_sb_backtest_results = NULL, config_name = config_name)
            return(meta_config)
          }
)

#' @describeIn create_sb_metabacktest_config Create meta config from ss_backtest_results
#'
#' @param meta_sb_backtest_config A `sb_backtest_config` with the configuration for the meta learner.
#' @param base_sb_backtest_results A list of `sb_backtest_results` objects.
#' @param ... Additional arguments (not used).
#'
#' @return An `sb_metabacktest_config` object containing the provided `sb_backtest_config` objects.
#' @export
setMethod("create_sb_metabacktest_config",
          signature(meta_sb_backtest_config = 'sb_backtest_config', base_sb_backtest_configs = "missing", base_sb_backtest_results = "list",
                    ss_backtest_configs = "missing", ss_backtest_results = "missing"),
          function(meta_sb_backtest_config, base_sb_backtest_results, config_name = "not_identified", ...) {

            # Check that all configs are sb_backtest_config objects
            if (!all(sapply(base_sb_backtest_results, function(x) is(x, "sb_backtest_results")))) {
              stop("All elements in 'base_sb_backtest_results' must be 'sb_backtest_results' objects.")
            }

            # Create the sb_metabacktest_config object
            meta_config <- new("sb_metabacktest_config", meta_sb_backtest_config = meta_sb_backtest_config, base_sb_backtest_configs = NULL,
                               base_sb_backtest_results = base_sb_backtest_results, config_name = config_name)
            return(meta_config)
          }
)

#' Add one or more sb_backtest_config objects to an sb_metabacktest_config
#'
#' @param object An `sb_metabacktest_config` object.
#' @param ... One or more `sb_backtest_config` objects to add.
#'
#' @return An updated `sb_metabacktest_config` object with added configurations.
#' @export
setGeneric("add_sb_backtest_config", function(object, ...) standardGeneric("add_sb_backtest_config"))

setMethod("add_sb_backtest_config", "sb_metabacktest_config", function(object, ...) {

  # Check that all new_configs are complete sb_backtest_config objects
  if(!all(sapply(new_configs, function(x) !is.null(x@tuning_strategy)))) {
    stop("All elements in '...' must be complete (with tuning_strategy) 'sb_backtest_config' objects.")
  }

  # Check that all new_configs are complete sb_backtest_config objects
  if(!all(sapply(new_configs, function(x) is.null(x@ss_backtest_config && is.null(x@ss_backtest_results))))) {
    stop("All elements in '...' must be complete (with ss_backtest_config or ss_backtest_results) 'sb_backtest_config' objects.")
  }

  # Combine the current base_sb_backtest_configs with the new configurations
  object@base_sb_backtest_configs <- c(object@base_sb_backtest_configs, new_configs)

  # Validate the object explicitly
  validObject(object)

  # Return the updated object
  return(object)
})



#' Remove an sb_backtest_config by name from an sb_metabacktest_config
#'
#' @param object An `sb_metabacktest_config` object.
#' @param config_name A character string specifying the name of the `sb_backtest_config` to remove.
#'
#' @return An updated `sb_metabacktest_config` object with the specified configuration removed.
#' @export
setGeneric("remove_sb_backtest_config", function(object, config_name) standardGeneric("remove_sb_backtest_config"))

setMethod("remove_sb_backtest_config", "sb_metabacktest_config", function(object, config_name) {
  # Check that config_name is provided and is a character string
  if (missing(config_name) || !is.character(config_name) || length(config_name) != 1) {
    stop("'config_name' must be a single character string specifying the configuration to remove.")
  }

  # Check if the specified config_name exists in the list
  if (!(config_name %in% sapply(object@base_sb_backtest_configs, function(x) x@config_name))) {
    stop(paste("No configuration found with the name:", config_name))
  }

  # Remove the specified configuration
  remove_index <- which(config_name %in% sapply(object@base_sb_backtest_configs, function(x) x@config_name))
  object@base_sb_backtest_configs <- object@base_sb_backtest_configs[-remove_index]

  # Validate the object explicitly
  validObject(object)

  # Return the updated object
  return(object)
})


#' @title Create an sb_metabacktest_results Object
#' @description Constructs an `sb_metabacktest_results` object from a list of `sb_backtest_results` objects for base learners a single
#' `sb_backtest_results` object for the meta learner.
#' It computes consolidated and time series evaluation metrics for machine learning backtests.
#'
#' @param meta_sb_backtest_results_list A list containing `sb_backtest_results` objects for the meta learner and for the two heuristic ensembles.
#' @param base_sb_backtest_results_list A named list of `sb_backtest_results` objects for the base learners.
#' @return An object of class `sb_metabacktest_results`.
#'
#' @export
setGeneric(
  name = "create_sb_metabacktest_results",
  def = function(meta_sb_backtest_results, base_sb_backtest_results) {
    standardGeneric("create_sb_metabacktest_results")
  }
)

#' @title Create an sb_metabacktest_results Object
#' @description Constructs an `sb_metabacktest_results` object from a list of `sb_backtest_results` objects for base learners a single
#' `sb_backtest_results` object for the meta learner.
#' It computes consolidated and time series evaluation metrics for machine learning backtests.
#'
#' @param meta_sb_backtest_results_list A list containing `sb_backtest_results` objects for the meta learner and for the two heuristic ensembles.
#' @param base_sb_backtest_results_list A named list of `sb_backtest_results` objects for the base learners.
#' @return An object of class `sb_metabacktest_results`.
#'
setGeneric(
  name = "create_sb_metabacktest_results",
  def = function(meta_sb_backtest_results_list, base_sb_backtest_results_list, ...) {
    standardGeneric("create_sb_metabacktest_results")
  }
)

#' @rdname create_sb_metabacktest_results
#' @aliases create_sb_metabacktest_results,list-method
setMethod(
  f = "create_sb_metabacktest_results",
  signature = signature(meta_sb_backtest_results_list = "list", base_sb_backtest_results_list = "list"),
  definition = function(meta_sb_backtest_results_list, base_sb_backtest_results_list, oos_predictions_m_df) {

  #Initial Checks
  ##################

    # Check that the meta_sb_backtest_results input is a list of 'sb_backtest_results' object
    if (!all(sapply(meta_sb_backtest_results_list, function(x) is(x, "sb_backtest_results")))) {
      stop("All elements in 'meta_sb_backtest_results_list' must be of class 'sb_backtest_results'")
    }
    if(length(meta_sb_backtest_results_list) != 3){
      stop("The 'meta_sb_backtest_results_list' list must contain exactly 3 elements: one for the meta learner and two for the heuristic ensembles.")
    }

    # Check that the base_sb_backtest_results input is a list of 'sb_backtest_results' objects
    if (!all(sapply(base_sb_backtest_results_list, function(x) is(x, "sb_backtest_results")))) {
      stop("All elements in 'base_sb_backtest_results_list' must be of class 'sb_backtest_results'")
    }

  ##################

  #Initialize
  ###################

    #Get ML Workflow from meta learner
    meta_learner_sb_backtest_workflow <- meta_sb_backtest_results_list[[1]]@sb_backtest_workflow

    # Get the names of the list elements
    base_sb_names <- names(base_sb_backtest_results_list)
    meta_sb_names <- names(meta_sb_backtest_results_list)

    #Consolidate all results
    all_sb_backtest_results <- c(base_sb_backtest_results_list, meta_sb_backtest_results_list)
    all_sb_names <- c(base_sb_names, meta_sb_names)

    #Common testing dates range
    common_testing_dates_range <- as.Date(Reduce(
      intersect,
      sapply(all_sb_backtest_results, function(x) x@sb_backtest_workflow$dates_testing_sample)
    ))

    # Initialize lists to collect metrics
    oos_metrics_list <- list()
    oos_metrics_common_dates_list <- list()
    validation_metrics_list <- list()

    # For time series metrics
    all_oos_metrics_long_df <- data.frame()
    all_validation_metrics_long_df <- data.frame()

    # Collect metric names
    oos_metric_names <- NULL
    validation_metric_names <- NULL

  ###################

  #Collect OOS Testing Metrics
  ##########################

    #Loop through all results
    for (i in seq_along(all_sb_backtest_results)) {
      #if(i == 3) browser()
      sb_backtest_result <- all_sb_backtest_results[[i]]
      sb_name <- all_sb_names[i]  # Use the name of the list element
      chosen_eval_metric <- sb_backtest_result@sb_backtest_workflow$chosen_eval_metric

      ## Out-of-Sample Testing Evaluation Metrics ##
      oos_testing_eval_metrics <- sb_backtest_result@oos_testing_eval_metrics

      # Exclude 'consolidated' row for time series data
      oos_metrics_time_series <- oos_testing_eval_metrics[rownames(oos_testing_eval_metrics) != "consolidated", , drop = FALSE]

      # Add Date and Backtest columns for time series data
      oos_metrics_time_series$Date <- rownames(oos_metrics_time_series)
      oos_metrics_time_series$Backtest <- sb_name  # Use sb_name instead of sb_algorithm

      # Reshape to long format for time series data
      oos_metrics_long <- as.data.frame(tidyr::pivot_longer(
        oos_metrics_time_series,  # Convert to data frame
        cols = -c(Date, Backtest),
        names_to = "Metric",
        values_to = "Value"
      ))

      # Combine with the main data frame
      all_oos_metrics_long_df <- rbind(all_oos_metrics_long_df, oos_metrics_long)

      # Get 'consolidated' row for consolidated metrics
      oos_metrics <- oos_testing_eval_metrics["consolidated", , drop = FALSE]
      oos_metrics_common_dates <-
        oos_testing_eval_metrics[which(rownames(oos_testing_eval_metrics) %in% common_testing_dates_range), , drop = FALSE]


      # Combine sb_name, chosen_eval_metric, and oos_metrics using cbind
          ##Consolidated
          oos_metrics_df <- cbind(
            data.frame(
              Backtest = sb_name,
              chosen_eval_metric = chosen_eval_metric,
              testing_dates_range = paste0(
                min(as.Date(sb_backtest_result@sb_backtest_workflow$dates_testing_sample)),
                "-",
                max(as.Date(sb_backtest_result@sb_backtest_workflow$dates_testing_sample))),
              check.names = FALSE,
              stringsAsFactors = FALSE
            ),
            as.data.frame(oos_metrics)  # Ensure it's a data frame
          )
          ##Common dates
          oos_metrics_common_dates_df <- cbind(
            data.frame(
              Backtest = sb_name,
              chosen_eval_metric = chosen_eval_metric,
              testing_dates_range = paste0(
                min(as.Date(common_testing_dates_range)),
                "-",
                max(as.Date(common_testing_dates_range))),
              check.names = FALSE,
              stringsAsFactors = FALSE
            ),
            as.data.frame(oos_metrics_common_dates)  # Ensure it's a data frame
          )



      # Remove row names from consolidated metrics
      rownames(oos_metrics_df) <- NULL
      rownames(oos_metrics_common_dates_df) <- NULL


      # Collect metric names
      oos_metric_names <- unique(c(oos_metric_names, names(oos_metrics_df)))

      # Append to the list
      oos_metrics_list[[i]] <- oos_metrics_df
      oos_metrics_common_dates_list[[i]] <- oos_metrics_common_dates_df

      ##########################

      #Collect ValMetrics
      ##########################

      ## Validation Evaluation Metrics (if available) ##
      validation_metrics <- sb_backtest_result@validation_eval_metrics_hyper_choice

      if (!is.null(validation_metrics) && nrow(validation_metrics) > 0) {
        # Exclude 'average' row for time series data
        validation_metrics_time_series <- validation_metrics[rownames(validation_metrics) != "average", , drop = FALSE]

        # Add Date and Algorithm columns for time series data
        validation_metrics_time_series$Date <- rownames(validation_metrics_time_series)
        validation_metrics_time_series$Backtest <- sb_name

        # Reshape to long format
        validation_metrics_long <- as.data.frame(tidyr::pivot_longer(
          validation_metrics_time_series,  # Convert to data frame
          cols = -c(Date, Backtest),
          names_to = "Metric",
          values_to = "Value"
        ))

        # Combine with the main data frame
        all_validation_metrics_long_df <- rbind(all_validation_metrics_long_df, validation_metrics_long)

        # Get 'average' row for consolidated validation metrics
        if ("average" %in% rownames(validation_metrics)) {
          validation_metrics_average <- validation_metrics["average", , drop = FALSE]
        } else {
          # Compute mean across rows if 'average' row is not present
          validation_metrics_average <- as.data.frame(t(colMeans(validation_metrics, na.rm = TRUE)))
        }

        # Combine sb_name, chosen_eval_metric, and validation_metrics_average using cbind
        validation_metrics_df <- cbind(
          data.frame(
            Backtest = sb_name,
            chosen_eval_metric = chosen_eval_metric,
            check.names = FALSE,
            stringsAsFactors = FALSE
          ),
          as.data.frame(validation_metrics_average)  # Ensure it's a data frame
        )

        # Remove row names from consolidated metrics
        rownames(validation_metrics_df) <- NULL

        # Collect validation metric names
        validation_metric_names <- unique(c(validation_metric_names, names(validation_metrics_df)))

        # Append to the list
        validation_metrics_list[[i]] <- validation_metrics_df
      }
    }

    ##########################

    #Adjust objects
    ###########################

    # Combine lists into data.frames
    full_periods_oos_testing_metrics <- do.call(rbind, oos_metrics_list)
    common_dates_oos_testing_metrics <- do.call(rbind, oos_metrics_common_dates_list)
    mean_validation_metrics <- do.call(rbind, validation_metrics_list)

    # Remove row names from consolidated metrics data frames
    rownames(full_periods_oos_testing_metrics) <- NULL
    rownames(common_dates_oos_testing_metrics) <- NULL
    rownames(mean_validation_metrics) <- NULL

    # Replace NaN with NA for clarity
    full_periods_oos_testing_metrics[is.nan(as.matrix(full_periods_oos_testing_metrics))] <- NA
    common_dates_oos_testing_metrics[is.nan(as.matrix(common_dates_oos_testing_metrics))] <- NA
    mean_validation_metrics[is.nan(as.matrix(mean_validation_metrics))] <- NA

    # Convert appropriate columns to numeric
    num_cols_oos <- sapply(full_periods_oos_testing_metrics, is.numeric)
    full_periods_oos_testing_metrics[, num_cols_oos] <- lapply(full_periods_oos_testing_metrics[, num_cols_oos], as.numeric)

    num_cols_common <- sapply(common_dates_oos_testing_metrics, is.numeric)
    common_dates_oos_testing_metrics[, num_cols_common] <- lapply(common_dates_oos_testing_metrics[, num_cols_common], as.numeric)

    num_cols_val <- sapply(mean_validation_metrics, is.numeric)
    mean_validation_metrics[, num_cols_val] <- lapply(mean_validation_metrics[, num_cols_val], as.numeric)

    ###########################

    #Create Time Series Objects
    ###########################

    # Create time series data for each metric
    time_series_oos_testing_metrics <- list()

    # Extract unique metric names from the long data frame
    time_series_metric_names <- unique(all_oos_metrics_long_df$Metric)

    for (metric in time_series_metric_names) {
      # Subset the data for the metric
      metric_df <- subset(all_oos_metrics_long_df, Metric == metric)

      # Reshape to wide format
      metric_wide_df <- as.data.frame(tidyr::pivot_wider(
        metric_df,
        id_cols = Date,
        names_from = Backtest,
        values_from = Value
      ))

      # Arrange by Date
      metric_wide_df <- metric_wide_df[order(as.Date(metric_wide_df$Date)), ]

      # Set rownames to Date
      rownames(metric_wide_df) <- metric_wide_df$Date
      metric_wide_df$Date <- NULL

      # Convert to data frame
      metric_wide_df <- as.data.frame(metric_wide_df)

      # Add to the list
      time_series_oos_testing_metrics[[metric]] <- metric_wide_df
    }

    # Similarly for validation metrics time series
    time_series_validation_metrics <- list()

    if (nrow(all_validation_metrics_long_df) > 0) {
      validation_time_series_metric_names <- unique(all_validation_metrics_long_df$Metric)

      for (metric in validation_time_series_metric_names) {
        # Subset the data for the metric
        metric_df <- subset(all_validation_metrics_long_df, Metric == metric)

        # Reshape to wide format
        metric_wide_df <- as.data.frame(tidyr::pivot_wider(
          metric_df,  # Ensure data frame
          id_cols = Date,
          names_from = Backtest,
          values_from = Value
        ))

        # Arrange by Date
        # Convert Date to Date class if possible
        metric_df_dates <- try(as.Date(metric_wide_df$Date), silent = TRUE)
        if (inherits(metric_df_dates, "Date")) {
          metric_wide_df <- metric_wide_df[order(metric_df_dates), ]
        } else {
          # If Date is not a real date, sort numerically or as character
          metric_wide_df <- metric_wide_df[order(metric_wide_df$Date), ]
        }

        # Set rownames to Date
        rownames(metric_wide_df) <- metric_wide_df$Date
        metric_wide_df$Date <- NULL

        # Convert to data frame
        metric_wide_df <- as.data.frame(metric_wide_df)

        # Add to the list
        time_series_validation_metrics[[metric]] <- metric_wide_df
      }
    }

  ###########################

   # Create the 'sb_metabacktest_results' object
    new_object <- new("sb_metabacktest_results",
                      meta_sb_backtest_results_list = meta_sb_backtest_results_list,
                      base_sb_backtest_results_list = base_sb_backtest_results_list,
                      base_learners_oos_predictions_meta_dataframe = oos_predictions_m_df,
                      consolidated_oos_testing_metrics = list(full_periods_oos_testing_metrics = full_periods_oos_testing_metrics,
                                                             common_dates_oos_testing_metrics = common_dates_oos_testing_metrics),
                      mean_validation_metrics = mean_validation_metrics,
                      time_series_oos_testing_metrics = time_series_oos_testing_metrics,
                      time_series_validation_metrics = time_series_validation_metrics,
                      backtest_identifier = meta_learner_sb_backtest_workflow$backtest_identifier
                      )

    return(new_object)
  }
)

#-----------------------------------------------------------------------
# liquidity_constraint_policy
#-----------------------------------------------------------------------


#' @title Add Liquidity Constraint Policy
#' @description Adds a liquidity constraint policy to a `port_backtest_config` object.
#'
#' @param port_backtest_config_obj A `port_backtest_config` object to which the liquidity constraint policy will be added.
#' @param liquidity_floor_rule A character string representing the liquidity floor rule (optional).
#' @param liquidity_cap_rules A named numeric vector where names represent liquidity classifications and values represent liquidity caps (optional).
#'
#' @return The updated `port_backtest_config` object with the added liquidity constraint policy.
#'
#' @examples
#' portfolio <- create_port_backtest_config()
#' portfolio <- add_liquidity_constraint_policy(portfolio, liquidity_floor_rule = "micro_caps")
#' portfolio <- add_liquidity_constraint_policy(portfolio, liquidity_cap_rules = c(micro_caps = 0.01))
#' portfolio <- add_liquidity_constraint_policy(portfolio, liquidity_floor_rule = "small_caps") # This should update the liquidity_floor_rule
#'
#' @export
setGeneric("add_liquidity_constraint_policy", function(port_backtest_config_obj, liquidity_floor_rule = NULL, liquidity_cap_rules = NULL) {
  standardGeneric("add_liquidity_constraint_policy")
})

#' @export
setMethod("add_liquidity_constraint_policy", "port_backtest_config", function(port_backtest_config_obj, liquidity_floor_rule = NULL, liquidity_cap_rules = NULL) {
  # Allowed classes
  allowed_classes <- c("micro_caps", "small_caps", "mid_caps", "large_caps", "mega_caps")

  # Validate liquidity_floor_rule
  if (!is.null(liquidity_floor_rule)) {
    if (!is.character(liquidity_floor_rule) || length(liquidity_floor_rule) != 1) {
      stop("Error: liquidity_floor_rule must be a single character string.")
    }
    if (!liquidity_floor_rule %in% allowed_classes) {
      stop(sprintf("Error: liquidity_floor_rule must be one of: %s.", paste(allowed_classes, collapse = ", ")))
    }
  }

  # Validate liquidity_cap_rules
  if (!is.null(liquidity_cap_rules)) {
    if (!is.numeric(liquidity_cap_rules) || length(liquidity_cap_rules) == 0) {
      stop("Error: liquidity_cap_rules must be a non-empty named numeric vector.")
    }
    if (!all(names(liquidity_cap_rules) %in% allowed_classes)) {
      stop("Error: All names in liquidity_cap_rules must be one of: micro_caps, small_caps, mid_caps, large_caps, mega_caps.")
    }
  }

  # Initialize new liquidity_constraint_policy
  new_liquidity_constraint_policy <- list(liquidity_floor_rule = NULL)

  # Add liquidity_floor_rule
  if (!is.null(liquidity_floor_rule)) {
    new_liquidity_constraint_policy$liquidity_floor_rule <- liquidity_floor_rule
  } else {
    new_liquidity_constraint_policy$liquidity_floor_rule <- port_backtest_config_obj@liquidity_constraint_policy$liquidity_floor_rule  # Keep the existing rule if it exists
  }

  # Add liquidity_cap_rules
  existing_rules <- port_backtest_config_obj@liquidity_constraint_policy
  existing_liquidity_floor_cap_rules <- existing_rules[!(names(existing_rules) %in% "liquidity_floor_rule")]

  if (!is.null(liquidity_cap_rules)) {
    existing_liquidity_floor_cap_rules_vector <- vapply(existing_liquidity_floor_cap_rules, function(x) x$liquidity_cap, numeric(1))
    names(existing_liquidity_floor_cap_rules_vector) <- sapply(existing_liquidity_floor_cap_rules, function(x) x$liquidity_classification)

    # Combine with new rules
    final_liquidity_floor_cap_rules_vector <- c(existing_liquidity_floor_cap_rules_vector[!(names(existing_liquidity_floor_cap_rules_vector) %in% names(liquidity_cap_rules))],
                                                liquidity_cap_rules)

    # Transform to the right format
    new_liquidity_cap_rules <- setNames(
      lapply(seq_along(final_liquidity_floor_cap_rules_vector), function(i) {
        list(
          liquidity_classification = names(final_liquidity_floor_cap_rules_vector)[i],
          liquidity_cap = final_liquidity_floor_cap_rules_vector[[i]]
        )
      }),
      paste0("liquidity_cap_rule_", seq_along(final_liquidity_floor_cap_rules_vector))
    )
  } else {
    #If there are no new rules, use old policy
    new_liquidity_cap_rules <- existing_liquidity_floor_cap_rules
  }


    # Add liquidity_cap_rules to the new policy
    new_liquidity_constraint_policy <- c(new_liquidity_constraint_policy, new_liquidity_cap_rules)




  # Create the S4 object
  new_port_backtest_config_obj <- new("port_backtest_config",
                                    liquidity_constraint_policy = new_liquidity_constraint_policy,
                                    signal_selection_policy = port_backtest_config_obj@signal_selection_policy,
                                    turnover_constraint_policy = port_backtest_config_obj@turnover_constraint_policy,
                                    concentration_constraint_policy = port_backtest_config_obj@concentration_constraint_policy,
                                    liquidity_floor_cutoffs = port_backtest_config_obj@liquidity_floor_cutoffs)

  return(new_port_backtest_config_obj)
})


#-----------------------------------------------------------------------
# turnover_constraint_policy
#-----------------------------------------------------------------------

#' @title Add Turnover Constraint Policy
#' @description Adds a turnover constraint policy to a `port_backtest_config` object.
#'
#' @param port_backtest_config_obj A `port_backtest_config` object to which the liquidity constraint policy will be added.
#' @param turnover_rule A list with the following structure, specifying the turnover rule:
#' #' \itemize{
#'   \item \strong{liquidity_classification:} One of "micro_caps", "small_caps", "mid_caps", "large_caps" or "mega_caps"
#'   \item \strong{turnover_cap:} A number indicating the turnover_cap
#'   \item \strong{top_stock_quantile_buffer:} A number indicating the minimum signal quantile
#'   }
#'
#' @return The updated `port_backtest_config` object with the added liquidity constraint policy.
#'
#' @examples
#' portfolio <- create_port_backtest_config()
#' portfolio <- add_turnover_constraint_policy(portfolio, turnover_rule = list(liquidity_classification = "micro_caps", turnover_cap = 0.01, top_stock_quantile_buffer = 0.66))
#'
#' @export
setGeneric("add_turnover_constraint_policy", function(port_backtest_config_obj, turnover_rules) {
  standardGeneric("add_turnover_constraint_policy")
})

#' @export
setMethod("add_turnover_constraint_policy", "port_backtest_config", function(port_backtest_config_obj, turnover_rules) {

  # Allowed classes
  allowed_classes <- c("micro_caps", "small_caps", "mid_caps", "large_caps", "mega_caps")


  # Validate turnover_rules
  if(!is.list(turnover_rules)){
    stop("turnover_rules must be a list or list of lists")
  }

  ##Create function to extract vector dependengin on nesting structure
  unnest_turnover_rules <- function(argument){
    if(all(sapply(turnover_rules, function(x) is.list(x)))){
      return(turnover_rules %>% sapply(function(x) x[[argument]]))
    } else {
      return(turnover_rules[[argument]])
    }
  }

  ##liquidity classification
  liquidity_classification_vector <- unnest_turnover_rules("liquidity_classification")

  if (!all(is.character(liquidity_classification_vector))) {
    stop("Error: liquidity_classification must be a character string.")
  }
  if (!all(liquidity_classification_vector %in% allowed_classes)) {
    stop(sprintf("Error: liquidity_classification must be one of: %s.", paste(allowed_classes, collapse = ", ")))
  }

  ##turnover_cap
  turnover_cap_vector <- unnest_turnover_rules("turnover_cap")
  if(!all(is.numeric(turnover_cap_vector))){
    stop("Error: turnover_cap must be numeric")
  }

  ##top_stock_quantile_buffer
  top_stock_quantile_buffer_vector <- unnest_turnover_rules("top_stock_quantile_buffer")
  if(!all(is.numeric(top_stock_quantile_buffer_vector))){
    stop("Error: top_stock_quantile_buffer must be numeric")
  }

  # Add turnover_cap_rules
  ##Get existing rules
  existing_rules <- port_backtest_config_obj@turnover_constraint_policy
  existing_rules_names <- sapply(existing_rules, function(x) x$liquidity_classification)

  #Combine old and new rules
  if(length(existing_rules) != 0){
    #Extract existing rules not to be overwritten
    unique_existing_rules <- existing_rules[[which(!existing_rules_names %in% sapply(turnover_rules, function(x) x$liquidity_classification))]]

      ###Check if unique_existing_rules is not list of lists
      if(!all(sapply(unique_existing_rules, function(x) is.list(x)))){
        final_rules <- list(unique_existing_rules) #Create list of lists obj
      } else {
        final_rules <- unique_existing_rules #do noth
      }

    #Combine new and old
    if(all(sapply(turnover_rules, function(x) is.list(x)))){

       #If new object is a list of lists, extract individually and add
      for(new_rule in turnover_rules){
          final_rules[[length(final_rules) + 1]] <- new_rule
      }
    }
      else {
        #In case it is not a list of lists
        final_rules[[length(final_rules) + 1]] <- new_rule
      }
    }

  ###In case there are no old rules

  else {

    if(all(sapply(turnover_rules, function(x) is.list(x)))){
      final_rules <- turnover_rules
    } else {
      final_rules <- list(turnover_rules)
    }
  }

    # Transform to the right format
    n <- length(final_rules)

    # Rename
    names(final_rules) <- paste0("buffer_zone_", seq_len(n))


    # Create the S4 object
    new_port_backtest_config_obj <- new("port_backtest_config",
                                      liquidity_constraint_policy = port_backtest_config_obj@liquidity_constraint_policy,
                                      signal_selection_policy = port_backtest_config_obj@signal_selection_policy,
                                      turnover_constraint_policy = final_rules,
                                      concentration_constraint_policy = port_backtest_config_obj@concentration_constraint_policy,
                                      liquidity_floor_cutoffs = port_backtest_config_obj@liquidity_floor_cutoffs)

    return(new_port_backtest_config_obj)
  })



#-----------------------------------------------------------------------
# concentration_constraint_policy
#-----------------------------------------------------------------------

#' @title Create Concentration Constraint Policy
#' @description Constructor for a `concentration_constraint_policy` object.
#'
#' @param benchmark A character vector (can be empty if no benchmark specified).
#' @param max_abs_active_individual_weight A numeric indicating the max
#'   absolute active weight for individual assets.
#' @param max_abs_active_group_weight A named numeric vector for group constraints.
#'
#' @return An S4 object of class `concentration_constraint_policy`.
#' @export
create_concentration_constraint_policy <- function(
    benchmark = character(0),
    max_abs_active_individual_weight = NA_real_,
    max_abs_active_group_weight = numeric(0)
) {
  obj <- new(
    "concentration_constraint_policy",
    benchmark = benchmark,
    max_abs_active_individual_weight = max_abs_active_individual_weight,
    max_abs_active_group_weight = max_abs_active_group_weight
  )
  validObject(obj)
  obj
}


#' @title Add Concentration Constraint Policy
#' @description Either add an existing \code{concentration_constraint_policy} to an object
#' (e.g., \code{port_backtest_config} or \code{sb_backtest_config}), or create one dynamically
#' when \code{policy} is missing.
#'
#' @param object An object of class \code{port_backtest_config} or \code{sb_backtest_config}.
#' @param policy A \code{concentration_constraint_policy} object, or missing if a new one is to be created.
#' @param ... Additional arguments used to create a new \code{concentration_constraint_policy}
#'   if \code{policy} is missing. These typically include:
#'   \itemize{
#'     \item \strong{benchmark} (character)
#'     \item \strong{max_abs_active_individual_weight} (numeric)
#'     \item \strong{max_abs_active_group_weight} (named numeric)
#'   }
#'
#' @return The updated \code{object} with the concentration policy added.
#' @export
setGeneric("add_concentration_constraint_policy", function(object, policy, ...) {
  standardGeneric("add_concentration_constraint_policy")
})

#' @describeIn add_concentration_constraint_policy
#'   Add an existing \code{concentration_constraint_policy} to a \code{port_backtest_config}.
#' @export
setMethod("add_concentration_constraint_policy",
          signature(object = "port_backtest_config", policy = "concentration_constraint_policy"),
          function(object, policy, ...) {

            object@concentration_constraint_policy <- policy
            methods::validObject(object)  # optional validity check
            return(object)
          }
)
#' @describeIn add_concentration_constraint_policy
#'   Dynamically create a \code{concentration_constraint_policy} and add it to a \code{port_backtest_config}.
#' @export
setMethod("add_concentration_constraint_policy",
          signature(object = "port_backtest_config", policy = "missing"),
          function(object,
                   policy,
                   benchmark = character(0),
                   max_abs_active_individual_weight = NA_real_,
                   max_abs_active_group_weight = numeric(0),
                   ...) {

            # Build a new policy on the fly
            new_policy <- create_concentration_constraint_policy(
              benchmark = benchmark,
              max_abs_active_individual_weight = max_abs_active_individual_weight,
              max_abs_active_group_weight = max_abs_active_group_weight
            )

            object@concentration_constraint_policy <- new_policy
            methods::validObject(object)
            return(object)
          }
)



#' @describeIn add_concentration_constraint_policy
#'   Add an existing \code{concentration_constraint_policy} to a \code{sb_backtest_config}.
#'   This method will store it inside \code{object@signal_port_parameters}.
#' @export
setMethod("add_concentration_constraint_policy",
          signature(object = "sb_backtest_config", policy = "concentration_constraint_policy"),
          function(object, policy, ...) {

            # Ensure signal_port_parameters is defined
            if (!methods::is(object@signal_port_parameters, "signal_port_parameters")) {
              object@signal_port_parameters <- methods::new("signal_port_parameters")
            }

            object@signal_port_parameters@concentration_constraint_policy <- policy
            methods::validObject(object)
            return(object)
          }
)

#' @describeIn add_concentration_constraint_policy
#'   Dynamically create a \code{concentration_constraint_policy} for \code{sb_backtest_config}.
#' @export
setMethod("add_concentration_constraint_policy",
          signature(object = "sb_backtest_config", policy = "missing"),
          function(object,
                   policy,
                   benchmark = character(0),
                   max_abs_active_individual_weight = NA_real_,
                   max_abs_active_group_weight = numeric(0),
                   ...) {

            # Build a new policy
            new_policy <- create_concentration_constraint_policy(
              benchmark = benchmark,
              max_abs_active_individual_weight = max_abs_active_individual_weight,
              max_abs_active_group_weight = max_abs_active_group_weight
            )

            # Ensure signal_port_parameters is defined
            if (!methods::is(object@signal_port_parameters, "signal_port_parameters")) {
              object@signal_port_parameters <- methods::new("signal_port_parameters")
            }

            #No group constraints for signal port
            if(length(max_abs_active_group_weight) > 0){
              stop("Group constraints are not supported for signal port")
            }

            # Assign
            object@signal_port_parameters@concentration_constraint_policy <- new_policy
            methods::validObject(object)
            return(object)
          }
)

























#' @title Add Signal Selection Policy
#' @description Adds a signal selection policy to a `port_backtest_config` object.
#'
#' @param port_backtest_config_obj A `port_backtest_config` object to which the signal selection policy will be added.
#' @param signal_blending_method A method for blending signals.
#' @param sb_benchmark_weighting A weighting strategy for the benchmark.
#' @param max_abs_active_individual_weight A number indicating the absolute weight differential from benchmark weights.
#' @param max_abs_active_group_weight A named vector indicating absolute group (sector) weight differentials in relation to the benchmark.
#' @param p_correction_method Method for p-value correction.
#' @param chosen_signals A vector of chosen signals.
#' @param signal_positions A data structure indicating positions of signals.
#' @param signal_significance_threshold A threshold for signal significance.
#' @param chosen_informative_data A data structure with informative data.
#' @param chosen_sb_metric A metric chosen for signal blending.
#' @param priors_type Type of priors to use.
#' @param data_availability_cutoff A cutoff for data availability.
#'
#' @return The updated `port_backtest_config` object with the added signal selection policy.
#'
#' @examples
#' portfolio <- create_port_backtest_config()
#' portfolio <- add_signal_selection_policy(portfolio, signal_blending_method = "method1", ...)
#'
#' @export
setGeneric("add_signal_selection_policy", function(port_backtest_config_obj,
                                                   #Signal Selection
                                                   chosen_signals = NULL, signal_positions = NULL,
                                                   #How to blend signals?
                                                   signal_blending_method = NULL, chosen_sb_metric = NULL,
                                                   #Restrictions on weights
                                                   sb_benchmark_weighting_method = NULL, max_abs_active_individual_weight = NULL, max_abs_active_group_weight = NULL,
                                                   #How to select signals
                                                   p_correction_method = "none", signal_significance_threshold = 0.05, data_availability_cutoff = 60,
                                                   priors_type = NULL, priors_informative_data = NULL) {

  standardGeneric("add_signal_selection_policy")
})

#' @export
setMethod("add_signal_selection_policy", "port_backtest_config", function(port_backtest_config_obj,
                                                                          #Signal Selection
                                                                          chosen_signals = NULL, signal_positions = NULL,
                                                                          #How to blend signals?
                                                                          signal_blending_method = NULL, chosen_sb_metric = NULL,
                                                                          #Restrictions on weights
                                                                          sb_benchmark_weighting_method = "theme_sb", max_abs_active_individual_weight = NULL, max_abs_active_group_weight = NULL,
                                                                          #How to select signals
                                                                          p_correction_method = "none", signal_significance_threshold = 0.05, data_availability_cutoff = 60,
                                                                          priors_type = NULL, priors_informative_data = NULL
                                                                        ) {

  # Validate arguments
    ## chosen_signals
    if(!is.null(chosen_signals)){
      if (!all(is.character(chosen_signals))){
        stop("chosen_signals should be a character vector.")
      }
      #Check if signal positions is also updated
      if(is.null(signal_positions)){
        stop("please always update chosen_signals with signal_positions to ensure consistency")
      }

      ## signal_position
      if (!all(is.character(signal_positions))){
        stop("signal_positions should be a character vector.")
      }
      if(length(signal_positions) != length(chosen_signals)){
        stop("lengths of signal_positions and chosen_signals should match.")
      }

      ## check for appropriate signal_blending_method
      if(length(chosen_signals) > 1){
        #If more than one signal is chosen, signal_blending_method can't be NULL
        if(any(!is.null(signal_blending_method), !is.null(port_backtest_config_obj@signal_selection_policy))){
          #Either the new signal_blending_method or the old one must be different from NULL
        }
      }
    }

    ## Signal Blending Method
    if (!is.null(signal_blending_method)){
      ###Character
      if(!is.character(signal_blending_method)) {
        stop("signal_blending_method should be a character")
      }
      ###Correct choice
      if(!signal_blending_method %in% c("EW", "SW", "RP", "MTO", "ML")){
        stop("signal_blending_method should be one of EW, SW, RP, MTO or ML")
      }
    }

    ##chosen_sb_metric
    if (!is.null(chosen_sb_metric)){
      ###Character
      if(!is.character(chosen_sb_metric)) {
        stop("chosen_sb_metric should be a character")
      }
      ###Correct choice
      if(!chosen_sb_metric %in% c("mean_active_return", "IR", "alpha", "AP", "treynor")){
        stop("chosen_sb_metric should be one of mean_active_return, IR, alpha, AP or treynor")
      }
    }

    ## Signal Blending Benchmark Weighting Method
    if (!is.null(sb_benchmark_weighting_method)){
      ###Character
      if(!is.character(sb_benchmark_weighting_method)) {
        stop("sb_benchmark_weighting_method should be a character")
      }
      ###Correct choice
      if(!sb_benchmark_weighting_method %in% c("individual_sb", "theme_sb")){
        stop("sb_benchmark_weighting_method should be one of individual_sb or theme_sb")
      }
    }

    ##max_abs_active_individual_weight
    if(!is.null(max_abs_active_individual_weight)){
      if(!is.numeric(max_abs_active_individual_weight)){
        stop("max_abs_active_individual_weight should be numeric")
      }
    }

    ##max_abs_active_group_weight
    if(!is.null(max_abs_active_group_weight)){
      if(!is.numeric(max_abs_active_group_weight)){
        stop("max_abs_active_group_weight should be numeric")
      }
    }


    ## p_correction_method
    if (!is.null(p_correction_method)){
      ###Character
      if(!is.character(p_correction_method)) {
        stop("p_correction_method should be a character")
      }
      ###Correct choice
      if(!p_correction_method %in% c("bayesian", "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")){
        stop("p_correction_method should be one of bayesian, holm, hochberg, hommel, bonferroni, BH, BY, fdr or none")
      }
    }

    ##signal_significance_threshold
    if(!is.numeric(signal_significance_threshold)){
      stop("signal_significance_threshold should be numeric")
    }

    ##data_availability_cutoff
    if(!is.numeric(data_availability_cutoff)){
      stop("data_availability_cutoff should be numeric")
    }

    ##priors_type
    if (!is.null(priors_type)){
      ###Character
      if(!is.character(priors_type)) {
        stop("priors_type should be a character")
      }
      ###Correct choice
      if(!priors_type %in% c("uninformative", "all", "mean", "user")){
        stop("priors_type should be one of uninformative, all, mean or user")
      }
    }

    ##priors_informative_data
    if (!is.null(priors_informative_data)){
      ###Character
      if(!is.character(priors_informative_data)) {
        stop("priors_informative_data should be a character")
      }
      ###Correct choice
      if(!priors_informative_data %in% c("jkp_emerging", "cz_global")){
        stop("priors_type should be one of jkp_emerging or cz_global")
      }
    }


  # Construct new_signal_selection_policy list
  new_signal_selection_policy <- list(
    chosen_signals = if (!is.null(chosen_signals)) chosen_signals else port_backtest_config_obj@signal_selection_policy$chosen_signals,
    signal_positions = if (!is.null(signal_positions)) signal_positions else port_backtest_config_obj@signal_selection_policy$signal_positions,
    signal_blending_method = if (!is.null(signal_blending_method)) signal_blending_method else port_backtest_config_obj@signal_selection_policy$signal_blending_method,
    chosen_sb_metric = if (!is.null(chosen_sb_metric)) chosen_sb_metric else port_backtest_config_obj@signal_selection_policy$chosen_sb_metric,
    sb_benchmark_weighting_method = if (!missing(sb_benchmark_weighting_method) && !is.null(sb_benchmark_weighting_method)) {
      sb_benchmark_weighting_method
    } else if (!is.null(port_backtest_config_obj@signal_selection_policy$sb_benchmark_weighting_method)) {
      port_backtest_config_obj@signal_selection_policy$sb_benchmark_weighting_method
    } else {
      "theme_sb"
    },
    max_abs_active_individual_weight = if (!is.null(max_abs_active_individual_weight)) max_abs_active_individual_weight else port_backtest_config_obj@signal_selection_policy$max_abs_active_individual_weight,
    max_abs_active_group_weight = if (!is.null(max_abs_active_group_weight)) max_abs_active_group_weight else port_backtest_config_obj@signal_selection_policy$max_abs_active_group_weight,
    p_correction_method = if (!missing(p_correction_method) && p_correction_method != "none") {
      p_correction_method
    } else if (!is.null(port_backtest_config_obj@signal_selection_policy$p_correction_method)) {
      port_backtest_config_obj@signal_selection_policy$p_correction_method
    } else {
      "none"
    },
    signal_significance_threshold = if (!is.null(signal_significance_threshold)) signal_significance_threshold else port_backtest_config_obj@signal_selection_policy$signal_significance_threshold,
    data_availability_cutoff = if (!is.null(data_availability_cutoff)) data_availability_cutoff else port_backtest_config_obj@signal_selection_policy$data_availability_cutoff,
      priors_type = if (!is.null(p_correction_method) && p_correction_method == "bayesian" && is.null(priors_type)) {
    "uninformative"
  } else if (!is.null(priors_type)) {
    priors_type
  } else {
    port_backtest_config_obj@signal_selection_policy$priors_type
  },
    priors_informative_data = if (!is.null(priors_informative_data)) priors_informative_data else port_backtest_config_obj@signal_selection_policy$priors_informative_data
  )

  # Final consistency checks

  # Check if max_abs_active_individual_weight is set but signal_blending_method is not "MTO"
  if (!is.null(new_signal_selection_policy$signal_blending_method) &&
      new_signal_selection_policy$signal_blending_method != "MTO" &
      !is.null(new_signal_selection_policy$max_abs_active_individual_weight)) {
    stop("signal_blending_method must be equal to MTO if max_abs_active_individual_weight is set")
  }

  # Check if max_abs_active_group_weight is set but signal_blending_method is not "MTO"
  if (!is.null(new_signal_selection_policy$signal_blending_method) &&
      new_signal_selection_policy$signal_blending_method != "MTO" &
      !is.null(new_signal_selection_policy$max_abs_active_group_weight)) {
    stop("signal_blending_method must be equal to MTO if max_abs_active_group_weight is set")
  }

  # Check if chosen_sb_metric is NULL when signal_blending_method is "SW" or "MTO"
  if (!is.null(new_signal_selection_policy$signal_blending_method) &&
      new_signal_selection_policy$signal_blending_method %in% c("SW", "MTO") &
      is.null(new_signal_selection_policy$chosen_sb_metric)) {
    stop("chosen_sb_metric can't be NULL if signal_blending_method is SW or MTO")
  }

  # Check if chosen_sb_metric is NULL when signal_blending_method is "SW" or "MTO"
  if (!is.null(new_signal_selection_policy$signal_blending_method) &&
      !new_signal_selection_policy$signal_blending_method %in% c("SW", "MTO") &
      !is.null(new_signal_selection_policy$chosen_sb_metric)) {
    stop("chosen_sb_metric is only needed if signal_blending_method is SW or MTO")
  }

  # Check for p_correction_method = "bayesian" and valid priors_type and priors_informative_data
  if (!is.null(new_signal_selection_policy$p_correction_method) &&
      new_signal_selection_policy$p_correction_method == "bayesian" &&
      !new_signal_selection_policy$priors_type %in% c("uninformative", "user") &&
      is.null(new_signal_selection_policy$priors_informative_data)) {
    stop("priors_informative_data can't be NULL if p_correction_method is bayesian and priors_type is not uninformative or user")
  }

  # Create the updated port_backtest_config object
  new_port_backtest_config_obj <- new("port_backtest_config",
                                    liquidity_constraint_policy = port_backtest_config_obj@liquidity_constraint_policy,
                                    signal_selection_policy = new_signal_selection_policy,
                                    turnover_constraint_policy = port_backtest_config_obj@turnover_constraint_policy,
                                    concentration_constraint_policy = port_backtest_config_obj@concentration_constraint_policy,
                                    liquidity_floor_cutoffs = port_backtest_config_obj@liquidity_floor_cutoffs)

  return(new_port_backtest_config_obj)
})


#' @title Add Liquidity Floor Cutoffs
#' @description Add liquidity floor cutoffs to the port_backtest_config object.
#' @param port_backtest_config_obj An S4 object of class `port_backtest_config`.
#' @param liquidity_metric A character string representing the liquidity metric.
#' @param cutoffs A numeric vector of length 5 representing the cutoff values for
#' micro_caps, small_caps, mid_caps, large_caps, and mega_caps.
#' @return An updated `port_backtest_config` object.
#' @export
setGeneric("add_liquidity_floor_cutoffs", function(port_backtest_config_obj, liquidity_metric, cutoffs) {
  standardGeneric("add_liquidity_floor_cutoffs")
})

#' @export
setMethod("add_liquidity_floor_cutoffs", "port_backtest_config", function(port_backtest_config_obj,
                                                                        liquidity_metric,
                                                                        cutoffs) {
  # Validate inputs
  if (!is.character(liquidity_metric) || length(liquidity_metric) != 1) {
    stop("liquidity_metric should be a single character string.")
  }

  if (!is.numeric(cutoffs) || length(cutoffs) != 5 || any(cutoffs <= 0)) {
    stop("cutoffs must be a numeric vector of length 5, and all values must be positive.")
  }

  # Check if cutoffs are in ascending order
  if (!all(diff(cutoffs) >= 0)) {
    stop("cutoffs must be provided in ascending order.")
  }

  # Initialize liquidity_floor_cutoffs if it doesn't exist
  if (is.null(port_backtest_config_obj@liquidity_floor_cutoffs)) {
    port_backtest_config_obj@liquidity_floor_cutoffs <- list(
      micro_caps = numeric(),
      small_caps = numeric(),
      mid_caps = numeric(),
      large_caps = numeric(),
      mega_caps = numeric()
    )
  }

  # Update each market capitalization class with the new liquidity metric cutoffs
  port_backtest_config_obj@liquidity_floor_cutoffs$micro_caps[liquidity_metric] <- cutoffs[1]
  port_backtest_config_obj@liquidity_floor_cutoffs$small_caps[liquidity_metric] <- cutoffs[2]
  port_backtest_config_obj@liquidity_floor_cutoffs$mid_caps[liquidity_metric] <- cutoffs[3]
  port_backtest_config_obj@liquidity_floor_cutoffs$large_caps[liquidity_metric] <- cutoffs[4]
  port_backtest_config_obj@liquidity_floor_cutoffs$mega_caps[liquidity_metric] <- cutoffs[5]

  # Return the updated portfolio policies object
  return(port_backtest_config_obj)
})



