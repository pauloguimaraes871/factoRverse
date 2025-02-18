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

          function(data, meta_dataframe_name = "not_identified",
                   workflow = NULL, ss_backtest_workflow = NULL, sb_backtest_workflow = NULL, port_backtest_workflow = NULL, type = "generic", ...) {

            #Check for type argument
              if(!type %in% c("generic", "signal_universe", "stock_universe", "oos_sb_outputs", "groups", "target", "weights", "priors", "signals", "features")){
                stop("type argument must be one of 'generic', 'signal_universe', 'stock_universe', 'oos_sb_outputs', 'groups', 'target',
                     'weights', 'priors'.")
              }


            #Is it coercible
            if(!is_coercible_to_meta_dataframe(data)){
              stop("The data frame is not coercible to a meta_dataframe object")
            }

            # Ensure no gaps in the dates sequence FOR GENERIC
            unique_dates <- unique(data$dates)
            full_dates <- seq(min(unique_dates), max(unique_dates), by = "month")
            missing_dates <- setdiff(full_dates, unique_dates)

            if (length(missing_dates) > 0 && type == "generic") {
              warning("There are gaps in the dates sequence. Missing dates: ", paste(as.Date(missing_dates), collapse = ", "))
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

            if (type %in% c("signals", "features")){

              return(
                new("signals_m_df",
                    data = data,
                    workflow = NULL,
                    signals = names(data)[-c(1:3)],
                    unique_dates = unique_dates_count,
                    unique_tickers = unique_tickers_count,
                    n_obs = total_observations_count,
                    meta_dataframe_name = meta_dataframe_name)
              )
            }
            if (type == "signal_universe"){

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
                    ss_backtest_workflow = ss_backtest_workflow)
              )
            }
            if(type == "oos_sb_outputs"){

              #Check for workflow
              if(is.null(sb_backtest_workflow)){
                stop("sb_backtest_workflow argument must be provided for signal_universe type")
              }

              return(
                new("oos_sb_outputs_m_df",
                    data = data,
                    workflow = NULL,
                    signals = names(data)[-c(1:3)],
                    unique_dates = unique_dates_count,
                    unique_tickers = unique_tickers_count,
                    n_obs = total_observations_count,
                    meta_dataframe_name = meta_dataframe_name,
                    sb_backtest_workflow = sb_backtest_workflow)
              )
            }

            if (type == "stock_universe"){

              #Check for workflow
              if(is.null(port_backtest_workflow)){
                stop("port_backtest_workflow argument must be provided for stock_universe type")
              }

              # Store metadata and column names
              return(
                new("stock_universe_m_df",
                    data = data,
                    workflow = NULL,
                    signals = names(data)[-c(1:3)],
                    unique_dates = unique_dates_count,
                    unique_tickers = unique_tickers_count,
                    n_obs = total_observations_count,
                    meta_dataframe_name = meta_dataframe_name,
                    port_backtest_workflow = port_backtest_workflow)
              )
            }
            if(type == "groups"){

              return(
                new("groups_m_df",
                    data = data,
                    workflow = NULL,
                    signals = names(data)[-c(1:3)],
                    unique_dates = unique_dates_count,
                    unique_tickers = unique_tickers_count,
                    n_obs = total_observations_count,
                    meta_dataframe_name = meta_dataframe_name)
              )
            }

            if (type == "priors"){

              return(
                new("priors_m_df",
                    data = data,
                    workflow = NULL,
                    signals = names(data)[-c(1:3)],
                    unique_dates = unique_dates_count,
                    unique_tickers = unique_tickers_count,
                    n_obs = total_observations_count,
                    meta_dataframe_name = meta_dataframe_name)
              )
            }

            if(type == "target"){

              return(
                new("target_m_df",
                    data = data,
                    workflow = NULL,
                    signals = names(data)[-c(1:3)],
                    unique_dates = unique_dates_count,
                    unique_tickers = unique_tickers_count,
                    n_obs = total_observations_count,
                    meta_dataframe_name = meta_dataframe_name)
              )
            }

            if(type == "weights"){

              return(
                new("weights_m_df",
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
# meta_xts
#-----------------------------------------------------------------------
#' Create a meta_xts, assets_meta_xts or metrics_meta_xts object.
#'
#' This constructor automatically sets most slots for you based on an
#' input xts object and the desired type: "assets" or "metrics".
#'
#' @param data An xts object containing your time series data.
#' @param type Character. Either \code{"assets"} (no holes allowed) or
#'   \code{"metrics"} (holes allowed). Defaults to \code{c("assets","metrics")}
#'   which means you must pick one.
#' @param meta_xts_name A character string. Defaults to \code{"default_name"}.
#' @param workflow An ANY object for workflow. Defaults to \code{NULL}.
#' @param source A character vector indicating data origin for each column.
#'   If \code{NULL}, defaults to \code{"not_identified"} repeated for each column.
#'
#' @return An S4 object of class \code{assets_meta_xts} or \code{metrics_meta_xts},
#'   depending on \code{type}.
#'
#' @examples
#' \dontrun{
#'   library(xts)
#'   # Simple example data
#'   dates <- seq(as.Date("2020-01-01"), as.Date("2020-01-05"), by = "day")
#'   x <- matrix(rnorm(5 * 2), ncol = 2)
#'   colnames(x) <- c("AssetA", "AssetB")
#'   my_xts <- xts::xts(x, order.by = dates)
#'
#'   # Create an assets_meta_xts
#'   obj_assets <- create_meta_xts(data = my_xts, type = "port",
#'                                meta_xts_name = "My Assets Data")
#'   validObject(obj_assets)
#'
#'   # Create a metrics_meta_xts
#'   obj_metrics <- create_meta_xts(data = my_xts, type = "metrics",
#'                                 meta_xts_name = "My Metrics Data")
#'   validObject(obj_metrics)
#' }
#'
#' @importFrom xts periodicity
#' @importFrom methods new
#' @export
create_meta_xts <- function(data,
                            type = c("returns", "metrics"),
                            asset_type = "not_identified",
                            meta_xts_name = "not_identified",
                            metric_name = NULL,
                            workflow = NULL,
                            source = NULL) {

  # Match 'type' argument
  type <- match.arg(type)

  # If source is NULL, default to a vector of "not_identified"
  # repeated for each column in 'data'.
  if (is.null(source)) {
    source <- rep("not_identified", ncol(data))
  }

  # Detect frequency automatically
  freq_info <-  suppressWarnings(xts::periodicity(data))
  discovered_scale <- if (nrow(data) == 1) "not_available" else freq_info$scale

  # Common slots for the parent class
  common_slots <- list(
    data          = data,
    meta_xts_name = meta_xts_name,
    workflow      = workflow,
    n_dates       = nrow(data),
    source        = source,
    frequency     = discovered_scale
  )

  # 5) Depending on 'type', build the appropriate subclass
  if (type == "returns") {
    # For assets_meta_xts, we fill the specialized slots:
    if(asset_type == "not_identified") message("Asset_type not identified for 'returns_meta_xts' subclass")

    obj <- methods::new(
      "returns_meta_xts",
      data = common_slots$data,
      asset_type = asset_type,
      meta_xts_name = common_slots$meta_xts_name,
      metric_name = if (is.null(metric_name)) "returns" else metric_name,
      workflow = common_slots$workflow,
      n_dates = common_slots$n_dates,
      source = common_slots$source,
      frequency = common_slots$frequency,
      assets   = colnames(data),
      n_assets = ncol(data)
    )
  } else { # type == "metrics"
    # For metrics_meta_xts, we fill the specialized slots:
    obj <- methods::new(
      "metrics_meta_xts",
      data = common_slots$data,
      meta_xts_name = common_slots$meta_xts_name,
      metric_name = if (is.null(metric_name)) "metrics" else metric_name,
      workflow = common_slots$workflow,
      n_dates = common_slots$n_dates,
      source = common_slots$source,
      frequency = common_slots$frequency,
      series   = colnames(data),
      n_series = ncol(data)
    )
  }

  # Return the newly created object
  return(obj)
}


#-----------------------------------------------------------------------
# ss_backtest
#-----------------------------------------------------------------------

#' @title Create an ss_backtest_config Object
#' @description This function constructs an object of class `ss_backtest_config`, ensuring the proper initialization
#' and validation of its slots.
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
#'   initial_sample_size = 200,
#'   rebalancing_months = 6,
#'   alpha_test_strategy = alpha_test_strategy_obj,
#'   config_name = "ExampleConfig"
#' )
#' @export
create_ss_backtest_config <- function(
    initial_sample_size,
    rebalancing_months,
    active_returns = TRUE,
    split_method = "expanding",
    alpha_test_strategy = NULL,
    config_name = "not_identified",
    chosen_signals_and_positions = "all"
) {
  #Message
  if (length(chosen_signals_and_positions) == 1 && chosen_signals_and_positions == "all"){
    message("chosen_signals_and_positions set as 'all'. All signals in signals_m_df will be used and a long position will be assumed to all.")
  }
  # Input validation
  if (initial_sample_size < 0) {
    stop("initial_sample_size cannot be negative.")
  }

  if (!split_method %in% c("expanding", "rolling")) {
    stop("split_method must be either 'expanding' or 'rolling'.")
  }

  # Create and return the object
  new("ss_backtest_config",
      chosen_signals_and_positions = chosen_signals_and_positions,
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

            ##Chosen_signals_and_positions
            object@chosen_signals_and_positions <- ss_backtest_obj@chosen_signals_and_positions
            message("chosen_signals_and_positions will follow underlying ss_backtest_config")

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

            ##Chosen_signals_and_positions
            object@chosen_signals_and_positions <- ss_backtest_obj@ss_backtest_workflow$chosen_signals_and_positions
            message("chosen_signals_and_positions will follow underlying ss_backtest_results")


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
          function(object, initial_sample_size, rebalancing_months, active_returns = TRUE, split_method = "expanding",
                   chosen_signals_and_positions = "all",
                   alpha_test_strategy = NULL, config_name = "not_identified") {

            #Create an empty alpha_test_strategy
            if(is.null(alpha_test_strategy)){
              alpha_test_strategy <- create_alpha_test_strategy()
            }

            #create ss_backtest_config
            ss_backtest_config <- create_ss_backtest_config(chosen_signals_and_positions = chosen_signals_and_positions,
                                                            initial_sample_size = initial_sample_size,
                                                            rebalancing_months = rebalancing_months,
                                                            active_returns = active_returns,
                                                            split_method = split_method,
                                                            alpha_test_strategy = alpha_test_strategy,
                                                            config_name = config_name)

            #Include
            object@ss_backtest_config <- ss_backtest_config

            ##Chosen_signals_and_positions
            object@chosen_signals_and_positions <- chosen_signals_and_positions
            message("chosen_signals_and_positions will follow underlying ss_backtest_config")



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
#' @description Adds a prior to a `bayesian_alpha_test_strategy` object based on the specified effect, type, distribution, and parameters.
#' @param object An object of class `bayesian_alpha_test_strategy`.
#' @param effect A character string specifying the effect of the prior. Must be one of "fixed" or "random".
#' @param type A character string specifying the type of prior. Options: "intercept", "slope", "sigma", or "cor".
#' @param theme A character vector specifying themes (e.g., "value", "momentum"). Applicable for fixed effects.
#' @param distribution_choice A character vector specifying the distribution of the prior (e.g., "normal", "student_t").
#' @param pars A list of named numeric vectors specifying parameters for the chosen distribution.
#' @param level A character string specifying the hierarchical level for random effects. Options: "signals" (default), "tickers", or "theme".
#' @return An updated `bayesian_alpha_test_strategy` object with the added prior.
#' @details
#' - For `effect = "fixed"`, you can specify global intercepts or slopes, or theme-specific priors using the `theme` argument.
#' - For `effect = "random"`, you can define hierarchical priors with `level` specifying the grouping structure.
#'
#' Supported `distribution_choice` options include:
#' - `"normal"`: Requires `pars` with `mean` and `sd`.
#' - `"student_t"`: Requires `pars` with `df`, `mean`, and `sd`.
#' - `"lkj"`: Requires `pars` with `eta` (only for `type = "cor"`).
#'
#' @examples
#' # Example 1: Adding fixed effect priors for theme-specific intercepts
#' obj <- new("bayesian_alpha_test_strategy")
#' obj <- add_brms_prior(
#'   object = obj,
#'   effect = "fixed",
#'   type = "intercept",
#'   theme = c("value", "momentum"),
#'   distribution_choice = c("normal", "normal"),
#'   pars = list(c(mean = 0.0012, sd = 0.0016), c(mean = 0.0025, sd = 0.0016))
#' )
#'
#' # Example 2: Adding a random effect prior for intercept at the signals level
#' obj <- add_brms_prior(
#'   object = obj,
#'   effect = "random",
#'   type = "intercept",
#'   level = "signals",
#'   distribution_choice = "student_t",
#'   pars = list(c(df = 30, mean = 0, sd = 0.0113))
#' )
#'
#' # Example 3: Adding a prior for residual error (sigma)
#' obj <- add_brms_prior(
#'   object = obj,
#'   effect = "random",
#'   type = "sigma",
#'   distribution_choice = "student_t",
#'   pars = list(c(df = 30, mean = 0, sd = 0.0256))
#' )
#'
#' # Example 4: Adding a prior for correlation (cor)
#' obj <- add_brms_prior(
#'   object = obj,
#'   effect = "random",
#'   type = "cor",
#'   distribution_choice = "lkj",
#'   pars = list(c(eta = 2))
#' )
#'
#' @export
setGeneric("add_brms_prior", function(object, ...) standardGeneric("add_brms_prior"))

#' @rdname add_brms_prior
#' @export
setMethod("add_brms_prior",
          signature(object = "bayesian_alpha_test_strategy"),
          function(object, effect, type, theme = NULL, distribution_choice, pars, level = "signals") {
            # Input validation
            effect <- match.arg(effect, choices = c("fixed", "random"))
            type <- match.arg(type, choices = c("intercept", "slope", "sigma", "cor"))
            level <- match.arg(level, choices = c("signals", "tickers", "theme"))

            # Ensure pars and distribution_choice have consistent lengths
            n <- length(distribution_choice)
            if (length(pars) != n) {
              stop("The lengths of `pars` and `distribution_choice` must match.")
            }

            # Handle fixed effects
            if (effect == "fixed") {
              priors <- lapply(seq_along(distribution_choice), function(i) {
                coef_name <- if (!is.null(theme)){
                  paste0("theme", theme[i], if (type == "slope") ":market_factor_proxy" else "")
                } else if (type == "slope") "market_factor_proxy" else "Intercept"
                brms::set_prior(
                  paste0(distribution_choice[i], "(", paste(pars[[i]], collapse = ", "), ")"),
                  class = "b", # Correctly setting class to 'b'
                  coef = coef_name
                )
              })
            }

            # Handle random effects
            if (effect == "random") {
              if (type %in% c("intercept", "slope")) {
                coef_name <- if (type == "intercept") "Intercept" else "market_factor_proxy"
                group_name <- if (level == "signals") "theme:tickers" else level
                priors <- list(brms::set_prior(
                  paste0(distribution_choice[1], "(", paste(pars[[1]], collapse = ", "), ")"),
                  class = "sd",
                  group = group_name,
                  coef = coef_name
                ))
              } else if (type == "sigma") {
                priors <- list(brms::set_prior(
                  paste0(distribution_choice[1], "(", paste(pars[[1]], collapse = ", "), ")"),
                  class = "sigma"
                ))
              } else if (type == "cor") {
                priors <- list(brms::set_prior(
                  paste0(distribution_choice[1], "(", paste(pars[[1]], collapse = ", "), ")"),
                  class = "cor"
                ))
              }
            }

            # Add priors to the object
            if (is.null(object@bayesian_model_parameters@user_priors)) {
              object@bayesian_model_parameters@user_priors <- do.call(rbind, priors)
            } else {
              object@bayesian_model_parameters@user_priors <- rbind(
                object@bayesian_model_parameters@user_priors,
                do.call(rbind, priors)
              )
            }

            # Validate the object and return it
            validObject(object)
            return(object)
          })


#' @rdname add_brms_prior
#' @export
setMethod(
  "add_brms_prior",
  signature(object = "ss_backtest_config"),
  function(object, effect, type, theme = NULL, distribution_choice, pars, level = "signals") {
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
      object = object@alpha_test_strategy,
      effect = effect,
      type = type,
      theme = theme,
      distribution_choice = distribution_choice,
      pars = pars,
      level = level
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
              stop("ols, sw, ew, rp and mvo do not require tuning.")
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

            #Check for sb algo
            if(!object@sb_algorithm %in% c("rp", "mvo")){
              stop("Covariance estimation method is only available for 'rp' and 'mvo' strategies.")
            }

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
          function(object, cov_est_method, cov_estimation_method = "sample", cov_matrix_sample_size = 36, active_returns = TRUE, cov_matrix_benchmark = NULL, ...) {

            #Check for sb algo
            if(!object@sb_algorithm %in% c("rp", "mvo")){
              stop("Covariance estimation method is only available for 'rp' and 'mvo' strategies.")
            }

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

            #Check for port construction method
            if(!object@port_construction_method %in% c("rp", "mvo")){
              stop("Covariance estimation method is only available for 'rp' and 'mvo' strategies.")
            }

            #Check for existence of selected_benchmark
            if(!is.null(object@selected_benchmark) && object@active_returns){
              message("Using port_backtest_config selected benchmark as active covariance matrix benchmark.")
              cov_est_method@cov_matrix_benchmark <- object@selected_benchmark
            }

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

            #Check for sb algo
            if(!object@port_construction_method %in% c("rp", "mvo")){
              stop("Covariance estimation method is only available for 'rp' and 'mvo' strategies.")
            }

            #Check for existence of selected_benchmark
            if(!is.null(object@selected_benchmark) && active_returns){
              message("Using port_backtest_config selected benchmark as active covariance matrix benchmark.")
              cov_matrix_benchmark <- object@selected_benchmark
            }

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


            #Check for sb
            if(!object@sb_algorithm == c("mvo")){
              stop("MVO parameters is only available for 'mvo' strategies.")
            }


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

            #Check for port construction method
            if(!object@port_construction_method == c("mvo")){
              stop("MVO parameters is only available for 'mvo' strategies.")
            }

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

            #Check for port construction method
            if(!object@port_construction_method == c("mvo")){
              stop("MVO parameters is only available for 'mvo' strategies.")
            }


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

            #Check for sb
            if(!object@sb_algorithm == c("rp")){
              stop("RP parameters is only available for 'rp' strategies.")
            }

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

            #Check for sb
            if(!object@sb_algorithm == c("rp")){
              stop("RP parameters is only available for 'rp' strategies.")
            }


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

            #Check for pcm
            if(!object@port_construction_method == c("rp")){
              stop("RP parameters is only available for 'rp' strategies.")
            }


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

            #Check for pcm
            if(!object@port_construction_method == c("rp")){
              stop("RP parameters is only available for 'rp' strategies.")
            }

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
                                      chosen_signals_and_positions = NULL,
                                      custom_objective = "squared_error", keras_architecture_parameters = NULL, signal_port_parameters = NULL, quantile_tau = 0.5, huber_delta = 1,
                                      config_name = "not_identified") {

  ##Give custom warning related to quantile tau and huber delta
  if (!is.null(quantile_tau) && quantile_tau != 0.5) {
    message("changing quantile_tau impacts both chosen_eval_metric and custom_objective.")
  }
  if (!is.null(huber_delta) && huber_delta != 1) {
    message("changing huber_delta impacts both chosen_eval_metric and custom_objective.")
  }

  ##Chosen_signals_and_positions
  ###Presence of other objects
  if (any(!is.null(ss_backtest_config),!is.null(ss_backtest_results))){
    if (!is.null(chosen_signals_and_positions)){
      stop("chosen_signals_and_positions should only be provided when 'ss_backtest_config' or 'ss_backtest_results' are missing.")
    }
    if (!is.null(ss_backtest_config)){
      chosen_signals_and_positions <- ss_backtest_config@chosen_signals_and_positions
      message("chosen_signals_and_positions will follow underlying ss_backtest_config")
    } else {
      chosen_signals_and_positions <- ss_backtest_results@ss_backtest_workflow$chosen_signals_and_positions
      message("chosen_signals_and_positions will follow underlying ss_backtest_results")
    }
    if (sb_algorithm == "custom_weights"){
      message("only positions in chosen_signals_and_positions are applied when sb_algorithm is custom_weights.")
    }
  }
  ###Custom weights warning
  if (sb_algorithm == "custom_weights" && is.null(chosen_signals_and_positions)){
    message("Only positions of chosen_signals_and_positions are used when sb_algorithm is custom_weights, as every non-zero weight",
            "in custom_signal_weights_m_df will be eligible.")
  }

  if (is.null(ss_backtest_config) && is.null(ss_backtest_results) && is.null(chosen_signals_and_positions)){
    chosen_signals_and_positions <- "all"
  }

  ###Check if chosen_signals_and_positions length > 1
  if(length(chosen_signals_and_positions) == 1 && chosen_signals_and_positions != "all"){
    stop("More than one signal must be provided in order to run a sb_backtest")
  }
  ###Check if there are repeated signals in chosen_signals
  if(!identical(names(chosen_signals_and_positions), unique(names(chosen_signals_and_positions)))){
    stop("each signal must be chosen only once")
  }
  ###Check for presence of low_
  if(any(grepl("low_", names(chosen_signals_and_positions)))){
    stop("chosen_signals_and_positions should not contain 'low_'.")
  }

  #Create default parameters for signal_port_parameters depending on sb_algo
  if(sb_algorithm %in% c("mvo", "rp") && is.null(signal_port_parameters)){
    cov_est_method <- create_cov_est_method(cov_estimation_method = "sample", cov_matrix_sample_size = 36, active_returns = TRUE, cov_matrix_benchmark = "IBOV")
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
      chosen_signals_and_positions = chosen_signals_and_positions,
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
#' @param base_ss_backtest_configs A list of `ss_backtest_config` objects (optional).
#' @param base_ss_backtest_results A list of `ss_backtest_results` objects (optional)
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
setGeneric("create_sb_metabacktest_config", function(meta_sb_backtest_config,
                                                     base_sb_backtest_configs, base_sb_backtest_results,
                                                     base_ss_backtest_configs, base_ss_backtest_results, ...) standardGeneric("create_sb_metabacktest_config"))


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
          signature(meta_sb_backtest_config = "sb_backtest_config",
                    base_sb_backtest_configs = "list", base_sb_backtest_results = "missing",
                    base_ss_backtest_configs = "list", base_ss_backtest_results = "missing"),

          function(meta_sb_backtest_config, base_sb_backtest_configs, base_ss_backtest_configs,
                   features_passthrough = "none", normalize_base_predictions = TRUE, winsorize_base_predictions = TRUE,
                   config_name = "not_identified", ...) {

            # Check that all base_sb_backtest_configs are sb_backtest_config objects
            if (!all(sapply(base_sb_backtest_configs, function(x) is(x, "sb_backtest_config")))) {
              stop("All elements in 'base_sb_backtest_configs' must be 'sb_backtest_config' objects.")
            }

            # Check that all ss_backtest_config are ss_backtest_config objects
            if (!all(sapply(base_ss_backtest_configs, function(x) is(x, "ss_backtest_config")))) {
              stop("All elements in 'base_ss_backtest_configs' must be 'ss_backtest_config' objects.")
            }

            # Get config names
            sb_config_names <- as.character(sapply(base_sb_backtest_configs, function(x) x@config_name))
            ss_config_names <- as.character(sapply(base_ss_backtest_configs, function(x) x@config_name))

            combined_configs <- list()


            # Iterate through each sb and ss configurations:
            for (i in seq_along(base_sb_backtest_configs)) {
              sb_config <- base_sb_backtest_configs[[i]]

              if (!is.null(sb_config@ss_backtest_config) || !is.null(sb_config@ss_backtest_results)) {
                stop("All elements in 'base_sb_backtest_configs' must have 'ss_backtest_config' and 'ss_backtest_results' set to NULL.")
              }

              for (j in seq_along(base_ss_backtest_configs)) {
                ss_config <- base_ss_backtest_configs[[j]]

                # Add the ss_backtest_config
                new_config <- add_ss_backtest_obj(sb_config, ss_config)

                # Add it to combined_configs
                combined_name <- paste0(sb_config_names[i], "_", ss_config_names[j])
                combined_configs[[combined_name]] <- new_config

              }
            }

            # Warn about not considering chosen_signals_and_positions at meta-level
            if (length(meta_sb_backtest_config@ss_backtest_config) > 0){
              message("The chosen_signals_and_positions parameter of the meta-level ss_backtest_config will not be considered.",
                      "This is because selection of features for meta-learner are chosen via features_passthrough, with positions derived by base-level chosen_signal_and_positions to ensure consistency.")
              meta_sb_backtest_config@ss_backtest_config@chosen_signals_and_positions <- "all"
            }
            if (length(meta_sb_backtest_config@ss_backtest_results) > 0){
              message("Please be sure if backtested signals of ss_backtest_results contemplate base-learners backtests + features_passthrough.")
            }
            if (length(meta_sb_backtest_config@chosen_signals_and_positions) == 1 && meta_sb_backtest_config@chosen_signals_and_positions != "ss_backtest_obj"){
              message("The chosen_signals_and_positions parameter of the meta-level sb_backtest_config will not be considered.",
                      "This is because selection of features for meta-learner are chosen via features_passthrough, with positions derived by base-level chosen_signal_and_positions to ensure consistency.")
              meta_sb_backtest_config@chosen_signals_and_positions <- "all"
            }


            # Create the sb_metabacktest_config object
            meta_config <- new("sb_metabacktest_config", meta_sb_backtest_config = meta_sb_backtest_config,
                               base_sb_backtest_configs = combined_configs,
                               base_sb_backtest_results = NULL,
                               features_passthrough = features_passthrough,
                               normalize_base_predictions = normalize_base_predictions,
                               winsorize_base_predictions = winsorize_base_predictions,
                               config_name = config_name)

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
          signature(meta_sb_backtest_config = "sb_backtest_config",
                    base_sb_backtest_configs = "list", base_sb_backtest_results = "missing",
                    base_ss_backtest_configs = "missing", base_ss_backtest_results = "list"),

          function(meta_sb_backtest_config, base_sb_backtest_configs, base_ss_backtest_results,
                   features_passthrough = "none", normalize_base_predictions = TRUE, winsorize_base_predictions = TRUE,
                   config_name = "not_identified", ...) {

            # Check that all base_sb_backtest_configs are sb_backtest_config objects
            if (!all(sapply(base_sb_backtest_configs, function(x) is(x, "sb_backtest_config")))) {
              stop("All elements in 'base_sb_backtest_configs' must be 'sb_backtest_config' objects.")
            }

            # Check that all ss_backtest_config are ss_backtest_results objects
            if (!all(sapply(base_ss_backtest_results, function(x) is(x, "ss_backtest_results")))) {
              stop("All elements in 'base_ss_backtest_results' must be 'ss_backtest_results' objects.")
            }

            # Get config names
            sb_config_names <- as.character(sapply(base_sb_backtest_configs, function(x) x@config_name))
            ss_results_names <- as.character(sapply(base_ss_backtest_results, function(x) x@backtest_identifier))

            combined_configs <- list()


            # Iterate through each sb and ss configurations:
            for (i in seq_along(base_sb_backtest_configs)) {
              sb_config <- base_sb_backtest_configs[[i]]

              if (!is.null(sb_config@ss_backtest_config) || !is.null(sb_config@ss_backtest_results)) {
                stop("All elements in 'base_sb_backtest_configs' must have 'ss_backtest_config' and 'ss_backtest_results' set to NULL.")
              }

              for (j in seq_along(base_ss_backtest_results)) {
                ss_results <- base_ss_backtest_results[[j]]

                # Add the ss_backtest_results
                new_config <- add_ss_backtest_obj(sb_config, ss_results)

                # Add it to combined_configs
                combined_name <- paste0(sb_config_names[i], "_", ss_results_names[j])
                combined_configs[[combined_name]] <- new_config

              }
            }

            # Warn about not considering chosen_signals_and_positions at meta-level
            if (length(meta_sb_backtest_config@ss_backtest_config) > 0){
              message("The chosen_signals_and_positions parameter of the meta-level ss_backtest_config will not be considered.",
                      "This is because selection of features for meta-learner are chosen via features_passthrough, with positions derived by base-level chosen_signal_and_positions to ensure consistency.")
              meta_sb_backtest_config@ss_backtest_config@chosen_signals_and_positions <- "all"
            }
            if (length(meta_sb_backtest_config@ss_backtest_results) > 0){
              message("Please be sure if backtested signals of ss_backtest_results contemplate base-learners backtests + features_passthrough.")
            }
            if (length(meta_sb_backtest_config@chosen_signals_and_positions) == 1 && meta_sb_backtest_config@chosen_signals_and_positions != "ss_backtest_obj"){
              message("The chosen_signals_and_positions parameter of the meta-level sb_backtest_config will not be considered.",
                      "This is because selection of features for meta-learner are chosen via features_passthrough, with positions derived by base-level chosen_signal_and_positions to ensure consistency.")
              meta_sb_backtest_config@chosen_signals_and_positions <- "all"
            }

            # Create the sb_metabacktest_config object
            meta_config <- new("sb_metabacktest_config", meta_sb_backtest_config = meta_sb_backtest_config,
                               base_sb_backtest_configs = combined_configs,
                               base_sb_backtest_results = NULL,
                               features_passthrough = features_passthrough,
                               normalize_base_predictions = normalize_base_predictions,
                               winsorize_base_predictions = winsorize_base_predictions,
                               config_name = config_name)

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
          signature(meta_sb_backtest_config = 'sb_backtest_config',
                    base_sb_backtest_configs = "list", base_sb_backtest_results = "missing",
                    base_ss_backtest_configs = "missing", base_ss_backtest_results = "missing"),

          function(meta_sb_backtest_config, base_sb_backtest_configs, config_name = "not_identified",
                   features_passthrough = "none",
                   normalize_base_predictions = TRUE, winsorize_base_predictions = TRUE,
                   ...) {

            # Check that all configs are sb_backtest_config objects
            if (!all(sapply(base_sb_backtest_configs, function(x) is(x, "sb_backtest_config")))) {
              stop("All elements in 'base_sb_backtest_configs' must be 'sb_backtest_config' objects.")
            }

            # Warn about not considering chosen_signals_and_positions at meta-level
            if (length(meta_sb_backtest_config@ss_backtest_config) > 0){
              message("The chosen_signals_and_positions parameter of the meta-level ss_backtest_config will not be considered.",
                      "This is because selection of features for meta-learner are chosen via features_passthrough, with positions derived by base-level chosen_signal_and_positions to ensure consistency.")
              meta_sb_backtest_config@ss_backtest_config@chosen_signals_and_positions <- "all"
            }
            if (length(meta_sb_backtest_config@ss_backtest_results) > 0){
              message("Please be sure if backtested signals of ss_backtest_results contemplate base-learners backtests + features_passthrough.")
            }
            if (length(meta_sb_backtest_config@chosen_signals_and_positions) == 1 && meta_sb_backtest_config@chosen_signals_and_positions != "ss_backtest_obj"){
              message("The chosen_signals_and_positions parameter of the meta-level sb_backtest_config will not be considered.",
                      "This is because selection of features for meta-learner are chosen via features_passthrough, with positions derived by base-level chosen_signal_and_positions to ensure consistency.")
              meta_sb_backtest_config@chosen_signals_and_positions <- "all"
            }

            # Create the sb_metabacktest_config object
            meta_config <- new("sb_metabacktest_config", meta_sb_backtest_config = meta_sb_backtest_config, base_sb_backtest_configs = base_sb_backtest_configs,
                               base_sb_backtest_results = NULL,
                               features_passthrough = features_passthrough,
                               normalize_base_predictions = normalize_base_predictions,
                               winsorize_base_predictions = winsorize_base_predictions,
                               config_name = config_name)
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
          signature(meta_sb_backtest_config = 'sb_backtest_config',
                    base_sb_backtest_configs = "missing", base_sb_backtest_results = "list",
                    base_ss_backtest_configs = "missing", base_ss_backtest_results = "missing"),

          function(meta_sb_backtest_config, base_sb_backtest_results, config_name = "not_identified",
                   features_passthrough = "none",
                   normalize_base_predictions = TRUE, winsorize_base_predictions = TRUE,
                   ...) {

            # Check that all configs are sb_backtest_config objects
            if (!all(sapply(base_sb_backtest_results, function(x) is(x, "sb_backtest_results")))) {
              stop("All elements in 'base_sb_backtest_results' must be 'sb_backtest_results' objects.")
            }

            # Warn about not considering chosen_signals_and_positions at meta-level
            if (length(meta_sb_backtest_config@ss_backtest_config) > 0){
              message("The chosen_signals_and_positions parameter of the meta-level ss_backtest_config will not be considered.",
                      "This is because selection of features for meta-learner are chosen via features_passthrough, with positions derived by base-level chosen_signal_and_positions to ensure consistency.")
              meta_sb_backtest_config@ss_backtest_config@chosen_signals_and_positions <- "all"
            }
            if (length(meta_sb_backtest_config@ss_backtest_results) > 0){
              message("Please be sure if backtested signals of ss_backtest_results contemplate base-learners backtests + features_passthrough.")
            }
            if (length(meta_sb_backtest_config@chosen_signals_and_positions) == 1 && meta_sb_backtest_config@chosen_signals_and_positions != "ss_backtest_obj"){
              message("The chosen_signals_and_positions parameter of the meta-level sb_backtest_config will not be considered.",
                      "This is because selection of features for meta-learner are chosen via features_passthrough, with positions derived by base-level chosen_signal_and_positions to ensure consistency.")
              meta_sb_backtest_config@chosen_signals_and_positions <- "all"
            }


            # Create the sb_metabacktest_config object
            meta_config <- new("sb_metabacktest_config",
                               meta_sb_backtest_config = meta_sb_backtest_config, base_sb_backtest_configs = NULL,
                               base_sb_backtest_results = base_sb_backtest_results,
                               features_passthrough = features_passthrough,
                               normalize_base_predictions = normalize_base_predictions,
                               winsorize_base_predictions = winsorize_base_predictions,
                               config_name = config_name)
            return(meta_config)
          }
)

#' Add one or more sb_backtest_config objects to a sb_metabacktest_config or port_backtest_config
#'
#' @param object A `sb_metabacktest_config` or a `port_backtest_config` object.
#' @param ... One or more `sb_backtest_config` objects to add.
#'
#' @return An updated `sb_metabacktest_config` or `port_backtest_config` object with added configurations.
#' @export
setGeneric("add_sb_backtest_config", function(object, ...) standardGeneric("add_sb_backtest_config"))

#' @describeIn add_sb_backtest_config Add one or more sb_backtest_config objects to a sb_metabacktest_config
setMethod("add_sb_backtest_config", "sb_metabacktest_config", function(object, new_configs, ...) {

  # Check that new_configs is a list os base_sb_backtest_configs
  # Check that all new_configs are complete sb_backtest_config objects
  if(!all(sapply(new_configs, function(x){
    (!x@sb_algorithm %in% c("ols", "rp", "mvo", "ew", "sw", "custom") && !is.null(x@tuning_strategy))
  }))){
    stop("All elements in 'new_configs' must be complete (with tuning_strategy) 'sb_backtest_config' objects.")
  }

  # Combine the current base_sb_backtest_configs with the new configurations
  object@base_sb_backtest_configs <- c(object@base_sb_backtest_configs, new_configs)

  # Validate the object explicitly
  validObject(object)

  # Return the updated object
  return(object)
})

#' @describeIn add_sb_backtest_config Add one or more sb_backtest_config objects to a port_backtest_config
setMethod("add_sb_backtest_config", "port_backtest_config", function(object, new_config, ...){

  # Check that new_config is a complete sb_backtest_config object
  if(!new_config@sb_algorithm %in% c("ols", "rp", "mvo", "ew", "sw", "custom") &&
     !is.null(new_config@tuning_strategy)){
    stop("'new_config' must be complete (with tuning_strategy) 'sb_backtest_config' object.")
  }

  #Add obj
  object@sb_backtest_config <- new_config

  # Validate the object explicitly
  validObject(object)

  # Return the updated object
  return(object)

})

#' Add one sb_backtest_results object to a port_backtest_config
#'
#' @param object A`port_backtest_config` object.
#' @param  One or more `sb_backtest_results` objects to add.
#'
#' @return An updated `port_backtest_config` object with added sb_backtest_results.
#' @export
setGeneric("add_sb_backtest_results", function(object, ...) standardGeneric("add_sb_backtest_results"))

setMethod("add_sb_backtest_results", "port_backtest_config", function(object, sb_backtest_results, ...){

  #Add obj
  object@sb_backtest_results <- sb_backtest_results

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
#' @param meta_sb_backtest_results  A `sb_backtest_results` object for the meta learner
#' @param base_sb_backtest_results_list A named list of `sb_backtest_results` objects for the base learners.
#' @return An object of class `sb_metabacktest_results`.
#'
setGeneric(
  name = "create_sb_metabacktest_results",
  def = function(meta_sb_backtest_results, base_sb_backtest_results_list, oos_predictions_m_df,
                 sb_metabacktest_config, ...) {
    standardGeneric("create_sb_metabacktest_results")
  }
)

#' @rdname create_sb_metabacktest_results
#' @aliases create_sb_metabacktest_results,list-method
setMethod(
  f = "create_sb_metabacktest_results",
  signature = signature(
    meta_sb_backtest_results = "sb_backtest_results",
    base_sb_backtest_results_list = "list",
    oos_predictions_m_df = "meta_dataframe",
    sb_metabacktest_config = "sb_metabacktest_config"
  ),
  definition = function(meta_sb_backtest_results, base_sb_backtest_results_list, oos_predictions_m_df, sb_metabacktest_config) {

    #Check that the base_sb_backtest_results input is a list of 'sb_backtest_results' objects
    if (!all(sapply(base_sb_backtest_results_list, function(x) is(x, "sb_backtest_results")))) {
      stop("All elements in 'base_sb_backtest_results_list' must be of class 'sb_backtest_results'")
    }

    #Get all objects
    all_sb_backtest_results <- c(base_sb_backtest_results_list, meta_sb_backtest_results)
    #Get names
    base_sb_names <- names(base_sb_backtest_results_list)
    meta_sb_name <- meta_sb_backtest_results@backtest_identifier


    #Call the helper consolidate function
    results <- consolidate_sb_metabacktest_results(
      all_sb_backtest_results = all_sb_backtest_results,
      meta_sb_name = meta_sb_name,
      base_sb_names = base_sb_names
    )

    # Create the sb_metabacktest_results object
    new_object <- new(
      "sb_metabacktest_results",
      sb_metabacktest_config = sb_metabacktest_config,
      meta_sb_backtest_results = meta_sb_backtest_results,
      base_sb_backtest_results_list = base_sb_backtest_results_list,
      base_learners_oos_predictions_m_df = oos_predictions_m_df,
      combined_oos_testing_metrics = list(
        all_dates_oos_testing_metrics = results$all_dates_oos_testing_metrics,
        common_dates_oos_testing_metrics = results$common_dates_oos_testing_metrics
      ),
      mean_validation_metrics = results$mean_validation_metrics,
      time_series_oos_testing_metrics = results$time_series_oos_testing_metrics,
      time_series_validation_metrics = results$time_series_validation_metrics,
      backtest_identifier = meta_sb_backtest_results@backtest_identifier
    )

    return(new_object)
  }
)

#-----------------------------------------------------------------------
# port_backtest_config
#-----------------------------------------------------------------------

#' @title Create port_backtest_config Object
#' @description Constructs a `port_backtest_config` object containing all necessary parameters for backtesting stock-level portfolios.
#'
#' @param chosen_score_metric_and_position An object (or named vector) specifying the expected return score metric and its associated position. Required if `sb_backtest_results` is not provided.
#' @param eligibility_quantile_range A numeric vector of length 2 (e.g., c(0.9, 1.0)) specifying the quantile range used to determine eligible assets.
#' @param selected_benchmark A character string indicating the benchmark to use for benchmark-relative backtests.
#' @param initial_buffer_period A numeric value indicating the number of initial dates to skip before starting the backtest.
#' @param rebalancing_months A numeric vector (e.g., c(3,6,9,12)) indicating the months when the portfolio should be rebalanced.
#' @param cov_est_method A `cov_est_method` object specifying the covariance estimation method and its parameters.
#' If not provided, a default using method "sample", sample size 36, active returns = TRUE, and the provided selected_benchmark is created.
#' @param port_construction_method A character string representing the portfolio construction method.
#' Must be one of "ew", "sw", "cw", "cs", "rp", or "mvo".
#' @param mvo_parameters An object of class `mvo_parameters` for mean-variance optimization. Only required if `port_construction_method` is "mvo".
#' If missing and port_construction_method is "mvo", a default is created.
#' @param rp_parameters An object of class `rp_parameters` for risk parity portfolios. Only required if `port_construction_method` is "rp".
#' If missing and port_construction_method is "rp", a default is created.
#' @param sb_backtest_results An object of class `sb_backtest_results`. Must be NULL if using an independent signal backtest configuration.
#' @param main_liquidity_metric A character string indicating which liquidity metric (i.e. column in liquidity_m_df) to use.
#' @param liquidity_floor_cutoffs An object (e.g., a data frame) containing liquidity cutoff values.
#' @param liquidity_constraint_policy An object of class `liquidity_constraint_policy` (optional).
#' @param turnover_constraint_policy An object of class `turnover_constraint_policy` (optional).
#' @param concentration_constraint_policy An object of class `concentration_constraint_policy` (optional).
#' @param transaction_costs_parameters An object specifying transaction cost parameters (optional).
#' @param config_name A character string representing the name of the configuration.
#'
#' @return An object of class `port_backtest_config`.
#' @export
create_port_backtest_config <- function(chosen_score_metric_and_position = NULL,
                                        eligibility_quantile_range = c(0.9, 1.0),
                                        min_eligible_assets_fallback = NULL,
                                        selected_benchmark = NULL,
                                        initial_buffer_period,
                                        rebalancing_months,
                                        cov_est_method = NULL,
                                        port_construction_method = "ew",
                                        mvo_parameters = NULL,
                                        rp_parameters = NULL,
                                        sb_backtest_config = NULL,
                                        sb_backtest_results = NULL,
                                        main_liquidity_metric,
                                        liquidity_floor_cutoffs = NULL,
                                        liquidity_constraint_policy = NULL,
                                        turnover_constraint_policy = NULL,
                                        concentration_constraint_policy = NULL,
                                        transaction_costs_parameters = NULL,
                                        config_name = "not_identified") {

  # Create a default covariance estimation method if none is provided
  if (is.null(cov_est_method)) {
    if (!is.null(selected_benchmark)){
      cov_est_method <- create_cov_est_method(
        cov_estimation_method = "sample",
        cov_matrix_sample_size = 252,
        active_returns = TRUE,
        cov_matrix_benchmark = selected_benchmark
      )
    } else {
      cov_est_method <- create_cov_est_method(
        cov_estimation_method = "sample",
        cov_matrix_sample_size = 252,
        active_returns = FALSE,
        cov_matrix_benchmark = NULL
      )
    }
  }

  # If the portfolio construction method is "mvo" but no mvo_parameters provided, create defaults
  if (port_construction_method == "mvo" && is.null(mvo_parameters)) {
    mvo_parameters <- create_mvo_parameters(
      opt_method = "random",
      random_ports_method = "sample",
      n_random_ports = 1000,
      opt_objective = "sharpe"
    )
  }

  # Similarly, if the method is "rp" and no rp_parameters are provided, create defaults
  if (port_construction_method == "rp" && is.null(rp_parameters)) {
    rp_parameters <- create_rp_parameters(
      rp_method = "cyclical-spinu"
    )
  }

  # Ensure that at least one of sb_backtest_results or chosen_score_metric_and_position is provided
  if (is.null(sb_backtest_results) && is.null(chosen_score_metric_and_position)) {
    stop("chosen_score_metric_and_position must be provided if sb_backtest_results is NULL.")
  }

  # Create and return the new port_backtest_config object
  new("port_backtest_config",
      chosen_score_metric_and_position = chosen_score_metric_and_position,
      min_eligible_assets_fallback = min_eligible_assets_fallback,
      eligibility_quantile_range = eligibility_quantile_range,
      selected_benchmark = selected_benchmark,
      initial_buffer_period = initial_buffer_period,
      rebalancing_months = rebalancing_months,
      cov_est_method = cov_est_method,
      port_construction_method = port_construction_method,
      mvo_parameters = mvo_parameters,
      rp_parameters = rp_parameters,
      sb_backtest_config = sb_backtest_config,
      sb_backtest_results = sb_backtest_results,
      main_liquidity_metric = main_liquidity_metric,
      liquidity_floor_cutoffs = liquidity_floor_cutoffs,
      liquidity_constraint_policy = liquidity_constraint_policy,
      turnover_constraint_policy = turnover_constraint_policy,
      concentration_constraint_policy = concentration_constraint_policy,
      transaction_costs_parameters = transaction_costs_parameters,
      config_name = config_name
  )
}


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

            #Check for a selected_benchmark
            if (length(object@selected_benchmark) == 0) {
              stop("A selected_benchmark must be provided to add a concentration constraint policy.")
            }

            #Check if port_construction_method is 'mvo'
            if (object@port_construction_method != 'mvo') {
              stop("Concentration constraint policy can only be added to a 'mvo' port_construction_method.")
            }

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
                   max_abs_active_individual_weight = NULL,
                   max_abs_active_group_weight = NULL,
                   ...) {

            #Check for a selected_benchmark
            if (length(object@selected_benchmark) == 0) {
              stop("A selected_benchmark must be provided to add a concentration constraint policy.")
            }

            # Get benchmark from object
            selected_benchmark <- object@selected_benchmark

            #Check if port_construction_method is 'mvo'
            if (object@port_construction_method != 'mvo') {
              stop("Concentration constraint policy can only be added to a 'mvo' port_construction_method.")
            }

            # Build a new policy on the fly
            new_policy <- create_concentration_constraint_policy(
              benchmark = selected_benchmark,
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
                   benchmark,
                   max_abs_active_individual_weight = NULL,
                   max_abs_active_group_weight = NULL,
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

            # Assign
            object@signal_port_parameters@concentration_constraint_policy <- new_policy
            methods::validObject(object)
            return(object)
          }
)


#-----------------------------------------------------------------------
# liquidity_constraint_policy
#-----------------------------------------------------------------------
#' @title Create Liquidity Constraint Policy
#' @description Constructor for a `liquidity_constraint_policy` object.
#'
#' @param liquidity_floor_rule A character string indicating the minimum liquidity classification.
#' @param liquidity_cap_rules A named numeric vector indicating liquidity cap rules.
#'
#' @return An S4 object of class `liquidity_constraint_policy`.
#' @export
create_liquidity_constraint_policy <- function(liquidity_floor_rule = NULL,
                                               liquidity_cap_rules = NULL) {
  obj <- new("liquidity_constraint_policy",
             liquidity_floor_rule = liquidity_floor_rule,
             liquidity_cap_rules = liquidity_cap_rules)
  validObject(obj)
  obj
}

#' @title Add Liquidity Constraint Policy
#' @description Add an existing or dynamically create a `liquidity_constraint_policy`
#' to a portfolio backtest configuration object.
#'
#' @param object An object of class `port_backtest_config`.
#' @param policy A `liquidity_constraint_policy` object. If missing, a new one is created.
#' @param liquidity_floor_rule A character string (see details) used to create a new policy when `policy` is missing.
#' @param liquidity_cap_rules A named numeric vector used to create a new policy when `policy` is missing.
#' @param ... Additional arguments (currently unused).
#'
#' @return The updated `object` with the liquidity constraint policy added.
#' @export
setGeneric("add_liquidity_constraint_policy", function(object, policy, ...) {
  standardGeneric("add_liquidity_constraint_policy")
})

#' @describeIn add_liquidity_constraint_policy
#'   Add an existing `liquidity_constraint_policy` to a `port_backtest_config`.
#' @export
setMethod("add_liquidity_constraint_policy",
          signature(object = "port_backtest_config", policy = "liquidity_constraint_policy"),
          function(object, policy, ...) {
            object@liquidity_constraint_policy <- policy

            validObject(object)
            return(object)
          }
)

#' @describeIn add_liquidity_constraint_policy
#'   Dynamically create a `liquidity_constraint_policy` and add it to a `port_backtest_config`.
#' @export
setMethod("add_liquidity_constraint_policy",
          signature(object = "port_backtest_config", policy = "missing"),
          function(object, policy, liquidity_floor_rule = NULL, liquidity_cap_rules = NULL, ...) {

            new_policy <- create_liquidity_constraint_policy(
              liquidity_floor_rule = liquidity_floor_rule,
              liquidity_cap_rules = liquidity_cap_rules
            )
            object@liquidity_constraint_policy <- new_policy

            validObject(object)
            return(object)
          }
)




#-----------------------------------------------------------------------
# turnover_constraint_policy
#-----------------------------------------------------------------------
#' @title Create Turnover Constraint Policy
#' @description Constructor for a `turnover_constraint_policy` object.
#'
#' @param quantile_range_buffer A numeric value indicating the increase in the quantile range for buffer zones.
#' @param turnover_cap_rules A named numeric vector indicating turnover cap rules.
#'
#' @return An S4 object of class `turnover_constraint_policy`.
#' @export
create_turnover_constraint_policy <- function(quantile_range_buffer,
                                              turnover_cap_rules = NULL) {
  obj <- methods::new("turnover_constraint_policy",
                      quantile_range_buffer = quantile_range_buffer,
                      turnover_cap_rules = turnover_cap_rules)
  methods::validObject(obj)
  obj
}

#' @title Add Turnover Constraint Policy
#' @description Add an existing or dynamically create a `turnover_constraint_policy`
#' to a portfolio backtest configuration object.
#'
#' @param object An object of class `port_backtest_config` or `sb_backtest_config`.
#' @param policy A `turnover_constraint_policy` object. If missing, a new one is created.
#' @param quantile_range_buffer A numeric value used to create a new policy when `policy` is missing.
#' @param turnover_cap_rules A named numeric vector used to create a new policy when `policy` is missing.
#' @param ... Additional arguments (currently unused).
#'
#' @return The updated `object` with the turnover constraint policy added.
#' @export
setGeneric("add_turnover_constraint_policy", function(object, policy, ...) {
  standardGeneric("add_turnover_constraint_policy")
})

#' @describeIn add_turnover_constraint_policy
#'   Add an existing `turnover_constraint_policy` to a `port_backtest_config`.
#' @export
setMethod("add_turnover_constraint_policy",
          signature(object = "port_backtest_config", policy = "turnover_constraint_policy"),
          function(object, policy, ...) {
            #Check if port_construction_method is 'mvo'
            if (object@port_construction_method != 'mvo') {
              stop("Concentration constraint policy can only be added to a 'mvo' port_construction_method.")
            }

            object@turnover_constraint_policy <- policy
            methods::validObject(object)
            return(object)
          }
)

#' @describeIn add_turnover_constraint_policy
#'   Dynamically create a `turnover_constraint_policy` and add it to a `port_backtest_config`.
#' @export
setMethod("add_turnover_constraint_policy",
          signature(object = "port_backtest_config", policy = "missing"),
          function(object, policy, quantile_range_buffer, turnover_cap_rules, ...) {

            #Check if port_construction_method is 'mvo'
            if (object@port_construction_method != 'mvo') {
              stop("Concentration constraint policy can only be added to a 'mvo' port_construction_method.")
            }

            new_policy <- create_turnover_constraint_policy(
              quantile_range_buffer = quantile_range_buffer,
              turnover_cap_rules = turnover_cap_rules
            )
            object@turnover_constraint_policy <- new_policy
            methods::validObject(object)
            return(object)
          }
)

#-----------------------------------------------------------------------
# transaction_cost_parameters
#-----------------------------------------------------------------------
#' Create a New Transaction Cost Parameters Object
#'
#' This function constructs a new \code{transaction_cost_parameters} S4 object.
#'
#' @param direct_transaction_cost A numeric value for the direct transaction cost.
#' @param strategy_aum A numeric value for the strategy's assets under management.
#' @param alpha A numeric value for the alpha parameter.
#' @param lambda A numeric value or the string "dynamic" for the lambda parameter.
#'
#' @return An object of class \code{transaction_cost_parameters}.
#'
#' @export
create_transaction_costs_parameters <- function(direct_transaction_cost, strategy_aum, alpha, lambda) {

  transaction_costs_parameters <- list(
    direct_transaction_cost = direct_transaction_cost,
    strategy_aum = strategy_aum,
    alpha = alpha,
    lambda = lambda
  )

  # Validate the parameters using the validation function
  validate_transaction_costs_parameters(transaction_costs_parameters)

  methods::new("transaction_costs_parameters",
               direct_transaction_cost = direct_transaction_cost,
               strategy_aum = strategy_aum,
               alpha = alpha,
               lambda = lambda)
}

#' Add Transaction Cost Parameters to a Portfolio Backtest Configuration
#'
#' This generic function adds an existing or dynamically creates a
#' \code{transaction_cost_parameters} object to a portfolio backtest configuration object.
#'
#' @param object An object of class \code{port_backtest_config}.
#' @param transaction_costs_parameters A \code{transaction_cost_parameters} object. If missing, a new one is created.
#' @param ... Additional arguments used when creating a new transaction cost parameters object.
#'
#' @return The updated \code{object} with the transaction cost parameters added.
#'
#' @export
setGeneric("add_transaction_costs_parameters", function(object, transaction_costs_parameters, ...) {
  standardGeneric("add_transaction_costs_parameters")
})

#' @describeIn add_transaction_costs_parameters
#'   Add an existing \code{transaction_cost_parameters} to a \code{port_backtest_config}.
#' @export
setMethod("add_transaction_costs_parameters",
          signature(object = "port_backtest_config", transaction_costs_parameters = "transaction_costs_parameters"),
          function(object, transaction_costs_parameters, ...) {
            object@transaction_costs_parameters <- transaction_costs_parameters
            methods::validObject(object)
            return(object)
          }
)

#' @describeIn add_transaction_costs_parameters
#'   Dynamically create a \code{transaction_cost_parameters} object and add it to a \code{port_backtest_config}.
#'
#'   Additional arguments (such as \code{direct_transaction_cost}, \code{strategy_aum}, \code{alpha}, and \code{lambda})
#'   are passed to \code{new_transaction_cost_parameters}.
#' @export
setMethod("add_transaction_costs_parameters",
          signature(object = "port_backtest_config", transaction_costs_parameters = "missing"),
          function(object, transaction_costs_parameters, ...) {
            new_tc_params <- create_transaction_costs_parameters(...)
            object@transaction_costs_parameters <- new_tc_params
            methods::validObject(object)
            return(object)
          }
)


#-----------------------------------------------------------------------
# liquidity_floor_cutoffs
#-----------------------------------------------------------------------

#' @title Create Liquidity Floor Cutoffs
#' @description Construct a liquidity_floor_cutoffs data frame from scratch.
#'
#' @param metric_name A character vector of metric names.
#' @param metric_cutoffs A list of named numeric vectors, one for each metric in
#'   metric_name. Each vector must have names exactly equal to
#'   c("micro_caps", "small_caps", "mid_caps", "large_caps", "mega_caps") (order may vary).
#'
#' @return A data.frame with a column `liquidity_classification` and one column per metric.
#'   The rows are ordered (after reordering each metric vector according to the allowed levels)
#'   so that the main liquidity metric (assumed to be the first element of metric_name)
#'   is in non-decreasing order.
#'
#' @details The function enforces that:
#'   \itemize{
#'     \item The returned object is a data.frame.
#'     \item The column names (besides `liquidity_classification`) are numeric with no NAs.
#'     \item The `liquidity_classification` values are exactly the allowed levels:
#'       "micro_caps", "small_caps", "mid_caps", "large_caps", "mega_caps".
#'     \item The main liquidity metric (the first metric in metric_name) is in ascending order.
#'     \item The ordering (ranking) of liquidity classifications is consistent across metrics.
#'   }
#'
#' @export
create_liquidity_floor_cutoffs <- function(metric_name, metric_cutoffs) {
  # Allowed liquidity classifications
  allowed_levels <- c("micro_caps", "small_caps", "mid_caps", "large_caps", "mega_caps")

  # Check that lengths match
  if (length(metric_name) != length(metric_cutoffs)) {
    stop("Length of metric_name must equal length of metric_cutoffs")
  }

  # Process each metric: check numeric, names, and reorder according to allowed_levels.
  metrics_list <- list()
  for (i in seq_along(metric_name)) {
    vec <- metric_cutoffs[[i]]
    if (!is.numeric(vec)) {
      stop(paste0("Metric '", metric_name[i], "' must be numeric"))
    }
    if (is.null(names(vec))) {
      stop(paste0("Metric '", metric_name[i], "' must be a named vector"))
    }
    if (!setequal(names(vec), allowed_levels)) {
      stop(paste0("Metric '", metric_name[i], "' must have names exactly: ",
                  paste(allowed_levels, collapse = ", ")))
    }
    # Reorder the vector so that its values are arranged by the allowed levels.
    vec <- vec[allowed_levels]
    metrics_list[[ metric_name[i] ]] <- vec
  }

  # Build the data.frame: first column is liquidity_classification.
  df <- data.frame(liquidity_classification = allowed_levels, stringsAsFactors = FALSE)
  for (nm in metric_name) {
    df[[nm]] <- unname(metrics_list[[nm]])
  }

  # Ensure all metric columns are numeric and free of NAs.
  if (!all(sapply(df[, -1, drop = FALSE], is.numeric))) {
    stop("All metric columns must be numeric")
  }
  if (any(is.na(df))) {
    stop("liquidity_floor_cutoffs elements must not have NAs")
  }

  # Check that the main liquidity metric (first metric) is in ascending order.
  main_metric <- metric_name[1]
  if (!all(diff(df[[main_metric]]) >= 0)) {
    stop("liquidity_floor_cutoffs is not in ascending order based on the main liquidity metric")
  }

  # Check that ordering of all metrics is consistent.
  orders_matrix <- sapply(df[, metric_name, drop = FALSE], function(x) order(x))
  for (i in seq_len(nrow(orders_matrix))) {
    if (length(unique(orders_matrix[i, ])) != 1) {
      stop("liquidity metrics orders in liquidity_floor_cutoffs are conflicting")
    }
  }

  return(df)
}

## ---- add_liquidity_floor_cutoffs ----

#' @title Add Liquidity Floor Cutoffs
#' @description Add or update liquidity floor cutoff values in an existing object.
#'
#' @param object An object of class `port_backtest_config` or `sb_backtest_config`.
#'   The object must have a slot `liquidity_floor_cutoffs` (which may be NULL)
#'   and a character slot `main_liquidity_metric`.
#' @param metric_name A character vector (or a single character) specifying the metric(s)
#'   to add or update.
#' @param metric_cutoffs For each metric, a named numeric vector containing cutoff values.
#'   For a single metric, this can be a named numeric vector.
#'
#' @return The updated `object` with its `liquidity_floor_cutoffs` slot merged with the new values.
#'
#' @details If the object already has a liquidity_floor_cutoffs data.frame, the function:
#'   \itemize{
#'     \item Ensures that rows exist for all allowed liquidity classifications.
#'     \item For each provided metric, if the metric column already exists, it updates the rows
#'       corresponding to the names provided in `metric_cutoffs`; otherwise a new column is added.
#'     \item After merging, the resulting data.frame is validated against the enforced structure:
#'       it must be a data.frame, contain the main liquidity metric, be sorted in ascending order,
#'       have consistent orders across metrics, contain only allowed liquidity classifications,
#'       have numeric columns (except for the classification column) and contain no NAs.
#'   }
#'
#' @export
add_liquidity_floor_cutoffs <- function(object, metric_name, metric_cutoffs) {
  # Allowed liquidity classifications
  allowed_levels <- c("micro_caps", "small_caps", "mid_caps", "large_caps", "mega_caps")

  # Ensure metric_cutoffs is a list when multiple metrics are provided.
  if (length(metric_name) > 1 && !is.list(metric_cutoffs)) {
    stop("When multiple metric names are provided, metric_cutoffs must be a list")
  }
  if (length(metric_name) == 1 && !is.list(metric_cutoffs)) {
    metric_cutoffs <- list(metric_cutoffs)
  }

  # If no liquidity_floor_cutoffs exists, create one from scratch.
  if (is.null(object@liquidity_floor_cutoffs)) {
    # For a new liquidity_floor_cutoffs, each metric must provide values for all allowed levels.
    for (i in seq_along(metric_name)) {
      vec <- metric_cutoffs[[i]]
      if (!setequal(names(vec), allowed_levels)) {
        stop(paste0("For new liquidity_floor_cutoffs, metric '", metric_name[i],
                    "' must have values for all liquidity classifications: ",
                    paste(allowed_levels, collapse = ", ")))
      }
    }
    new_df <- create_liquidity_floor_cutoffs(metric_name, metric_cutoffs)
    object@liquidity_floor_cutoffs <- new_df
    return(object)
  }

  # Otherwise, merge new information with the existing liquidity_floor_cutoffs.
  df <- object@liquidity_floor_cutoffs
  if (!is.data.frame(df)) {
    stop("liquidity_floor_cutoffs must be a data.frame")
  }
  if (!"liquidity_classification" %in% colnames(df)) {
    stop("liquidity_floor_cutoffs must have a 'liquidity_classification' column")
  }

  # Ensure that all allowed liquidity classifications are present.
  missing_levels <- setdiff(allowed_levels, df$liquidity_classification)
  if (length(missing_levels) > 0) {
    new_rows <- data.frame(liquidity_classification = missing_levels, stringsAsFactors = FALSE)
    for (col in setdiff(colnames(df), "liquidity_classification")) {
      new_rows[[col]] <- NA_real_
    }
    df <- rbind(df, new_rows)
  }
  # Reorder rows according to allowed_levels.
  df <- df[match(allowed_levels, df$liquidity_classification), , drop = FALSE]

  # For each provided metric, update or add its column.
  for (i in seq_along(metric_name)) {
    nm <- metric_name[i]
    vec <- metric_cutoffs[[i]]
    if (!is.numeric(vec) || is.null(names(vec))) {
      stop(paste0("Metric '", nm, "' must be a named numeric vector"))
    }
    if (!all(names(vec) %in% allowed_levels)) {
      stop(paste0("Metric '", nm, "' names must be among: ", paste(allowed_levels, collapse = ", ")))
    }
    if (nm %in% colnames(df)) {
      # Update the specified liquidity classifications.
      for (lev in names(vec)) {
        idx <- which(df$liquidity_classification == lev)
        df[idx, nm] <- vec[lev]
      }
    } else {
      # Create a new column (with NA) and update the specified rows.
      df[[nm]] <- NA_real_
      for (lev in names(vec)) {
        idx <- which(df$liquidity_classification == lev)
        df[idx, nm] <- vec[lev]
      }
    }
  }

  # After merging, ensure no NAs remain in any metric column.
  metric_cols <- setdiff(colnames(df), "liquidity_classification")
  if (any(is.na(df[, metric_cols]))) {
    stop("After adding, liquidity_floor_cutoffs elements must not have NAs")
  }

  # Check that the main liquidity metric is present.
  main_metric <- object@main_liquidity_metric
  if (!(main_metric %in% colnames(df))) {
    stop("main_liquidity_metric is not represented in liquidity_floor_cutoffs")
  }

  # Ensure that df is sorted in ascending order by the main liquidity metric.
  df_sorted <- dplyr::arrange(df, !!rlang::sym(main_metric))
  if (!all(df_sorted$liquidity_classification == df$liquidity_classification)) {
    stop("liquidity_floor_cutoffs is not in ascending order based on main_liquidity_metric")
  }

  # Check that the ordering (ranking) is consistent across all metric columns.
  orders_matrix <- sapply(df[, metric_cols, drop = FALSE], function(x) order(x))
  for (i in seq_len(nrow(orders_matrix))) {
    if (length(unique(orders_matrix[i, ])) != 1) {
      stop("liquidity metrics orders in liquidity_floor_cutoffs are conflicting")
    }
  }

  object@liquidity_floor_cutoffs <- df
  return(object)
}

#-----------------------------------------------------------------------
# create_port_backtest_cohort
#-----------------------------------------------------------------------
#' Create Portfolio Backtest Cohort
#'
#' This function creates a `port_backtest_cohort` object by merging a list of
#' `port_backtest_results` objects. It checks for compatibility across backtests by
#' verifying that key elements of the `port_backtest_workflow` slot are identical.
#'
#' The function then merges:
#' \itemize{
#'   \item{\strong{port_weights_m_df:}} Merges the underlying meta data.frames (accessed via `@data`) by matching `id`, `tickers` and `dates`.
#'         The individual `eop_port_weights` columns are renamed to the corresponding backtest identifier.
#'         If a benchmark was used (i.e. `selected_benchmark` is not `NULL`), the common `bench_weights` column is
#'         checked for consistency and included only once.
#'   \item{\strong{port_costs_m_xts:}} For each cost type (direct_cost, market_impact_cost, total_cost, turnover),
#'         the function extracts the respective column from each backtest’s `@port_costs_m_xts@data`,
#'         renames it to the backtest identifier, and then merges them (after checking that dates match).
#'   \item{\strong{port_returns_m_xts:}} Separately merges columns for raw_return, net_return, raw_active_return, and net_active_return.
#'         For raw_return and net_return, if a benchmark is present the common `selected_bench_return` column is included
#'         after checking consistency.
#'   \item{\strong{port_metrics_m_xts:}} As the metrics may be built from custom stock metrics,
#'         the function iterates over the primary metric columns (i.e. those not starting with `bench_`)
#'         and merges them across backtests. If a corresponding bench column exists (and benchmarks are used)
#'         and is identical across backtests, it is included once.
#' }
#'
#' After merging, the function uses `create_meta_dataframe` and `create_meta_xts` (from your package)
#' to reconstruct the meta objects. The resulting `port_backtest_cohort` object contains the merged data
#' and the common backtest workflow parameters.
#'
#' @param port_backtest_results_list A list of `port_backtest_results` objects.
#' @param cohort_name A character string representing the name for the merged cohort.
#'
#' @return An object of class `port_backtest_cohort`.
#' @export
create_port_backtest_cohort <- function(port_backtest_results_list, cohort_name) {

  # Input Validation
  ## Check that port_backtest_results_list is a non-empty list
  if (!is.list(port_backtest_results_list) || length(port_backtest_results_list) == 0) {
    stop("port_backtest_results_list must be a non-empty list of port_backtest_results objects.")
  }
  ## Check that cohort_name is a single character string
  if (!is.character(cohort_name) || length(cohort_name) != 1) {
    stop("cohort_name must be a single character string.")
  }
  ## Check that all elements are of class 'port_backtest_results'
  if (!all(sapply(port_backtest_results_list, function(x) methods::is(x, "port_backtest_results")))) {
    stop("All elements of port_backtest_results_list must be of class 'port_backtest_results'.")
  }
  ## Ensure that all backtests use the same benchmark
  if (length(unique(sapply(port_backtest_results_list, function(x) x@port_backtest_workflow$selected_benchmark))) > 1) {
    stop("All backtests must use the same benchmark.")
  }
  ## Ensure all backtest identifiers are unique
  if (length(unique(sapply(port_backtest_results_list, function(x) x@backtest_identifier))) != length(port_backtest_results_list)) {
    stop("All backtests must have unique backtest identifiers.")
  }

  # Step 1: Check Compatibility Across port_backtest_workflow Slots
  ## Define required parameters that must match across all backtests
  required_params <- c("selected_benchmark", "dates_covered", "initial_buffer_period", "dates_backtest",
                       "signals_object_name", "fwd_returns_object_name", "stock_groups_object_name",
                       "benchmark_returns_object_name", "daily_assets_returns_object_name", "daily_bench_returns_object_name",
                       "liquidity_object_name", "volatility_object_name", "benchmark_weights_object_name")
  ## Extract the workflow list from each backtest object
  workflow_list <- lapply(port_backtest_results_list, function(x) x@port_backtest_workflow)
  ## Store the common values from the first backtest for comparison
  common_values <- workflow_list[[1]][required_params]
  ## Loop over each backtest and verify required parameters match
  for (i in seq_along(workflow_list)) {
    current_values <- workflow_list[[i]][required_params]
    for (param in required_params) {
      if (!identical(common_values[[param]], current_values[[param]])) {
        stop(paste("Incompatibility found in parameter:", param, "for backtest result at index", i))
      }
    }
  }
  ## Determine if a benchmark is used based on the selected_benchmark value
  benchmark_used <- !is.null(common_values[["selected_benchmark"]])

  # Step 2: Merge port_weights_m_df
  ## Extract and process the meta data.frames from each backtest
  weights_m_dfs <- lapply(port_backtest_results_list, function(x) {
    m_df <- x@port_weights_m_df@data
    ### Set required columns based on whether benchmark is used
    if (benchmark_used) {
      required_cols <- c("id", "tickers", "dates", "eop_port_weights", "bench_weights")
    } else {
      required_cols <- c("id", "tickers", "dates", "eop_port_weights")
    }
    ### Validate that required columns exist
    if (!all(required_cols %in% names(m_df))) {
      stop("Each port_weights_m_df must contain columns: ", paste(required_cols, collapse = ", "))
    }
    ### Rename 'eop_port_weights' column to the backtest identifier
    colnames(m_df)[colnames(m_df) == "eop_port_weights"] <- x@backtest_identifier
    m_df
  })

  ## Verify that id, tickers, dates (and bench_weights if applicable) match across all data.frames
  base_m_df <- weights_m_dfs[[1]][, c("id", "tickers", "dates", if (benchmark_used) "bench_weights" else NULL)]
  for (i in seq_along(weights_m_dfs)) {
    current_m_df <- weights_m_dfs[[i]][, c("id", "tickers", "dates", if (benchmark_used) "bench_weights" else NULL)]
    if (!all(base_m_df$id == current_m_df$id &
             base_m_df$tickers == current_m_df$tickers &
             base_m_df$dates == current_m_df$dates)) {
      stop("Mismatch in id, tickers, or dates in port_weights_m_df of backtest result at index ", i)
    }
    if (benchmark_used) {
      if (!all(base_m_df$bench_weights == current_m_df$bench_weights)) {
        stop("Mismatch in bench_weights in port_weights_m_df of backtest result at index ", i)
      }
    }
  }

  ## Merge the individual backtest eop_port_weights columns into one meta data.frame
  merged_weights_m_df <- base_m_df
  for (i in seq_along(weights_m_dfs)) {
    current_m_df <- weights_m_dfs[[i]][, c("id", setdiff(names(weights_m_dfs[[i]]), c("id", "tickers", "dates", if (benchmark_used) "bench_weights" else NULL))), drop = FALSE]
    merged_weights_m_df <- dplyr::left_join(merged_weights_m_df, current_m_df, by = "id")
  }
  ## If benchmark is used, ensure bench_weights is the last column in the merged data.frame
  if (benchmark_used) {
    bench_weights <- merged_weights_m_df$bench_weights
    merged_weights_m_df <- merged_weights_m_df %>% dplyr::select(-bench_weights) %>% dplyr::mutate(bench_weights = bench_weights)
  }

  ## Create the merged meta data.frame using create_meta_dataframe (type = "weights")
  merged_port_weights_m_df <- create_meta_dataframe(data = merged_weights_m_df,
                                                    meta_dataframe_name = cohort_name,
                                                    type = "weights")

  # Step 3: Merge port_costs_m_xts
  ## Define the cost types to merge
  cost_types <- c("direct_cost", "market_impact_cost", "total_cost", "turnover")
  ## Initialize a list to store merged meta_xts objects for each cost type
  port_costs_m_xts_list <- list()
  ## Loop over each cost type and merge across backtests
  for (cost in cost_types) {
    cost_m_xts_list <- lapply(port_backtest_results_list, function(x) {
      m_xts <- x@port_costs_m_xts@data[, cost, drop = FALSE]
      colnames(m_xts) <- x@backtest_identifier
      m_xts
    })
    ### Check that dates match across all meta_xts objects for the cost type
    dates_list <- lapply(cost_m_xts_list, function(x) as.character(zoo::index(x)))
    if (!all(sapply(dates_list, function(d) identical(d, dates_list[[1]])))) {
      stop("Dates do not match across port_costs_m_xts for cost type: ", cost)
    }
    ### Merge the cost meta_xts objects
    merged_cost_m_xts <- do.call(xts::merge.xts, cost_m_xts_list)
    ### Create a meta_xts object for the cost type using create_meta_xts
    port_costs_m_xts_list[[paste0(cost, "_m_xts")]] <- create_meta_xts(data = merged_cost_m_xts,
                                                                       type = "metrics",
                                                                       meta_xts_name = cohort_name,
                                                                       metric_name = cost,
                                                                       source = sapply(port_backtest_results_list, function(x) x@backtest_identifier))
  }

  # Step 4: Merge port_returns_m_xts
  ## If a benchmark is used, extract and verify the "selected_bench_return" column
  if (benchmark_used) {
    bench_returns_list <- lapply(port_backtest_results_list, function(x) {
      m_xts <- x@port_returns_m_xts@data[, "selected_bench_return", drop = FALSE]
      m_xts
    })
    bench_ref <- bench_returns_list[[1]]
    for (i in seq_along(bench_returns_list)) {
      if (!all(bench_returns_list[[i]] == bench_ref)) {
        stop("selected_bench_return mismatch in port_returns_m_xts for backtest result at index ", i)
      }
    }
  } else {
    bench_ref <- NULL
  }

  ## Define a helper function to merge a specified return column across backtests
  merge_return_column <- function(colname, include_bench = FALSE) {
    ret_m_xts_list <- lapply(port_backtest_results_list, function(x) {
      m_xts <- x@port_returns_m_xts@data[, colname, drop = FALSE]
      colnames(m_xts) <- x@backtest_identifier
      m_xts
    })
    ### Verify that dates match for the return column across backtests
    dates_list <- lapply(ret_m_xts_list, function(x) as.character(zoo::index(x)))
    if (!all(sapply(dates_list, function(d) identical(d, dates_list[[1]])))) {
      stop("Dates do not match across port_returns_m_xts for column: ", colname)
    }
    ### Merge the return meta_xts objects
    merged_ret_m_xts <- do.call(xts::merge.xts, ret_m_xts_list)
    ### If including benchmark data, append the benchmark return column
    if (include_bench && !is.null(bench_ref)) {
      merged_ret_m_xts <- cbind(merged_ret_m_xts, bench_ref)
      merged_ret_m_xts <- merged_ret_m_xts[, c(colnames(merged_ret_m_xts)[-ncol(merged_ret_m_xts)],
                                               colnames(merged_ret_m_xts)[ncol(merged_ret_m_xts)])]
    }
    merged_ret_m_xts
  }

  ## Set include_bench flag based on benchmark_used and merge individual return columns using the helper function
  include_bench <- benchmark_used  # Only include benchmark returns if benchmark is used
  raw_returns_m_xts <- merge_return_column("raw_return", include_bench = include_bench)
  net_returns_m_xts  <- merge_return_column("net_return", include_bench = include_bench)
  raw_active_returns_m_xts <-  if(benchmark_used) merge_return_column("raw_active_return", include_bench = FALSE) else NULL
  net_active_returns_m_xts <- if (benchmark_used) merge_return_column("net_active_return", include_bench = FALSE) else NULL
  ## Define the benchmark source (if applicable) and backtest sources
  if (include_bench) {
    bench_source <- port_backtest_results_list[[1]]@port_backtest_workflow$benchmark_returns_object_name
  }
  backtest_sources <- sapply(port_backtest_results_list, function(x) x@backtest_identifier)

  ## Create the merged port_returns meta_xts objects using create_meta_xts
  port_returns_m_xts_list <- list(
    raw_returns_m_xts = create_meta_xts(data = raw_returns_m_xts,
                                        type = "returns",
                                        asset_type = "ports",
                                        meta_xts_name = cohort_name,
                                        metric_name = "raw_return",
                                        source = if (include_bench) c(backtest_sources, bench_source) else backtest_sources),
    net_returns_m_xts = create_meta_xts(data = net_returns_m_xts,
                                       type = "returns",
                                       asset_type = "ports",
                                       meta_xts_name = cohort_name,
                                       metric_name = "net_return",
                                       source = if (include_bench) c(backtest_sources, bench_source) else backtest_sources)
  )

  if (benchmark_used) {
    port_returns_m_xts_list <- c(port_returns_m_xts_list,
                                 raw_active_returns_m_xts = create_meta_xts(data = raw_active_returns_m_xts,
                                                                           type = "returns",
                                                                           asset_type = "ports",
                                                                           meta_xts_name = cohort_name,
                                                                           metric_name = "raw_active_return",
                                                                           source = backtest_sources),
                                 net_active_returns_m_xts = create_meta_xts(data = net_active_returns_m_xts,
                                                                           type = "returns",
                                                                           asset_type = "ports",
                                                                           meta_xts_name = cohort_name,
                                                                           metric_name = "net_active_return",
                                                                           source = backtest_sources)
                                 )
  }

  # Step 5: Merge port_metrics_m_xts
  ## Consolidate all primary metric names across all backtests (excluding bench_* columns)
  all_primary_metrics_list <- lapply(port_backtest_results_list, function(x) {
    if (is.null(x@port_metrics_m_xts)) {
      return(character(0))
    } else {
      cols <- colnames(x@port_metrics_m_xts@data)
      primary <- cols[!grepl("^bench_", cols)]
      return(primary)
    }
  })
  union_primary_metrics <- sort(unique(unlist(all_primary_metrics_list)))

  ## Initialize list for merged port_metrics meta_xts objects
  port_metrics_m_xts_list <- list()

  ## Loop over each metric in the consolidated union of primary metrics
  for (metric in union_primary_metrics) {
    ### Create a list to hold the metric data from each backtest (if available)
    metric_m_xts_list <- lapply(port_backtest_results_list, function(x) {
      if (!is.null(x@port_metrics_m_xts) && metric %in% colnames(x@port_metrics_m_xts@data)) {
        m_xts <- x@port_metrics_m_xts@data[, metric, drop = FALSE]
        colnames(m_xts) <- x@backtest_identifier
        return(m_xts)
      } else {
        return(NULL)
      }
    })
    ### Remove NULLs for backtests that do not have this metric
    metric_m_xts_list <- Filter(Negate(is.null), metric_m_xts_list)

    ### If no backtest has this metric, skip to the next metric
    if (length(metric_m_xts_list) == 0) next

    ### Check that dates match among the available metric data
    dates_list <- lapply(metric_m_xts_list, function(x) as.character(zoo::index(x)))
    if (length(dates_list) > 1 && !all(sapply(dates_list, function(d) identical(d, dates_list[[1]])))) {
      stop("Dates do not match across port_metrics_m_xts for metric: ", metric)
    }

    ### Merge the available metric meta_xts objects
    merged_metric_m_xts <- do.call(xts::merge.xts, metric_m_xts_list)

    ### Handle the corresponding bench metric column (if benchmark is used)
    bench_col <- paste0("bench_", metric)
    bench_m_xts_list <- lapply(port_backtest_results_list, function(x) {
      if (!is.null(x@port_metrics_m_xts) && benchmark_used && bench_col %in% colnames(x@port_metrics_m_xts@data)) {
        return(x@port_metrics_m_xts@data[, bench_col, drop = FALSE])
      } else {
        return(NULL)
      }
    })
    bench_m_xts_list <- Filter(Negate(is.null), bench_m_xts_list)

    ### Initialize bench_consistent flag as FALSE
    bench_consistent <- FALSE

    ### If bench metric data is available, check for consistency
    if (length(bench_m_xts_list) > 0) {
      bench_ref_metric <- bench_m_xts_list[[1]]
      bench_consistent <- TRUE
      for (bm in bench_m_xts_list) {
        if (!all(bm == bench_ref_metric)) {
          bench_consistent <- FALSE
          break
        }
      }
      #### If bench metric data is consistent, combine it with the merged metric data
      if (bench_consistent) {
        merged_metric_m_xts <- cbind(merged_metric_m_xts, bench_ref_metric)
        merged_metric_m_xts <- merged_metric_m_xts[, c(colnames(merged_metric_m_xts)[!grepl("^bench_", colnames(merged_metric_m_xts))],
                                                       bench_col)]
      }
    }

    ### Define backtest sources for this metric
    backtest_sources <- sapply(port_backtest_results_list, function(x){
      if(!is.null(x@port_metrics_m_xts) && metric %in% colnames(x@port_metrics_m_xts@data)) x@backtest_identifier
    }) %>% unlist()

    ### If bench metric was added, include the bench source as well
    if (bench_consistent) {
      bench_source <- port_backtest_results_list[[1]]@port_backtest_workflow$benchmark_returns_object_name
      source_vector <- c(backtest_sources, bench_source)
    } else {
      source_vector <- backtest_sources
    }

    ### Create the consolidated meta_xts object for this metric using create_meta_xts
    port_metrics_m_xts_list[[paste0(metric, "_m_xts")]] <- create_meta_xts(
      data = merged_metric_m_xts,
      type = "metrics",
      meta_xts_name = cohort_name,
      metric_name = metric,
      source = source_vector
    )
  }

  # Step 6: Create and Return the port_backtest_cohort Object
  cohort_obj <- new("port_backtest_cohort",
                    cohort_name = cohort_name,
                    port_backtest_results_list = port_backtest_results_list,
                    port_weights_m_df = merged_port_weights_m_df,
                    port_costs_m_xts_list = port_costs_m_xts_list,
                    port_returns_m_xts_list = port_returns_m_xts_list,
                    port_metrics_m_xts_list = port_metrics_m_xts_list,
                    backtest_workflow_common = common_values)

  return(cohort_obj)
}
