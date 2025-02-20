#-----------------------------------------------------------------------
# External Objects
#-----------------------------------------------------------------------

# Register 'xts' as an S4 class
setOldClass("xts")

#-----------------------------------------------------------------------
# meta_dataframe
#-----------------------------------------------------------------------

#' Define the `meta_dataframe` S4 Class
#'
#' This class represents a sb_backtest_workflow-enhanced data frame. It extends the functionality
#' of a standard data frame by including additional sb_backtest_workflow slots. The class is designed
#' to ensure that the input data frame adheres to specific structural requirements, including
#' unique identifiers, valid date formats, and unique column names.
#'
#' @slot data A \code{data.frame} containing the actual data.
#' @slot workflow A \code{list} for storing sb_backtest_workflow about the data manipulation workflow.
#' @slot signals A \code{character} vector containing the names of columns that represent signals.
#' @slot unique_dates A \code{numeric} value representing the count of unique dates in the data.
#' @slot unique_tickers A \code{numeric} value representing the count of unique tickers in the data.
#' @slot n_obs A \code{numeric} value representing the total number of observations in the data.
#'
#' @details
#' The \code{meta_dataframe} class ensures that the data frame is structured correctly with the required columns,
#' and includes sb_backtest_workflow about the data. The \code{signals} slot holds the names of columns representing various signals.
#' The \code{unique_dates}, \code{unique_tickers}, and \code{n_obs} slots store the sb_backtest_workflow related to the number of unique dates,
#' tickers, and total observations respectively.
#'
#' @examples
#' # Define a sample data frame
#' df <- data.frame(
#'   id = c("A-2024-01-01", "B-2024-02-01"),
#'   tickers = c("A", "B"),
#'   dates = as.Date(c("2024-01-01", "2024-02-01")),
#'   value = c(10, 20)
#' )
#'
#' # Create a meta_dataframe object
#' meta_df <- create_meta_dataframe(df)
#'
#' # Print the meta_dataframe object
#' print(meta_df)
#'
#' @export
setClass("meta_dataframe",
         slots = c(
           data = "data.frame",        # Slot for the data frame
           workflow = "ANY",          # Slot for storing sb_backtest_workflow about the data manipulation workflow
           signals = "character",      # Slot for storing column names
           unique_dates = "numeric",   # Slot for storing count of unique dates
           unique_tickers = "numeric", # Slot for storing count of unique tickers
           n_obs = "numeric",          #  Slot for storing total number of observations
           meta_dataframe_name = "character"
         ), validity = function(object){

           #Check coercibility
           is_coercible_to_meta_dataframe(object@data)

           #Check for presence of low
           if(any(grepl("low_", object@signals))){
             stop("Column names should not contain 'low_', as it will bring problems when running backtesting functions")
           }

           #Check for spaces in tickers
           #if(any(grepl(" ", object@data[["tickers"]]))){
           #  stop("Tickers should not contain spaces")
           #}

         })

#' Define the signals_m_df S4 Class
#'
#' This class inherits from \code{meta_dataframe} and enforces that the underlying data is adherent to a signals meta_dataframe.
#'
#' @export
setClass(
  "signals_m_df",
  contains = "meta_dataframe",
  validity = function(object) {
  if (any(object@data %>% apply(2, function(x) any(is.na(x))))){
    stop("Data contains missing values")
    }
  }
)


#' Define the groups S4 Class
#'
#' This class inherits from \code{meta_dataframe} and enforces that the underlying data is adherent to a grouping meta_dataframe.
#'
#' @export
setClass(
  "groups_m_df",
  contains = "meta_dataframe",
  validity = function(object) {
    groups <- object@signals
    for (g in groups) {
      # Count distinct classifications for each ticker
      df_counts <- object@data %>%
        dplyr::group_by(.data[["tickers"]]) %>%
        dplyr::summarise(
          n_class = dplyr::n_distinct(.data[[g]]),
          .groups = "drop"
        )

      # Identify tickers with more than one classification
      multi_idx <- which(df_counts$n_class > 1)
      if (length(multi_idx) > 0) {
        multi_tickers <- df_counts[["tickers"]][multi_idx]
        warning(
          sprintf(
            "Ticker(s) %s have multiple classifications in '%s'.",
            paste(multi_tickers, collapse = ", "),
            g
          ),
          call. = FALSE
        )
      }
    }
  }
)


#' Define the tickers S4 Class
#'
#' This class inherits from \code{meta_dataframe} and enforces that the underlying data is adherent to a tickers meta_dataframe.
#'
#' @export
setClass(
  "target_m_df",
  contains = "meta_dataframe",
  validity = function(object) {

  target_fwd_name_right_pattern <- "^[A-Za-z_]+_[0-9]{1,2}m$"

  for(target in object@signals){

    if(!grepl(target_fwd_name_right_pattern, target)){
      stop(cat(paste(target, " is not a valid target variable name. The
            target_m_df colnames should follow the format XXXX_number_m, where ' XXXX is the name of the target variable,
            number is the amount of forward periods and m indicates periods are measured in months.")))
    }
  }
  }
)

#' Define the priors_m_df S4 Class
#'
#' This class inherits from \code{meta_dataframe} and enforces that the underlying data is adherent to a priros meta_dataframe.
#'
#' @export
setClass(
  "priors_m_df",
  contains = "meta_dataframe",
  validity = function(object) {

    if (any(object@data %>% apply(2, function(x) any(is.na(x))))){
      stop("Data contains missing values")
    }

    if (!any(colnames(object@data) %in% c("return", "theme", "market_factor_proxy"))){
      stop("Data does not contain the required columns 'return', 'theme', and 'market_factor_proxy'")
    }
  }
)

#' Define the signal_universe_meta_dataframe S4 Class
#'
#' This class inherits from \code{meta_dataframe} and enforces that the underlying data is adherent to the output of a signal selection backtest workflow.
#'
#' @slot universe_name A \code{character} string describing the universe name.
#' @slot ss_backtest_workflow A \code{list} storing the ss_backtest_workflow that generated the signal_universe_meta_dataframe object.
#'
#'
#' @export
setClass(
  "signal_universe_m_df",
  slots = c(
    ss_backtest_workflow = "ANY"
  ),
  contains = "meta_dataframe",
  validity = function(object) {

    #Check for valid column names
    valid_performance_metrics_names <- c(
      "arith_mean_ret", "geom_mean_ret", "ann_ret", "std_dev", "ann_std_dev",
      "semi_dev", "down_dev", "dd_dev", "down_freq", "exp_short", "pain", "ulcer", "max_dd", "skew", "kurt",
      "sharpe_ratio", "ann_sharpe_ratio", "sharpe_ratio_semi_dev", "sortino_ratio", "ann_burke_ratio",
      "inv_d_ratio", "sharpe_ratio_exp_short", "ann_pain_ratio", "ann_martin_ratio", "ann_calmar_ratio",
      "ann_adj_sharpe_ratio", "omega", "rachev_ratio", "avg_dd_rec", "avg_dd_length", "hurst", "min_track_record",
      "prob_sharpe_ratio", "modigliani", "ann_modigliani",
      "act_arith_mean_ret", "act_geom_mean_ret", "act_ann_ret", "track_err", "ann_track_err",
      "act_semi_dev", "act_down_dev", "act_dd_dev", "act_down_freq", "act_exp_short", "act_pain", "act_ulcer",
      "act_max_dd", "act_skew", "act_kurt", "info_ratio", "ann_info_ratio", "info_ratio_semi_dev",
      "act_sortino_ratio", "act_ann_burke_ratio", "act_inv_d_ratio", "info_ratio_exp_short", "act_ann_pain_ratio",
      "act_ann_martin_ratio", "act_ann_calmar_ratio", "ann_adj_info_ratio", "act_omega", "act_rachev_ratio",
      "act_avg_dd_rec", "act_avg_dd_length", "act_hurst", "act_min_track_record", "prob_info_ratio",
      "act_modigliani", "act_ann_modigliani",
      "alpha", "theme_alpha", "individual_alpha", "alpha_se", "beta", "theme_beta", "individual_beta", "specific_risk",
      "alpha_t_stat", "treynor_ratio", "appraisal_ratio", "p_value",
      "posterior_theme_alpha", "posterior_individual_alpha", "posterior_alpha_se", "posterior_theme_beta", "posterior_individual_beta",
      "posterior_specific_risk", "posterior_alpha_t_stat", "posterior_treynor_ratio", "posterior_appraisal_ratio", "pd_theme_alpha", "pd_alpha"
    )

    # Check if columns adhere to expectations
    colnames <- colnames(object@data)

    if(any(!colnames %in% c("id", "tickers", "dates", valid_performance_metrics_names, "adjusted_p_value", "pre_eligible_assets", "is_eligible",
                            "theme", "theme_ss_bench_weights", "theme_sb_bench_weights"))){
      message("User-inputed metrics were identified in signal_universe_m_df object")
    }

    if(any(!c("pre_eligible_assets", "is_eligible") %in% colnames)){
      return("signal_universe_m_df object must contain pre_eligible_assets and is_eligible columns.")
    }

    if(any(!c("theme_ss_bench_weights", "theme_sb_bench_weights", "theme") %in% colnames)){
      return("signal_universe_m_df object must contain theme_ss_bench_weights, theme_sb_bench_weights and theme columns.")
    }

    # If all checks pass
    TRUE
  }
)

#' Define the oos_sb_outputs_m_df S4 Class
#'
#' This class inherits from \code{meta_dataframe} and enforces that the underlying data is adherent to the output of a
#' signal blending backtest workflow.
#'
#' @export
setClass(
  "oos_sb_outputs_m_df",
  contains = "meta_dataframe",
  slots = c(
    sb_backtest_workflow = "ANY"
  ),
  validity = function(object) {
    #Colnames adherence
    if(!any(colnames(object@data) == c("id", "tickers", "dates", "target", "pred", "error"))){
      stop("Column names do not adhere to expected oos_sb_outputs_m_df object")
    }
    # Check if 1) target not NA, error is target - pred 2) target NA, error is also NA
    valid_indices <- !(is.na(object@data$target))

    # Check for incorrect error values where valid indices exist
    if (any((object@data$target[valid_indices] - object@data$pred[valid_indices]) != object@data$error[valid_indices],
            na.rm = TRUE)) {
      stop("Error values do not match target - pred where target and pred are not both NA.")
    }

    # Check if both target and pred are NA, and error should also be NA
    if (any(is.na(object@data$target) & is.na(object@data$pred) & !is.na(object@data$error))) {
      stop("Error should be NA when both target and pred are NA.")
    }

  }
)


#' Define the signal_universe_meta_dataframe S4 Class
#'
#' This class inherits from \code{meta_dataframe} and enforces that the underlying data is adherent to the output of a signal selection backtest workflow.
#'
#' @slot universe_name A \code{character} string describing the universe name.
#' @slot port_backtest_workflow A \code{list} storing the ss_backtest_workflow that generated the signal_universe_meta_dataframe object.
#'
#'
#' @export
setClass(
  "stock_universe_m_df",
  slots = c(
    port_backtest_workflow = "ANY"
  ),
  contains = "meta_dataframe",
  validity = function(object) {

    #Check for exp_ret_score, pre_eligible_assets and is_eligible
    colnames <- colnames(object@data)

    if(any(!c("pre_eligible_assets", "is_eligible", "exp_ret_score") %in% colnames)){
      return("stock_universe_m_df object must contain exp_ret_score, pre_eligible_assets and is_eligible columns.")
    }

    # If all checks pass
    TRUE
  }
)

#' Define the weights_m_df S4 Class
#'
#' This class inherits from \code{meta_dataframe} and enforces that the underlying data is adherent to a weights_meta_dataframe
#'
#' @export
setClass(
  "weights_m_df",
  contains = "meta_dataframe",
  validity = function(object) {


    #Check that all columns except for id, tickers and dates are between 0 and 1
    if(any(apply(object@data %>% dplyr::select(-id, -tickers, -dates), 2, function(x) any(x < 0 | x > 1)))){
      stop("All columns except for id, tickers and dates must be between 0 and 1.")
    }

    #Check if weights sum to 1
    # Pivot the data longer (all columns except id, tickers, dates)
    res_long <- object@data %>%
      tidyr::pivot_longer(
        cols = -c(id, tickers, dates),
        names_to = "variable",
        values_to = "weight"
      ) %>%
      dplyr::group_by(dates, variable) %>%
      dplyr::summarise(sum_w = sum(weight), .groups = "drop") %>%
      dplyr::mutate(check_sum = abs(sum_w - 1) < 0.02)

    # Check if any group does not satisfy the condition
    if(any(!res_long$check_sum)) {
      problematic <- res_long %>% dplyr::filter(!check_sum)
      stop(paste("Weights do not sum to 1 for the following variable-date combinations:",
                 paste0(problematic$variable, " at ", problematic$dates, collapse = "; ")))
    }

    return(TRUE)


  }
)


#' An S4 class that stores a main xts object plus metadata about backtested returns.
#'
#' The meta_xts class is designed to hold:
#' \itemize{
#'   \item \code{data}: An xts object containing the time series data.
#'   \item \code{meta_xts_name}: A character name/label for the series.
#'   \item \code{workflow}: An ANY object that can hold workflow objects/pipelines.
#'   \item \code{n_dates}: A numeric value equal to the number of rows in \code{data}.
#'   \item \code{source}: A character vector indicating the origin of each column
#'         (same length as number of columns in \code{data}).
#'   \item \code{frequency}: A character value representing time frequency
#'         (e.g., "daily", "weekly", "monthly", "yearly", etc.).
#' }
#'
#' @slot data An xts object containing the time series.
#' @slot meta_xts_name Character. A label or ID for the series.
#' @slot workflow ANY. A placeholder for user-defined workflow/pipeline objects.
#' @slot n_dates Numeric. Number of rows in \code{data}.
#' @slot source Character. Source of each column (same length as number of columns in \code{data}).
#' @slot frequency Character. Detected frequency (daily, monthly, yearly, etc.).
#'
#' @import methods
#' @importFrom xts xts
#' @importFrom zoo index
#' @export
setClass(
  Class = "meta_xts",
  slots = c(
    data          = "xts",
    meta_xts_name = "character",
    metric_name   = "character",
    workflow      = "ANY",
    n_dates       = "numeric",
    source        = "character",
    frequency     = "character"
  ),
  validity = function(object) {
    # The underlying data is in object@data
    main_xts <- object@data

    idx <- zoo::index(main_xts)
    if (!all(diff(idx) > 0)) {
      return("Dates must be strictly increasing (oldest to newest).")
    }
    if (object@n_dates != nrow(main_xts)) {
      return("Slot 'n_dates' does not match the number of rows in the xts slot 'data'.")
    }
    current_colnames <- colnames(main_xts)
    ncols <- length(current_colnames)
    if (length(object@source) != ncols) {
      return("Slot 'source' must have the same length as the number of columns in slot 'data'.")
    }

    if (length(unique(idx)) < length(idx)) {
      return("There are duplicated rows (time index) in 'data'.")
    }
    if (anyDuplicated(current_colnames) > 0) {
      return("There are duplicated column names in 'data'.")
    }

    return(TRUE)
  }
)

#' An S4 subclass of meta_xts for asset returns with no holes.
#'
#' In addition to the parent slots, it has:
#' \itemize{
#'   \item \code{assets}: a character vector with names of asset columns.
#'   \item \code{n_assets}: a numeric value equal to the number of asset columns.
#' }
#'
#' @slot assets Character. Names of the columns (assets).
#' @slot n_assets Numeric. Number of asset columns.
#'
#' @importFrom methods setClass
#' @export
setClass(
  Class = "returns_meta_xts",
  contains = "meta_xts",
  slots = c(
    asset_type = "character",
    assets   = "character",
    n_assets = "numeric"
  ),
  validity = function(object) {
    main_xts <- object@data
    idx <- zoo::index(main_xts)
    freq_info <- xts::periodicity(main_xts)
    discovered_scale <- freq_info$scale

    # Check for consecutive dates
    if (discovered_scale %in% c("daily", "weekly", "monthly", "quarterly", "yearly")) {
      full_seq <- switch(
        discovered_scale,
        "daily"     = seq(idx[1], idx[length(idx)], by = "day"),
        "weekly"    = seq(idx[1], idx[length(idx)], by = "week"),
        "monthly"   = seq(idx[1], idx[length(idx)], by = "month"),
        "quarterly" = seq(idx[1], idx[length(idx)], by = "quarter"),
        "yearly"    = seq(idx[1], idx[length(idx)], by = "year")
      )
      if (length(full_seq)*0.5 > length(idx)) {
        return(paste0("Dates are probably not consecutive for detected frequency: ", discovered_scale, "."))
      }
    }

    all_values <- as.numeric(main_xts)
    if (median(abs(all_values[!is.na(all_values)])) < 1) {
      warning("Data might be in decimal form (e.g. 0.02 for 2%). ",
              "Some functions depend on a percent representation (use 2.0 instead of 0.02), but this check might ",
              "be wrong. Please confirm if data format is right.")
    }

    # Check assets & n_assets
    if (object@n_assets != ncol(main_xts)) {
      return("Slot 'n_assets' does not match the number of columns in the xts slot 'data'.")
    }
    current_colnames <- colnames(main_xts)
    if (!identical(object@assets, current_colnames)) {
      return("Slot 'assets' does not match the colnames of the xts slot 'data'.")
    }

    #Check for NAs
    if (any(is.na(main_xts))) {
      if(object@asset_type == "ports"){
        #NA Values are forbidden for ports
        stop("There are NA values in the time series.")
      } else {
        warning("There are NA values in the time series.")
      }
    }


    freq_info <- object@frequency
    message("Detected frequency is: ", freq_info)

    return(TRUE)
  }
)

#' An S4 subclass of meta_xts for metric time series (holes allowed).
#'
#' In addition to the parent slots, it has:
#' \itemize{
#'   \item \code{metrics}: a character vector with names of metric columns.
#'   \item \code{n_metrics}: a numeric value equal to the number of metric columns.
#' }
#'
#' @slot series Character. Names of the columns (metrics).
#' @slot n_series Numeric. Number of metric columns.
#'
#' @importFrom methods setClass
#' @export
setClass(
  Class = "metrics_meta_xts",
  contains = "meta_xts",
  slots = c(
    series   = "character",
    n_series = "numeric"
  ),
  validity = function(object) {
    main_xts <- object@data
    if (object@n_series != ncol(main_xts)) {
      return("Slot 'n_series' does not match the number of columns in the xts slot 'data'.")
    }
    current_colnames <- colnames(main_xts)
    if (!identical(object@series, current_colnames)) {
      return("Slot 'series' does not match the colnames of the xts slot 'data'.")
    }
    # No check for consecutive dates (holes allowed).
    return(TRUE)
  }
)


#' Define the transactions_log S4 Class
#'
#' This class enforces that the underlying data is adherent to the output of a
#' port_backtest_workflow
#'
#' @export
setClass(
  "transactions_log",
  slots = c(
    data = "list",
    workflow = "ANY"
  ),
  validity = function(object) {
    #Check that all elements of the list are dataframes
    if(!all(sapply(object@data, is.data.frame))){
      stop("All elements of the list must be dataframes.")
    }
    #Check that all have required colnames
    required_colnames <- c("id", "tickers", "dates", "eop_port_weights", "daily_vol", "bop_port_weights", "obs", "delta", "order", "relative_order_size", "alpha",
                           "lambda", "direct_cost", "market_impact_cost", "total_cost")
    if(!all(sapply(object@data, function(x) all(required_colnames %in% colnames(x))))){
      stop("Column names do not adhere to expected transactions_log object")
    }

    # Check if there are NAs
    if(any(sapply(object@data, function(x) any(is.na(x))))){
      stop("NAs are not allowed in transactions_log object.")
    }

    #Check if unique dates length is equal to 2 for each element in list
    if(any(sapply(object@data, function(x) length(unique(x$dates)) != 2))){
      stop("transactions_log object must contain two unique dates for each element.")
    }

    #Check if alpha and lambda are between 0 and 1 for each element
    if(any(purrr::map_lgl(object@data, function(x) any(x$alpha <= 0 | x$alpha > 1)))){
      stop("Alpha must be between 0 and 1.")
    }
    if(any(purrr::map_lgl(object@data, function(x) any(x$lambda <= 0 | x$lambda > 1)))){
      stop("Lambda must be between 0 and 1.")
    }

    #Check if total_cost equals direct_cost + market_impact_cost for each row
    if(any(purrr::map_lgl(object@data, function(x) any(x$total_cost != x$direct_cost + x$market_impact_cost)))){
      stop("Total cost must equal direct cost + market impact cost.")
    }
  }
)


#-----------------------------------------------------------------------
# hyperparams
#-----------------------------------------------------------------------


#' Define the `hyper_grid_domain` S4 Class
#'
#' This class represents parameters for defining the hyperparameter domain based on which tuning will be performed.
#' It helps the user in correctly setting this object in the context of the `ml_walk_forward_validation` function.
#'
#' @slot hyperparameter_list A list with the hyperparameters relevant to the specified machine learning algorithm.
#'
#'
#' @export
setClass(
  "hyper_grid_domain",
  slots = list(
    hyperparameter_list = "list"
  ), validity = function(object){

    #Check for valid choice in hyperparameter
    if(length(object@hyperparameter_list) != 0){
      valid_hyperparameters <- c("alpha", "lambda.min.ratio", "mtry", "num.trees", "max.depth", "min.bucket", "min_child_weight", "max_depth", "subsample", "colsample_bytree",
                                 "eta", "gamma", "nrounds", "regularizer_l1", "regularizer_l2", "droprate", "lr", "size_of_batch", "number_of_epochs")

      if (any(!names(object@hyperparameter_list) %in% valid_hyperparameters) ){
        return("Invalid choice for hyperparameter. Should be one of alpha, lambda.min.ratio (glmnet), mtry, num.trees, max.depth, min.bucket (rf),
             min_child_weight, max_depth, subsample, colsample_bytree, eta, gamma, nrounds (xgb),
             regularizer_l1, regularizer_l2, droprate, lr, size_of_batch, number_of_epochs (nn)")
      }

    }

  })

#-----------------------------------------------------------------------
# tuning_strat
#-----------------------------------------------------------------------

#' @title Base class for hyperparameter tuning strategies
#' @description This class defines the common slots and structure for hyperparameter tuning strategies such as grid search, random search, and Bayesian optimization.
#' @slot tuning_method Character string indicating the hyperparameter tuning method ('grid_search', 'random_search', or 'bayesian_opt').
#' @slot validation_sample_size Numeric value representing the size of the validation sample. If provided a decimal, it will be set based on proportion of training sample size.
#' @slot chosen_eval_metric Character or NULL, specifying the evaluation metric to be optimized.
#' @slot hyper_grid_domain An object of class `hyper_grid_domain`, representing the hyperparameter domain based on which tuning will be performed.
#' It contains a list slot `hyperparameter_list` with the hyperparameters relevant to the specified machine learning algorithm.
#' The structure of this list depends on the specified tuning method:
#' \itemize{
#'   \item \strong{For grid search:} Must be a list of named vectors:
#'   \item \strong{For random search:} Must be a list of named lists, where each named list contains:
#'     \itemize{
#'       \item \code{distribution_choice}: A character string specifying the distribution (one of "normal", "uniform", "lognormal", "constant").
#'       \item \code{pars}: A named numeric vector of parameters corresponding to the chosen distribution.
#'       \item \code{value}: A numeric value (only present if \code{distribution_choice} is "constant").
#'     }
#'   \item \strong{For Bayesian optimization:} Must be a list of named numeric vectors, each of length 2, representing the boundaries for the hyperparameters.
#' }
#' @slot early_stop Sets a halting criteria to prevent overfitting in xgb and nn.
#' @export
setClass(
  "tuning_strategy",
  slots = list(
    tuning_method = "character",
    validation_sample_size = "numeric",
    chosen_eval_metric = "character",
    hyper_grid_domain = "hyper_grid_domain",
    early_stop = "ANY"
  ),
  validity = function(object) {
    if (!(object@tuning_method %in% c("grid_search", "random_search", "bayesian_opt"))) {
      return("Invalid tuning_method.")
    }
    if(is.null(object@chosen_eval_metric)){
      stop("chosen_eval_metric can't be missing.")
    }
    valid_eval_metrics <- c("rss", "rmse", "cp", "mae", "mphe", "mpe", "mape", "hr", "mb")
    if (!is.null(object@chosen_eval_metric) && !(object@chosen_eval_metric %in% valid_eval_metrics)) {
      return("Invalid chosen_eval_metric. Choose from 'rss', 'rmse', 'cp', 'mae', 'mphe', 'mpe', 'mape', 'hr', 'mb'.")
    }

    # Validate hyperparameters based on tuning_method

    ##Grid search
    if (object@tuning_method == "grid_search") {
      if (!all(sapply(object@hyper_grid_domain@hyperparameter_list, function(x) is.numeric(x) && is.vector(x)))) {
        stop("For 'grid_search', hyperparameters must be a list of numeric vectors.")
      }

      ##Random search
    }

    else if (object@tuning_method == "random_search") {
      for (name in names(object@hyper_grid_domain@hyperparameter_list)) {
        if (!is.list(object@hyper_grid_domain@hyperparameter_list[[name]]) || !all(c("distribution_choice") %in% names(object@hyper_grid_domain@hyperparameter_list[[name]]))) {
          stop("For 'random_search', each hyperparameters must be a list with 'distribution_choice'.")
        }

        distribution_choice <- object@hyper_grid_domain@hyperparameter_list[[name]]$distribution_choice

        if (is.null(distribution_choice) || !(distribution_choice %in% c("normal", "uniform", "lognormal", "constant"))) {
          stop("distribution_choice must be one of 'normal', 'uniform', 'lognormal', or 'constant'.")
        }

        if (distribution_choice == "constant") {
          if (is.null(object@hyper_grid_domain@hyperparameter_list[[name]]$value) || !is.numeric(object@hyper_grid_domain@hyperparameter_list[[name]]$value)) {
            stop("For 'constant', the second argument must be a numeric vector named 'value'.")
          }
        } else {
          if (!is.null(object@hyper_grid_domain@hyperparameter_list[[name]]$value)) {
            stop("For distributions other than 'constant', do not specify 'value'.")
          }
          pars <- object@hyper_grid_domain@hyperparameter_list[[name]]$pars
          if (is.null(pars) || !is.numeric(pars) || !is.vector(pars)) {
            stop("For distributions, the second argument must be a numeric vector named 'pars'.")
          }

          # Additional checks based on distribution_choice
          if (distribution_choice == "normal") {
            if (!all(names(pars) %in% c("mean", "sd"))) {
              stop("For 'normal', 'pars' must have names 'mean' and 'sd'.")
            }
          } else if (distribution_choice == "uniform") {
            if (!all(names(pars) %in% c("min", "max"))) {
              stop("For 'uniform', 'pars' must have names 'min' and 'max'.");
            }
          } else if (distribution_choice == "lognormal") {
            if (!all(names(pars) %in% c("meanlog", "sdlog"))) {
              stop("For 'lognormal', 'pars' must have names 'meanlog' and 'sdlog'.");
            }
          }
        }
      }
    }

    #Bayesian Optimization
    else if (object@tuning_method == "bayesian_opt") {
      if (any(sapply(object@hyper_grid_domain@hyperparameter_list, function(x) !is.numeric(x) || length(x) != 2))) {
        stop("For 'bayesian_opt', each hyperparameters must be a numeric vector of length 2 representing the bounds.")
      }
    }

    else {
      stop("Invalid tuning_method. Only 'grid_search', 'random_search', and 'bayesian_opt' are supported.")
    }

    return(TRUE)
  }
)


#' @title Grid Search Tuning Strategy
#' @description A subclass of `tuning_strategy` that implements grid search.
#' @slot tuning_method The tuning method is set to 'grid_search'.
#' @export
setClass(
  "grid_search_strategy",
  contains = "tuning_strategy",
  slots = list(),
  prototype = list(tuning_method = "grid_search")
)

#' @title Random Search Tuning Strategy
#' @description A subclass of `tuning_strategy` that implements random search.
#' @slot tuning_method The tuning method is set to 'random_search'.
#' @slot n_iter For random_search, it should be the number of random draws for each hyperparameter to which a distribution has been assigned.
#' Random samples of n_iter size will be generated for each hyperparameter and their unique values will be exhaustively combined.
#' Therefore, for n_iter = 5 and 2 hyperparameters, the ml algorithm validation error should be generally evaluated 5² = 25 times.
#' In case a constant vector is passed, the n_iter argument is not applied to this hyperparameter.

#' @export
setClass(
  "random_search_strategy",
  contains = "tuning_strategy",
  slots = list(
    n_iter = "numeric"
  ),
  prototype = list(tuning_method = "random_search")
)

#' @title Bayesian Opt Tuning Strategy
#' @description A subclass of `tuning_strategy` that implements bayesian optimization.
#' @slot tuning_method The tuning method is set to 'bayesian_opt'.
#' @slot n_iter For bayesian_opt, it should be the number of times the ml algorithm will be evaluated after initialization.
#' @slot acq Acquisition function for Bayesian optimization: "ucb", "ei", or "poi".
#' @param init_points Number of initial random points for Bayesian optimization.
#' @param k_iter Integer that specifies the number of times to sample eval_function at each Epoch during Bayesian optimization.
#' If the intention is running in parallel, set k_iter to a multiple of the number of cores. Must be lower and preferably a multiple of n_iter.

#' @export
setClass(
  "bayesian_opt_strategy",
  contains = "tuning_strategy",
  slots = list(
    n_iter = "numeric",
    acq = "character",
    init_points = "numeric",
    k_iter = "numeric"
  ),
  prototype = list(
    tuning_method = "bayesian_opt",
    acq = "ucb"),
  validity = function(object){
    if (!object@acq %in% c("ucb", "ei", "poi")) {
      stop("acq must be one of 'ucb', 'ei', or 'poi'.")
    }
    return(TRUE)
  }
)

#-----------------------------------------------------------------------
# keras
#-----------------------------------------------------------------------

#' @title Keras Architecture Parameters
#' @description Class to encapsulate parameters for constructing a Keras neural network architecture.
#'
#' @slot units A numeric vector specifying the number of units (neurons) for each layer.
#' @slot n_layers A numeric value representing the total number of layers in the model.
#' @slot activation A character vector containing the activation functions for each layer.
#' @slot nn_optimizer A character string indicating the optimization algorithm used (length = 1).
#' @slot batch_norm_option A character vector specifying whether to apply batch normalization for each layer.
#'
#' @export
setClass(
  "keras_architecture_parameters",
  slots = list(
    units = "numeric",            # Vector of numeric units per layer
    n_layers = "numeric",         # Total number of layers
    activation = "character",     # Vector of activation functions
    nn_optimizer = "character",    # Optimization algorithm
    batch_norm_option = "logical" # Vector of batch normalization options
  )
)


#-----------------------------------------------------------------------
# cov_est_method
#-----------------------------------------------------------------------

#' Define the `cov_est_method` S4 Class
#'
#' S4 class to represent a set of configurations for estimating the covariance matrix.
#'
#' @slot cov_estimation_method A character string representing the covariance estimation method. Must be one of 'sample', 'ewma', 'cc', 'pca1', 'pca2', 'shrink_id' or 'shrink_cc'.
#' @slot cov_matrix_sample_size Number of periods to subset return sample when estimating the covariance matrix. A high number will provide
#' higher degrees of freedom, but old returns might not reflect current risk due to parameter shift. A low number will tend to expose estimation
#' to dimensionality curse.
#' @slot active_returns logical. If TRUE, the covariance matrix will be estimated using active returns.
#'  If FALSE, the covariance matrix will be estimated using raw returns.
#' @slot cov_matrix_benchmark  A character string representing the benchmark from benchmark_returns_xts to be used when estimating the covariance matrix.
#' Only needed when active_returns is TRUE.
#'
#' @return An S4 object of class `cov_est_method`.
#'
#' @export
setClass("cov_est_method",
         slots = list(
           cov_estimation_method = "character",
           cov_matrix_sample_size = "numeric",
           active_returns = "logical",
           cov_matrix_benchmark = "ANY"
         ),
         prototype = list(
           cov_estimation_method = "sample",
           cov_matrix_sample_size = 36,
           active_returns = TRUE
         ),
         validity = function(object){
           if(!object@cov_estimation_method %in% c("sample", "ewma", "cc", "pca1", "pca2", "shrink_id", "shrink_cc")){
             stop("Invalid cov_estimation_method. Must be one of 'sample', 'ewma', 'cc', 'pca1', 'pca2', 'shrink_id' or 'shrink_cc'.")
           }
           if(object@active_returns && is.null(object@cov_matrix_benchmark)){
             stop("cov_matrix_benchmark must be provided when active_returns is TRUE.")
           }
         }
)

#-----------------------------------------------------------------------
# mvo_parameters
#-----------------------------------------------------------------------


#' Define the `mvo_parameters` S4 Class
#'
#' S4 class to represent a set of configurations for mean-variance optimization.
#'
#' @slot opt_method A character indicating the optimization method. The only current available method is 'random'. In this case, n_random_portfolios are
#' generated under the constraints defined in the mvo_parameters object and the one that optimizes the opt_objective will be selected.
#' @slot random_ports_method A character string representing the method that will be passed to PortfolioAnalytics::random_portfolios to generate random portfolios. Options are
#' 'sample', 'simplex or 'grid'.
#' @slot n_random_ports Number of random portfolios to generate. Only needed when opt_method is 'random'.
#' @slot opt_objective A character indicating the optimization objective. Possible options are 'return', 'risk' or 'sharpe'.
#'
#' @return An S4 object of class `port_backtest_config`.
#'
#' @export
setClass("mvo_parameters",
         slots = list(
           opt_method = "character",
           random_ports_method = "character",
           n_random_ports = "numeric",
           opt_objective = "character"
         ),
         prototype = list(
           opt_method = "random",
           random_ports_method = "sample",
           n_random_ports = 1000,
           opt_objective = "sharpe"
         ),
         validity = function(object){
           if (!object@opt_method %in% c("random")) {
             stop("Currently, 'opt_method' must be 'random'.")
           }
           if (!object@random_ports_method %in% c("sample", "simplex", "grid")) {
             stop("random_ports_method must be one of 'sample', 'simplex', 'grid'.")
           }
           if (object@n_random_ports < 1) {
             stop("n_random_ports must be at least 1.")
           }
           if (!object@opt_objective %in% c("return", "risk", "sharpe")) {
             stop("opt_objective must be one of 'return', 'risk', 'sharpe'.")
           }
           TRUE
         }
)

#-----------------------------------------------------------------------
# rp_parameters
#-----------------------------------------------------------------------
#' Define the `rp_parameters` S4 Class
#'
#' S4 class to represent a set of configurations for risk-parity portfolios.
#'
#' @slot rp_method A character indicating the method to compute the risk-parity vanilla solution. It is passed to riskParityPortfolio::riskParityPortfolio function as method_init.
#' Default is "cyclical-spinu"
#'
#' @return An S4 object of class `port_backtest_config`.
#'
#' @export
setClass("rp_parameters",
         slots = list(
           rp_method = "character"
         ),
         prototype = list(
           rp_method = "cyclical-spinu"
         )
)


#-----------------------------------------------------------------------
# concentration_constraint_policy
#-----------------------------------------------------------------------

#' @title Concentration Constraint Policy
#' @description An S4 class to represent a concentration constraint policy
#' in portfolio construction.
#'
#' @slot benchmark A character vector indicating which benchmark(s) to use.
#' For stocks, must be a column in benchmarks_m_df.
#' For signals, must be theme_ss or theme_sb
#' @slot max_abs_active_individual_weight A numeric value indicating the
#'   maximum absolute active weight for individual assets.
#' @slot max_abs_active_group_weight A **named** numeric vector indicating
#'   maximum absolute group weights in relation to the benchmark.
#'   Names should match columns in a groups_m_df.
#'
#' @export
setClass(
  "concentration_constraint_policy",
  slots = c(
    benchmark = "character",
    max_abs_active_individual_weight = "ANY",
    max_abs_active_group_weight = "ANY"
  ),
  validity = function(object){
    validate_concentration_constraint_policy(as.list(object))
  }
)


#-----------------------------------------------------------------------
# liquidity_constraint_policy
#-----------------------------------------------------------------------

#' @title Liquidity Constraint Policy
#' @description An S4 class to represent a liquidity constraint policy in portfolio construction.
#'
#' @slot liquidity_floor_rule A character indicating the minimum liquidity classification
#' for an asset to be considered eligible. Should be one of 'micro_caps', 'small_caps', 'mid_caps', 'large_caps' or 'mega_caps'.
#' This constraint will work even if port_construction_method is not 'mvo'.
#' It can be added via add_liquidity_floor_rule function.
#' @slot liquidity_cap_rules A named vector in which each element is a value indicating the maximum (active) weight that can be assigned to assets
#' with the classification specified by its name. The names should be one of 'micro_caps', 'small_caps', 'mid_caps', 'large_caps' or 'mega_caps'.
#' A liquidity_constraint_policy can contain as many liquidity_cap_rules as needed, but names can't be repeated and a less liquid asset can't have a higher cap than a more liquid one.
#' New cap rules can be added through the add_liquidity_cap_rule function.
#' @export
setClass(
  "liquidity_constraint_policy",
  slots = c(
    liquidity_floor_rule = "ANY",
    liquidity_cap_rules = "ANY"
  ),
  validity = function(object) {

  validate_liquidity_constraint_policy(as.list(object))

  }
)

#-----------------------------------------------------------------------
# turnover_constraint_policy
#-----------------------------------------------------------------------

#' @title Turnover Constraint Policy
#' @description An S4 class to represent a turnover constraint policy in portfolio construction.
#'
#' @slot quantile_range_buffer A numeric indicating the increase in the quantile_range for an asset to be considered eligible to the buffer zone.
#' Stocks in this quantile that were present in bop_port_weights will be included in the buffer zone, if they meet buffer_zones_rules.
#' @slot turnover_cap_rules A named vector in which each element is a value indicating the maximum absolute weight deviation in relation to the bop_port_weights
#' that can be assigned to assets, with the classification specified by its name. The names should be one of 'micro_caps', 'small_caps', 'mid_caps', 'large_caps' or 'mega_caps'.
#' A turnover_constraint_policy can contain as many buffer_zone_rules as needed, but names can't be repeated and a less liquid asset can't have a higher cap than a more liquid one.
#' New cap rules can be added through the add_turnover_cap_rule function.
#' @export
setClass(
  "turnover_constraint_policy",
  slots = c(
    quantile_range_buffer = "numeric",
    turnover_cap_rules = "numeric"
  ),
  validity = function(object) {

    validate_turnover_constraint_policy(as.list(object))

  }
)

#-----------------------------------------------------------------------
# transaction_costs_parameters
#-----------------------------------------------------------------------

#' Transaction Cost Parameters S4 Class
#'
#' This S4 class stores transaction cost parameters based on the BARRA model.
#'
#' @slot direct_transaction_cost A numeric value representing the direct transaction cost (ie, brokerage fees). Should be in percentage (0.07 = 0.07%) .
#' @slot strategy_aum A numeric value representing the strategy's assets under management (AUM). Should be in same units as main_liquidity_metrics
#' @slot alpha A numeric value representing the alpha parameter.
#' @slot lambda A numeric value or the string "dynamic" representing the lambda parameter.
#'
#' @export
methods::setClass(
  Class = "transaction_costs_parameters",
  slots = c(
    direct_transaction_cost = "numeric",
    strategy_aum = "numeric",
    alpha = "numeric",
    lambda = "ANY"  # Accepts numeric or "dynamic"
  ),
  validity = function(object) {

    validate_transaction_costs_parameters(as.list(object))

  }
)



#-----------------------------------------------------------------------
# signal_port_parameters
#-----------------------------------------------------------------------


#' @title Signal Portfolio Parameters
#' @description Class to encapsulate parameters for constructing signal portfolios (portfolio-blending). Only needed when
#' sb_algorithm is 'rp' or 'mvo'.
#'
#' @slot cov_est_method An object of class `cov_est_method` representing the covariance estimation method and relevant parameters. Current methods are: 'sample', 'ewma', 'cc' (constant correlation),
#' 'pca1', 'pca2', 'shrink_id' (shrinkage to identity matrix), 'shrink_cc' (shrinkage to constant correlation). This is only relevant for 'rp' and 'mvo'.
#' @slot mvo_parameters An object of class `mvo_parameters` representing the parameters for mean-variance optimization. This is only relevant for 'mvo'.
#' @slot rp_parameters An object of class `rp_parameters` representing the parameters for risk parity. This is only relevant for 'rp'.
#' @slot concentration_constraint_policy The policy to handle concentration constraints.
#'  It contains up to to three elements:
#' - `benchmark`: A character vector describing the benchmark to be used to apply constraint.
#' For signal portfolios, possible options are theme_ss or theme_sb.
#' For stock portfolios, there must be a correspondence in `benchmark_weights_m_df`
#' - `max_abs_active_individual_weight`: The maximum absolute individual active weights.
#' - `max_abs_active_group_weight`: The maximum absolute theme active weight used for creating group constraints.
#'
#' @export
setClass(
  "signal_port_parameters",
  slots = list(
    cov_est_method = "cov_est_method",
    mvo_parameters = "ANY",
    rp_parameters = "ANY",
    concentration_constraint_policy = "ANY"
  ),
  validity = function(object){
    if (!is.null(object@mvo_parameters)) {
      if (!inherits(object@mvo_parameters, "mvo_parameters")) {
        stop("mvo_parameters must be of class 'mvo_parameters'")
      }
    }
    if (!is.null(object@rp_parameters)) {
      if (!inherits(object@rp_parameters, "rp_parameters")) {
        stop("rp_parameters must be of class 'rp_parameters'")
      }
    }
    if (!is.null(object@concentration_constraint_policy)) {
      if (!inherits(object@concentration_constraint_policy, "concentration_constraint_policy")) {
        stop("concentration_constraint_policy must be of class 'concentration_constraint_policy'")
      }
      if(!object@concentration_constraint_policy@benchmark %in% c("theme_ss", "theme_sb")){
          stop("Only allowed benchmarks for concentration_constraint_policy in 'signal_port_parameters' are 'theme_ss' and 'theme_sb'")
      }

    }
  }
)



#-----------------------------------------------------------------------
# alpha_test
#-----------------------------------------------------------------------

#' @title alpha_test_strategy Class
#' @description The alpha_test_strategy class is designed to specify parameters of hypothesis testing regarding
#' CAPM alpha under a multiple testing framework, with frequentist and bayesian approaches.
#' In the latter, the user can change the hierarchical model specification and how priors are going to be set.
#' @slot model_structure A character describing the model structure.
#' @slot signal_significance_threshold A decimal indicating the hypothesis testing negative-alpha null-hypothesis rejection criteria. If one wants to select all chosen_signals,
#' provide 1. In any case, a signal being selected demands a significant CAPM alpha.
#' @slot p_correction_method The method for p-value correction. Possible options are:
#'\itemize{
#'  \item{"none"}: No correction.
#'  \item{"bayesian"}: When bayesian is set, a hierarchical mixed-effects bayesian linear model is fitted to the data, using the `brms` package,
#'  which is an interface to the `Stan` probabilistic programming language.
#'  The user can also choose one of the following frequentist methods, which will control Family-Wise Error Rate (FWER) or the False Discovery Rate (FDR).
#'  FDR is less stringent than FWER.
#'  For FWER, possible options are:
#'  \item{"bonferroni"}: Bonferroni correction, which is dominated by Holm's method.
#'  \item{"holm"}: Holm's (1979) method.
#'  \item{"hochberg"}: Hochberg's (1988) method, valid when hypothesis tests are independent or non-negatively associated. Less powerful than Hommel's (1988) method, but
#'  faster to compute.
#'  \item{"hommel"}: Hommel's (1988) method, also valid when hypothesis tests are independent or non-negatively associated, but is more powerful than Hochberg (1988).
#'  For FDR, possible options are:
#'  \item{"BH" or "fdr"}: Benjamini-Hochberg (1995) procedure.
#'  \item{"BY"}: Benjamini-Yekutieli (2001) procedure.
#'  }
#' @slot market_factor_proxy A character string indicating the market factor proxy to be used in the CAPM model.
#' Should correspond to one of the columns in `benchmark_returns_df`.
#' @slot bayesian_model_parameters An object of class `bayesian_model_parameters`, containing the
#' parameters needed to build the hierarhicical bayesian model and specify its priors.
#' @slot enable_theme_representativeness A logical indicating whether, if a given theme in `signal_themes_m_df` does not have any eligible signal, the signal
#' with highest alpha t-stat should be elected.

setClass("alpha_test_strategy",
         slots = list(
           model_structure = "character",
           theme_level_intercept = "ANY",
           theme_level_slope = "ANY",
           lmer_control = "ANY",
           signal_significance_threshold = "numeric",
           p_correction_method = "character",
           enable_theme_representativeness = "logical",
           market_factor_proxy = "character"
         ),
         validity = function(object) {

           if (!(object@p_correction_method %in% c(
             "none", "bonferroni", "holm", "hochberg", "hommel", "BH", "fdr", "BY", "bayesian"
           ))) {
             stop("Invalid p_correction_method.")
           }
           if(object@p_correction_method == "bayesian" && object@model_structure != "partial_pooled"){
             stop("Currently, bayesian p_correction method is only available for partial_pooled model_structure")
           }
           if (object@signal_significance_threshold < 0 || object@signal_significance_threshold > 1) {
             stop("signal_significance_threshold must be between 0 and 1.")
           }
           if(!object@model_structure %in% c("partial_pooled", "no_pooled")){
             stop("Currently, model_structure must be one of partial_pooled or no_pooled")
           }

           if(object@model_structure == "partial_pooled"){
             if (!object@theme_level_intercept %in% c("fixed", "random", "theme_specific")){
               stop("theme_level_intercept must be 'fixed', 'random' or 'theme_specific'")
             }
             if (!object@theme_level_slope %in% c("fixed", "theme_specific")){
               stop("Currently, theme_level_slope can only be 'fixed' or 'theme_specific'")
             }
             avaiable_combinations <- c(c("random_intercept_fixed_slope"), #old random_intercept
                                        c("theme_specific_intercept_fixed_slope"), #old fixed_intercepts
                                        c("theme_specific_intercept_theme_specific_slope"), #old fixed_intercepts_fixed_slopes
                                        c("fixed_intercept_fixed_slope")) #one none
             chosen_combination <- paste0(object@theme_level_intercept, "_intercept_", object@theme_level_slope, "_slope")

             if(!chosen_combination %in% avaiable_combinations){
               stop("Chosen combination of theme_level_intercept and theme_level_slope is currently not supported.")
             }

             if (!is.null(object@lmer_control)) {
               if (!is.list(object@lmer_control) || any(!names(object@lmer_control) %in% c("lmer_optimizer", "lmer_optimization_objective", "hierarchical_p_value_method"))) {
                 return("lmer_control must be a list with 'lmer_optimizer', 'lmer_optimization_objective' and/or 'hierarchical_p_value_method'.")
               }
               if (!is.null(object@lmer_control$lmer_optimizer)){
                 if (!is.character(object@lmer_control$lmer_optimizer) || !object@lmer_control$lmer_optimizer %in% c("nloptwrap", "bobyqa", "Nelder_Mead", "nlminbwrap")) {
                   stop("lmer_optimizer must be one of 'nloptwrap', 'bobyqa', 'Nelder_Mead', or 'nlminbwrap'.")
                 }
               }
               if( !is.null(object@lmer_control$lmer_optimization_objective)){
                 if(!is.character(object@lmer_control$lmer_optimization_objective) || !object@lmer_control$lmer_optimization_objective %in% c("likelihood", "REML")){
                   stop("lmer_optimization_objective should be one of 'likelihood' or 'REML'.")
                 }
               }
             }
           }
           else {
             if(any(!is.null(object@theme_level_intercept), !is.null(object@theme_level_slope))){
               stop("Theme-level parameters are only avaiable for partial pooled models.")
             }
             if(!is.null(object@lmer_control)){
               stop("lmer_control is only avaiable for partial pooled models.")
             }
           }
           TRUE
         },
         prototype = list(
           signal_significance_threshold = 0.05,
           p_correction_method = "none",
           enable_theme_representativeness = TRUE,
           model_structure = "no_pooled",
           theme_level_intercept = NULL,
           theme_level_slope = NULL
         )
)

#' @title frequentist_alpha_test_strategy Class
#' @description A subclass of alpha_test_strategy for frequentist methods.
setClass("frequentist_alpha_test_strategy",
         contains = "alpha_test_strategy",
         validity = function(object) {
           if (object@p_correction_method == "bayesian") {
             stop("p_correction_method cannot be 'bayesian' for frequentist_alpha_test_strategy.")
           }
           TRUE
         }
)

#-----------------------------------------------------------------------
# bayesian_model_params
#-----------------------------------------------------------------------

#' @title bayesian_model_parameters Class
#' @description A class encapsulating parameters necessary to specify the hierarchical Bayesian model and its priors.
#'
#' @slot user_priors An object of class `brmsprior` with user-defined priors for the hierarchical Bayesian model.
#' Should be structured according to the `model_spec_theme_level`.
#' @slot prior_derivation_control A list of additional parameters for deriving priors when `priors_type` is `"informative_exogenous_dataset"`.
#' Should include:
#'   - `half_t_df`: Degrees of freedom for the half-t distribution applied to sd priors.
#'   - `lmer_optimizer`: Optimizer to be used in `lme4::lmer` for deriving priors.
#'     Options include: `"nloptwrap"`, `"bobyqa"`, `"Nelder_Mead"`, `"nlminbwrap"`.
#'   - `lmer_optimization_objective`: Criteria to be optimized in `lme4::lmer` for deriving priors.
#'     Options include: `likelihood`, `REML`.
#' @slot brms_control A list of additional parameters to be passed to `brms::brm` for MCMC sampling, including:
#'   - `chains`: Number of Markov chains to run (default is 4).
#'   - `iter`: Total number of iterations per chain (default is 2000).
#'   - `warmup`: Number of warmup iterations per chain (default is `floor(iter / 2)`).
#'   - `thin`: Thinning interval for MCMC sampling (default is 1).
#'   - `seed`: Seed for reproducibility (default is `NA` for random seeding).
#'   - `adapt_delta`: Target acceptance probability for the Hamiltonian Monte Carlo sampler (default is 0.99).
#'
#' @export
setClass(
  "bayesian_model_parameters",
  slots = list(
    user_priors = "ANY", # To accommodate brmsprior objects or NULL
    prior_derivation_control = "ANY",
    brms_control = "ANY"
  ),
  prototype = list(
    user_priors = NULL,
    brms_control = list(
      chains = 4,
      iter = 2000,
      warmup = 1000,
      thin = 1,
      seed = NA,
      adapt_delta = 0.80
    )
  ),
  validity = function(object) {

    # Validate user_priors if not NULL
    if (!is.null(object@user_priors)) {
      if (!inherits(object@user_priors, "brmsprior")) {
        return("user_priors must be a 'brmsprior' object.")
      }
    }
    # Validate prior_derivation_control
    if (!is.null(object@prior_derivation_control)) {
      if (!is.list(object@prior_derivation_control) || any(!names(object@prior_derivation_control) %in% c("half_t_df"))) {
        return("prior_derivation_control must be a list with 'half_t_df'.")
      }
      if (!is.null(object@prior_derivation_control$half_t_df)){
        if (!is.numeric(object@prior_derivation_control$half_t_df) || object@prior_derivation_control$half_t_df <= 0) {
          stop("half_t_df must be a positive numeric value.")
        }
      }
    }
    #Validate brms_control
    if(!is.null(object@brms_control)){
      if(any(!names(object@brms_control) %in% c("chains", "iter", "warmup", "thin", "seed", "adapt_delta"))){
        stop("brms_control must be a list containing 'chains', 'iter', 'warmup', 'thin', 'seed' and/or 'adapt_delta'.")
      }

      #chains
      if(!is.null(object@brms_control$chains)){
        if(!is.numeric(object@brms_control$chains) || object@brms_control$chains <= 0){
          stop("chains must be a positive number.")
        }
      }

      #iter
      if(!is.null(object@brms_control$iter)){
        if(!is.numeric(object@brms_control$iter) || object@brms_control$iter <= 0){
          stop("iter must be a positive number.")
        }
      }

      #warmup
      if(!is.null(object@brms_control$warmup)){
        if(!is.numeric(object@brms_control$warmup) || object@brms_control$warmup <= 0){
          stop("warmup must be a positive number.")
        }
      }

      #thin
      if(!is.null(object@brms_control$thin)){
        if(!is.numeric(object@brms_control$thin) || object@brms_control$thin <= 0){
          stop("thin must be a positive number.")
        }
      }

      #seed
      if(!is.null(object@brms_control$seed)){
        if(!is.numeric(object@brms_control$seed) || object@brms_control$seed <= 0){
          stop("seed must be a positive number.")
        }
      }

      #adapt_delta
      if(!is.null(object@brms_control$adapt_delta)){
        if(!is.numeric(object@brms_control$adapt_delta) || object@brms_control$adapt_delta <= 0 || object@brms_control$adapt_delta > 1){
          stop("adapt_delta should be between 0 and 1.")
        }
      }

      #warmup and iter
      if(!is.null(object@brms_control$warmup) && !is.null(object@brms_control$iter) && object@brms_control$warmup >= object@brms_control$iter){
        stop("warmup must be less than iter.")
      }
    }

    TRUE
  }
)

#' @title bayesian_alpha_test_strategy Class
#' @description A subclass of alpha_test_strategy for Bayesian methods.
#' @slot bayesian_model_parameters Parameters for the hierarchical Bayesian model.
setClass("bayesian_alpha_test_strategy",
         contains = "alpha_test_strategy",
         slots = list(
           bayesian_model_parameters = "bayesian_model_parameters"
         ),
         validity = function(object) {
           if (object@p_correction_method != "bayesian") {
             stop("p_correction_method must be 'bayesian' for bayesian_alpha_test_strategy.")
           }

           # Warnings for missing typical priors based on theme_level parameters
           theme_level_intercept <- object@theme_level_intercept
           theme_level_slope <- object@theme_level_slope

           if(!is.null(object@bayesian_model_parameters@user_priors)){
             #Get user priors
             priors_df <- as.data.frame(object@bayesian_model_parameters@user_priors)

             if (theme_level_intercept == "random" && theme_level_slope == "fixed") {
               # Check for prior with class 'sd', coef 'Intercept', group 'theme'
               required_row <- subset(priors_df, class == "sd" & coef == "Intercept" & group == "theme")
               if (nrow(required_row) == 0) {
                 warning("For this model specification, remember to add a prior with effect = 'random', type = 'intercept', and level = 'theme'.")
               }
             } else if (theme_level_intercept == "theme_specific" && theme_level_slope == "fixed") {
               # Check for priors with class 'b' and coef matching 'theme...'
               required_rows <- subset(priors_df, class == "b" & grepl("^theme", coef))
               if (nrow(required_rows) == 0) {
                 warning("For this model specification, it is recommended to include priors for type = 'intercept' and effect = 'fixed' for each desired theme.")
               }
             } else if (theme_level_intercept == "theme_specific" && theme_level_slope == "theme_specific") {

               # Check for priors with class 'b' and coef matching 'theme...' and 'theme...:market_factor_proxy'
               required_rows_intercepts <- subset(priors_df, class == "b" & grepl("^theme[^:]*$", coef))
               required_rows_slopes <- subset(priors_df, class == "b" & grepl("^theme[^:]*:market_factor_proxy$", coef))
               if (nrow(required_rows_intercepts) == 0) {
                 warning("For this model specification, it is recommended to include priors for type = 'intercept' and effect = 'fixed' for each desired theme.")
               }
               if (nrow(required_rows_slopes) == 0) {
                 warning("For this model specification, it is recommended to include priors for type = 'slope' and effect = 'fixed' for each desired theme.")
               }
             }
           }

           TRUE
         }
)

#-----------------------------------------------------------------------
# ss_backtest_config
#-----------------------------------------------------------------------

#' @title ss_backtest_config Class
#' @description The ss_backtest_config class is designed to define an end-to-end signal selection experiment based on
#' backtest returns of associated strategies. The class includes parameters for manipulating the backtest returns object and
#' conducting hypothesis tests regarding CAPM alpha under a multiple testing framework, with frequentist and bayesian approaches. In the
#' latter, a hierarhical model is fit, with informative priors set according to an exogeneous dataset or by the user, or
#' default uninformative priors.
#' @slot initial_sample_size A numeric indicating the minimum number of observations required to begin the backtest.
#' @slot split_method The method used for splitting the data, either "expanding" or "rolling" (default is "expanding").
#' @slot alpha_test_strategy An `alpha_test_strategy` object with the configuration for the alpha test.
#' @slot chosen_signals_and_positions A character indicating to which signals ss_backtest should be applied and their positions (long and short).
#' For example, chosen_signals_and_positions = c(book_yield = "long", vol_36m = "short").
#' @export
setClass("ss_backtest_config",
         slots = list(
           chosen_signals_and_positions = "character",
           initial_sample_size = "numeric",
           rebalancing_months = "numeric",
           active_returns = "logical",
           split_method = "character",
           alpha_test_strategy = "ANY",
           config_name = "character"
         ), prototype = list(
           split_method = "expanding"
         ),
         validity = function(object) {

           if(length(object@chosen_signals_and_positions) == 1){
             if(!object@chosen_signals_and_positions == "all"){
               stop("chosen_signals_and_positions should be 'all' or a named vector with signals and positions")
             }
           } else {
             if(any(!object@chosen_signals_and_positions %in% c("long", "short", "force"))){
               stop("chosen_signals_and_positions should be either 'long', 'short' or 'force'.")
             }
           }
           if(object@initial_sample_size < 0){
             stop("initial_sample_size can't be negative")
           }

           if (!is.null(object@alpha_test_strategy)){
             if (!inherits(object@alpha_test_strategy, "alpha_test_strategy")) {
               stop("alpha_test_strategy must be an object of class alpha_test_strategy")
             }
           }
         }
)

#-----------------------------------------------------------------------
# ss_backtest_results
#-----------------------------------------------------------------------
#' S4 Class for Signal Selection Backtest Results
#'
#' This S4 class encapsulates the results and parameters from performing a signal selection backtest.
#' It includes information about eligible signals, signal universes, Bayesian fits, and the backtest workflow.
#'
#' @slot signal_universe_m_df A meta dataframe containing the signal universes at each rebalancing period.
#' @slot final_signal_universe_m_d_ref A meta dataframe containing the last signal universe.
#' @slot final_bayesian_fit_list A list of Bayesian model fit results for each rebalancing period.
#' @slot p_correction_method A character string indicating the p-value correction method used.
#' @slot ss_backtest_workflow A list describing the signal selection backtest workflow, including parameters and metadata.
#' @slot backtest_identifier A character string representing the backtest identifier.
#'
#' @return An S4 object of class `ss_backtest_results`.
#'
#' @export
setClass(
  "ss_backtest_results",
  slots = list(
    ss_backtest_config = "ANY",
    signal_universe_m_df = "meta_dataframe",
    final_signal_universe_m_d_ref = "meta_dataframe",
    selected_market_factor_proxy_m_xts = "meta_xts",
    frequentist_results = "ANY",
    bayesian_results = "ANY",
    p_correction_method = "character",
    ss_backtest_workflow = "list",
    backtest_identifier = "character"
  )
)

#-----------------------------------------------------------------------
# sb_backtest_config
#-----------------------------------------------------------------------

#' @title sb_backtest_config Class
#' @description The sb_backtest_config class is designed to define an end-to-end signal-blending (heuristic or machine learning)
#' experiment, including the hyperparameter tuning strategy, algorithm parameters, and other experiment-specific configurations.
#' @slot sb_algorithm Character string specifying the signal-blending algorithm to be used. Should be one of
#' ew (Equal Weight), sw (Signal Weighting), rp (Risk Parity) or mvo (Mean Variance Optimization),
#' ols (Ordinary Least Squares), glmnet (Elastic Net), rf (Random Forest), xgb (eXtreme Gradient Boosting), and nn (Keras Neural Networks).
#' @slot target_fwd_name Name of the target variable in `target_m_df`.
#' @slot tuning_strategy An object of class `tuning_strategy`, specifying the strategy for tuning hyperparameters.
#' @slot ss_backtest_config An object of class `ss_backtest_config`, specifying the single strategy backtest configuration.
#' @slot ss_backtest_results An object of class `ss_backtest_results`, containing the results of the single strategy backtest.
#' @slot port_backtest_config An object of class `port_backtest_config`, containing instructions to create SB portfolios for heuristic algorithms.
#' @slot training_sample_size Number of observations to include in each training sample.
#' @slot rebalancing_months Months (numeric) when model should be rebalanced (refit).
#' @slot split_method Character string indicating the data splitting method ('expanding' or 'rolling').
#' @slot custom_objective Character string specifying the custom objective function ('squared_error', 'pseudo_huber_error', 'absolute_error') or NULL.
#' Custom objective  should be a double differentiable loss function and is only applicable for xgboost and nn algorithms.
#' @slot keras_architecture_parameters An object of class `keras_architecture_parameters` or NULL, providing parameters specific to keras-based neural networks.
#' It includes:
#' \itemize{
#'   \item \strong{units}: A numeric vector specifying the number of neurons in each layer.
#'   \item \strong{n_layers}: An integer indicating the total number of layers in the neural network.
#'   \item \strong{activation}: A character vector listing the activation functions for each layer (e.g., "relu", "sigmoid", "tanh").
#'   \item \strong{nn_optimizer}: A character string specifying the optimizer used for training the model (options: "Adam" or "RMSProp").
#'   \item \strong{batch_norm_option}: A logical vector indicating whether batch normalization should be applied after each respective layer (TRUE or FALSE).
#' }
#' @slot signal_port_parameters An object of class `signal_port_parameters`, specifying the parameters for constructing signal portfolios (portfolio-blending).
#' @slot quantile_tau A single numeric value indicating the tau parameter used for quantile regression, between 0 and 1.
#' @slot huber_delta A single positive numeric value indicating the boundary that separates where the loss function turns from quadratic to linear.
#' @slot config_name A character string to identify the configuration.
#' @export
setClass(
  "sb_backtest_config",
  slots = list(
    sb_algorithm = "character",
    target_fwd_name = "character",
    tuning_strategy = "ANY",
    ss_backtest_config = "ANY",
    ss_backtest_results = "ANY",
    chosen_signals_and_positions = "character",
    split_method = "character",
    training_sample_size = "numeric",
    rebalancing_months = "numeric",
    custom_objective = "character",
    keras_architecture_parameters = "ANY",
    signal_port_parameters = "ANY",
    quantile_tau = "numeric",
    huber_delta = "numeric",
    config_name = "character"
  ),
  prototype = list(
    sb_algorithm = "ols",
    split_method = "expanding",
    custom_objective = "squared_error",
    quantile_tau = 0.5,
    huber_delta = 1
  ),
  validity = function(object) {

    #Check for ss_backtest_config OR ss_backtest_results
    if(!is.null(object@ss_backtest_config) && !is.null(object@ss_backtest_results)) {
      return("Only one of a ss_backtest_config or a ss_backtest_results object should be provided.")
    }
    ##SS Backtest Config Class
    if(!is.null(object@ss_backtest_config)){
      if(!inherits(object@ss_backtest_config, "ss_backtest_config")) {
        return("ss_backtest_config must be of class 'ss_backtest_config'.")
      }
    }
    ##SS Backtest Results Class
    if(!is.null(object@ss_backtest_results)){
      if(!inherits(object@ss_backtest_results, "ss_backtest_results")) {
        return("ss_backtest_results must be of class 'ss_backtest_results'.")
      }
    }
    ##Chosen Signals and Positions
    if (is.null(object@ss_backtest_config) && is.null(object@ss_backtest_results)) {
      if (length(object@chosen_signals_and_positions) == 1){
        if(!object@chosen_signals_and_positions == "all"){
          stop("chosen_signals_and_positions should be 'all' or a named vector with signals and positions")
        }
      } else {
        if(any(!object@chosen_signals_and_positions %in% c("long", "short"))){
          stop("chosen_signals_and_positions should be either 'long' or 'short'.")
        }
      }
    }


    #Check for valid sb_algorithm
    valid_sb_algorithms <- c("ols", "glmnet", "rf", "xgb", "nn", "ew", "sw", "rp", "mvo", "custom_weights")
    if(!(object@sb_algorithm %in% valid_sb_algorithms)) {
      return("Invalid sb_algorithm. Choose from 'ew', 'sw', 'rp', 'mvo', 'ols', 'glmnet', 'rf', 'xgb', 'nn' or 'custom_weights'.")
    }

    #Check for custom objective
    if(object@sb_algorithm %in% c("sw", "mvo")){
      if (!grepl("^max_|^min_", object@custom_objective)){
        stop("Invalid custom_objective. Should be 'max_' or 'min_' + one of valid heuristic performance metrics.
             To see complete list of valid heuristic performance metrics, use 'display_valid_custom_objectives()'")
      }
      ###Valid Metrics
      valid_heuristic_sb_metrics <- c(
        "arith_mean_ret", "geom_mean_ret", "ann_ret", "std_dev", "ann_std_dev",
        "semi_dev", "down_dev", "dd_dev", "down_freq", "exp_short", "pain", "ulcer", "max_dd", "skew", "kurt",
        "sharpe_ratio", "ann_sharpe_ratio", "sharpe_ratio_semi_dev", "sortino_ratio", "ann_burke_ratio",
        "inv_d_ratio", "sharpe_ratio_exp_short", "ann_pain_ratio", "ann_martin_ratio", "ann_calmar_ratio",
        "ann_adj_sharpe_ratio", "omega", "rachev_ratio", "avg_dd_rec", "avg_dd_length", "hurst", "min_track_record",
        "prob_sharpe_ratio", "modigliani", "ann_modigliani",
        "act_arith_mean_ret", "act_geom_mean_ret", "act_ann_ret", "track_err", "ann_track_err",
        "act_semi_dev", "act_down_dev", "act_dd_dev", "act_down_freq", "act_exp_short", "act_pain", "act_ulcer",
        "act_max_dd", "act_skew", "act_kurt", "info_ratio", "ann_info_ratio", "info_ratio_semi_dev",
        "act_sortino_ratio", "act_ann_burke_ratio", "act_inv_d_ratio", "info_ratio_exp_short", "act_ann_pain_ratio",
        "act_ann_martin_ratio", "act_ann_calmar_ratio", "ann_adj_info_ratio", "act_omega", "act_rachev_ratio",
        "act_avg_dd_rec", "act_avg_dd_length", "act_hurst", "act_min_track_record", "prob_info_ratio",
        "act_modigliani", "act_ann_modigliani",
        "alpha", "theme_alpha", "individual_alpha", "alpha_se", "theme_beta", "individual_beta", "specific_risk",
        "alpha_t_stat", "treynor_ratio", "appraisal_ratio", "p_value",
        "posterior_theme_alpha", "posterior_individual_alpha", "posterior_alpha_se", "posterior_theme_beta", "posterior_individual_beta",
        "posterior_specific_risk", "posterior_alpha_t_stat", "posterior_treynor_ratio", "posterior_appraisal_ratio", "pd_theme_alpha", "pd_alpha"
      )
      valid_oos_eval_metrics <- c("rss", "cp", "rmse", "mae", "mphe", "mpe", "mape", "hr", "mb")
      if (!substr(object@custom_objective, 5, nchar(object@custom_objective)) %in% c(valid_heuristic_sb_metrics, valid_oos_eval_metrics)){
        warning("Custom_objective is not one of typical valid heuristic performance. Please be sure that the metric is present in signal_univers_m_df")
      }

      if (substr(object@custom_objective, 5, nchar(object@custom_objective)) %in% valid_oos_eval_metrics){
        message("This custom_objective is valid only for meta sb backtests. Please be sure that the current configuration is for such a backtest.")
      }

      typical_max_objective <- c(
        "arith_mean_ret", "geom_mean_ret", "ann_ret",
        "sharpe_ratio", "ann_sharpe_ratio", "sharpe_ratio_semi_dev", "sortino_ratio", "ann_burke_ratio",
        "inv_d_ratio", "sharpe_ratio_exp_short", "ann_pain_ratio", "ann_martin_ratio", "ann_calmar_ratio",
        "ann_adj_sharpe_ratio", "omega", "rachev_ratio", "avg_dd_rec", "hurst",
        "prob_sharpe_ratio", "modigliani", "ann_modigliani",
        "act_arith_mean_ret", "act_geom_mean_ret", "act_ann_ret",
        "info_ratio", "ann_info_ratio", "info_ratio_semi_dev",
        "act_sortino_ratio", "act_ann_burke_ratio", "act_inv_d_ratio", "info_ratio_exp_short", "act_ann_pain_ratio",
        "act_ann_martin_ratio", "act_ann_calmar_ratio", "ann_adj_info_ratio", "act_omega", "act_rachev_ratio",
        "act_avg_dd_rec", "act_avg_dd_length", "act_hurst", "prob_info_ratio",
        "act_modigliani", "act_ann_modigliani",
        "alpha", "theme_alpha", "individual_alpha", "alpha_se",
        "alpha_t_stat", "treynor_ratio", "appraisal_ratio",
        "posterior_theme_alpha", "posterior_individual_alpha", "posterior_theme_beta", "posterior_individual_beta",
        "posterior_alpha_t_stat", "posterior_treynor_ratio", "posterior_appraisal_ratio", "pd_theme_alpha", "pd_alpha",
        "rss", "cp", "hr", "mb"
      )

      if (any(stringr::str_remove(object@custom_objective, "min_") %in% typical_max_objective %in% typical_max_objective)){
        warning("Custom_objective is min, but chosen metric is typically maximized.")
      }

      typical_min_objective <- setdiff(valid_heuristic_sb_metrics, typical_max_objective)

      if (any(stringr::str_remove(object@custom_objective, "max_") %in% typical_min_objective)){
        warning("Custom_objective is max, but chosen metric is typically minimized.")
      }

    } else {
      if (!is.null(object@custom_objective) && !(object@custom_objective %in% c("squared_error", "pseudo_huber_error", "absolute_error"))) {
        return("Invalid custom_objective. Choose from 'squared_error', 'pseudo_huber_error', or 'absolute_error'.")
      }
    }
    if (!(object@sb_algorithm %in% c("xgb", "nn", "sw", "mvo")) && !is.null(object@custom_objective) && object@custom_objective != "squared_error") {
      return("Invalid custom_objective. Custom objectives are only allowed for 'sw', 'mvo', 'xgb' or 'nn' algorithms.")
    }
    if (!(object@sb_algorithm %in% c("xgb", "nn")) && !is.null(object@tuning_strategy) && !is.null(object@tuning_strategy@early_stop)) {
      return("Invalid early_stop. Early stop is only allowed for 'xgb' or 'nn' algorithms.")
    }
    #Check for tuning strategy
    if(!object@sb_algorithm %in% c("ew", "sw", "rp", "mvo", "ols", "custom_weights") && is.null(object@tuning_strategy)){
      message("when sb_algorithm is not 'ew', 'sw', 'rp', 'mvo' or 'ols', a tuning_strategy must be set")
    }
    #ETC
    if((object@training_sample_size < 0)){
      stop("training_sample_size should be positive.")
    }
    if (object@split_method != "expanding") {
      return("split_method should be expanding.")
    }
    if (object@rebalancing_months < 0 || object@rebalancing_months > 12){
      stop("rebalancing_months should be between 1 and 12.")
    }
    ##Keras
    if(!is.null(object@keras_architecture_parameters)){
      if(!is_keras_architecture_parameters(object@keras_architecture_parameters)){
        return("Invalid keras_architecture_parameters. Should be of class keras_architecture_parameters")
      }
      if(object@sb_algorithm != "nn"){
        return("keras_architecture_parameters is only needed when sb_algorithm is nn")
      }
    }
    ##Heuristic Portfolio
    if(!is.null(object@signal_port_parameters)){
      if(!inherits(object@signal_port_parameters, "signal_port_parameters")){
        return("Invalid signal_port_parameters Should be of class signal_port_parameters")
      }
      if(!object@sb_algorithm %in% c("rp", "mvo")){
        return("signal_port_parameters is only needed when sb_algorithm is rp or mvo")
      }
    }
    ##Quantile tau and huber delta
    if (!is.null(object@quantile_tau) && (object@quantile_tau <= 0 || object@quantile_tau >= 1)) {
      return("quantile_tau must be between 0 and 1.")
    }
    if (!is.null(object@huber_delta) && object@huber_delta <= 0) {
      return("huber_delta must be greater than 0.")
    }
    #Check if hypers are correctly set
    if(!is.null(object@tuning_strategy)){
      if(!is_tuning_strategy(object@tuning_strategy)){
        return("Invalid tuning_strategy. Should be of class tuning_strategy")
      }
      if(object@sb_algorithm %in% c("ew", "sw", "rp", "mvo", "ols", "custom_weights")){
        return("ew, sw, rp, mvo, ols and custom_weights do not support hyperparameter tuning")
      }

      # Check hyperparameters validity based on sb_algorithm
      hyperparameters_names <- names(object@tuning_strategy@hyper_grid_domain@hyperparameter_list)

      # GLMNET
      expected_hyperparameters_glmnet <- c("alpha", "lambda.min.ratio")
      if (object@sb_algorithm == "glmnet" && any(!hyperparameters_names %in% expected_hyperparameters_glmnet)) {
        stop("hyperparameters do not match sb_algorithm choice for 'glmnet'")
      }
      hyperparameters_missing <- expected_hyperparameters_glmnet[which(!expected_hyperparameters_glmnet %in% hyperparameters_names)]
      if(length(hyperparameters_missing) != 0 && object@sb_algorithm == "glmnet"){
        cat("\n")
        message(paste("The following hyperparameter(s) must still be configured for the",
                      object@sb_algorithm, "algorithm:",
                      paste(hyperparameters_missing, collapse = ", ")))
        cat("\n")
      }


      # RF
      expected_hyperparameters_rf <- c("mtry", "num.trees", "max.depth", "min.bucket")
      if (object@sb_algorithm == "rf" && any(!hyperparameters_names %in% expected_hyperparameters_rf)) {
        stop("hyperparameters do not match sb_algorithm choice for 'rf'")
      }
      hyperparameters_missing <- expected_hyperparameters_rf[which(!expected_hyperparameters_rf %in% hyperparameters_names)]
      if(length(hyperparameters_missing) != 0 && object@sb_algorithm == "rf"){
        cat("\n")
        message(paste("The following hyperparameter(s) must still be configured for the",
                      object@sb_algorithm, "algorithm:",
                      paste(hyperparameters_missing, collapse = ", ")))
        cat("\n")
      }


      # XGB
      expected_hyperparameters_xgb <- c("min_child_weight", "max_depth", "subsample", "colsample_bytree", "eta", "alpha", "gamma", "nrounds")
      if (object@sb_algorithm == "xgb" && any(!hyperparameters_names %in% expected_hyperparameters_xgb)) {
        stop("hyperparameters do not match sb_algorithm choice for 'xgb'")
      }
      hyperparameters_missing <- expected_hyperparameters_xgb[which(!expected_hyperparameters_xgb %in% hyperparameters_names)]
      if(length(hyperparameters_missing) != 0 && object@sb_algorithm == "xgb"){
        cat("\n")
        message(paste("The following hyperparameter(s) must still be configured for the",
                      object@sb_algorithm, "algorithm:",
                      paste(hyperparameters_missing, collapse = ", ")))
        cat("\n")
      }


      # NN
      expected_hyperparameters_nn <- c("regularizer_l1", "regularizer_l2", "droprate", "lr", "size_of_batch", "number_of_epochs")
      if (object@sb_algorithm == "nn" && any(!hyperparameters_names %in% expected_hyperparameters_nn)) {
        stop("hyperparameters do not match sb_algorithm choice for 'nn'")
      }
      hyperparameters_missing <- expected_hyperparameters_nn[which(!expected_hyperparameters_nn %in% hyperparameters_names)]
      if(length(hyperparameters_missing) != 0 && object@sb_algorithm == "nn"){
        cat("\n")
        message(paste("The following hyperparameter(s) must still be configured for the",
                      object@sb_algorithm, "algorithm:",
                      paste(hyperparameters_missing, collapse = ", ")))
        cat("\n")
      }

    }


    # Validate hyperparameters based on sb_algorithm
    if(!is.null(object@tuning_strategy) && !is.null(object@tuning_strategy@hyper_grid_domain@hyperparameter_list)){

      #Hyperparameters
      hyperparameter_list <- object@tuning_strategy@hyper_grid_domain@hyperparameter_list
      tuning_method <- object@tuning_strategy@tuning_method

      if (length(hyperparameter_list) > 0) {
        for (hyperparameter in names(hyperparameter_list)) {
          #Extract value for grid_searcho or bayesian_opt
          value <- hyperparameter_list[[hyperparameter]]

          #Extract distribution and parameters for random_search
          if(tuning_method == "random_search"){
            distribution_choice <- object@tuning_strategy@hyper_grid_domain@hyperparameter_list[[hyperparameter]]$distribution_choice  #Distribution Choice
            pars <- object@tuning_strategy@hyper_grid_domain@hyperparameter_list[[hyperparameter]]$pars  #Pars
          }

          #GLMET Logic
          if (object@sb_algorithm == "glmnet") {
            if(tuning_method != "random_search"){
              if (hyperparameter == "alpha" && (any(value < 0) || any(value > 1))) {
                stop("alpha should be set in interval [0, 1]")
              }
              if (hyperparameter == "lambda.min.ratio" && (any(value < 0) || any(value >= 1))) {
                stop("lambda.min.ratio should be set in interval [0, 1)")
              }
            } else {
              if (hyperparameter == "alpha"){
                if(distribution_choice == "uniform" && pars["max"] > 1) warning("max above upper range for alpha")
                if(distribution_choice == "uniform" && pars["min"] < 0) warning("min below lower range for alpha")
                if(distribution_choice == "normal" && (pars["mean"] + pars["sd"]/2 > 1)) warning("mean + sd/2 above upper range for alpha")
                if(distribution_choice == "normal" && (pars["mean"] - pars["sd"]/2 < 0)) warning("mean - sd/2 below lower range for alpha")
                if(distribution_choice == "lognormal" && (pars["meanlog"] + pars["sdlog"]/2 > 1)) warning("meanlog + sdlog/2 above upper range for alpha")
                if(distribution_choice == "lognormal" && (pars["meanlog"] - pars["sdlog"]/2 < 0)) warning("meanlog - sdlog/2 below lower range for alpha")
                if(distribution_choice == "constant" && (any(value$value < 0) || any(value$value > 1))) stop("alpha should be set in interval [0, 1]")
              }
              if (hyperparameter == "lambda.min.ratio"){
                if(distribution_choice == "uniform" && pars["max"] >= 1) warning("max above upper range for lambda.min.ratio")
                if(distribution_choice == "uniform" && pars["min"] < 0) warning("min below lower range for lambda.min.ratio")
                if(distribution_choice == "normal" && (pars["mean"] + pars["sd"]/2 >= 1)) warning("mean + sd/2 above upper range for lambda.min.ratio")
                if(distribution_choice == "normal" && (pars["mean"] - pars["sd"]/2 < 0)) warning("mean - sd/2 below lower range for lambda.min.ratio")
                if(distribution_choice == "lognormal" && (pars["meanlog"] + pars["sdlog"]/2 >= 1)) warning("meanlog + sdlog/2 above upper range for lambda.min.ratio")
                if(distribution_choice == "lognormal" && (pars["meanlog"] + pars["sdlog"]/2 < 0)) warning("meanlog - sdlog/2 below lower range for lambda.min.ratio")
                if(distribution_choice == "constant" && (any(value$value < 0) || any(value$value >= 1)))stop("lambda.min.ratio should be set in interval [0, 1)")
              }
            }
          }

          #RF Logic
          if (object@sb_algorithm == "rf") {
            if(tuning_method != "random_search"){
              if (hyperparameter == "num.trees" && tuning_method == "bayesian_opt" && (any(value <= 0) || any(!is.integer(value)))) {
                stop("num.trees should be a positive integer without decimals")
              }
              if (hyperparameter == "num.trees" && tuning_method == "grid_search" && (any(value <= 0) || any(!(value == floor(value))))) {
                stop("num.trees should be a positive integer without decimals")
              }
              if (hyperparameter == "mtry" && (any(value < 0) || any(value > 1))) {
                stop("mtry should be set in interval [0, 1]")
              }
              if (hyperparameter == "max.depth" && tuning_method == "bayesian_opt" && (any(value <= 0) || any(!is.integer(value)))) {
                stop("max.depth should be a positive integer without decimals")
              }
              if (hyperparameter == "max.depth" && tuning_method == "grid_search" && (any(value <= 0) || any(!(value == floor(value))))) {
                stop("max.depth should be a positive integer without decimals")
              }

            } else {
              if (hyperparameter == "num.trees"){
                if(distribution_choice != "constant" && any(!is.integer(pars))) stop("pars should be set as integers for num.trees")
                if(distribution_choice == "uniform" && pars["min"] < 0) warning("min below lower range for num.trees")
                if(distribution_choice == "normal" && (pars["mean"] - pars["sd"]/2 < 0)) warning("mean - sd/2 below lower range for num.trees")
                if(distribution_choice == "lognormal" && (pars["meanlog"] - pars["sdlog"]/2 < 0)) warning("meanlog - sdlog/2 below lower range for num.trees")
                if(distribution_choice == "constant" && (any(value$value < 0))) stop("num.trees should be positive")
              }
              if (hyperparameter == "mtry"){
                if(distribution_choice == "uniform" && pars["max"] > 1) warning("max above upper range for mtry")
                if(distribution_choice == "uniform" && pars["min"] < 0) warning("min below lower range for mtry")
                if(distribution_choice == "normal" && (pars["mean"] + pars["sd"]/2 > 1)) warning("mean + sd/2 above upper range for mtry")
                if(distribution_choice == "normal" && (pars["mean"] - pars["sd"]/2 < 0)) warning("mean - sd/2 below lower range for mtry")
                if(distribution_choice == "lognormal" && (pars["meanlog"] + pars["sdlog"]/2 > 1)) warning("meanlog + sdlog/2 above upper range for mtry")
                if(distribution_choice == "lognormal" && (pars["meanlog"] - pars["sdlog"]/2 < 0)) warning("meanlog - sdlog/2 below lower range for mtry")
                if(distribution_choice == "constant" && (any(value$value < 0) || any(value$value > 1))) stop("mtry should be set in interval [0, 1]")
              }
              if (hyperparameter == "max.depth"){
                if(distribution_choice != "constant" && any(!is.integer(pars))) stop("pars should be set as integers for max.depth")
                if(distribution_choice == "uniform" && pars["min"] < 0) warning("min below lower range for max.depth")
                if(distribution_choice == "normal" && (pars["mean"] - pars["sd"]/2 < 0)) warning("mean - sd/2 below lower range for max.depth")
                if(distribution_choice == "lognormal" && (pars["meanlog"] - pars["sdlog"]/2 < 0)) warning("meanlog - sdlog/2 below lower range for max.depth")
                if(distribution_choice == "constant" && (any(value$value < 0))) stop("max.depth should be positive")
              }
            }
          }

          #XGB Logic
          if (object@sb_algorithm == "xgb") {
            if(tuning_method != "random_search"){
              if (hyperparameter == "eta" && (any(value < 0) || any(value >= 1))) {
                stop("eta should be set in interval [0, 1)")
              }
              if (hyperparameter == "colsample_bytree" && (any(value < 0) || any(value > 1))) {
                stop("colsample_bytree should be set in interval [0, 1]")
              }
              if (hyperparameter == "subsample" && (any(value < 0) || any(value > 1))) {
                stop("subsample should be set in interval [0, 1]")
              }
              if (hyperparameter == "max_depth" && tuning_method == "grid_search" && (any(value <= 0) || any(!(value == floor(value))))) {
                stop("max_depth should be a positive integer without decimals")
              }
              if (hyperparameter == "max_depth" && tuning_method == "bayesian_opt" && (any(value <= 0) || any(!is.integer(value)))) {
                stop("max_depth should be a positive integer without decimals")
              }

            } else {
              if (hyperparameter == "eta"){
                if(distribution_choice == "uniform" && pars["max"] > 1) warning("max above upper range for eta")
                if(distribution_choice == "uniform" && pars["min"] < 0) warning("min below lower range for eta")
                if(distribution_choice == "normal" && (pars["mean"] + pars["sd"]/2 > 1)) warning("mean + sd/2 above upper range for eta")
                if(distribution_choice == "normal" && (pars["mean"] - pars["sd"]/2 < 0)) warning("mean - sd/2 below lower range for eta")
                if(distribution_choice == "lognormal" && (pars["meanlog"] + pars["sdlog"]/2 > 1)) warning("meanlog + sdlog/2 above upper range for eta")
                if(distribution_choice == "lognormal" && (pars["meanlog"] - pars["sdlog"]/2 < 0)) warning("meanlog - sdlog/2 below lower range for eta")
                if(distribution_choice == "constant" && (any(value$value < 0) || any(value$value > 1))) stop("eta should be set in interval [0, 1]")
              }
              if (hyperparameter == "colsample_bytree"){
                if(distribution_choice == "uniform" && pars["max"] > 1) warning("max above upper range for colsample_bytree")
                if(distribution_choice == "uniform" && pars["min"] < 0) warning("min below lower range for colsample_bytree")
                if(distribution_choice == "normal" && (pars["mean"] + pars["sd"]/2 > 1)) warning("mean + sd/2 above upper range for colsample_bytree")
                if(distribution_choice == "normal" && (pars["mean"] - pars["sd"]/2 < 0)) warning("mean - sd/2 below lower range for colsample_bytree")
                if(distribution_choice == "lognormal" && (pars["meanlog"] + pars["sdlog"]/2 > 1)) warning("meanlog + sdlog/2 above upper range for colsample_bytree")
                if(distribution_choice == "lognormal" && (pars["meanlog"] - pars["sdlog"]/2 < 0)) warning("meanlog - sdlog/2 below lower range for colsample_bytree")
                if(distribution_choice == "constant" && (any(value$value < 0) || any(value$value > 1))) stop("colsample_bytree should be set in interval [0, 1]")
              }
              if (hyperparameter == "subsample"){
                if(distribution_choice == "uniform" && pars["max"] > 1) warning("max above upper range for subsample")
                if(distribution_choice == "uniform" && pars["min"] < 0) warning("min below lower range for subsample")
                if(distribution_choice == "normal" && (pars["mean"] + pars["sd"]/2 > 1)) warning("mean + sd/2 above upper range for subsample")
                if(distribution_choice == "normal" && (pars["mean"] - pars["sd"]/2 < 0)) warning("mean - sd/2 below lower range for subsample")
                if(distribution_choice == "lognormal" && (pars["meanlog"] + pars["sdlog"]/2 > 1)) warning("meanlog + sdlog/2 above upper range for subsample")
                if(distribution_choice == "lognormal" && (pars["meanlog"] - pars["sdlog"]/2 < 0)) warning("meanlog - sdlog/2 below lower range for subsample")
                if(distribution_choice == "constant" && (any(value$value < 0) || any(value$value > 1))) stop("subsample should be set in interval [0, 1]")
              }
              if (hyperparameter == "max_depth"){
                if(distribution_choice != "constant" && any(!is.integer(pars))) warning("pars should be set as integers for max_depth")
                if(distribution_choice == "uniform" && pars["min"] < 0) warning("min below lower range for max_depth")
                if(distribution_choice == "normal" && (pars["mean"] - pars["sd"]/2 < 0)) warning("mean - sd/2 below lower range for max_depth")
                if(distribution_choice == "lognormal" && (pars["meanlog"] - pars["sdlog"]/2 < 0)) warning("meanlog - sdlog/2 below lower range for max_depth")
                if(distribution_choice == "constant" && (any(value$value < 0))) stop("max_depth should be positive")
              }
            }
          }


          #NN Logic
          if (object@sb_algorithm == "nn") {
            if(tuning_method != "random_search"){
              if (hyperparameter == "droprate" && (any(value < 0) || any(value >= 1))) {
                stop("droprate should be set in interval [0, 1)")
              }
              if (hyperparameter == "number_of_epochs" && tuning_method == "grid_search" && (any(value <= 0) || any(!(value == floor(value))))) {
                stop("number_of_epochs should be a positive integer without decimals")
              }
              if (hyperparameter == "number_of_epochs" && tuning_method == "bayesian_opt" && (any(value <= 0) || any(!is.integer(value)))) {
                stop("number_of_epochs should be a positive integer without decimals")
              }
              if (hyperparameter == "size_of_batch" && tuning_method == "grid_search" && (any(value <= 0) || any(!(value == floor(value))))) {
                stop("size_of_batch should be a positive integer without decimals")
              }
              if (hyperparameter == "size_of_batch" && tuning_method == "bayesian_opt" && (any(value <= 0) || any(!is.integer(value)))) {
                stop("size_of_batch should be a positive integer without decimals")
              }
            } else {
              if (hyperparameter == "droprate"){
                if(distribution_choice == "uniform" && pars["max"] > 1) warning("max above upper range for droprate")
                if(distribution_choice == "uniform" && pars["min"] < 0) warning("min below lower range for droprate")
                if(distribution_choice == "normal" && (pars["mean"] + pars["sd"]/2 > 1)) warning("mean + sd/2 above upper range for droprate")
                if(distribution_choice == "normal" && (pars["mean"] - pars["sd"]/2 < 0)) warning("mean - sd/2 below lower range for droprate")
                if(distribution_choice == "lognormal" && (pars["meanlog"] + pars["sdlog"]/2 > 1)) warning("meanlog + sdlog/2 above upper range for droprate")
                if(distribution_choice == "lognormal" && (pars["meanlog"] - pars["sdlog"]/2 < 0)) warning("meanlog - sdlog/2 below lower range for droprate")
                if(distribution_choice == "constant" && (any(value$value < 0) || any(value$value > 1))) stop("droprate should be set in interval [0, 1]")
              }
              if (hyperparameter == "number_of_epochs"){
                if(distribution_choice != "constant" && any(!is.integer(pars))) stop("pars should be set as integers for number_of_epochs")
                if(distribution_choice == "uniform" && pars["min"] < 0) warning("min below lower range for number_of_epochs")
                if(distribution_choice == "normal" && (pars["mean"] - pars["sd"]/2 < 0)) warning("mean - sd/2 below lower range for number_of_epochs")
                if(distribution_choice == "lognormal" && (pars["meanlog"] - pars["sdlog"]/2 < 0)) warning("meanlog - sdlog/2 below lower range for number_of_epochs")
                if(distribution_choice == "constant" && (any(value$value < 0))) stop("number_of_epochs should be positive")
              }
              if (hyperparameter == "size_of_batch"){
                if(distribution_choice != "constant" && any(!is.integer(pars))) stop("pars should be set as integers for size_of_batch")
                if(distribution_choice == "uniform" && pars["min"] < 0) warning("min below lower range for size_of_batch")
                if(distribution_choice == "normal" && (pars["mean"] - pars["sd"]/2 < 0)) warning("mean - sd/2 below lower range for size_of_batch")
                if(distribution_choice == "lognormal" && (pars["meanlog"] - pars["sdlog"]/2 < 0)) warning("meanlog - sdlog/2 below lower range for size_of_batch")
                if(distribution_choice == "constant" && (any(value$value < 0))) stop("size_of_batch should be positive")
              }
            }
          }
        }
        #Check if init_points > number of hypers
        if(tuning_method == "bayesian_opt" && length(hyperparameter_list) >= object@tuning_strategy@init_points){
          stop("init_points should be greater than the number of hyperparameters")
        }

      }
    }
    return(TRUE)
  }
)


#' @title sb_metabacktest_config Class
#' @description The sb_metabacktest_config class is designed to store and manage a collection of sb_backtest_config objects.
#' @slot meta_sb_backtest_config A `sb_backtest_config` with the configuration for the meta learner
#' @slot base_sb_backtest_configs A list of `sb_backtest_config` objects whose oos predictions will be fed to the meta learner.
#' @slot base_sb_backtest_results A list of `sb_backtest_result` objects whose oos predictions will be fed to the meta learner.
#' @slot normalize_predictions Logical; if \code{TRUE}, normalizes the base learners' predictions before passing them to the meta learner. Default is \code{TRUE}.
#' @slot features_passthrough A character vector indicating which features from \code{features_m_df} are to be passed through to the meta learner.
#'   Alternatively, if \code{'all'}, all features are passed through. If \code{'none'}, no features are passed through. Default is \code{'none'}.
#' @slot config_name A character string with the name of the configuration
#' @export
setClass(
  "sb_metabacktest_config",
  slots = list(
    meta_sb_backtest_config = "sb_backtest_config",
    base_sb_backtest_configs = "ANY",
    base_sb_backtest_results = "ANY",
    features_passthrough = "character",
    normalize_base_predictions = "logical",
    winsorize_base_predictions = "logical",
    config_name = "character"
  ),
  validity = function(object) {

    #Check for tuning strat
    if (!object@meta_sb_backtest_config@sb_algorithm %in% c("ols", "ew", "sw", "rp", "mvo") && is.null(object@meta_sb_backtest_config@tuning_strategy)){
      stop("tuning_strategy in meta_sb_backtest_config can't be NULL (except for ols and heuristic sb algorithms).")
    }

    #Check for rp or mvo at meta-level
    if (object@meta_sb_backtest_config@sb_algorithm %in% c("rp", "mvo")){
      stop("rp and mvo are not supported at meta-level at this time.")
    }

    #Check for ss_backtest_config/results at meta-level
    if (all(length(object@meta_sb_backtest_config@ss_backtest_config) == 0, length(object@meta_sb_backtest_config@ss_backtest_results) == 0)){
      if (object@meta_sb_backtest_config@chosen_signals_and_positions != "all"){
        stop("chosen_signals_and_positions should always be 'all' at meta-level.",
             "This is because features positions are already corrected through features_passthrough, which will replicate base chosen_signal_and_positions.")
      }
    }
    if (length(object@meta_sb_backtest_config@ss_backtest_config) > 0){
      stop("meta-level signal selection is not supported at this time.")
      if(object@meta_sb_backtest_config@ss_backtest_config@chosen_signals_and_positions != "all"){
        stop("chosen_signals_and_positions should always be 'all' at meta-level.",
             "This is because features positions are already corrected through features_passthrough, which will replicate base chosen_signal_and_positions.")
      }
    }
    if (length(object@meta_sb_backtest_config@ss_backtest_results) > 0){
      stop("meta-level signal selection is not supported at this time.")
    }

    #Check for features_passthrough
    if (any(object@features_passthrough %in% c("long", "short", "force"))){
      stop ("features_passthrough should just declare which signals from features_m_df should be added to meta learner features.
            Postions will be corrected based on chosen_signals_and_positions.")
    }

    #Check for simultaneous base_sb_backtest_configs and base_sb_backtest_results
    if (!is.null(object@base_sb_backtest_configs) & !is.null(object@base_sb_backtest_results)){
      stop("base_sb_backtest_configs and base_sb_backtest_results can't be set at the same time.")
    }


    #Base SB Backtest Configs Check
    if (!is.null(object@base_sb_backtest_configs)){

      if (length(object@base_sb_backtest_configs) == 1){
        stop("base_sb_backtest_configs should contain more than one sb_backtest_config objects.")
      }

      if (!all(sapply(object@base_sb_backtest_configs, function(x) is(x, "sb_backtest_config")))) {
        stop("All elements in 'base_sb_backtest_configs' must be of class 'sb_backtest_config'.")
      }

      # Initialize an empty character vector to collect error messages
      errors <- character()

      # Check that all elements are sb_backtest_config objects
      if (!all(sapply(object@base_sb_backtest_configs, function(x) is(x, "sb_backtest_config")))) {
        errors <- c(errors, "All elements in 'base_sb_backtest_configs' must be of class 'sb_backtest_config'.")
      }

      # Check for identical objects in base_sb_backtest_configs
      num_configs <- length(object@base_sb_backtest_configs)
      for (i in 1:(num_configs - 1)) {
        for (j in (i + 1):num_configs) {
          if (identical(object@base_sb_backtest_configs[[i]], object@base_sb_backtest_configs[[j]])) {
            return("Duplicate objects found in 'base_sb_backtest_configs'. Each configuration must be unique.")
          }
        }
      }

      # Check for duplicate names in base_sb_backtest_configs
      config_names <- names(object@base_sb_backtest_configs)
      if (any(duplicated(config_names))) {
        return("Duplicate names found in 'base_sb_backtest_configs'. Each configuration must have a unique name.")
      }

      # Check that training_sample_size + validation_sample_size matches across all configurations
      sample_sizes <- sapply(object@base_sb_backtest_configs, function(x){
        x@training_sample_size + if(!x@sb_algorithm %in% c("ols", "ew", "sw", "rp", "mvo")) x@tuning_strategy@validation_sample_size else 0
      })
      if (length(unique(sample_sizes)) > 1) {
        errors <- c(errors, "Training sample size + validation sample size must match across all 'sb_backtest_config' elements.")
      }

      # Check that rebalancing months match
      rebalancing_months <- sapply(object@base_sb_backtest_configs, function(x) x@rebalancing_months)
      if (length(unique(rebalancing_months)) > 1){
        errors <- c(errors, "Rebalancing months must match across all 'sb_backtest_config' elements.")
      }


      # Loop over each sb_backtest_config in the list and check general params
      for (i in seq_along(object@base_sb_backtest_configs)) {
        config <- object@base_sb_backtest_configs[[i]]

        # Get sb_algorithm
        sb_algorithm <- config@sb_algorithm

        # If ml_algo is ols, skip hyperparameter checks
        if (sb_algorithm %in% c("ols", "ew", "sw", "rp", "mvo")) {
          next
        }
        # Get hyperparameters_list names
        hyperparameters_list <- config@tuning_strategy@hyper_grid_domain@hyperparameter_list
        hyperparameters_names <- names(hyperparameters_list)

        # Expected hyperparameters for each algorithm
        expected_hyperparameters <- switch(sb_algorithm,
                                           "glmnet" = c("alpha", "lambda.min.ratio"),
                                           "rf" = c("mtry", "num.trees", "max.depth", "min.bucket"),
                                           "xgb" = c("min_child_weight", "max_depth", "subsample", "colsample_bytree", "eta", "alpha", "gamma", "nrounds"),
                                           "nn" = c("regularizer_l1", "regularizer_l2", "droprate", "lr", "size_of_batch", "number_of_epochs"),
                                           "ols" = character(0), # OLS does not require hyperparameters
                                           character(0) # default for unrecognized algorithms
        )

        # If sb_algorithm is not recognized, record an error
        if (length(expected_hyperparameters) == 0) {
          errors <- c(errors, paste0("Unknown sb_algorithm '", sb_algorithm, "' in config ", i, "."))
          next
        }

        #If custom_objective is meta_specific, record an error
        if (stringr::str_remove(stringr::str_remove(config@custom_objective, "min_"), "max_") %in% c("rmse", "rss", "hr", "mb", "cp", "mae", "mape", "mphe", "mpe")){
          errors <- c(errors, paste0("custom_objective can't be set to ", config@custom_objective, " in config ", i, "as it is exclusive for meta-learners."))
        }

        # For algorithms other than 'ols', perform hyperparameter checks
        # Check for missing hyperparameters
        missing_hyperparameters <- setdiff(expected_hyperparameters, hyperparameters_names)
        if (length(missing_hyperparameters) > 0) {
          errors <- c(errors, paste0("In config ", i, ", missing hyperparameters for algorithm '", sb_algorithm, "': ",
                                     paste(missing_hyperparameters, collapse = ", "), "."))
        }

        # Check for unexpected hyperparameters
        extra_hyperparameters <- setdiff(hyperparameters_names, expected_hyperparameters)
        if (length(extra_hyperparameters) > 0) {
          errors <- c(errors, paste0("In config ", i, ", unexpected hyperparameters for algorithm '", sb_algorithm, "': ",
                                     paste(extra_hyperparameters, collapse = ", "), "."))
        }
      }

      # If any errors were collected, return them
      if (length(errors) > 0) {
        return(paste(errors, collapse = "\n"))
      }

    }

    #Base SB Backtest Results Check
    if(!is.null(object@base_sb_backtest_results)){
      if (!all(sapply(object@base_sb_backtest_results, function(x) is(x, "sb_backtest_results")))) {
        stop("All elements in 'base_sb_backtest_results' must be of class 'sb_backtest_results'.")
      }

      if (length(object@base_sb_backtest_results) == 1){
        stop("base_sb_backtest_results should contain more than one sb_backtest_results objects.")
      }

    }

    return(TRUE)
  }
)

#-----------------------------------------------------------------------
# sb_model
#-----------------------------------------------------------------------

#' Define the `sb_model` S4 Class
#'
#' This class represents a (re)fitted sb model. It encapsulates the algorithm used, hyperparameters, custom objective,
#' feature data, target variable, and the fitted model object.
#'
#' @slot sb_algorithm A character string specifying the machine learning algorithm used (e.g., "ols", "glmnet", "rf", "xgb", "nn", "ew", "rp" or "sw").
#' @slot best_hyperparameters The chosen hyperparameters relevant to the specified machine learning algorithm. Applicable only for machine-learning algorithms.
#' @slot model The fitted model object, which varies based on the algorithm used.
#' @slot model_class A character string specifying the class of the model object.
#' @slot eligible_signals A vector of eligible signals used to fit the model.
#' @slot custom_objective A custom objective function used to fit the model.
#' @slot huber_delta A numeric value specifying the delta parameter for the Huber loss function. Applicable only for machine-learning algorithms.
#' @slot keras_architecture_parameters A list of parameters used to define the architecture of a neural network model. Applicable only for the "nn" algorithm.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{refit()}}{Refits the model based on the specified algorithm and hyperparameters.}
#'   \item{\code{predict(new_features)}}{Generates predictions using the fitted model on new feature data.}
#' }
#'
#' @export
setClass(
  "sb_model",
  slots = list(
    model = "ANY",
    eligible_signals = "character",
    model_class = "character",
    sb_algorithm = "character",
    best_hyperparameters = "ANY",
    custom_objective = "ANY",
    huber_delta = "numeric",
    keras_architecture_parameters = "ANY"
  )
)


#-----------------------------------------------------------------------
# sb_backtest_results
#-----------------------------------------------------------------------

#' S4 Class for Time Series Walk-Forward Validation Results of Machine-Learning Models
#'
#' This S4 class encapsulates the results and parameters from performing walk-forward
#' validation on time series data using machine learning algorithms. It includes
#' information about the model, data, tuning process, and performance metrics.
#'
#' @slot oos_sb_outputs_m_df A meta dataframe containing out-of-sample predictions, target and errors, all indexed by testing dates.
#' @slot oos_testing_eval_metrics A list of evaluation metrics for the out-of-sample testing samples.
#' @slot final_model The final refitted machine learning model with best hyperparameters found after tuning. Possibly a object of sb_model S4 class.
#' @slot chosen_eval_metric_validation A list of data.frames with the chosen evaluation metric calculated for the hyperparameter grid.
#' @slot best_hyperparameters_xts A data frame containing the best hyperparameters selected during tuning for each rebalancing period.
#' @slot validation_eval_metrics_hyper_choice All evaluation metrics calculated for the set of best hyperparameters.
#' @slot sb_backtest_workflow A list containing sb_backtest_workflow about the walk-forward validation process.
#'
#'
#' @return An S4 object of class `sb_backtest_results` containing all the specified results and sb_backtest_workflow.
#'
#'
#'@export
setClass(
  "sb_backtest_results",
  slots = list(
    oos_sb_outputs_m_df = "meta_dataframe",
    sb_backtest_config = "ANY",
    oos_testing_eval_metrics_m_xts = "meta_xts",
    consolidated_eval_metrics = "data.frame",
    final_sb_model = "sb_model",
    final_gsm = "ANY",
    chosen_eval_metric_validation = "ANY",
    best_hyperparameters_m_xts = "ANY",
    validation_eval_metrics_hyper_choice_m_xts = "ANY",
    feature_importance_m_df = "meta_dataframe",
    final_feature_importance_m_d_ref = "meta_dataframe",
    ss_backtest_results = "ANY",
    sb_backtest_workflow = "list",
    backtest_identifier = "character"
  ), validity = function(object){

    #Sb backtest config
    if (!is.null(object@sb_backtest_config)) {
      if (!inherits(object@sb_backtest_config, "sb_backtest_config")) {
        return("sb_backtest_config must be a 'sb_backtest_config' object")
      }
    }


    if(!class(object@final_gsm) %in% c("lm", "rpart")){
      stop("final_gsm must be a 'lm' or 'rpart' object")
    }

    if(!is.null(object@ss_backtest_results) && !inherits(object@ss_backtest_results, "ss_backtest_results")){
      stop("ss_backtest_results must be an 'ss_backtest_results' object")
    }

    if(!is.null(object@validation_eval_metrics_hyper_choice_m_xts) && !inherits(object@validation_eval_metrics_hyper_choice_m_xts, "meta_xts")){
      stop("validation_eval_metrics_hyper_choice_xts must be an 'meta_xts' object")
    }

    if(!is.null(object@best_hyperparameters_m_xts) && !inherits(object@best_hyperparameters_m_xts, "meta_xts")){
      stop("best_hyperparameters_xts must be an 'meta_xts' object")
    }

  }
)

#' @title sb_metabacktest_results Class
#' @description An S4 class designed to store and manage a collection of `sb_backtest_results` objects,
#' along with consolidated and time series evaluation metrics for machine learning models.
#'
#' @slot sb_backtest_results A list of `sb_backtest_results` objects.
#' @slot consolidated_oos_testing_metrics A data frame containing consolidated out-of-sample testing evaluation metrics for each algorithm.
#' @slot mean_validation_metrics A data frame containing the mean validation metrics for each algorithm.
#' @slot time_series_oos_testing_metrics A list of data frames for each evaluation metric over time (out-of-sample testing).
#' @slot time_series_validation_metrics A list of data frames for each evaluation metric over time (validation).
#'
#' @export
setClass(
  "sb_metabacktest_results",
  slots = list(
    sb_metabacktest_config = "ANY",
    meta_sb_backtest_results = "sb_backtest_results",
    base_sb_backtest_results_list = "list",
    base_learners_oos_predictions_m_df = "meta_dataframe",
    combined_oos_testing_metrics = "list",
    mean_validation_metrics = "ANY",
    time_series_oos_testing_metrics = "list",
    time_series_validation_metrics = "list",
    backtest_identifier = "character"
  ),
  validity = function(object) {

    if (!is.null(object@sb_metabacktest_config)){
      if (!inherits(object@sb_metabacktest_config, "sb_metabacktest_config")) {
        return("sb_metabacktest_config must be a 'sb_metabacktest_config' object")
      }
    }

    if (!all(sapply(object@base_sb_backtest_results_list, function(x) is(x, "sb_backtest_results")))) {
      return("All elements in 'base_sb_backtest_results_list' must be of class 'sb_backtest_results'.")
    }

    if (!all(sapply(object@combined_oos_testing_metrics, function(x) is(x, "data.frame")))){
      return("All elements in 'combined_oos_testing_metrics' must be of class 'data.frame'.")
    }

    if (!is.null(object@mean_validation_metrics)){
      if (!is(object@mean_validation_metrics, "data.frame")){
        return("mean_validation_metrics must be a 'data.frame' object")
      }
    }

    TRUE
  }
)

#-----------------------------------------------------------------------
# port_backtest_config
#-----------------------------------------------------------------------

#' Class for Port Backtest Config
#'
#' An S4 class specifying parameters for backtesting stock-level portfolios.
#'
#' @slot port_construction_method A character string representing the type of portfolio. Must be one of 'ew', 'sw', 'cw', 'cs', 'rp' or 'mvo'. For signal portfolios,
#' 'cw' and 'cs' are not applicable. For signal portfolios, this is inferred based on sb_algorithm.
#' @slot cov_est_method An object of class `cov_est_method` representing the covariance estimation method and relevant parameters. Current methods are: 'sample', 'ewma', 'cc' (constant correlation),
#' 'pca1', 'pca2', 'shrink_id' (shrinkage to identity matrix), 'shrink_cc' (shrinkage to constant correlation). This is only relevant for 'rp' and 'mvo'.
#' @slot mvo_parameters An object of class `mvo_parameters` representing the parameters for mean-variance optimization. This is only relevant for 'mvo'.
#' @slot rp_parameters An object of class `rp_parameters` representing the parameters for risk parity. This is only relevant for 'rp'.
#' @slot concentration_constraint_policy The policy to handle concentration constraints. This is the only constraint that is applicable to either signal or stock portfolios.
#'  It contains up to to four elements:
#' - `benchmark`: A character vector describing the benchmark to be used to apply constraint.
#' For signal portfolios, possible options are theme_ss or theme_sb.
#' For stock portfolios, there must be a correspondence in `benchmark_weights_m_df`
#' - `max_abs_active_individual_weight`: The maximum absolute individual active weights.
#' - `max_abs_active_group_weight`: The maximum absolute sector/theme active weight used for creating group constraints in `generate_sector_constraints`.
#' If a given sector has no eligible stock, the one with the greatest signal will be automatically promoted. In case of signal portfolios, during ss_backtest, signals with highest alpha_t_values are promoted if
#' enable_theme_representativeness is TRUE.
#' Note that, in the context of stocks, a `benchmark_weights_m_d_ref` data frame must also be supplied.
#' @slot liquidity_constraint_policy The policy to handle liquidity constraints. It is only relevant for stocks. Possible elements are:
#' - `liquidity_floor_rule`: A character indicating the liquidity classification (e.g., micro_caps, small_caps) used to filter stocks. Stocks with less liquidity than specified in `liquidity_floor_rule` will be considered ineligible.
#'   In the case of the `generate_box_constraints` function, `liquidity_constraint_policy` can also contain:
#' - `liquidity_cap_rules`: A named vector with one or many elements used to create upper bounds for weights based on a liquidity classification.
#'   Each element's name and corresponding value represents, respectively:
#'   - `liquidity_classification`: The character classification for the cap.
#'   - `liquidity_cap`: A numeric value indicating the cap (upper bound) for stocks with that liquidity classification.
#'   Many liquidity caps might be created.
#' @slot turnover_constraint_policy The policy to handle turnover constraints. It is only relevant for stocks. Its elements are used to build buffer zones and apply turnover constraints.
#' - It should contain:
#'  - `quantile_range_buffer`: A numeric value indicating the increase of quantile eligibility (both sides) range to be used for the buffer zones.
#'  - `turnover_cap_rules`: A named vector with one or many elements used to create maximum absolute bounds for weights in relation to the old portfolio, based on a liquidity classification.
#'   Each element's name and corresponding value represents, respectively:
#'   - `liquidity_classification`: The character classification for the cap.
#'   - `turnover_cap`: A numeric value indicating the cap (lower and upper bounds) for stocks with that liquidity classification.
#'   Many turnover caps might be created.
#'   Stocks that are less liquid than specified for a buffer zone and have a signal higher than the respective buffer quantile will be considered eligible, even if they do not meet the `liquidity_floor_rule`.
#' @slot liquidity_floor_cutoffs Mandatory if `turnover_constraint_policy` and/or `liquidity_constraint_policy` are provided.
#' A data.frame containing a liquidity_classification column and liquidity metrics that define cutoff values to classify stocks according to liquidity.
#' Each liquidity_classification must be named according to the 5 following liquidity classifications: ("micro_caps", "small_caps", "mid_caps", "large_caps" and "mega_caps)
#' and numeric column indicate the minimum acceptable values (adjusted for inflation) for stocks to have that classification.
#' Classification should be in ascending order (from least liquid to most liquid) for all metrics.
#' If set in decimals, values will be interpreted as quantiles and classification will be set accordingly.
#' Stocks with liquidity lower than micro_caps will receive nano_caps classification.
#' @slot main_liquidity_metric A character string indicating which of the variables in `liquidity_m_df` should be ultimately used.
#' @slot config_name A character string representing the name of the configuration.
#'
#' @export
setClass(
  "port_backtest_config",
  slots = list(
    chosen_score_metric_and_position = "ANY",
    eligibility_quantile_range = "numeric",
    min_eligible_assets_fallback = "ANY",
    selected_benchmark = "ANY",
    initial_buffer_period = "numeric",
    rebalancing_months = "numeric",
    cov_est_method = "cov_est_method",
    port_construction_method = "character",
    mvo_parameters = "ANY",
    rp_parameters = "ANY",
    sb_backtest_config = "ANY",
    sb_backtest_results = "ANY",
    main_liquidity_metric = "character",
    liquidity_floor_cutoffs = "ANY",
    liquidity_constraint_policy = "ANY",
    turnover_constraint_policy = "ANY",
    concentration_constraint_policy = "ANY",
    transaction_costs_parameters = "ANY",
    config_name = "character"
  ),
  validity = function(object){
    #Check if selected benchmark is character if not NULL
    if (!is.null(object@selected_benchmark)){
      if (!is.character(object@selected_benchmark)){
        stop("selected_benchmark must be a character.")
      }
    }

    #Port method
    if (object@port_construction_method == "custom_weights"){
      stop("custom_weights port_construction_method is not supported at this time.")
    }

    if (!object@port_construction_method %in% c("ew", "sw", "cw", "cs", "rp", "mvo")){
      stop("port_construction_method must be one of 'ew', 'sw', 'cw', 'cs', 'rp' or 'mvo'")
    }

    #Check if eligibility_quantile_range has length of 2 between 0 and 1
    if (length(object@eligibility_quantile_range) != 2 | any(object@eligibility_quantile_range < 0) | any(object@eligibility_quantile_range > 1)){
      stop("eligibility_quantile_range must be a numeric vector of length 2 between 0 and 1.")
    }

    #Check if min_eligible_assets_fallback is a integer single numeric value if not null
    if (!is.null(object@min_eligible_assets_fallback)){
      if (!is.numeric(object@min_eligible_assets_fallback) | length(object@min_eligible_assets_fallback) != 1 | !is.numeric(object@min_eligible_assets_fallback) |
          !(object@min_eligible_assets_fallback %% 1 == 0)){
        stop("min_eligible_assets_fallback must be a single integer numeric value.")
      }
    }

    ###Check classes
      ####SB Config
      if (!is.null(object@sb_backtest_config)){
        if(!inherits(object@sb_backtest_config, "sb_backtest_config")){
          stop("sb_backtest_config must be an object of class sb_backtest_config.")
        }
        if(!is.null(object@sb_backtest_results)){
          stop("sb_backtest_results must be NULL if sb_backtest_config is provided.")
        }
      }
      ####SB Results
      if (!is.null(object@sb_backtest_results)){
        if(!inherits(object@sb_backtest_results, "sb_backtest_results")){
          stop("sb_backtest_results must be an object of class sb_backtest_results")
        }
        if(!is.null(object@sb_backtest_config)){
          stop("sb_backtest_config must be NULL if sb_backtest_results is provided.")
        }
      }
      ####MVO
      if (!is.null(object@mvo_parameters)){
        if(!inherits(object@mvo_parameters, "mvo_parameters")){
          stop("mvo_parameters must be an object of class mvo_parameters.")
        }
      }
      ####RP Pars
      if (!is.null(object@rp_parameters)){
        if(!inherits(object@rp_parameters, "rp_parameters")){
          stop("rp_parameters must be an object of class rp_parameters.")
        }
      }

    ###Check if chosen_score_metric_and_position is provided if sb_backtest_results and sb_backtest_config are not provided
    if (is.null(object@sb_backtest_results) & is.null(object@sb_backtest_config) & is.null(object@chosen_score_metric_and_position)){
      stop("chosen_score_metric_and_position must be provided if sb_backtest_results and sb_backtest_config are not provided.")
    }

    ##Check chosen_score_metric_and_position
    if(!is.null(object@chosen_score_metric_and_position) &&
       (length(object@chosen_score_metric_and_position) != 1 || !object@chosen_score_metric_and_position %in% c("long", "short"))){
      stop("chosen_score_metric_and_position must a single named vector with either 'long' or 'short' values")
    }

    ###Check benchmark of cov_est_method
    if(!is.null(object@selected_benchmark)){
      if (object@cov_est_method@cov_matrix_benchmark != object@selected_benchmark){
        stop("cov_est_method benchmark must be the same as selected_benchmark")
      }
    }

    ###Check liquidity_floor_cutoffs
    if (!is.null(object@liquidity_floor_cutoffs)){

      validate_liquidity_floor_cutoffs(object@liquidity_floor_cutoffs, object@main_liquidity_metric)

      ###Check if liquidity_floor_rule is contemplated
      if (!is.null(object@liquidity_constraint_policy@liquidity_floor_rule) &&
          !object@liquidity_constraint_policy@liquidity_floor_rule %in% dplyr::pull(object@liquidity_floor_cutoffs, liquidity_classification)){
        stop("liquidity_floor_rule must be contemplated in liquidity_floor_cutoffs")
      }

    } else {
      if (!is.null(object@liquidity_constraint_policy)){
        stop("liquidity_floor_cutoffs must be provided if liquidity_constraint_policy is provided")
      }
      if (!is.null(object@turnover_constraint_policy)){
        stop("liquidity_floor_cutoffs must be provided if turnover_constraint_policy is provided")
      }
    }

    #Liquidity constraint policy
    if (!is.null(object@liquidity_constraint_policy)){
      #S4 Class
      if(!inherits(object@liquidity_constraint_policy, "liquidity_constraint_policy")){
        stop("liquidity_constraint_policy must be an object of class liquidity_constraint_policy.")
      }

      #Check if liquidity_cap_rules match liquidity_floor_cutoffs
      if (!all(names(object@liquidity_constraint_policy@liquidity_cap_rules) %in% dplyr::pull(object@liquidity_floor_cutoffs, liquidity_classification))){
        stop("liquidity_cap_rules must match liquidity_floor_cutoffs")
      }
    }

    #Concentration constraint policy
    if (!is.null(object@concentration_constraint_policy)){
      #S4 Class
      if(!inherits(object@concentration_constraint_policy, "concentration_constraint_policy")){
        stop("concentration_constraint_policy must be an object of class concentration_constraint_policy.")
      }
      #Check benchmark
      if(object@concentration_constraint_policy@benchmark != object@selected_benchmark){
        stop("concentration_constraint_policy benchmark must be the same as selected_benchmark")
      }
    }

    TRUE
  }
)

#-----------------------------------------------------------------------
# port
#-----------------------------------------------------------------------

#' Portfolio classes for backtesting portfolios
#'
#' These classes encapsulate various parameters and constraints used in backtesting portfolios.
#'
#' @section port-class:
#' The \code{port} class is a base S4 class specifying general parameters for portfolio construction.
#'
#' @slot universe_m_df A meta_dataframe containing the universe of assets.
#' @slot port_construction_method A \code{character} string specifying the method used to construct the portfolio. Must be one of: \code{c("ew", "sw", "cw", "cs", "rp", "mvo")}.
#' @slot eligible_assets A \code{character} vector of eligible assets for the portfolio.
#' @slot exp_ret_score The eligible assets expected return scores. Necessary if \code{port_construction_method \%in\% c("sw","cs","mvo")}.
#' @slot covariance_matrix The eligible assets covariance matrix of returns. Must have rownames identical to colnames and be symmetric.
#' @slot correlation_matrix An object storing the correlation matrix of returns (optional if covariance is provided).
#' @slot weights A \code{numeric} vector of portfolio weights for eligible assets.
#' @slot rel_risk_contr An object representing relative risk contributions (must be provided if \code{covariance_matrix} is not \code{NULL}).
#' @slot mvo_port_spec An object of class \code{portfolio.spec} (from the \pkg{PortfolioAnalytics} or similar package) used for Markowitz optimization.
#' @slot ind_max_weights A numeric vector specifying maximum weight constraints per asset.
#' @slot ind_min_weights A numeric vector specifying minimum weight constraints per asset.
#' @slot random_port_weights An object for storing random portfolio weights (used in MVO).
#' @slot groups An object for grouping assets (e.g., sectors).
#' @slot port_name A \code{character} giving a unique name or label for the configuration.
#'
#' @section signal_port-class:
#' Inherits from \code{port}. Restricts \code{port_construction_method} to one of
#' \code{c("ew","sw","rp","mvo")}. Additionally, it introduces:
#' \describe{
#'   \item{\code{heuristic_sb_metric}}{\code{ANY} object that must be non-\code{NULL}
#'         when \code{port_construction_method} is \code{"sw"} or \code{"mvo"}.}
#' }
#'
#' @section stock_port-class:
#' Inherits from \code{port} and introduces:
#' \describe{
#'   \item{\code{type}}{\code{character} specifying the portfolio subtype. Must
#'   \strong{not} be "signal_blend" or "single_signal" in this class.}
#'   \item{\code{main_liquidity_metric}}{\code{ANY} object specifying a liquidity metric;
#'         must be non-\code{NULL} when \code{port_construction_method} is \code{"cw"} or \code{"cs"}.}
#' }
#'
#' @section sb_stock_port-class:
#' Inherits from \code{stock_port}. Intended for scenarios where \code{sb_algorithm}
#' can be one of \code{"cw"} or \code{"cs"}, with additional liquidity constraints required.
#'
#' @section single_signal_stock_port-class:
#' Inherits from \code{stock_port}. Specialized stock-level portfolio class with
#' \code{sb_algorithm} possibly being \code{"cw"} or \code{"cs"}, requiring the same
#' liquidity-related parameters as \code{sb_stock_port}.
#'
#' @name port-class
#'
NULL

#--------------------
# Base class: port
#--------------------

#' @rdname port-class
#' @export
setClass(
  "port",
  slots = list(
    universe_m_d_ref = "meta_dataframe",
    port_construction_method = "character",
    eligible_assets = "character",
    exp_ret_score = "ANY",
    covariance_matrix = "ANY",
    correlation_matrix = "ANY",
    weights = "numeric",
    rel_risk_contr = "ANY",
    mvo_port_spec = "ANY",
    ind_max_weights = "ANY",
    ind_min_weights = "ANY",
    random_port_weights = "ANY",
    groups = "ANY",
    port_name = "character"
  ),
  validity = function(object) {

    # port_construction_method must be one of the allowed
    if (!object@port_construction_method %in% c("ew","sw","cw","cs","rp","mvo","custom_weights")) {
      stop("port_construction_method must be one of 'ew', 'sw', 'cw', 'cs', 'rp', 'mvo' or 'custom_weights'.")
    }

    #weights and eligible_assets
    if(length(object@weights) != length(object@eligible_assets)){
      stop("weights must have the same length as eligible_assets")
    }

    if(sum(object@weights) - 1 > 0.1){
      stop("weights must sum to 1.")
    }

    #exp_ret_score
    if (object@port_construction_method %in% c("sw", "cs", "mvo")){
      if (is.null(object@exp_ret_score)){
        stop("exp_ret_score must be provided for port_construction_method 'sw', 'cs' or 'mvo'.")
      }
      if(!is.numeric(object@exp_ret_score)){
        stop("exp_ret_score must be numeric.")
      }

      if(length(object@eligible_assets) != length(object@exp_ret_score)){
        stop("exp_ret_score must have the same length as eligible_assets")
      }
    }

    # Check covariance_matrix rownames vs. colnames and symmetry
    if(object@port_construction_method %in% c("mvo", "rp") & is.null(object@covariance_matrix)){
      stop("covariance_matrix must be provided for port_construction_method 'mvo' or 'rp'.")
    }
    if (!is.null(object@covariance_matrix)) {
      if (!identical(rownames(object@covariance_matrix), colnames(object@covariance_matrix))) {
        stop("covariance_matrix must have rownames identical to colnames.")
      }
      if (!isSymmetric(object@covariance_matrix)) {
        stop("covariance_matrix must be symmetric.")
      }
      if (!is.numeric(object@covariance_matrix)){
        stop("covariance_matrix must be numeric.")
      }
      if (is.null(object@rel_risk_contr)){
        stop("rel_risk_contr must be provided when covariance_matrix is not NULL.")
      }

      if(length(object@eligible_assets) != ncol(object@covariance_matrix)){
        stop("covariance_matrix must have the same number of cols as eligible_assets")
      }
      if(!any(colnames(object@covariance_matrix) == object@eligible_assets)){
        stop("covariance_matrix must have the same colnames as eligible_assets")
      }

      if(length(object@eligible_assets) != length(object@rel_risk_contr)){
        stop("rel_risk_contr must have the same length as eligible_assets")
      }
    }

    # Check for mvo
    if(object@port_construction_method == "mvo"){
      if(is.null(object@random_port_weights)){
        stop("random_port_weights must not be NULL for port_construction_method 'mvo'.")
      }

      if(!any(object@random_port_weights$tickers %in% object@eligible_assets)){
        stop("random_port_weights must have the same colnames as eligible_assets")
      }

      if(!is.null(object@ind_max_weights)){
        if(length(object@ind_max_weights) != length(object@eligible_assets)){
          stop("ind_max_weights must have the same length as eligible_assets")
        }
      }

      if(!is.null(object@ind_min_weights)){
        if(length(object@ind_min_weights) != length(object@eligible_assets)){
          stop("ind_min_weights must have the same length as assets")
        }
      }
    }
    TRUE
  }
)

#--------------------
# Subclass: signal_port
#--------------------
#' @rdname port-class
#' @export
setClass(
  "signal_port",
  contains = "port",
  slots = list(
    heuristic_sb_metric = "ANY"
  ),
  validity = function(object) {

    # Restrict port_construction_method
    if (!object@port_construction_method %in% c("ew","sw","rp","mvo","custom_weights")) {
      stop("For signal_port, port_construction_method must be one of 'ew','sw','rp','mvo' or 'custom_weights'.")
    }

    # If sw or mvo => heuristic_sb_metric should not be NULL
    if (object@port_construction_method %in% c("sw","mvo")) {
      if (is.null(object@heuristic_sb_metric)) {
        stop("heuristic_sb_metric cannot be NULL when port_construction_method is 'sw' or 'mvo'.")
      }
    }

    TRUE
  }
)

#---------------------------
# Subclass: stock_port class
#---------------------------

#' @rdname port-class
#' @export
setClass(
  "stock_port",
  contains = "port",
  slots = list(
    type = "character",
    main_liquidity_metric = "ANY"
  ),
  validity = function(object) {

    # If port_construction_method is 'cw' or 'cs', we enforce
    # liquidity-related constraints must not be NULL
    if (object@port_construction_method %in% c("cw","cs")) {
      if (is.null(object@main_liquidity_metric)) {
        stop("main_liquidity_metric cannot be NULL when port_construction_method is 'cw' or 'cs'.")
      }
    }
    if(!object@type %in% c("signal_blend", "single_signal", "custom_weights")){
      stop("type must be one of 'signal_blend', 'single_signal' or 'custom_weights'")
    }

    TRUE
  }
)

#-----------------------------------------------------------------------
# port_backtest_results
#-----------------------------------------------------------------------


#' S4 Class for Portfolio Backtest Results
#'
#' This S4 class encapsulates the results and parameters from running a portfolio backtest based on
#' signals derived from simple stock characteristics or expected returns from machine learning model predictions.
#'
#' @slot port_weights_m_df A meta dataframe containing the portfolio weights across different dates.
#' @slot transactions_log_m_df A meta dataframe containing the transaction logs from portfolio allocations.
#' @slot port_costs_m_xts A meta xts object containing portfolio costs (e.g., direct cost, market impact cost, total cost, turnover) indexed by dates.
#' @slot port_metrics_m_xts A meta xts object containing portfolio performance metrics (if provided) indexed by dates.
#' @slot port_returns_m_xts A meta xts object containing portfolio returns (raw and net returns) indexed by dates.
#' @slot port_backtest_workflow A list detailing the portfolio backtest workflow, including parameters, rebalancing dates, and other metadata.
#' @slot backtest_identifier A character string representing the backtest identifier.
#'
#' @export
setClass(
  "port_backtest_results",
  slots = list(
    port_backtest_config = "ANY",
    port_weights_m_df = "meta_dataframe",
    transactions_log = "transactions_log",
    port_costs_m_xts = "meta_xts",
    port_metrics_m_xts = "ANY",
    port_returns_m_xts = "meta_xts",
    final_stock_port = "stock_port",
    port_construction_method = "character",
    sb_backtest_results = "ANY",
    stock_universe_m_df = "stock_universe_m_df",
    final_stock_universe_m_d_ref = "stock_universe_m_df",
    port_backtest_workflow = "list",
    backtest_identifier = "character"
  ),
  validity = function(object) {

    if (!is.null(object@port_backtest_config)) {
      if (!inherits(object@port_backtest_config, "port_backtest_config")) {
        return("port_backtest_config must be a 'port_backtest_config' object")
      }
    }

    if (!is.null(object@port_metrics_m_xts) && !inherits(object@port_metrics_m_xts, "meta_xts")) {
      return("port_metrics_m_xts must be a 'meta_xts' object or NULL")
    }

     if (!is.null(object@sb_backtest_results) && !inherits(object@sb_backtest_results, "sb_backtest_results")) {
      return("sb_backtest_results must be a 'sb_backtest_results' object")
     }

    if (length(object@backtest_identifier) != 1) {
      return("backtest_identifier must be a single character string")
    }
    TRUE
  }
)


#################################################################


#' S4 Class for Portfolio Backtest Cohort
#'
#' This S4 class encapsulates the merged results of multiple portfolio backtests.
#' It contains the merged portfolio weights (as a meta_dataframe), portfolio costs,
#' portfolio returns, and portfolio metrics (each as lists of meta_xts objects), as well as
#' the common backtest workflow parameters.
#'
#' @slot cohort_name A character string representing the cohort name.
#' @slot port_weights_m_df A meta_dataframe containing merged portfolio weights.
#' @slot port_costs_m_xts_list A list of meta_xts objects for portfolio costs (direct_cost, market_impact_cost, total_cost, turnover).
#' @slot port_returns_m_xts_list A list of meta_xts objects for portfolio returns (raw_return, net_return, raw_active_return, net_active_return).
#' @slot port_metrics_m_xts_list A list of meta_xts objects for portfolio metrics.
#' @slot backtest_workflow_common A list containing the common backtest workflow parameters (used for compatibility).
#'
#' @export
setClass("port_backtest_cohort",
         slots = list(
           cohort_name = "character",
           port_backtest_results_list = "list",
           port_weights_m_df = "meta_dataframe",
           port_costs_m_xts_list = "list",
           port_returns_m_xts_list = "list",
           port_metrics_m_xts_list = "list",
           backtest_workflow_common = "list"
         ),
         validity = function(object) {
           if (length(object@cohort_name) != 1)
             return("cohort_name must be a single character string")
           TRUE
         }
)












#####################################
#########Accesor Methods############
#####################################


##########################################################


# meta_dataframe acessors -------------------------------------------------

#' Accessor Methods for meta_dataframe
#'
#' These methods are used to access components of a `meta_dataframe` object.
#'
#' @param object An object of class `meta_dataframe`.
#' @return The respective slot of the `meta_dataframe` object.
#' @name meta_dataframe_accessors
#' @export
setGeneric("get_data", function(object) standardGeneric("get_data"))

#' @export
setMethod("get_data", "meta_dataframe", function(object) {
  return(object@data)
})

#' @export
setGeneric("get_workflow", function(object) standardGeneric("get_workflow"))

#' @export
setMethod("get_workflow", "meta_dataframe", function(object) {
  return(object@workflow)
})

#' @export
setGeneric("get_tickers", function(object) standardGeneric("get_tickers"))

#' @export
setMethod("get_tickers", "meta_dataframe", function(object) {
  stocks <- unique(object@data$stocks)
  return(stocks)
})

#' @export
setGeneric("get_dates", function(object, ...) standardGeneric("get_dates"))

#' @export
setMethod("get_dates", "meta_dataframe", function(object) {
  dates <- unique(object@data$dates)[order(unique(object@data$dates))]
  return(dates)
})


#' @export
setMethod(
  "as.data.frame", "meta_dataframe", function(x) {
    as.data.frame(x@data)
  }
)
###############################################

# sb_model acessors -------------------------------------------------


#' Accessor Methods for sb_model
#'
#' These methods are used to access components of a `sb_model` object.
#'
#' @param object An object of class `sb_model`.
#' @return The respective slot of the `sb_model` object.
#' @name sb_model_accessors
#' @export
setGeneric("get_sb_algorithm", function(object) standardGeneric("get_sb_algorithm"))

#' @export
setMethod("get_sb_algorithm", "sb_model", function(object) {
  return(object@sb_algorithm)
})

#' @export
setGeneric("get_best_hyperparameters", function(object) standardGeneric("get_best_hyperparameters"))

#' @export
setMethod("get_best_hyperparameters", "sb_model", function(object) {
  return(object@best_hyperparameters)
})

#' @export
setGeneric("get_model", function(object) standardGeneric("get_model"))

#' @export
setMethod("get_model", "sb_model", function(object) {
  return(object@model)
})

#############################################

# sb_backtest_results and ss_backtest_results acessors --------------------------------------------

#' Accessor Methods for sb_backtest_results
#'
#' These methods are used to access various components of an `sb_backtest_results` object.
#'
#' @param object An object of class `sb_backtest_results`.
#' @return The respective slot of the `sb_backtest_results` object.
#' @name sb_backtest_results_accessors
#' @export
setGeneric("get_oos_prediction_list", function(object) standardGeneric("get_oos_prediction_list"))

#' @export
setMethod("get_oos_prediction_list", "sb_backtest_results", function(object) {
  return(object@oos_prediction_list)
})

#' @export
setGeneric("get_oos_error_list", function(object) standardGeneric("get_oos_error_list"))

#' @export
setMethod("get_oos_error_list", "sb_backtest_results", function(object) {
  return(object@oos_error_list)
})

#' @export
setGeneric("get_oos_y_list", function(object) standardGeneric("get_oos_y_list"))

#' @export
setMethod("get_oos_y_list", "sb_backtest_results", function(object) {
  return(object@oos_y_list)
})

#' @export
setGeneric("get_oos_testing_eval_metrics", function(object) standardGeneric("get_oos_testing_eval_metrics"))

#' @export
setMethod("get_oos_testing_eval_metrics", "sb_backtest_results", function(object) {
  return(object@oos_testing_eval_metrics)
})

#' @export
setGeneric("get_final_model", function(object) standardGeneric("get_final_model"))

#' @export
setMethod("get_final_model", "sb_backtest_results", function(object) {
  return(object@final_model)
})

#' @export
setMethod("get_tickers", "sb_backtest_results", function(object) {
  return(object@sb_backtest_workflow$tickers)
})


#' @export
setMethod("get_dates", "sb_backtest_results", function(object, type = "complete") {

  if(!type %in% c("complete", "testing", "rebalance")) stop("sample_type must be one of `complete`, `testing` or `rebalance`")

  if(type == "complete") return(object@sb_backtest_workflow$dates_covered)
  if(type == "testing") return(object@sb_backtest_workflow$dates_testing_sample)
  if(type == "rebalance") return(object@sb_backtest_workflow$rebalance_dates)

})

#' @export
setGeneric("get_chosen_eval_metric_validation", function(object) standardGeneric("get_chosen_eval_metric_validation"))

#' @export
setMethod("get_chosen_eval_metric_validation", "sb_backtest_results", function(object) {
  return(object@chosen_eval_metric_validation)
})

#' @export
setMethod("get_best_hyperparameters", "sb_backtest_results", function(object) {
  return(object@best_hyperparameters_xts)
})

#' @export
setGeneric("get_validation_eval_metrics_hyper_choice", function(object) standardGeneric("get_validation_eval_metrics_hyper_choice"))

#' @export
setMethod("get_validation_eval_metrics_hyper_choice", "sb_backtest_results", function(object) {
  return(object@validation_eval_metrics_hyper_choice)
})

#' @export
setMethod("get_workflow", "sb_backtest_results", function(object) {
  return(object@sb_backtest_workflow)
})

#' @export
setMethod("as.list", "sb_backtest_results", function(x) {
  # Get the names of all slots
  slot_names <- slotNames(x)

  # Create a list to hold the extracted slots, ignoring NULL slots
  slot_list <- lapply(slot_names, function(slot) {
    value <- slot(x, slot)  # Extract the slot using the slot name
    if (!is.null(value)) {
      return(value)  # Return the value only if it's not NULL
    }
    return(NULL)  # Return NULL if the slot is NULL
  })

  # Filter out NULL entries
  non_null_indices <- which(!sapply(slot_list, is.null))
  slot_list <- slot_list[non_null_indices]

  # Set names for the list elements based on non-NULL slots
  names(slot_list) <- slot_names[non_null_indices]

  return(slot_list)
})

#' Accessor Methods for ss_backtest_results
#'
#' These methods are used to access various components of an `ss_backtest_results` object.
#'
#' @param object An object of class `ss_backtest_results`.
#' @return The respective slot of the `ss_backtest_results` object.
#' @name ss_backtest_results_accessors
#' @export
setMethod("get_final_model", "ss_backtest_results", function(object) {
  models_list <- list(frequentist = object@frequentist_results,
                      bayesian = object@bayesian_results)
  return(models_list)
})


#' @export
setGeneric("get_eligible_signals", function(object){
  standardGeneric("get_eligible_signals")
})

#' @export
setMethod("get_eligible_signals", "ss_backtest_results", function(object){
  eligible_signals_df <- object@signal_universe_m_df %>% dplyr::filter(is_eligible == 1) %>%
    dplyr::group_by(dates) %>% dplyr::summarise(ticker_list = list(tickers), .groups = "drop")

  eligible_signals_list <- stats::setNames(eligible_signals_df$ticker_list, as.character(eligible_signals_df$dates))

  return(eligible_signals_list)
})

#' @export
setGeneric("get_signal_universe", function(object){
  standardGeneric("get_signal_universe")
})

#' @export
setMethod("get_signal_universe", "ss_backtest_results", function(object){
  return(object@signal_universe_m_df)
})


#' @export
setGeneric("get_bayesian_fit", function(object){
  standardGeneric("get_bayesian_fit")
})

#' @export
setMethod("get_bayesian_fit", "ss_backtest_results", function(object){
  if(object@p_correction_method == "bayesian"){
    return(object@bayesian_results)
  } else {
    stop("bayesian_results not available for non-bayesian models.")
  }
})

#' @export
setMethod("get_dates", "ss_backtest_results", function(object, type = "complete") {

  if(!type %in% c("complete", "backtest", "rebalance")) stop("sample_type must be one of `complete`, `backtest` or `rebalance`")

  if(type == "complete") return(object@sb_backtest_workflow$dates_covered)
  if(type == "backtest") return(object@sb_backtest_workflow$dates_backtest)
  if(type == "rebalance") return(object@sb_backtest_workflow$rebalance_dates)

})

#' @export
setGeneric("get_selected_market_factor_proxy", function(object){
  standardGeneric("get_selected_market_factor_proxy")
})

#' @export
setMethod("get_selected_market_factor_proxy", "ss_backtest_results", function(object) {
  return(object@selected_market_factor_proxy_xts)
})

#' @export
setMethod("get_workflow", "ss_backtest_results", function(object) {
  return(object@ss_backtest_workflow)
})



#' @export
setMethod("as.list", "ss_backtest_results", function(x) {
  # Get the names of all slots
  slot_names <- slotNames(x)

  # Create a list to hold the extracted slots, ignoring NULL slots
  slot_list <- lapply(slot_names, function(slot) {
    value <- slot(x, slot)  # Extract the slot using the slot name
    if (!is.null(value)) {
      return(value)  # Return the value only if it's not NULL
    }
    return(NULL)  # Return NULL if the slot is NULL
  })

  # Filter out NULL entries
  non_null_indices <- which(!sapply(slot_list, is.null))
  slot_list <- slot_list[non_null_indices]

  # Set names for the list elements based on non-NULL slots
  names(slot_list) <- slot_names[non_null_indices]

  return(slot_list)
})

##########################

# configs acessors -------------------------------------------------

# get sb_backtest_config
#' @title Get SB Backtest Config Object
#' @description Accessor function to retrieve the sb_backtest_config object from a sb_metabacktest_config object or a sb_backtest_results object
#'
#' @param object A `sb_metabacktest_config` or a `sb_backtest_results` object.
#'
#' @return The `sb_backtest_configs` slot of the `sb_metabacktest_config` object or a `sb_backtest_config` derived from a `sb_backtest_results` object.
#' @export
setGeneric("get_sb_backtest_config", function(object) standardGeneric("get_sb_backtest_config"))

#' @rdname get_sb_backtest_config
#' @export
setMethod("get_sb_backtest_config", "sb_metabacktest_config", function(object) {
  return(object@sb_backtest_configs)
})

#' @rdname get_sb_backtest_config
#' @export
setMethod("get_sb_backtest_config", "sb_backtest_results", function(object) {

  sb_backtest_workflow <- object@sb_backtest_workflow

  #Fabricate tuning strategy
  tuning_strategy <- get_tuning_strategy(object)

  #Create Backtest Config
  sb_backtest_config <- create_sb_backtest_config(
    sb_algorithm = sb_backtest_workflow$sb_algorithm,
    target_fwd_name = sb_backtest_workflow$target_fwd_name,
    training_sample_size = sb_backtest_workflow$training_sample_size,
    rebalancing_months = sb_backtest_workflow$rebalancing_months,
    split_method = sb_backtest_workflow$split_method,
    tuning_strategy = tuning_strategy,
    custom_objective = sb_backtest_workflow$custom_objective,
    quantile_tau = sb_backtest_workflow$quantile_tau,
    huber_delta = sb_backtest_workflow$huber_delta
  )

  #Add keras_architecture_parameters if ml algo is nn
  if(sb_backtest_workflow$sb_algorithm == "nn") {
    keras_architecture_parameters <- sb_backtest_workflow$keras_architecture_parameters
    sb_backtest_config <- add_keras_architecture(sb_backtest_config,
                                                 nn_optimizer = keras_architecture_parameters$nn_optimizer,
                                                 units = keras_architecture_parameters$units,
                                                 activation = keras_architecture_parameters$activation,
                                                 batch_norm_option = keras_architecture_parameters$batch_norm_option
    )
  }

  return(sb_backtest_config)

})

# get ss_backtest_config
#' @title Get SS Backtest Config Object
#' @description Accessor function to retrieve the ss_backtest_config object from a ss_backtest_results object or a sb_backtest_results object
#'
#' @param object A `ss_backtest_results` or a `sb_backtest_results` object.
#'
#' @return A `ss_backtest_config` derived from a `sb_backtest_config` `ss_backtest_results` or object.
#' @export
setGeneric("get_ss_backtest_config", function(object) standardGeneric("get_ss_backtest_config"))

#' @rdname get_ss_backtest_config
#' @export
setMethod("get_ss_backtest_config", "sb_backtest_config", function(object) {
  return(object@ss_backtest_config)
})

##########################

# get tuning strategy -----------------------------------------------------

#' @title Get Hyperparameter Tuning Strategy
#' @description Accessor function to retrieve the hyperparameter tuning strategy from an sb_backtest_config object.
#'
#' @param object An sb_backtest_config object.
#'
#' @return The `tuning_strategy` slot of the `sb_backtest_config` object.
#' @export
setGeneric("get_tuning_strategy", function(object) {
  standardGeneric("get_tuning_strategy")
})

#' @rdname get_tuning_strategy
#' @export
setMethod("get_tuning_strategy", "sb_backtest_config", function(object) {
  return(object@tuning_strategy)
})


#' @rdname get_tuning_strategy
#' @export
setMethod("get_tuning_strategy", "sb_metabacktest_config", function(object) {
  return(lapply(object@sb_backtest_configs, function(config) get_tuning_strategy(config)))
})

#' @rdname get_tuning_strategy
#' @export
setMethod("get_tuning_strategy", "sb_backtest_results", function(object){

  #WF
  sb_backtest_workflow <- object@sb_backtest_workflow

  if(sb_backtest_workflow$sb_algorithm %in% c("ols", "ew", "sw", "rp", "mvo")) return(NULL)

  #Hyper Grid Domain
  hyper_grid_domain <- get_hyper_grid_domain(object)

  if(!sb_backtest_workflow$sb_algorithm %in% c("ols", "ew", "sw", "rp", "mvo")){
    tuning_strategy <- create_tuning_strategy(tuning_method = sb_backtest_workflow$tuning_method,
                                              validation_sample_size = sb_backtest_workflow$validation_sample_size,
                                              chosen_eval_metric = sb_backtest_workflow$chosen_eval_metric,
                                              hyper_grid_domain = hyper_grid_domain,
                                              early_stop = sb_backtest_workflow$early_stop,
                                              n_iter =  if(sb_backtest_workflow$tuning_method != "grid_search") sb_backtest_workflow$n_iter else NULL,
                                              acq = if(sb_backtest_workflow$tuning_method == "bayesian_opt") sb_backtest_workflow$acq else NULL,
                                              init_points = if(sb_backtest_workflow$tuning_method == "bayesian_opt") sb_backtest_workflow$init_points else NULL,
                                              k_iter = if(sb_backtest_workflow$tuning_method == "bayesian_opt") sb_backtest_workflow$k_iter else NULL
    )
  }

  return(tuning_strategy)

})

###########################

# get hyper grid domain -----------------------------------------------------

#' @title Get Hyperparameter Grid Domain
#' @description Accessor function to retrieve the hyperparameter grid domain.
#'
#' @param object An sb_backtest_config or tuning_strategy object
#'
#' @return The `hyper_grid_domain` object stored in the `tuning_strategy`.
setGeneric("get_hyper_grid_domain", function(object) {
  standardGeneric("get_hyper_grid_domain")
})

#' @rdname get_hyper_grid_domain
setMethod("get_hyper_grid_domain", "sb_backtest_config", function(object) {
  if(is.null(object@tuning_strategy)){
    stop("tuning_strategy not avaiable.")
  } else {
    return(object@tuning_strategy@hyper_grid_domain)
  }
})

setMethod("get_hyper_grid_domain", "tuning_strategy", function(object) {
  return(object@hyper_grid_domain)
})

#' @rdname get_hyper_grid_domain
#' @export
setMethod("get_hyper_grid_domain", "sb_metabacktest_config", function(object) {
  return(lapply(object@sb_backtest_configs, function(config) {
    if(is.null(config@tuning_strategy)) {
      stop("tuning_strategy not available for one of the sb_backtest_config objects.")
    } else {
      return(config@tuning_strategy@hyper_grid_domain@hyperparameter_list)
    }
  }))
})

#' @rdname get_hyper_grid_domain
#' @export
setMethod("get_hyper_grid_domain", "sb_backtest_results", function(object){

  hyper_grid_domain <- new("hyper_grid_domain", hyperparameter_list = object@sb_backtest_workflow$hyper_grid_domain_list)
  return(hyper_grid_domain)
})



################################

# get keras architecture -----------------------------------------------------

#' @title Get Keras Architecture Parameters
#' @description Accessor function to retrieve the keras architecture parameters.
#'
#' @param object A sb_backtest_config, a sb_metabacktest_config or a sb_backtest_results object.
#'
#' @return A `keras_architecture_parameters` S4 class.
setGeneric("get_keras_architecture_parameters", function(object) standardGeneric("get_keras_architecture_parameters"))

#' @rdname get_keras_architecture_parameters
setMethod("get_keras_architecture_parameters", "sb_backtest_config", function(object) {

  if(object@sb_algorithm != "nn"){
    stop("keras_architecture_parameters not available for non-neural network algorithms.")
  }

  if(is.null(object@keras_architecture_parameters)){
    stop("keras_architecture_parameters not available.")
  } else {
    return(object@keras_architecture_parameters)
  }
})

#' @rdname get_keras_architecture_parameters
setMethod("get_keras_architecture_parameters", "sb_metabacktest_config", function(object) {
  return(lapply(object@sb_metabacktest_configs[sapply(object@sb_metabacktest_configs, function(config) config@sb_algorithm == "nn")],
                function(nn_config) get_keras_architeture_parameters(nn_config)
  ))
})

#' @rdname get_keras_architecture_parameters
setMethod("get_keras_architecture_parameters", "sb_backtest_results", function(object) {

  if(object@sb_backtest_workflow$sb_algorithm != "nn"){
    stop("keras_architecture_parameters not available for non-neural network algorithms.")
  } else {

    keras_architecture_parameters <- create_keras_architecture(
      nn_optimizer = object@sb_backtest_workflow$keras_architecture_parameters$nn_optimizer,
      units = object@sb_backtest_workflow$keras_architecture_parameters$units,
      activation = object@sb_backtest_workflow$keras_architecture_parameters$activation,
      batch_norm_option = object@sb_backtest_workflow$keras_architecture_parameters$batch_norm_option
    )

  }

  return(keras_architecture_parameters)

})

#' @rdname get_keras_architecture_parameters
setMethod("get_keras_architecture_parameters", "sb_model", function(object) {

  if(object@sb_algorithm != "nn"){
    stop("keras_architecture_parameters not available for non-neural network algorithms.")
  } else {
    keras_architecture_parameters <- create_keras_architecture(
      nn_optimizer = object@keras_architecture_parameters$nn_optimizer,
      units = object@keras_architecture_parameters$units,
      activation = object@keras_architecture_parameters$activation,
      batch_norm_option = object@keras_architecture_parameters$batch_norm_option
    )
  }

  return(keras_architecture_parameters)

})


#' @title Convert Keras Architecture Parameters to List
#' @description Converts a `keras_architecture_parameters` object to a list.
#'
#' This method extracts the relevant attributes from a `keras_architecture_parameters`
#' object and returns them as a list, making it easier to work with the parameters
#' in a more general R context.
#'
#' @param x A `keras_architecture_parameters` object that contains the architecture parameters
#'          for a Keras model.
#'
#' @return A list containing the following elements:
#' \item{units}{The number of units in the layer.}
#' \item{n_layers}{The number of layers in the architecture.}
#' \item{activation}{The activation function used in the architecture.}
#' \item{nn_optimizer}{The optimizer used for training the neural network.}
#' \item{batch_norm_option}{Indicates if batch normalization is applied.}
#'
#' @export
setMethod("as.list", "keras_architecture_parameters", function(x) {
  list(
    units = x@units,
    n_layers = x@n_layers,
    activation = x@activation,
    nn_optimizer = x@nn_optimizer,
    batch_norm_option = x@batch_norm_option
  )
})

################################

# get alpha_test_strategy -----------------------------------------------------

#' @title Get Alpha Test Strategy
#' @description Accessor function to retrieve the alpha test strategy from `ss_backtest_config`, `ss_backtest_results`   object.
#'
#' @param object An ss_backtest_config object.
#'
#' @return The `alpha_test_strategy` slot of the `ss_backtest_config` object.
#' @export
setGeneric("get_alpha_test_strategy", function(object) {
  standardGeneric("get_alpha_test_strategy")
})

#' @rdname get_alpha_test_strategy
#' @export
setMethod("get_alpha_test_strategy", "ss_backtest_config", function(object){
  alpha_test_strategy <- object@alpha_test_strategy

  if(!is.null(alpha_test_strategy)){
    return(alpha_test_strategy)
  } else {
    stop("alpha_test_strategy not available.")
  }

})

#' @rdname get_alpha_test_strategy
#' @export
setMethod("get_alpha_test_strategy", "ss_backtest_results", function(object){

  #WF
  ss_backtest_workflow <- object@ss_backtest_workflow

  #Fabricate bayesian results
  if(ss_backtest_workflow$p_correction_method == "bayesian"){
    bayesian_model_parameters <- new("bayesian_model_parameters",
                                     user_priors = ss_backtest_workflow$user_priors,
                                     prior_derivation_control = ss_backtest_workflow$prior_derivation_control,
                                     brms_control = ss_backtest_workflow$brms_control
    )
  } else {
    bayesian_model_parameters <- NULL
  }

  alpha_test_strategy <- create_alpha_test_strategy(
    model_structure = ss_backtest_workflow$model_structure,
    theme_level_intercept = ss_backtest_workflow$theme_level_intercept,
    theme_level_slope = ss_backtest_workflow$theme_level_slope,
    signal_significance_threshold = ss_backtest_workflow$signal_significance_threshold,
    p_correction_method = ss_backtest_workflow$p_correction_method,
    market_factor_proxy = ss_backtest_workflow$market_factor_proxy,
    bayesian_model_parameters = bayesian_model_parameters,
    enable_theme_representativeness = ss_backtest_workflow$enable_theme_representativeness,
    lmer_control = ss_backtest_workflow$lmer_control
  )


  return(alpha_test_strategy)

})

#' @rdname get_alpha_test_strategy
#' @export
setMethod("get_alpha_test_strategy", "sb_backtest_config", function(object) {

  ss_object <- if(!is.null(object@ss_backtest_config)) object@ss_backtest_config else object@ss_backtest_results
  alpha_test_strategy <- get_alpha_test_strategy(ss_object)

  return(alpha_test_strategy)
})



###########################

# get priors -----------------------------------------------------
#' @title Get brms priors
#' @description Accessor function to retrieve brms priors.
#'
#' @param object A `ss_backtest_config` or a `ss_backtest_results` object.
#'
#' @return A `brmsprior` S4 class.
setGeneric("get_brms_prior", function(object){
  standardGeneric("get_brms_prior")
})

#' @export
setMethod("get_brms_prior", "ss_backtest_results", function(object){
  if(!object@p_correction_method == "bayesian"){
    stop("brms prior not available for non-bayesian models.")
  }
  return(object@bayesian_results$elected_priors)

})

#' @export
setMethod("get_brms_prior", "ss_backtest_config", function(object){

  alpha_test_strategy <- object@alpha_test_strategy

  if(!is.null(alpha_test_strategy) && !is.null(alpha_test_strategy@bayesian_model_parameters) && !is.null(alpha_test_strategy@bayesian_model_parameters@user_priors)){
    return(alpha_test_strategy@bayesian_model_parameters@user_priors)
  } else {
    stop("brms prior not available.")
  }

})
###########################

# get bayesian_model_params -----------------------------------------------------
#' @title Get Bayesian Model Parameters
#' @description Extracts the \code{bayesian_model_parameters} from an object (e.g. a
#'   \code{bayesian_alpha_test_strategy} or an \code{ss_backtest_config} holding a Bayesian strategy).
#'
#' @param object An S4 object, typically \code{bayesian_alpha_test_strategy} or \code{ss_backtest_config}.
#'
#' @return An object of class \code{bayesian_model_parameters}.
#' @export
setGeneric("get_bayesian_model_parameters", function(object) {
  standardGeneric("get_bayesian_model_parameters")
})

#' @describeIn get_bayesian_model_parameters
#'   Extract parameters from a \code{bayesian_alpha_test_strategy}.
#' @export
setMethod("get_bayesian_model_parameters", "bayesian_alpha_test_strategy",
          function(object) {
            return(object@bayesian_model_parameters)
          }
)

#' @describeIn get_bayesian_model_parameters
#'   Extract parameters from an \code{ss_backtest_config}, if it has a Bayesian alpha test strategy.
#' @export
setMethod("get_bayesian_model_parameters", "ss_backtest_config",
          function(object) {
            alpha_strat <- object@alpha_test_strategy

            # Check if we actually have a bayesian_alpha_test_strategy
            if (!is(alpha_strat, "bayesian_alpha_test_strategy")) {
              # Option 1: Return NULL
              # return(NULL)

              # Option 2: Throw an error
              stop("This ss_backtest_config does not hold a bayesian_alpha_test_strategy.")
            }

            return(alpha_strat@bayesian_model_parameters)
          }
)



###########################

# concentration_constraint policy  -----------------------------------------------------
#' @title Get the Concentration Constraint Policy
#' @description Accessor method to extract the `concentration_constraint_policy` from an object.
#'
#' @param object An object of class \code{port_backtest_config} or \code{sb_backtest_config}.
#'
#' @return An S4 object of class \code{concentration_constraint_policy}.
#' @export
setGeneric("get_concentration_constraint_policy", function(object) {
  standardGeneric("get_concentration_constraint_policy")
})

#' @describeIn get_concentration_constraint_policy
#'   Extract the concentration policy from \code{port_backtest_config}.
#' @export
setMethod("get_concentration_constraint_policy",
          signature(object = "port_backtest_config"),
          function(object) {
            return(object@concentration_constraint_policy)
          }
)

#' @describeIn get_concentration_constraint_policy
#'   Extract the concentration policy from \code{sb_backtest_config},
#'   which stores it inside \code{object@signal_port_parameters}.
#' @export
setMethod("get_concentration_constraint_policy",
          signature(object = "sb_backtest_config"),
          function(object) {
            return(object@signal_port_parameters@concentration_constraint_policy)
          }
)

#' Turn a concentration_constraint_policy into a list
#'
#' @param x An S4 object of class \code{concentration_constraint_policy}.
#'
#' @return A named list containing:
#'   \itemize{
#'     \item \code{benchmark}
#'     \item \code{max_abs_active_individual_weight}
#'     \item \code{max_abs_active_group_weight}
#'   }
#'
#' @export
setMethod("as.list", "concentration_constraint_policy", function(x) {
  list(
    benchmark = x@benchmark,
    max_abs_active_individual_weight = x@max_abs_active_individual_weight,
    max_abs_active_group_weight = x@max_abs_active_group_weight
  )
})

###########################

# liquidity_constraint_policy -----------------------------------------------------
#' @title Accessor for Liquidity Constraint Policy
#' @description Retrieves the liquidity constraint policy from a `port_backtest_config` object.
#' @param port_backtest_config_obj A `port_backtest_config` object.
#' @return The liquidity constraint policy list.
#' @export
setGeneric("get_liquidity_constraint_policy", function(port_backtest_config_obj) {
  standardGeneric("get_liquidity_constraint_policy")
})

#' @export
setMethod("get_liquidity_constraint_policy", "port_backtest_config", function(port_backtest_config_obj) {
  return(port_backtest_config_obj@liquidity_constraint_policy)
})

#' as.list Method for liquidity_constraint_policy S4 Class
#'
#' Converts a liquidity_constraint_policy S4 object to a list with elements
#' \code{liquidity_floor_rule} and \code{liquidity_cap_rules}.
#'
#' @param x A liquidity_constraint_policy S4 object.
#' @param ... Additional arguments (unused).
#'
#' @return A list with elements \code{liquidity_floor_rule} and \code{liquidity_cap_rules}.
#'
#' @export
setMethod(
  f = "as.list",
  signature = "liquidity_constraint_policy",
  definition = function(x, ...) {
    list(
      liquidity_floor_rule = x@liquidity_floor_rule,
      liquidity_cap_rules = x@liquidity_cap_rules
    )
  }
)


# turnover_constraint_policy -----------------------------------------------------
#' @title Accessor for Turnover Constraint Policy
#' @description Retrieves the turnover constraint policy from a `port_backtest_config` object.
#' @param port_backtest_config_obj A `port_backtest_config` object.
#' @return The turnover constraint policy list.
#' @export
setGeneric("get_turnover_constraint_policy", function(port_backtest_config_obj) {
  standardGeneric("get_turnover_constraint_policy")
})

#' @export
setMethod("get_turnover_constraint_policy", "port_backtest_config", function(port_backtest_config_obj) {
  return(port_backtest_config_obj@turnover_constraint_policy)
})

#' as.list Method for turnover_constraint_policy S4 Class
#'
#' Converts a turnover_constraint_policy S4 object to a list with elements
#' \code{quantile_range_buffer} and \code{turnover_cap_rules}.
#'
#' @param x A turnover_constraint_policy S4 object.
#' @param ... Additional arguments (unused).
#'
#' @return A list with elements \code{quantile_range_buffer} and \code{turnover_cap_rules}.
#'
#' @export
setMethod("as.list", "turnover_constraint_policy", function(x, ...) {
  list(
    quantile_range_buffer = x@quantile_range_buffer,
    turnover_cap_rules = x@turnover_cap_rules
  )
})


# transaction_costs_parameters -----------------------------------------------------
#' @title Accessor for Transaction Cost Parameters
#' @description Retrieves the transaction_costs_parameters from a `port_backtest_config` object.
#' @param port_backtest_config_obj A `port_backtest_config` object.
#' @return The turnover constraint policy list.
#' @export
setGeneric("get_transaction_costs_parameters", function(port_backtest_config_obj) {
  standardGeneric("get_transaction_costs_parameters")
})

#' @export
setMethod("get_transaction_costs_parameters", "port_backtest_config", function(port_backtest_config_obj) {
  return(port_backtest_config_obj@transaction_costs_parameters)
})

#' as.list Method for transaction_costs_parameters S4 Class
#'
#' Converts a transaction_costs_parameters S4 object to a list with elements
#' \code{direct_transaction_cost}, \code{alpha}, \code{lambda} and \code{strategy_aum}.
#'
#' @param x A transaction_costs_parameters S4 object.
#' @param ... Additional arguments (unused).
#'
#' @return A list with elements \code{quantile_range_buffer} and \code{turnover_cap_rules}.
#'
#' @export
setMethod("as.list", "transaction_costs_parameters", function(x, ...) {
  list(
    direct_transaction_cost = x@direct_transaction_cost,
    strategy_aum = x@strategy_aum,
    alpha = x@alpha,
    lambda = x@lambda
  )
})



# liquidity_floor_cutoffs -----------------------------------------------------
#' @title Accessor for Liquidity Floor Cutoffs
#' @description Retrieves the liquidity floor cutoffs from a `port_backtest_config` object.
#' @param port_backtest_config_obj A `port_backtest_config` object.
#' @return The liquidity floor cutoffs list.
#' @export
setGeneric("get_liquidity_floor_cutoffs", function(port_backtest_config_obj) {
  standardGeneric("get_liquidity_floor_cutoffs")
})

#' @export
setMethod("get_liquidity_floor_cutoffs", "port_backtest_config", function(port_backtest_config_obj) {
  return(port_backtest_config_obj@liquidity_floor_cutoffs)
})





###########################










