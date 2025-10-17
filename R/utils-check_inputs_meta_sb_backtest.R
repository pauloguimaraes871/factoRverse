#' Validate Meta Signal Selection and Data Inputs
#'
#' This function performs a series of validation checks on the meta-level
#' signal selection, features passthrough, meta dataframes, and xts objects.
#'
#' @param config An object containing the meta backtest configuration.
#' @param features_m_df A dataframe containing features
#' @param target_m_df A dataframe containing targets
#' @param base_sb_backtest_results_list A list of base signal blend backtest results.
#' @param base_signal_themes_m_df Optional base signal themes meta dataframe.
#' @param meta_signal_themes_m_df Optional meta signal themes meta dataframe.
#' @param base_custom_signal_weights_m_df Optional base custom signal weights meta dataframe.
#' @param meta_custom_signal_weights_m_df Optional meta custom signal weights meta dataframe.
#' @param base_custom_signal_universe_metrics_m_df Optional base custom signal universe metrics meta dataframe.
#' @param meta_custom_signal_universe_metrics_m_df Optional meta custom signal universe metrics meta dataframe.
#' @param base_backtest_returns_m_xts Optional xts object for base backtest returns.
#' @param base_benchmark_returns_m_xts Optional xts object for base benchmark returns.
#' @param meta_backtest_returns_m_xts Optional xts object for meta backtest returns.
#' @param meta_benchmark_returns_m_xts Optional xts object for meta benchmark returns.
#' @param verbose A boolean indicating whether to print detailed messages.
#'
#' @return None. Stops execution if validation checks fail.
check_inputs_meta_sb_backtest <- function(
    config, features_m_df, target_m_df,
    base_sb_backtest_results_list,
    base_signal_themes_m_df, base_custom_signal_weights_m_df, base_custom_signal_universe_metrics_m_df,
    meta_signal_themes_m_df, meta_custom_signal_weights_m_df, meta_custom_signal_universe_metrics_m_df,
    base_backtest_returns_m_xts, base_benchmark_returns_m_xts,
    meta_backtest_returns_m_xts, meta_benchmark_returns_m_xts,
    verbose
) {


  #Objects Structure
  ##########################
  ##Amount of base results
  if (!is.null(base_sb_backtest_results_list) && length(base_sb_backtest_results_list) == 1){
    stop("More than one base_sb_backtest_results_list must be supplied.")
  }
  ##Class
  if (!all(sapply(base_sb_backtest_results_list, function(x) inherits(x, "sb_backtest_results")))) {
    stop("All elements in 'base_sb_backtest_results_list' must be of class 'sb_backtest_results'.")
  }
  ##Check for meta_dataframe objects
  meta_dfs <- list(
    base_signal_themes_m_df,
    base_custom_signal_weights_m_df,
    base_custom_signal_universe_metrics_m_df,
    meta_signal_themes_m_df,
    meta_custom_signal_weights_m_df,
    meta_custom_signal_universe_metrics_m_df
  )
  names(meta_dfs) <- c("base_signal_themes_m_df", "base_custom_signal_weights_m_df", "base_custom_signal_universe_metrics_m_df",
                       "meta_signal_themes_m_df", "meta_custom_signal_weights_m_df", "meta_custom_signal_universe_metrics_m_df")
  for (df_name in names(meta_dfs)) {
    if (!is.null(meta_dfs[[df_name]]) && !is_meta_dataframe(meta_dfs[[df_name]])) {
      stop(paste("If provided,", df_name, "must be a meta_dataframe object."))
    }
  }

  ##Check for xts objects
  xts_objects <- list(
    base_backtest_returns_m_xts,
    base_benchmark_returns_m_xts,
    meta_backtest_returns_m_xts,
    meta_benchmark_returns_m_xts
  )
  names(xts_objects) <- c("base_backtest_returns_m_xts", "base_benchmark_returns_m_xts",
                          "meta_backtest_returns_m_xts", "meta_benchmark_returns_m_xts")
  for (xts_name in names(xts_objects)) {
    if (!is.null(xts_objects[[xts_name]]) && !inherits(xts_objects[[xts_name]], "meta_xts")) {
      stop(paste("If provided,", xts_name, "must be a meta_xts object."))
    }
  }
  ##########################


  #Features Passthrough
  ##########################
  # Check for features_passthrough presence
  if (!(length(config@features_passthrough) == 1 && config@features_passthrough %in% c("all", "none"))) {
    if (!all(config@features_passthrough %in% colnames(features_m_df@data))) {
      stop("features_passthrough should be contained in features_m_df.")
    }
  }
  ##########################

  #Signal Blending Meta Level
  ##########################
  oos_testing_eval_metrics <- c("rss", "cp", "rmse", "mae", "mphe", "mpe", "mape", "hr", "mb")
  ##Check if custom_objective is oos_testing_eval_metrics
  if (stringr::str_remove(stringr::str_remove(config@meta_sb_backtest_config@custom_objective, "min_"), "max_") %in% oos_testing_eval_metrics){
    if (length(config@features_passthrough) > 1 ||  config@features_passthrough != "none"){
      stop("features_passthrough should be 'none' when using custom_objective from oos_testing_eval_metrics.")
    }
    if (any(!is.null(base_custom_signal_universe_metrics_m_df), !is.null(meta_custom_signal_universe_metrics_m_df))){
      stop("base_custom_signal_universe_metrics_m_df and meta_custom_signal_universe_metrics_m_df should be NULL when using custom_objective from oos_testing_eval_metrics.")
    }
  }
  ##Check if custom_objective is in custom_signal_universe_metrics
  if (!is.null(meta_custom_signal_universe_metrics_m_df) &&
      !stringr::str_remove(stringr::str_remove(config@meta_sb_backtest_config@custom_objective, "min_"), "max_") %in% colnames(meta_custom_signal_universe_metrics_m_df@data)) {
    stop("custom_objective should be contained in meta_custom_signal_universe_metrics_m_df.")
  }

  # Meta and Base Conformity
  ##########################
  ##Check for same objects being supplied and other checks
  ###Backtest Returns
  if (!is.null(base_backtest_returns_m_xts) && !is.null(meta_backtest_returns_m_xts)){
    if (base_backtest_returns_m_xts@meta_xts_name == meta_backtest_returns_m_xts@meta_xts_name) {
      stop("base_backtest_returns_m_xts and meta_backtest_returns_m_xts should be different objects.")
    }
    if (any(colnames(base_backtest_returns_m_xts@data) %in% colnames(meta_backtest_returns_m_xts@data))) {
      stop("base_backtest_returns_m_xts and meta_backtest_returns_m_xts should not share any columns.")
    }
    if (any(!zoo::index(meta_backtest_returns_m_xts@data) %in% zoo::index(base_backtest_returns_m_xts@data))){
      stop("all meta_backtest_returns_m_xts dates should be contemplated in base_backtest_returns_m_xts")
    }
  }
  if (!is.null(meta_backtest_returns_m_xts) && config@features_passthrough != "none" && is.null(base_backtest_returns_m_xts)){
    stop("base_backtest_returns_m_xts should be provided when features_passthrough is different to 'none'.")
  }

  ###Backtest Returns
  if (!is.null(base_benchmark_returns_m_xts) && !is.null(meta_benchmark_returns_m_xts)){
    if (base_benchmark_returns_m_xts@meta_xts_name == meta_benchmark_returns_m_xts@meta_xts_name) {
      stop("base_benchmark_returns_m_xts and meta_benchmark_returns_m_xts should be different objects.")
    }
    if (any(colnames(base_benchmark_returns_m_xts@data) %in% colnames(meta_benchmark_returns_m_xts@data))) {
      stop("base_benchmark_returns_m_xts and meta_benchmark_returns_m_xts should not share any columns.")
    }
    if (any(!zoo::index(meta_benchmark_returns_m_xts@data) %in% zoo::index(base_benchmark_returns_m_xts@data))){
      stop("all meta_benchmark_returns_m_xts dates should be contemplated in base_benchmark_returns_m_xts")
    }
  }

  ###meta and base meta_dataframe lists
  base_mdfs_list <- list(
    signal_themes_m_df = base_signal_themes_m_df,
    custom_signal_weights_m_df = base_custom_signal_weights_m_df,
    custom_signal_universe_metrics_m_df = base_custom_signal_universe_metrics_m_df
  )
  meta_mdfs_list <- list(
    signal_themes_m_df = meta_signal_themes_m_df,
    custom_signal_weights_m_df = meta_custom_signal_weights_m_df,
    custom_signal_universe_metrics_m_df = meta_custom_signal_universe_metrics_m_df
  )

  ##Create function to validate meta and base meta_dataframes
  validate_meta_and_base_m_df <- function(base_mdfs_list, meta_mdfs_list, features_passthrough, use_data_slot = TRUE) {

    # Helper function to extract data from the object based on the use_data_slot flag
    get_data <- function(obj, use_data_slot) {
      if (use_data_slot) obj@data else obj
    }

    # Iterate over each pair of base and meta data frames using purrr::pmap
    purrr::pmap(
      list(base_mdfs_list, meta_mdfs_list, names(base_mdfs_list)),
      function(base_obj, meta_obj, obj_name) {

        # Validation: Ensure both base and meta objects are provided
        if (!is.null(base_obj) && !is.null(meta_obj)) {

          # Validation: Check that base and meta objects are not the same
          if (base_obj@meta_dataframe_name == meta_obj@meta_dataframe_name) {
            stop(paste0("base_", obj_name, " and meta_", obj_name, " should be different objects."))
          }

          # Extract data from the base and meta objects
          base_data <- get_data(base_obj, use_data_slot)
          meta_data <- get_data(meta_obj, use_data_slot)

          # Validation: Ensure no shared 'id' values between base and meta data
          if (any(base_data$id %in% meta_data$id)) {
            stop(paste0("base_", obj_name, " and meta_", obj_name, " should not share any ids."))
          }

          # Validation: Ensure no shared 'tickers' between base and meta data
          if (any(base_data$tickers %in% meta_data$tickers)) {
            stop(paste0("base_", obj_name, " and meta_", obj_name, " should not share any tickers."))
          }

          # Validation: Check that base and meta data have the same columns
          if (!all(colnames(base_data) == colnames(meta_data))) {
            stop(paste0("base_", obj_name, " and meta_", obj_name, " should have the same columns."))
          }

          # Validation: Ensure all dates in meta data are present in base data
          if (any(!unique(dplyr::pull(meta_data, dates)) %in% dplyr::pull(base_data, dates))) {
            stop(paste0("All dates in meta_", obj_name, " should be present in base_", obj_name, "."))
          }
        }

        # Validation: If meta object is provided and features_passthrough is not "none",
        # ensure the corresponding base object is also provided
        if (!is.null(meta_obj) && (!"none" %in% config@features_passthrough) && is.null(base_obj)) {
          stop(paste0("base_", obj_name, " should be provided when features_passthrough is different from 'none'."))
        }
      }
    )
  }

  ##Validate meta and base meta_dataframes
  validate_meta_and_base_m_df(base_mdfs_list, meta_mdfs_list, config@features_passthrough)


  ##########################

  #Structure of Base Backtest Results
  ##########################
  ###Check for repeated backtest_identifier
  if (length(unique(sapply(base_sb_backtest_results_list, function(x) x@backtest_identifier))) != length(base_sb_backtest_results_list)){
    stop("Base sb backtest identifiers must have unique names.")
  }

  ###Check if is right format
  if (all(sapply(base_sb_backtest_results_list, function(x) class(x)) != "sb_backtest_results")) {
    stop("base_sb_backtest_results must be a list of sb_backtest_results objects.")
  }

  ##########################

  ##Base Conformity at SB Level
  ##########################
  if (verbose) cat("Checking conformity of base-level sb objects.\n")
  ###features_m_df, target_m_dt, signal_themes_m_df, backtest_returns_m_xts, benchmark_returns_m_xts (only makes sense to check in sb_backtest_results)
  ###In sb_backtest_config, those objects will be necessarilly equal because backtest will be run with objects from arguments

  ###chosen_signals_and_positions
  get_and_check_chosen_signals_and_positions(
    base_sb_backtest_results_list = base_sb_backtest_results_list,
    features_passthrough = config@features_passthrough,
    features_m_df = features_m_df@data
  )

  ###Between supplied for meta backtest and base learners (and base learners themselves)
  ####in features_m_df object name
  if (any(sapply(base_sb_backtest_results_list,
                 function(x){
                   sb_backtest_workflow_last_batch <- x@sb_backtest_workflow[[length(x@sb_backtest_workflow)]]
                   sb_backtest_workflow_last_batch$features_object_name != features_m_df@meta_dataframe_name
                 } ))) {
    stop("features_m_df object is not the same in every base SB base backtest results and/or with the features_m_df being currently supplied.")
  }
  ####in target_m_df object name
  if (any(sapply(base_sb_backtest_results_list,
                 function(x){
                   sb_backtest_workflow_last_batch <- x@sb_backtest_workflow[[length(x@sb_backtest_workflow)]]
                   sb_backtest_workflow_last_batch$target_object_name != target_m_df@meta_dataframe_name
                 } ))) {
    stop("target_m_df object is not the same in every base SB base backtest results and/or with the target_m_df being currently supplied.")
  }
  ###Between base_learners themselves
  ###Do it for signal_themes_m_df, backtest_returns and benchmark_returns (only valid for ew, rp, hrp, sw, mmaf and mvo)
  valid_algos <- c("rp", "hrp", "mvo", "mmaf")
  ####signal_themes, backtest_returns and benchmark_returns
  relevant_indices_signal <- sapply(base_sb_backtest_results_list, function(x) {
    sb_backtest_workflow_last_batch <- x@sb_backtest_workflow[[length(x@sb_backtest_workflow)]]
    sb_backtest_workflow_last_batch$sb_algorithm %in% valid_algos
  })
  check_object_consistency <- function(obj_name) {
    #####Which elements should be checked (those using valid_algos)
    keep <- sapply(base_sb_backtest_results_list, function(x) {
      sb_backtest_workflow_last_batch <- x@sb_backtest_workflow[[length(x@sb_backtest_workflow)]]
      sb_backtest_workflow_last_batch$sb_algorithm %in% valid_algos
    })

    #####If at least one element is relevant, compare them
    if (any(keep)) {
      vals <- sapply(
        base_sb_backtest_results_list[keep],
        function(x) x@sb_backtest_workflow[[length(x@sb_backtest_workflow)]][[obj_name]]
      )
      if (length(unique(vals)) > 1) {
        stop(paste(obj_name, "objects are not the same among the filtered backtest results."))
      }
    }
  }
  ####Apply the consistency check for each field
  lapply(
    c("signal_themes_object_name",
      "backtest_returns_object_name",
      "benchmark_returns_object_name"),
    check_object_consistency
  )
  ####Training Sample + Validation Sample
  if (any(sapply(base_sb_backtest_results_list,
                 function(x){
                   sb_backtest_workflow_last_batch <- x@sb_backtest_workflow[[length(x@sb_backtest_workflow)]]
                   (sb_backtest_workflow_last_batch$training_sample_size + sb_backtest_workflow_last_batch$validation_sample_size) !=
                     (base_sb_backtest_results_list[[1]]@sb_backtest_workflow[[length(base_sb_backtest_results_list[[1]]@sb_backtest_workflow)]]$training_sample_size +
                      base_sb_backtest_results_list[[1]]@sb_backtest_workflow[[length(base_sb_backtest_results_list[[1]]@sb_backtest_workflow)]]$validation_sample_size)
                 }))){
    stop("training_sample_size plus validation_sample_size is not the same in every base SB backtest results.")
  }
  ####Rebalancing Month
  if (any(sapply(base_sb_backtest_results_list,
                 function(x) !identical(x@sb_backtest_workflow[[length(x@sb_backtest_workflow)]]$rebalancing_months,
                                        base_sb_backtest_results_list[[1]]@sb_backtest_workflow[[length(x@sb_backtest_workflow)]]$rebalancing_months)))){
    stop("rebalancing_months is not the same in every base SB backtest results.")
  }
  ####Target FWD Name
  if (any(sapply(base_sb_backtest_results_list,
                 function(x) !identical(x@sb_backtest_workflow[[length(x@sb_backtest_workflow)]]$target_fwd_name,
                                        base_sb_backtest_results_list[[1]]@sb_backtest_workflow[[length(x@sb_backtest_workflow)]]$target_fwd_name)))){
    stop("target_fwd_name is not the same in every base SB backtest results.")
  }
  ####Testing Dates
  if (any(sapply(base_sb_backtest_results_list,
                 function(x) !identical(x@sb_backtest_workflow[[length(x@sb_backtest_workflow)]]$dates_testing_sample,
                                        base_sb_backtest_results_list[[1]]@sb_backtest_workflow[[length(x@sb_backtest_workflow)]]$dates_testing_sample)))){
    stop("dates_testing_sample is not the same in every base SB backtest results.")
  }
  ####Check if custom objective depends on consolidated oos_eval_metrics_xts
  if (stringr::str_remove(stringr::str_remove(config@meta_sb_backtest_config@custom_objective, "max_"), "min_") %in% c("rmse", "rss", "hr", "mb", "cp", "mae", "mape", "mphe", "mpe") &&
      !is.null(meta_custom_signal_universe_metrics_m_df)){
    stop("custom_objective depends on base-level consolidated oos_eval_metrics_xts. Please set meta_custom_signal_universe_metrics_m_df to NULL.")
  }
}
