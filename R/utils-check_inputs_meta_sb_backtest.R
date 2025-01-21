#' Validate Meta Signal Selection and Data Inputs
#'
#' This function performs a series of validation checks on the meta-level
#' signal selection, features passthrough, meta dataframes, and xts objects.
#'
#' @param config An object containing the meta backtest configuration.
#' @param features_m_df A dataframe containing feature names.
#' @param base_signal_themes_m_df Optional base signal themes meta dataframe.
#' @param base_priors_m_df Optional base priors meta dataframe.
#' @param base_custom_signal_weights_m_df Optional base custom signal weights meta dataframe.
#' @param meta_signal_themes_m_df Optional meta signal themes meta dataframe.
#' @param meta_priors_m_df Optional meta priors meta dataframe.
#' @param meta_custom_signal_weights_m_df Optional meta custom signal weights meta dataframe.
#' @param base_backtest_returns_xts Optional xts object for base backtest returns.
#' @param base_benchmark_returns_xts Optional xts object for base benchmark returns.
#' @param meta_backtest_returns_xts Optional xts object for meta backtest returns.
#' @param meta_benchmark_returns_xts Optional xts object for meta benchmark returns.
#'
#' @return None. Stops execution if validation checks fail.
validate_meta_config <- function(
    config,
    features_m_df,
    base_signal_themes_m_df = NULL,
    base_priors_m_df = NULL,
    base_custom_signal_weights_m_df = NULL,
    meta_signal_themes_m_df = NULL,
    meta_priors_m_df = NULL,
    meta_custom_signal_weights_m_df = NULL,
    base_backtest_returns_xts = NULL,
    base_benchmark_returns_xts = NULL,
    meta_backtest_returns_xts = NULL,
    meta_benchmark_returns_xts = NULL
) {

  # Objects Structure
  ##########################
  meta_dfs <- list(
    base_signal_themes_m_df,
    base_priors_m_df,
    base_custom_signal_weights_m_df,
    meta_signal_themes_m_df,
    meta_priors_m_df,
    meta_custom_signal_weights_m_df
  )
  names(meta_dfs) <- c("base_signal_themes_m_df", "base_priors_m_df", "base_custom_signal_weights_m_df",
                       "meta_signal_themes_m_df", "meta_priors_m_df", "meta_custom_signal_weights_m_df")
  for (df_name in names(meta_dfs)) {
    if (!is.null(meta_dfs[[df_name]]) && !is_meta_dataframe(meta_dfs[[df_name]])) {
      stop(paste("If provided,", df_name, "must be a meta_dataframe object."))
    }
  }

  # Check for xts objects
  xts_objects <- list(
    base_backtest_returns_xts,
    base_benchmark_returns_xts,
    meta_backtest_returns_xts,
    meta_benchmark_returns_xts
  )
  names(xts_objects) <- c("base_backtest_returns_xts", "base_benchmark_returns_xts",
                          "meta_backtest_returns_xts", "meta_benchmark_returns_xts")
  for (xts_name in names(xts_objects)) {
    if (!is.null(xts_objects[[xts_name]]) && !xts::is.xts(xts_objects[[xts_name]])) {
      stop(paste("If provided,", xts_name, "must be an xts object."))
    }
  }
  ##########################


  #Signal Selection Meta Level
  ##########################
  # Check for 'all' at signal_selection at meta level
  if (length(config@meta_sb_backtest_config@ss_backtest_config) > 0) {
    if (config@meta_sb_backtest_config@ss_backtest_config$chosen_signals_and_positions != "all") {
      stop("chosen_signals_and_positions should always be 'all' at meta-level.\n" ,
           "This is because features positions are already corrected through features_passthrough.")
    }
  }
  if (length(config@meta_sb_backtest_config@ss_backtest_results) > 0) {
    if (config@meta_sb_backtest_config@ss_backtest_results@sb_backtest_workflow$chosen_signals_and_positions != "all") {
      stop("chosen_signals_and_positions should always be 'all' at meta-level.\n",
           "This is because features positions are already corrected through features_passthrough.")
    }
  }
  # Check for features_passthrough presence
  if (!(length(config@features_passthrough) == 1 && config@features_passthrough %in% c("all", "none"))) {
    if (!all(config@features_passthrough %in% colnames(features_m_df))) {
      stop("features_passthrough should be contained in features_m_df.")
    }
  }
  ##########################

  # Meta and Base Conformity
  ##########################
  # Check for same objects being supplied
  if (!is.null(base_signal_themes_m_df) && !is.null(meta_signal_themes_m_df) &&
      base_signal_themes_m_df@meta_dataframe_name == meta_signal_themes_m_df@meta_dataframe_name) {
    stop("base_signal_themes_m_df and meta_signal_themes_m_df should be different objects.")
  }
  if (!is.null(base_priors_m_df) && !is.null(meta_priors_m_df) &&
      base_priors_m_df@meta_dataframe_name == meta_priors_m_df@meta_dataframe_name) {
    stop("base_priors_m_df and meta_priors_m_df should be different objects.")
  }
  if (!is.null(base_custom_signal_weights_m_df) && !is.null(meta_custom_signal_weights_m_df) &&
      base_custom_signal_weights_m_df@meta_dataframe_name == meta_custom_signal_weights_m_df@meta_dataframe_name) {
    stop("base_custom_signal_weights_m_df and meta_custom_signal_weights_m_df should be different objects.")
  }
  ##########################

  ##Base conformity at SS level
  ##########################
  if (verbose) cat("Checking conformity of objects at Signal-Selection level.\n")
  ###Between supplied for meta backtest and base learners
  ####in signals_m_df object name
  if (!is.null(config@base_sb_backtest_results)){
    base_sb_backtest_results_list <- config@base_sb_backtest_results
  }

  if (any(sapply(base_sb_backtest_results_list,
                 function(x){
                   length(x@ss_backtest_results$ss_backtest_workflow$signals_object_name) > 0 && x@ss_backtest_results$ss_backtest_workflow$signals_object_name != features_m_df@object_name
                 }))) {
    warning("signals_m_df object is not the same in every base SS backtest results.")
  }
  ###Between base_learners themselves
  ####in chosen_signals_and_positions
  #####Get raw chosen_signals_and_positions_list
  chosen_signals_and_positions_list <-
    sapply(base_sb_backtest_results_list, function(x) x@ss_backtest_results$ss_backtest_workflow$chosen_signals_and_positions)
  #####Reconstruct NULL entries with "long" for all features
  for (i in seq_along(chosen_signals_and_positions_list)) {
    if (is.null(chosen_signals_and_positions_list[[i]])) {
      reconstructed <- rep("long", n_features)
      names(reconstructed) <- feature_names
      chosen_signals_and_positions_list[[i]] <- reconstructed
    }
  }
  #####Verify that objects are the same and elect the reference object
  if (length(chosen_signals_and_positions_list) > 1) {
    chosen_signals_and_positions_reference <- chosen_signals_and_positions_list[[1]] #Choose first one as reference
    for (i in seq_along(chosen_signals_and_positions_list)) {
      current_vec <- chosen_signals_and_positions_list[[i]]
      if (!identical(current_vec, chosen_signals_and_positions_reference)) {
        #If they are not identifical, warn
        warning("chosen_signals_and_positions differ at element index: ", i, ".")
        #If features_passthrough is enabled, choose the longest one as reference
        if (config@features_passthrough != "none"){
          cat("Choosing the longest chosen_signals_and_positions as reference when establishing features_passthrough positions. \n")
          chosen_signals_and_positions_reference <- chosen_signals_and_positions_list[[which.max(sapply(chosen_signals_and_positions_list, function(x) length(x)))]]
        }
      }
    }
  } else {
    chosen_signals_and_positions_reference <- chosen_signals_and_positions_list[[1]]
  }

  ####in signal_themes_m_df object name
  if (any(sapply(base_sb_backtest_results_list,
                 function(x){
                   length(x@ss_backtest_results$ss_backtest_workflow$signal_themes_object_name) > 0 && x@ss_backtest_results$ss_backtest_workflow$signal_themes_object_name != base_sb_backtest_results_list[[1]]@ss_backtest_results@ss_backtest_workflow$signal_themes_object_name
                 }))) {
    warning("signal_themes_m_df object is not the same in every base SS backtest results.")
  }
  ####in backtest_returns object name
  if (any(sapply(base_sb_backtest_results_list,
                 function(x){
                   length(x@ss_backtest_results$ss_backtest_workflow$backtest_returns_object_name) > 0 && x@ss_backtest_results$ss_backtest_workflow$backtest_returns_object_name != base_sb_backtest_results_list[[1]]@ss_backtest_results@ss_backtest_workflow$backtest_returns_object_name
                 }))) {
    warning("backtest_returns_xts object is not the same in every base SS backtest results.")
  }
  ####in benchmark_returns object name
  if (any(sapply(base_sb_backtest_results_list,
                 function(x){
                   length(x@ss_backtest_results$ss_backtest_workflow$benchmark_returns_object_name) > 0 && x@ss_backtest_results$ss_backtest_workflow$benchmark_returns_object_name != base_sb_backtest_results_list[[1]]@ss_backtest_results@ss_backtest_workflow$benchmark_returns_object_name
                 }))) {
    warning("backtest_returns_xts object is not the same in every base SS backtest results.")
  }
  ####in priors_m_df object name
  if (any(sapply(base_sb_backtest_results_list,
                 function(x){
                   length(x@ss_backtest_results$ss_backtest_workflow$priors_object_name) > 0 && x@ss_backtest_results$ss_backtest_workflow$priors_object_name != base_sb_backtest_results_list[[1]]@ss_backtest_results@ss_backtest_workflow$priors_object_name
                 }))) {
    warning("priors_m_df object is not the same in every base SS backtest results.")
  }

  ##Check for objects conformity at SB level
  if (verbose) cat("Checking conformity of objects at Signal-Blending level.\n")
  ###Between supplied for meta backtest and base learners
  ####in features_m_df object name
  if (any(sapply(base_sb_backtest_results_list,
                 function(x) x@sb_backtest_workflow$features_object_name != features_m_df@object_name))) {
    warning("features_m_df object is not the same in every base SB base backtest results and/or with the features_m_df being currently supplied.")
  }
  ####in target_m_df object name
  if (any(sapply(base_sb_backtest_results_list,
                 function(x) x@sb_backtest_workflow$target_object_name != target_m_df@object_name))) {
    warning("target_m_df object is not the same in every base SB base backtest results and/or with the target_m_df being currently supplied.")
  }
  ###Between base_learners themselves
  ####in signal_themes_m_df object name
  if (any(sapply(base_sb_backtest_results_list,
                 function(x) x@sb_backtest_workflow$signal_themes_object_name != base_sb_backtest_results_list[[1]]@sb_backtest_workflow$signal_themes_object_name))) {
    warning("signal_themes_m_df object is not the same in every base SB backtest results.")
  }
  ####in backtest_returns_xts object name
  if (any(sapply(base_sb_backtest_results_list,
                 function(x) x@sb_backtest_workflow$backtest_returns_object_name != base_sb_backtest_results_list[[1]]@sb_backtest_workflow$backtest_returns_object_name))) {
    warning("backtest_returns_xts object name must be the same in all base SB backtest results.")
  }
  ####in benchmark_returns_xts object name
  if (any(sapply(base_sb_backtest_results_list,
                 function(x) x@sb_backtest_workflow$benchmark_returns_object_name != base_sb_backtest_results_list[[1]]@sb_backtest_workflow$benchmark_returns_object_name))) {
    warning("benchmark_returns_xts object is not the same in every base SB backtest results.")
  }




}
