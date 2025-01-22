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
#' @param base_backtest_returns_m_xts Optional xts object for base backtest returns.
#' @param base_benchmark_returns_m_xts Optional xts object for base benchmark returns.
#' @param meta_backtest_returns_m_xts Optional xts object for meta backtest returns.
#' @param meta_benchmark_returns_m_xts Optional xts object for meta benchmark returns.
#'
#' @return None. Stops execution if validation checks fail.
check_inputs_meta_sb_backtest <- function(
    config,
    features_m_df,
    base_signal_themes_m_df = NULL,
    base_priors_m_df = NULL,
    base_custom_signal_weights_m_df = NULL,
    meta_signal_themes_m_df = NULL,
    meta_priors_m_df = NULL,
    meta_custom_signal_weights_m_df = NULL,
    base_backtest_returns_m_xts = NULL,
    base_benchmark_returns_m_xts = NULL,
    meta_backtest_returns_m_xts = NULL,
    meta_benchmark_returns_m_xts = NULL,
    verbose
) {

  # Objects Structure
  ##########################
  # Check for meta_dataframe objects
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
    if (!all(config@features_passthrough %in% colnames(features_m_df@data))) {
      stop("features_passthrough should be contained in features_m_df.")
    }
  }
  ##########################

  # Meta and Base Conformity
  ##########################
    ##Check for same objects being supplied
    if (!is.null(base_signal_themes_m_df) && !is.null(meta_signal_themes_m_df)){
      if (base_signal_themes_m_df@meta_dataframe_name == meta_signal_themes_m_df@meta_dataframe_name) {
        stop("base_signal_themes_m_df and meta_signal_themes_m_df should be different objects.")
      }
      if (any(base_signal_themes_m_df@data$id %in% meta_signal_themes_m_df@data$id)) {
        stop("base_signal_themes_m_df and meta_signal_themes_m_df should not share any ids.")
      }
    }

    if (!is.null(base_priors_m_df) && !is.null(meta_priors_m_df)){
      if (base_priors_m_df@meta_dataframe_name == meta_priors_m_df@meta_dataframe_name) {
        stop("base_priors_m_df and meta_priors_m_df should be different objects.")
      }
      if (any(base_priors_m_df@data$id %in% meta_priors_m_df@data$id)) {
        stop("base_priors_m_df and meta_priors_m_df should not share any ids.")
      }
    }

    if (!is.null(base_custom_signal_weights_m_df) && !is.null(meta_custom_signal_weights_m_df)){
      if (base_custom_signal_weights_m_df@meta_dataframe_name == meta_custom_signal_weights_m_df@meta_dataframe_name) {
        stop("base_custom_signal_weights_m_df and meta_custom_signal_weights_m_df should be different objects.")
      }
      if (any(base_custom_signal_weights_m_df@data$id %in% meta_custom_signal_weights_m_df@data$id)) {
        stop("base_custom_signal_weights_m_df and meta_custom_signal_weights_m_df should not share any ids.")
      }
    }


  ##########################

  #Structure of Base Backtest Results and Configs
  ##########################
  if (!is.null(config@base_sb_backtest_results)){
    base_sb_backtest_results_list <- config@base_sb_backtest_results
    ###Check for repeated backtest_identifier
    if (length(unique(sapply(base_sb_backtest_results_list, function(x) x@backtest_identifier))) != length(base_sb_backtest_results_list)){
      stop("Base sb backtest identifiers must have unique names.")
    }

    ###Check if is right format
    if (all(sapply(base_sb_backtest_results_list, function(x) class(x)) != "sb_backtest_results")) {
      stop("base_sb_backtest_results must be a list of sb_backtest_results objects.")
    }

  } else {
    base_sb_backtest_configs_list <- config@base_sb_backtest_configs #Get list
    ###Check for repeated config_names
    if (length(unique(sapply(base_sb_backtest_configs_list, function(x) x@config_name))) != length(base_sb_backtest_configs_list)){
      stop("Base sb backtest configurations must have unique names.")
    }

    ###Check if there is more than one sb_algorithm assigned as custom_weights
    if (length(which(sapply(base_sb_backtest_configs_list, function(x) x@sb_algorithm) %in% c("custom_weights"))) > 1){
      warning("All custom_weights models will be assined the same base_custom_signal_weights_m_df")
    }
  }
  ##########################


  ##Base conformity at SS level
  ##########################
  if (verbose) cat("Checking conformity of objects at Signal-Selection level.\n")
    ###signals_m_df, signal_themes_m_df, backtest_returns_m_xts, benchmark_returns_m_xts (only makes sense to check in sb_backtest_results)
    ###In sb_backtest_config, those objects will be necessarilly equal because backtest will be run with objects from arguments
    if (!is.null(config@base_sb_backtest_results)){
      base_sb_backtest_results_list <- config@base_sb_backtest_results
      ###Between supplied for meta backtest and base learners (and base learners themselves)
        ####in signals_m_df object name
        if (any(sapply(base_sb_backtest_results_list,
                       function(x){
                         length(x@ss_backtest_results$ss_backtest_workflow$signals_object_name) > 0 && x@ss_backtest_results$ss_backtest_workflow$signals_object_name != features_m_df@meta_dataframe_name
                       }))) {
          stop("signals_m_df object is not the same in every base SS backtest results and/or with the features_m_df being currently supplied.")
        }
      ###Between base learners themselves
        ####in signal_themes_m_df object name
        if (any(sapply(base_sb_backtest_results_list,
                       function(x){
                         length(x@ss_backtest_results$ss_backtest_workflow$signal_themes_object_name) > 0 && x@ss_backtest_results$ss_backtest_workflow$signal_themes_object_name != base_sb_backtest_results_list[[1]]@ss_backtest_results@ss_backtest_workflow$signal_themes_object_name
                     }))) {
          stop("signal_themes_m_df object is not the same in every base SS backtest results.")
        }

        ####in backtest_returns object name
        if (any(sapply(base_sb_backtest_results_list,
                       function(x){
                         length(x@ss_backtest_results$ss_backtest_workflow$backtest_returns_object_name) > 0 && x@ss_backtest_results$ss_backtest_workflow$backtest_returns_object_name != base_sb_backtest_results_list[[1]]@ss_backtest_results@ss_backtest_workflow$backtest_returns_object_name
                     }))) {
          stop("backtest_returns_m_xts object is not the same in every base SS backtest results.")
        }
        ####in benchmark_returns object name
        if (any(sapply(base_sb_backtest_results_list,
                       function(x){
                         length(x@ss_backtest_results$ss_backtest_workflow$benchmark_returns_object_name) > 0 && x@ss_backtest_results$ss_backtest_workflow$benchmark_returns_object_name != base_sb_backtest_results_list[[1]]@ss_backtest_results@ss_backtest_workflow$benchmark_returns_object_name
                     }))) {
          stop("benchmark_returns_m_xts object is not the same in every base SS backtest results.")
        }
    }

    ###chosen_signals_and_positions
    get_and_check_chosen_signals_and_positions(
      base_sb_backtest_results_list = config@base_sb_backtest_results,
      base_sb_backtest_configs_list = config@base_sb_backtest_configs,
      features_passthrough = config@features_passthrough,
      features_m_df = features_m_df@data
    )

  ##########################

  ##Base Conformity at SB Level
  ##########################
  if (verbose) cat("Checking conformity of objects at Signal-Blending level.\n")
    ###features_m_df, target_m_dt, signal_themes_m_df, backtest_returns_m_xts, benchmark_returns_m_xts (only makes sense to check in sb_backtest_results)
    ###In sb_backtest_config, those objects will be necessarilly equal because backtest will be run with objects from arguments
    if (!is.null(config@base_sb_backtest_results)){
      base_sb_backtest_results_list <- config@base_sb_backtest_results
      ###Between supplied for meta backtest and base learners (and base learners themselves)
        ####in features_m_df object name
        if (any(sapply(base_sb_backtest_results_list,
                       function(x) x@sb_backtest_workflow$features_object_name != features_m_df@object_name))) {
          stop("features_m_df object is not the same in every base SB base backtest results and/or with the features_m_df being currently supplied.")
        }
        ####in target_m_df object name
        if (any(sapply(base_sb_backtest_results_list,
                       function(x) x@sb_backtest_workflow$target_object_name != target_m_df@object_name))) {
          stop("target_m_df object is not the same in every base SB base backtest results and/or with the target_m_df being currently supplied.")
        }
      ###Between base_learners themselves
      ###Do it for signal_themes_m_df, backtest_returns and benchmark_returns (only valid for ew, rp, sw and mvo)
      valid_algos <- c("ew", "rp", "sw", "mvo")
        ####signal_themes, backtest_returns and benchmark_returns
        relevant_indices_signal <- sapply(base_sb_backtest_results_list, function(x) {
          x@sb_backtest_workflow$sb_algorithm %in% valid_algorithms
        })
        check_object_consistency <- function(obj_name) {
          #####Which elements should be checked (those using valid_algos)
          keep <- sapply(base_sb_backtest_results_list, function(x) {
            x@sb_backtest_workflow$sb_algorithm %in% valid_algos
          })

          #####If at least one element is relevant, compare them
          if (any(keep)) {
            vals <- sapply(
              base_sb_backtest_results_list[keep],
              function(x) x@sb_backtest_workflow[[obj_name]]
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
                         (x@sb_backtest_workflow$training_sample_size + x@sb_backtest_workflow$validation_sample_size) !=
                         (base_sb_backtest_results_list[[1]]@sb_backtest_workflow$training_sample_size + base_sb_backtest_results_list[[1]]@sb_backtest_workflow$validation_sample_size)
                       }))){
          stop("training_sample_size + validation_sample_size is not the same in every base SB backtest results.")
        }
        ####Rebalancing Month
        if (any(sapply(base_sb_backtest_results_list,
                       function(x) !identical(x@sb_backtest_workflow$rebalancing_months, base_sb_backtest_results_list[[1]]@sb_backtest_workflow$rebalancing_months)))){
          stop("rebalancing_months is not the same in every base SB backtest results.")
        }
        ####Target FWD Name
        if (any(sapply(base_sb_backtest_results_list,
                       function(x) !identical(x@sb_backtest_workflow$target_fwd_name, base_sb_backtest_results_list[[1]]@sb_backtest_workflow$target_fwd_name)))){
          stop("target_fwd_name is not the same in every base SB backtest results.")
        }

    }  else {
      base_sb_backtest_configs_list <- config@base_sb_backtest_configs #Get list

      ####Training Sample + Validation Sample
      if (any(sapply(base_sb_backtest_configs_list,
                     function(x){
                       (x@training_sample_size + x@validation_sample_size) !=
                         (base_sb_backtest_configs_list[[1]]@training_sample_size + base_sb_backtest_configs_list[[1]]@validation_sample_size)
                     }))){
        stop("training_sample_size + validation_sample_size is not the same in every base SB backtest configs")
      }
      ####Rebalancing Month
      if (any(sapply(base_sb_backtest_configs_list,
                     function(x) !identical(x@rebalancing_months, base_sb_backtest_configs_list[[1]]@rebalancing_months)))){
        stop("rebalancing_months is not the same in every base SB backtest configs.")
      }
      ####Target FWD Name
      if (any(sapply(base_sb_backtest_configs_list,
                     function(x) !identical(x@target_fwd_name, base_sb_backtest_configs_list[[1]]@target_fwd_name)))){
        stop("target_fwd_name is not the same in every base SB backtest configs.")
      }
    }

}
