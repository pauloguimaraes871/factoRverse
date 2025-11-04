#' @title Summarize Performance Metrics of Backtests
#'
#' @description
#' Computes performance metrics for a set of signal backtests using either simple (no-pooled) linear CAPM models or hierarchical mixed-effects CAPM models.
#' The output includes per-signal metrics such as alphas, betas, specific risk, t-statistics, p-values, Treynor ratios, and appraisal ratios, adjusted for active returns if applicable.
#'
#' @param selected_backtest_returns_corrected_positions_m_xts_upd_ref An `xts` object containing backtest returns for each signal (columns are signal tickers, rows are dates).
#'
#' @param selected_market_factor_proxy_m_xts_upd_ref An `xts` object containing benchmark or market factor returns. Used to estimate systematic risk exposures (betas).
#'
#' @param model_structure A character string indicating the CAPM model structure. Must be one of:
#' \itemize{
#'   \item `"no_pooled"`: Fits separate CAPM models per signal using OLS.
#'   \item `"partial_pooled"`: Fits a hierarchical CAPM model using `lme4::lmer`.
#' }
#'
#' @param model_spec_theme_level A character string defining the structure of the hierarchical CAPM. Must be one of:
#' \itemize{
#'   \item `"random_intercept_fixed_slope"`
#'   \item `"theme_specific_intercept_fixed_slope"`
#' \item `"theme_specific_intercept_theme_specific_slope"`
#' \item `"fixed_intercept_fixed_slope"`
#' }
#'
#' @param lmer_control A named list of control parameters for `lme4::lmer()`. Valid entries include:
#' \itemize{
#'   \item `lmer_optimizer`: Optimizer used (e.g., `"nloptwrap"`).
#'   \item `lmer_optimization_objective`: `"REML"` or `"ML"`.
#'   \item `hierarchical_p_value_method`: Method for p-value approximation; `"Satterthwaite"`, `"Kenward-Roger"`, or `"lme4"`.
#' }
#'
#' @param selected_signal_themes_m_d_ref A dataframe describing signal membership in themes. Must include:
#' \itemize{
#'   \item `tickers`: Signal identifiers matching those in the return matrix.
#'   \item `theme`: Group (e.g., sector) membership of each signal.
#'   \item `dates`: Date of signal activity.
#' }
#'
#' @param custom_signal_universe_metrics_m_upd_ref Optional dataframe with additional signal-level metrics. Most recent available date is used. It must include:
#' \itemize{
#'   \item `tickers`, `dates`, and `id`
#'   \item Any additional performance metrics to append.
#' }
#'
#' @param active_returns Logical. If `TRUE`, return series are converted to active returns (i.e., excess returns over the benchmark) before estimation.
#'
#' @param verbose Logical. If `TRUE`, prints progress messages.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{`signal_universe_m_d_ref`}{A meta dataframe with enriched performance metrics including alphas, betas, t-statistics, risk-adjusted ratios, and p-values.}
#'   \item{`frequentist_fit_results_list`}{A list of fitted model objects:
#'     \itemize{
#'       \item For `"no_pooled"`: A list of `lm` models (one per signal).
#'       \item For `"partial_pooled"`: A fitted `lmer` model.
#'     }
#'   }
#' }
#'
summarize_performance <- function(selected_backtest_returns_corrected_positions_m_xts_upd_ref,
                                  selected_market_factor_proxy_m_xts_upd_ref,
                                  model_structure, model_spec_theme_level, lmer_control,
                                  selected_signal_themes_m_d_ref,
                                  custom_signal_universe_metrics_m_upd_ref = NULL,
                                  active_returns = TRUE,
                                  verbose = TRUE
){

  #Early return when input is NULL
  if (is.null(selected_backtest_returns_corrected_positions_m_xts_upd_ref) ||
      nrow(selected_backtest_returns_corrected_positions_m_xts_upd_ref) == 0) {

    if (identical(model_structure, "partial_pooled")) {
      stop("selected_backtest_returns_corrected_positions_m_xts_upd_ref can't be NULL when model_structure == 'partial_pooled'.")
    }

    ## Build the empty base table
    base_signal_universe_m_d_ref <- create_performance_m_df(
      selected_backtest_returns_corrected_positions_m_xts_upd_ref = NULL,
      selected_market_factor_proxy_m_xts_upd_ref = selected_market_factor_proxy_m_xts_upd_ref,
      active_returns = active_returns,
      verbose = verbose
    )

    ## Ensure no_pooled branch columns are present (empty, zero-row)
    no_pooled_cols <- c(
      "alpha","alpha_se","beta","specific_risk","alpha_t_stat",
      "treynor_ratio","appraisal_ratio","p_value"
    )
    for (nm in no_pooled_cols) {
      if (!nm %in% colnames(base_signal_universe_m_d_ref)) {
        base_signal_universe_m_d_ref[[nm]] <- NA_real_[0]
      }
    }
    performance_summary_list <- list(
      signal_universe_m_d_ref = base_signal_universe_m_d_ref,
      frequentist_fit_results_list = NULL
    )
    return(performance_summary_list)
  }


  #Initial Preparations
  ##################
  ###Get objects from selected_backtest_returns_corrected_positions_m_xts_upd_ref
  selected_signals <- colnames(selected_backtest_returns_corrected_positions_m_xts_upd_ref)
  current_date <- zoo::index(selected_backtest_returns_corrected_positions_m_xts_upd_ref) %>% max()


  ##################

  #Create baseline signal_universe_m_d_ref
  #################################
  ###Create base_signal_universe_m_d_ref
  base_signal_universe_m_d_ref <- create_performance_m_df(
    selected_backtest_returns_corrected_positions_m_xts_upd_ref = selected_backtest_returns_corrected_positions_m_xts_upd_ref,
    selected_market_factor_proxy_m_xts_upd_ref = selected_market_factor_proxy_m_xts_upd_ref,
    active_returns = active_returns,
    verbose = verbose
  )

  #################################

  ##Add Custom Signal Universe Metrics
  if (!is.null(custom_signal_universe_metrics_m_upd_ref)){
    ##Get most recent custom signal universe metrics
    most_recent_custom_signal_universe_metrics_m_d_ref <- custom_signal_universe_metrics_m_upd_ref %>% dplyr::filter(dates == max(dates))

      ###Check if most recent one is not current date and send warning if not
      if (unique(dplyr::pull(most_recent_custom_signal_universe_metrics_m_d_ref, dates)) != current_date){
        message(paste("custom_signal_universe_metrics_m_d_ref does not contain data for current date. Using most recent date available:",
                      unique(dplyr::pull(most_recent_custom_signal_universe_metrics_m_d_ref, dates))))
      }

      ###Check if all signals are contemplated
      if (!any((base_signal_universe_m_d_ref %>% dplyr::pull(tickers)) %in% (most_recent_custom_signal_universe_metrics_m_d_ref %>% dplyr::pull(tickers)))){
        stop("Not all signals are contemplated in custom_signal_universe_metrics_m_d_ref")
      }

    ###Join by tickers
    base_signal_universe_m_d_ref <- dplyr::left_join(base_signal_universe_m_d_ref,
                                                     dplyr::select(most_recent_custom_signal_universe_metrics_m_d_ref, -id, -dates), by = "tickers")
  }

  ##Market-factor related
  #################################
  ###No Pooled Structure
  if (model_structure == "no_pooled"){

    ###Get CAPM Models
    #################
    capm_model_list <- selected_backtest_returns_corrected_positions_m_xts_upd_ref %>% apply(2, function(x){
      stats::lm(x ~ selected_market_factor_proxy_m_xts_upd_ref)
    })
    #################

    ###Build CAPM data.frame
    #################
    no_pooled_CAPM_metrics_m_d_ref <- data.frame(
      # ID
      id = paste0(selected_signals, "-", current_date),
      # Tickers
      tickers = selected_signals,
      # Dates
      dates = current_date,
      ##Alpha
      alpha = as.numeric(sapply(capm_model_list, function(x) summary(x)$coefficients[1])),
      ##Alpha SE
      alpha_se = as.numeric(sapply(capm_model_list, function(x) summary(x)$coefficients[3])),
      ##Exposure to Systematic Risk
      beta = as.numeric(sapply(capm_model_list, function(x) summary(x)$coefficients[2])),
      ##Specific Risk
      specific_risk = as.numeric(sapply(capm_model_list, function(x) sigma(x))),
      ##Alpha T-Stat
      alpha_t_stat = as.numeric(sapply(capm_model_list, function(x) summary(x)$coefficients[5])),
      ##Treynor Ratio
      treynor_ratio = as.numeric(
        PerformanceAnalytics::mean.geometric(selected_backtest_returns_corrected_positions_m_xts_upd_ref/100, na.rm = TRUE)*100/
        sapply(capm_model_list, function(x) summary(x)$coefficients[2])
        ),
      ##Appraisal Ratio
      appraisal_ratio = as.numeric(
        (sapply(capm_model_list, function(x) summary(x)$coefficients[1]))/
        (sapply(capm_model_list, function(x) sigma(x)))
      ),
      ##P-value
      p_value = as.numeric(sapply(capm_model_list, function(x) summary(x)$coefficients[7]))/2
    )
    #################

    ###Join
    signal_universe_m_d_ref <- dplyr::left_join(base_signal_universe_m_d_ref, dplyr::select(no_pooled_CAPM_metrics_m_d_ref, -tickers, -dates), by = "id")

  }

  ###Pooled Structure
  if (model_structure == "partial_pooled"){

    #Get parameters of lmer_control (use default values in case of NULL)
    lmer_optimizer <- if (is.null(lmer_control$lmer_optimizer)) "nloptwrap" else lmer_control$lmer_optimizer
    lmer_optimization_objective <- if (is.null(lmer_control$lmer_optimization_objective)) "REML" else lmer_control$lmer_optimization_objective
    lmer_optimization_objective <- if (lmer_optimization_objective == "REML") TRUE else FALSE
    hierarchical_p_value_method <- if (is.null(lmer_control$hierarchical_p_value_method)) "Satterthwaite" else lmer_control$hierarchical_p_value_method

    ##Get unique hierarchical CAPM model
    #################
    hierarchical_frequentist_fit_results_list <- fit_frequentist_hierarchical_model(
      ###Data
      signal_universe_m_d_ref = base_signal_universe_m_d_ref,
      selected_backtest_returns_corrected_positions_m_xts_upd_ref = selected_backtest_returns_corrected_positions_m_xts_upd_ref,
      selected_market_factor_proxy_m_xts_upd_ref = selected_market_factor_proxy_m_xts_upd_ref,
      ###Signal Themes
      selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref,
      model_spec_theme_level = model_spec_theme_level,
      ###Lmer Control
      lmer_optimizer = lmer_optimizer, lmer_optimization_objective = lmer_optimization_objective, hierarchical_p_value_method = hierarchical_p_value_method
    )

    #################
    pooled_CAPM_metrics_m_d_ref <- hierarchical_frequentist_fit_results_list$pooled_CAPM_metrics_m_d_ref
    lmer_model <- hierarchical_frequentist_fit_results_list$lmer_model

    ###Join
      ###First check for any potential unmatches
        ####Get unmatched IDs
        unmatched_ids <- setdiff(base_signal_universe_m_d_ref$id, pooled_CAPM_metrics_m_d_ref$id)

        ####Perform pre-check
        if (length(unmatched_ids) > 0) {
          stop("The following IDs in base_signal_universe_m_d_ref do not have a match in pooled_CAPM_metrics_m_d_ref: ",
               paste(unmatched_ids, collapse = ", "))
        }
        ####Join
        signal_universe_m_d_ref <- dplyr::left_join(base_signal_universe_m_d_ref, dplyr::select(pooled_CAPM_metrics_m_d_ref, -tickers, -dates), by = "id")
  }

  ##Return
  performance_summary_list <- list(
    signal_universe_m_d_ref = signal_universe_m_d_ref,
    frequentist_fit_results_list = if (model_structure == "no_pooled") capm_model_list else lmer_model
  )

  return(performance_summary_list)

}
