#' @title Summarize Performance Metrics of Backtests
#'
#' @param selected_backtest_returns_corrected_positions_xts_upd_ref A xts containing the backtest returns data for various signals.
#'
#' @param selected_market_factor_proxy_xts_upd_ref A xts containing benchmark returns data.
#'
#' @param selected_signal_themes_m_d_ref A data frame containing metadata about signals. This data frame should include:
#'   - `tickers`: Signal identifiers matching those in `selected_backtest_returns_corrected_positions_xts_upd_ref`.
#'   - `theme`: Group membership for each signal, defining the clusters for the Bayesian hierarchical model.
#'   - `dates`: Dates corresponding to the backtest data.
#'   This input ensures proper alignment between signals and their associated themes.
#'
#' @param model_spec_theme_level A character string specifying the desired Bayesian model structure.
#'   Options include:
#'   - `"random_intercept_fixed_slope"`: Includes random effects for the intercept at the theme level.
#'   - `"theme_specific_intercept_fixed_slope"`: Uses fixed intercepts for each theme.
#'   - `"theme_specific_intercept_theme_specific_slope"`: Includes fixed intercepts and slopes for each theme.
#'   - `"fixed_intercept_fixed_slope"`: Omits theme-level intercepts but includes random effects at the theme:signal level.
#'
#' @param active_returns If TRUE, calculate ative returns before applying performance functions.
#'
#' @import PerformanceAnalytics
#' @importFrom PerformanceAnalytics StdDev SharpeRatio ES
#'
#' @export
summarize_performance <- function(selected_backtest_returns_corrected_positions_xts_upd_ref,
                                  selected_market_factor_proxy_xts_upd_ref,
                                  model_structure, model_spec_theme_level, lmer_control,
                                  selected_signal_themes_m_d_ref,
                                  custom_signal_universe_metrics_m_upd_ref = NULL,
                                  active_returns = TRUE,
                                  verbose = TRUE
                                  ){

  #Initial Preparations
  ##################
  ###Get objects from selected_backtest_returns_corrected_positions_xts_upd_ref
  selected_signals <- colnames(selected_backtest_returns_corrected_positions_xts_upd_ref)
  current_date <- zoo::index(selected_backtest_returns_corrected_positions_xts_upd_ref) %>% max()


  ##################

  #Create baseline signal_universe_m_d_ref
  #################################
  ###Create base_signal_universe_m_d_ref
  base_signal_universe_m_d_ref <- create_performance_m_df(
    selected_backtest_returns_corrected_positions_xts_upd_ref = selected_backtest_returns_corrected_positions_xts_upd_ref,
    selected_market_factor_proxy_xts_upd_ref = selected_market_factor_proxy_xts_upd_ref,
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
        warning(paste("custom_signal_universe_metrics_m_d_ref does not contain data for current date. Using most recent date available:",
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
    capm_model_list <- selected_backtest_returns_corrected_positions_xts_upd_ref %>% apply(2, function(x){
      lm(x ~ selected_market_factor_proxy_xts_upd_ref)
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
        PerformanceAnalytics::mean.geometric(selected_backtest_returns_corrected_positions_xts_upd_ref/100, na.rm = TRUE)*100/
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
      selected_backtest_returns_corrected_positions_xts_upd_ref = selected_backtest_returns_corrected_positions_xts_upd_ref,
      selected_market_factor_proxy_xts_upd_ref = selected_market_factor_proxy_xts_upd_ref,
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
