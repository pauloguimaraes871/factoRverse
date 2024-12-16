#' @param selected_backtest_returns_corrected_positions_xts_upd_ref A xts containing the backtest returns data for various signals.
#'
#' @param selected_market_factor_proxy_xts_upd_ref A xts containing benchmark returns data.
#'
#' @param signal_themes_m_d_ref A data frame containing metadata about signals. This data frame should include:
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
summarize_performance <- function(selected_backtest_returns_corrected_positions_xts_upd_ref,
                                  selected_market_factor_proxy_xts_upd_ref,
                                  model_structure, model_spec_theme_level, lmer_control,
                                  signal_themes_m_d_ref,
                                  active_returns = TRUE
                                  ){

  #Initial Preparations
  ##################
  ###Get objects from selected_backtest_returns_corrected_positions_xts_upd_ref
  selected_signals <- colnames(selected_backtest_returns_corrected_positions_xts_upd_ref)
  current_date <- zoo::index(selected_backtest_returns_corrected_positions_xts_upd_ref)[length(zoo::index(selected_backtest_returns_corrected_positions_xts_upd_ref))]


  ##################

  #Create baseline signal_universe_m_d_ref
  #################################
  ###Create base_signal_universe_m_d_ref
  base_signal_universe_m_d_ref <- create_performance_m_df(
    selected_backtest_returns_corrected_positions_xts_upd_ref = selected_backtest_returns_corrected_positions_xts_upd_ref,
    selected_market_factor_proxy_xts_upd_ref = selected_market_factor_proxy_xts_upd_ref,
    active_returns = active_returns
  )

  #################################

  ##Market-factor related
  #################################
  ###No Pooled Structure
  if(model_structure == "no_pooled"){

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
      )
    )
    #################

    ###Join
    signal_universe_m_d_ref <- dplyr::left_join(base_signal_universe_m_d_ref, dplyr::select(no_pooled_CAPM_metrics_m_d_ref, -tickers, -dates), by = "id")

  }

  ###Pooled Structure
  if(model_structure == "pooled"){

    #Get parameters of lmer_control
    lmer_optimizer <- if(is.null(lmer_control$lmer_optimizer)) "nloptwrap" else lmer_control$lmer_optimizer
    lmer_optimization_objective <- if(is.null(lmer_control$lmer_optimization_objective)) "REML" else lmer_control$lmer_optimization_objective
    lmer_optimization_objective <- if(lmer_control$lmer_optimization_objective == "REML") TRUE else FALSE
    hierarchical_p_value_method <- if(is.null(lmer_control$hierarchical_p_value_method)) "Satterthwaite" else lmer_control$hierarchical_p_value_method

    ##Get unique hierarchical CAPM model
    #################
    hierarchical_frequentist_fit_results_list <- fit_frequentist_hierarchical_model(
      ###Data
      signal_universe_m_d_ref = base_signal_universe_m_d_ref,
      selected_backtest_returns_corrected_positions_xts_upd_ref = selected_backtest_returns_corrected_positions_xts_upd_ref,
      selected_market_factor_proxy_xts_upd_ref = selected_market_factor_proxy_xts_upd_ref,
      ###Signal Themes
      signal_themes_m_d_ref = signal_themes_m_d_ref,
      model_spec_theme_level = model_spec_theme_level,
      ###Lmer Control
      lmer_optimizer = lmer_optimizer, lmer_optimization_objective = lmer_optimization_objective, hierarchical_p_value_method = hierarchical_p_value_method
      )

    #################
    pooled_CAPM_metrics_m_d_ref <- hierarchical_frequentist_fit_results_list$pooled_CAPM_metrics_m_d_ref
    lmer_model <- hierarchical_frequentist_fit_results_list$lmer_model

    ###Join
    signal_universe_m_d_ref <- dplyr::left_join(base_signal_universe_m_d_ref, dplyr::select(pooled_CAPM_metrics_m_d_ref, -tickers, -dates), by = "id")
  }

  ##Return
  performance_summary_list <- list(
    signal_universe_m_d_ref = signal_universe_m_d_ref,
    frequentist_fit_results_list = if(model_structure == "no_pooled") capm_model_list else hierarchical_frequentist_fit_results_list
  )

  return(performance_summary_list)

}
