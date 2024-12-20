#' Fit a Frequentist Hierarchical Regression Model and Summarize Metrics
#'
#' This function fits a frequentist hierarchical regression model using the `lme4` package and computes summary
#' statistics for signal-level metrics. The model captures signal clustering within themes and provides detailed
#' metrics for use in performance analysis.
#'
#' @param signal_universe_m_d_ref A data frame containing the signal universe. If provided, data in this object will be updated with hierarchical model metrics.
#'
#' @param selected_backtest_returns_corrected_positions_upd_ref A data frame containing the backtest returns data for various signals.
#'   - The first column should include dates.
#'   - Remaining columns represent signals (e.g., tickers) and their respective active returns.
#'
#' @param selected_market_factor_proxy_vector_upd_ref A numeric vector containing benchmark returns data. The vector will be recycled
#'   to match the length of the backtest returns data.
#'
#' @param selected_signal_themes_m_d_ref A data frame containing metadata about signals. This data frame should include:
#'   - `tickers`: Signal identifiers matching those in `selected_backtest_returns_corrected_positions_upd_ref`.
#'   - `theme`: Group membership for each signal, defining the clusters for the Bayesian hierarchical model.
#'   - `dates`: Dates corresponding to the backtest data.
#'   This input ensures proper alignment between signals and their associated themes.
#'
#' @param selected_backtest_returns_corrected_positions_m_upd_ref An already processed `selected_backtest_returns_corrected_positions_upd_ref`.
#' This data.frame is already in long format and contemplates both the `selected_market_factor_proxy_vector_upd_ref` and the
#' `selected_signal_themes_m_d_ref` theme data.
#'
#' @param model_spec_theme_level A character string specifying the desired model structure.
#'   Options include:
#'   - `"random_intercept_fixed_slope"`: Includes random effects for the intercept at the theme level.
#'   - `"theme_specific_intercept_fixed_slope"`: Uses fixed intercepts for each theme.
#'   - `"theme_specific_intercept_theme_specific_slope"`: Includes fixed intercepts and slopes for each theme.
#'   - `"fixed_intercept_fixed_slope"`: Omits theme-level intercepts but includes random effects at the theme:signal level.
#'
#' @param verbose A logical indicating whether to display progress messages during the model fitting process. Defaults to `TRUE`.
#'
#' @return A named list containing:
#'   - `frequentist_model`: A `lmermod` object containing the fitted frequentist model.
#'   - `signal_universe_m_d_ref`: A data frame containing the updated `signal_universe_m_d_ref`.
#'
#' @details
#' The function performs the following steps:
#' 1. **Preprocessing**: Converts the backtest returns data into long format, merges metadata about signals and themes,
#'    and adds the market factor proxy.
#' 2. **Formula Specification**: Defines the model formula based on the chosen `model_spec_theme_level`.
#' 3. **Model Fitting**: Fits the frequentist model using the `lmer` function from the `lme4` package, incorporating the
#'     prepared data.
#' 4. **Summarization**:
#'    Updates the `signal_universe_m_d_ref` data frame with new metrics.
#'
#' This hierarchical approach allows the model to capture both global (theme-level) and local (signal-level) effects.
#'
#''@section P-value calculation
#'The calculation of p-values is a controversial topic regarding linear mixed-effects models, which is the reason why
#' `lme4::lmer` does not provide p-values by default. The `afex` package provides a range of methods to calculate p-values for mixed models
#'
#'
fit_frequentist_hierarchical_model <- function(signal_universe_m_d_ref,
                                               selected_backtest_returns_corrected_positions_xts_upd_ref, selected_market_factor_proxy_xts_upd_ref, #Data
                                               selected_backtest_returns_corrected_positions_m_upd_ref = NULL,
                                               selected_signal_themes_m_d_ref, model_spec_theme_level, #Hierarhical Model spec
                                               lmer_optimizer, lmer_optimization_objective, hierarchical_p_value_method){ #lmer parameters

  #Check inputs
  ######################
  if(!lmer_optimizer %in% c("nloptwrap", "bobyqa", "Nelder_Mead", "nlminbwrap")){
    stop("Invalid optimizer. Please choose from 'nloptwrap', 'bobyqa', 'Nelder_Mead' or 'nlminbwrap'.")
  }
  if(!lmer_optimization_objective %in% c(TRUE, FALSE)){
    stop("Invalid optimization objective. Please choose from TRUE or FALSE.")
  }

  ######################

  #Prepare objects
  ########################
  lmer_model_inputs_list <- prepare_hierarchical_model_inputs(
    selected_backtest_returns_corrected_positions_xts_upd_ref = selected_backtest_returns_corrected_positions_xts_upd_ref,
    selected_market_factor_proxy_xts_upd_ref = selected_market_factor_proxy_xts_upd_ref,
    #If selected_backtest_returns_corrected_positions_m_upd_ref is not NULL, it will just give formulas
    selected_backtest_returns_corrected_positions_m_upd_ref = selected_backtest_returns_corrected_positions_m_upd_ref,
    selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref,
    model_spec_theme_level = model_spec_theme_level
  )

  #Get m_upd_ref backtest data and brm formula
  selected_backtest_returns_corrected_positions_m_upd_ref <- lmer_model_inputs_list$selected_backtest_returns_corrected_positions_m_upd_ref
  lmer_formula <- lmer_model_inputs_list$formula

  #Fit lmer model
  ########################
  lmer_model <- lmerTest::lmer(formula = lmer_formula,
                               data = selected_backtest_returns_corrected_positions_m_upd_ref,
                               control = lme4::lmerControl(optimizer = lmer_optimizer), REML = lmer_optimization_objective
                               )

  ########################

  #Summarize metrics
  ########################
  if(!is.null(signal_universe_m_d_ref)){

    pooled_CAPM_metrics_m_d_ref <- summarize_lmer_model(
      ###Lmer Model
      lmer_model = lmer_model,
      #Data
      signal_universe_m_d_ref = signal_universe_m_d_ref,
      #Theme
      selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref, model_spec_theme_level = model_spec_theme_level,
      #How to calc p-value
      hierarchical_p_value_method = hierarchical_p_value_method
      )

  }
  ########################

  #Final Obj
  frequentist_fit_results_list <- list(
    lmer_model = lmer_model,
    pooled_CAPM_metrics_m_d_ref = if(is.null(signal_universe_m_d_ref)) NULL else pooled_CAPM_metrics_m_d_ref
  )

  return(frequentist_fit_results_list)

}

