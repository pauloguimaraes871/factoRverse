#' Fit a Bayesian Model for a Given Theme
#'
#' This function fits a Bayesian regression model to the backtest returns data for a specified theme using the `brms` package. It uses the priors defined for the theme and includes both fixed and random effects in the model.
#'
#' @param theme A character string specifying the theme for which the Bayesian model is to be fitted. This theme is used to select the relevant signals and priors.
#' @param groups A data frame that contains information about the signals and their associated themes. It should have at least two columns: `tickers` (the signals) and `theme` (the theme associated with each signal).
#' @param backtest_returns A data frame or matrix containing backtest returns data for the signals. The rows represent different time points, and the columns represent different signals.
#' @param priors A named list of priors for the Bayesian model. The list should contain prior distributions for each theme, as specified in the `brms` package format.
#'
#' @return An object of class `brmsfit` representing the fitted Bayesian regression model. This object contains the results of the Bayesian analysis, including estimates of the model parameters and diagnostics.
#'
fit_bayesian_model <- function(theme, signals_groups_m_d_ref, selected_signals_backtest_returns_upd_ref, selected_benchmark_returns_upd_ref_vector, elected_priors_list){

  #Prepare objects
  ######################
  ##Get which signals belong to current theme
  signals_in_current_theme <- unique(signals_groups_m_d_ref$tickers[which(signals_groups_m_d_ref$theme == theme)])
    ###Subset backtest returns accordingly
    selected_signals_backtest_returns_upd_ref_signals_in_current_theme <- as.data.frame(selected_signals_backtest_returns_upd_ref[,signals_in_current_theme])
  ##Adjust
    ##Melt
    selected_signals_backtest_returns_upd_ref_signals_in_current_theme <- reshape2::melt(selected_signals_backtest_returns_upd_ref_signals_in_current_theme)
    ##Add bench returns
    selected_signals_backtest_returns_upd_ref_signals_in_current_theme$bench_return <- selected_benchmark_returns_upd_ref_vector #R will recycle
    ###Rename
    colnames(selected_signals_backtest_returns_upd_ref_signals_in_current_theme)[1:2] <- c("signal", "active_return")
    ##Adjust signal labels
    selected_signals_backtest_returns_upd_ref_signals_in_current_theme$signal <- rep(signals_in_current_theme, each = length(selected_benchmark_returns_upd_ref_vector))

    #######################

    #Fit brm
    ########################
    brm_model <- brms::brm(
      brms::brmsformula(
        #Mean-level formula
        active_return ~ bench_return + (bench_return | signal),
        #Sigma formula
        sigma ~ 1 + (1 | signal)),
      #Priors
      prior = elected_priors_list[[theme]],
      data = selected_signals_backtest_returns_upd_ref_signals_in_current_theme
    )
    ########################
    return(brm_model)
}
