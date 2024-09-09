#' Summarize Posterior Draws for Signal Universe
#'
#' This function computes various posterior summary statistics for a given set of signals, themes, and posterior draws.
#' It updates the `signal_universe_m_d_ref` data frame with posterior statistics including alphas, betas, sigmas, and other metrics.
#'
#' @param signal_universe_m_d_ref A data frame containing the signal universe data with tickers and additional columns that will be updated.
#' @param posteriors_draws A named list where each element is a data frame containing posterior draws for a specific theme.
#' @param groups A data frame that maps signals groups to themes
#'
#' @return The `signal_universe_m_d_ref` data frame is updated in place with posterior summary statistics.
#' @details The function performs the following operations for each theme:
#' \itemize{
#'   \item Computes and updates the posterior overall alpha and individual alpha for each signal.
#'   \item Computes and updates the probability of the (positive) direction for posterior alphas.
#'   \item Computes and updates the posterior overall beta and individual beta for each signal.
#'   \item Computes and updates the posterior sigma for each signal.
#'   \item Computes and updates posterior metrics such as active return, tracking error, and information ratio (IR).
#'   \item Computes and updates additional performance metrics like Appraisal Ratio (AP) and Treynor ratio.
#' }
#'
summarize_posterior_draws <- function(signal_universe_m_d_ref, posteriors_draws_list, signals_groups_m_d_ref, selected_benchmark_returns_upd_ref_vector){

  #Get themes
  themes <- names(posteriors_draws_list)
  frequentist_metrics <- colnames(signal_universe_m_d_ref)[-1]
  #Loop through all signals
  for(i in 1:length(posteriors_draws_list)){

    #Get references of signals
    signals_in_current_theme <- unique(signals_groups_m_d_ref$tickers[which(signals_groups_m_d_ref$theme == themes[i])])
    positions_of_signals_in_current_theme <- which(signal_universe_m_d_ref$tickers %in% signals_in_current_theme)
    #Get draws from current theme
    current_theme_posteriors_draws <- posteriors_draws_list[[i]]

    ##Alpha
    ###################
    #Get random and fixed effects
    fixed_effect_intercept <- current_theme_posteriors_draws$b_Intercept
    random_effects_intercept <- current_theme_posteriors_draws[, paste0("r_signal[", signal_universe_m_d_ref$tickers[positions_of_signals_in_current_theme], ",Intercept]")]
    both_effects_intercept <- data.frame(random_effects_intercept + fixed_effect_intercept)

    ##Posterior Overall Alpha
    signal_universe_m_d_ref[positions_of_signals_in_current_theme, "posterior_overall_alpha"] <- median(fixed_effect_intercept)

    ##Posterior Individual Alpha
    signal_universe_m_d_ref[positions_of_signals_in_current_theme, "posterior_alpha"] <- apply(both_effects_intercept, 2, function(x) median(x))

    ##Probability of (Positive) Direction of Posterior Overall Alpha
    signal_universe_m_d_ref[positions_of_signals_in_current_theme, "pd_overall_alpha"] <- mean(fixed_effect_intercept > 0)

    ##Probability of Direction Posterior Individual Alpha
    signal_universe_m_d_ref[positions_of_signals_in_current_theme, "pd_alpha"] <- mean(both_effects_intercept > 0)
    #####################

    ##Beta
    ###################
    #Get random and fixed effects
    fixed_effect_beta <- current_theme_posteriors_draws$b_bench_return
    random_effects_beta <- current_theme_posteriors_draws[, paste0("r_signal[", signal_universe_m_d_ref$tickers[positions_of_signals_in_current_theme], ",bench_return]")]
    both_effects_beta <- data.frame(random_effects_beta + fixed_effect_beta)

    ##Posterior Overall Beta
    signal_universe_m_d_ref[positions_of_signals_in_current_theme, "posterior_overall_beta"] <- median(fixed_effect_beta)

    ##Posterior Individual Beta
    signal_universe_m_d_ref[positions_of_signals_in_current_theme, "posterior_beta"] <- apply(both_effects_beta, 2, function(x) median(x))

    #####################

    ##Sigma
    ###################
    #Get random and fixed effects
    fixed_effect_sigma <- current_theme_posteriors_draws$b_sigma_Intercept
    random_effects_sigma <- current_theme_posteriors_draws[, paste0("r_signal__sigma[", signal_universe_m_d_ref$tickers[positions_of_signals_in_current_theme], ",Intercept]")]
    both_effects_sigma <- data.frame(exp(random_effects_sigma + fixed_effect_sigma))

    ##Posterior Individual sigma
    signal_universe_m_d_ref[positions_of_signals_in_current_theme, "posterior_sigma"] <- apply(both_effects_sigma, 2, function(x) median(x))

    #####################

    ##Other metrics
    ###Posterior Active Return, Tracking Error and IR
    for(i in 1:ncol(both_effects_beta)){
      current_signal <- signal_universe_m_d_ref$tickers[positions_of_signals_in_current_theme][i]
      # Multiply the current beta column by each benchmark return and add alpha (result will be (Beta * Ibov) + Alpha)
      active_returns_df <- as.data.frame(sapply(selected_benchmark_returns_upd_ref_vector, function(x) both_effects_beta[,i] * x + both_effects_intercept[,i]))
      #Get the mean active return and te for each draw
      mean_active_returns_df <- apply(active_returns_df, 1, function(x) mean(x))
      te_df <- apply(active_returns_df, 1, function(x) sd(x))
      #Summarize with median
      median_mean_active_return <- median(mean_active_returns_df)
      median_te <- median(te_df)

      #Place in df
      signal_universe_m_d_ref[which(signal_universe_m_d_ref$tickers == current_signal), "posterior_mean_active_return"] <- median_mean_active_return
      signal_universe_m_d_ref[which(signal_universe_m_d_ref$tickers == current_signal), "posterior_tracking_error"] <- median_te
      signal_universe_m_d_ref[which(signal_universe_m_d_ref$tickers == current_signal), "posterior_IR"] <- median_mean_active_return/median_te
    }

    ###AP
    signal_universe_m_d_ref[positions_of_signals_in_current_theme, "posterior_AP"] <-
      signal_universe_m_d_ref[positions_of_signals_in_current_theme, "posterior_alpha"]/signal_universe_m_d_ref[positions_of_signals_in_current_theme, "posterior_sigma"]

    ###Treynor
    signal_universe_m_d_ref[positions_of_signals_in_current_theme, "posterior_treynor"] <-
      signal_universe_m_d_ref[positions_of_signals_in_current_theme, "posterior_mean_active_return"]/signal_universe_m_d_ref[positions_of_signals_in_current_theme, "posterior_beta"]

  }

  #Reorder

    ##New metrics
    bayesian_metrics <- setdiff(colnames(signal_universe_m_d_ref)[-1], frequentist_metrics)
    ordered_metrics <- c(frequentist_metrics, "posterior_mean_active_return", "posterior_sigma", "posterior_IR", "posterior_overall_alpha",
                         "posterior_alpha", "posterior_AP", "posterior_overall_beta", "posterior_beta",
                         "posterior_treynor", "pd_overall_alpha", "pd_alpha")
    signal_universe_m_d_ref <- signal_universe_m_d_ref[, c("id", ordered_metrics)]
    rownames(signal_universe_m_d_ref) <- NULL

  return(signal_universe_m_d_ref)


}
