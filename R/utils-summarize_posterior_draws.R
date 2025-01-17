#' Summarize Posterior Draws for Signal Universe
#'
#' This function computes various posterior summary statistics for a given set of signals, themes, and posterior draws.
#' It updates the `signal_universe_m_d_ref` data frame with posterior statistics including alphas, betas, sigmas, and other metrics.
#'
#' @param brm_model A bayesian model fit with `brms::brm`.
#' @param signal_universe_m_d_ref A dataframe with tickers, is_eligible and final_signal columns
#' @param selected_signal_themes_m_d_ref A (meta) data frame with id, tickers ("signals") and dates column contemplating all signals in `signals_m_df` and a "theme" column providing group membership for each signal, which is needed
#' for defining clusters in bayesian hierarchical model. It should contain data only for current date.
#'
#' @param model_spec_theme_level A character string specifying the desired Bayesian model structure.
#'   Options include:
#'   - `"random_intercept_fixed_slope"`: Includes random effects for the intercept at the theme level.
#'   - `"theme_specific_intercept_fixed_slope"`: Uses fixed intercepts for each theme.
#'   - `"theme_specific_intercept_theme_specific_slope"`: Includes fixed intercepts and slopes for each theme.
#'   - `"fixed_intercept_fixed_slope"`: Omits theme-level intercepts but includes random effects at the theme:signal level.
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
summarize_posteriors_draws <- function(brm_model, signal_universe_m_d_ref = NULL, selected_signal_themes_m_d_ref, model_spec_theme_level){

  #Get frequentist metrics
  frequentist_metrics <- colnames(signal_universe_m_d_ref)[-c(1:3)]

  #Get tidy posterior draws from brm_model
  #####################
  vars <- tidybayes::get_variables(brm_model) #Get all variables in model
  theme_tickers_key <- selected_signal_themes_m_d_ref %>% dplyr::mutate(tickers = paste0(theme, "_", tickers)) %>% dplyr::select(tickers, theme) #Create key

    ##Alpha (Beta_0)
    ################
      ###Random Effects at Theme Level
      if(model_spec_theme_level == "random_intercept_fixed_slope"){
        tidy_posterior_draws_intercept <- brm_model %>%
          tidybayes::spread_draws(b_Intercept, r_theme[theme, theme_term], `r_theme:tickers`[tickers, tickers_term]) %>%
          dplyr::filter(stringr::str_detect(tickers, theme) & theme_term == "Intercept" & tickers_term == "Intercept") %>% # Keep only rows where tickers contain the theme
          dplyr::mutate(
            #Posterior Theme
            posterior_theme_alpha = b_Intercept + r_theme, #Posterior Theme Alpha and Beta = Fixed Effect + Random Effect at Theme Level
            #Posterior Individual
            posterior_individual_alpha = b_Intercept + r_theme + `r_theme:tickers` #Posterior Individual Alpha and Beta = Fixed Effect + Random Effect at Theme Level + Random Effect at Signal Level
          ) %>%
          dplyr::rename(r_tickers_intercept = `r_theme:tickers`) #Rename
      }

      ###Fixed Intercepts for Each Theme
      ###Select variables starting with 'b_' and not containing 'market_factor_proxy'
      if(model_spec_theme_level %in% c("theme_specific_intercept_fixed_slope", "theme_specific_intercept_theme_specific_slope")){
        b_variables <- vars[grepl("^b_", vars) & !grepl("market_factor_proxy", vars)] #Select variables starting with 'b_' and not containing 'market_factor_proxy'

        tidy_posterior_draws_intercept <- brm_model %>%
          tidybayes::spread_draws(c(!!!rlang::syms(b_variables)), `r_theme:tickers`[tickers, tickers_term]) %>%
          dplyr::filter(tickers_term == "Intercept") %>% # Keep only rows where tickers contain the theme
          dplyr::left_join(theme_tickers_key, by = "tickers") %>% #Join with theme_tickers_key to get theme_tickers
          tidyr::pivot_longer(cols = dplyr::all_of(b_variables), names_to = "fixed_effect_name", values_to = "fixed_effect_value") %>% #Pivot long
          dplyr::mutate(theme_fixed_effect = sub("^b_theme", "", fixed_effect_name)) %>% #Extract theme from fixed_effect_name
          dplyr::filter(theme_fixed_effect == theme) %>% #Keep only rows where theme_fixed == theme
          dplyr::mutate(
            #Posterior Theme
            posterior_theme_alpha = fixed_effect_value, #Posterior Theme Alpha and Beta = Fixed Effect + Random Effect at Theme Level
            #Posterior Individual
            posterior_individual_alpha = fixed_effect_value + `r_theme:tickers` #Posterior Individual Alpha and Beta = Fixed Effect + Random Effect at Theme Level + Random Effect at Signal Level
          ) %>%
          dplyr::select(-theme, -theme_fixed_effect, -fixed_effect_value, -fixed_effect_name) %>% #Remove intermediate colms
          dplyr::rename(r_tickers_intercept = `r_theme:tickers`) #Rename
      }

      ###No effects on theme level
      if(model_spec_theme_level == "fixed_intercept_fixed_slope"){
        tidy_posterior_draws_intercept <- brm_model %>%
          tidybayes::spread_draws(b_Intercept, `r_theme:tickers`[tickers, tickers_term]) %>%
          dplyr::left_join(theme_tickers_key, by = "tickers") %>% #Join with theme_tickers_key to get theme_tickers
          dplyr::filter(stringr::str_detect(tickers, theme) & tickers_term == "Intercept") %>% # Keep only rows where tickers contain the theme
          dplyr::mutate(
            #Posterior Theme
            posterior_theme_alpha = b_Intercept, #Posterior Theme Alpha and Beta = Fixed Effect + Random Effect at Theme Level
            #Posterior Individual
            posterior_individual_alpha = b_Intercept + `r_theme:tickers` #Posterior Individual Alpha and Beta = Fixed Effect + Random Effect at Theme Level + Random Effect at Signal Level
          ) %>%
          dplyr::select(-theme) %>% #Remove intermediate colms
          dplyr::rename(r_tickers_intercept = `r_theme:tickers`) #Rename
      }

      ###Summarize median and 89% CI
      tidy_posterior_draws_intercept_summary <- tidy_posterior_draws_intercept %>%
        #Compute CIs with 89% width (more stable)
        tidybayes::median_qi(.width = 0.89) %>% #Quantile interval
        #Select relevant columns
        dplyr::select(tickers,
                      posterior_theme_alpha, posterior_theme_alpha.lower, posterior_theme_alpha.upper,
                      posterior_individual_alpha, posterior_individual_alpha.lower, posterior_individual_alpha.upper) %>% as.data.frame() #get df

    ################

    ##Slope (Beta_1)
    ################
      ###Fixed Slopes for Each Theme
      if(model_spec_theme_level %in% c("theme_specific_intercept_theme_specific_slope")){
        b_variables <- vars[grepl("^b_", vars) & grepl("market_factor_proxy", vars)] #Select variables starting with 'b_' and containing 'market_factor_proxy'

        tidy_posterior_draws_slope <- brm_model %>%
          tidybayes::spread_draws(c(!!!rlang::syms(b_variables)), `r_theme:tickers`[tickers, tickers_term]) %>%
          dplyr::filter(tickers_term == "market_factor_proxy") %>% # Keep only rows where tickers contain the theme
          dplyr::left_join(theme_tickers_key, by = "tickers") %>% #Join with theme_tickers_key to get theme_tickers
          tidyr::pivot_longer(cols = dplyr::all_of(b_variables), names_to = "fixed_effect_name", values_to = "fixed_effect_value") %>% #Pivot long
          dplyr::mutate(theme_fixed_effect = sub("^b_theme([^:]+):.*$", "\\1", fixed_effect_name)) %>% #Extract theme from fixed_effect_name
          dplyr::filter(theme_fixed_effect == theme) %>% #Keep only rows where theme_fixed == theme
          dplyr::mutate(
            #Posterior Theme
            posterior_theme_beta = fixed_effect_value, #Posterior Theme Alpha and Beta = Fixed Effect + Random Effect at Theme Level
            #Posterior Individual
            posterior_individual_beta = fixed_effect_value + `r_theme:tickers` #Posterior Individual Alpha and Beta = Fixed Effect + Random Effect at Theme Level + Random Effect at Signal Level
          ) %>%
          dplyr::select(-theme, -theme_fixed_effect, -fixed_effect_value, -fixed_effect_name) %>% #Remove intermediate colms
          dplyr::rename(r_tickers_slope = `r_theme:tickers`) #Rename
      }

      ###No effects on theme level
      if(model_spec_theme_level %in% c("random_intercept_fixed_slope", "theme_specific_intercept_fixed_slope", "fixed_intercept_fixed_slope")){
      tidy_posterior_draws_slope <- brm_model %>%
        tidybayes::spread_draws(b_market_factor_proxy, `r_theme:tickers`[tickers, tickers_term]) %>%
        dplyr::filter(tickers_term == "market_factor_proxy") %>% # Keep only rows where tickers contain the theme
        dplyr::mutate(
          #Posterior Theme
          posterior_theme_beta = b_market_factor_proxy, #Posterior Theme Alpha and Beta = Fixed Effect + Random Effect at Theme Level
          #Posterior Individual
          posterior_individual_beta = b_market_factor_proxy + `r_theme:tickers` #Posterior Individual Alpha and Beta = Fixed Effect + Random Effect at Theme Level + Random Effect at Signal Level
        ) %>%
        dplyr::rename(r_tickers_slope = `r_theme:tickers`) #Rename
      }

      #Summarize median and 89% CI
      tidy_posterior_draws_slope_summary <- tidy_posterior_draws_slope %>%
        #Compute CIs with 89% width (more stable)
        tidybayes::median_qi(.width = 0.89) %>% #Quantile interval
        #Select relevant columns
        dplyr::select(tickers,
                      posterior_theme_beta, posterior_theme_beta.lower, posterior_theme_beta.upper,
                      posterior_individual_beta, posterior_individual_beta.lower, posterior_individual_beta.upper) %>% as.data.frame() #get df

    ################

    ##Sigma
    ################
      ##Random Effects at Theme Level
      if(model_spec_theme_level == "random_intercept_fixed_slope"){
      tidy_posterior_draws_sd <- brm_model %>%
        tidybayes::spread_draws(
          sd_theme__Intercept, #sigma(u_0j)
          `sd_theme:tickers__Intercept`, #sigma(v_0k(j))
          `sd_theme:tickers__market_factor_proxy`, #sigma(v_1k(j))
          sigma, #sigma(e_ijk)
          `cor_theme:tickers__Intercept__market_factor_proxy`
        ) %>% dplyr::rename(
          posterior_r_theme_alpha = sd_theme__Intercept,
          posterior_r_tickers_alpha = `sd_theme:tickers__Intercept`,
          posterior_r_tickers_beta = `sd_theme:tickers__market_factor_proxy`,
          posterior_sigma = sigma,
          posterior_cor_r_alpha_beta = `cor_theme:tickers__Intercept__market_factor_proxy`
        )
      }

      ##No Random Effects at Theme Level
      if(model_spec_theme_level %in% c("theme_specific_intercept_fixed_slope", "theme_specific_intercept_theme_specific_slope", "fixed_intercept_fixed_slope")){
        tidy_posterior_draws_sd <- brm_model %>%
          tidybayes::spread_draws(
            `sd_theme:tickers__Intercept`, #sigma(v_0k(j))
            `sd_theme:tickers__market_factor_proxy`, #sigma(v_1k(j))
            sigma, #sigma(e_ijk)
            `cor_theme:tickers__Intercept__market_factor_proxy`
          ) %>% dplyr::rename(
            posterior_r_tickers_alpha = `sd_theme:tickers__Intercept`,
            posterior_r_tickers_beta = `sd_theme:tickers__market_factor_proxy`,
            posterior_sigma = sigma,
            posterior_cor_r_alpha_beta = `cor_theme:tickers__Intercept__market_factor_proxy`
          )
      }

      #Summarize median and 89% CI
      tidy_posterior_draws_sd_summary <- tidy_posterior_draws_sd %>%
        #Compute CIs with 89% width (more stable)
        tidybayes::median_qi(.width = 0.89) %>%  #Quantile interval
        #Select relevant columns
        dplyr::select(-.width, -.point, -.interval) %>% as.data.frame() #get df

      ################

      ##Expectation of Posterior Predictive (E(active_returns))
      ################
      tidy_posterior_epred_draws <- brm_model$data %>% tidybayes::add_epred_draws(brm_model)

      #Summarize median and 89% CI
      tidy_posterior_epred_draws_summary <- tidy_posterior_epred_draws %>%
        #Compute CIs with 89% width (more stable)
        tidybayes::median_qi(.width = 0.89) %>% #Quantile interval
        #Select relevant columns
        dplyr::select(-.width, -.point, -.interval) %>% as.data.frame() #get df

      ################

      ##Posterior Predictive (active_returns ~ Normal(E(active_returns), sigma))
      ################
      tidy_posterior_predicted_draws <- brm_model$data %>% tidybayes::add_predicted_draws(brm_model)

      #Summarize median and 89% CI
      tidy_posterior_predicted_draws_summary <- tidy_posterior_predicted_draws %>%
        #Compute CIs with 89% width (more stable)
        tidybayes::median_qi(.width = 0.89) %>% #Quantile interval
        #Select relevant columns
        dplyr::select(-.width, -.point, -.interval) %>% as.data.frame() #get df

      ################

      #####################

      #Add posterior results to signal_universe_m_d_ref
      if(!is.null(signal_universe_m_d_ref)){
      #####################
      theme_tickers_key <- selected_signal_themes_m_d_ref %>% dplyr::mutate(theme_tickers = paste0(theme, "_", tickers)) %>% dplyr::select(tickers, theme_tickers) #Redefine key
      signal_universe_m_d_ref <- signal_universe_m_d_ref %>% dplyr::left_join(theme_tickers_key, by = "tickers") #Add key to signal_universe_m_d_ref

      ##Intercept Metrics
      posterior_intercept_metrics <- tidy_posterior_draws_intercept %>%
        dplyr::group_by(tickers) %>%
        dplyr::summarise(
          pd_theme_alpha = mean(posterior_theme_alpha > 0), #Probability of direction for alpha at theme level
          posterior_theme_alpha = median(posterior_theme_alpha), #Median of alpha at theme level
          pd_alpha = mean(posterior_individual_alpha > 0), #Probability of direction for alpha at individual level
          posterior_alpha_t_stat = mean(posterior_individual_alpha)/sd(posterior_individual_alpha), #T-stat of alpha at individual level
          posterior_alpha_se = sd(posterior_individual_alpha), #Standard error of alpha at individual level
          posterior_individual_alpha = median(posterior_individual_alpha) #Median of alpha at individual level
        )
        ###Add to signal_universe
        signal_universe_m_d_ref <- signal_universe_m_d_ref %>% dplyr::left_join(posterior_intercept_metrics, by = c("theme_tickers" = "tickers"))

      ##Slope Metrics
      posterior_slope_metrics <- tidy_posterior_draws_slope %>%
        dplyr::group_by(tickers) %>%
        dplyr::summarise(
          posterior_theme_beta = median(posterior_theme_beta), #Median of beta at theme level
          posterior_individual_beta = median(posterior_individual_beta) #Median of beta at individual level
        )
        ###Add to signal_universe
        signal_universe_m_d_ref <- signal_universe_m_d_ref %>% dplyr::left_join(posterior_slope_metrics, by = c("theme_tickers" = "tickers"))

      ##Add specific risk
      signal_universe_m_d_ref[, "posterior_specific_risk"] <- median(tidy_posterior_draws_sd$posterior_sigma)

      ##Get other metrics
      posterior_performance_metrics <- tidy_posterior_predicted_draws %>%
        dplyr::group_by(tickers) %>%
        dplyr::summarise(
          posterior_geom_mean_ret = PerformanceAnalytics::mean.geometric(.prediction/100)*100 #Mean Geometric Return
        )

        ###Add to signal_universe
        signal_universe_m_d_ref <- signal_universe_m_d_ref %>% dplyr::left_join(posterior_performance_metrics, by = c("tickers")) %>%
          dplyr::mutate(posterior_treynor_ratio = posterior_geom_mean_ret/posterior_individual_beta, #Posterior Treynor Ratio
                        posterior_appraisal_ratio = posterior_individual_alpha/posterior_specific_risk #Posterior Appraisal Ratio
          )


      #####################

      #Re-order
      ##New metrics
      bayesian_metrics <- c("posterior_theme_alpha", "posterior_individual_alpha", "posterior_alpha_se", "posterior_theme_beta", "posterior_individual_beta",
                            "posterior_specific_risk", "posterior_alpha_t_stat", "posterior_treynor_ratio", "posterior_appraisal_ratio", "pd_theme_alpha", "pd_alpha")

      ordered_metrics <- c("id", "tickers", "dates", frequentist_metrics, bayesian_metrics)

      signal_universe_m_d_ref <- signal_universe_m_d_ref %>% dplyr::select(dplyr::all_of(ordered_metrics))
      rownames(signal_universe_m_d_ref) <- NULL

        ###Check for any resulting NAs
        if (any(is.na(select(signal_universe_m_d_ref, dplyr::all_of(bayesian_metrics))))) {
          stop("NA values detected in the bayesian_metrics columns.")
        }


    #Create result object
    posteriors_results_list <- list(
      signal_universe_m_d_ref = signal_universe_m_d_ref,
      posterior_draws_summaries = list(
        intercept_summary = tidy_posterior_draws_intercept_summary,
        slope_summary = tidy_posterior_draws_slope_summary,
        sd_summary = tidy_posterior_draws_sd_summary,
        epred_summary = tidy_posterior_epred_draws_summary,
        predicted_summary = tidy_posterior_predicted_draws_summary
      )
    )

  }  else  {

    #Create result object in case of signal_universe is NULL
    posteriors_results_list <- list(
      tidy_posterior_draws_intercept = tidy_posterior_draws_intercept,
      tidy_posterior_draws_slope = tidy_posterior_draws_slope,
      tidy_posterior_draws_sd = tidy_posterior_draws_sd,
      tidy_posterior_epred_draws = tidy_posterior_epred_draws,
      tidy_posterior_predicted_draws = tidy_posterior_predicted_draws
    )

  }

  #Return
  return(posteriors_results_list)

  }




