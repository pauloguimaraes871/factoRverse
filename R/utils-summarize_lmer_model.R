#' Summarize Posterior Draws for Signal Universe
#'
#' This function computes various posterior summary statistics for a given set of signals, themes, and posterior draws.
#' It updates the `signal_universe_m_d_ref` data frame with posterior statistics including alphas, betas, sigmas, and other metrics.
#'
#' @param brm_model A bayesian model fit with `brms::brm`.
#' @param signal_universe_m_d_ref A dataframe with tickers, is_eligible and final_signal columns
#' @param selected_signal_themes_m_d_ref A (meta) data frame with id, tickers ("signals") and dates column contemplating all signals in `signals_m_df` and a "theme" column providing group membership for each signal, which is needed
#' for defining clusters in bayesian hierarchical model. It should contain data only for current date.
#' @param model_spec_theme_level A character string specifying the desired model structure.
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

summarize_lmer_model <- function(lmer_model, signal_universe_m_d_ref = NULL, selected_signal_themes_m_d_ref, model_spec_theme_level,
                                 hierarchical_p_value_method){

  #Check inputs
  ######################

  if(!hierarchical_p_value_method %in% c("Satterthwaite", "Kenward-Roger", "lme4")){
    stop("Invalid hierarchical p-value method. Please choose from 'Satterthwaite', 'Kenward-Roger', or 'lme4'.")
  }

  ######################

  ####Get coefs from lmer mod
  #############################
  lmer_model_coefs <- summary(lmer_model, ddf = hierarchical_p_value_method)$coefficients

  ##Get fixed effects
  #################
    fixed_effects_df <- lmer_model_coefs %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "coef") %>%
      dplyr::select(-df, -`t value`)
    colnames(fixed_effects_df) <- c("coef", "fixed_effect", "fixed_effect_sd", "fixed_effect_p_values")

      ####Adjust theme-column
      if(model_spec_theme_level %in% c("theme_specific_intercept_fixed_slope", "theme_specific_intercept_theme_specific_slope")){
        fixed_effects_df <- fixed_effects_df %>% dplyr::mutate(coef = stringr::str_remove(coef, "theme")) #Remove "theme" from theme column
      }

    ###Split into alpha and beta
    ###not theme_specific_intercept_theme_specific_slope
    if(model_spec_theme_level != "theme_specific_intercept_theme_specific_slope"){

      ###Alpha
      alpha_fixed_effects_df <- fixed_effects_df %>% dplyr::filter(coef != "market_factor_proxy") %>% #Filter intercept
        ####Alpha Effects
        dplyr::rename(alpha_fixed_effect = fixed_effect,
                      alpha_fixed_effect_sd = fixed_effect_sd,
                      alpha_p_value = fixed_effect_p_values) #Two-sided -> one-sided

      ###Beta
      beta_fixed_effects_df <- fixed_effects_df %>% dplyr::filter(coef == "market_factor_proxy") %>% #Filter beta
        ####Beta Effects
        dplyr::rename(beta_fixed_effect = fixed_effect,
                      beta_fixed_effect_sd = fixed_effect_sd)%>%
        dplyr::select(-coef) %>%
        dplyr::select(-fixed_effect_p_values)

    } else {
      ###theme_specific_intercept_theme_specific_slope
      ###Alpha
      alpha_fixed_effects_df <- fixed_effects_df %>% dplyr::filter(!stringr::str_detect(coef, ":market_factor_proxy")) %>% #Filter intercept
        ####Alpha Effects
        dplyr::rename(alpha_fixed_effect = fixed_effect,
                      alpha_fixed_effect_sd = fixed_effect_sd,
                      alpha_p_value = fixed_effect_p_values) #Two-sided -> one-sided

      ###Beta
      beta_fixed_effects_df <- fixed_effects_df %>% dplyr::filter(stringr::str_detect(coef, ":market_factor_proxy")) %>% #Filter beta
        dplyr::mutate(coef = stringr::str_remove(coef, ":market_factor_proxy")) %>%
        ####Beta Effects
        dplyr::rename(beta_fixed_effect = fixed_effect,
                      beta_fixed_effect_sd = fixed_effect_sd) %>%
        dplyr::select(-fixed_effect_p_values)
    }


  #################

  ##Get random effects
  #################
    random_effects_df <- lme4::ranef(lmer_model)$`theme:tickers` %>%
      as.data.frame() %>% #Convert to df
      tibble::rownames_to_column(var = "tickers") %>% #Convert rownames to column
      dplyr::mutate(tickers = stringr::str_remove(tickers, ".*:")) #Remove "theme:"  from tickers column
    colnames(random_effects_df)[c(2:3)] <- c("alpha_random_effect", "beta_random_effect")

  if(model_spec_theme_level == "random_intercept_fixed_slope"){
    random_effects_theme_level_df <- lme4::ranef(lmer_model)$theme %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "theme")
    colnames(random_effects_theme_level_df)[2] <- c("theme_alpha_random_effect")
  }

  #################

  ##Get sigma and geometric return
  #################
    y_sigma <- sigma(lmer_model)
    y_mean_geom_df <- lmer_model@frame %>% dplyr::group_by(tickers) %>% dplyr::summarize(mean_geom = PerformanceAnalytics::mean.geometric(return/100)*100)
    y_mean_geom_df <- dplyr::left_join(dplyr::select(selected_signal_themes_m_d_ref, tickers), y_mean_geom_df, by = "tickers") #Re-order

  #################

  ##Join everything according to model spec
    ########################################
    ###random_intercept_fixed_slope
    if(model_spec_theme_level == "random_intercept_fixed_slope"){

      ##Start to join
      pooled_CAPM_metrics_m_d_ref <- selected_signal_themes_m_d_ref %>%
        dplyr::mutate(alpha_fixed_effect =  alpha_fixed_effects_df$alpha_fixed_effect,
                      alpha_fixed_effect_sd = alpha_fixed_effects_df$alpha_fixed_effect_sd,
                      alpha_p_value = alpha_fixed_effects_df$alpha_p_value) %>% #Add global alpha
        dplyr::mutate(beta_fixed_effect = beta_fixed_effects_df$beta_fixed_effect,
                      beta_fixed_effect_sd = beta_fixed_effects_df$beta_fixed_effect_sd) %>% #Add beta
        dplyr::left_join(random_effects_theme_level_df, by = "theme") %>% #Join with random_effects_theme_level by theme
        dplyr::left_join(random_effects_df, by = "tickers") %>% #Join with random_effects by tickers
        dplyr::mutate(
          #Alpha
          theme_alpha = alpha_fixed_effect + theme_alpha_random_effect,
          individual_alpha = alpha_fixed_effect + theme_alpha_random_effect + alpha_random_effect,
          alpha_se = alpha_fixed_effect_sd,
          #Beta
          theme_beta = beta_fixed_effect, individual_beta = beta_fixed_effect + beta_random_effect,
          #Specific Risk
          specific_risk = y_sigma,
          #Other
          alpha_t_stat = individual_alpha/alpha_se,
          treynor_ratio = y_mean_geom_df$mean_geom/individual_beta,
          appraisal_ratio = individual_alpha/specific_risk,
          #P-value
          p_value = alpha_p_value/2
          ) %>%
        dplyr::select(-theme, -alpha_p_value , -alpha_fixed_effect, -alpha_fixed_effect_sd, -beta_fixed_effect, -beta_fixed_effect_sd,
                      -theme_alpha_random_effect, -alpha_random_effect, -beta_random_effect)

    }
    ###theme_specific_intercept_fixed_slope
    if(model_spec_theme_level == "theme_specific_intercept_fixed_slope"){

      ##Adjust colnames
      colnames(alpha_fixed_effects_df)[1] <- "theme"

      ##Start to join
      pooled_CAPM_metrics_m_d_ref <- selected_signal_themes_m_d_ref %>%
        dplyr::left_join(alpha_fixed_effects_df, by = "theme") %>% #Join with alpha by theme
        dplyr::mutate(beta_fixed_effect = beta_fixed_effects_df$beta_fixed_effect,
                      beta_fixed_effect_sd = beta_fixed_effects_df$beta_fixed_effect_sd) %>%
        dplyr::left_join(random_effects_df, by = "tickers") %>% #Join with random_effects by tickers
        dplyr::mutate(
          #Alpha
          theme_alpha = alpha_fixed_effect, individual_alpha = alpha_fixed_effect + alpha_random_effect, alpha_se = alpha_fixed_effect_sd,
          #Beta
          theme_beta = beta_fixed_effect, individual_beta = beta_fixed_effect + beta_random_effect,
          #Specific Risk
          specific_risk = y_sigma,
          #Other
          alpha_t_stat = individual_alpha/alpha_se,
          treynor_ratio = y_mean_geom_df$mean_geom/individual_beta,
          appraisal_ratio = individual_alpha/specific_risk,
          #P-value
          p_value = alpha_p_value/2
          ) %>%
        dplyr::select(-theme, -alpha_p_value, -alpha_fixed_effect, -alpha_fixed_effect_sd, -beta_fixed_effect, -beta_fixed_effect_sd,
                      -alpha_random_effect, -beta_random_effect)

    }
    ###theme_specific_intercept_theme_specific_slope
    if(model_spec_theme_level == "theme_specific_intercept_theme_specific_slope"){

        ##Adjust colnames
        colnames(alpha_fixed_effects_df)[1] <- "theme"
        colnames(beta_fixed_effects_df)[1] <- "theme"

        ##Start to join
        pooled_CAPM_metrics_m_d_ref <- selected_signal_themes_m_d_ref %>%
          dplyr::left_join(alpha_fixed_effects_df, by = "theme") %>% #Join with alpha by theme
          dplyr::left_join(beta_fixed_effects_df, by = "theme") %>%
          dplyr::left_join(random_effects_df, by = "tickers") %>% #Join with random_effects by tickers
          dplyr::mutate(
            #Alpha
            theme_alpha = alpha_fixed_effect, individual_alpha = alpha_fixed_effect + alpha_random_effect,
            alpha_se = alpha_fixed_effect_sd,
            #Beta
            theme_beta = beta_fixed_effect, individual_beta = beta_fixed_effect + beta_random_effect,
            #Specific Risk
            specific_risk = y_sigma,
            #Other
            alpha_t_stat = individual_alpha/alpha_se,
            treynor_ratio = y_mean_geom_df$mean_geom/individual_beta,
            appraisal_ratio = individual_alpha/specific_risk,
            p_value = alpha_p_value/2) %>%
          dplyr::select(-theme, -alpha_p_value ,-alpha_fixed_effect, -alpha_fixed_effect_sd, -beta_fixed_effect, -beta_fixed_effect_sd,
                        -alpha_random_effect, -beta_random_effect)
    }
    ###fixed_intercept_fixed_slope
    if(model_spec_theme_level == "fixed_intercept_fixed_slope"){

      ##Start to join
      pooled_CAPM_metrics_m_d_ref <- selected_signal_themes_m_d_ref %>%
        #Add fixed effects
        dplyr::mutate(alpha_fixed_effect = alpha_fixed_effects_df$alpha_fixed_effect,
                      alpha_fixed_effect_sd = alpha_fixed_effects_df$alpha_fixed_effect_sd,
                      alpha_p_value = alpha_fixed_effects_df$alpha_p_value,
                      beta_fixed_effect = beta_fixed_effects_df$beta_fixed_effect
        ) %>%
        #Add random effects
        dplyr::left_join(random_effects_df, by = "tickers") %>% #Join with random_effects by tickers
        dplyr::mutate(
          #Alpha
          theme_alpha = alpha_fixed_effect, individual_alpha = alpha_fixed_effect + alpha_random_effect,
          alpha_se = alpha_fixed_effect_sd,
          #Beta
          theme_beta = beta_fixed_effect, individual_beta = beta_fixed_effect + beta_random_effect,
          #Specific Risk
          specific_risk = y_sigma,
          #Other
          alpha_t_stat = individual_alpha/alpha_se,
          treynor_ratio = y_mean_geom_df$mean_geom/individual_beta,
          appraisal_ratio = individual_alpha/specific_risk,
          #P-value
          p_value = alpha_p_value/2) %>%
        dplyr::select(-theme, -alpha_p_value ,-alpha_fixed_effect, -alpha_fixed_effect_sd, -beta_fixed_effect,
                      -alpha_random_effect, -beta_random_effect)
    }

    return(pooled_CAPM_metrics_m_d_ref)
  }

