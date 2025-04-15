#' @title Summarize Hierarchical CAPM Model (lmer)
#'
#' @description
#' Summarizes the output of a hierarchical CAPM fitted using `lme4::lmer()`, extracting fixed and random effects,
#' computing relevant statistics (alphas, betas, p-values, t-stats, Treynor ratio, appraisal ratio), and aggregating results
#' according to a specified model structure (theme-level or signal-level).
#'
#' @param lmer_model A fitted `lme4::lmer` model object, typically estimating a hierarchical CAPM across signals.
#' @param signal_universe_m_d_ref *(Optional)* A meta dataframe including all signals under consideration. This argument is not currently used but can be included for consistency or future extension.
#' @param selected_signal_themes_m_d_ref A meta dataframe including signals that passed eligibility filters, containing columns like `tickers` and `theme`. Used to join results and determine final structure.
#' @param model_spec_theme_level A character string indicating the hierarchical structure of the model. Must be one of:
#' \itemize{
#'   \item `"random_intercept_fixed_slope"`: Random intercepts per theme, shared slope.
#'   \item `"theme_specific_intercept_fixed_slope"`: Fixed intercepts per theme, shared slope.
#'   \item `"theme_specific_intercept_theme_specific_slope"`: Fixed intercepts and slopes per theme.
#'   \item `"fixed_intercept_fixed_slope"`: No grouping; simple fixed-effects model with intercept and slope.
#' }
#' @param hierarchical_p_value_method A character string indicating the degrees of freedom approximation method for fixed-effect p-values. Must be one of:
#' `"Satterthwaite"`, `"Kenward-Roger"`, or `"lme4"` (which skips approximation).
#'
#' @return A dataframe (`pooled_CAPM_metrics_m_d_ref`) with per-signal CAPM metrics, including:
#' \itemize{
#'   \item `individual_alpha`, `individual_beta`: Total signal-specific alpha and beta.
#'   \item `theme_alpha`, `theme_beta`: Theme-level alpha and beta components.
#'   \item `alpha_se`, `alpha_t_stat`: Standard error and t-statistic of alpha.
#'   \item `p_value`: One-sided p-value for alpha.
#'   \item `specific_risk`: Residual volatility (sigma).
#'   \item `treynor_ratio`, `appraisal_ratio`: Risk-adjusted performance measures.
#' }
#'
#' @details
#' This function is used to decompose the result of mixed-effects CAPM models into interpretable metrics.
#' It handles different model structures flexibly and integrates fixed and random effects appropriately.
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

    } else if (model_spec_theme_level == "theme_specific_intercept_fixed_slope"){
      ###theme_specific_intercept_fixed_slope

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

    } else if (model_spec_theme_level == "theme_specific_intercept_theme_specific_slope"){

      ###theme_specific_intercept_theme_specific_slope

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
    } else if(model_spec_theme_level == "fixed_intercept_fixed_slope"){

      ###fixed_intercept_fixed_slope

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

