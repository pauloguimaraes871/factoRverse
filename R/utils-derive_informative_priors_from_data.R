#' Derive Informative Priors from Data
#'
#' This function fits a frequentist hierarchical linear mixed-effects model using the `lme4` package, based on the input data and the specified model structure. The resulting parameter estimates are used to derive informative priors for Bayesian modeling using the `brms` package.
#'
#' @param priors_m_upd_ref A (meta)data frame containing the following columns:
#'   \itemize{
#'     \item \code{id}: Identifier for each observation.
#'     \item \code{tickers}: tickers corresponding to individual securities or instruments.
#'     \item \code{dates}: Date of each observation.
#'     \item \code{theme}: Theme associated with each signal, used for clustering in the hierarchical Bayesian model (e.g., "value", "growth").
#'     \item \code{active_returns}: Excess returns of a signal over its benchmark.
#'     \item \code{benchmark_returns}: Returns of the benchmark associated with each signal.
#'   }
#' The data frame should only include observations up to the current date.
#' @param half_t_df A numeric indicating the degrees of freedom in the half-t distribution to be applied to model random effects.
#' Although the function specifies a regular student t distribution, `brms` will use half-t distribution, ensuring strictly positive parameters.
#' @param lmer_optimizer A character string specifying the optimizer to be used in the `lme4::lmer` function.
#' It will be passed to lme4::lmerControl, which will be used in the `lme4::lmer` function.
#' Options include: 'nloptwrap', 'bobyqa', 'Nelder_Mead' or 'nlminbwrap'
#' @param lmer_optimization_objective A character string indicating whether estimates should be chosen to optimize the 'REML' criterion or the 'likelihood'.
#' @param model_spec_theme_level A character string indicating the structure of the hierarchical Bayesian model. This parameter controls the specification of parameters at the \code{theme} level, assuming tickers are uniquely nested within each theme. Options include:
#'   \itemize{
#'     \item \code{"random_intercept"}: Random effects on the \code{theme}-level intercept. Includes random intercepts for themes and both random intercepts and slopes for each theme-signal combination. This captures variability at both levels.
#'     \item \code{"fixed_intercepts"}: Fixed intercepts for each \code{theme}, with a global slope for the market factor proxy. Nested variability within themes is modeled using random intercepts and slopes for theme-signal combinations.
#'     \item \code{"fixed_intercepts_and_slopes"}: Fixed intercepts and slopes for each \code{theme}. Includes interaction terms between themes and the market factor proxy, with random intercepts for tickers.
#'   }
#'
#' @details
#' The function uses frequentist linear mixed-effects models to estimate parameters that are subsequently translated into Bayesian priors:
#'   \itemize{
#'     \item Priors for location parameters (e.g., intercepts, slopes) follow a normal distribution.
#'     \item Priors for scale parameters (e.g., random effect standard deviations) follow a half-t distribution.
#'     \item Correlation priors for random effects are modeled using the LKJ (Lewandowski-Kurowicka-Joe) distribution.
#'   }
#'
#' ### Model Specifications at Theme Level
#'
#' #### \code{random_intercept}
#' This model includes:
#'   \itemize{
#'     \item Fixed intercept and slope for the market factor proxy.
#'     \item Random intercepts at the \code{theme} level.
#'     \item Random intercepts and slopes for each theme-signal combination.
#'   }
#' The model equation is:
#' \deqn{y_i = \beta_0 + \beta_1 \cdot x_i + b_{0,t_i} + b_{0,g_i} + b_{1,g_i} \cdot x_i + \epsilon_i}
#' See the detailed breakdown in the example section.
#'
#' #### \code{fixed_intercepts}
#' This model includes:
#'   \itemize{
#'     \item Fixed intercepts for each \code{theme}, expressed as a summation over all themes.
#'     \item A global fixed slope for the market factor proxy.
#'     \item Random intercepts and slopes for theme-signal combinations.
#'   }
#' The model equation is:
#' \deqn{y_{i} = \sum_{k} \beta_{k} \cdot \text{theme}_{k,i} + \beta_{m} \cdot x_{i} + b_{0,g_{i}} + b_{1,g_{i}} \cdot x_{i} + \epsilon_{i}}
#'
#'
#' #### \code{fixed_intercepts_and_slopes}
#' This model includes:
#'   \itemize{
#'     \item Fixed intercepts and slopes for each \code{theme}.
#'     \item Interaction terms between themes and the market factor proxy.
#'     \item Random intercepts for tickers.
#'   }
#' The model equation is:
#' \deqn{y_{it} = \sum_k \beta_k \cdot \text{theme}_{k,i} + \sum_k \gamma_k \cdot \text{theme}_{k,i} \cdot x_{it} + \beta_m \cdot X_{it} + b_{0,i} + \epsilon_{it}}
#'
#' @return A list with two components:
#'   \itemize{
#'     \item \code{priors}: A list of \code{brms::set_prior} objects specifying the derived priors.
#'     \item \code{model}: The fitted linear mixed-effects model (\code{lme4::lmer} object).
#'   }
#'
#'
derive_informative_priors_from_data <- function(priors_m_upd_ref, model_spec_theme_level,
                                                half_t_df = 30, lmer_optimizer = "nloptwrap", lmer_optimization_objective = "REML"){

  #Initial checks
  ###Model specification correct
  if(!model_spec_theme_level %in% c("random_intercept_fixed_slope", "theme_specific_intercept_fixed_slope",
                                    "theme_specific_intercept_theme_specific_slope", "fixed_intercept_fixed_slope")){
    stop("Invalid model specification.")
  }

  ###Check if each ticker is uniquely linked to a single theme
  if(!all(tapply(priors_m_upd_ref$theme, priors_m_upd_ref$tickers, function(x) length(unique(x)) == 1))){
    stop("Each ticker should be uniquely linked to a single theme.")
  }

  ###Check if df is numeric
  if(!is.numeric(half_t_df)){
    stop("half_t_df should be numeric")
  }

  ###Check validity of lmer optimizer
  if(!lmer_optimizer %in% c("nloptwrap", "bobyqa", "Nelder_Mead", "nlminbwrap")){
    stop("Invalid optimizer. Please choose from 'nloptwrap', 'bobyqa', 'Nelder_Mead' or 'nlminbwrap'.")
  }

  ##Check validitidy of optimization_objective
  if(!lmer_optimization_objective %in% c("REML", "likelihood")){
    stop("optimization_objective should be either 'REML' or 'likelihood'.")
  } else {
    if(lmer_optimization_objective == "REML"){
      lmer_optimization_objective <- TRUE
    } else {
      lmer_optimization_objective <- FALSE
    }
  }

  #Fit frequentist model to derive priors.
  ###Set all NULL, but selected_backtest_returns_corrected_positions_m_upd_ref to skip data preprocessing
  lmer_model <- fit_frequentist_hierarchical_model(signal_universe_m_d_ref = NULL,
                                                   selected_backtest_returns_corrected_positions_m_upd_ref = priors_m_upd_ref,
                                                   selected_backtest_returns_corrected_positions_m_xts_upd_ref = NULL,
                                                   selected_market_factor_proxy_m_xts_upd_ref = NULL,
                                                   selected_signal_themes_m_d_ref = NULL,
                                                   model_spec_theme_level = model_spec_theme_level,
                                                   lmer_optimizer = lmer_optimizer, lmer_optimization_objective = lmer_optimization_objective
                                                   )$lmer_model

  if(model_spec_theme_level == "random_intercept_fixed_slope"){

    # Extract fixed effects estimates and standard errors from the model summary
    fixed_effects <-  summary(lmer_model)$coefficients

    # Extract random effects standard deviations from VarCorr
    random_effects <- as.data.frame(lme4::VarCorr(lmer_model))
    ##Check for 0 sd
    if(any(random_effects$sdcor == 0)){
      warning("Some random effects standard deviations are zero. Replacing with 0.01")
      random_effects$sdcor[random_effects$sdcor == 0] <- 0.01
    }

    # Define informative priors
    priors <- c(
      # Fixed effects priors
      ## Intercept
      brms::set_prior(paste0("normal(",
                             round(fixed_effects["(Intercept)","Estimate"], 4), ", ",
                             round(fixed_effects["(Intercept)", "Std. Error"], 4), ")"),
                      class = "Intercept"
      ),
      ## Market factor proxy
      brms::set_prior(paste0("normal(",
                             round(fixed_effects["market_factor_proxy", "Estimate"], 4), ", ",
                             round(fixed_effects["market_factor_proxy", "Std. Error"], 4), ")"),
                      class = "b", coef = "market_factor_proxy"
      ),

      # Random effects priors for theme:tickers
      ## Intercept
      brms::set_prior(paste0("student_t(",half_t_df,",0,",
                             round(random_effects$sdcor[
                               random_effects$grp == "theme:tickers" &
                                 is.na(random_effects$var2) &
                                 random_effects$var1 == "(Intercept)"], 4), ")"),
                      class = "sd", group = "theme:tickers", coef = "Intercept"
      ),
      ## Slope
      brms::set_prior(paste0("student_t(",half_t_df,",0,",
               round(random_effects$sdcor[
                 random_effects$grp == "theme:tickers" &
                   random_effects$var1 == "market_factor_proxy"], 4), ")"),
        class = "sd", group = "theme:tickers", coef = "market_factor_proxy"
      ),

      # Random effects priors for theme
      ## Intercept
      brms::set_prior(paste0("student_t(",half_t_df,",0,",
               round(random_effects$sdcor[
                 random_effects$grp == "theme" &
                   is.na(random_effects$var2) &
                   random_effects$var1 == "(Intercept)"], 4), ")"),
        class = "sd", group = "theme", coef = "Intercept"
      ),

      # Residual Standard Deviation Prior
      brms::set_prior(paste0("student_t(",half_t_df,",0,",
               round(stats::sigma(lmer_model), 4), ")"),
        class = "sigma"
      )
    )

    # Add correlation prior
      # Correlation prior between random intercept and slope in theme:tickers
      priors <- c(
        priors,
        brms::set_prior("lkj(2)", class = "cor")
      )


  }

  if(model_spec_theme_level == "theme_specific_intercept_fixed_slope"){

    # Extract fixed effects estimates and standard errors from the model summary
    fixed_effects <-  summary(lmer_model)$coefficients
    theme_coefficients <- rownames(fixed_effects)[grepl("^theme", rownames(fixed_effects))]

    # Extract random effects standard deviations from VarCorr
    random_effects <- as.data.frame(lme4::VarCorr(lmer_model))
      ##Check for 0 sd
      if(any(random_effects$sdcor == 0)){
        warning("Some random effects standard deviations are zero. Replacing with 0.01")
        random_effects$sdcor[random_effects$sdcor == 0] <- 0.01
      }

    # Dynamically construct priors for themes
    priors <- data.frame(
      prior = character(),
      class = character(),
      coef = character(),
      group = character(),
      resp = character(),
      dpar = character(),
      nlpar = character(),
      lb = numeric(),
      ub = numeric(),
      source = character(),
      stringsAsFactors = FALSE
    )

    # Add priors for themes
    for (coef_name in theme_coefficients) {
      priors <- rbind(
        priors,
        data.frame(
          prior = paste0("normal(",
                         round(fixed_effects[coef_name, "Estimate"], 4), ", ",
                         round(fixed_effects[coef_name, "Std. Error"], 4), ")"),
          class = "b",
          coef = coef_name,
          group = "",
          resp = "",
          dpar = "",
          nlpar = "",
          lb = NA,
          ub = NA,
          source = "user",
          stringsAsFactors = FALSE
        )
      )
    }

    # Add prior for market_factor_proxy
    priors <- rbind(
      priors,
      data.frame(
        prior = paste0("normal(",
                       round(fixed_effects["market_factor_proxy", "Estimate"], 4), ", ",
                       round(fixed_effects["market_factor_proxy", "Std. Error"], 4), ")"),
        class = "b",
        coef = "market_factor_proxy",
        group = "",
        resp = "",
        dpar = "",
        nlpar = "",
        lb = NA,
        ub = NA,
        source = "user",
        stringsAsFactors = FALSE
      )
    )

    # Add priors for random effects for theme:tickers
    priors <- rbind(
      priors,
      data.frame(
        prior = paste0("student_t(",half_t_df,",0,",
                       round(random_effects$sdcor[random_effects$grp == "theme:tickers" &
                                                  random_effects$var1 == "(Intercept)" &
                                                  is.na(random_effects$var2)
                                                  ], 4), ")"),
        class = "sd",
        coef = "Intercept",
        group = "theme:tickers",
        resp = "",
        dpar = "",
        nlpar = "",
        lb = NA,
        ub = NA,
        source = "user",
        stringsAsFactors = FALSE
      ),
      data.frame(
        prior = paste0("student_t(",half_t_df,",0,",
                       round(random_effects$sdcor[
                             random_effects$grp == "theme:tickers" &
                             random_effects$var1 == "market_factor_proxy"], 4), ")"),
        class = "sd",
        coef = "market_factor_proxy",
        group = "theme:tickers",
        resp = "",
        dpar = "",
        nlpar = "",
        lb = NA,
        ub = NA,
        source = "user",
        stringsAsFactors = FALSE
      )
    )

    # Add prior for residual standard deviation
    priors <- rbind(
      priors,
      data.frame(
        prior = paste0("student_t(",half_t_df,",0,", round(stats::sigma(lmer_model), 4), ")"),
        class = "sigma",
        coef = "",
        group = "",
        resp = "",
        dpar = "",
        nlpar = "",
        lb = NA,
        ub = NA,
        source = "user",
        stringsAsFactors = FALSE
      )
    )

    # Add prior for correlation between random effects
    priors <- rbind(
      priors,
      data.frame(
        prior = "lkj(2)",
        class = "cor",
        coef = "",
        group = "",
        resp = "",
        dpar = "",
        nlpar = "",
        lb = NA,
        ub = NA,
        source = "user",
        stringsAsFactors = FALSE
      )
    )


    # Ensure the priors object is treated as a brmsprior object
    class(priors) <- c("brmsprior", class(priors))
  }

  if(model_spec_theme_level == "theme_specific_intercept_theme_specific_slope"){

    # Extract fixed effects estimates and standard errors from the model summary
    fixed_effects <-  summary(lmer_model)$coefficients
    theme_coefficients <- rownames(fixed_effects)[grepl("^theme", rownames(fixed_effects))]

    # Extract random effects standard deviations from VarCorr
    random_effects <- as.data.frame(lme4::VarCorr(lmer_model))
      ##Check for 0 sd
      if(any(random_effects$sdcor == 0)){
        warning("Some random effects standard deviations are zero. Replacing with 0.01")
        random_effects$sdcor[random_effects$sdcor == 0] <- 0.01
      }

    # Dynamically construct priors for themes
    priors <- data.frame(
      prior = character(),
      class = character(),
      coef = character(),
      group = character(),
      resp = character(),
      dpar = character(),
      nlpar = character(),
      lb = numeric(),
      ub = numeric(),
      source = character(),
      stringsAsFactors = FALSE
    )

    # Add priors for themes
    for (coef_name in theme_coefficients) {
      priors <- rbind(
        priors,
        data.frame(
          prior = paste0("normal(",
                         round(fixed_effects[coef_name, "Estimate"], 4), ", ",
                         round(fixed_effects[coef_name, "Std. Error"], 4), ")"),
          class = "b",
          coef = coef_name,
          group = "",
          resp = "",
          dpar = "",
          nlpar = "",
          lb = NA,
          ub = NA,
          source = "user",
          stringsAsFactors = FALSE
        )
      )
    }

    # Add priors for random effects for theme:tickers
    priors <- rbind(
      priors,
      data.frame(
        prior = paste0("student_t(",half_t_df,",0,",
                       round(random_effects$sdcor[random_effects$grp == "theme:tickers" &
                                                  random_effects$var1 == "(Intercept)" &
                                                  is.na(random_effects$var2)
                       ], 4), ")"),
        class = "sd",
        coef = "Intercept",
        group = "theme:tickers",
        resp = "",
        dpar = "",
        nlpar = "",
        lb = NA,
        ub = NA,
        source = "user",
        stringsAsFactors = FALSE
      ),
      data.frame(
        prior = paste0("student_t(",half_t_df,",0,",
                       round(random_effects$sdcor[
                         random_effects$grp == "theme:tickers" &
                           random_effects$var1 == "market_factor_proxy"], 4), ")"),
        class = "sd",
        coef = "market_factor_proxy",
        group = "theme:tickers",
        resp = "",
        dpar = "",
        nlpar = "",
        lb = NA,
        ub = NA,
        source = "user",
        stringsAsFactors = FALSE
      )
    )

    # Add prior for residual standard deviation
    priors <- rbind(
      priors,
      data.frame(
        prior = paste0("student_t(",half_t_df,",0,", round(stats::sigma(lmer_model), 4), ")"),
        class = "sigma",
        coef = "",
        group = "",
        resp = "",
        dpar = "",
        nlpar = "",
        lb = NA,
        ub = NA,
        source = "user",
        stringsAsFactors = FALSE
      )
    )

    # Add prior for correlation between random effects
    priors <- rbind(
      priors,
      data.frame(
        prior = "lkj(2)",
        class = "cor",
        coef = "",
        group = "",
        resp = "",
        dpar = "",
        nlpar = "",
        lb = NA,
        ub = NA,
        source = "user",
        stringsAsFactors = FALSE
      )
    )


    # Ensure the priors object is treated as a brmsprior object
    class(priors) <- c("brmsprior", class(priors))

  }

  if(model_spec_theme_level == "fixed_intercept_fixed_slope"){

    # Extract fixed effects estimates and standard errors from the model summary
    fixed_effects <-  summary(lmer_model)$coefficients

    # Extract random effects standard deviations from VarCorr
    random_effects <- as.data.frame(lme4::VarCorr(lmer_model))
      ##Check for 0 sd
      if(any(random_effects$sdcor == 0)){
        warning("Some random effects standard deviations are zero. Replacing with 0.01")
        random_effects$sdcor[random_effects$sdcor == 0] <- 0.01
      }

    # Define informative priors
    priors <- c(
      # Fixed effects priors
      ## Intercept
      brms::set_prior(paste0("normal(",
                             round(fixed_effects["(Intercept)","Estimate"], 4), ", ",
                             round(fixed_effects["(Intercept)", "Std. Error"], 4), ")"),
                      class = "Intercept"
      ),
      ## Market factor proxy
      brms::set_prior(paste0("normal(",
                             round(fixed_effects["market_factor_proxy", "Estimate"], 4), ", ",
                             round(fixed_effects["market_factor_proxy", "Std. Error"], 4), ")"),
                      class = "b", coef = "market_factor_proxy"
      ),

      # Random effects priors for theme:tickers
      ## Intercept
      brms::set_prior(paste0("student_t(",half_t_df,",0,",
                             round(random_effects$sdcor[
                               random_effects$grp == "theme:tickers" &
                                 is.na(random_effects$var2) &
                                 random_effects$var1 == "(Intercept)"], 4), ")"),
                      class = "sd", group = "theme:tickers", coef = "Intercept"
      ),
      ## Slope
      brms::set_prior(paste0("student_t(",half_t_df,",0,",
                             round(random_effects$sdcor[
                               random_effects$grp == "theme:tickers" &
                                 random_effects$var1 == "market_factor_proxy"], 4), ")"),
                      class = "sd", group = "theme:tickers", coef = "market_factor_proxy"
      ),

      # Residual Standard Deviation Prior
      brms::set_prior(paste0("student_t(",half_t_df,",0,",
                             round(stats::sigma(lmer_model), 4), ")"),
                      class = "sigma"
      )
    )

    # Add correlation prior if enabled
      # Correlation prior between random intercept and slope in theme:tickers
      priors <- c(
        priors,
        brms::set_prior("lkj(2)", class = "cor")
      )


  }

  #Get final result
  elected_priors_list <- list(
    priors = priors,
    model = lmer_model
  )

  return(elected_priors_list)

}
