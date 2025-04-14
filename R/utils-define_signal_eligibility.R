#' Define Signal Eligibility
#'
#' This function evaluates the eligibility of signals for inclusion in the investment universe based on various performance metrics and statistical adjustments. It performs initial data checks, computes performance metrics, applies statistical adjustments (both frequentist and Bayesian), and classifies signals according to the specified selection policy.
#'
#' @param selected_backtest_returns_corrected_positions_m_xts_upd_ref A \code{xts} object containing backtest returns for various signals.
#' @param selected_market_factor_proxy_m_xts_upd_ref A \code{xts} object containing benchmark returns data. The first column should be dates, and the subsequent columns should contain returns.
#' @param custom_signal_universe_metrics_m_upd_ref Optional meta-dataframe with custom metrics for signals, such as precomputed alpha, IR, etc.
#'
#' @param p_correction_method The method for p-value correction. Possible options are:
#' \itemize{
#'   \item \code{"none"}: No correction.
#'   \item \code{"bayesian"}: Bayesian hierarchical model (via `brms`) is used for adjustment.
#'   \item \code{"bonferroni"}, \code{"holm"}, \code{"hochberg"}, \code{"hommel"}: FWER-controlling methods.
#'   \item \code{"BH"}, \code{"fdr"}, \code{"BY"}: FDR-controlling methods.
#' }
#'
#' @param signal_significance_threshold A numeric value for the alpha significance level. If set to 1, all signals with valid alphas are selected. Signals are retained only if their CAPM alpha is statistically significant.
#' @param enable_theme_representativeness Logical. If \code{TRUE}, ensures at least one signal per theme is selected, even if no signal passes the significance threshold, by choosing the signal with the highest alpha t-stat.
#'
#' @param model_structure A character string, either \code{"partial_pooled"} or \code{"no_pooled"}, indicating the type of model structure to use for mixed-effects estimation.
#' @param theme_level_intercept A string specifying intercept structure at the theme level. Only relevant when \code{model_structure == "partial_pooled"}.
#' @param theme_level_slope A string specifying slope structure at the theme level. Only relevant when \code{model_structure == "partial_pooled"}.
#' @param lmer_control A list of arguments passed to \code{lme4::lmer}. Expected components:
#' \itemize{
#'   \item \code{lmer_optimizer}: Optimization algorithm used (e.g., \code{"nloptwrap"}, \code{"bobyqa"}, \code{"Nelder_Mead"}, \code{"nlminbwrap"}).
#'   \item \code{lmer_optimization_objective}: Either \code{"REML"} or \code{"ML"}.
#'   \item \code{hierarchical_p_value_method}: p-value calculation method (e.g., \code{"Satterthwaite"}).
#' }
#'
#' @param active_returns Logical. If \code{TRUE}, returns are adjusted by subtracting benchmark returns before computing performance metrics.
#'
#' @param priors_m_upd_ref A meta-dataframe used to derive informative priors. Must include columns: \code{id}, \code{characteristic}, \code{dates}, \code{theme}, \code{alpha}, \code{beta}, \code{sigma}. Should not be provided if \code{user_priors} is set.
#' @param user_priors A list of \code{brms::set_prior()} objects manually defined by the user. Only used when \code{priors_m_upd_ref} is \code{NULL}.
#'
#' @param brms_control A list of parameters to control the Bayesian model fitting with \code{brms::brm}:
#' \itemize{
#'   \item \code{chains}: Number of MCMC chains (default 4).
#'   \item \code{iter}: Total iterations per chain (default 2000).
#'   \item \code{warmup}: Warmup iterations per chain (default = \code{floor(iter / 2)}).
#'   \item \code{thin}: Thinning interval (default 1).
#'   \item \code{seed}: Random seed (default \code{NA}).
#'   \item \code{adapt_delta}: Target acceptance rate for HMC sampler (default 0.99).
#' }
#'
#' @param prior_derivation_control A list of additional parameters for prior generation:
#' \itemize{
#'   \item \code{half_t_df}: Degrees of freedom in half-t distribution for scale parameters (default 30).
#' }
#'
#' @param selected_signal_themes_m_d_ref A meta-dataframe with columns \code{id}, \code{dates}, and \code{theme}. Specifies group membership for each signal. Should include data for the current date only.
#'
#' @param lower_quantile_winsorization Numeric between 0 and 1. Lower quantile threshold for winsorization of signal metrics (e.g., alpha t-stats).
#' @param upper_quantile_winsorization Numeric between 0 and 1. Upper quantile threshold for winsorization of signal metrics.
#'
#' @param verbose Logical. If \code{TRUE}, prints messages to the console.
#' @param parallel Logical. Enables parallel execution (only for Bayesian models).
#'
#' @return A list with the following components:
#' \describe{
#'   \item{\code{signal_universe_m_d_ref}}{A dataframe with computed performance metrics, adjusted p-values, and classification flags.}
#'   \item{\code{frequentist_results}}{Results from the frequentist mixed-effects model (if applicable).}
#'   \item{\code{bayesian_results}}{Results from the Bayesian model (if applicable).}
#' }
#'
#' @details
#' When \code{priors_m_upd_ref} is provided, the function uses a frequentist mixed-effects model to derive priors, later used in a Bayesian model:
#' \itemize{
#'   \item Priors for location parameters (e.g., alpha, beta) follow normal distributions.
#'   \item Priors for scale parameters (e.g., random effect std. devs) follow half-t distributions.
#'   \item Correlation priors follow an LKJ distribution.
#' }
#'
#' @section Model Specifications at Theme Level:
#' \describe{
#'   \item{\code{random_intercept}}{Includes a global intercept and random intercepts at the theme and signal level.}
#'   \item{\code{fixed_intercepts}}{Uses fixed effects for themes and random effects for signals.}
#'   \item{\code{fixed_intercepts_and_slopes}}{Includes fixed theme-wise intercepts and slopes, and random signal intercepts.}
#'   \item{\code{none}}{Only signal-level random effects are modeled.}
#' }
#'
define_signal_eligibility <- function(
    #Backtests
  selected_backtest_returns_corrected_positions_m_xts_upd_ref,
  selected_market_factor_proxy_m_xts_upd_ref,
  custom_signal_universe_metrics_m_upd_ref = NULL,
  #P-Values
  p_correction_method = "none", signal_significance_threshold = 0.05,
  #Theme representativeness
  enable_theme_representativeness = TRUE,
  #Model Structure
  model_structure = "no_pooled", theme_level_intercept = NULL, theme_level_slope = NULL,
  lmer_control = list(lmer_optimizer = "nloptwrap", lmer_optimization_objective = "REML", hierarchical_p_value_method = "Satterthwaite"),
  active_returns = TRUE,
  #Bayesian method
  priors_m_upd_ref = NULL, user_priors = NULL,
  brms_control = list(iter = 2000, chains = 4, thin = 1, seed = NA, adapt_delta = 0.99),
  prior_derivation_control = list(half_t_df = 30),
  #Signal Themes
  selected_signal_themes_m_d_ref,
  #Winsorization
  lower_quantile_winsorization = 0.025, upper_quantile_winsorization = 0.975,
  #Verbose & Parallel
  verbose = TRUE, parallel = TRUE
){

  #Initial Preparations
  ################
  #Create model_spec_theme_level
  if (model_structure == "partial_pooled"){
    model_spec_theme_level <- paste0(theme_level_intercept, "_intercept_", theme_level_slope, "_slope")
  } else {
    model_spec_theme_level <- NULL
  }

  ################

  #Summarize Backtest Performance
  ################

  #Get main stats of backtest
  performance_summary_list <- summarize_performance(
    #CAPM Model Specification
    model_structure = model_structure, model_spec_theme_level = model_spec_theme_level, lmer_control = lmer_control,
    #Themes
    selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref,
    active_returns = active_returns,
    #Data
    selected_backtest_returns_corrected_positions_m_xts_upd_ref = selected_backtest_returns_corrected_positions_m_xts_upd_ref,
    selected_market_factor_proxy_m_xts_upd_ref = selected_market_factor_proxy_m_xts_upd_ref,
    custom_signal_universe_metrics_m_upd_ref = custom_signal_universe_metrics_m_upd_ref,
    verbose = verbose
    )

    ###Extract
    signal_universe_m_d_ref <- performance_summary_list$signal_universe_m_d_ref
    frequentist_results <- performance_summary_list$frequentist_fit_results_list

  #################################

    #P-adjust!
    ############################

    #Frequentist version
    ######################
    if (!p_correction_method == "bayesian"){

      ##Frequentist adjustment
      p_value_df <- data.frame(p_value = unique(signal_universe_m_d_ref$p_value)) #First get all unique p_values. This will avoid too much strictness when using hierarchical methods
      p_value_df$adjusted_p_value <- stats::p.adjust(p_value_df$p_value, method = p_correction_method) #Adjust them
      ##Join
      signal_universe_m_d_ref <- signal_universe_m_d_ref %>% dplyr::left_join(p_value_df, by = "p_value") #Join p-value adjust

      #Elect final signal for signal_universe_m_d_ref
      signal_universe_m_d_ref[, "exp_ret_score"] <- signal_transform(
        signal_universe_m_d_ref[, "alpha_t_stat"],
        upper_quantile_winsorization = upper_quantile_winsorization,
        lower_quantile_winsorization = lower_quantile_winsorization
      )
      #######################


    } else {
      #Beware of the ALMIGHTY Bayesian model
      ######################################

      #Get parameters of brms_control (using default values in case of NULL)
      chains <- if (is.null(brms_control$chains)) 4 else brms_control$chains
      iter <- if (is.null(brms_control$iter)) 2000 else brms_control$iter
      warmup <- if (is.null(brms_control$warmup)) round(iter/2) else brms_control$warmup
      thin <- if (is.null(brms_control$thin)) 1 else brms_control$thin
      seed <- if (is.null(brms_control$seed)) NA else brms_control$seed
      adapt_delta <- if (is.null(brms_control$adapt_delta)) 0.80 else brms_control$adapt_delta

      #Get parameters of prior_derivation_control
      half_t_df <- if (is.null(prior_derivation_control$half_t_df)) 30 else prior_derivation_control$half_t_df
      lmer_optimizer <- if (is.null(lmer_control$lmer_optimizer)) "nloptwrap" else lmer_control$lmer_optimizer
      lmer_optimization_objective <- if (is.null(lmer_control$lmer_optimization_objective)) "REML" else lmer_control$lmer_optimization_objective

      #Bayesian adjustment
      bayesian_adjustment_results_list <- bayesian_adjustment(
        #Signals and benchmark
        signal_universe_m_d_ref = signal_universe_m_d_ref,
        selected_backtest_returns_corrected_positions_m_xts_upd_ref = selected_backtest_returns_corrected_positions_m_xts_upd_ref,
        selected_market_factor_proxy_m_xts_upd_ref = selected_market_factor_proxy_m_xts_upd_ref,
        #Priors
        priors_m_upd_ref = priors_m_upd_ref,
        user_priors = user_priors,
        model_spec_theme_level = model_spec_theme_level,
        #lmer control
        half_t_df = half_t_df, lmer_optimizer = lmer_optimizer, lmer_optimization_objective = lmer_optimization_objective,
        #brms control
        chains = chains, iter = iter, warmup = warmup, thin = thin, seed = seed, adapt_delta = adapt_delta,
        #Groups
        selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref,
        parallel = parallel, verbose = verbose
      )

      ##Get results from bayesian adjustment
      signal_universe_m_d_ref <- bayesian_adjustment_results_list$posterior_signal_universe_m_d_ref
      bayesian_results <- bayesian_adjustment_results_list[-1]


      #Elect final signal for signal_universe_m_d_ref
      signal_universe_m_d_ref[, "exp_ret_score"] <- signal_transform(
        signal_universe_m_d_ref[, paste0("posterior_", "alpha_t_stat")], #Final signal
        #Winsorization quantiles
        upper_quantile_winsorization = upper_quantile_winsorization,
        lower_quantile_winsorization = lower_quantile_winsorization
      )
      ######################################

    }
    ############################

    #Classify it!
    ###################################
    signal_universe_m_d_ref <- classify_investment_universe(
      universe_m_d_ref = signal_universe_m_d_ref, #Signal Universe
      signal_significance_threshold = signal_significance_threshold, #Signal Significance Threshold
      groups_m_d_ref = selected_signal_themes_m_d_ref, #Groups to select
      #Build concentration constraint policy for signals
      concentration_constraint_policy =  list(
        benchmark = c("theme_ss", "theme_sb"), #Reference benchmark
        max_abs_active_group_weight = if(enable_theme_representativeness) 0.1 else NULL), #Set an arbitrary value to enable theme representativeness
      asset_object = "signals"
    )
    ################################

    signal_eligibility_results_list <- list()
    signal_eligibility_results_list$signal_universe_m_d_ref <- signal_universe_m_d_ref %>% dplyr::select(-exp_ret_score)
    signal_eligibility_results_list$frequentist_results <- frequentist_results
    try(signal_eligibility_results_list$bayesian_results <- bayesian_results, silent = TRUE)

    return(signal_eligibility_results_list)

}
