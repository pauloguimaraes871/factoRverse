#' Define Signal Eligibility
#'
#' This function evaluates the eligibility of signals for inclusion in the investment universe based on various performance metrics and statistical adjustments. It performs initial data checks, computes performance metrics, applies statistical adjustments (both frequentist and Bayesian), and classifies signals according to the specified selection policy.
#'
#' @param selected_backtest_returns_corrected_positions_xts_upd_ref A xts containing backtest returns for various signals.
#' @param selected_market_factor_proxy_xts_upd_ref A xts containing benchmark returns data. The first column should be identifiers for dates, and the subsequent columns should contain the returns data.
#' @param p_correction_method The method for p-value correction. Possible options are:
#'\itemize{
#'  \item{"none"}: No correction.
#'  \item{"bayesian"}: When bayesian is set, a hierarchical mixed-effects bayesian linear model is fitted to the data, using the `brms` package,
#'  which is an interface to the `Stan` probabilistic programming language.
#'  The user can also choose one of the following frequentist methods, which will control Family-Wise Error Rate (FWER) or the False Discovery Rate (FDR).
#'  FDR is less stringent than FWER.
#'  For FWER, possible options are:
#'  \item{"bonferroni"}: Bonferroni correction, which is dominated by Holm's method.
#'  \item{"holm"}: Holm's (1979) method.
#'  \item{"hochberg"}: Hochberg's (1988) method, valid when hypothesis tests are independent or non-negatively associated. Less powerful than Hommel's (1988) method, but
#'  faster to compute.
#'  \item{"hommel"}: Hommel's (1988) method, also valid when hypothesis tests are independent or non-negatively associated, but is more powerful than Hochberg (1988).
#'  For FDR, possible options are:
#'  \item{"BH" or "fdr"}: Benjamini-Hochberg (1995) procedure.
#'  \item{"BY"}: Benjamini-Yekutieli (2001) procedure.
#'  }
#' @param signal_significance_threshold A decimal indicating the hypothesis testing negative-alpha null-hypothesis rejection criteria. If one wants to select all chosen_signals,
#' provide 1. In any case, a signal being selected demands a significant CAPM alpha.
#' @param enable_theme_representativeness If TRUE, in case a given theme in `selected_signal_themes_m_d_ref` does not have any eligible signal, the signal
#' with highest alpha t-stat will be elected.
#' @param priors_m_df_upd_ref A (meta) data frame with columns including "id", "characteristic/signal", "dates", "theme" (used for clustering in hierarchical bayesian model)
#' and values for alpha (mean and se), beta (mean and se) and sigma, which are used to build priors. It should contain data only for current date.
#' Should not be provided if `user_priors_list` is already being provided.
#' When `priors_m_df_upd_ref` is provided, a lme4 model is fit to the data using the `lme4` package, and the priors are then estimated from the model.
#' In such case, ther user can control some aspects of the prior definition process with params: `model_spec_theme_level`, `v` and `lmer_optimizer`
#'
#' @param model_structure A character indicating whether the model should be estimated using a ´partial_pooled´ or ´no_pooled´ approach.
#' @param theme_level_intercept A character specifying the specification of effects of the intercept at the theme level.
#' @param theme_level_slope A character specifying the specification of effects of the slope at the theme level.
#' @param lmer_control Other additional parameters to be passed to `lme4::lmer` function.
#' \itemize{
#' #' \item{lmer_optimizer} A character string specifying the optimizer to be used in the
#' It will be passed to lme4::lmerControl, which will be used in the `lme4::lmer` function.
#' Options include: 'nloptwrap', 'bobyqa', 'Nelder_Mead' or 'nlminbwrap'
#'
#' \item{lmer_optimization_objective} A character string indicating whether estimates should be chosen to optimize the 'REML' criterion or the 'likelihood'.
#'
#' \item{hierarchical_p_value_method}
#' }
#'
#' @param active_returns A character string indicating whether performance metrics should be calculated based on active returns or raw returns. If TRUE,
#' backtest_returns_xts will be adjusted by subtracting the selected market factor proxy in benchmark_returns_xts. This does not
#' impact calculation of the CAPM model, whether it is frequentist or bayesian, pooled or partial pooled.
#'
#' @param prior_derivation_control A list of additional parameters to be passed to the `lme4::lmer` function:
#' \itemize{
#' \item{half_t_df} A numeric indicating the degrees of freedom in the half-t distribution to be applied in sd parameters.
#'}
#' @param brms_control Other additional parameters to be passed to brms::brm function:
#' \itemize{
#' \item{chains} Integer.
#' The number of Markov chains to run for the MCMC sampling. Default is `4`.
#'
#' \item{iter} Integer.
#' The total number of iterations per chain for the MCMC sampling. Default is `2000`.
#'
#' \item{warmup} Integer.
#' The number of warmup (burn-in) iterations per chain for the MCMC sampling. Default is `floor(iter / 2)`.
#'
#' \item{thin} Integer.
#' The thinning interval for MCMC sampling. Default is `1`.
#'
#' \item{seed} Integer or `NA`.
#' The seed for random number generation to ensure reproducibility. Set to a specific integer for reproducible results or `NA` for random seeding. Default is `NA`.
#'
#' \item{adapt_delta] Numeric.
#' The target acceptance probability for the Hamiltonian Monte Carlo sampler. Higher values can lead to better convergence at the cost of slower sampling. Must be between `0` and `1`. Default is `0.99`.
#' }
#'
#' @param parallel Logical.
#'   Indicates whether to enable parallel computation using the `future` package. Only avaialable for bayesian model. Default is `TRUE`.
#'
#' @param selected_signal_themes_m_d_ref A (meta) data frame with id, tickers ("signals") and dates column contemplating all signals in `signals_m_df` and a "theme" column providing group membership for each signal, which is needed
#' for defining clusters in bayesian hierarchical model. It should contain data only for current date.
#'
#' @param user_priors An object of class `brmsprior` with user-defined priors for the hierarchical bayesian model. It should be set with `model_spec_theme_level` structure in mind.
#' brms::set_prior() can be used to define the priors. Should not be provided if `priors_m_d_ref` is already being provided.
#'
#' @examples
#' \dontrun{
#'  Definition of priors for model_spec_theme_level = "random_intercept"
#'   elected_priors <- c(
#'   Prior for Intercept
#'  brms::set_prior("normal(0.0012, 0.0016)", class = "Intercept"),
#'
#'  Prior for market_factor_proxy coefficient
#'  brms::set_prior("normal(0.0003, 0.0003)", class = "b", coef = "market_factor_proxy"),
#'
#'  Prior for sd of Intercept at theme:tickers level
#'  brms::set_prior("student_t(30, 0, 0.0113)", class = "sd", group = "theme:tickers", coef = "Intercept"),
#'
#'  Prior for sd of market_factor_proxy at theme:tickers level
#'  brms::set_prior("student_t(30, 0, 0.0018)", class = "sd", group = "theme:tickers", coef = "market_factor_proxy"),
#'
#'  Prior for sd of Intercept at theme level
#'  brms::set_prior("student_t(30, 0, 0.0011)", class = "sd", group = "theme", coef = "Intercept"),
#'
#'  Prior for residual error (sigma)
#'  brms::set_prior("student_t(30, 0, 0.0256)", class = "sigma"),
#'
#'  LKJ prior for correlations
#'  brms::set_prior("lkj(2)", class = "cor")
#')
#'
#'
#' @param upper_quantile_winsorization Numeric value for upper winsorization.
#' @param lower_quantile_winsorization Numeric value for lower winsorization.
#' @param verbose A boolean indicating whether to print messages to the console.
#' @return A data frame containing the updated signal universe with computed performance metrics, adjusted p-values, and final signal classifications. The columns include:
#' \describe{
#'   \item{\code{tickers}}{The identifiers for the signals.}
#'   \item{\code{mean_active_return}}{The mean active return of each signal.}
#'   \item{\code{tracking_error}}{The tracking error of each signal.}
#'   \item{\code{IR}}{The information ratio of each signal.}
#'   \item{\code{alpha}}{The OLS CAPM alpha of each signal.}
#'   \item{\code{AP}}{The t-statistic of the alpha.}
#'   \item{\code{beta}}{The beta of each signal.}
#'   \item{\code{treynor}}{The Treynor ratio of each signal.}
#'   \item{\code{p_value}}{The p-value of the alpha.}
#'   \item{\code{adjusted_p_value}}{The p-value adjusted for multiple comparisons, if applicable.}
#'   \item{\code{exp_ret_score}}{The final signal classification after applying transformations and adjustments.}
#' }
#'
#'
#' #' @details
#'
#' When providing `priors_m_df_upd_ref` to set informative priors, the function uses frequentist linear mixed-effects models to estimate parameters that are subsequently translated into Bayesian priors:
#'
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
#' #### \code{none}
#' This model includes no parameter at the theme level and just models random intercepts and slopes for each signal.
#' The model equation is:
#' \deqn{y_i = \beta_0 + \beta_1 \cdot x_i + b_{0,g_i} + b_{1,g_i} \cdot x_i + \epsilon_i}
#'
#' @export
define_signal_eligibility <- function(
  #Backtests
  selected_backtest_returns_corrected_positions_m_xts_upd_ref,
  selected_market_factor_proxy_m_xts_upd_ref,
  custom_signal_universe_metrics_m_upd_ref,
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
      p_value_df$adjusted_p_value <- p.adjust(p_value_df$p_value, method = p_correction_method) #Adjust them
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
      signals_m_d_ref = signal_universe_m_d_ref, #Signal Universe
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
