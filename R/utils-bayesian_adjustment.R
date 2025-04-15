#' Bayesian Adjustment Function
#'
#' Performs Bayesian p-value adjustment by setting priors, fitting a Bayesian hierarchical model to signals, and summarizing posterior draws. Optionally provides progress updates if `verbose` is set to `TRUE`.
#'
#' @param signal_universe_m_d_ref Data frame.
#'   A dataframe containing information about signals with the following columns:
#'   \describe{
#'     \item{tickers}{Unique identifiers for each signal.}
#'     \item{is_eligible}{Logical indicator specifying if a signal is eligible for modeling.}
#'     \item{final_signal}{Finalized signals to be used in the Bayesian adjustment.}
#'   }
#'
#' @param selected_backtest_returns_corrected_positions_m_xts_upd_ref Meta xts containing backtest returns for selected signals
#'
#' @param selected_market_factor_proxy_m_xts_upd_ref  Meta xts containing backtest returns for selected markte factor proxy
#'
#' @param priors_m_upd_ref Data frame.
#'   A (meta)data frame with the following columns:
#'   \describe{
#'     \item{id}{Identifier for each observation.}
#'     \item{characteristic/signal}{Characteristic or signal associated with each observation.}
#'     \item{dates}{Date of each observation.}
#'     \item{theme}{Theme associated with each signal, used for clustering in the hierarchical Bayesian model.}
#'     \item{alpha}{Mean and standard error values for the alpha parameter.}
#'     \item{beta}{Mean and standard error values for the beta parameter.}
#'     \item{sigma}{Sigma values used to build priors.}
#'   }
#'   **Note:** This dataframe should contain data only for the current date.
#'
#' @param model_spec_theme_level Character string.
#'   Specifies the structure of the hierarchical Bayesian model. Options include:
#'   \describe{
#'     \item{`"random_intercept_fixed_slope"`}{Random intercepts for signals, fixed slope across all themes.}
#'     \item{`"theme_specific_intercept_fixed_slope"`}{Fixed intercepts per theme, fixed slope across themes.}
#'     \item{`"theme_specific_intercept_theme_specific_slope"`}{Fixed intercepts and slopes per theme.}
#'     \item{`"fixed_intercept_fixed_slope"`}{One fixed intercept and slope across all observations.}
#'   }
#'
#' @param half_t_df Numeric.
#'   The degrees of freedom in the half-t distribution applied to model random effects. This parameter controls the tails of the distribution. Default is `30`.
#'
#' @param lmer_optimizer Character string.
#'   Specifies the optimizer to be used in the `lme4::lmer` function. Options include:
#'   \describe{
#'     \item{`"nloptwrap"`}{Non-linear optimization using the NLopt library.}
#'     \item{`"bobyqa"`}{Bound optimization BY quadratic approximation.}
#'     \item{`"Nelder_Mead"`}{Simplex-based Nelder-Mead optimization.}
#'     \item{`"nlminbwrap"`}{Wrapper for the `nlminb` optimizer.}
#'   }
#'   Default is `"nloptwrap"`.
#'
#' @param lmer_optimization_objective A character string indicating whether estimates should be chosen to optimize the 'REML' criterion or the 'likelihood'.
#'
#' @param selected_signal_themes_m_d_ref Data frame.
#'   A (meta)data frame containing metadata about signals with the following columns:
#'   \describe{
#'     \item{id}{Identifier for each observation.}
#'     \item{tickers}{Signal identifiers matching those in `signal_universe_m_d_ref` and `selected_backtest_returns_corrected_positions_upd_ref`.}
#'     \item{dates}{Dates corresponding to the backtest data.}
#'     \item{theme}{Group membership for each signal, defining the clusters for the Bayesian hierarchical model.}
#'   }
#'   **Note:** This dataframe should contain data only for the current date.
#'
#' @param user_priors List.
#'   A list containing user-defined priors for the hierarchical Bayesian model, used when `priors_type` is `"user"`. The list should conform to the structure required by the `brms` package.
#'
#' @param chains Integer.
#'   The number of Markov chains to run for the MCMC sampling. Default is `4`.
#'
#' @param iter Integer.
#'   The total number of iterations per chain for the MCMC sampling. Default is `2000`.
#'
#' @param warmup Integer.
#'   The number of warmup (burn-in) iterations per chain for the MCMC sampling. Default is `floor(iter / 2)`.
#'
#' @param thin Integer.
#'   The thinning interval for MCMC sampling. Default is `1`.
#'
#' @param seed Integer or `NA`.
#'   The seed for random number generation to ensure reproducibility. Set to a specific integer for reproducible results or `NA` for random seeding. Default is `NA`.
#'
#' @param adapt_delta Numeric.
#'   The target acceptance probability for the Hamiltonian Monte Carlo sampler. Higher values can lead to better convergence at the cost of slower sampling. Must be between `0` and `1`. Default is `0.99`.
#'
#' @param parallel Logical.
#'   Indicates whether to enable parallel computation using the `future` package. Default is `TRUE`.
#'
#' @param verbose Logical.
#'   Indicates whether to print progress messages during the Bayesian model fitting process. If `TRUE`, progress messages are printed; otherwise, they are suppressed. Default is `TRUE`.
#'
#' @details
#' This function performs Bayesian p-value adjustment through the following steps:
#' \enumerate{
#'   \item **Initial Checks**: Validates input parameters, ensuring `priors_type` is valid and that necessary prior data is provided based on the specified `priors_type`.
#'   \item **Set Priors**:
#'     \itemize{
#'       \item If `priors_m_upd_ref` is provided, it sets priors based on the provided informative data using the `derive_informative_priors_from_data` function.
#'       \item If `user_priors_list` is provided, it utilizes user-defined priors provided.
#'       \item If neither `user_priors_list` or `priors_m_upd_ref` are provided, it employs default uninformative priors as defined by the `brms` package.
#'     }
#'   \item **Fit Bayesian Hierarchical Model**:
#'     Fits a Bayesian hierarchical model to each theme in `selected_signal_themes_m_d_ref` using the `fit_bayesian_model` function. This process leverages parallel processing for efficiency. Progress messages are displayed if `verbose` is `TRUE`.
#'   \item **Extract and Summarize Posteriors**:
#'     Extracts posterior draws from the fitted models and summarizes them using the `summarize_posterior_draws` function. The summary includes metrics such as alphas, betas, sigmas, active returns, tracking errors, information ratios, appraisal ratios, and Treynor ratios.
#' }
#'
#' The Bayesian hierarchical model accounts for both theme-level and signal-level variations, allowing for nuanced adjustments based on the hierarchical structure of the data. Informative priors can be derived from existing data or specified by the user, providing flexibility in modeling approaches.
#'
#' @return
#' A named list with the following components:
#' \describe{
#'   \item{`posterior_signal_universe_m_d_ref`}{Data frame.
#'     The input `signal_universe_m_d_ref` updated with posterior summary statistics derived from the Bayesian model fitting. This includes metrics such as posterior alphas, betas, sigmas, active returns, tracking errors, information ratios (IR), appraisal ratios (AP), and Treynor ratios.
#'   }
#'   \item{`bayesian_model`}{`brmsfit` object.
#'     The fitted Bayesian hierarchical model containing posterior distributions, parameter estimates, diagnostics, and other details of the model fit.
#'   }
#'   \item{`elected_priors`}{List.
#'     The priors used in the Bayesian model, either derived from data or provided by the user.
#'   }
#'   \item{`frequentist_model`}{`lme4::lmer` object.
#'     The fitted frequentist linear mixed-effects model used to derive informative priors.
#'   }
#'   #'   \item{`posterior_draws_summaries`}{Data frame.
#'     Summary statistics of posterior distributions for each signal including mean, standard deviation, and confidence intervals for model parameters.
#'   }
#' }
#'
#' @export
bayesian_adjustment <- function(signal_universe_m_d_ref, selected_backtest_returns_corrected_positions_m_xts_upd_ref, selected_market_factor_proxy_m_xts_upd_ref, #Data
                                priors_m_upd_ref = NULL, model_spec_theme_level, user_priors = NULL, #Priors
                                lmer_optimization_objective = "REML", half_t_df = 30, lmer_optimizer = "nloptwrap",  #lme4 parameters
                                selected_signal_themes_m_d_ref,
                                chains = 4, iter = 2000, warmup = floor(iter/2), thin = 1, seed = NA, adapt_delta = 0.80, #MCMC parameters
                                parallel = TRUE, verbose = TRUE){

  #Initial checks
  ########################
  ##Infere and configure priors_type based on objects
  if(all(!is.null(priors_m_upd_ref), !is.null(user_priors))){
    stop("Only one of priors_m_upd_ref or user_priors should be provided.")
  } else {
    #Define and tell user which prior will be used
    if(!is.null(user_priors)){
      if(verbose) message("Priors in user_priors will be used")
      priors_type <- "user"
    }
    if(!is.null(priors_m_upd_ref)){
      if(verbose) message("Priors based on provided informative data will be used")
      priors_type <- "informative"
    }
    if(is.null(priors_m_upd_ref) & is.null(user_priors)){
      if(verbose) message("Default uninformative priors will be used")
      priors_type <- "uninformative"
    }
  }

  ##Model Spec Theme Level
  if(model_spec_theme_level %in% c("random_intercept_fixed_slope", "theme_specific_intercept_fixed_slope",
                                   "theme_specific_intercept_theme_specific_slope", "fixed_intercept_fixed_slope")){
    message("Model specification for theme-level is: ", model_spec_theme_level)
  } else {
    stop("model_spec_theme_level must be one of 'random_intercept_fixed_slope', 'theme_specific_intercept_fixed_slope',
         'theme_specific_intercept_theme_specific_slope' or 'fixed_intercept_fixed_slope'")
  }
  ########################

  #Set priors based on outside informative data
  #############################################
  #Check if priors are to be set (otherwise, use brms default uninformative priors)
  if(!priors_type == "uninformative"){
    #Check if user provided a prior
    if(priors_type == "user"){
      elected_priors_list <- list(priors = user_priors)
    } else {
      elected_priors_list <- derive_informative_priors_from_data(
        priors_m_upd_ref = priors_m_upd_ref, #priors_m_d_ref
        model_spec_theme_level = model_spec_theme_level, #Specification for hierarchical model
        half_t_df = half_t_df, #Degrees of freedom for student-t prior
        lmer_optimizer = lmer_optimizer, #LMER Optimizer,
        lmer_optimization_objective = lmer_optimization_objective #LMER Optimization Objective
      )
    }
    if(verbose){
      #Print results if verbose is TRUE
      cat("\nPriors have been set:\n")
      print(elected_priors_list$priors[,c("prior", "class", "coef", "group")])
    }
  } else {
    elected_priors_list <- NULL
  }
  #############################################

  #Fit bayesian hierarchical model
  #############################################
  ##Get themes
  signals_themes <- data.frame(theme = unique(selected_signal_themes_m_d_ref$theme)) #Get themes from signals in signal_universe

  ##Fit to all themes in parallel
  ###give message
  if(verbose){
    tictoc::tic(msg = crayon::green("Ended bayesian hierarchical fit"))
    cat("\n")
    cat("Starting bayesian hierarchical fit")
    cat("\n")
  }

  ###Fit
  posteriors_results_list <- fit_bayesian_hierarchical_model(
                               #Data
                               signal_universe_m_d_ref = signal_universe_m_d_ref,
                               selected_backtest_returns_corrected_positions_m_xts_upd_ref = selected_backtest_returns_corrected_positions_m_xts_upd_ref,
                               selected_market_factor_proxy_m_xts_upd_ref = selected_market_factor_proxy_m_xts_upd_ref,
                               #Groups
                               selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref,
                               #Priors
                               elected_priors = elected_priors_list$priors, model_spec_theme_level = model_spec_theme_level,
                               #MCMC Parameters
                               chains = chains, iter = iter, warmup = warmup, thin = thin, seed = seed, adapt_delta = adapt_delta,
                               #Future
                               parallel = parallel,
                               #Other
                               verbose = verbose
                               )

  ###message
  if(verbose) tictoc::toc()

  #############################################

  bayesian_adjustment_results_list <- list(
    posterior_signal_universe_m_d_ref = posteriors_results_list$signal_universe_m_d_ref,
    brm_model = posteriors_results_list$brm_model,
    posterior_draws_summaries = posteriors_results_list$posterior_draws_summaries,
    elected_priors =  elected_priors_list$priors,
    elected_priors_frequentist_model = elected_priors_list$model
  )

  return(bayesian_adjustment_results_list)

}

