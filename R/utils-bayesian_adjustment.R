#' Bayesian Adjustment Function
#'
#' Performs Bayesian p-value adjustment based on priors, fits a Bayesian hierarchical model to signals, and summarizes posterior draws. Optionally provides progress updates if verbose is set to TRUE.
#'
#' @param selected_signals_backtest_returns_upd_ref A data frame or matrix containing the backtest returns for the selected signals. This data is used for model fitting.
#' @param selected_benchmark_returns_upd_ref A data frame or matrix containing the updated benchmark returns. The second column is used for model fitting and posterior analysis.
#' @param selected_priors_informative_data_m_upd_ref A data frame or matrix containing informative prior data. This data is used to set priors if `priors_type` is not `"none"`.
#' @param priors_type A character string indicating if and how priors should be set. If `"none"`, no priors are set. Otherwise, the function `set_priors` is used to determine the priors.
#' @param signals_groups_m_d_ref A data frame containing groups of signals with associated metadata, such as themes. This information is used to fit the Bayesian hierarchical model.
#' @param verbose A logical value indicating whether to print progress messages. If TRUE, progress messages are printed; otherwise, they are suppressed. Default is TRUE.
#'
#' @details
#' This function performs the following steps:
#' \itemize{
#'   \item If `priors_type` is not `"uninformative"`, it sets priors based on the provided informative data using the `set_priors` function.
#'   \item It fits a Bayesian hierarchical model to each theme in `signals_groups_m_d_ref` using parallel processing. Progress messages are printed if `verbose` is TRUE.
#'   \item It extracts posterior draws from the fitted models and summarizes them using the `summarize_posterior_draws` function.
#' }
#'
#' @return
#' A data frame or matrix (`signal_universe_m_d_ref`) updated with posterior statistics based on the Bayesian model fitting.
#'
#' @export
bayesian_adjustment <- function(selected_signals_backtest_returns_upd_ref, selected_benchmark_returns_vector_upd_ref, signal_universe_m_d_ref,
                                selected_priors_informative_data_m_upd_ref, priors_type, user_priors_list,
                                signals_groups_m_d_ref, verbose = TRUE){

  #Initial checks
  ##prior type
  if(is.null(priors_type) || !priors_type %in% c("uninformative", "all", "user", "mean")){
    stop("priors_type should be one of uninformative, all, user or mean")
  }

  ##chosen_informative_data
  if(!priors_type %in% c("uninformative", "user")){
    #If priors type is not exogenous, a prior dataframe should be set
    if(is.null(selected_priors_informative_data_m_upd_ref)){
      stop("priors_m_df_list must be provided and also contemplate chosen_informative_data if priors_type is all or mean")
    }
  }


  #Set priors based on outside informative data
  #############################################
  #Check if priors are to be set (otherwise, use brms default uninformative priors)
  if(!priors_type == "uninformative"){
    #Check if user provided a prior
    if(priors_type == "user"){
      elected_priors_list <- user_priors_list
    } else {
      elected_priors_list <- get_priors_from_informative_data(
        selected_priors_informative_data_m_upd_ref = selected_priors_informative_data_m_upd_ref, #selected_priors_informative_data_m_upd_ref
        priors_type = priors_type #Should scale parameters also have priors?
      )
    }
  }
  #############################################

  #Fit bayesian hierarchical model
  #############################################
  ##Get themes
  signals_themes <- data.frame(theme = unique(signals_groups_m_d_ref$theme)) #Get themes from signals in signal_universe

  ##Fit to all themes in parallel
  ###give message
  if(verbose){
    tictoc::tic(msg = crayon::green("Ended bayesian hierarchical fit"))
    cat("\n")
    cat("Starting bayesian hierarchical fit")
  }

  ###Fit
  bayesian_fit_list <- furrr::future_pmap(signals_themes, #List of themes to apply model
                                          ~ fit_bayesian_model( #Bayesian fit call
                                            ...,
                                            #Data
                                            selected_signals_backtest_returns_upd_ref = selected_signals_backtest_returns_upd_ref,
                                            selected_benchmark_returns_vector_upd_ref = selected_benchmark_returns_vector_upd_ref,
                                            #Groups
                                            signals_groups_m_d_ref = signals_groups_m_d_ref,
                                            #Priors
                                            elected_priors_list = elected_priors_list
                                          ),
                                          .options = furrr::furrr_options(seed = TRUE),
                                          .progress = TRUE
  )

  ###message
  if(verbose) tictoc::toc()

  #############################################

  #Get Posteriors
  #############################################
  #Draw posteriors
  posteriors_draws_list <- lapply(bayesian_fit_list, function(x) insight::get_parameters(x, effects = "all"))
  names(posteriors_draws_list) <- signals_themes$theme #Get names

  #Update signal_universe_m_d_ref with posterior statistics
  posterior_signal_universe_m_d_ref <- summarize_posterior_draws(signal_universe_m_d_ref = signal_universe_m_d_ref, #Signal Universe
                                                                 posteriors_draws_list = posteriors_draws_list, #Posteriors Draws from Bayesian Model
                                                                 selected_benchmark_returns_vector_upd_ref = selected_benchmark_returns_vector_upd_ref,
                                                                 signals_groups_m_d_ref = signals_groups_m_d_ref) #Groups
  #############################################
  bayesian_adjustment_results_list <- list(
    posterior_signal_universe_m_d_ref = posterior_signal_universe_m_d_ref,
    bayesian_fit_list = bayesian_fit_list,
    elected_priors_list = elected_priors_list
  )

  return(bayesian_adjustment_results_list)

}

