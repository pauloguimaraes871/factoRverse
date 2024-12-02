#' Perform validation checks on inputs for Signal Selection (SS) Workflow
#'
#' This function validates and checks various inputs required for signal selection in the context of run_ss_backtest function.
#' @param signals_m_df A (meta) data frame with columns including "id", "tickers", "dates", and the selected signals.
#' @param chosen_signals A vector with user-defined characteristics to be considered.
#' @param signal_positions A named vector with same length and names as chosen_signals describing whether positions should be taken "long" or "short".
#' @param backtest_returns_df A data frame with a 'dates' column and remaining columns named according to signals in signals_m_df, containing historical backtested returns.
#' @param data_availability_cutoff The minimum number of non-NA observations required for a backtest to be considered.
#' @param rebalancing_months Months (numeric) when signal selection should be implemented.
#' @param selected_benchmark_returns_df A data frame with a 'dates' column and a column with benchmark returns, named accordingly.
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
#' @param signal_significance_threshold A decimal indicating the hypothesis testing zero-alpha null-hypothesis rejection criteria. If one wants to select all chosen_signals,
#' provide 1. In any case, a signal being selected demands a significant CAPM alpha.
#' @param priors_m_df A (meta) data frame with columns including "id", "characteristic/signal", "dates", "theme" (used for clustering in hierarchical bayesian model)
#' and values for alpha (mean and se), beta (mean and se) and sigma, which are used to build priors.
#' @param priors_type A flag indicating which priors should be set. Possible options are:
#' \itemize{
#'    \item {"all"}: Set priors for all parameters, including mean (´mu´), variance (´tau´) and correlation based on `priors_m_df` data.
#'    \item: {"mean"}: Set priors only for mean ('mu').
#'    \item: {"uninformative"}: Set uninformative priors for all parameters.
#'    \item: {"user"}: Set priors defined by the user.
#' @param signal_themes_m_df A (meta) data frame with id, tickers ("signals") and dates column contemplating all signals in `signals_m_df` and a "theme" column providing group membership for each signal, which is needed
#' for defining clusters in bayesian hierarchical model.
#'
#' @export
check_inputs_ss_backtest <- function(
    #Dates
  rebalancing_months, data_availability_cutoff,
  #Signals
  signals_m_df, chosen_signals, signal_positions,
  #Backtests
  backtest_returns_df, selected_benchmark_returns_df,
  #P-value
  p_correction_method, signal_significance_threshold,
  #Bayesian variables
  priors_m_df, priors_type, signal_themes_m_df){

  #Structure
  #########################

  #signals_m_df
  ###Coercible
  if(!(is_coercible_to_meta_dataframe(signals_m_df))){
    stop("signals_m_df should be coercible to meta_dataframe object")
  }

  if(!all(sapply(signals_m_df[,-c(1:3)], function(x) is.numeric(x) && all(!is.na(x))))){
    stop("signals_m_df should contain only numeric columns with non-NAs.")
  }

  ###Check if all chosen_signals are present in signals_m_upd_ref
  if(any(!chosen_signals %in% colnames(signals_m_df))){
    stop("signal selection not avaiable in signals_m_df")
  }

  #backtest_returns_df
  if(colnames(backtest_returns_df)[1] != "dates"){
    stop("backtest_returns_df must have a 'dates' first column")
  }

  if(nrow(backtest_returns_df) < data_availability_cutoff){
    stop("backtest_returns_df must have at least data_availability_cutoff rows")
  }

  if(any(!backtest_returns_df$dates %in% signals_m_df$dates)){
    stop("all dates in backtest_returns_df must be present in signals_m_df")
  }


  #signal_themes_m_df
  ###Check if signal_themes_m_df contemplates theme column
  if(any(!colnames(signal_themes_m_df) == c("id", "tickers", "dates", "theme"))){
    stop("signal_themes_m_df must have columns 'id', 'tickers', 'dates' and 'theme'")
  }

  ###Check if theme column is character
  if(!is.character(signal_themes_m_df$theme)){
    stop("theme column in signal_themes_m_df must be character")
  }

  ###Check if dates in signal_themes_m_df and signals_m_df are the same
  signal_dates_m_vector <- as.Date(unique(signals_m_df$dates))
  signal_themes_dates_m_vector <- as.Date(unique(signal_themes_m_df$dates))
  if(any(!signal_dates_m_vector %in% signal_themes_dates_m_vector)){
    stop("dates in signal_themes_m_df and signals_m_df must be the same")
  }

  ###Check if all themes are present
  if(!all(unique(signal_themes_m_df$theme) %in% unique(priors_m_df$theme))){
    stop("all themes in signal_themes_m_df should be present in priors_m_df")
  }


  #priors_m_df
  ###Coercible
  if(!(is_coercible_to_meta_dataframe(priors_m_df))){
    stop("priors_m_df should be coercible to meta_dataframe object")
  }

  ###Check if all themes are present
  if(!all(unique(priors_m_df$theme) %in% unique(signal_themes_m_df$theme)) ||
     !all(unique(signal_themes_m_df$theme) %in% unique(priors_m_df$theme))
     ){
    stop("themes in priors_m_df and signal_themes_m_df should match")
  }

  if(colnames(priors_m_df) %in% c("alpha", "beta", "sigma")){
    stop("priors_m_df should contain columns 'alpha', 'beta' and 'sigma'")
  }


  #chosen signals and signal positions
  ###Check if there are repeated signals in chosen_signals
  if(!identical(chosen_signals, unique(chosen_signals))){
    stop("each signal must be chosen only once")
  }

  ###Check if all signals have a position
  if(!identical(chosen_signals, names(signal_positions))){
    stop("all chosen signals should have a matching position in signal_positions.")
  }

  ###Check if bayesian fit can be run
  if(p_correction_method == "bayesian"){
    if(is.null(user_priors_list) && is.null(priors_m_df)){
      stop("bayesian fit requires priors_m_df or user_priors_list.")
    }

    if(is.null(user_priors_list) && is.null(signal_themes_m_df)){
      stop("bayesian fit requires signal_themes_m_df.")
    }

    if(is.null(user_priors_list) && is.null(priors_type)){
      stop("bayesian fit requires priors_type.")
    }

    if(names(user_priors_list) != unique(signal_themes_m_df$theme)){
      stop("user_priors_list should have the same names as themes in signal_themes_m_df.")
    }

    if(!is.null(user_priors_list)){
      if(!is.list(user_priors_list)){
        stop("user_priors_list should be a list.")
      }

      if(!all(names(user_priors_list) %in% unique(signal_themes_m_df$theme))){
        stop("user_priors_list should have the same names as themes in signal_themes_m_df.")
      }

      if(!all(sapply(user_priors_list, function(x) class(x) == "brmsprior"))){
        stop("user_priors_list should contain only brmsprior objects.")
      }
    }


  }


  #Check structure of rebalancing_months
  if(!is.numeric(rebalancing_months)){
    stop("rebalancing_months should be numeric.")
  }

  if(rebalancing_months < 0 || rebalancing_months > 12){
    stop("rebalancing_months should be between 1 and 12.")
  }

}

