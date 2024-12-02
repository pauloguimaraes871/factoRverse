#' Run a Signal Selection Backtest
#'
#' This function performs iterative signal selection based on frequentist or Bayesian methods, which are applied to portfolio returns backtests to identify which signals can be considered significant in stock-level return prediction.
#'
#' To determine whether a signal matters in cross-sectional predictability, the literature typically runs regressions of signal portfolios against a benchmark factor model (e.g., CAPM), computing alphas and corresponding t-stats. Due to the large number of signals identified in the literature (the "factor zoo"), methods to control for multiple testing are often advocated.
#'
#' @param signals_m_df A (meta) data frame with columns including "id", "tickers", "dates", and the selected signals.
#' @param chosen_signals A vector of user-defined characteristics to be considered.
#' @param signal_positions A named vector with the same length and names as `chosen_signals`, describing whether positions should be taken "long" or "short".
#' @param backtest_returns_df A data frame with a 'dates' column and remaining columns named according to signals in `signals_m_df`, containing historical backtested returns.
#' @param data_availability_cutoff The minimum number of non-NA observations required for a backtest to be considered.
#' @param split_method The method used for splitting the data, either "expanding" or "rolling" (default is "expanding").
#' @param rebalancing_months Numeric months when signal selection should be implemented.
#' @param selected_benchmark_returns_df A data frame with a 'dates' column and a column with benchmark returns, named accordingly.
#' @param p_correction_method The method for p-value correction. Possible options are:
#' \itemize{
#'   \item \strong{"none"}: No correction.
#'   \item \strong{"bayesian"}: A hierarchical mixed-effects Bayesian linear model is fitted to the data using the `brms` package.
#'
#'   The user can also choose one of the following frequentist methods, which control the Family-Wise Error Rate (FWER) or the False Discovery Rate (FDR). FDR is less stringent than FWER.
#'
#'   For FWER, possible options are:
#'   \itemize{
#'     \item \strong{"bonferroni"}: Bonferroni correction.
#'     \item \strong{"holm"}: Holm's (1979) method.
#'     \item \strong{"hochberg"}: Hochberg's (1988) method.
#'     \item \strong{"hommel"}: Hommel's (1988) method.
#'   }
#'
#'   For FDR, possible options are:
#'   \itemize{
#'     \item \strong{"BH"} or \strong{"fdr"}: Benjamini-Hochberg (1995) procedure.
#'     \item \strong{"BY"}: Benjamini-Yekutieli (2001) procedure.
#'   }
#' }
#' @param signal_significance_threshold A decimal indicating the hypothesis testing zero-alpha null-hypothesis rejection criteria. If you want to select all `chosen_signals`, provide 1.
#' @param heuristic_sb_metric A signal in `signals_m_df` used for `sb_algorithms` "SW" and "MTO". It selects which signal will be elected in case of theme representativeness when no signal in a given theme was deemed significant.
#' @param max_abs_active_group_weight A decimal indicating the maximum absolute weight that a theme of signals can have in the final portfolio.
#' @param priors_m_df A (meta) data frame with columns including "id", "ticker", "dates", "theme" (used for clustering in the Bayesian hierarchical model),
#' and values for active_return, bench_return, alpha (mean and se), beta (mean and se), and sigma. Data should be exogenous, as it will be used to set priors for the hierarchical Bayesian model.
#' @param priors_type A flag indicating which priors should be set. Possible options are:
#' \itemize{
#'   \item \strong{"all"}: Set priors for all parameters, including mean (`mu`), variance (`tau`), and correlation based on `priors_m_df` data.
#'   \item \strong{"mean"}: Set priors only for mean (`mu`).
#'   \item \strong{"uninformative"}: Set uninformative priors for all parameters.
#'   \item \strong{"user"}: Set priors defined by the user.
#' }
#' @param signal_themes_m_df A (meta) data frame with "id", "tickers" ("signals"), and "dates" columns, including all signals in `signals_m_df`, and a "theme" column providing group membership for each signal.
#' @param user_priors_list A list with user-defined priors for the hierarchical Bayesian model, used when `priors_type = "user"`.
#' Each element of the list represents a theme and should contain priors set with brms::set_prior (class `brmsprior`). For instance, setting priors only for
#' alpha and beta global parameters can be done with:
#' c(brms::set_prior("normal(0,1)", class = "Intercept"), brms::set_prior("normal(0,1)", class = "b", coef = "bench_return"))
#'
#' @param upper_quantile_winsorization Numeric value for upper winsorization.
#' @param lower_quantile_winsorization Numeric value for lower winsorization.
#' @param verbose A boolean indicating whether to print messages.
#'
#' @section Bayesian Hierarchical Model
#'
#' One way of introducing shrinkage to alpha estimates from signal portfolios is by using bayesian statistics, which might be specially useful in the context
#' of small samples of strategy returns. Bayesian statistics allows for the incorporation of prior information about the parameters of interest (alpha and beta),
#' which can be particularly useful in multiple testing.
#'
#' A Bayesian model depends on parameters that are themselves random variables. For instance, one can assume a normal distribution for the CAPM alpha parameter,
#' with a mean of 0 (the strategy is not profitable) and a given standard deviation, effectively shrinking posterior estimates towards the prior mean, with an
#' intensity that depends on the prior standard deviation and the sample distribution. Alternatively, one can utilize priors derived from signal strategies
#' from other countries or asset classes than the ones being studied. Either way, the researcher will incorporate the reasoning process people usually have, as
#' we usually have prior beliefs about a subject and then will update our beliefs based on new information. If another research possesses other priors,
#' he or she can incorporate those into Bayes rule and derive other posterior distribution for parameters of interest, which is kind of more
#' transparent than the frequentist approach, who will inherently give ultimate importance to associational evidence from (possibly overfit) data.
#' Most of the time, in a frequentist setting, the researcher usually does not disclose the reasoning process behind the model. Therefore, bayesian statistics
#' are being often recommended as a method to deal with multiple testing problems.
#'
#' Suppose one has return observations for multiple signal portfolios across time and wants to estimate the parameters of a single-factor model (CAPM)
#' that describes the relationship between these signals portfolios and the market factor, being interested in the significance of the alpha parameter, while
#' accounting for its exposure to systematic market risk. A complete pooled model will treat all observations as being independent and treating as irrelevant any
#' information about individual strategies, which might be misleading, because it does not account for the hierarchical structure of the data.
#' Observations of a given signal portfolio are possible correlated. On the other hand, a no pooling model will estimate a separate model for each signal portfolio,
#' fitting specific parameters for each individual strategy, which might be inefficient, as it does not borrow information across signals. Signals belonging
#' to a given theme tend to be similar. Signals in the value theme, for instance, are usually valuation multiples (eg book yield, earnings yield, fcf yield, sales yield etc)
#' and so are very similar, thus it would be unfortunate to ignore information from other value strategies when analyzing, for example, book yield alpha.
#' By fitting a hierarchical model, alpha estimates are first shrunk towards theme mean and then towards priors. In this setting, there is top layer represented
#' by the population of all signal strategies under a theme (eg. the value theme alpha) and a second layer for the backtested strategies that fall under that theme,
#' for which he have repeated observations.
#'
#' By using a hierarchical model, one can study both within-signal variability, examining how consistent the signal is across time, and between-signal variability,
#' examining how performance patterns vary across different signals in a theme. Total variance is given by sum of within-signal variance and between-signal variance.
#'
#' Threfore, there are three layers for R_s,t (t-th observation of return of signal s):
#' \itemize{
#'  \item{Signal-specific layer:}{R_s,t ~ N(alpha_s + beta_s * R_m,t, sigma_s^2). This represents how returns vary within strategy s}
#'  \item{Theme-specific layer:}{alpha_s ~ N(alpha_theme, sigma_a_theme^2); beta_s ~ N(beta_theme, sigma_b_theme^2).
#'   This represents how the typical alpha/beta vary across strategies in a theme}
#'  \item{Priors:}{alpha_theme, sigma_a_theme, beta_theme, sigma_b_theme, sigma_s. Global parameters shared between strategies.}
#'  }
#' More specifically, signal-specific mean parameters are treated as deviations from global parameters (u_1, u_2 and u_sigma for alpha, beta and sigma respectively,
#' with corresponding mean and standard deviations mu_u1, tau_u1,  mu_u2, tau_u2, mu_u_sigma and tau_u_sigma).
#' If the user provide a `priors_m_df`, the function will, for each theme in respective column (that should match possible options in `signal_themes_m_df`):
#' \itemize{
#'    \item{For each date, calculate the average and standard deviation of individual signals alphas/betas/sigmas. Based on these average values,
#'    a prior for overall alpha parameters will be chosen based on maximum likelihood}
#'    \item{For each date, calculate the differential of individual signals alpha/betas/sigmas from overall counterparts. For alpha, this means
#'     getting mu_u1 and tau_u1, mean and standard deviation of differentials of individual signals to overall mean.
#'     In particular, tau_u1 measures alpha variability between signals, the dispersion of differentials of individual signals from theme alpha.}
#'     \item{Use maximum likelihood to derive priors for each tau and also for correlation, according to `priors_type`}
#'  }
#'
#' For location parameters, priors are chosen between normal and t distributions, given the option that minimizes BIC. For scale parameters,
#' other candidate distributions are cauchy, inverse-gamma and log-normal. For correlation, the LKJ distribution is used.
#'
#' By considering the hierarchical structure of the data, it is possible to borrow information across signals and, thus, hierarchical models are better at
#' balancing bias and variance than estimates from complete pooling (high bias) or no pooling models (high variance).
#'
#' To speed up computation, Bayesian models are fitted in parallel using the `future` framework.
#'
#' @section Signal Engineering Benchmarks
#'
#' The process of generating a final signal (also known as Signal Engineering) incorporates two steps:
#' \itemize{
#'   \item \strong{Signal Selection}: Selecting signals deemed significant based on a hypothesis testing zero-alpha null-hypothesis rejection criteria applied to associated signal portfolios returns in `backtest_returns_df`.
#'   \item \strong{Signal Blending}: Blending selected signals into a final signal used to generate the final portfolio at the stock level.
#' }
#'
#' The Signal Engineering Benchmarks (SE Benchmarks) evaluate the performance of both steps:
#' \itemize{
#'   \item \strong{Signal Selection Benchmark}: Built using the universe of all signals in `chosen_signals`. It evaluates the performance of the signal selection process.
#'   \item \strong{Signal Blending Benchmark}: Built using only signals derived from the signal selection process. It evaluates the performance of the signal blending process.
#' }
#'
#' Comparing predictive and return performance of the SS and SB Benchmarks provides insights into the effectiveness of the signal selection process. Additionally, comparing performance between the SB Benchmark and the final portfolio evaluates the performance of the signal blending process. SE Benchmarks are built based on themes; weights are first equally distributed among themes and then equally distributed among signals within each theme.
#'
#' @section MTO Constraint Policy
#'
#' When `sb_algorithm` is "MTO", you can set a `concentration_constraint_policy` to impose diversification throughout the signal engineering process. In this case, `signal_themes_m_df`, `heuristic_sb_metric`, and `max_abs_active_group_weight` are used to guarantee theme representativeness at the signal selection step. If no signal in a given theme is deemed significant, the signal with the highest value of `heuristic_sb_metric` is elected.
#'
#' @details
#' The function performs the following operations:
#' \itemize{
#'   \item Extracts the user-defined chosen signals from `signals_m_df` and subsets the data frame to include only these signals.
#'   \item Checks for consistency between the length of `chosen_signals` and `signal_positions` and ensures that all chosen signals have corresponding positions.
#'   \item Adjusts the signal positions based on whether they are "short" by multiplying their values by -1.
#'   \item Updates column names in the data frame to reflect the corrected positions of the signals.
#'   \item Validates that all adjusted signals have corresponding columns in `backtest_returns_df`.
#'   \item Calculates time-series metrics from backtested returns, including mean active return, IR, alpha, t-stat, beta, Treynor ratio, and p-value. Signals without adequate data (less than `data_availability_cutoff` periods) are removed.
#'   \item Adjusts p-values based on the method selected by the user.
#'   \item Defines which signals are eligible for blending.
#' }
#'
#' @return A list containing eligible signals and signal universes throughout time.
#' @export
run_ss_backtest_internal <- function(
    #Dates
    rebalancing_months, data_availability_cutoff = 60, split_method = "expanding",
    #Signals
    signals_m_df, chosen_signals, signal_positions,
    #Backtests and benchmarks
    backtest_returns_df, benchmark_returns_df, market_factor_proxy = "IBOV",
    #P-value
    p_correction_method = "none", signal_significance_threshold = 0.05,
    #Eligiblity for MTO Constraints
    heuristic_sb_metric = "IR", max_abs_active_group_weight,
    #Bayesian variables
    priors_m_df = NULL, priors_type = "all", signal_themes_m_df = NULL, user_priors_list = NULL,
    #Winsorization
    lower_quantile_winsorization = 0.025, upper_quantile_winsorization = 0.975,
    verbose = TRUE
    ){

  #Create structures to get results
  signal_universe_m_d_ref_list <- list()
  bayesian_fit_nested_list <- list()
  eligible_signals_list <- list()


  #Measure time to run and run gc
  elapsed_time <- system.time({

    #Extract dates
    dates_m_vector <- unique(as.Date(signals_m_df$dates, format = "%Y-%m-%d")) #coerce just to be sure
    dates_m_vector <- dates_m_vector[order(dates_m_vector)] #Re-order ascending just to be sure

    #Backtest length
    backtest_length <- length(dates_m_vector) - data_availability_cutoff + 1 #calculate backtest_length

    #Rebalancing Dates
    dates_backtest <- dates_m_vector[data_availability_cutoff:
                                    (data_avaialability_cutoff + backtest_length - 1)] #These are dates inside backtest

    first_rebalance_date <- min(dates_backtest) #Get first rebalancing date
    rebalance_dates <- unique( #Unique is to eliminate repeated dates, in case month of first_rebalance_date is a rebalancing month
      c(first_rebalance_date, dates_backtest[which(lubridate::month(dates_backtest) %in% rebalancing_months)]) #Dates corresponding to rebalancing_months
    )
    rebalance_dates <- rebalance_dates[order(rebalance_dates)] #Re-order

    #Number of rebalancing months
    n_rebalance_months <- length(rebalance_dates)

    #Last rebalance date
    last_rebalance_date <- max(rebalance_dates)


    #################
    ##Check Parameters
    check_inputs_ss_backtest(
      rebalancing_months = rebalancing_months, data_availability_cutoff = data_availability_cutoff,
      signals_m_df = signals_m_df, chosen_signals = chosen_signals, signal_positions = signal_positions,
      backtest_returns_df = backtest_returns_df, selected_benchmark_returns_df = selected_benchmark_returns_df,
      p_correction_method = p_correction_method, signal_significance_threshold = signal_significance_threshold,
      priors_m_df = priors_m_df, priors_type = priors_type, signal_themes_m_df = signal_themes_m_df
    )

    #Initial Prints
    if(verbose){
      cat("\n")
      cat(crayon::cyan(paste("Starting signal selection backtest")))
      cat("\n")
      cat(paste("Factor model: CAPM with", market_factor_proxy, "as proxy for market factor"))
      cat("\n")
      cat(crayon::yellow(paste("P-values will be adjusted following the", p_correction_method, "method")))
      cat("\n")
      cat(paste("Signal significance threshold set as", signal_significance_threshold))
      cat("\n")

    }


    #################

    ##Select signals, benchmark and respective backtests based on user choice
    ########################
    selected_signals_and_backtest_list <- select_and_correct_signals(
      #signals_m_df
      signals_m_df = signals_m_df,
      #Chosen signals and positions
      chosen_signals = chosen_signals, signal_positions = signal_positions,
      #Signals backtest
      backtest_returns_df = backtest_returns_df
    )

    ###Selected signals_m_df with corrected positions
    selected_signals_corrected_positions_m_df <- selected_signals_and_backtest_list$selected_signals_corrected_positions_m_df
    ###Selected signals backtest returns
    selected_backtest_returns_corrected_positions_df <- selected_signals_and_backtest_list$selected_backtest_returns_corrected_positions_df

    ###Check if both are contemplated in signal_themes
    if(any(!colnames(dplyr::select(selected_signals_corrected_positions_m_df, -id, -tickers, -dates)) %in% signal_themes_m_df$tickers)){
      stop("all selected signals (with corrected positions) should have a theme classification in signal_themes_m_df")
    }
    if(!is.null(selected_backtest_returns_corrected_positions_df)){
      if(
        any(!colnames(dplyr::select(selected_backtest_returns_corrected_positions_df, -dates)) %in% signal_themes_m_df$tickers)){
        stop("all selected signals in backtests (with corrected positions) should have a theme classification in signal_themes_m_df")
      }
    }

    ###Select market factor proxy from benchmark returns df
    selected_market_factor_proxy_df <- benchmark_returns_df[, c("dates", market_factor_proxy)]
    ########################

    ##Start Backtest##
    #################
    for(d in data_availability_cutoff:(data_availability_cutoff + backtest_length - 1)){
      #Extract date and references
      current_date <- dates_m_vector[d]
      upd_ref <- which(as.Date(signals_m_df$dates,  format = "%Y-%m-%d") <= current_date) #Get upd_ref

      #Subset signals, backtest, market factor and priors
      selected_signals_corrected_positions_m_upd_ref <- selected_signals_corrected_positions_m_df[upd_ref,]
      selected_backtest_returns_corrected_positions_upd_ref <- selected_backtest_returns_corrected_positions_df[which(selected_backtest_returns_corrected_positions_df$dates <= current_date), ]
      selected_market_factor_proxy_upd_ref <- selected_market_factor_proxy_df[which(selected_market_factor_proxy_df$dates <= current_date), ]
      priors_m_upd_ref <- priors_m_df[which(priors_m_df$dates <= current_date), ]
      signal_themes_m_d_ref <- signal_themes_m_df[which(signal_themes_m_df$dates == current_date), ]

      #Check if it's a rebalancing month
      if((lubridate::month(current_date) %in% rebalancing_months) || d == (data_avaialability_cutoff)){
        #Print refitting message
        if(verbose){
          cat("\n")
          cat(crayon::yellow(paste("Running signal selection backtest at:", current_date)))
          cat("\n")
        }

        ###Elect signals
        signal_eligibility_results_list <- define_signal_eligibility(
          #Backtests
          selected_backtest_returns_corrected_positions_upd_ref = selected_backtest_returns_corrected_positions_upd_ref,
          selected_market_factor_proxy_upd_ref = selected_market_factor_proxy_upd_ref,
          #Data Avaialability
          data_availability_cutoff = data_availability_cutoff,
          #P adjustment
          p_correction_method = p_correction_method, signal_significance_threshold = signal_significance_threshold,
          #Theme Representativeness Eligibiility
          max_abs_active_group_weight = max_abs_active_group_weight, heuristic_sb_metric = heuristic_sb_metric,
          #Bayesian method
          priors_type = priors_type, priors_m_upd_ref = priors_m_upd_ref, signal_themes_m_d_ref = signal_themes_m_d_ref,
          user_priors = user_priors
        )
        ###Get results
        signal_universe_m_d_ref <- signal_eligibility_results_list$signal_universe_m_d_ref
        bayesian_fit_list <- signal_eligibility_results_list$bayesian_fit_list

        ###Print results
        if(verbose){
          cat("\n")
          cat(crayon::green(paste0("Eligible signals defined: ")))
          cat(signal_universe_m_d_ref$tickers[which(signal_universe_m_d_ref$is_eligible == 1)])
        }

      ###Set results
      signal_universe_m_d_ref_list[[which(rebalance_dates %in% current_date)]] <-  signal_universe_m_d_ref
      bayesian_fit_nested_list[[which(rebalance_dates %in% current_date)]] <- bayesian_fit_list
      eligible_signals_list[[d - data_availability_cutoff + 1]] <- signal_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::select(tickers)


      } #End rebalancing month

    #When there is no rebalancing, just repeat eligible signals from last month
    eligible_signals_list[[d - data_availability_cutoff + 1]] <- signal_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::select(tickers)
    } #End loop

    #Get workflow
    ss_backtest_workflow <- list(
      config_name = "not_identified",
      backtest_identifier = "not_identified",
      backtest_type = if(p_correction_method == "bayesian") "bayesian" else "frequentist",
      #Hypothesis tests
      p_correction_method = p_correction_method,
      signal_significance_threshold = signal_significance_threshold,
      market_factor_proxy = market_factor_proxy,
      data_availability_cutoff = data_availability_cutoff,
      #MTO constraints
      heuristic_sb_metric = heuristic_sb_metric,
      max_abs_active_group_weight = max_abs_active_group_weight,
      #Bayesian
      priors_type = priors_type,
      user_priors_list = user_priors_list,
      #Dates
      dates_covered = dates_m_vector,
      n_dates = length(dates_m_vector),
      dates_backtest = dates_backtest,
      rebalance_dates = rebalance_dates,
      n_rebalance_months = n_rebalance_months,
      first_rebalance_date = first_rebalance_date,
      split_method = split_method,
      #Signals
      chosen_signals = chosen_signals,
      signal_positions = signal_positions,
      selected_signals_corrected_positions = colnames(selected_backtest_returns_corrected_positions_upd_ref)[-1],
      n_signals = length(chosen_signals),
      signals_workflow = NULL,
      signals_object = "not_identified",
      signal_themes_object = "not_identified",
      priors_object = "not_identified",
      backtest_returns_object = "not_identified",
      benchmark_returns_object = "not_identified",
      #Performance
      elapsed_time = elapsed_time,
      timestamps = c(initialization = Sys.time()),
      #Call
      call = match.call()
    )


    #Get final object
    ss_backtest_results <- new("ss_backtest_results",
                               backtest_type = ss_backtest_workflow$backtest_type,
                               signal_universe_m_d_ref_list = signal_universe_m_d_ref_list,
                               bayesian_fit_nested_list = bayesian_fit_nested_list,
                               eligible_signals_list = eligible_signals_list,
                               ss_backtest_workflow = ss_backtest_workflow
                               )

    })
    #End timer
    print(elapsed_time)

    #Return
    return(ss_backtest_results)

}




