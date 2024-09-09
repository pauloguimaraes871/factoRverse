#' Define Signal Eligibility
#'
#' This function evaluates the eligibility of signals for inclusion in the investment universe based on various performance metrics and statistical adjustments. It performs initial data checks, computes performance metrics, applies statistical adjustments (both frequentist and Bayesian), and classifies signals according to the specified selection policy.
#'
#' @param selected_signals_backtest_returns_upd_ref A data frame containing backtest returns for various signals. The first column should be identifiers for the signals, and the subsequent columns should contain the returns data.
#' @param selected_benchmark_returns_upd_ref A data frame containing benchmark returns data. The first column should be identifiers for the benchmarks, and the subsequent columns should contain the returns data.
#' @param signal_selection_policy A list containing the signal selection policy. This list should include:
#' \describe{
#'   \item{\code{data_availability_cutoff}}{The minimum number of non-NA observations required for a backtest to be considered.}
#'   \item{\code{p_correction_method}}{The method for p-value correction ("none", holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr" or "bayesian").}
#'   \item{\code{benchmark}}{The benchmark to use for performance comparisons.}
#'   \item{\code{chosen_prior}}{Dataset used to build prior for Bayesian adjustments.}
#'   \item{\code{priors_type}}{A flag indicating whether priors should be set ("none" if not).}
#'   \item{\code{chosen_sb_metric}}{The signal to use for final selection after adjustments.}
#'   \item{\code{sb_benchmark_weighting}}{The benchmark weighting for the concentration constraint policy.}
#'   \item{\code{max_abs_active_group_weight}}{The maximum absolute weight for any group in the concentration constraint policy.}
#' }
#' @param signals_groups_d_ref An optional data frame that maps signals to groups for Bayesian modeling and concentration constraints. It should contain a column named \code{theme} that categorizes the signals.
#' @param priors_m_d_ref An optional list of prior distributions for Bayesian adjustments. The list should contain prior data frames or objects indexed by names corresponding to \code{chosen_prior}.
#'
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
#'   \item{\code{final_signal}}{The final signal classification after applying transformations and adjustments.}
#' }
#' @export
define_signal_eligibility <- function(selected_signals_backtest_returns_upd_ref, selected_benchmark_returns_upd_ref,
                                      signal_selection_policy,
                                      signals_groups_m_d_ref = NULL,
                                      selected_priors_informative_data_m_upd_ref = NULL,
                                      upper_quantile_winsorization = 0.975, lower_quantile_winsorization = 0.025){


  #Initial Preparations
  ##################
  ###Get objects from signal_selection_policy
  data_availability_cutoff <- signal_selection_policy$data_avaialability_cutoff
  p_correction_method <- signal_selection_policy$p_correction_method
  priors_type <- signal_selection_policy$priors_type
  user_priors_list <- signal_selection_policy$user_priors_list
  chosen_sb_metric <- signal_selection_policy$chosen_sb_metric
  ###Get objects from selected_signals_backtest_returns_upd_ref
  selected_signals <- colnames(selected_signals_backtest_returns_upd_ref)[-1]
  current_date <- selected_signals_backtest_returns_upd_ref$dates[length(selected_signals_backtest_returns_upd_ref$dates)]
  ###Get selected_benchmark_returns_vector_upd_ref
  selected_benchmark_returns_vector_upd_ref <- selected_benchmark_returns_upd_ref[,2]
  ##################

  #Create signal_universe_m_d_ref
  #################################
  signal_universe_m_d_ref <- data.frame(
    #ID
    id = paste0(selected_signals,"-",current_date),
    #Tickers
    tickers = selected_signals,
    #Dates
    dates = current_date,
    #Mean Active Return
    mean_active_return = selected_signals_backtest_returns_upd_ref[,-1] %>% apply(2, function(x) mean(x, na.rm = TRUE)),
    #Tracking Error
    tracking_error =  selected_signals_backtest_returns_upd_ref[,-1] %>% apply(2, function(x) sd(x, na.rm = TRUE)),
    #Information Ratio
    IR = selected_signals_backtest_returns_upd_ref[,-1] %>% apply(2, function(x) mean(x, na.rm = TRUE)/sd(x, na.rm = TRUE)), #Calculate IR
    #Alpha
    alpha = selected_signals_backtest_returns_upd_ref[,-1] %>% apply(2, function(x){
      summary(lm(x ~ selected_benchmark_returns_vector_upd_ref))$coefficients[1]}), #Calculate OLS Capm Alpha
    #AP (T-STAT)
    AP = selected_signals_backtest_returns_upd_ref[,-1] %>% apply(2, function(x){
      summary(lm(x ~ selected_benchmark_returns_vector_upd_ref))$coefficients[5]}), #Get Alpha t-stat
    #Beta
    beta = selected_signals_backtest_returns_upd_ref[,-1] %>% apply(2, function(x){
      summary(lm(x ~ selected_benchmark_returns_vector_upd_ref))$coefficients[2]}), #Get Beta
    #Treynor
    treynor = selected_signals_backtest_returns_upd_ref[,-1] %>% apply(2, function(x){
      mean(x, na.rm = TRUE)/summary(lm(x ~ selected_benchmark_returns_vector_upd_ref))$coefficients[2]}), #Get Treynor
    #Alpha P-value
    p_value = selected_signals_backtest_returns_upd_ref[,-1] %>% apply(2, function(x){
      summary(lm(x ~ selected_benchmark_returns_vector_upd_ref))$coefficients[7]}), #Get Alpha p-value

    row.names = NULL
  )

  #Correct based on backtest length
    ##Check if backtests have enough length to be considered
    cutted_out_backtests <- selected_signals_backtest_returns_upd_ref[,-1] %>% apply(2, function(col){
      length(which(is.na(col))) >= data_availability_cutoff
    })

    ##Send warning
    if(any(cutted_out_backtests)){
      warning(paste0("The following signals backtests have less periods than data_avaiability_cutoff and will not be used: ",
                     names(cutted_out_backtests[which(cutted_out_backtests)])))
    ##Ignore signals that do not have enough data
      signal_universe_m_d_ref[which(cutted_out_backtests), -c(1:3)] <- NA #NA reflects lack of knowledge about signal behavior
    }
  #################################

  #P-adjust!
  ############################

  #Frequentist version
  ######################
  if(!p_correction_method == "bayesian"){
    #Frquentist adjustment
    signal_universe_m_d_ref$adjusted_p_value <- p.adjust(signal_universe_m_d_ref$p_value, method = p_correction_method) #Frequentist p-value adjust

    #Elect final signal for signal_universe_m_d_ref
    signal_universe_m_d_ref[, "final_signal"] <- signal_transform(
      signal_universe_m_d_ref[, chosen_sb_metric],
      upper_quantile_winsorization = upper_quantile_winsorization,
      lower_quantile_winsorization = lower_quantile_winsorization
    )
  #######################


  } else {
    #Beware of the ALMIGHTY Bayesian model
    ######################################

    #Bayesian adjustment
    bayesian_adjustment_results_list <- bayesian_adjustment(
      #Signals and benchmark
      signal_universe_m_d_ref = signal_universe_m_d_ref,
      selected_signals_backtest_returns_upd_ref = selected_signals_backtest_returns_upd_ref,
      selected_benchmark_returns_vector_upd_ref = selected_benchmark_returns_vector_upd_ref,
      #Priors
      selected_priors_informative_data_m_upd_ref = selected_priors_informative_data_m_upd_ref,
      priors_type = priors_type,
      user_priors_list = user_priors_list,
      #Grou´s
      signals_groups_m_d_ref = signals_groups_m_d_ref
    )

      ##Get results from bayesian adjustment
      signal_universe_m_d_ref <- bayesian_adjustment_results_list$posterior_signal_universe_m_d_ref
      bayesian_fit_list <- bayesian_adjustment_results_list$bayesian_fit_list


    #Elect final signal for signal_universe_m_d_ref
    signal_universe_m_d_ref[, "final_signal"] <- signal_transform(
      signal_universe_m_d_ref[, paste0("posterior_", chosen_sb_metric)], #Final signal
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
    signal_significance_threshold = signal_selection_policy$signal_significance_threshold, #Signal Significance Threshold
    groups_m_d_ref = signals_groups_m_d_ref, #Groups to select
    #Build concentration constraint policy for signals
    concentration_constraint_policy =  list(
      benchmark = signal_selection_policy$sb_benchmark_weighting, #Reference benchmark
      max_abs_active_group_weight = signal_selection_policy$max_abs_active_group_weight), #Max group weight for signal
    asset_object = "signals"
  )
  ################################

  signal_eligibility_results_list <- list()
  signal_eligibility_results_list$signal_universe_m_d_ref <- signal_universe_m_d_ref
  try(signal_eligibility_results_list$bayesian_fit_list <- bayesian_fit_list, silent = TRUE)

  return(signal_eligibility_results_list)

}
