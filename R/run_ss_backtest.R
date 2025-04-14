#' Update Signal Selection Backtest
#' The `update_ss_backtest` function will take an existing `port_backtest_results` object and update it with
#' new dates. This function is useful when you want to add new dates to an existing backtest without having to re-run the entire backtest.
#'
#' @param signals_m_df A meta_dataframe containing the signal features. It must include at least the columns \code{id}, \code{tickers}, and \code{dates}.
#' @param updated_backtest_returns_m_xts An up-to-date xts containing historical backtested returns named according to signals in `signals_m_df`,
#' @param updated_port_backtest_cohort An up-to-date `port_backtest_cohort` object containing historical backtested returns named according to signals in `signals_m_df`,
#' @param benchmark_returns_m_xts A xts with benchmark returns, named accordingly.
#' @param signal_themes_m_df A (meta) data frame with "id", "tickers" ("signals"), and "dates" columns, including all signals in `signals_m_df`, and a "theme" column providing group membership for each signal.
#' @param old_results An object of class \code{ss_backtest_results} to be updated with new results.
#' @param priors_m_df A (meta) data frame with columns including "id", "ticker", "dates", "theme" (used for clustering in the Bayesian hierarchical model),
#' and values for active_return, bench_return, alpha (mean and se), beta (mean and se), and sigma. Data should be exogenous, as it will be used to set priors for the hierarchical Bayesian model.
#' @param custom_signal_universe_metrics_m_df A \code{meta_dataframe} containing user-defined metrics for the signal universe, used for custom filtering or classification.
#' @param verbose A boolean indicating whether to print messages.
#' @param parallel A boolean indicating whether to run the backtest in parallel.
#' @param ... Additional arguments (not used in this method).
#'
#' @return An object of class \code{ss_backtest_results} containing the portfolio backtest results.
#'
#' @export
setGeneric("update_ss_backtest", function(signals_m_df, updated_backtest_returns_m_xts, updated_port_backtest_cohort, benchmark_returns_m_xts, signal_themes_m_df, old_results, ...){
  standardGeneric("update_ss_backtest")
})

#' @describeIn update_ss_backtest Updates a signal selection backtest based on a \code{ss_backtest_results} object and a \code{port_backtest_cohort}.
#'
#' This method extracts the parameters from the \code{results} object (of class \code{ss_backtest_results}), modifies initial_sample_size, performs the
#' new backtest and then binds results to the old results.
#'
#' @export
setMethod("update_ss_backtest",
          signature(signals_m_df = "meta_dataframe", updated_backtest_returns_m_xts = "missing", updated_port_backtest_cohort = "port_backtest_cohort",
                    benchmark_returns_m_xts = "meta_xts", signal_themes_m_df = "meta_dataframe", old_results = "ss_backtest_results"),

          function(signals_m_df, updated_port_backtest_cohort, benchmark_returns_m_xts, signal_themes_m_df, old_results, #Base objects
                   priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL, #Auxiliary objects
                   verbose = TRUE, parallel = TRUE){

            #Extract backtest_returns_m_xts from updated_port_backtest_cohort
            #######################
              ##Check if cohort name matches expectation
              if (is.null(old_results@ss_backtest_workflow[[length(old_results@ss_backtest_workflow)]]$port_backtest_cohort_object_name)){
                stop("port_backtest_cohort_object_name in old_results is NULL, but updated_port_backtest_cohort is not")
              }
              if (updated_port_backtest_cohort@cohort_name !=
                  old_results@ss_backtest_workflow[[length(old_results@ss_backtest_workflow)]]$port_backtest_cohort_object_name){
                stop("The updated port_backtest_cohort object does not match the original port_backtest_cohort object.")
              }

              ##Run extraction
              extracted_returns_m_xts <- extract_returns_m_xts(
                port_backtest_cohort = updated_port_backtest_cohort,
                signals_m_df = signals_m_df, benchmark_returns_m_xts = benchmark_returns_m_xts,
                verbose = verbose
              )
              ##Assign extracted values
              updated_backtest_returns_m_xts <- extracted_returns_m_xts$backtest_returns_m_xts
              benchmark_returns_m_xts <- extracted_returns_m_xts$benchmark_returns_m_xts

            #######################

            #Update the backtest
            #######################
              updated_ss_backtest_results <- update_ss_backtest(
                signals_m_df = signals_m_df,
                updated_backtest_returns_m_xts = updated_backtest_returns_m_xts, benchmark_returns_m_xts = benchmark_returns_m_xts, #Backtest returns
                signal_themes_m_df = signal_themes_m_df, #Themes
                priors_m_df = priors_m_df, custom_signal_universe_metrics_m_df = custom_signal_universe_metrics_m_df,
                old_results = old_results,
                verbose = verbose, parallel = parallel
              )

              ###Add cohort name
              updated_ss_backtest_results@ss_backtest_workflow[[length(updated_ss_backtest_results@ss_backtest_workflow)]]$port_backtest_cohort_object_name <-
                updated_port_backtest_cohort@cohort_name


            #######################

            return(updated_ss_backtest_results)


          })

#' @describeIn update_ss_backtest Updates a signal selection backtest based on a \code{ss_backtest_results} object and a \code{backtest_returns_m_xts}.
#'
#' This method extracts the parameters from the \code{results} object (of class \code{ss_backtest_results}), modifies initial_sample_size, performs the
#' new backtest and then binds results to the old results.
#'
#' @export
setMethod("update_ss_backtest",
          signature(signals_m_df = "meta_dataframe", updated_backtest_returns_m_xts = "meta_xts", updated_port_backtest_cohort = "missing",
                    benchmark_returns_m_xts = "meta_xts", signal_themes_m_df = "meta_dataframe", old_results = "ss_backtest_results"),

          function(signals_m_df, updated_backtest_returns_m_xts, benchmark_returns_m_xts, signal_themes_m_df, old_results, #Base objects
                   priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL, #Auxiliary objects
                   verbose = TRUE, parallel = TRUE){

          #Get old ss workflow
          old_ss_workflow_last_batch <- old_results@ss_backtest_workflow[[length(old_results@ss_backtest_workflow)]]
          #Check adherence between new objects and old object (names and dates)
          #######################
            ##Check if current_date is equal to old_results current_date + 1 months
            if(signals_m_df@current_date != ##It will be checked that current_date matches across new objects in update_ss_backtest
               lubridate::add_with_rollback(old_ss_workflow_last_batch$current_date, months(1))){
              stop("The current_date in the new signals_m_df is not equal to the current_date in the old_results + 1 month")
            }
            ##Backtest returns m xts
            if (!identical(updated_backtest_returns_m_xts@meta_xts_name, old_ss_workflow_last_batch$backtest_returns_object_name)){
              stop("The updated_backtest_returns_m_xts object does not match the backtest_returns_object_name in the old_results object")
            }
            if (updated_backtest_returns_m_xts@current_date != lubridate::add_with_rollback(old_ss_workflow_last_batch$current_date, months(1))){
              stop("The current_date in the updated_backtest_returns_m_xts object is not equal to the current_date in the old_results object + 1 month")
            }

            ##Gather all arguments into a single named list (only those that have @meta_dataframe_name or meta_xts_name)
            new_objects_list <- list(
              signals_m_df = signals_m_df,
              backtest_returns_m_xts = updated_backtest_returns_m_xts,
              benchmark_returns_m_xts = benchmark_returns_m_xts,
              signal_themes_m_df = signal_themes_m_df,
              priors_m_df = priors_m_df,
              custom_signal_universe_metrics_m_df = custom_signal_universe_metrics_m_df
            )

            old_objects_names_list <- list(
              signals_m_df = old_ss_workflow_last_batch$signals_object_name,
              backtest_returns_m_xts = old_ss_workflow_last_batch$backtest_returns_object_name,
              benchmark_returns_m_xts = old_ss_workflow_last_batch$benchmark_returns_object_name,
              signal_themes_m_df = old_ss_workflow_last_batch$signal_themes_object_name,
              priors_m_df = old_ss_workflow_last_batch$priors_object_name,
              custom_signal_universe_metrics_m_df = old_ss_workflow_last_batch$custom_signal_universe_metrics_object_name
            )

            old_objects_dates_covered_list <- list(  ##Baseline info for dates comparison
              signals_m_df = old_ss_workflow_last_batch$signals_dates,
              backtest_returns_m_xts = old_ss_workflow_last_batch$backtest_returns_dates_covered,
              benchmark_returns_m_xts = old_ss_workflow_last_batch$benchmark_returns_dates_covered,
              signal_themes_m_df = old_ss_workflow_last_batch$signal_themes_dates,
              priors_m_df = old_ss_workflow_last_batch$priors_dates,
              custom_signal_universe_metrics_m_df = old_ss_workflow_last_batch$custom_signal_universe_dates
            )

            ##Perform check
            check_update_backtest_objects(new_objects_list = new_objects_list, old_objects_names_list = old_objects_names_list,
                                          old_objects_dates_covered_list = old_objects_dates_covered_list, n_update = 1)

          #######################

          #Update old_ss_backtest_config
          #######################
          new_config <- old_results@ss_backtest_config
          #We are in 2023-05-15. We then just update forward 1 date to 2023-06-15. There are not fwd objects for ss, making it simple
          new_config@initial_sample_size <- old_ss_workflow_last_batch$n_dates + 1  #New update must happen at last date of last backtest + 1

            ##Check if new initial_buffer_period is equal to length(zoo::index(updated_backtest_returns_m_xts@data))
            if(new_config@initial_sample_size != length(zoo::index(updated_backtest_returns_m_xts@data))){
              stop("The new initial_sample_size is not equal to amount of unique dates in updated_backtest_returns_m_xts")
            }

            ##Get old winsorization probs
            winsorization_probs <- sort(c(old_ss_workflow_last_batch$lower_quantile_winsorization, old_ss_workflow_last_batch$upper_quantile_winsorization))

          #######################

          #Re-run!!
          #######################
          updated_ss_backtest_results <- run_ss_backtest(
            config = new_config,
            ###Base SS Backtest Obj
            signals_m_df = signals_m_df, backtest_returns_m_xts = updated_backtest_returns_m_xts, benchmark_returns_m_xts = benchmark_returns_m_xts,
            signal_themes_m_df = signal_themes_m_df, priors_m_df = priors_m_df, custom_signal_universe_metrics_m_df = custom_signal_universe_metrics_m_df,
            ###Other
            verbose = verbose, parallel = parallel, winsorization_probs = winsorization_probs,
            .update = TRUE
          )
          #######################

          #Consolidate results
          #######################
           ##results objects
           new_ss_backtest_outputs_list <- list(
             signal_universe_m_df = updated_ss_backtest_results@signal_universe_m_df
           )

           ##Consolidate
           updated_results_list <- consolidate_backtest_results(new_backtest_outputs_list = new_ss_backtest_outputs_list,
                                                                old_backtest_results = old_results)

           ##Reassign the updated objects back into 'updated_ss_backtest_results'
           ##Note that one does not need to consolidate final_signal_universe_m_d_ref
           updated_ss_backtest_results@signal_universe_m_df <- updated_results_list[["signal_universe_m_df"]]



           ##In case of an empty update
           if (is.null(updated_ss_backtest_results@final_signal_universe_m_d_ref)){
             updated_ss_backtest_results@frequentist_results <- old_results@frequentist_results
             updated_ss_backtest_results@bayesian_results <- old_results@bayesian_results
             updated_ss_backtest_results@signal_universe_m_df <- old_results@signal_universe_m_df
             updated_ss_backtest_results@final_signal_universe_m_d_ref <- old_results@final_signal_universe_m_d_ref
           }

           ##Consolidate ss backtest workflow
           updated_ss_backtest_results@ss_backtest_workflow <- c(old_results@ss_backtest_workflow, updated_ss_backtest_results@ss_backtest_workflow)
           names(updated_ss_backtest_results@ss_backtest_workflow)[length(names(updated_ss_backtest_results@ss_backtest_workflow))] <-
             paste0("update_", signals_m_df@current_date)

          #######################

           return(updated_ss_backtest_results)

          })


#' Run Signal Selection Backtest
#'
#' Performs out-of-sample testing of alpha signal selection across time using statistical methods (frequentist or Bayesian).
#' It filters and ranks signals based on their predictive power, typically using regression-based techniques.
#'
#' @param config An object of class `ss_backtest_config`, specifying the parameters of the signal selection backtest, including methodology and thresholds.
#' @param signals_m_df A `meta_dataframe` containing alpha signals (columns), identified by `tickers`, with `id` and `dates` columns.
#' @param backtest_returns_m_xts A `meta_xts` containing signal backtest returns (xts format). Must align with `signals_m_df` and include one column per signal.
#' @param port_backtest_cohort A `port_backtest_cohort` object. Used to extract backtest returns via `extract_returns_m_xts()` (only for the wrapper method).
#' @param benchmark_returns_m_xts A `meta_xts` object with benchmark returns. Used to compute active returns for each signal.
#' @param signal_themes_m_df A `meta_dataframe` that maps each signal to a theme. Includes columns: `id`, `tickers`, `dates`, and `theme`.
#' @param priors_m_df A (meta) data frame with columns including "id", "ticker", "dates", "theme" (used for clustering in the Bayesian hierarchical model), and values for active_return, bench_return, alpha (mean and se), beta (mean and se), and sigma. Data should be exogenous, as it will be used to set priors for the hierarchical Bayesian model.
#' @param custom_signal_universe_metrics_m_df Optional. A `meta_dataframe` with custom signal-level metrics to guide eligibility filtering.
#' @param verbose A boolean indicating whether to print messages.
#' @param parallel A boolean indicating whether to use parallel processing.
#' @param winsorization_probs Numeric vector of length 2. Defines lower and upper quantiles for winsorizing signal values. Default is `c(0.025, 0.975)`.
#' @param .test_seed (Internal) Integer or `NULL`. Used in unit tests to set random seed for reproducibility.
#' @param .update (Internal) Logical. If `TRUE`, updates a previous backtest incrementally. Default is `FALSE`.
#' @param .old_backtest_covered_dates (Internal) Vector of dates already covered in a previous backtest. Used only if `.update = TRUE`.
#' @param .old_oos_ss_outputs_m_df (Internal) A `meta_dataframe` of previously computed signal scores for reuse. Used only in update mode.
#'
#' @details
#'
#' This function performs iterative signal selection based on frequentist or Bayesian methods, which are applied to portfolio returns backtests to identify which signals can be considered significant in stock-level return prediction.
#'
#' To determine whether a signal matters in cross-sectional predictability, the literature typically runs regressions of signal portfolios against a benchmark factor model (e.g., CAPM), computing alphas and corresponding t-stats. Due to the large number of signals identified in the literature (the "factor zoo"), methods to control for multiple testing are often advocated.
#'
#' @section Bayesian Hierarchical Model:
#'
#' One way of introducing shrinkage to alpha estimates from signal portfolios is by using bayesian statistics, which might be specially useful in the context of small samples of strategy returns. Bayesian statistics allows for the incorporation of prior information about the parameters of interest (alpha and beta), which can be particularly useful in multiple testing.
#'
#' A Bayesian model depends on parameters that are themselves random variables. For instance, one can assume a normal distribution for the CAPM alpha parameter, with a mean of 0 (the strategy is not profitable) and a given standard deviation, effectively shrinking posterior estimates towards the prior mean, with an intensity that depends on the prior standard deviation and the sample distribution. Alternatively, one can utilize priors derived from signal strategies from other countries or asset classes than the ones being studied. Either way, the researcher will incorporate the reasoning process people usually have, as we usually have prior beliefs about a subject and then will update our beliefs based on new information. If another research possesses other priors, he or she can incorporate those into Bayes rule and derive other posterior distribution for parameters of interest, which is kind of more transparent than the frequentist approach, who will inherently give ultimate importance to associational evidence from (possibly overfit) data. Most of the time, in a frequentist setting, the researcher usually does not disclose the reasoning process behind the model. Therefore, bayesian statistics are being often recommended as a method to deal with multiple testing problems.
#'
#' Suppose one has return observations for multiple signal portfolios across time and wants to estimate the parameters of a single-factor model (CAPM) that describes the relationship between these signals portfolios and the market factor, being interested in the significance of the alpha parameter, while accounting for its exposure to systematic market risk. A complete pooled model will treat all observations as being independent and treating as irrelevant any information about individual strategies, which might be misleading, because it does not account for the hierarchical structure of the data. Observations of a given signal portfolio are possible correlated. On the other hand, a no pooling model will estimate a separate model for each signal portfolio, fitting specific parameters for each individual strategy, which might be inefficient, as it does not borrow information across signals. Signals belonging to a given theme tend to be similar. Signals in the value theme, for instance, are usually valuation multiples (eg book yield, earnings yield, fcf yield, sales yield etc) and so are very similar, thus it would be unfortunate to ignore information from other value strategies when analyzing, for example, book yield alpha. By fitting a hierarchical model, alpha estimates are first shrunk towards theme mean and then towards priors. In this setting, there is top layer represented by the population of all signal strategies under a theme (eg. the value theme alpha) and a second layer for the backtested strategies that fall under that theme, for which he have repeated observations.
#'
#' By using a hierarchical model, one can study both within-signal variability, examining how consistent the signal is across time, and between-signal variability, examining how performance patterns vary across different signals in a theme. Total variance is given by sum of within-signal variance and between-signal variance.
#'
#' Threfore, there are three layers for R_s,t (t-th observation of return of signal s):
#' \itemize{
#'  \item{Signal-specific layer:}{R_s,t ~ N(alpha_s + beta_s * R_m,t, sigma_s^2). This represents how returns vary within strategy s}
#'  \item{Theme-specific layer:}{alpha_s ~ N(alpha_theme, sigma_a_theme^2); beta_s ~ N(beta_theme, sigma_b_theme^2). This represents how the typical alpha/beta vary across strategies in a theme}
#'  \item{Priors:}{alpha_theme, sigma_a_theme, beta_theme, sigma_b_theme, sigma_s. Global parameters shared between strategies.}
#' }
#' More specifically, signal-specific mean parameters are treated as deviations from global parameters (u_1, u_2 and u_sigma for alpha, beta and sigma respectively, with corresponding mean and standard deviations mu_u1, tau_u1,  mu_u2, tau_u2, mu_u_sigma and tau_u_sigma). If the user provide a `priors_m_df`, the function will, for each theme in respective column (that should match possible options in `signal_themes_m_df`):
#' \itemize{
#'    \item{For each date, calculate the average and standard deviation of individual signals alphas/betas/sigmas. Based on these average values, a prior for overall alpha parameters will be chosen based on maximum likelihood}
#'    \item{For each date, calculate the differential of individual signals alpha/betas/sigmas from overall counterparts. For alpha, this means getting mu_u1 and tau_u1, mean and standard deviation of differentials of individual signals to overall mean. In particular, tau_u1 measures alpha variability between signals, the dispersion of differentials of individual signals from theme alpha.}
#'    \item{Use maximum likelihood to derive priors for each tau and also for correlation, according to `priors_type`}
#' }
#'
#' For location parameters, priors are chosen between normal and t distributions, given the option that minimizes BIC. For scale parameters, other candidate distributions are cauchy, inverse-gamma and log-normal. For correlation, the LKJ distribution is used.
#'
#' By considering the hierarchical structure of the data, it is possible to borrow information across signals and, thus, hierarchical models are better at balancing bias and variance than estimates from complete pooling (high bias) or no pooling models (high variance).
#'
#' To speed up computation, Bayesian models are fitted in parallel using the `future` framework.
#'
#' @section Signal Engineering Benchmarks:
#'
#' The process of generating a final signal (also known as Signal Engineering) incorporates two steps:
#' \itemize{
#'   \item{\strong{Signal Selection}: Selecting signals deemed significant based on a hypothesis testing zero-alpha null-hypothesis rejection criteria applied to associated signal portfolios returns in `backtest_returns_m_xts`.}
#'   \item{\strong{Signal Blending}: Blending selected signals into a final signal used to generate the final portfolio at the stock level.}
#' }
#'
#' The Signal Engineering Benchmarks (SE Benchmarks) evaluate the performance of both steps:
#' \itemize{
#'   \item{\strong{Signal Selection Benchmark}: Built using the universe of all signals in `chosen_signals`. It evaluates the performance of the signal selection process.}
#'   \item{\strong{Signal Blending Benchmark}: Built using only signals derived from the signal selection process. It evaluates the performance of the signal blending process.}
#' }
#'
#' Comparing predictive and return performance of the SS and SB Benchmarks provides insights into the effectiveness of the signal selection process. Additionally, comparing performance between the SB Benchmark and the final portfolio evaluates the performance of the signal blending process. SE Benchmarks are built based on themes; weights are first equally distributed among themes and then equally distributed among signals within each theme.
#'
#' @return An object of class `ss_backtest_results`, including:
#' \itemize{
#'   \item `signal_universe_m_df`: a meta_dataframe containing signal eligibility results across time.
#'   \item `final_signal_universe_m_d_ref`: final snapshot of selected signals.
#'   \item `selected_market_factor_proxy_m_xts`: benchmark series used in backtests.
#'   \item `frequentist_results`, `bayesian_results`: detailed testing outcomes.
#'   \item `ss_backtest_workflow`: metadata about execution, timestamps, and config.
#' }
#'
#' @seealso \code{\link{run_sb_backtest}}, \code{\link{meta_dataframe}}, \code{\link{meta_xts}}, \code{\link{ss_backtest_config}}
#' @export
setGeneric("run_ss_backtest", function(config, signals_m_df, backtest_returns_m_xts, port_backtest_cohort, benchmark_returns_m_xts, signal_themes_m_df, ...) {
  standardGeneric("run_ss_backtest")
})

#' @describeIn run_ss_backtest Wrapper method that internally derives `backtest_returns_m_xts` from a `port_backtest_cohort` object.
#'
#' @param port_backtest_cohort A `port_backtest_cohort` object. Used to extract historical return data via `extract_returns_m_xts()`.
#' @param priors_m_df Optional. A `meta_dataframe` of priors used for Bayesian signal testing.
#' @param custom_signal_universe_metrics_m_df Optional. A `meta_dataframe` with user-defined metrics to influence signal selection.
#'
#' @export
setMethod("run_ss_backtest",
          signature(config = "ss_backtest_config", signals_m_df = "meta_dataframe", backtest_returns_m_xts = "missing", port_backtest_cohort = "port_backtest_cohort",
                    benchmark_returns_m_xts = "meta_xts", signal_themes_m_df = "meta_dataframe"),

          function(config, signals_m_df, port_backtest_cohort, benchmark_returns_m_xts, signal_themes_m_df,
                   priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                   verbose = TRUE, parallel = TRUE, winsorization_probs = c(0.025, 0.975)){


            ## Extract backtest_returns_m_xts from port_backtest_cohort
            #######################
              ###Run extraction
              extracted_returns_m_xts <- extract_returns_m_xts(
                port_backtest_cohort = port_backtest_cohort,
                signals_m_df = signals_m_df, benchmark_returns_m_xts = benchmark_returns_m_xts,
                verbose = verbose
              )
              ###Assign extracted values
              backtest_returns_m_xts <- extracted_returns_m_xts$backtest_returns_m_xts
              benchmark_returns_m_xts <- extracted_returns_m_xts$benchmark_returns_m_xts

            #######################

            ##Run SS Backtest
            #######################
            ss_backtest_results <- run_ss_backtest(
              config = config, signals_m_df = signals_m_df, backtest_returns_m_xts = backtest_returns_m_xts,
              benchmark_returns_m_xts = benchmark_returns_m_xts, signal_themes_m_df = signal_themes_m_df,
              priors_m_df = priors_m_df, custom_signal_universe_metrics_m_df = custom_signal_universe_metrics_m_df,
              verbose = verbose, parallel = parallel, winsorization_probs = winsorization_probs,
              .update = FALSE
            )

              ###Add cohort name
              ss_backtest_results@ss_backtest_workflow[[length(ss_backtest_results@ss_backtest_workflow)]]$port_backtest_cohort_object_name <-
                port_backtest_cohort@cohort_name

            return(ss_backtest_results)
            #######################


          })



#' @describeIn run_ss_backtest Main method. Runs a full signal selection backtest using either Bayesian or frequentist statistical methods.
#'
#' @param priors_m_df Optional. A `meta_dataframe` of priors used for Bayesian model specification. Required if `p_correction_method = "bayesian"` and
#' `user_priors` are not supplied.
#' @param custom_signal_universe_metrics_m_df Optional. A `meta_dataframe` with additional signal-level metrics, used to influence eligibility.
#'
#' @export
setMethod("run_ss_backtest",
          signature(config = "ss_backtest_config", signals_m_df = "meta_dataframe", backtest_returns_m_xts = "meta_xts", port_backtest_cohort = "missing",
                    benchmark_returns_m_xts = "meta_xts", signal_themes_m_df = "meta_dataframe"),

          function(config, signals_m_df, backtest_returns_m_xts,  benchmark_returns_m_xts, signal_themes_m_df,
                   priors_m_df = NULL, custom_signal_universe_metrics_m_df = NULL,
                   verbose = TRUE, parallel = TRUE, winsorization_probs = c(0.025, 0.975),
                   .update = FALSE){

            ## Initial Preparations
            #######################

            #Assign default values for internal function (to avoid getting vars from global environ)
            active_returns <- TRUE
            split_method <- "expanding"
            market_factor_proxy <- "IBOV"
            p_correction_method <- "none"
            signal_significance_threshold <- 0.05
            enable_theme_representativeness <- TRUE
            user_priors <- NULL
            model_structure <- "no_pooled"
            theme_level_intercept <- NULL
            theme_level_slope <- NULL
            brms_control <- list(iter = 2000, chains = 4, thin = 1, seed = NA, adapt_delta = 0.80, warmup = 1000)
            prior_derivation_control <- list(half_t_df = 30)
            lmer_control <- list(lmer_optimizer = "nloptwrap", lmer_optimization_objective = "REML", hierarchical_p_value_method = "Satterthwaite")


            #Winsorization probs
            lower_quantile_winsorization <- min(winsorization_probs)
            upper_quantile_winsorization <- max(winsorization_probs)

            ##Set missing values to TRUE
            if(missing(verbose)){
              verbose <- TRUE
            }
            if(missing(parallel)){
              parallel <- TRUE
            }
            #######################

            #Get data from S4 objects
            #########################
            ##chosen_signals_and_positions
            chosen_signals_and_positions <- config@chosen_signals_and_positions
              ##Convert 'all' chosen_signals_and_positions
              if(length(chosen_signals_and_positions) == 1 && chosen_signals_and_positions == "all"){
                if (verbose) cat("According to user choice, SS backtest will contemplate all signals in signals_m_df, assuming a 'long' position.")
                chosen_signals <- colnames(signals_m_df@data)[-c(1:3)] #Get all signals in signals_m_df
                chosen_signals_and_positions <- rep("long", length(chosen_signals)) #Set all positions as 'long'
                names(chosen_signals_and_positions) <- chosen_signals
              } else {
                if (verbose) cat("According to user choice, SS backtest will contemplate the following signals in signals_m_df:\n")
                if (verbose) print(chosen_signals_and_positions)
              }

            ##signals_m_df
            signals_workflow <- signals_m_df@workflow #Get workflow
            signals_object_name <- signals_m_df@meta_dataframe_name #Get mdf name
            signals_current_date <- signals_m_df@current_date #Get current date
            signals_m_df <- signals_m_df@data #Get signals_m_df

            ##signal_themes_m_df
            signal_themes_workflow <- signal_themes_m_df@workflow #Get workflow
            signal_themes_object_name <- signal_themes_m_df@meta_dataframe_name #Get mdf name
            signal_themes_current_date <- signal_themes_m_df@current_date #Get current date
            signal_themes_m_df <- signal_themes_m_df@data #Get signal_themes_m_df

            ##custom_signal_universe_metrics_m_df
            if(!is.null(custom_signal_universe_metrics_m_df)){
              custom_signal_universe_metrics_workflow <- custom_signal_universe_metrics_m_df@workflow #Get workflow
              custom_signal_universe_metrics_object_name <- custom_signal_universe_metrics_m_df@meta_dataframe_name #Get mdf name
              custom_signal_universe_metrics_current_date <- custom_signal_universe_metrics_m_df@current_date #Get current date
              custom_signal_universe_metrics_m_df <- custom_signal_universe_metrics_m_df@data #Get custom_signal_universe_metrics_m_df
            }

            ##backtest_returns_m_xts
            backtest_returns_workflow <- backtest_returns_m_xts@workflow #Get workflow
            backtest_returns_object_name <- backtest_returns_m_xts@meta_xts_name #Get mxts name
            backtest_returns_current_date <- backtest_returns_m_xts@current_date #Get current date
            backtest_returns_m_xts <- backtest_returns_m_xts@data #Get backtest_returns_m_xts

            ##benchmark_returns_m_xts
            benchmark_returns_workflow <- benchmark_returns_m_xts@workflow #Get workflow
            benchmark_returns_object_name <- benchmark_returns_m_xts@meta_xts_name #Get mxts name
            benchmark_returns_current_date <- benchmark_returns_m_xts@current_date #Get current date
            benchmark_returns_m_xts <- benchmark_returns_m_xts@data #Get benchmark_returns_m_xts


            ##Get general info from contig
            initial_sample_size <- config@initial_sample_size
            rebalancing_months <- config@rebalancing_months
            active_returns <- config@active_returns
            split_method <- config@split_method
            config_name <- config@config_name
            alpha_test_strategy <- config@alpha_test_strategy

            ##Get general info from alpha_test_strategy
            signal_significance_threshold <- alpha_test_strategy@signal_significance_threshold
            p_correction_method <- alpha_test_strategy@p_correction_method
            market_factor_proxy <- alpha_test_strategy@market_factor_proxy
            enable_theme_representativeness <- alpha_test_strategy@enable_theme_representativeness
            model_structure <- alpha_test_strategy@model_structure
            theme_level_intercept <- alpha_test_strategy@theme_level_intercept
            theme_level_slope <- alpha_test_strategy@theme_level_slope
            lmer_control <- if(!is.null(alpha_test_strategy@lmer_control)) alpha_test_strategy@lmer_control else lmer_control


            if(p_correction_method == "bayesian"){
              #Get information that is specific to bayesian approach

              ##priors_m_df
              if(!is.null(priors_m_df)){
                priors_workflow <- priors_m_df@workflow #Get workflow
                priors_object_name <- priors_m_df@meta_dataframe_name #Get mdf name
                priors_current_date <- priors_m_df@current_date #Get current date
                priors_m_df <- priors_m_df@data #Get priors_m_df
              }

              bayesian_model_parameters <- alpha_test_strategy@bayesian_model_parameters
              user_priors <- bayesian_model_parameters@user_priors


              brms_control <- if(!is.null(bayesian_model_parameters@brms_control)) bayesian_model_parameters@brms_control else brms_control
              prior_derivation_control <- if(!is.null(bayesian_model_parameters@prior_derivation_control)) bayesian_model_parameters@prior_derivation_control else prior_derivation_control
            }

            #########################

            #Run SS Backtest Internal
            #########################
              ##Check if all objects current_date match
              check_consistent_dates(list(
                if (!is.null(signals_m_df)) signals_current_date else NULL,
                if (!is.null(signal_themes_m_df)) signal_themes_current_date else NULL,
                if (!is.null(priors_m_df)) priors_current_date else NULL,
                if (!is.null(custom_signal_universe_metrics_m_df)) custom_signal_universe_metrics_current_date else NULL,
                if (!is.null(benchmark_returns_m_xts)) benchmark_returns_current_date else NULL,
                if (!is.null(backtest_returns_m_xts)) backtest_returns_current_date else NULL
              ))

              ##Run internal FUN
              ss_backtest_results <- run_ss_backtest_internal(
                #Signals Data
                signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions, custom_signal_universe_metrics_m_df = custom_signal_universe_metrics_m_df,
                #Return Data
                backtest_returns_m_xts = backtest_returns_m_xts, benchmark_returns_m_xts = benchmark_returns_m_xts, market_factor_proxy = market_factor_proxy,
                #Signal Themes Data
                signal_themes_m_df,
                #Training Scheme
                initial_sample_size = initial_sample_size, rebalancing_months = rebalancing_months, split_method = split_method,
                #Alpha Test Strategy
                p_correction_method = p_correction_method, signal_significance_threshold = signal_significance_threshold, enable_theme_representativeness = enable_theme_representativeness,
                model_structure = model_structure, theme_level_intercept = theme_level_intercept, theme_level_slope = theme_level_slope, lmer_control = lmer_control,
                active_returns = active_returns,
                #Bayesian Model Parameters
                priors_m_df = priors_m_df, user_priors = user_priors,
                brms_control = brms_control, prior_derivation_control = prior_derivation_control,
                #Misc
                lower_quantile_winsorization =  lower_quantile_winsorization, upper_quantile_winsorization = upper_quantile_winsorization, verbose = verbose, parallel = parallel,
                .update = .update
              )


            #########################

            #Adjust SS Backtest WF
            ###########################

            ##Config
            ss_backtest_results@ss_backtest_config <- config

            ###IDs
            ss_backtest_results@ss_backtest_workflow$config_name <- config@config_name
            ss_backtest_results@ss_backtest_workflow$backtest_identifier <-
              paste0("c:",config@config_name, "_s:", signals_object_name, "_st:", signal_themes_object_name)
            ss_backtest_results@ss_backtest_workflow$current_date <- signals_current_date #already tested if all match

            ##Add port_cohort identifier


            #Add for priors_m_df
            if(!is.null(priors_m_df)){
              ss_backtest_results@ss_backtest_workflow$backtest_identifier <-
                paste0(ss_backtest_results@ss_backtest_workflow$backtest_identifier, "_p:", priors_object_name)
            }
            ss_backtest_results@backtest_identifier <- ss_backtest_results@ss_backtest_workflow$backtest_identifier

            ##Add workflows, config_name and objects for target and features
              ###Signals
              ss_backtest_results@ss_backtest_workflow$signals_object_name <- signals_object_name
              ss_backtest_results@ss_backtest_workflow$signals_workflow <- signals_workflow

              ###Signal Themes
              ss_backtest_results@ss_backtest_workflow$signal_themes_object_name <- signal_themes_object_name
              ss_backtest_results@ss_backtest_workflow$signal_themes_workflow <- signal_themes_workflow

              ###Priors Themes
              if(!is.null(priors_m_df)){
              ss_backtest_results@ss_backtest_workflow$priors_object_name <- priors_object_name
              ss_backtest_results@ss_backtest_workflow$priors_workflow <- priors_workflow
              }

              ###Custom Signal Universe Metrics
              if(!is.null(custom_signal_universe_metrics_m_df)){
                ss_backtest_results@ss_backtest_workflow$custom_signal_universe_metrics_object_name <- custom_signal_universe_metrics_object_name
                ss_backtest_results@ss_backtest_workflow$custom_signal_universe_metrics_workflow <- custom_signal_universe_metrics_workflow
              }

              ###Backtest Returns
              ss_backtest_results@ss_backtest_workflow$backtest_returns_object_name <- backtest_returns_object_name
              ss_backtest_results@ss_backtest_workflow$backtest_returns_workflow <- backtest_returns_workflow

              ###Benchmark Returns
              ss_backtest_results@ss_backtest_workflow$benchmark_returns_object_name <- benchmark_returns_object_name
              ss_backtest_results@ss_backtest_workflow$benchmark_returns_workflow <- benchmark_returns_workflow

              ###Workflow and names for signal_universe_m_df and final_signal_universe_m_d_ref
                ####Signal Universe
                if (!(.update && is.null(ss_backtest_results@signal_universe_m_df))){ #Skip empty updates
                  ss_backtest_results@signal_universe_m_df@workflow <- list(paste0("signal_universe_m_df result of ", ss_backtest_results@backtest_identifier))
                  ss_backtest_results@signal_universe_m_df@meta_dataframe_name <- paste0("ss_backtest___:",ss_backtest_results@ss_backtest_workflow$backtest_identifier)
                }
                if (!is.null(ss_backtest_results@signal_universe_m_df)){
                ss_backtest_results@final_signal_universe_m_d_ref@workflow <- list(paste0("final_signal_universe_m_d_ref result of ", ss_backtest_results@backtest_identifier))
                ss_backtest_results@final_signal_universe_m_d_ref@meta_dataframe_name <- paste0("ss_backtest___:",ss_backtest_results@ss_backtest_workflow$backtest_identifier)
                }
                ###Call
                ss_backtest_results@ss_backtest_workflow$call <- sys.call(-2)

                ###Add date to workflow
                ss_backtest_results@ss_backtest_workflow <- list(ss_backtest_results@ss_backtest_workflow)
                names(ss_backtest_results@ss_backtest_workflow) <- signals_current_date

                return(ss_backtest_results)

          }
)

#' Run Signal Selection Backtest (Internal)
#'
#' Internal engine for signal selection backtest. Called by \code{run_ss_backtest()}.
#' This function handles the logic for selecting eligible alpha signals based on cross-sectional regression statistics.
#'
#' It supports both frequentist and Bayesian methods for p-value adjustment and shrinkage, and handles all internal steps of the signal selection workflow.
#'
#' Not intended for direct use by package users.
#'
#' @param initial_sample_size Integer. Number of periods required before initiating the backtest.
#' @param rebalancing_months Integer vector. Specifies which months (e.g., c(1, 4, 7, 10)) trigger signal selection.
#' @param split_method Character. Either "expanding" or "rolling". Determines training sample construction.
#' @param signals_m_df A `meta_dataframe` with columns `id`, `tickers`, `dates`, and signal columns.
#' @param chosen_signals_and_positions Named character vector of signals and their positions ("long", "short", or "force").
#' @param custom_signal_universe_metrics_m_df Optional `meta_dataframe` used to refine signal eligibility.
#' @param backtest_returns_m_xts A `meta_xts` of signal portfolio returns.
#' @param benchmark_returns_m_xts A `meta_xts` with benchmark factor returns.
#' @param market_factor_proxy Character. Name of the column in `benchmark_returns_m_xts` used as the market factor.
#' @param p_correction_method Character. Method for p-value adjustment. See `run_ss_backtest()` for full list of options.
#' @param signal_significance_threshold Numeric between 0 and 1. Threshold for rejecting null alpha = 0.
#' @param enable_theme_representativeness Logical. If TRUE, retains one signal per theme even if none meet the threshold.
#' @param model_structure Character. "partial_pooled" or "no_pooled". Determines the model structure.
#' @param theme_level_intercept,theme_level_slope Optional characters. Control how intercept and slope vary across themes.
#' @param lmer_control List of arguments passed to `lme4::lmerControl`, including optimizer and p-value method.
#' @param active_returns Logical. Whether to compute active returns (vs raw).
#' @param priors_m_df Optional `meta_dataframe` with prior estimates for each signal.
#' @param user_priors Optional list of priors created with `brms::set_prior()`.
#' @param brms_control List of settings for `brms::brm()`, including MCMC controls.
#' @param prior_derivation_control List of control parameters for prior distribution derivation.
#' @param signal_themes_m_df A `meta_dataframe` mapping each signal to a theme.
#' @param lower_quantile_winsorization,upper_quantile_winsorization Numeric. Quantiles used for winsorizing signal data.
#' @param verbose Logical. Whether to print progress messages.
#' @param parallel Logical. Whether to use parallel computation with `future`.
#' @param .update Logical. If TRUE, executes incremental update mode.
#'
#' @return An object of class `ss_backtest_results`.
#' @noRd
#' @keywords internal
run_ss_backtest_internal <- function(
  #Dates
  initial_sample_size, rebalancing_months, split_method = "expanding",
  #Signals
  signals_m_df, chosen_signals_and_positions, custom_signal_universe_metrics_m_df = NULL,
  #Backtests and benchmarks
  backtest_returns_m_xts, benchmark_returns_m_xts, market_factor_proxy = "IBOV",
  #P-value
  p_correction_method = "none", signal_significance_threshold = 0.05,
  #Theme Representativeness Eligiblity
  enable_theme_representativeness = TRUE,
  #Model Structure
  model_structure = "no_pooled", theme_level_intercept = NULL, theme_level_slope = NULL,
  lmer_control = list(lmer_optimizer = "nloptwrap", lmer_optimization_objective = "REML", hierarchical_p_value_method = "Satterthwaite"),
  active_returns = TRUE,
  #Bayesian variables
  priors_m_df = NULL, user_priors = NULL,
  brms_control = list(iter = 2000, chains = 4, thin = 1, seed = NA, adapt_delta = 0.80),
  prior_derivation_control = list(half_t_df = 30),
  #Signal Themes
  signal_themes_m_df,
  #Winsorization
  lower_quantile_winsorization = 0.025, upper_quantile_winsorization = 0.975,
  verbose = TRUE, parallel = TRUE,
  .update = FALSE
){

  #Measure time to run and run gc
  elapsed_time <- system.time({

    ##Initial Setup
    #########################
      ###Get forced signals and exclude them from chosen_signals_and_positions
      if(any(chosen_signals_and_positions == "force")){
        forced_signals <- chosen_signals_and_positions[which(chosen_signals_and_positions == "force")]
      } else {
        forced_signals <- NULL
      }
      original_chosen_signals_and_positions <- chosen_signals_and_positions #Get original object in case forced_signals were taken out
      chosen_signals_and_positions <- chosen_signals_and_positions[which(chosen_signals_and_positions != "force")]

    #########################

    ##Check Parameters
    #########################
      ###Check installed packages
      if (verbose){
        if (!requireNamespace("crayon", quietly = TRUE) || !requireNamespace("tictoc", quietly = TRUE)) {
          stop("Packages 'crayon' and 'tictoc' are required to generate logs. Please install them using install.packages() or set verbose as FALSE")
        }
      }

      ###Check args
      check_inputs_ss_backtest(
        #Dates
        initial_sample_size = initial_sample_size, rebalancing_months = rebalancing_months, split_method = split_method,
        #Signals
        signals_m_df = signals_m_df, chosen_signals_and_positions = chosen_signals_and_positions, forced_signals = forced_signals, custom_signal_universe_metrics_m_df = custom_signal_universe_metrics_m_df,
        #Backtest and benchmarks
        backtest_returns_m_xts = backtest_returns_m_xts, benchmark_returns_m_xts = benchmark_returns_m_xts, market_factor_proxy = market_factor_proxy,
        #P-value
        p_correction_method = p_correction_method, signal_significance_threshold = signal_significance_threshold,
        #Theme Representativeness Eligiblity
        enable_theme_representativeness = enable_theme_representativeness,
        #Model Structure
        model_structure = model_structure, theme_level_intercept = theme_level_intercept, theme_level_slope = theme_level_slope, lmer_control = lmer_control, active_returns = active_returns,
        #Priors
        priors_m_df = priors_m_df, user_priors = user_priors,
        brms_control = brms_control, prior_derivation_control = prior_derivation_control,
        #Signal Themes
        signal_themes_m_df = signal_themes_m_df,
        #Winsorization
        lower_quantile_winsorization = lower_quantile_winsorization, upper_quantile_winsorization = upper_quantile_winsorization
      )

    #########################

    ##Init objects
    #########################
      ###Extract dates
      dates_m_vector <- unique(as.Date(zoo::index(backtest_returns_m_xts), format = "%Y-%m-%d")) #coerce just to be sure
      dates_m_vector <- dates_m_vector[order(dates_m_vector)] #Re-order ascending just to be sure

      ###Backtest length
      backtest_length <- length(dates_m_vector) - initial_sample_size + 1 #calculate backtest_length

      ###Rebalancing Dates
      dates_backtest <- dates_m_vector[initial_sample_size:(initial_sample_size + backtest_length - 1)] #These are dates inside backtest

      if (!.update){
        ####Get first rebalancing date
        first_rebalance_date <- min(dates_backtest)
        ####Get all rebalancing dates
        rebalance_dates <- unique( #Unique is to eliminate repeated dates, in case month of first_rebalance_date is a rebalancing month
          c(first_rebalance_date, dates_backtest[which(lubridate::month(dates_backtest) %in% rebalancing_months)]) #Dates corresponding to rebalancing_months
        )
        ####Re-order ascending just to be sure
        rebalance_dates <- rebalance_dates[order(rebalance_dates)]
        ###Last rebalance date
        last_rebalance_date <- max(rebalance_dates)
      } else {
        rebalance_dates <- dates_backtest[which(lubridate::month(dates_backtest) %in% rebalancing_months)]
        if (length(rebalance_dates) > 0){
          first_rebalance_date <- min(rebalance_dates) #In an update, first testing date is not a rebalancing month necessarily
          last_rebalance_date <- max(rebalance_dates)
          rebalance_dates <- rebalance_dates[order(rebalance_dates)]
        } else {
          first_rebalance_date <- NULL
          last_rebalance_date <- NULL
        }
      }

      ###Number of rebalancing months
      n_rebalance_months <- length(rebalance_dates)

      ###Create signal_universe list structure to get results
      signal_universe_m_d_ref_list <- list()

    #########################

    #Initial Prints
    #########################
    if(verbose){
      ##Text otherwise
      if (.update){
        cat(crayon::cyan(paste("Updating signal selection backtest")))
        cat("\n")
      } else {
        cat(crayon::cyan(paste("Starting signal selection backtest")))
        cat("\n")
      }
      cat("=============================\n")
      cat(paste("Performance metrics calculated with", if(active_returns) "active returns" else "raw returns"))
      cat("\n")
      cat(paste("Factor model:", model_structure, "CAPM with", market_factor_proxy, "as proxy for market factor"))
      cat("\n")
      if(p_correction_method == "none"){
        cat(crayon::yellow(paste("P-values will not be adjusted. Beware for multiple testing bias")))
      } else {
        cat(crayon::yellow(paste("P-values will be adjusted following the", p_correction_method, "method")))
      }
      cat("\n")
      cat(paste("Signal significance threshold set as", signal_significance_threshold))
      cat("\n")
      cat(paste("Theme representativeness eligibility is set to", enable_theme_representativeness))
      cat("\n")
      if(!is.null(forced_signals)){
        cat("Forced signals:", paste(names(forced_signals), collapse = ", "))
        cat("\n")
      }
    }

    #################

    ##Select signals, benchmark and respective backtests based on user choice
    ########################
    selected_signals_and_backtest_list <- select_and_correct_signals(
      #signals_m_df
      signals_m_df = signals_m_df,
      #Signal Themes
      signal_themes_m_df = signal_themes_m_df,
      #Chosen signals and positions
      chosen_signals_and_positions = chosen_signals_and_positions,
      #Signals backtest
      backtest_returns_m_xts = backtest_returns_m_xts
    )

    ###Selected signals_m_df with corrected positions
    selected_signals_corrected_positions_m_df <- selected_signals_and_backtest_list$selected_signals_corrected_positions_m_df
    ###Select signals_themes_m_df
    selected_signal_themes_m_df <- selected_signals_and_backtest_list$selected_signal_themes_m_df
    ###Selected signals backtest returns
    selected_backtest_returns_corrected_positions_m_xts <- selected_signals_and_backtest_list$selected_backtest_returns_corrected_positions_m_xts

    ###Check if both are contemplated in signal_themes
    if(any(!colnames(dplyr::select(selected_signals_corrected_positions_m_df, -id, -tickers, -dates)) %in% unique(selected_signal_themes_m_df$tickers))){
      stop("all selected signals (with corrected positions) should have a theme classification in selected_signal_themes_m_df")
    }
    if(any(!colnames(selected_backtest_returns_corrected_positions_m_xts) %in% selected_signal_themes_m_df$tickers)){
      stop("all selected signals in backtests (with corrected positions) should have a theme classification in selected_signal_themes_m_df")
    }

    ###Select market factor proxy from benchmark returns xts
    selected_market_factor_proxy_m_xts <- benchmark_returns_m_xts[, market_factor_proxy]

    ########################

    ##Start Backtest
    ##Apply backtest only if multiple signals are being provided
    #################
    for (d in initial_sample_size:(initial_sample_size + backtest_length - 1)){
      #Extract date and references
      current_date <- dates_m_vector[d]
      if (verbose) print(current_date)

      #Rebalance if it's a rebalancing month
      if (.update){
        #For an update, is_rebalancing_month does not consider d == initial_sample_size
        is_rebalancing_month <- (lubridate::month(current_date) %in% rebalancing_months)
      } else {
        is_rebalancing_month <- (lubridate::month(current_date) %in% rebalancing_months) || d == (initial_sample_size)
      }

      if (is_rebalancing_month){
        #Print refitting message
        if (verbose){
          cat("\n")
          cat(crayon::yellow(paste("Running signal selection backtest at:", current_date)))
          cat("\n")
        }

        #Subset signals, backtest, market factor, priors and custom_signal_universe_metrics
        selected_signals_corrected_positions_m_upd_ref <- selected_signals_corrected_positions_m_df %>% dplyr::filter(dates <= current_date)
        selected_signal_themes_m_d_ref <- selected_signal_themes_m_df %>% dplyr::filter(dates == current_date) #Just one date reference needed
        priors_m_upd_ref <- if (!is.null(priors_m_df)) priors_m_df %>% dplyr::filter(dates <= current_date) else NULL
        custom_signal_universe_metrics_m_upd_ref <- if (!is.null(custom_signal_universe_metrics_m_df)) custom_signal_universe_metrics_m_df %>% dplyr::filter(dates <= current_date) else NULL

        selected_backtest_returns_corrected_positions_m_xts_upd_ref <- selected_backtest_returns_corrected_positions_m_xts[which(zoo::index(selected_backtest_returns_corrected_positions_m_xts) <= current_date), ]
        selected_market_factor_proxy_m_xts_upd_ref <- selected_market_factor_proxy_m_xts[which(zoo::index(selected_market_factor_proxy_m_xts) <= current_date), ]


        ###Elect signals
        signal_eligibility_results_list <- define_signal_eligibility(
          #Backtests
          selected_backtest_returns_corrected_positions_m_xts_upd_ref = selected_backtest_returns_corrected_positions_m_xts_upd_ref,
          selected_market_factor_proxy_m_xts_upd_ref = selected_market_factor_proxy_m_xts_upd_ref,
          custom_signal_universe_metrics_m_upd_ref = custom_signal_universe_metrics_m_upd_ref,
          #P adjustment
          p_correction_method = p_correction_method, signal_significance_threshold = signal_significance_threshold,
          #Theme Representativeness Eligibiility
          enable_theme_representativeness = enable_theme_representativeness,
          #Model Structure
          model_structure = model_structure, theme_level_intercept = theme_level_intercept, theme_level_slope = theme_level_slope, lmer_control = lmer_control,
          active_returns = active_returns,
          #Bayesian method
          priors_m_upd_ref = priors_m_upd_ref, user_priors = user_priors,
          brms_control = brms_control, prior_derivation_control = prior_derivation_control,
          #Signal Themes
          selected_signal_themes_m_d_ref = selected_signal_themes_m_d_ref,
          #Winsorization
          lower_quantile_winsorization = lower_quantile_winsorization, upper_quantile_winsorization = upper_quantile_winsorization,
          #Verbose & Parallel
          verbose = verbose, parallel = parallel
        )

        ###Get results
        signal_universe_m_d_ref <- signal_eligibility_results_list$signal_universe_m_d_ref

        ##Add forced signals
        if (length(forced_signals) > 0){
          ###Create a forced_signals_m_d_ref
          forced_signals_m_d_ref <- expand.grid( #Create an expanded grid with the forced variable
            tickers = names(forced_signals),
            dates = unique(signal_universe_m_d_ref$dates),
            theme = "forced", #Assume a general 'forced' theme
            theme_ss_bench_weights = 0,
            theme_sb_bench_weights = 0,
            stringsAsFactors = FALSE
          ) %>%
            dplyr::mutate(
              id = paste0(tickers, "-", dates),
              pre_eligible_assets = 0, #Forced variables are not treated as pre eligible assets
              is_eligible = 1 #But they are being elected in a forced way (same as enable_theme_representativeness)
            ) %>%
            dplyr::select(
              id, tickers, dates, theme_ss_bench_weights, theme_sb_bench_weights, theme, is_eligible #Select those columns
            )

          ###Add to signal_universe_m_d_ref
          signal_universe_m_d_ref <- signal_universe_m_d_ref %>% dplyr::bind_rows(forced_signals_m_d_ref)
        }

        ###Print results
        if(verbose){
          cat("\n")
          cat(crayon::green(paste0("Eligible signals defined: ")))
          cat(paste(signal_universe_m_d_ref$tickers[which(signal_universe_m_d_ref$is_eligible == 1)], collapse = ", "))
          cat("\n")
        }

        ###Get results
        signal_universe_m_d_ref_list[[which(rebalance_dates %in% current_date)]] <-  signal_universe_m_d_ref



      } #End rebalancing month
    } #End loop

    #Assign dates to names of objects
    if (!.update || (.update && length(signal_universe_m_d_ref_list) > 0)){
      #Rename obj
      names(signal_universe_m_d_ref_list) <- rebalance_dates

      #Turn signal_universe_m_d_ref_list into a signle meta_dataframe
      signal_universe_m_df <- do.call(rbind, signal_universe_m_d_ref_list)
      rownames(signal_universe_m_df) <- NULL #erase rownames
    }

    #End timer
  })
  print(elapsed_time)

  #Get workflow
  ss_backtest_workflow <- list(
    config_name = "not_identified",
    backtest_identifier = "not_identified",
    backtest_type = if(p_correction_method == "bayesian") "bayesian" else "frequentist",
    #Hypothesis tests
    p_correction_method = p_correction_method,
    signal_significance_threshold = signal_significance_threshold,
    market_factor_proxy = market_factor_proxy,
    active_returns = active_returns,
    model_structure = model_structure,
    enable_theme_representativeness = enable_theme_representativeness,
    theme_level_intercept = theme_level_intercept,
    theme_level_slope = theme_level_slope,
    lmer_control = lmer_control,
    #Bayesian
    user_priors = user_priors,
    brms_control = brms_control,
    prior_derivation_control = prior_derivation_control,
    #Dates
    initial_sample_size = initial_sample_size,
    rebalancing_months = rebalancing_months,
    dates_covered = dates_m_vector,
    n_dates = length(dates_m_vector),
    dates_backtest = dates_backtest,
    rebalance_dates = rebalance_dates,
    n_rebalance_months = n_rebalance_months,
    first_rebalance_date = first_rebalance_date,
    split_method = split_method,
    #Signals
    chosen_signals_and_positions = original_chosen_signals_and_positions,
    selected_signals_corrected_positions = colnames(selected_signals_corrected_positions_m_df),
    n_signals = length(original_chosen_signals_and_positions),
    signals_workflow = NULL,
    signals_object_name = "not_identified",
    signals_dates =  sort(unique(dplyr::pull(signals_m_df, dates))),
    signal_themes_workflow = NULL,
    signal_themes_object_name = "not_identified",
    signal_themes_dates = sort(unique(dplyr::pull(signals_m_df, dates))),
    priors_workflow = NULL,
    priors_object_name = "not_present",
    priors_dates = if (!is.null(priors_m_df)) sort(unique(dplyr::pull(priors_m_df, dates))) else NULL,
    backtest_returns_object_name = "not_identified",
    benchmark_returns_object_name = "not_identified",
    benchmark_returns_dates_covered = zoo::index(benchmark_returns_m_xts),
    backtest_returns_dates_covered = zoo::index(backtest_returns_m_xts),
    custom_signal_universe_metrics_workflow = NULL,
    custom_signal_universe_metrics_object_name = "not_identified",
    custom_signal_universe_dates = if (!is.null(custom_signal_universe_metrics_m_df)) sort(unique(dplyr::pull(custom_signal_universe_metrics_m_df, dates))) else NULL,
    lower_quantile_winsorization = lower_quantile_winsorization,
    upper_quantile_winsorization = upper_quantile_winsorization,
    #Performance
    elapsed_time = elapsed_time,
    timestamps = c(initialization = Sys.time()),
    #Call
    call = match.call()
  )

  ##Create meta_dataframes
  ###Create signal_universe_m_df
  if (!.update || (.update && exists("signal_universe_m_df") > 0)){
  signal_universe_m_df <- withCallingHandlers({
    signal_universe_m_df <- signal_universe_m_df %>% dplyr::arrange(id)
    suppressMessages(create_meta_dataframe(signal_universe_m_df, ss_backtest_workflow = ss_backtest_workflow, type = "signal_universe"))
  },
  warning = function(w) {
    warning("Signal universe creation warning: ", conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  }
  ###Create final_signal_universe_m_d_ref
  if (!.update || (.update && exists("signal_universe_m_df") > 0)){
  final_signal_universe_m_d_ref <- withCallingHandlers({
    signal_universe_m_d_ref <- signal_universe_m_d_ref %>% dplyr::arrange(id)
    suppressMessages(create_meta_dataframe(signal_universe_m_d_ref, ss_backtest_workflow = ss_backtest_workflow, type = "signal_universe"))
  },
  warning = function(w) {
    warning("Final signal universe creation warning: ", conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  }

  ##Create meta_xts
  ###Create selected_market_factor_proxy_m_xts
  selected_market_factor_proxy_m_xts <- withCallingHandlers({
    suppressMessages(create_meta_xts(selected_market_factor_proxy_m_xts, type = "returns", asset_type = "benchmark", source = ss_backtest_workflow$backtest_identifier))
  },
  warning = function(w) {
    warning("Market factor proxy creation warning: ", conditionMessage(w))
    invokeRestart("muffleWarning")
  })

  #Get final object
  ss_backtest_results <- methods::new("ss_backtest_results",
                                       ss_backtest_config = NULL,
                                       signal_universe_m_df = if (exists("signal_universe_m_df")) signal_universe_m_df else NULL,
                                       final_signal_universe_m_d_ref = if (exists("final_signal_universe_m_d_ref")) final_signal_universe_m_d_ref else NULL ,
                                       selected_market_factor_proxy_m_xts = selected_market_factor_proxy_m_xts,
                                       frequentist_results = if (exists("signal_eligibility_results_list")) signal_eligibility_results_list$frequentist_results else NULL,
                                       bayesian_results = if (exists("signal_eligibility_results_list")) signal_eligibility_results_list$bayesian_results else NULL,
                                       p_correction_method = p_correction_method,
                                       ss_backtest_workflow = ss_backtest_workflow,
                                       backtest_identifier = "not_identified"
  )

  #Return
  return(ss_backtest_results)

}




