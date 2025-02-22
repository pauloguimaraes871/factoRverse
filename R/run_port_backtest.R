#' Update Portfolio Backtest
#' The `update_port_backtest` function will take an existing `port_backtest_results` object and update it with
#' new dates. This function is useful when you want to add new dates to an existing backtest without having to re-run the entire backtest.
#'
#' @param signals_m_df A meta_dataframe containing the signal features. It must include at least the columns \code{id}, \code{tickers}, and \code{dates}.
#' @param fwd_return_m_df A meta_dataframe containing forward returns.
#' @param liquidity_m_df A meta_dataframe containing liquidity metrics.
#' @param volatility_m_df A meta_dataframe containing volatility metrics.
#' @param results An object of class \code{port_backtest_results} specifying the portfolio backtest results to be updated.
#' @param n_update The expected number of periods to update
#' @param parallel Logical; if \code{TRUE}, executes parts of the backtest in parallel (default is \code{TRUE}).
#' @param ... Additional arguments (if needed).
#'
#' @return An object of class \code{port_backtest_results} containing the portfolio backtest results.
#'
#' @export
setGeneric("update_port_backtest", function(signals_m_df, fwd_return_m_df, liquidity_m_df, volatility_m_df, n_update, old_results, new_config, ...) standardGeneric("update_port_backtest"))

#' @describeIn update_port_backtest Updates a portfolio backtest using based on a \code{port_backtest_results} object.
#'
#' This method extracts the parameters from the \code{results} object (of class \code{port_backtest_results}), modifies initial_buffer_period, performs the
#' new backtest and then binds results#'
#'
#' @export
setMethod("update_port_backtest",
          signature(signals_m_df = "meta_dataframe", fwd_return_m_df = "meta_dataframe", liquidity_m_df = "meta_dataframe", volatility_m_df = "meta_dataframe", n_update = "numeric",
                    old_results = "port_backtest_results", new_config = "port_backtest_config"),
          function(signals_m_df, fwd_return_m_df, liquidity_m_df, volatility_m_df, n_update, old_results, new_config, #Base Port Backtest Objs
                   stock_groups_m_df = NULL, benchmark_weights_m_df = NULL, ##Constraints Objs
                   daily_stock_returns_m_xts = NULL, daily_bench_returns_m_xts = NULL, #Covariance Estimation
                   custom_stock_weights_m_df = NULL, custom_stock_metrics_m_df = NULL, user_defined_OR_rules_m_df = NULL, user_defined_AND_rules_m_df = NULL, #Custom Objs
                   winsorization_probs = c(0.025, 0.975), #Winsorization
                   verbose = TRUE, parallel = TRUE){

            #Check adherence between new objects and old object (names and dates)
            #######################
              ##Gather all arguments into a single named list (only those that have @meta_dataframe_name or meta_xts_name)
              new_objects_list <- list(
                signals_m_df = signals_m_df,
                fwd_return_m_df = fwd_return_m_df,
                liquidity_m_df = liquidity_m_df,
                volatility_m_df = volatility_m_df,
                benchmark_returns_m_xts = benchmark_returns_m_xts,
                stock_groups_m_df = stock_groups_m_df,
                benchmark_weights_m_df = benchmark_weights_m_df,
                daily_stock_returns_m_xts = daily_stock_returns_m_xts,
                daily_bench_returns_m_xts = daily_bench_returns_m_xts,
                custom_stock_weights_m_df = custom_stock_weights_m_df,
                custom_stock_metrics_m_df = custom_stock_metrics_m_df,
                user_defined_OR_rules_m_df = user_defined_OR_rules_m_df,
                user_defined_AND_rules_m_df = user_defined_AND_rules_m_df
                )

              old_objects_names_list <- list(
                signals_m_df = old_results@port_backtest_workflow$signals_object_name,
                fwd_return_m_df = old_results@port_backtest_workflow$fwd_return_object_name,
                liquidity_m_df = old_results@port_backtest_workflow$liquidity_object_name,
                volatility_m_df = old_results@port_backtest_workflow$volatility_object_name,
                benchmark_returns_m_xts = old_results@port_backtest_workflow$benchmark_returns_object_name,
                stock_groups_m_df = old_results@port_backtest_workflow$stock_groups_object_name,
                benchmark_weights_m_df = old_results@port_backtest_workflow$benchmark_weights_object_name,
                daily_stock_returns_m_xts = old_results@port_backtest_workflow$daily_stock_returns_object_name,
                daily_bench_returns_m_xts = old_results@port_backtest_workflow$daily_bench_returns_object_name,
                custom_stock_weights_m_df = old_results@port_backtest_workflow$custom_stock_weights_object_name,
                custom_stock_metrics_m_df = old_results@port_backtest_workflow$custom_stock_metrics_object_name,
                user_defined_OR_rules_m_df = old_results@port_backtest_workflow$user_defined_OR_rules_object_name,
                user_defined_AND_rules_m_df = old_results@port_backtest_workflow$user_defined_AND_rules_object_name
              )

              ##Baseline info for dates comparison
              dates_covered <- old_results@port_backtest_workflow$dates_covered

              ##Perform check
              check_update_backtest_objects(new_objects_list = new_objects_list, old_objects_names_list = old_objects_names_list, dates_covered = dates_covered)
              #######################

            #Check config_name and sb_backtest_results
            #######################
              ##Config Name
              if (new_config@config_name != old_results@port_backtest_workflow$config_name){
                stop("config_name in new_config does not match the one in old_results.")
              }

              ##SB Backtest Results
              if (!is.null(new_config@sb_backtest_config)){
                stop("update_port_backtest does not support sb_backtest_config. Please use a config with sb_backtest_results instead.")
              }

              if (!is.null(new_config@sb_backtest_results)){
                ###This is the case for sb_backtest_results in new_config
                if (!identical(new_config@sb_backtest_results@backtest_identifier, old_results@sb_backtest_results@backtest_identifier)){
                  stop("sb backtest_identifier in new_config does not match the one in old_results.")
                }
              } else {
                ###This is the case for no sb_backtest_results in new_config
                if (!is.null(old_results@sb_backtest_results)){
                  stop("sb_backtest_results in old_results is not NULL but new_config does not have sb_backtest_results.")
                }
              }

            #######################








            #######################

            #Adapt initial_buffer_period and re-run
            #######################
              ##Change initial_buffer_period
              config@initial_buffer_period <- length(dates_covered) + 1
              ##Re-run
              update_port_backtest_results <- run_port_backtest(
                ###Base Port Backtest Objs
                signals_m_df = signals_m_df, fwd_return_m_df = fwd_return_m_df, liquidity_m_df = liquidity_m_df, volatility_m_df, config = config,
                ###SB/SS Backtest Objs
                target_m_df = target_m_df, port_backtest_cohort = port_backtest_cohort, backtest_returns_m_xts = backtest_returns_m_xts, benchmark_returns_m_xts = benchmark_returns_m_xts,
                signal_themes_m_df = signal_themes_m_df, priors_m_df = priors_m_df, custom_signal_weights_m_df = custom_signal_weights_m_df,
                custom_signal_universe_metrics_m_df = custom_signal_universe_metrics_m_df, gsm_algorithm = gsm_algorithm, .test_seed = .test_seed,
                ###Constraints Objs
                stock_groups_m_df = stock_groups_m_df, benchmark_weights_m_df = benchmark_weights_m_df,
                ###Covariance Estimation
                daily_stock_returns_m_xts = daily_stock_returns_m_xts, daily_bench_returns_m_xts = daily_bench_returns_m_xts,
                ###Custom Objs
                custom_stock_weights_m_df = custom_stock_weights_m_df, custom_stock_metrics_m_df = custom_stock_metrics_m_df,
                user_defined_OR_rules_m_df = user_defined_OR_rules_m_df, user_defined_AND_rules_m_df = user_defined_AND_rules_m_df,
                ###Winsorization
                winsorization_probs = winsorization_probs,
                ###Other
                verbose = verbose, parallel = parallel
              )

            #######################

            #Consolidate results
            #######################
              ##results objects
              port_backtest_outputs_list <- list(
                port_weights_m_df = update_port_backtest_results@port_weights_m_df,
                stock_universe_m_df = update_port_backtest_results@stock_universe_m_df,
                port_returns_m_xts = update_port_backtest_results@port_returns_m_xts,
                port_costs_m_xts = update_port_backtest_results@port_costs_m_xts,
                port_metrics_m_xts = update_port_backtest_results@port_metrics_m_xts
              )

              ##Consolidate
              updated_results_list <- consolidate_backtest_results(backtest_outputs_list = port_backtest_outputs_list)

              ##Reassign the updated objects back into 'update_port_backtest_results'
              update_port_backtest_results@port_weights_m_df   <- updated_m_df_results_list[["port_weights_m_df"]]
              update_port_backtest_results@stock_universe_m_df <- updated_m_df_results_list[["stock_universe_m_df"]]
              update_port_backtest_results@port_returns_m_xts  <- updated_m_df_results_list[["port_returns_m_xts"]]
              update_port_backtest_results@port_costs_m_xts    <- updated_m_df_results_list[["port_costs_m_xts"]]
              update_port_backtest_results@port_metrics_m_xts  <- updated_m_df_results_list[["port_metrics_m_xts"]]

              #######################

              return(update_port_backtest_results)

          })





#' Run Portfolio Backtest
#'
#' The `run_port_backtest` function serves as a wrapper for the internal function
#' `run_port_backtest_internal`. It extracts the required parameters from a
#' `port_backtest_config` object, applies any additional pre‐processing to the inputs,
#' and then calls the internal function to perform the portfolio backtest.
#'
#' @param signals_m_df A meta_dataframe containing the signal features. It must include at least the columns \code{id}, \code{tickers}, and \code{dates}.
#' @param fwd_return_m_df A meta_dataframe containing forward returns.
#' @param liquidity_m_df A meta_dataframe containing liquidity metrics.
#' @param volatility_m_df A meta_dataframe containing volatility metrics.
#' @param config An object of class \code{port_backtest_config} specifying the portfolio backtest configuration.
#' @param parallel Logical; if \code{TRUE}, executes parts of the backtest in parallel (default is \code{TRUE}).
#' @param ... Additional arguments (if needed).
#'
#' @return An object of class \code{port_backtest_results} containing the portfolio backtest results.
#'
#' @export
setGeneric("run_port_backtest", function(signals_m_df, fwd_return_m_df, liquidity_m_df, volatility_m_df, config, ...) standardGeneric("run_port_backtest"))

#' @describeIn run_port_backtest Runs a portfolio backtest using a \code{port_backtest_config} object.
#'
#' This method extracts the parameters from the \code{config} object (of class \code{port_backtest_config}) and then calls
#' the internal function \code{run_port_backtest_internal} to perform the backtest.
#'
#'
#' @export
setMethod("run_port_backtest",
          signature(signals_m_df = "meta_dataframe", fwd_return_m_df = "meta_dataframe", liquidity_m_df = "meta_dataframe", volatility_m_df = "meta_dataframe",
                    config = "port_backtest_config"),
          function(signals_m_df, fwd_return_m_df, liquidity_m_df, volatility_m_df, config,  #Base Port Backtest Objs
                   target_m_df = NULL, port_backtest_cohort = NULL, backtest_returns_m_xts = NULL, benchmark_returns_m_xts = NULL, signal_themes_m_df = NULL, priors_m_df = NULL, ##SB Backtest Objs
                   custom_signal_weights_m_df = NULL, custom_signal_universe_metrics_m_df = NULL, gsm_algorithm = "ols", .test_seed = NULL, ##SB Backtest Objs
                   stock_groups_m_df = NULL, benchmark_weights_m_df = NULL, ##Constraints Objs
                   daily_stock_returns_m_xts = NULL, daily_bench_returns_m_xts = NULL, #Covariance Estimation
                   custom_stock_weights_m_df = NULL, custom_stock_metrics_m_df = NULL, user_defined_OR_rules_m_df = NULL, user_defined_AND_rules_m_df = NULL, #Custom Objs
                   winsorization_probs = c(0.025, 0.975), #Winsorization
                   verbose = TRUE, parallel = TRUE) {

            ##Assign default values for internal function
            ###########################

              ###Base objs
              chosen_score_metric_and_position <- NULL
              selected_benchmark <- NULL
              oos_predictions_m_df <- NULL
              eligibility_quantile_range = c(0.9, 1.0)
              min_eligible_assets_fallback <- NULL

              ###Portfolio objs
              port_construction_method <- "ew"
              cov_matrix_sample_size <- 21
              cov_estimation_method <- "sample"
              cov_matrix_benchmark <- NULL
              active_returns <- TRUE
              rp_method <- "cyclical-spinu"
              n_random_ports <- 2000
              random_ports_method <- "sample"
              opt_objective <- "sharpe"
              opt_method <- "random"
              concentration_constraint_policy <- NULL
              liquidity_constraint_policy <- NULL
              turnover_constraint_policy <- NULL

              ###Liq objs
              liquidity_floor_cutoffs <- NULL

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
            ###########################

            #Get or fabricate oos_sb_outputs_m_df
            ###########################
              sb_backtest_config <- config@sb_backtest_config
              sb_backtest_results <- config@sb_backtest_results
              ####If SB Config is provided, run_sb_backtest
              if (!is.null(sb_backtest_config) && is.null(sb_backtest_results)){
                ##Print
                if(verbose){
                  cat(crayon::cyan("Running Signal Blending Backtest based on sb_backtest_config \n"))
                }
                ###Check is sb_backtest_config 'class' is 'sb_backtest_config' and not 'sb_metabacktest_config'
                if(class(sb_backtest_config) == "sb_metabacktest_config"){
                  stop("run_port_backtest currently does not support running sb_metabacktests inside it. Please provide a",
                       "'sb_metabacktest_results' instead")
                }

                ##Run!
                sb_backtest_results <- run_sb_backtest(
                  #Base SB Objs
                  features_m_df = signals_m_df, target_m_df = target_m_df, config = sb_backtest_config,
                  #SS Objs
                  port_backtest_cohort = port_backtest_cohort, backtest_returns_m_xts = backtest_returns_m_xts, ##Derive backtest returns from either
                  benchmark_returns_m_xts = benchmark_returns_m_xts, ##Benchmark is the same object
                  signal_themes_m_df = signal_themes_m_df, priors_m_df = priors_m_df,
                  #Custom objs
                  custom_signal_weights_m_df = custom_signal_weights_m_df, custom_signal_universe_metrics_m_df = custom_signal_universe_metrics_m_df,
                  #Misc
                  winsorization_probs = winsorization_probs, gsm_algorithm = gsm_algorithm, .test_seed = .test_seed, verbose = verbose, parallel = parallel
                )
              }

              ###Extract oos_predictions_m_df
              if (!is.null(sb_backtest_results)){

                ####Check for object conformity
                  #####signals_m_df
                  if (sb_backtest_results@sb_backtest_workflow$features_object_name != signals_m_df@meta_dataframe_name){
                    stop("Signals object name does not match the one used in the SB Backtest")
                  }
                  #####benchmark_returns_m_xts
                  if (sb_backtest_results@sb_backtest_workflow$backtest_type != "base_learner" && #Test is only applicable for base-learners
                      sb_backtest_results@sb_backtest_workflow$benchmark_returns_object_name != benchmark_returns_m_xts@meta_xts_name){
                    stop("Benchmark Returns object name does not match the one used in the SB Backtest")
                  }

                ####Get results
                oos_predictions_m_df <- sb_backtest_results@oos_sb_outputs_m_df@data %>%
                  dplyr::select(id, tickers, dates, pred) #Get OOS Predictions and exclude target to be more confident of no data leakage
                oos_predictions_workflow <- sb_backtest_results@oos_sb_outputs_m_df@workflow
                oos_predictions_object_name <- sb_backtest_results@oos_sb_outputs_m_df@meta_dataframe_name
              } else {
                oos_predictions_m_df <- NULL
              }

           ###########################

           #Get data from S4 Obj
           ###########################
              ##signals_m_df
              signals_workflow <- signals_m_df@workflow #Get workflow
                ###Check for normalization
                if (!"normalization" %in% unlist(signals_workflow)){
                  warning ("Normalization not found in workflow. It is advisable that data is normalized before being fed to run_port_backtest.")
                }
                signals_object_name <- signals_m_df@meta_dataframe_name #Get mdf name
                signals_m_df <- signals_m_df@data #Get signals_m_df

              ##fwd_return_m_df
              fwd_return_workflow <- fwd_return_m_df@workflow #Get workflow
                ###Check for normalization
                if ("normalization" %in% unlist(fwd_return_workflow)){
                  stop ("Normalization found in fwd_return_m_df workflow.")
                }
                fwd_return_object_name <- fwd_return_m_df@meta_dataframe_name #Get mdf name
                fwd_return_m_df <- fwd_return_m_df@data #Get fwd_return_m_df

              ##liquidity_m_df
                liquidity_workflow <- liquidity_m_df@workflow #Get workflow
                ###Check for normalization
                if ("normalization" %in% unlist(liquidity_workflow)){
                  stop ("Normalization found in liquidity_m_df workflow.")
                }
                liquidity_object_name <- liquidity_m_df@meta_dataframe_name #Get mdf name
                liquidity_m_df <- liquidity_m_df@data #Get liquidity_m_df

              ##volatility_m_df
                volatility_workflow <- volatility_m_df@workflow #Get workflow
                ###Check for normalization
                if ("normalization" %in% unlist(volatility_workflow)){
                  stop ("Normalization found in volatility_m_df workflow.")
                }
                volatility_object_name <- volatility_m_df@meta_dataframe_name #Get mdf name
                volatility_m_df <- volatility_m_df@data #Get volatility_m_df

              ##Extract configuration parameters from the port_backtest_config object
                ###Base Backtest Config
                chosen_score_metric_and_position <- config@chosen_score_metric_and_position
                selected_benchmark <- config@selected_benchmark
                eligibility_quantile_range <- config@eligibility_quantile_range
                min_eligible_assets_fallback <- config@min_eligible_assets_fallback
                initial_buffer_period <- config@initial_buffer_period
                rebalancing_months <- config@rebalancing_months

                ###Port objs
                port_construction_method <- config@port_construction_method
                  ####Liquidity
                  main_liquidity_metric <- config@main_liquidity_metric
                  liquidity_floor_cutoffs <- config@liquidity_floor_cutoffs
                  liquidity_constraint_policy <- as.list(config@liquidity_constraint_policy) #All port construction can have liquidity floor rule


                  ####Transaction costs
                  transaction_costs_parameters <- as.list(config@transaction_costs_parameters)

                  ####Covariance Estimation
                    #####Config objs
                    cov_est_method <- config@cov_est_method
                    cov_estimation_method <- cov_est_method@cov_estimation_method
                    cov_matrix_sample_size <- cov_est_method@cov_matrix_sample_size
                    active_returns <- cov_est_method@active_returns
                    cov_matrix_benchmark <- cov_est_method@cov_matrix_benchmark

                    #####Data objs
                    if (!is.null(daily_stock_returns_m_xts)){
                      daily_stock_returns_object_name <- daily_stock_returns_m_xts@meta_xts_name
                      daily_stock_returns_workflow <- daily_stock_returns_m_xts@workflow
                      daily_stock_returns_m_xts <- daily_stock_returns_m_xts@data
                    }
                    if (!is.null(daily_bench_returns_m_xts)){
                      ###Check
                      if (is.null(selected_benchmark)){
                        stop("selected_benchmark must be provided with daily_bench_returns_m_xts.")
                      }
                      daily_bench_returns_object_name <- daily_bench_returns_m_xts@meta_xts_name
                      daily_bench_returns_workflow <- daily_bench_returns_m_xts@workflow
                      daily_bench_returns_m_xts <- daily_bench_returns_m_xts@data
                    }
                    if (!is.null(stock_groups_m_df)){ #Used for both NA filling as for concentration_constraint_policy group constraints
                      stock_groups_object_name <- stock_groups_m_df@meta_dataframe_name
                      stock_groups_workflow <- stock_groups_m_df@workflow
                      stock_groups_m_df <- stock_groups_m_df@data
                    }

                  ####Benchmark-Related
                    ####Bench weights
                    if (!is.null(benchmark_weights_m_df)){
                      ###Check
                      if (is.null(selected_benchmark)){
                        stop("selected_benchmark must be provided with benchmark_weights_m_df.")
                      }
                      benchmark_weights_object_name <- benchmark_weights_m_df@meta_dataframe_name
                      benchmark_weights_workflow <- benchmark_weights_m_df@workflow
                      benchmark_weights_m_df <- benchmark_weights_m_df@data
                    }
                    ####Bench Returns
                    if (!is.null(benchmark_returns_m_xts)){
                      ###Check
                      if (is.null(selected_benchmark)){
                        stop("selected_benchmark must be provided with benchmark_returns_m_xts.")
                      }
                      benchmark_returns_object_name <- benchmark_returns_m_xts@meta_xts_name
                      benchmark_returns_workflow <- benchmark_returns_m_xts@workflow
                      benchmark_returns_m_xts <- benchmark_returns_m_xts@data
                    }

                  ####Determine portfolio construction parameters based on the method
                  if (port_construction_method %in% c("rp")) {

                    ####RP Parameters
                    rp_parameters <- config@rp_parameters
                      rp_method <- rp_parameters@rp_method

                    } else if (port_construction_method %in% c("mvo")) {
                    ####MVO Parameters
                    mvo_parameters <- config@mvo_parameters
                      n_random_ports <- mvo_parameters@n_random_ports
                      random_ports_method <- mvo_parameters@random_ports_method
                      opt_objective <- mvo_parameters@opt_objective
                      opt_method <- mvo_parameters@opt_method
                      ##Constraints
                      if (!is.null(config@turnover_constraint_policy)) turnover_constraint_policy <- as.list(config@turnover_constraint_policy)
                      if (!is.null(config@concentration_constraint_policy)) concentration_constraint_policy <- as.list(config@concentration_constraint_policy)
                  }

                  ####Custom
                    #####Custom Stock Weights
                    if (!is.null(custom_stock_weights_m_df)){
                      custom_stock_weights_object_name <- custom_stock_weights_m_df@meta_dataframe_name
                      custom_stock_weights_workflow <- custom_stock_weights_m_df@workflow
                      custom_stock_weights_m_df <- custom_stock_weights_m_df@data
                    }
                    #####Custom Stock Metrics
                    if (!is.null(custom_stock_metrics_m_df)){
                      custom_stock_metrics_object_name <- custom_stock_metrics_m_df@meta_dataframe_name
                      custom_stock_metrics_workflow <- custom_stock_metrics_m_df@workflow
                      custom_stock_metrics_m_df <- custom_stock_metrics_m_df@data
                    }
                    #####User Defined OR Rules
                    if (!is.null(user_defined_OR_rules_m_df)){
                      user_defined_OR_rules_object_name <- user_defined_OR_rules_m_df@meta_dataframe_name
                      user_defined_OR_rules_workflow <- user_defined_OR_rules_m_df@workflow
                      user_defined_OR_rules_m_df <- user_defined_OR_rules_m_df@data
                    }
                    #####User Defined AND Rules
                    if (!is.null(user_defined_AND_rules_m_df)){
                      user_defined_AND_rules_object_name <- user_defined_AND_rules_m_df@meta_dataframe_name
                      user_defined_AND_rules_workflow <- user_defined_AND_rules_m_df@workflow
                      user_defined_AND_rules_m_df <- user_defined_AND_rules_m_df@data
                    }

           ###########################

           #Run Port Backtest Internal
           ###########################
           port_backtest_results <- run_port_backtest_internal(
             #Base Objects
             signals_m_df = signals_m_df, oos_predictions_m_df = oos_predictions_m_df, chosen_score_metric_and_position = chosen_score_metric_and_position, #Expected Return Score metric is needed when oos_predictions_m_df is not provided
             #Backtest Scheme
             rebalancing_months = rebalancing_months, initial_buffer_period = initial_buffer_period,
             #Portfolio Construction (If provided, selected_benchmark will give a benchmark-relative view)
             port_construction_method = port_construction_method, eligibility_quantile_range = eligibility_quantile_range, min_eligible_assets_fallback = min_eligible_assets_fallback,
             selected_benchmark = selected_benchmark,
             #RP/MVO Parameters
             rp_method = rp_method, n_random_ports = n_random_ports, random_ports_method = random_ports_method, opt_objective = opt_objective, opt_method = opt_method, #RP/MVO
             #Covariance Estimation
             cov_estimation_method = cov_estimation_method, cov_matrix_sample_size = cov_matrix_sample_size, active_returns = active_returns, cov_matrix_benchmark = cov_matrix_benchmark,
             daily_stock_returns_m_xts = daily_stock_returns_m_xts, daily_bench_returns_m_xts = daily_bench_returns_m_xts, benchmark_returns_m_xts = benchmark_returns_m_xts,
             #Constraints
             liquidity_constraint_policy = liquidity_constraint_policy, turnover_constraint_policy = turnover_constraint_policy, concentration_constraint_policy = concentration_constraint_policy,
             #Liquidity Information (Constraints and Active Returns Calculation)
             liquidity_m_df = liquidity_m_df, liquidity_floor_cutoffs = liquidity_floor_cutoffs, main_liquidity_metric = main_liquidity_metric,
             #Group and benchmark constraints (stock groups also used to fill covariance data)
             stock_groups_m_df = stock_groups_m_df, benchmark_weights_m_df = benchmark_weights_m_df,
             #Return calculation (needs also liquidity and vol for net returns)
             volatility_m_df = volatility_m_df, fwd_return_m_df = fwd_return_m_df, transaction_costs_parameters = transaction_costs_parameters,
             #Stock Weights
             custom_stock_weights_m_df = custom_stock_weights_m_df, custom_stock_metrics_m_df = custom_stock_metrics_m_df,
             user_defined_OR_rules_m_df = user_defined_OR_rules_m_df, user_defined_AND_rules_m_df = user_defined_AND_rules_m_df,
             #Misc
             lower_quantile_winsorization = lower_quantile_winsorization, upper_quantile_winsorization = upper_quantile_winsorization,
             verbose = verbose, parallel = parallel, .test_seed = .test_seed
           )
           ###########################

           #Adjust Port Backtest Results
           ###########################
             ##Add signal_blending_results
             port_backtest_results@sb_backtest_results <- sb_backtest_results

             ##IDs
             port_backtest_results@port_backtest_workflow$config_name <- config@config_name
             port_backtest_results@backtest_identifier <- paste0("c:", config@config_name, "_s:", signals_object_name, "_f:", fwd_return_object_name)
             port_backtest_results@port_backtest_workflow$backtest_identifier <- port_backtest_results@backtest_identifier

             ##Workflow and names for stock_universe, port_returns, port_metrics etc
               ###Workflow/Source
                 ####Meta Dataframes
                 port_backtest_results@stock_universe_m_df@workflow <- list(paste0("stock_universe_m_df result of ", port_backtest_results@backtest_identifier))
                 port_backtest_results@final_stock_universe_m_d_ref@workflow <- list(paste0("final_stock_universe_m_d_ref result of ", port_backtest_results@backtest_identifier))
                 port_backtest_results@port_weights_m_df@workflow <- list(paste0("port_weights_m_df result of ", port_backtest_results@backtest_identifier))
                 port_backtest_results@transactions_log@workflow <- list(paste0("transactions_log result of ", port_backtest_results@backtest_identifier))

                 ####Meta xts
                 port_backtest_results@port_returns_m_xts@source <- rep(paste0("port_backtest__:",port_backtest_results@port_backtest_workflow$backtest_identifier), ncol(port_backtest_results@port_returns_m_xts@data))
                 port_backtest_results@port_costs_m_xts@source <- rep(paste0("port_backtest__:",port_backtest_results@port_backtest_workflow$backtest_identifier), ncol(port_backtest_results@port_costs_m_xts@data))
                 if (!is.null(custom_stock_metrics_m_df)){
                   port_backtest_results@port_metrics_m_xts@source <- rep(paste0("port_backtest__:",port_backtest_results@port_backtest_workflow$backtest_identifier), ncol(port_backtest_results@port_metrics_m_xts@data))
                 }

                ###Names
                 ####Meta Dataframes
                 port_backtest_results@stock_universe_m_df@meta_dataframe_name <- paste0("port_backtest__:",port_backtest_results@port_backtest_workflow$backtest_identifier)
                 port_backtest_results@final_stock_universe_m_d_ref@meta_dataframe_name <- paste0("port_backtest__:",port_backtest_results@port_backtest_workflow$backtest_identifier)
                 port_backtest_results@port_weights_m_df@meta_dataframe_name <- paste0("port_backtest__:",port_backtest_results@port_backtest_workflow$backtest_identifier)


             ##Add workflows, config_names and objects
              ###Fwd Returns
              port_backtest_results@port_backtest_workflow$fwd_return_object_name <- fwd_return_object_name
              port_backtest_results@port_backtest_workflow$fwd_return_workflow <- fwd_return_workflow
              ###Signals
              port_backtest_results@port_backtest_workflow$signals_object_name <- signals_object_name
              port_backtest_results@port_backtest_workflow$signals_workflow <- signals_workflow
              ###Liquidity
              port_backtest_results@port_backtest_workflow$liquidity_object_name <- liquidity_object_name
              port_backtest_results@port_backtest_workflow$liquidity_workflow <- liquidity_workflow
              ###Volatility
              port_backtest_results@port_backtest_workflow$volatility_object_name <- volatility_object_name
              port_backtest_results@port_backtest_workflow$volatility_workflow <- volatility_workflow
              ###OOS Predictions
              if (!is.null(oos_predictions_m_df)){
                port_backtest_results@port_backtest_workflow$oos_predictions_object_name <- oos_predictions_object_name
                port_backtest_results@port_backtest_workflow$oos_predictions_workflow <- oos_predictions_workflow
                port_backtest_results@port_backtest_workflow$port_backtest_workflow$backtest_identifier <-
                  c(port_backtest_results@port_backtest_workflow$backtest_identifier, "_oos:", oos_predictions_object_name)
              }
              ###Stock Groups
              if (!is.null(stock_groups_m_df)){
                port_backtest_results@port_backtest_workflow$stock_groups_object_name <- stock_groups_object_name
                port_backtest_results@port_backtest_workflow$stock_groups_workflow <- stock_groups_workflow
              }
              ###Benchmark Weights
              if (!is.null(benchmark_weights_m_df)){
                port_backtest_results@port_backtest_workflow$benchmark_weights_object_name <- benchmark_weights_object_name
                port_backtest_results@port_backtest_workflow$benchmark_weights_workflow <- benchmark_weights_workflow
              }
              ###Benchmark Returns
              if (!is.null(benchmark_returns_m_xts)){
                port_backtest_results@port_backtest_workflow$benchmark_returns_object_name <- benchmark_returns_object_name
                port_backtest_results@port_backtest_workflow$benchmark_returns_workflow <- benchmark_returns_workflow
              }
              ###Daily Stock Returns
              if (!is.null(daily_stock_returns_m_xts)){
                port_backtest_results@port_backtest_workflow$daily_stock_returns_object_name <- daily_stock_returns_object_name
                port_backtest_results@port_backtest_workflow$daily_stock_returns_workflow <- daily_stock_returns_workflow
              }
              ###Daily Bench Returns
              if (!is.null(daily_bench_returns_m_xts)){
                port_backtest_results@port_backtest_workflow$daily_bench_returns_object_name <- daily_bench_returns_object_name
                port_backtest_results@port_backtest_workflow$daily_bench_returns_workflow <- daily_bench_returns_workflow
              }
              ###Custom Stock Weights
              if (!is.null(custom_stock_weights_m_df)){
                port_backtest_results@port_backtest_workflow$custom_stock_weights_object_name <- custom_stock_weights_object_name
                port_backtest_results@port_backtest_workflow$custom_stock_weights_workflow <- custom_stock_weights_workflow
              }
              ###Custom Stock Metrics
              if (!is.null(custom_stock_metrics_m_df)){
                port_backtest_results@port_backtest_workflow$custom_stock_metrics_object_name <- custom_stock_metrics_object_name
                port_backtest_results@port_backtest_workflow$custom_stock_metrics_workflow <- custom_stock_metrics_workflow
              }

            ##Call
            port_backtest_results@port_backtest_workflow$call <- sys.call(-2)

            return(port_backtest_results)
          })



#' Run Port Backtest
#'
#' The `run_port_backtest_internal` function backtests a portfolio based on signals derived from simple stock characteristics (simple factors) or expected returns obtained from sb model predictions.
#' It supports a myriad of portfolio construction methods, including equal-weighted (EW), cap-weighted (CW), custom-weighted, signal-weighted (SW), cap-scaled (CS), mean-variance optimization (MVO) and risk-parity (RP).
#' In the case of mean-variance optimization, various constraints (turnover, concentration, and liquidity) can be introduced to reduce transaction costs and tracking error.
#' Furthermore, the function accommodates multiple covariance estimation methods (e.g., sample, EWMA, constant correlation, PCA, shrinkage) and supports both agnostic and benchmark-relative backtests.
#'
#' @param signals_m_df A matrix or data frame containing simple features with at least the columns: \code{id}, \code{tickers}, and \code{dates}. These are used for constructing the expected return score.
#' @param oos_predictions_m_df A meta_dataframe object containing out-of-sample predictions from sb models (result of a \code{run_sb_backtest} call). Can be \code{NULL} if not provided.
#' @param chosen_score_metric_and_position An object or list specifying the expected return score metric and its associated position. This is required when \code{oos_predictions_m_df} is not provided.
#' @param rebalancing_months A numeric vector indicating the months (1-12) when the portfolio should be rebalanced.
#' @param initial_buffer_period An integer specifying the number of initial dates (from the ordered dates vector) to skip before starting the backtest.
#' @param port_construction_method A character string indicating the portfolio construction method. Possibilities include \code{"ew"}, \code{"sw"}, \code{"cw"}, \code{"cs"}, \code{"rp"}, or \code{"mvo"}.
#' @param eligibility_quantile_range A numeric vector (of length 2) specifying the quantile range to determine eligible assets.
#' @param selected_benchmark (Optional) A character string indicating the benchmark to use for a benchmark-relative backtest.
#'
#' @param rp_method A character string specifying the risk parity method (e.g., \code{"cyclical-spinu"}).
#' @param n_random_ports An integer specifying the number of random portfolios to generate for optimization (applicable for MVO/RP).
#' @param random_ports_method A character string specifying the method for generating random portfolios (e.g., \code{"sample"}).
#' @param opt_objective A character string indicating the optimization objective (e.g., \code{"sharpe"}).
#' @param opt_method A character string specifying the optimization method (e.g., \code{"random"}).
#'
#' @param cov_estimation_method A character string specifying the covariance estimation method. Options include \code{"sample"}, \code{"ewma"}, \code{"cc"} (constant correlation),
#' \code{"pca1"}, \code{"pca2"}, \code{"shrink_id"} (shrinkage to identity), or \code{"shrink_cc"} (shrinkage to constant correlation).
#' @param cov_matrix_sample_size An integer specifying the sample size to use when estimating the covariance matrix.
#' @param active_returns A logical indicating whether to compute active returns.
#' @param cov_matrix_benchmark (Optional) An object specifying the benchmark for covariance estimation.
#' @param daily_stock_returns_m_xts A meta_xts object containing daily stock returns.
#' @param daily_bench_returns_m_xts A meta_xts object containing daily benchmark returns.
#' @param benchmark_returns_m_xts A meta_xts object containing benchmark returns.
#'
#' @param liquidity_constraint_policy An object (or list) specifying the liquidity constraint policy.
#' @param turnover_constraint_policy An object (or list) specifying the turnover constraint policy.
#' @param concentration_constraint_policy An object (or list) specifying the concentration constraint policy.
#'
#' @param liquidity_m_df A data frame containing liquidity measures for stocks, with columns such as \code{id}, \code{tickers}, and \code{dates}.
#' @param liquidity_floor_cutoffs A data frame defining cutoff thresholds for liquidity classification.
#' @param main_liquidity_metric A character string indicating the primary liquidity metric to be used.
#'
#' @param stock_groups_m_df (Optional) A data frame providing stock group or sector information.
#' @param benchmark_weights_m_df (Optional) A data frame containing benchmark weights for stocks.
#'
#' @param volatility_m_df A data frame containing volatility measures for return calculations.
#' @param fwd_return_m_df A data frame with forward returns.
#' @param transaction_costs_parameters (Optional) An object specifying parameters for transaction cost calculations.
#'
#' @param custom_stock_weights_m_df (Optional) A data frame containing custom stock weights.
#' @param custom_stock_metrics_m_df (Optional) A data frame containing custom metrics for stocks.
#' @param user_defined_OR_rules_m_df (Optional) A data frame containing user-defined OR rules.
#' @param user_defined_AND_rules_m_df (Optional) A data frame containing user-defined AND rules.
#'
#' @param lower_quantile_winsorization A numeric value specifying the lower quantile cutoff for winsorization (default is 0.025).
#' @param upper_quantile_winsorization A numeric value specifying the upper quantile cutoff for winsorization (default is 0.975).
#'
#' @param verbose A logical indicating whether to print detailed progress messages (default is \code{TRUE}).
#' @param parallel A logical indicating whether to execute portions of the backtest in parallel (default is \code{TRUE}).
#'
#' @return An S4 object of class \code{port_backtest_results} containing:
#' \itemize{
#'   \item \code{port_weights_m_df}: A meta_dataframe with the portfolio weights.
#'   \item \code{transactions_log_m_df}: A meta_dataframe logging the portfolio transactions.
#'   \item \code{port_costs_m_xts}: A meta_xts object with portfolio cost metrics.
#'   \item \code{port_metrics_m_xts}: (Optional) A meta_xts object with portfolio performance metrics.
#'   \item \code{port_returns_m_xts}: A meta_xts object with portfolio return series.
#'   \item \code{final_stock_port}: The final stock portfolio object used in the backtest.
#'   \item \code{stock_universe_m_df} and \code{final_stock_universe_m_d_ref}: Meta dataframes representing the stock universe during the backtest.
#'   \item \code{port_backtest_workflow}: A list detailing the backtest workflow, including dates, parameters, and configuration settings.
#'   \item \code{backtest_identifier}: A character string identifier for the backtest.
#' }
#'
run_port_backtest_internal <- function(
  #Base Objects
  signals_m_df, oos_predictions_m_df = NULL, chosen_score_metric_and_position = NULL, #Expected Return Score metric is needed when oos_predictions_m_df is not provided
  #Backtest Scheme
  rebalancing_months, initial_buffer_period,
  #Portfolio Construction (If provided, selected_benchmark will give a benchmark-relative view)
  port_construction_method = "ew", eligibility_quantile_range = c(0.9, 1.0), selected_benchmark = NULL, min_eligible_assets_fallback = NULL,
  #RP/MVO Parameters
  rp_method = "cyclical-spinu", n_random_ports = 2000, random_ports_method = "sample", opt_objective = "sharpe", opt_method = "random", #RP/MVO
  #Covariance Estimation
  cov_estimation_method = "sample", cov_matrix_sample_size = 252, active_returns = FALSE, cov_matrix_benchmark = NULL,
  daily_stock_returns_m_xts = NULL, daily_bench_returns_m_xts = NULL, benchmark_returns_m_xts = NULL,
  #Constraints
  liquidity_constraint_policy, turnover_constraint_policy, concentration_constraint_policy,
  #Liquidity Information (Constraints and Active Returns Calculation)
  liquidity_m_df, liquidity_floor_cutoffs = NULL, main_liquidity_metric,
  #Group and benchmark constraints (stock groups also used to fill covariance data)
  stock_groups_m_df = NULL, benchmark_weights_m_df = NULL,
  #Return calculation (needs also liquidity and vol for net returns)
  volatility_m_df, fwd_return_m_df, transaction_costs_parameters,
  #Stock Weights
  custom_stock_weights_m_df = NULL, custom_stock_metrics_m_df = NULL,
  user_defined_OR_rules_m_df = NULL, user_defined_AND_rules_m_df = NULL,
  #Misc
  lower_quantile_winsorization = 0.025, upper_quantile_winsorization = 0.975,
  verbose = TRUE, parallel = TRUE, .test_seed = NULL){

  #Measure time to run and run gc
  elapsed_time <- system.time({

    #####################
    ##Check Parameters: This function will test whether inputs match format and current functionalities
    check_inputs_port_backtest(
      #Base Objects
      signals_m_df = signals_m_df, oos_predictions_m_df = oos_predictions_m_df, chosen_score_metric_and_position = chosen_score_metric_and_position,
      #Backtest Scheme
      rebalancing_months = rebalancing_months, initial_buffer_period = initial_buffer_period,
      #Portfolio Construction Method
      port_construction_method = port_construction_method, eligibility_quantile_range = eligibility_quantile_range, min_eligible_assets_fallback = min_eligible_assets_fallback,
      selected_benchmark = selected_benchmark,
      #RP/MVO Parameters
      rp_method = rp_method, n_random_ports = n_random_ports, random_ports_method = random_ports_method, opt_objective = opt_objective, opt_method = opt_method,
      #Covariance Estimation
      cov_estimation_method = cov_estimation_method, cov_matrix_sample_size = cov_matrix_sample_size, active_returns = active_returns, cov_matrix_benchmark = cov_matrix_benchmark,
      daily_stock_returns_m_xts = daily_stock_returns_m_xts, daily_bench_returns_m_xts = daily_bench_returns_m_xts, benchmark_returns_m_xts = benchmark_returns_m_xts,
      #Constraints
      liquidity_constraint_policy = liquidity_constraint_policy, turnover_constraint_policy = turnover_constraint_policy, concentration_constraint_policy = concentration_constraint_policy,
      #Liquidity Information (Constraints and Active Returns Calculation)
      liquidity_m_df = liquidity_m_df, liquidity_floor_cutoffs = liquidity_floor_cutoffs, main_liquidity_metric = main_liquidity_metric,
      #Group and benchmark constraints (stock groups also used to fill covariance data)
      stock_groups_m_df = stock_groups_m_df, benchmark_weights_m_df = benchmark_weights_m_df,
      #Return calculation (needs also liquidity and vol for net returns)
      volatility_m_df = volatility_m_df, fwd_return_m_df = fwd_return_m_df, transaction_costs_parameters = transaction_costs_parameters,
      #Custom Stock Weights, Metrics and OR/AND rules
      custom_stock_weights_m_df = custom_stock_weights_m_df, custom_stock_metrics_m_df = custom_stock_metrics_m_df,
      user_defined_OR_rules_m_df = user_defined_OR_rules_m_df, user_defined_AND_rules_m_df = user_defined_AND_rules_m_df,
      #Misc
      lower_quantile_winsorization = lower_quantile_winsorization, upper_quantile_winsorization = upper_quantile_winsorization,
      verbose = verbose
    )

    #####################

    ##Init objects
    #####################
    ###Extract dates objects
    dates_m_vector <- unique(as.Date(signals_m_df %>% dplyr::pull(dates), format = "%Y-%m-%d")) #coerce just to be sure
    dates_m_vector <- dates_m_vector[order(dates_m_vector)] #Re-order ascending just to be sure

    ###Backtest length
    backtest_length <- length(dates_m_vector) - initial_buffer_period + 1 #Backtest length follows signals_m_df format, varying accordingly to dates_m_vector.

    ###Backtest dates
    dates_backtest <- dates_m_vector[(initial_buffer_period):(initial_buffer_period + backtest_length - 1)] #These are dates of backtest

    ###Create dates vector for port returns (move all dates vector 1m in time)
    ####Extended dates for port returns according to fwd_return_m_df (target_fwd = 1)
    dates_port_returns <- c(dates_backtest, lubridate::add_with_rollback(dates_backtest[backtest_length], months(1)))
    ####Remove first date
    dates_port_returns <- dates_port_returns[-1]

    ###Rebalancing Dates
    first_rebalance_date <- min(dates_backtest) #Get first rebalancing date
    rebalance_dates <- unique( #Unique is to eliminate repeated dates, in case month of first_rebalance_date is a rebalancing month
      c(first_rebalance_date, dates_backtest[which(lubridate::month(dates_backtest) %in% rebalancing_months)]) #Dates corresponding to rebalancing_months
    )
    rebalance_dates <- rebalance_dates[order(rebalance_dates)] #Re-order

    ###Number of rebalancing months
    n_rebalance_months <- length(rebalance_dates)

    ###Last rebalance date
    last_rebalance_date <- max(rebalance_dates)

    ###Port Objects List
    ####Create port_weights list structure to stored merged port weights results
    port_weights_m_d_ref_list <- list()
    ####Create transaction_list
    transactions_log_m_d_ref_list <- list()

    ####Create object to store portfolio returns
    port_returns_m_xts <- xts::xts(data.frame(
      raw_return = rep(NA, length(dates_port_returns)), #Raw target returns
      net_return = rep(NA, length(dates_port_returns)) #Net target returns
    ), order.by = dates_port_returns) #These are most up-to-date portfolio returns (one month-ahead in time of dates backtest)

    ###Create object to store portfolio costs, with one day ahead of current_dates, as allocation is done the following day
    port_costs_m_xts <- xts::xts(data.frame(
      direct_cost = rep(NA, length(dates_port_returns)), #Direct cost
      market_impact_cost = rep(NA, length(dates_port_returns)), #Market impact cost
      total_cost = rep(NA, length(dates_port_returns)), #Total cost
      turnover = rep(NA, length(dates_port_returns)) #Turnover
    ), order.by = dates_backtest + 1) #These are most up-to-date portfolio costs (one day-ahead in time of dates backtest)

    ###Create object to store portfolio metrics
    if (!is.null(custom_stock_metrics_m_df)){
      ##Get most up-to-date portfolio metrics (matching current date)
      port_metrics_m_xts <- xts::xts(as.data.frame(lapply(
        custom_stock_metrics_m_df %>% dplyr::select(-id, -tickers, -dates),
               function(x) rep(NA, length(dates_backtest)))
      ), order.by = dates_backtest)
      ##Get colnames
      colnames(port_metrics_m_xts) <- colnames(custom_stock_metrics_m_df %>% dplyr::select(-id, -tickers, -dates))
    }

    ###Benchmark objects
    ####Create objects
    if (!is.null(selected_benchmark)){
      ####Insert benchmark in port_returns_m_xts
      port_returns_m_xts <- merge(port_returns_m_xts, #Add
                                  xts::xts(data.frame(selected_bench_return = rep(NA, length(dates_port_returns)),  #Create a bench_returns column
                                                      raw_active_return = rep(NA, length(dates_port_returns)), #Raw active returns
                                                      net_active_return = rep(NA, length(dates_port_returns)) #Net active returns
                                  ), order.by = dates_port_returns))

      ####Select benchmark_m_xts
      selected_benchmark_returns_m_xts <- benchmark_returns_m_xts[ ,selected_benchmark]

      ####Select daily benchmark_m_xts
      selected_daily_cov_matrix_bench_m_xts <- daily_bench_returns_m_xts[ ,cov_matrix_benchmark]

      ####Select benchmark_weights_m_df
      if (!is.null(benchmark_weights_m_df)){
        selected_benchmark_weights_m_df <- benchmark_weights_m_df %>% dplyr::select(id, tickers, dates, !!rlang::sym(selected_benchmark)) #Select only selected_benchmark
      } else {
        selected_benchmark_weights_m_df <- NULL
      }

      ####Create object to store benchmark metrics
      if (!is.null(custom_stock_metrics_m_df)){
        benchmark_metrics_m_xts <- xts::xts(as.data.frame(
          lapply(custom_stock_metrics_m_df %>% dplyr::select(-id, -tickers, -dates),
                 function(x) rep(NA, length(dates_backtest)))
        ), order.by = dates_backtest) #These are most up-to-date benchmark metrics
        colnames(benchmark_metrics_m_xts) <- paste0("bench_", colnames(benchmark_metrics_m_xts))
        ####Insert benchmark in port_metrics_m_xts
        port_metrics_m_xts <- merge(port_metrics_m_xts, benchmark_metrics_m_xts)
      }

    } else {
      selected_benchmark_returns_m_xts <- NULL
      selected_benchmark_weights_m_df <- NULL
      selected_daily_bench_returns_m_xts <- NULL
      selected_daily_cov_matrix_bench_m_xts <- NULL
    }


    ###Create stock universe list to get results
    stock_universe_m_d_ref_list <- list()

    #####################

    ##Initial Prints
    #########################
    if (verbose){
      ###Text otherwise
      cat("=============================\n")
      cat(crayon::cyan(paste("Portfolio Construction Method:", port_construction_method)))
      cat("\n")
      cat("Building portfolio based on:")
      cat("\n")
      cat(paste("  Expected Return Score Metric:"))
      cat("\n")
      if(!is.null(chosen_score_metric_and_position)){
        cat(paste0(names(chosen_score_metric_and_position), ": ", chosen_score_metric_and_position))
      } else {
        cat("OOS Signal Blend Predictions")
      }
      cat("\n")
      if (port_construction_method %in% c("rp", "mvo")){
        cat("  Covariance Matrix:")
        cat(paste("   Estimation Method:", cov_estimation_method))
        cat(paste("   Sample Size:", cov_matrix_sample_size))
        cat(paste("   Active Returns:", active_returns))

        if (port_construction_method == "mvo"){
          if (any(!is.null(concentration_constraint_policy), !is.null(liquidity_constraint_policy), !is.null(turnover_constraint_policy))){
            cat("  \n\nConstraints:")
            if (!is.null(concentration_constraint_policy)){
              cat("   \nConcentration Constraint Policy:")
              cat(paste("    Benchmark:", concentration_constraint_policy$benchmark))
              cat(paste("    Individual Constraints:", concentration_constraint_policy$max_abs_active_individual_weight))
              cat(paste("    Group Constraints:", concentration_constraint_policy$max_abs_active_group_weight))
            }
            if (!is.null(liquidity_constraint_policy)){
              cat(paste("   \nLiquidity Constraint Policy:", liquidity_constraint_policy$policy))
              cat(paste("   Liquidity Constraint Threshold:", liquidity_constraint_policy$threshold))
              cat(paste("   Liquidity Constraint Benchmark:", liquidity_constraint_policy$benchmark))
            }
            if (!is.null(turnover_constraint_policy)){
              cat(paste("   \nTurnover Constraint Policy:", turnover_constraint_policy$policy))
              cat(paste("   Turnover Constraint Threshold:", turnover_constraint_policy$threshold))
            }
          }
        }
      }
      cat(paste("  \n\nSelected Benchmark:", if(!is.null(selected_benchmark)) selected_benchmark else "None"))
      cat("\n")
    }
    #########################

    ##Start Backtest
    #####################
    ##Loop through
    for (d in (initial_buffer_period):(initial_buffer_period + backtest_length - 1)){
      ###Current and last date
      current_date <- dates_m_vector[d]
      last_date <- lubridate::add_with_rollback(current_date, months(-1)) #Get last month date
      next_date <- lubridate::add_with_rollback(current_date, months(1)) #Get next month date
      if (verbose) print(current_date)

      ###Get objects for current date
      ##############################
      ####Meta Dataframes
      #####Base Objects
      signals_m_d_ref <- signals_m_df %>% dplyr::filter(dates == current_date)
      fwd_return_m_d_ref <- fwd_return_m_df %>% dplyr::filter(dates == current_date)
      oos_predictions_m_d_ref <- if (!is.null(oos_predictions_m_df)) oos_predictions_m_df %>% dplyr::filter(dates == current_date) else NULL

      #####Stock Info
      liquidity_m_d_ref <- if (!is.null(liquidity_m_df)) liquidity_m_df %>% dplyr::filter(dates == current_date) else NULL
      volatility_m_d_ref <- if (!is.null(volatility_m_df)) volatility_m_df %>% dplyr::filter(dates == current_date) else NULL
      selected_benchmark_weights_m_d_ref <- if (!is.null(selected_benchmark_weights_m_df)) selected_benchmark_weights_m_df %>% dplyr::filter(dates == current_date) else NULL
      stock_groups_m_d_ref <- if (!is.null(stock_groups_m_df)) stock_groups_m_df %>% dplyr::filter(dates == current_date) else NULL
      custom_stock_weights_m_d_ref <- if (!is.null(custom_stock_weights_m_df)) custom_stock_weights_m_df %>% dplyr::filter(dates == current_date) else NULL
      custom_stock_metrics_m_d_ref <- if (!is.null(custom_stock_metrics_m_df)) custom_stock_metrics_m_df %>% dplyr::filter(dates == current_date) else NULL
      user_defined_AND_rules_m_d_ref <- if (!is.null(user_defined_AND_rules_m_df)) user_defined_AND_rules_m_df %>% dplyr::filter(dates == current_date) else NULL
      user_defined_OR_rules_m_d_ref <- if (!is.null(user_defined_OR_rules_m_df)) user_defined_OR_rules_m_df %>% dplyr::filter(dates == current_date) else NULL

      #####Port Weights
      #####End-of-period portfolio weights placeholder
      #####At this point, this is a placeholder with zero weights, reflecting all stocks that are currently in the universe.
      #####If it is a rebalancing month, merge_and_rescale_weights and calculate_trade_orders will generate transactions and costs needed to transform the old composition into rebalanced one.
      #####If it is not, merge_and_rescale will just exclude delisted stocks and rescale weights to sum to 1, with calculate_trade_orders also generating a transactions_m_df.
      #####Important: at apply_buffer_rule time, the function will look at current composition through signals_m_df and will check for positions in bop_port_weights_m_d_ref, using left_join. Therefore, delisted stocks will not be considered in the buffer rule.
      #####After merge_and_rescale_weights, eop_port_weights will then carry weights reflecting the end-of-period port. The object is then submitted to calc_port_returns, originating fwd_port_weights_m_d_ref.
      port_weights_placeholder_m_d_ref <- signals_m_d_ref %>%
        dplyr::select(id, tickers, dates) %>% #This represents most up-to-date portfolio weights
        dplyr::mutate(eop_port_weights = 0) #Initialize portfolio weights

      #####Old Composition Beggining-of-Period Portfolio Weights
      #####This is the old end-of-period portfolio with weights updated by fwd_1m_return (ie. composition from last period, with weights reflecting the current period). Delisted stocks are present at this point.
      if (d > initial_buffer_period){
        updated_port_weights_m_lstd_ref <- rolled_fwd_port_weights_m_d_ref %>%
          dplyr::select(id, tickers, dates, updated_port_weights) %>% #Unselect benchmark weights, if its the case
          dplyr::rename(bop_port_weights = updated_port_weights) #This is eop_port_weights from last period updated by fwd_1m_returns. This means that this carries the composition from last period.
      } else {
        #For first period, just get the composition of last period. Weights are initialized as zero.
        updated_port_weights_m_lstd_ref <- signals_m_df %>%
          dplyr::select(id, tickers, dates) %>%
          dplyr::filter(dates == last_date) %>%
          dplyr::mutate(bop_port_weights = 0)
      }

      ####Meta xts (up to date references)
      #####Daily Up-to-date reference
      daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts[which(zoo::index(daily_stock_returns_m_xts) <= current_date), ]
      if (!is.null(selected_daily_cov_matrix_bench_m_xts)){
        selected_daily_cov_matrix_bench_m_xts_upd_ref <-  selected_daily_cov_matrix_bench_m_xts[which(zoo::index(selected_daily_cov_matrix_bench_m_xts) <= current_date), ]
      } else {
        selected_daily_cov_matrix_bench_m_xts_upd_ref <- NULL
      }


      #####Fwd benchmark reference
      if (!is.null(selected_benchmark_returns_m_xts)){
        fwd_selected_benchmark_return <- selected_benchmark_returns_m_xts[which(zoo::index(selected_benchmark_returns_m_xts) == next_date), ] %>% as.numeric()
      } else {
        fwd_selected_benchmark_return <- NULL
      }

      ##############################

      ###Rebalance if it's a rebalancing month
      ##############################
      is_rebalancing_month <- (lubridate::month(current_date) %in% rebalancing_months) || d == (initial_buffer_period)
      if (is_rebalancing_month){

        ####Print refitting message
        if (verbose){
          cat("\n")
          cat(crayon::yellow(paste("Starting portfolio rebalancing at:", current_date)))
          cat("\n")
        }

        ####Create stock_universe_m_d_ref and classify it
        ##############################
        #####Derive Stock Universe
        stock_universe_m_d_ref <- derive_stock_universe_m_d_ref(
          #Signals
          signals_m_d_ref = signals_m_d_ref,

          #OOS Predictions
          oos_predictions_m_d_ref = oos_predictions_m_d_ref,

          #Chosen Score Metric and Position
          chosen_score_metric_and_position = chosen_score_metric_and_position,

          #Winsorization
          lower_quantile_winsorization = lower_quantile_winsorization,
          upper_quantile_winsorization = upper_quantile_winsorization
        )

        #####Classify Stock Universe
        stock_universe_m_d_ref <- classify_investment_universe(
          #Stock Universe
          universe_m_d_ref = stock_universe_m_d_ref,

          #Regular eligibility
          eligibility_quantile_range = eligibility_quantile_range, #Quantile range to elect stocks
          min_eligible_assets_fallback = min_eligible_assets_fallback, #Min number of assets to elect

          ##Liquidity floor rule and classification
          liquidity_m_d_ref = liquidity_m_d_ref, #Liquidity information to apply liquidity floor rule
          liquidity_constraint_policy =  liquidity_constraint_policy, #Liquidity policy
          liquidity_floor_cutoffs = liquidity_floor_cutoffs, #Definitions about liquidity

          ##Active concentration eligiblity
          benchmark_weights_m_d_ref = selected_benchmark_weights_m_d_ref, #Selected benchmark weights information to apply
          groups_m_d_ref = stock_groups_m_d_ref, #Sectors data
          concentration_constraint_policy = concentration_constraint_policy, #Active weights policy

          ##Turnover eligibility
          updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref, #Old portfolio weights to apply buffer rule
          turnover_constraint_policy = turnover_constraint_policy, #Turnover policy

          #User defined rules
          user_defined_AND_rules_m_d_ref = user_defined_AND_rules_m_d_ref,
          user_defined_OR_rules_m_d_ref = user_defined_OR_rules_m_d_ref
        )

        #####Subset Daily Stock Returns
        selected_daily_stock_returns_m_xts_upd_ref <- daily_stock_returns_m_xts_upd_ref[, stock_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers)]


        ##############################

        ####Set Portfolio Weights
        ##############################
          ###Place a .test_seed if needed
          if (!is.null(.test_seed)){
            set.seed(.test_seed)
          }

          ###Run Portfolio Construction
          stock_port <- set_portfolio_weights(
            #Stock universe object
            universe_m_d_ref = stock_universe_m_d_ref,
            #Stock Portfolio Construction method
            port_construction_method = port_construction_method,
            #Set liquidity constraint policy for stocks
            liquidity_constraint_policy = liquidity_constraint_policy, liquidity_m_d_ref = liquidity_m_d_ref, cap_weighting_metric = main_liquidity_metric,
            #Set concentration constraint policy for stocks
            concentration_constraint_policy = concentration_constraint_policy,
            #Set turnover constraint policy for stocks
            turnover_constraint_policy = turnover_constraint_policy,
            #Groups
            groups_m_d_ref = stock_groups_m_d_ref,
            #Covariance Estimation Method
            cov_estimation_method = cov_estimation_method, cov_matrix_sample_size = cov_matrix_sample_size, #Sample size to estimate cov matrix (NULL => full period)
            active_returns = active_returns,
            #Returns sample for covariance estimation
            returns_m_xts_upd_ref = selected_daily_stock_returns_m_xts_upd_ref, selected_benchmark_m_xts_upd_ref = selected_daily_cov_matrix_bench_m_xts_upd_ref,
            #Risk-Parity method
            rp_method = rp_method,
            #MVO Optimization
            n_random_ports = n_random_ports, random_ports_method = random_ports_method, opt_objective = opt_objective, opt_method = opt_method,
            #Custom Weights
            custom_weights_m_d_ref = custom_stock_weights_m_d_ref,
            #Winsorization
            lower_quantile_winsorization = lower_quantile_winsorization, upper_quantile_winsorization = upper_quantile_winsorization #Quantiles for winsorization
          )


          #####Transform port_obj into stock_port obj
          stock_port <- new(
            "stock_port",
            universe_m_d_ref = stock_port@universe_m_d_ref,
            port_construction_method = stock_port@port_construction_method,
            eligible_assets = stock_port@eligible_assets,
            exp_ret_score = stock_port@exp_ret_score,
            covariance_matrix = stock_port@covariance_matrix,
            correlation_matrix = stock_port@correlation_matrix,
            weights = stock_port@weights,
            rel_risk_contr = stock_port@rel_risk_contr,
            mvo_port_spec = stock_port@mvo_port_spec,
            random_port_weights = stock_port@random_port_weights,
            ind_max_weights = stock_port@ind_max_weights,
            ind_min_weights = stock_port@ind_min_weights,
            groups = stock_port@groups,
            port_name = stock_port@port_name,
            type = if (port_construction_method == "custom_weights") "custom_weights" else if (!is.null(oos_predictions_m_df)) "signal_blend" else "single_signal",
            main_liquidity_metric = main_liquidity_metric
          )

          #####Get stock_universe_m_d_ref
          stock_universe_m_d_ref <- stock_port@universe_m_d_ref@data
          stock_universe_m_d_ref_list[[which(rebalance_dates %in% current_date)]] <- stock_universe_m_d_ref

        ##############################
      }

      ####Print
      if(verbose){
        cat("\n")
        cat(crayon::green(paste("Portfolio rebalancing completed")))
      }

      ##############################

      ###Allocate Portfolio
      ##############################
      ####Print
      if(verbose){
        cat("\n")
        cat("Allocating portfolio")
        cat("\n")
      }

      ###Allocate the portfolio and register transactions and costs
      port_allocation_results_list <- allocate_port(
        #Compute Transactions
          ##Bop and eop port weights (bop will be passed to be rescaled in case of no rebalancing and then added to placeholder)
          port_weights_placeholder_m_d_ref = port_weights_placeholder_m_d_ref, updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref,
          ##Stock universe (If rebalancing month, rebalanced weights will be passed along)
          stock_universe_m_d_ref = if (is_rebalancing_month) stock_universe_m_d_ref else NULL,

        #Transaction costs data
          ##Liquidity and vol data
          liquidity_m_d_ref = liquidity_m_d_ref, volatility_m_d_ref = volatility_m_d_ref, main_liquidity_metric = main_liquidity_metric,
          ##BARRA parameters and direct cost
          transaction_costs_parameters <- transaction_costs_parameters,

        #Selected benchmark weights
        selected_benchmark_weights_m_d_ref = selected_benchmark_weights_m_d_ref,

        #Misc
        verbose = verbose
      )

      ####Collect data
      #####Portfolio weights
      port_weights_m_d_ref_list[[d - initial_buffer_period + 1]] <- port_allocation_results_list$port_weights_m_d_ref
      transactions_log_m_d_ref_list[[d - initial_buffer_period + 1]] <- port_allocation_results_list$transactions_log_m_d_ref

      #####Portfolio costs
      port_costs_m_xts[current_date + 1, ] <- port_allocation_results_list$port_costs_d_ref %>%
        dplyr::select(colnames(port_costs_m_xts)) %>% #Ensure order
        as.numeric() #Convert to numeric

      ##############################

      ###Calculate Port Metrics
      ##############################
      ####Calculate Port Metrics if provided
      if (!is.null(custom_stock_metrics_m_d_ref)){
        port_metrics_d_ref <- calculate_port_metrics(
          #Port Weights
          port_weights_m_d_ref = port_allocation_results_list$port_weights_m_d_ref,
          #Custom Metrics
          custom_stock_metrics_m_d_ref = custom_stock_metrics_m_d_ref
        )

        ####Collect data
        #####Portfolio costs
        port_metrics_m_xts[current_date, ] <- port_metrics_d_ref %>%
          dplyr::select(colnames(port_metrics_m_xts)) %>% #Ensure order
          as.numeric() #Convert to numeric

      }

      ##############################

      ###Roll Portfolio
      ##############################

      ####Roll Portfolio
      rolled_port_results_list <- roll_port(
        #Fwd Returns
        fwd_return_m_d_ref = fwd_return_m_d_ref, fwd_selected_benchmark_return = fwd_selected_benchmark_return,
        #Current Weights
        port_weights_m_d_ref = port_allocation_results_list$port_weights_m_d_ref,
        #Total cost
        total_cost = port_allocation_results_list$port_costs_d_ref %>% dplyr::pull(total_cost),
        #Verbose
        verbose = verbose
      )

      ####Collect data
      if (!is.null(rolled_port_results_list$fwd_port_returns_d_ref)){
        ####Returns
        port_returns_m_xts[next_date, ] <- rolled_port_results_list$fwd_port_returns_d_ref %>%
          dplyr::rename_with(~stringr::str_remove(., "fwd_")) %>% #Remove fwd_ prefix
          dplyr::select(colnames(port_returns_m_xts)) %>% #Ensure order
          as.numeric() #Convert to numeric

        ####Updated Port weights (current portfolio with weights updated to next period)
        rolled_fwd_port_weights_m_d_ref <- rolled_port_results_list$rolled_fwd_port_weights_m_d_ref #Updated weights
      }

      ##############################
    }
    #End backtest
    #####################

  })

  #Print elapsed time
  print(elapsed_time)
  if(verbose) cat("=============================\n")


  ##Build Final Objs
  ##################


  ###port_backtest_workflow
  port_backtest_workflow <- list(
    #Method
    port_construction_metrod = port_construction_method,
    chosen_score_metric_and_position = chosen_score_metric_and_position,
    eligibility_quantile_range = eligibility_quantile_range,
    min_eligible_assets_fallback = min_eligible_assets_fallback,
    selected_benchmark = selected_benchmark,
    config_name = "not_identified",
    backtest_identifier = "not_identified",
    oos_predictions_object_name = "not_identified",
    oos_predictions_workflow = "not_identified",
    #Dates
    dates_covered = dates_m_vector,
    n_dates = length(dates_m_vector),
    dates_backtest = dates_backtest,
    initial_buffer_period = initial_buffer_period,
    dates_port_returns = dates_port_returns,
    first_rebalance_date = first_rebalance_date,
    rebalance_dates = rebalance_dates,
    last_rebalance_date = last_rebalance_date,
    n_rebalance_months = n_rebalance_months,
    rebalancing_months = rebalancing_months,
    #Stocks
    ids = signals_m_df %>% dplyr::pull(id),
    nobs = length(signals_m_df %>% dplyr::pull(id)),
    tickers = unique(signals_m_df %>% dplyr::pull(tickers)),
    n_stocks = length(unique(signals_m_df %>% dplyr::pull(tickers))),
    #Signals
    signals = colnames(signals_m_df[,-c(1:3)]),
    signals_workflow = NULL,
    signals_object_name = "not_identified",
    #Fwd Returns
    fwd_return_object_name = "not_identified",
    fwd_return_workflow = NULL,
    #Stock Groups
    stock_groups_object_name = "not_identified",
    stock_groups_workflow = NULL,
    #Custom
    custom_stock_metrics_object_name = "not_identified",
    custom_stock_metrics_workflow = NULL,
    custom_stock_weights_object_name = "not_identified",
    custom_stock_weights_workflow = NULL,
    #RP/MVO Parameters
    rp_method = rp_method,
    n_random_ports = n_random_ports,
    random_ports_method = random_ports_method,
    opt_objective = opt_objective,
    opt_method = opt_method,
    #Covariance Estimation
    cov_estimation_method = cov_estimation_method,
    cov_matrix_sample_size = cov_matrix_sample_size,
    active_returns = active_returns,
    cov_matrix_benchmark = cov_matrix_benchmark,
    benchmark_returns_object_name = "not_identified",
    benchmark_returns_workflow = NULL,
    daily_stocks_returns_object_name = "not_identified",
    daily_stocks_returns_workflow = NULL,
    daily_bench_returns_object_name = "not_identified",
    daily_bench_returns_workflow = NULL,
    #Constraints
    liquidity_constraint_policy = liquidity_constraint_policy,
    turnover_constraint_policy = turnover_constraint_policy,
    concentration_constraint_policy = concentration_constraint_policy,
    #Liquidity Information (Constraints and Active Returns Calculation)
    liquidity_object_name = "not_identified",
    liquidity_workflow = NULL,
    volatility_object_name = "not_identified",
    volatility_workflow = NULL,
    liquidity_floor_cutoffs = liquidity_floor_cutoffs,
    main_liquidity_metric = main_liquidity_metric,
    transaction_costs_parameters = transaction_costs_parameters,
    benchmark_weights_object_name = "not_identified",
    benchmark_weights_workflow = NULL,
    #Misc
    user_defined_OR_rules_object_name = "not_identified",
    user_defined_OR_rules_workflow = NULL,
    user_defined_AND_rules_object_name = "not_identified",
    user_defined_AND_rules_workflow = NULL,
    lower_quantile_winsorization = lower_quantile_winsorization,
    upper_quantile_winsorization = upper_quantile_winsorization,
    #Call
    call = match.call()
  )

  ###Port Weights
  port_weights_m_df <- create_meta_dataframe(do.call(rbind, port_weights_m_d_ref_list) %>% dplyr::arrange(id), type = "weights")
  ###Port Allocationg Log
  names(transactions_log_m_d_ref_list) <- dates_backtest
  transactions_log <- new("transactions_log", data = transactions_log_m_d_ref_list, workflow = port_backtest_workflow)

  ###Port Costs
  port_costs_m_xts <- create_meta_xts(port_costs_m_xts, type = "metrics",
                                      metric_name = "port_costs",
                                      meta_xts_name = "not_identified", source = rep("not_identified", ncol(port_costs_m_xts)))
  ###Port Metrics
  if (!is.null(custom_stock_metrics_m_d_ref)){
    metrics <- paste0(custom_stock_metrics_m_d_ref %>% dplyr::select(-id, -tickers, -dates) %>% colnames(), collapse = "_") #Derive metrics names
    port_metrics_m_xts <- create_meta_xts(port_metrics_m_xts, type = "metrics",
                                          metric_name = metrics,
                                          meta_xts_name = "not_identified", source = rep("not_identified", ncol(port_metrics_m_xts)))
  } else {
    port_metrics_m_xts <- NULL
  }
  ###Port Returns
  rows_to_keep <- !apply(port_returns_m_xts, 1, function(row) all(is.na(row))) #Exclude rows with all NAs
  port_returns_m_xts <- create_meta_xts(port_returns_m_xts[rows_to_keep,], type = "returns", asset_type = "ports",
                                        meta_xts_name = "not_identified", source = rep("not_identified", ncol(port_returns_m_xts)))

  ###Stock Universe (turn into a signle meta_dataframe)
    ####Complete
    stock_universe_m_df <- do.call(rbind, stock_universe_m_d_ref_list) %>% dplyr::arrange(id)
    rownames(stock_universe_m_df) <- NULL
    stock_universe_m_df <- create_meta_dataframe(stock_universe_m_df, type = "stock_universe", port_backtest_workflow = port_backtest_workflow)
    ####Final
    final_stock_universe_m_d_ref <- stock_universe_m_d_ref %>% dplyr::arrange(id)
    rownames(final_stock_universe_m_d_ref) <- NULL
    final_stock_universe_m_d_ref <- create_meta_dataframe(final_stock_universe_m_d_ref, type = "stock_universe", port_backtest_workflow = port_backtest_workflow)

  ###Get final object
    port_backtest_results <- new(
      "port_backtest_results",
      port_weights_m_df = port_weights_m_df,
      transactions_log = transactions_log,
      port_costs_m_xts = port_costs_m_xts,
      port_metrics_m_xts = port_metrics_m_xts,
      port_returns_m_xts = port_returns_m_xts,
      final_stock_port = stock_port,
      port_construction_method = port_construction_method,
      sb_backtest_results = NULL,
      stock_universe_m_df = stock_universe_m_df,
      final_stock_universe_m_d_ref = final_stock_universe_m_d_ref,
      port_backtest_workflow = port_backtest_workflow,
      backtest_identifier = port_backtest_workflow$backtest_identifier
    )

    #Return
    return(port_backtest_results)

}










