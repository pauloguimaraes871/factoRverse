#' Update Portfolio Backtest
#' The `update_port_backtest` function will take an existing `port_backtest_results` object and update it with
#' new dates. This function is useful when you want to add new dates to an existing backtest without having to re-run the entire backtest.
#'
#' @param signals_m_df A meta_dataframe containing the signal features. It must include at least the columns \code{id}, \code{tickers}, and \code{dates}.
#' @param fwd_return_m_df A meta_dataframe containing forward returns.
#' @param liquidity_m_df A meta_dataframe containing liquidity metrics.
#' @param volatility_m_df A meta_dataframe containing volatility metrics.
#' @param old_results An object of class \code{port_backtest_results} specifying the portfolio backtest results to be updated.
#' @param parallel Logical; if \code{TRUE}, executes parts of the backtest in parallel (default is \code{TRUE}).
#' @param ... Additional arguments (if needed).
#' @param updated_sb_backtest_results An optional object of class \code{sb_backtest_results} or \code{sb_metabacktest_results} used to update the backtest using style-based results.
#' @param stock_groups_m_df A \code{meta_dataframe} containing stock group classifications, if applicable.
#' @param benchmark_weights_m_df A \code{meta_dataframe} with benchmark weights.
#' @param daily_stock_returns_m_xts An object of class \code{meta_xts} with daily stock returns, used in covariance estimation.
#' @param daily_bench_returns_m_xts An object of class \code{meta_xts} with daily benchmark returns, used in covariance estimation.
#' @param benchmark_returns_m_xts An object of class \code{meta_xts} containing benchmark returns over the rebalancing periods.
#' @param custom_stock_weights_m_df A \code{meta_dataframe} with user-defined stock weights for portfolio construction.
#' @param custom_stock_metrics_m_df A \code{meta_dataframe} with user-defined metrics to be used in portfolio rules.
#' @param user_defined_OR_rules_m_df A \code{meta_dataframe} specifying OR-based portfolio inclusion rules.
#' @param user_defined_AND_rules_m_df A \code{meta_dataframe} specifying AND-based portfolio inclusion rules.
#' @param verbose Logical; if \code{TRUE}, prints progress messages (default is \code{TRUE}).
#' @param .test_seed Optional seed for reproducibility when testing.

#'
#' @return An object of class \code{port_backtest_results} containing the portfolio backtest results.
#'
#' @export
setGeneric("update_port_backtest", function(signals_m_df, fwd_return_m_df, liquidity_m_df, volatility_m_df, old_results, ...) standardGeneric("update_port_backtest"))

#' @describeIn update_port_backtest Updates a portfolio backtest using based on a \code{port_backtest_results} object.
#'
#' This method extracts the parameters from the \code{results} object (of class \code{port_backtest_results}), modifies initial_buffer_period, performs the
#' new backtest and then binds results
#'
#' @include class_definitions.R
#' @exportMethod update_port_backtest
setMethod("update_port_backtest",
          signature(signals_m_df = "meta_dataframe", fwd_return_m_df = "meta_dataframe", liquidity_m_df = "meta_dataframe", volatility_m_df = "meta_dataframe",
                    old_results = "port_backtest_results"),
          function(signals_m_df, fwd_return_m_df, liquidity_m_df, volatility_m_df, old_results, #Base Port Backtest Objs
                   updated_sb_backtest_results = NULL, #Updated SB Backtest Objs
                   stock_groups_m_df = NULL, benchmark_weights_m_df = NULL, ##Constraints Objs
                   daily_stock_returns_m_xts = NULL, daily_bench_returns_m_xts = NULL, benchmark_returns_m_xts = NULL, #Covariance Estimation
                   custom_stock_weights_m_df = NULL, custom_stock_metrics_m_df = NULL, user_defined_OR_rules_m_df = NULL, user_defined_AND_rules_m_df = NULL, #Custom Objs
                   verbose = TRUE, parallel = TRUE, .test_seed = NULL){

            #Get old_port_backtest_workflow
            old_port_workflow_last_batch <- old_results@port_backtest_workflow[[length(old_results@port_backtest_workflow)]]

            #Check adherence between new objects and old object (names and dates)
            #######################
            ##Check if current_date is equal to old_results current_date + 1 months
            if(signals_m_df@current_date != ##It will be checked that current_date matches across new objects in run_port_backtest
               lubridate::add_with_rollback(old_port_workflow_last_batch$current_date, months(1))){
              stop("The current_date in the new signals_m_df is not equal to the current_date in the old_results + 1 month")
            }

            ##SB Backtest Results
            if (!is.null(updated_sb_backtest_results)){

              ###Checks whether it is sb_metabacktest_results and retrive appropriate workflow
              if (inherits(updated_sb_backtest_results, "sb_metabacktest_results")){
                updated_sb_workflow_last_batch <-
                  updated_sb_backtest_results@meta_sb_backtest_results@sb_backtest_workflow[[length(updated_sb_backtest_results@meta_sb_backtest_results@sb_backtest_workflow)]]
              } else {
                updated_sb_workflow_last_batch <-
                  updated_sb_backtest_results@sb_backtest_workflow[[length(updated_sb_backtest_results@sb_backtest_workflow)]]
              }

              ###Check backtest identifier
              if (!identical(updated_sb_backtest_results@backtest_identifier, old_port_workflow_last_batch$sb_backtest_identifier)){
                stop("backtest_identifier in updated_sb_backtest_results does not match the one in old_results.")
              }
              if (updated_sb_workflow_last_batch$current_date !=
                  lubridate::add_with_rollback(old_port_workflow_last_batch$current_date, months(1))){
                stop("current_date in updated_sb_backtest_results does not match the one in old_results + 1 month.")
              }
            } else {
              ###This is the case for no updated_sb_backtest_results
              if (!is.null(old_port_workflow_last_batch$sb_backtest_identifier)){
                stop("sb_backtest_identifier in old_results is not NULL but updated_sb_backtest_results is.")
              }
            }

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
              signals_m_df = old_port_workflow_last_batch$signals_object_name,
              fwd_return_m_df = old_port_workflow_last_batch$fwd_return_object_name,
              liquidity_m_df = old_port_workflow_last_batch$liquidity_object_name,
              volatility_m_df = old_port_workflow_last_batch$volatility_object_name,
              benchmark_returns_m_xts = old_port_workflow_last_batch$benchmark_returns_object_name,
              stock_groups_m_df = old_port_workflow_last_batch$stock_groups_object_name,
              benchmark_weights_m_df = old_port_workflow_last_batch$benchmark_weights_object_name,
              daily_stock_returns_m_xts = old_port_workflow_last_batch$daily_stock_returns_object_name,
              daily_bench_returns_m_xts = old_port_workflow_last_batch$daily_bench_returns_object_name,
              custom_stock_weights_m_df = old_port_workflow_last_batch$custom_stock_weights_object_name,
              custom_stock_metrics_m_df = old_port_workflow_last_batch$custom_stock_metrics_object_name,
              user_defined_OR_rules_m_df = old_port_workflow_last_batch$user_defined_OR_rules_object_name,
              user_defined_AND_rules_m_df = old_port_workflow_last_batch$user_defined_AND_rules_object_name
            )

            old_objects_dates_covered_list <- list(  ##Baseline info for dates comparison
              signals_m_df = old_port_workflow_last_batch$signals_dates,
              fwd_return_m_df = old_port_workflow_last_batch$fwd_return_dates,
              liquidity_m_df = old_port_workflow_last_batch$liquidity_dates,
              volatility_m_df = old_port_workflow_last_batch$volatility_dates,
              benchmark_returns_m_xts = old_port_workflow_last_batch$benchmark_returns_dates,
              stock_groups_m_df = old_port_workflow_last_batch$stock_groups_dates,
              benchmark_weights_m_df = old_port_workflow_last_batch$benchmark_weights_dates,
              daily_stock_returns_m_xts = old_port_workflow_last_batch$daily_stock_returns_dates,
              daily_bench_returns_m_xts = old_port_workflow_last_batch$daily_bench_returns_dates,
              custom_stock_weights_m_df = old_port_workflow_last_batch$custom_stock_weights_dates,
              custom_stock_metrics_m_df = old_port_workflow_last_batch$custom_stock_metrics_dates,
              user_defined_OR_rules_m_df = old_port_workflow_last_batch$user_defined_OR_rules_dates,
              user_defined_AND_rules_m_df = old_port_workflow_last_batch$user_defined_AND_rules_dates
            )

            ##Perform check
            check_update_backtest_objects(new_objects_list = new_objects_list, old_objects_names_list = old_objects_names_list,
                                          old_objects_dates_covered_list = old_objects_dates_covered_list, n_update = 1)


            #######################

            #Update old port_backtest_config
            #######################
            new_config <- old_results@port_backtest_config
            #New update must happen at last date of last backtest, in order to re-use
            #now populated fwd_returns_m_df at the last date.
            #Suppose last date was 2023-05-15. This means that we have weights for 2023-05-15, but naturally we do not have fwd_returns for 2023-05-16.
            #In new update, we are in 2023-06-15. We take backtest back to 2023-05-15 and use fwd_returns obj, which will be used to roll port forward.
            new_config@initial_buffer_period <- old_port_workflow_last_batch$n_dates

            ##Check if new initial_buffer_period is equal to length(signals_m_df@data$dates)
            if(new_config@initial_buffer_period != length(unique(signals_m_df@data$dates)) - 1){
              stop("The new initial_buffer_period is not equal to amount of unique dates in signals_m_df - 1")
            }

            ##Get old winsorization probs
            winsorization_probs <- sort(c(old_port_workflow_last_batch$lower_quantile_winsorization, old_port_workflow_last_batch$upper_quantile_winsorization))


            #######################

            #Re-run!!
            #######################

            ##Retrive objects from last period
            .old_backtest_covered_dates <- old_objects_dates_covered_list[["signals_m_df"]] %>% sort()
            .old_backtest_port_weights_m_d_ref <- old_results@port_weights_m_df@data %>%
              dplyr::filter(dates == sort(.old_backtest_covered_dates)[length(.old_backtest_covered_dates)])
            .old_backtest_port_costs_d_ref <- old_results@port_costs_m_xts@data %>% as.data.frame() %>% dplyr::slice_tail(n = 1)

            updated_port_backtest_results <- run_port_backtest(
              ###Base Port Backtest Objs
              signals_m_df = signals_m_df, fwd_return_m_df = fwd_return_m_df, liquidity_m_df = liquidity_m_df, volatility_m_df, config = new_config,
              ###SB Backtest Results
              sb_backtest_results = updated_sb_backtest_results,
              ###Constraints Objs
              stock_groups_m_df = stock_groups_m_df, benchmark_weights_m_df = benchmark_weights_m_df, benchmark_returns_m_xts = benchmark_returns_m_xts,
              ###Covariance Estimation
              daily_stock_returns_m_xts = daily_stock_returns_m_xts, daily_bench_returns_m_xts = daily_bench_returns_m_xts,
              ###Custom Objs
              custom_stock_weights_m_df = custom_stock_weights_m_df, custom_stock_metrics_m_df = custom_stock_metrics_m_df,
              user_defined_OR_rules_m_df = user_defined_OR_rules_m_df, user_defined_AND_rules_m_df = user_defined_AND_rules_m_df,
              ###Winsorization
              winsorization_probs = winsorization_probs,
              ###Other
              verbose = verbose, parallel = parallel, .test_seed = .test_seed,
              .update = TRUE, .old_backtest_port_weights_m_d_ref = .old_backtest_port_weights_m_d_ref,
              .old_backtest_port_costs_d_ref = .old_backtest_port_costs_d_ref, .old_backtest_covered_dates = .old_backtest_covered_dates
            )

            #######################

            #Consolidate results
            #######################
            ##results objects
            ##No need to bind information already in old_port_backtest_outputs_list
            new_port_backtest_outputs_list <- list(
              port_weights_m_df = updated_port_backtest_results@port_weights_m_df,
              stock_universe_m_df = updated_port_backtest_results@stock_universe_m_df,
              port_returns_m_xts = updated_port_backtest_results@port_returns_m_xts,
              port_costs_m_xts = updated_port_backtest_results@port_costs_m_xts,
              port_metrics_m_xts = updated_port_backtest_results@port_metrics_m_xts
            )

            ##Consolidate
            ###m_df and m_xts
            updated_results_list <- consolidate_backtest_results(new_backtest_outputs_list = new_port_backtest_outputs_list,
                                                                 old_backtest_results = old_results)
            ###others
            updated_port_backtest_results@transactions_log@data <- c(old_results@transactions_log@data,
                                                                     updated_port_backtest_results@transactions_log@data)


            ##Reassign the updated objects back into 'updated_port_backtest_results'
            updated_port_backtest_results@port_weights_m_df   <- updated_results_list[["port_weights_m_df"]]
            updated_port_backtest_results@stock_universe_m_df <- updated_results_list[["stock_universe_m_df"]]
            updated_port_backtest_results@port_returns_m_xts  <- updated_results_list[["port_returns_m_xts"]]
            updated_port_backtest_results@port_costs_m_xts    <- updated_results_list[["port_costs_m_xts"]]
            updated_port_backtest_results@port_metrics_m_xts  <- updated_results_list[["port_metrics_m_xts"]]

            ###In case of an empty update
            if (is.null(updated_port_backtest_results@final_stock_port)){
              updated_port_backtest_results@final_stock_port <- old_results@final_stock_port
              updated_port_backtest_results@stock_universe_m_df <- old_results@stock_universe_m_df
              updated_port_backtest_results@final_stock_universe_m_d_ref <- old_results@final_stock_universe_m_d_ref
            }

            ###Consolidate port_backtest_workflow
            updated_port_backtest_results@port_backtest_workflow <- c(old_results@port_backtest_workflow, updated_port_backtest_results@port_backtest_workflow)
            names(updated_port_backtest_results@port_backtest_workflow)[length(names(updated_port_backtest_results@port_backtest_workflow))] <-
              paste0("update_", signals_m_df@current_date)

            #######################

            return(updated_port_backtest_results)

          })


#run_port_backtest--------------------------------------
#' Run Portfolio Backtest
#'
#' Main entry point for portfolio simulations in the `factoRverse` ecosystem.
#' This generic function defines the interface and documentation for `run_port_backtest()`.
#'
#' @name run_port_backtest
#' @title Run Portfolio Backtest
#' @export
setGeneric("run_port_backtest", function(signals_m_df, fwd_return_m_df, liquidity_m_df, volatility_m_df, config, ...) standardGeneric("run_port_backtest"))

#' @describeIn run_port_backtest Run Portfolio Backtest with Signal Filtering and Eligibility Rules
#'
#' Executes a full pipeline portfolio backtest, from asset/signal classification and eligibility filtering to benchmark construction and weight optimization, based on expected return scores (`exp_ret_score`) or signal performance. This function is designed to incorporate realistic portfolio constraints and rules—such as turnover, liquidity, group representativeness, and user-defined filters—while also supporting fallback logic to ensure a viable investment universe is selected in each rebalance date.
#'
#' @details
#' The function integrates multiple components:
#'
#' ## 1. **Stock Classification**
#' - If `asset_object = "stocks"`:
#'   Each asset is scored based on an `exp_ret_score`, and only those inside a defined quantile range (e.g., top 20%) are considered "pre-eligible". If too few assets are selected, a fallback expands the quantile range iteratively until a minimum number of assets is included or a maximum range width is reached.
#'
#' ## 2. **Eligibility Filtering**
#' A stock is promoted to the final investment universe (`filtered_universe`) if it satisfies at least one of several eligibility criteria:
#'
#' ### Regular Eligibility:
#' - **Quantile Rule**: The asset is within the top quantile of `exp_ret_score`.
#' - **Liquidity Floor Rule**: The asset meets a minimum liquidity threshold based on predefined liquidity classifications (e.g., micro_caps).
#'
#' ### Policy-Based Eligibility:
#' - **Turnover Policy**: Assets in buffer zones from the previous portfolio can be retained even if they fall outside the quantile rule.
#' - **Active Weights Constraint**: Assets with benchmark weights above the active weight threshold are automatically included.
#' - **Group Representativeness**: If no asset from a required group (e.g., sector, theme) is eligible, the top-scoring asset from that group is forcibly promoted to preserve group balance.
#'
#' ### Custom Rules:
#' - **user_defined_OR_rules**: Stocks matching *any* custom inclusion rule are always promoted.
#' - **user_defined_AND_rules**: Stocks that fail *any* custom exclusion rule are removed regardless of other criteria.
#'
#' ### Rule Hierarchy:
#' - **Active Weights Constraint** overrides all other filters.
#' - **Turnover Policy** takes precedence over liquidity rules.
#' - **OR rules** force inclusion, **AND rules** force exclusion.
#' - **Group representativeness** is a fallback to ensure all relevant themes or sectors are represented.
#'
#' ## 3. **Benchmark Construction**
#' The user may supply benchmark weights explicitly.
#'
#' ## 4. **Portfolio Construction**
#' After defining the filtered investment universe:
#' - The portfolio is constructed based on signal strength (via `exp_ret_score`), user-specified optimization policies, or equal weighting.
#' - Constraints (e.g., turnover, group weight limits) are applied at this stage, potentially using box or group constraints via `generate_box_constraints()` and `generate_group_constraints()`.
#'
#' ## 5. **Parallel and Verbose Execution**
#' The function supports verbose output and parallel execution to improve transparency and computational efficiency when backtesting over long time windows or using Bayesian inference.
#'
#' @param signals_m_df A `meta_dataframe` containing alpha signals. Must include columns `id`, `tickers`, and `dates`.
#' @param fwd_return_m_df A `meta_dataframe` of forward returns (e.g., 1M-ahead).
#' @param liquidity_m_df A `meta_dataframe` with liquidity metrics.
#' @param volatility_m_df A `meta_dataframe` with volatility metrics (e.g., 1M historical vol).
#' @param config A `port_backtest_config` object defining portfolio construction logic and constraints.
#' @param sb_backtest_results (Optional) An `sb_backtest_results` or `sb_metabacktest_results` object. If provided, its predictions are used in place of signals.
#' @param stock_groups_m_df (Optional) Sector or group data for use in group constraints.
#' @param benchmark_weights_m_df (Optional) Benchmark stock weights.
#' @param daily_stock_returns_m_xts (Optional) Daily stock returns for covariance estimation.
#' @param daily_bench_returns_m_xts (Optional) Daily benchmark returns (only if active returns are used).
#' @param benchmark_returns_m_xts (Optional) Monthly benchmark returns, used to compute active returns and benchmark-relative metrics.
#' @param custom_stock_weights_m_df (Optional) User-defined portfolio weights (used only with `port_construction_method = "custom_weights"`).
#' @param custom_stock_metrics_m_df (Optional) Additional metrics to be aggregated in the portfolio.
#' @param user_defined_OR_rules_m_df (Optional) Rules that override stock eligibility if any OR condition is met.
#' @param user_defined_AND_rules_m_df (Optional) Rules that override stock eligibility only if all conditions are met.
#' @param winsorization_probs Numeric vector of length 2 (default = c(0.025, 0.975)). Determines quantiles used to winsorize signals.
#' @param verbose Logical. If `TRUE`, prints progress logs and diagnostic information. Default is `TRUE`.
#' @param parallel Logical. If `TRUE`, runs computation in parallel. Default is `TRUE`.
#' @param .test_seed (Internal) Seed used during testing to ensure reproducibility. Default is `NULL`.
#' @param .update (Internal) Logical; whether this is an update to an existing backtest. Default is `FALSE`.
#' @param .old_backtest_port_weights_m_d_ref (Internal) Previously computed portfolio weights (used when `.update = TRUE`).
#' @param .old_backtest_port_costs_d_ref (Internal) Previously computed cost series (used when `.update = TRUE`).
#' @param .old_backtest_covered_dates (Internal) Dates already covered in the previous backtest (used when `.update = TRUE`).
#' @param ... Additional arguments passed to class-specific methods (e.g., cohort or single backtests).
#'
#' @return An object of class \code{port_backtest_results}, containing:
#' \itemize{
#'   \item \code{port_weights_m_df}: Portfolio weights by stock and date.
#'   \item \code{transactions_log}: Transaction log with costs and weights.
#'   \item \code{port_costs_m_xts}: Time series of cost metrics (turnover, market impact, etc.).
#'   \item \code{port_returns_m_xts}: Net and raw portfolio returns (and benchmark-relative returns if applicable).
#'   \item \code{port_metrics_m_xts}: Aggregated portfolio metrics (if `custom_stock_metrics_m_df` is supplied).
#'   \item \code{stock_universe_m_df}: Data frame with signal scores, eligibility flags, and classification for each stock.
#'   \item \code{port_backtest_workflow}: A list tracking workflow metadata, inputs, and date coverage.
#' }
#' @export
setMethod("run_port_backtest",
          signature(signals_m_df = "meta_dataframe", fwd_return_m_df = "meta_dataframe", liquidity_m_df = "meta_dataframe", volatility_m_df = "meta_dataframe",
                    config = "port_backtest_config"),

          function(signals_m_df, fwd_return_m_df, liquidity_m_df, volatility_m_df, config,  #Base Port Backtest Objs
                   sb_backtest_results = NULL, #SB Backtest Results
                   stock_groups_m_df = NULL, benchmark_weights_m_df = NULL, ##Constraints Objs
                   daily_stock_returns_m_xts = NULL, daily_bench_returns_m_xts = NULL, benchmark_returns_m_xts = NULL, #Covariance Estimation and active rets
                   custom_stock_weights_m_df = NULL, custom_stock_metrics_m_df = NULL, user_defined_OR_rules_m_df = NULL, user_defined_AND_rules_m_df = NULL, #Custom Objs
                   winsorization_probs = c(0.025, 0.975), #Winsorization
                   verbose = TRUE, parallel = TRUE, .test_seed = NULL,
                   .update = FALSE, .old_backtest_port_weights_m_d_ref = NULL, .old_backtest_port_costs_d_ref = NULL, .old_backtest_covered_dates = NULL) {

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

            #Get oos_sb_outputs_m_df
            ###########################
            ###Extract oos_predictions_m_df
            if (!is.null(sb_backtest_results)){

              ####Check if it is a sb_backtest_results or a sb_metabacktest_results
              if (inherits(sb_backtest_results, "sb_metabacktest_results")){
                #####Extract meta learner and config name
                meta_config_name <- sb_backtest_results@sb_metabacktest_config@config_name
                sb_backtest_results <- sb_backtest_results@meta_sb_backtest_results
              }

              ####Check for object conformity
              sb_backtest_workflow_last_batch <- sb_backtest_results@sb_backtest_workflow[[length(sb_backtest_results@sb_backtest_workflow)]]

              if (sb_backtest_workflow_last_batch$backtest_type == "base_learner"){
                #####signals_m_df
                if (sb_backtest_workflow_last_batch$features_object_name != signals_m_df@meta_dataframe_name){
                  stop("Signals object name does not match the one used in the SB Backtest")
                }
                #####benchmark_returns_m_xts
                if (sb_backtest_workflow_last_batch$backtest_type != "base_learner" && #Test is only applicable for base-learners
                    sb_backtest_workflow_last_batch$benchmark_returns_object_name != benchmark_returns_m_xts@meta_xts_name){
                  stop("Benchmark Returns object name does not match the one used in the SB Backtest")
                }
              } else {
                #####signals_m_df
                if (stringr::str_remove(sb_backtest_workflow_last_batch$features_object_name,
                                        pattern = paste0("m_config:", meta_config_name, "_", "f_mdf:")) !=
                    signals_m_df@meta_dataframe_name){
                  stop("Signals object name does not match the one used in the SB Backtest")
                }
              }

              ####Get results
              oos_predictions_m_df <- sb_backtest_results@oos_sb_outputs_m_df@data %>%
                dplyr::select(id, tickers, dates, pred) #Get OOS Predictions and exclude target to be more confident of no data leakage
              oos_predictions_workflow <- sb_backtest_results@oos_sb_outputs_m_df@workflow
              oos_predictions_object_name <- sb_backtest_results@oos_sb_outputs_m_df@meta_dataframe_name
              oos_predictions_current_date <- sb_backtest_results@oos_sb_outputs_m_df@current_date
            } else {
              if (is.null(config@chosen_score_metric_and_position) && config@port_construction_method != "custom_weights"){
                stop("chosen_score_metric_and_position must be provided if sb_backtest object is NULL and port_construction_method is not 'custom_weights'.")
              }
              oos_predictions_m_df <- NULL
            }

            ###########################

            #Get data from S4 Obj
            ###########################
            ##signals_m_df
            signals_workflow <- signals_m_df@workflow #Get workflow
            ###Check for normalization
            ####Identify workflow elements containing 'preprocessing_recipe'
            recipe_idx <- which(stringr::str_detect(names(signals_workflow), "preprocessing_recipe"))

            ####First check if there is a preprocessing_recipe
            if (length(recipe_idx) == 0){
              warning("Normalization not found in signals_m_df workflow. It is advisable that data is normalized before being fed to run_port_backtest.")
            }

            ####For each recipe found, check if any numeric transform step is present
            for (rec in lapply(recipe_idx, function(i) signals_workflow[[i]]$recipe)) {
              steps <- vapply(rec$steps, function(s) class(s)[1], "")
              if (!any(steps %in% c("step_center","step_scale","step_normalize","step_range")))
                warning("Normalization not found in signals_m_df workflow. It is advisable that data is normalized before being fed to run_port_backtest.")
            }
            signals_object_name <- signals_m_df@meta_dataframe_name #Get mdf name
            signals_current_date <- signals_m_df@current_date #Get current date
            signals_m_df <- signals_m_df@data #Get signals_m_df

            ##fwd_return_m_df
            fwd_return_workflow <- fwd_return_m_df@workflow #Get workflow
            ###Check for normalization
            ####Identify workflow elements containing 'preprocessing_recipe'
            recipe_idx <- which(stringr::str_detect(names(fwd_return_workflow), "preprocessing_recipe"))

            ####For each recipe found, check if any numeric transform step is present
            for (rec in lapply(recipe_idx, function(i) fwd_return_workflow[[i]]$recipe)) {
              steps <- vapply(rec$steps, function(s) class(s)[1], "")
              if (any(steps %in% c("step_center","step_scale","step_normalize","step_range")))
                stop("Normalization found in fwd_return_m_df workflow.")
            }
            fwd_return_object_name <- fwd_return_m_df@meta_dataframe_name #Get mdf name
            fwd_returns_current_date <- fwd_return_m_df@current_date #Get current date
            fwd_return_m_df <- fwd_return_m_df@data #Get fwd_return_m_df

            ##liquidity_m_df
            liquidity_workflow <- liquidity_m_df@workflow #Get workflow
            ###Check for normalization
            ####Identify workflow elements containing 'preprocessing_recipe'
            recipe_idx <- which(stringr::str_detect(names(liquidity_workflow), "preprocessing_recipe"))

            ####For each recipe found, check if any numeric transform step is present
            for (rec in lapply(recipe_idx, function(i) liquidity_workflow[[i]]$recipe)) {
              steps <- vapply(rec$steps, function(s) class(s)[1], "")
              if (any(steps %in% c("step_center","step_scale","step_normalize","step_range")))
                stop("Normalization found in liquidity_m_df workflow.")
            }

            liquidity_object_name <- liquidity_m_df@meta_dataframe_name #Get mdf name
            liquidity_current_date <- liquidity_m_df@current_date #Get current date
            liquidity_m_df <- liquidity_m_df@data #Get liquidity_m_df

            ##volatility_m_df
            volatility_workflow <- volatility_m_df@workflow #Get workflow
            ###Check for normalization
            ####Identify workflow elements containing 'preprocessing_recipe'
            recipe_idx <- which(stringr::str_detect(names(volatility_workflow), "preprocessing_recipe"))

            ####For each recipe found, check if any numeric transform step is present
            for (rec in lapply(recipe_idx, function(i) volatility_workflow[[i]]$recipe)) {
              steps <- vapply(rec$steps, function(s) class(s)[1], "")
              if (any(steps %in% c("step_center","step_scale","step_normalize","step_range")))
                stop("Normalization found in volatility_workflow workflow.")
            }
            volatility_object_name <- volatility_m_df@meta_dataframe_name #Get mdf name
            volatility_current_date <- volatility_m_df@current_date #Get current date
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
              daily_stock_returns_current_date <- daily_stock_returns_m_xts@current_date
              daily_stock_returns_m_xts <- daily_stock_returns_m_xts@data
            }
            if (!is.null(daily_bench_returns_m_xts)){
              ###Check
              if (is.null(selected_benchmark)){
                stop("selected_benchmark must be provided with daily_bench_returns_m_xts.")
              }
              daily_bench_returns_object_name <- daily_bench_returns_m_xts@meta_xts_name
              daily_bench_returns_workflow <- daily_bench_returns_m_xts@workflow
              daily_bench_returns_current_date <- daily_bench_returns_m_xts@current_date
              daily_bench_returns_m_xts <- daily_bench_returns_m_xts@data
            }
            if (!is.null(stock_groups_m_df)){ #Used for both NA filling as for concentration_constraint_policy group constraints
              stock_groups_object_name <- stock_groups_m_df@meta_dataframe_name
              stock_groups_workflow <- stock_groups_m_df@workflow
              stock_groups_current_date <- stock_groups_m_df@current_date
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
              benchmark_weights_current_date <- benchmark_weights_m_df@current_date
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
              benchmark_returns_current_date <- benchmark_returns_m_xts@current_date
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
              custom_stock_weights_current_date <- custom_stock_weights_m_df@current_date
              custom_stock_weights_m_df <- custom_stock_weights_m_df@data
            }
            #####Custom Stock Metrics
            if (!is.null(custom_stock_metrics_m_df)){
              custom_stock_metrics_object_name <- custom_stock_metrics_m_df@meta_dataframe_name
              custom_stock_metrics_workflow <- custom_stock_metrics_m_df@workflow
              custom_stock_metrics_current_date <- custom_stock_metrics_m_df@current_date
              custom_stock_metrics_m_df <- custom_stock_metrics_m_df@data
            }
            #####User Defined OR Rules
            if (!is.null(user_defined_OR_rules_m_df)){
              user_defined_OR_rules_object_name <- user_defined_OR_rules_m_df@meta_dataframe_name
              user_defined_OR_rules_workflow <- user_defined_OR_rules_m_df@workflow
              user_defined_OR_rules_current_date <- user_defined_OR_rules_m_df@current_date
              user_defined_OR_rules_m_df <- user_defined_OR_rules_m_df@data
            }
            #####User Defined AND Rules
            if (!is.null(user_defined_AND_rules_m_df)){
              user_defined_AND_rules_object_name <- user_defined_AND_rules_m_df@meta_dataframe_name
              user_defined_AND_rules_workflow <- user_defined_AND_rules_m_df@workflow
              user_defined_AND_rules_current_date <- user_defined_AND_rules_m_df@current_date
              user_defined_AND_rules_m_df <- user_defined_AND_rules_m_df@data
            }

            ###########################

            #Run Port Backtest Internal
            ###########################
            ##Check if all objects current_date match
            check_consistent_dates(list(
              if (!is.null(signals_m_df)) signals_current_date else NULL,
              if (!is.null(fwd_return_m_df)) fwd_returns_current_date else NULL,
              if (!is.null(volatility_m_df)) volatility_current_date else NULL,
              if (!is.null(liquidity_m_df)) liquidity_current_date else NULL,
              if (!is.null(stock_groups_m_df)) stock_groups_current_date else NULL,
              if (!is.null(benchmark_weights_m_df)) benchmark_weights_current_date else NULL,
              if (!is.null(benchmark_returns_m_xts)) benchmark_returns_current_date else NULL,
              if (!is.null(daily_stock_returns_m_xts)) daily_stock_returns_current_date else NULL,
              if (!is.null(daily_bench_returns_m_xts)) daily_bench_returns_current_date else NULL,
              if (!is.null(custom_stock_weights_m_df)) custom_stock_weights_current_date else NULL,
              if (!is.null(custom_stock_metrics_m_df)) custom_stock_metrics_current_date else NULL,
              if (!is.null(user_defined_OR_rules_m_df)) user_defined_OR_rules_current_date else NULL,
              if (!is.null(user_defined_AND_rules_m_df)) user_defined_AND_rules_current_date else NULL,
              if (!is.null(oos_predictions_m_df)) oos_predictions_current_date else NULL
            ))

            ##Run internal FUN
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
              verbose = verbose, parallel = parallel, .test_seed = .test_seed,
              #Update
              .update = .update, .old_backtest_port_weights_m_d_ref = .old_backtest_port_weights_m_d_ref,
              .old_backtest_port_costs_d_ref = .old_backtest_port_costs_d_ref, .old_backtest_covered_dates = .old_backtest_covered_dates
            )
            ###########################

            #Adjust Port Backtest Results
            ###########################

            ##Config
            port_backtest_results@port_backtest_config <- config

            ##IDs
            port_backtest_results@port_backtest_workflow$config_name <- config@config_name
            port_backtest_results@backtest_identifier <- paste0("c:", config@config_name, "_s:", signals_object_name, "_f:", fwd_return_object_name)
            port_backtest_results@port_backtest_workflow$backtest_identifier <- port_backtest_results@backtest_identifier
            port_backtest_results@port_backtest_workflow$current_date <- signals_current_date #already tested if all match

            ##Add sb identifier
            if (!is.null(sb_backtest_results)){
              port_backtest_results@port_backtest_workflow$sb_backtest_identifier <- sb_backtest_results@backtest_identifier
            }


            ##Workflow and names for stock_universe, port_returns, port_metrics etc
            ###Workflow/Source
            ####Meta Dataframes
            if (!(.update && length(port_backtest_results@stock_universe_m_df) == 0)){ #Skip empty updates
              port_backtest_results@stock_universe_m_df@workflow <- list(paste0("stock_universe_m_df result of ", port_backtest_results@backtest_identifier))
              port_backtest_results@final_stock_universe_m_d_ref@workflow <- list(paste0("final_stock_universe_m_d_ref result of ", port_backtest_results@backtest_identifier))
            }
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
            if (!(.update && length(port_backtest_results@stock_universe_m_df) == 0)){ #Skip empty updates
              port_backtest_results@stock_universe_m_df@meta_dataframe_name <- paste0("port_backtest__:",port_backtest_results@port_backtest_workflow$backtest_identifier)
              port_backtest_results@final_stock_universe_m_d_ref@meta_dataframe_name <- paste0("port_backtest__:",port_backtest_results@port_backtest_workflow$backtest_identifier)
            }
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
            ###user_defined_OR_rules_m_df
            if (!is.null(user_defined_OR_rules_m_df)){
              port_backtest_results@port_backtest_workflow$user_defined_OR_rules_object_name <- user_defined_OR_rules_object_name
              port_backtest_results@port_backtest_workflow$user_defined_OR_rules_workflow <- user_defined_OR_rules_workflow
            }
            ###user_defined_AND_rules_m_df
            if (!is.null(user_defined_AND_rules_m_df)){
              port_backtest_results@port_backtest_workflow$user_defined_AND_rules_object_name <- user_defined_AND_rules_object_name
              port_backtest_results@port_backtest_workflow$user_defined_AND_rules_workflow <- user_defined_AND_rules_workflow
            }

            ###Call
            port_backtest_results@port_backtest_workflow$call <- sys.call(-2)

            ###Add date to workflow
            port_backtest_results@port_backtest_workflow <- list(port_backtest_results@port_backtest_workflow)
            names(port_backtest_results@port_backtest_workflow) <- signals_current_date

            return(port_backtest_results)
          })



#' Run Portfolio Backtest (Internal)
#'
#' Internal engine for portfolio backtesting. Called by \code{run_port_backtest()}.
#' This function performs the core logic for portfolio filtering, constraint enforcement,
#' optimization, and return calculation.
#'
#' Not intended for direct use by package users.
#'
#' @param signals_m_df A `meta_dataframe` with signal scores. Required columns: `id`, `tickers`, `dates`, plus score columns.
#' @param oos_predictions_m_df Optional `meta_dataframe` with out-of-sample signal predictions.
#' @param chosen_score_metric_and_position Named character vector indicating which score to use and its direction ("long" or "short").
#' @param rebalancing_months Integer vector. Months (e.g., 1:12) during which rebalancing occurs.
#' @param initial_buffer_period Integer. Number of months to wait before beginning the backtest.
#' @param port_construction_method Character. Method to construct portfolio: "ew", "rp", "mvo", etc.
#' @param eligibility_quantile_range Numeric vector of length 2. Percentile cutoffs for eligible assets.
#' @param selected_benchmark Optional character. Name of benchmark index column in `benchmark_returns_m_xts`.
#' @param min_eligible_assets_fallback Optional integer. Minimum number of eligible assets to construct portfolio.
#' @param rp_method Character. Risk parity allocation method. Used if `port_construction_method = "rp"`.
#' @param n_random_ports Integer. Number of random portfolios to simulate (for `opt_method = "random"`).
#' @param random_ports_method Character. Method to sample random portfolios.
#' @param opt_objective Character. Optimization target (e.g., "sharpe"). Used in `mvo`.
#' @param opt_method Character. Optimization method: "random", "deterministic", etc.
#' @param cov_estimation_method Character. Method to estimate covariance matrix: "sample", "shrinkage", etc.
#' @param cov_matrix_sample_size Integer. Sample size for covariance estimation.
#' @param active_returns Logical. If TRUE, uses benchmark-adjusted returns.
#' @param cov_matrix_benchmark Character. Name of benchmark used in covariance estimation.
#' @param daily_stock_returns_m_xts An `xts` object with daily stock-level returns.
#' @param daily_bench_returns_m_xts An `xts` object with daily benchmark returns.
#' @param benchmark_returns_m_xts An `xts` object with monthly benchmark returns.
#' @param liquidity_constraint_policy A `list` defining liquidity constraints.
#' @param turnover_constraint_policy A `list` defining turnover constraints.
#' @param concentration_constraint_policy A `list` defining concentration constraints.
#' @param liquidity_m_df A `meta_dataframe` containing liquidity metrics.
#' @param liquidity_floor_cutoffs Optional numeric vector defining liquidity-based eligibility thresholds.
#' @param main_liquidity_metric Character. Column name in `liquidity_m_df` used as main liquidity measure.
#' @param stock_groups_m_df Optional `meta_dataframe` mapping stocks to groups (e.g., sectors).
#' @param benchmark_weights_m_df Optional `meta_dataframe` of historical benchmark weights.
#' @param volatility_m_df A `meta_dataframe` with volatility estimates for each stock.
#' @param fwd_return_m_df A `meta_dataframe` with forward return targets.
#' @param transaction_costs_parameters A list of transaction cost model parameters.
#' @param custom_stock_weights_m_df Optional `meta_dataframe` with user-specified portfolio weights.
#' @param custom_stock_metrics_m_df Optional `meta_dataframe` with custom metrics to compute.
#' @param user_defined_OR_rules_m_df Optional `meta_dataframe` defining additional OR rules.
#' @param user_defined_AND_rules_m_df Optional `meta_dataframe` defining additional AND rules.
#' @param lower_quantile_winsorization Numeric. Lower percentile for winsorizing input data.
#' @param upper_quantile_winsorization Numeric. Upper percentile for winsorizing input data.
#' @param verbose Logical. If TRUE, prints progress messages.
#' @param parallel Logical. If TRUE, enables parallel computation.
#' @param .test_seed Optional integer. Used to fix the seed for reproducibility.
#' @param .update Logical. If TRUE, activates update mode for extending existing backtests.
#' @param .old_backtest_port_weights_m_d_ref Optional. Previous period's portfolio weights.
#' @param .old_backtest_port_costs_d_ref Optional. Previous period's portfolio costs.
#' @param .old_backtest_covered_dates Optional. Vector of dates already covered by a prior backtest.
#'
#' @return An object of class `port_backtest_results`.
#' @noRd
#' @keywords internal
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
  verbose = TRUE, parallel = TRUE, .test_seed = NULL,
  #Update
  .update = FALSE, .old_backtest_port_weights_m_d_ref = NULL, .old_backtest_port_costs_d_ref = NULL, .old_backtest_covered_dates = NULL){


  #Measure time to run and run gc
  elapsed_time <- system.time({

    #####################
    ##Check installed packages
    if (verbose){
    if (!requireNamespace("crayon", quietly = TRUE) || !requireNamespace("tictoc", quietly = TRUE)) {
      stop("Packages 'crayon' and 'tictoc' are required to generate logs. Please install them using install.packages() or set verbose as FALSE.")
      }
    }
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
    if (!.update){
      ####Get first rebalancing date
      first_rebalance_date <- min(dates_backtest)
      ####Get all rebalancing dates
      rebalance_dates <- unique( #Unique is to eliminate repeated dates, in case month of first_rebalance_date is a rebalancing month
        c(first_rebalance_date, dates_backtest[which(lubridate::month(dates_backtest) %in% rebalancing_months)]) #Dates corresponding to rebalancing_months
      )
      ####Re-order ascending just to be sure
      rebalance_dates <- rebalance_dates[order(rebalance_dates)]
      ####Last rebalance date
      last_rebalance_date <- max(rebalance_dates)
    } else {
      rebalance_dates <- dates_backtest[which(lubridate::month(dates_backtest) %in% rebalancing_months)]
      rebalance_dates <- setdiff(rebalance_dates, .old_backtest_covered_dates) %>% as.Date() #Remove dates alredy covered
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
      if (!is.null(custom_stock_metrics_m_df) &&
          !is.null(selected_benchmark_weights_m_df)){ #Selected bench weights are needed to calc bench metrics
        ####Get most up-to-date benchmark metrics (matching current date)
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
      selected_daily_cov_matrix_bench_m_xts <- NULL
    }


    ###Create stock universe list to get results
    stock_universe_m_d_ref_list <- list()

    #####################

    ##Initial Prints
    #########################
    if (verbose){
      ###Text otherwise
      if (.update){
        cat(crayon::cyan(paste("Updating portfolio backtest")))
        cat("\n")
      } else {
        cat(crayon::cyan(paste("Starting portfolio backtest")))
        cat("\n")
      }
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
        if (last_date %in% unique(signals_m_df$dates)){
          ##If there is a last date, it will be used as reference for first updated_port_weights_m_lstd_ref
          updated_port_weights_m_lstd_ref <- signals_m_df %>%
            dplyr::select(id, tickers, dates) %>%
            dplyr::filter(dates == last_date) %>%
            dplyr::mutate(bop_port_weights = 0)
        } else {
          ##If there is no last date, current_date will be used as reference for first updated_port_weights_m_lstd_ref.
          ##This brings no problem to the backtest
          if (verbose) message(paste0("As initial_buffer_period is 1, current_date, instead of last_date, will be used as a reference for",
                                      " first updated_port_weights_m_lstd_ref. This brings no problem to the backtest, but invalidates delisted and ipo tickers",
                                      " inference at first period."))
          updated_port_weights_m_lstd_ref <- signals_m_df %>%
            dplyr::select(id, tickers, dates) %>%
            dplyr::filter(dates == current_date) %>%
            dplyr::mutate(bop_port_weights = 0)
          }
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

      #Rebalance if it's a rebalancing month
      ##############################
      #Define if it is a rebalancing month based on .update
      if (.update){
        #For an update, don't rebuild the portfolio at initial_buffer_period, as this port is already done.
        if (current_date %in% .old_backtest_covered_dates){
          is_rebalancing_month <- FALSE #Don't rebuild
          is_update_pickup <- TRUE #Pickup last port
        } else {
          is_rebalancing_month <- (lubridate::month(current_date) %in% rebalancing_months) #Rebalance at rebalancing months given it is not the initial_buffer_period
          is_update_pickup <- FALSE #Don't pickup last port
        }
      } else {
        is_rebalancing_month <- (lubridate::month(current_date) %in% rebalancing_months) || d == (initial_buffer_period)
        is_update_pickup <- FALSE
      }

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
        if (is_update_pickup){
          cat(crayon::green(paste("Picking-up old portfolio")))
        } else {
          cat(crayon::green(paste("Portfolio rebalancing completed")))
        }
      }

      ##############################

      ###Allocate Portfolio (if not an update pickup)
      ##############################
      if (!is_update_pickup){
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
      }

      ##############################

      ###Roll Portfolio
      ##############################

      ####Roll Portfolio
      rolled_port_results_list <- roll_port(
        #Fwd Returns
        fwd_return_m_d_ref = fwd_return_m_d_ref, fwd_selected_benchmark_return = fwd_selected_benchmark_return,
        #Current Weights
        port_weights_m_d_ref = if (is_update_pickup){
          .old_backtest_port_weights_m_d_ref #Just pick-up last portfolio in .update
        } else {
          port_allocation_results_list$port_weights_m_d_ref
        } ,
        #Total cost
        total_cost = if (is_update_pickup){
          .old_backtest_port_costs_d_ref %>% dplyr::pull(total_cost) #Just pick-up last portfolio in .update
        } else {
          port_allocation_results_list$port_costs_d_ref %>% dplyr::pull(total_cost)
        },
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
    signals_dates = sort(unique(dplyr::pull(signals_m_df, dates))),
    #Fwd Returns
    fwd_return_object_name = "not_identified",
    fwd_return_workflow = NULL,
    fwd_return_dates = sort(unique(dplyr::pull(fwd_return_m_df, dates))),
    #Stock Groups
    stock_groups_object_name = "not_identified",
    stock_groups_workflow = NULL,
    stock_groups_dates = if (!is.null(stock_groups_m_df)) sort(unique(dplyr::pull(stock_groups_m_df, dates))) else NULL,
    #Custom
    custom_stock_metrics_object_name = "not_identified",
    custom_stock_metrics_workflow = NULL,
    custom_stock_metrics_dates = if (!is.null(custom_stock_metrics_m_df)) sort(unique(dplyr::pull(custom_stock_metrics_m_df, dates))) else NULL,
    custom_stock_weights_object_name = "not_identified",
    custom_stock_weights_workflow = NULL,
    custom_stock_weights_dates = if (!is.null(custom_stock_weights_m_df)) sort(unique(dplyr::pull(custom_stock_weights_m_df, dates))) else NULL,
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
    benchmark_returns_dates = if (!is.null(benchmark_returns_m_xts)) zoo::index(benchmark_returns_m_xts) else NULL,
    daily_stocks_returns_object_name = "not_identified",
    daily_stocks_returns_workflow = NULL,
    daily_stocks_returns_dates = if (!is.null(daily_stock_returns_m_xts)) zoo::index(daily_stock_returns_m_xts) else NULL,
    daily_bench_returns_object_name = "not_identified",
    daily_bench_returns_workflow = NULL,
    daily_bench_returns_dates = if (!is.null(daily_bench_returns_m_xts)) zoo::index(daily_bench_returns_m_xts) else NULL,
    #Constraints
    liquidity_constraint_policy = liquidity_constraint_policy,
    turnover_constraint_policy = turnover_constraint_policy,
    concentration_constraint_policy = concentration_constraint_policy,
    #Liquidity Information (Constraints and Active Returns Calculation)
    liquidity_object_name = "not_identified",
    liquidity_workflow = NULL,
    liquidity_dates = if (!is.null(liquidity_m_df)) sort(unique(dplyr::pull(liquidity_m_df, dates))) else NULL,
    volatility_object_name = "not_identified",
    volatility_workflow = NULL,
    volatility_dates = if (!is.null(volatility_m_df)) sort(unique(dplyr::pull(volatility_m_df, dates))) else NULL,
    liquidity_floor_cutoffs = liquidity_floor_cutoffs,
    main_liquidity_metric = main_liquidity_metric,
    transaction_costs_parameters = transaction_costs_parameters,
    benchmark_weights_object_name = "not_identified",
    benchmark_weights_workflow = NULL,
    benchmark_weights_dates = if (!is.null(benchmark_weights_m_df)) sort(unique(dplyr::pull(benchmark_weights_m_df, dates))) else NULL,
    #Misc
    user_defined_OR_rules_object_name = "not_identified",
    user_defined_OR_rules_workflow = NULL,
    user_defined_OR_rules_dates = if (!is.null(user_defined_OR_rules_m_df)) sort(unique(dplyr::pull(user_defined_OR_rules_m_df, dates))) else NULL,
    user_defined_AND_rules_object_name = "not_identified",
    user_defined_AND_rules_workflow = NULL,
    user_defined_AND_rules_dates = if (!is.null(user_defined_AND_rules_m_df)) sort(unique(dplyr::pull(user_defined_AND_rules_m_df, dates))) else NULL,
    lower_quantile_winsorization = lower_quantile_winsorization,
    upper_quantile_winsorization = upper_quantile_winsorization,
    #Call
    update = .update,
    call = match.call()
  )

  ###Port Weights
  if (.update) port_weights_m_d_ref_list <- port_weights_m_d_ref_list[sapply(port_weights_m_d_ref_list, function(x) !is.null(x))]
  port_weights_m_df <- create_meta_dataframe(do.call(rbind, port_weights_m_d_ref_list) %>% dplyr::arrange(id), type = "weights")
  ###Port Allocationg Log
  names(transactions_log_m_d_ref_list) <- dates_backtest
  if (.update) transactions_log_m_d_ref_list <- transactions_log_m_d_ref_list[sapply(transactions_log_m_d_ref_list, function(x) !is.null(x))]
  transactions_log <- new("transactions_log", data = transactions_log_m_d_ref_list, workflow = port_backtest_workflow)

  ###Port Costs
  if (.update) port_costs_m_xts <- port_costs_m_xts %>% na.omit()
  port_costs_m_xts <- suppressMessages(
    create_meta_xts(port_costs_m_xts, type = "metrics",
                    metric_name = "port_costs",
                    meta_xts_name = "not_identified", source = rep("not_identified", ncol(port_costs_m_xts)))
  )
  ###Port Metrics
  if (!is.null(custom_stock_metrics_m_d_ref)){
    metrics <- paste0(custom_stock_metrics_m_d_ref %>% dplyr::select(-id, -tickers, -dates) %>% colnames(), collapse = "_") #Derive metrics names
    if (.update) port_metrics_m_xts <- port_metrics_m_xts %>% na.omit()
    port_metrics_m_xts <- suppressMessages(
      create_meta_xts(port_metrics_m_xts, type = "metrics",
                      metric_name = metrics,
                      meta_xts_name = "not_identified", source = rep("not_identified", ncol(port_metrics_m_xts)))
    )
  } else {
    port_metrics_m_xts <- NULL
  }
  ###Port Returns
  rows_to_keep <- !apply(port_returns_m_xts, 1, function(row) all(is.na(row))) #Exclude rows with all NAs
  port_returns_m_xts <- suppressMessages(
    create_meta_xts(port_returns_m_xts[rows_to_keep,], type = "returns", asset_type = "ports",
                    meta_xts_name = "not_identified", source = rep("not_identified", ncol(port_returns_m_xts)))
  )

  ###Stock Universe (turn into a signle meta_dataframe)
    if (.update && length(stock_universe_m_d_ref_list) == 0 && !exists("stock_port")){
      #####This refers to an empty update, when there is no rebalancing month in the update
      stock_universe_m_df <- NULL
      final_stock_universe_m_d_ref <- NULL
      stock_port <- NULL
    } else {
      if (.update){
        ###If stock_universe_m_d_ref_list is not empty, remove NULL elements
        stock_universe_m_d_ref_list <- stock_universe_m_d_ref_list[sapply(stock_universe_m_d_ref_list, function(x) !is.null(x))]
      }
      ####Get stock universes
        #####Complete
        stock_universe_m_df <- do.call(rbind, stock_universe_m_d_ref_list) %>% dplyr::arrange(id)
        rownames(stock_universe_m_df) <- NULL
        stock_universe_m_df <- create_meta_dataframe(stock_universe_m_df, type = "stock_universe", port_backtest_workflow = port_backtest_workflow)

        #####Final
        final_stock_universe_m_d_ref <- stock_universe_m_d_ref %>% dplyr::arrange(id)
        rownames(final_stock_universe_m_d_ref) <- NULL
        final_stock_universe_m_d_ref <- create_meta_dataframe(final_stock_universe_m_d_ref, type = "stock_universe", port_backtest_workflow = port_backtest_workflow)

    }

  ###Get final object
    port_backtest_results <- new(
      "port_backtest_results",
      port_weights_m_df = port_weights_m_df,
      transactions_log = transactions_log,
      port_costs_m_xts = port_costs_m_xts,
      port_metrics_m_xts = port_metrics_m_xts,
      port_returns_m_xts = port_returns_m_xts,
      final_stock_port = if (.update && !exists("stock_port")) NULL else stock_port, #If an update is applied without rebalancing, this will be NULL
      port_construction_method = port_construction_method,
      port_backtest_config = NULL,
      stock_universe_m_df = stock_universe_m_df, #If an update is applied without rebalancing, this will be NULL
      final_stock_universe_m_d_ref = final_stock_universe_m_d_ref, #If an update is applied without rebalancing, this will be NULL
      port_backtest_workflow = port_backtest_workflow,
      backtest_identifier = port_backtest_workflow$backtest_identifier,
      update = .update
    )

    #Return
    return(port_backtest_results)

}










