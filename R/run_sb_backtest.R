#' Update Signal Blending Backtest
#'
#' The `update_sb_backtest` function will take an existing `sb_backtest_results` object and update it with
#' new dates. This function is useful when you want to add new dates to an existing backtest without having to re-run the entire backtest.
#'
#' @param features_m_df A meta_dataframe containing features.
#' @param target_m_df A meta_dataframe containing target variable(s), with corresponding dates. Columns should follow the format XXXX_number_m, where
#' XXXX is the name of the target variable, number is the amount of forward periods and m indicates periods are measured in months.
#' @param old_results An object of class \code{sb_backtest_results} specifying the sb backtest results to be updated.
#' @param parallel Logical; if \code{TRUE}, executes parts of the backtest in parallel (default is \code{TRUE}).
#' @param ... Additional arguments (if needed).
#' @param updated_ss_backtest_results An optional object of class \code{ss_backtest_results}, used when the SB model depends on updated SS results.
#' @param updated_port_backtest_cohort An optional object of class \code{port_backtest_cohort} used to derive updated return and benchmark data.
#' @param updated_backtest_returns_m_xts A \code{meta_xts} object containing updated signal-based backtest returns.
#' @param benchmark_returns_m_xts A \code{meta_xts} object with benchmark returns over the backtest period.
#' @param signal_themes_m_df A \code{meta_dataframe} class object containing signal theme information for risk parity and MVO algorithms.
#' @param custom_signal_weights_m_df A \code{meta_dataframe} with user-defined signal weights.
#' @param custom_signal_universe_metrics_m_df A \code{meta_dataframe} with custom signal-level metrics for filtering or selection.
#' @param updated_base_sb_backtest_results A list of \code{sb_backtest_results} used for updating the base learners in the SB meta-backtest.
#' @param updated_base_port_backtest_cohort An optional \code{port_backtest_cohort} object used for the base learners.
#' @param updated_base_backtest_returns_m_xts A \code{meta_xts} with signal-based returns for the base learners.
#' @param base_benchmark_returns_m_xts A \code{meta_xts} object with benchmark returns used for the base learners.
#' @param base_signal_themes_m_df A \code{meta_dataframe} with signal themes used in the base learners.
#' @param base_priors_m_df A \code{meta_dataframe} containing prior beliefs or constraints used for Bayesian learners in the base layer.
#' @param base_custom_signal_weights_m_df A \code{meta_dataframe} of custom signal weights for base learners.
#' @param base_custom_signal_universe_metrics_m_df A \code{meta_dataframe} of custom signal metrics for base learners.
#' @param updated_meta_port_backtest_cohort An optional \code{port_backtest_cohort} used in the meta learner layer.
#' @param updated_meta_backtest_returns_m_xts A \code{meta_xts} with returns for the meta learner.
#' @param meta_benchmark_returns_m_xts A \code{meta_xts} with benchmark returns for the meta learner.
#' @param meta_signal_themes_m_df A \code{meta_dataframe} with signal themes used in the meta learner.
#' @param meta_priors_m_df A \code{meta_dataframe} with priors used by the
#'
#' @return An object of class \code{sb_backtest_results} containing the sb backtest results.
#'
#' @export
setGeneric("update_sb_backtest", function(features_m_df, target_m_df, old_results, ...) standardGeneric("update_sb_backtest"))

#' @describeIn update_sb_backtest Updates a sb backtest using based on a \code{sb_backtest_results} object.
#'
#' This method extracts the parameters from the \code{results} object (of class \code{sb_backtest_results}), modifies training_sample_size, performs the
#' new backtest and then binds results
#'
#' @include class_definitions.R
#' @export
setMethod("update_sb_backtest",
          signature(features_m_df = "meta_dataframe", target_m_df = "meta_dataframe", old_results = "sb_backtest_results"),

          function(features_m_df, target_m_df, old_results, #SB Backtest
                   updated_ss_backtest_results = NULL, #SS Backtest Results
                   updated_port_backtest_cohort = NULL, #Port Backtest Cohort
                   updated_backtest_returns_m_xts = NULL, benchmark_returns_m_xts = NULL, signal_themes_m_df = NULL, #Cov Estimation
                   custom_signal_weights_m_df = NULL, custom_signal_universe_metrics_m_df = NULL, #Custom objects
                   verbose = TRUE, parallel = TRUE, .test_seed = NULL){

            #Get old_sb_backtest workflow
            old_sb_workflow_last_batch <-  old_results@sb_backtest_workflow[[length(old_results@sb_backtest_workflow)]]

            #Check adherence between new objects and old object (names and dates)
            #######################
            ##Check if current_date is equal to old_results current_date + 1 months
            if(features_m_df@current_date != ##It will be checked that current_date matches across new objects in run_port_backtest
               lubridate::add_with_rollback(old_sb_workflow_last_batch$current_date, months(1))){
              stop("The current_date in the new features_m_df is not equal to the current_date in the old_results + 1 month")
            }
            ##SS Backtest Results
            if (!is.null(updated_ss_backtest_results)){
              ###Check backtest identifier
              if (!identical(updated_ss_backtest_results@backtest_identifier, old_sb_workflow_last_batch$ss_backtest_identifier)){
                stop("backtest_identifier in updated_ss_backtest_results does not match the one in old_results.")
              }
              if (updated_ss_backtest_results@ss_backtest_workflow[[length(updated_ss_backtest_results@ss_backtest_workflow)]]$current_date !=
                  lubridate::add_with_rollback(old_sb_workflow_last_batch$current_date, months(1))){
                stop("current_date in updated_ss_backtest_results does not match the one in old_results + 1 month.")
              }
            } else {
              ###This is the case for no updated_ss_backtest_results
              if (!is.null(old_sb_workflow_last_batch$ss_backtest_identifier)){
                stop("ss_backtest_identifier in old_results is not NULL but updated_ss_backtest_results is.")
              }
            }
            ##Port backtest cohort
            if (!is.null(updated_port_backtest_cohort)){
              ###Check identifier
              if (!identical(updated_port_backtest_cohort@cohort_name, old_sb_workflow_last_batch$backtest_returns_object_name)){
                stop("cohort_name in updated_port_backtest_cohort does not match the one in old_results.")
              }
              if (updated_port_backtest_cohort@backtest_workflow_common$current_date !=
                  lubridate::add_with_rollback(old_sb_workflow_last_batch$current_date, months(1))){
                stop("current_date in updated_port_backtest_cohort does not match the one in old_results + 1 month.")
              }

              ###Extract returns_m_xts (this must be done before check_update_backtest_objects to enable backtest and benchmark comparison)
              extracted_returns_m_xts <- extract_returns_m_xts(
                port_backtest_cohort = updated_port_backtest_cohort, #Port Backtest Cohort
                signals_m_df = features_m_df,
                benchmark_returns_m_xts = benchmark_returns_m_xts, #Objects to check consistency
                verbose = verbose
              )
              ###Assign extracted values
              updated_backtest_returns_m_xts <- extracted_returns_m_xts$backtest_returns_m_xts
              benchmark_returns_m_xts <- extracted_returns_m_xts$benchmark_returns_m_xts

            }

            ##Gather all arguments into a single named list (only those that have @meta_dataframe_name or meta_xts_name)
            new_objects_list <- list(
              features_m_df = features_m_df,
              target_m_df = target_m_df,
              backtest_returns_m_xts = updated_backtest_returns_m_xts,
              benchmark_returns_m_xts = benchmark_returns_m_xts,
              signal_themes_m_df = signal_themes_m_df
            )

            old_objects_names_list <- list(
              features_m_df = old_sb_workflow_last_batch$features_object_name,
              target_m_df = old_sb_workflow_last_batch$target_object_name,
              backtest_returns_m_xts = old_sb_workflow_last_batch$backtest_returns_object_name,
              benchmark_returns_m_xts = old_sb_workflow_last_batch$benchmark_returns_object_name,
              signal_themes_m_df = old_sb_workflow_last_batch$signal_themes_object_name
            )

            old_objects_dates_covered_list <- list(  ##Baseline info for dates comparison
              features_m_df = old_sb_workflow_last_batch$features_dates,
              target_m_df = old_sb_workflow_last_batch$target_dates,
              backtest_returns_m_xts = old_sb_workflow_last_batch$backtest_returns_dates,
              benchmark_returns_m_xts = old_sb_workflow_last_batch$benchmark_returns_dates,
              signal_themes_m_df = old_sb_workflow_last_batch$signal_themes_dates
            )

            ##Perform check
            check_update_backtest_objects(new_objects_list = new_objects_list, old_objects_names_list = old_objects_names_list,
                                          old_objects_dates_covered_list = old_objects_dates_covered_list, n_update = 1)

            ##For custom objs, check just if name match
            if (!is.null(custom_signal_weights_m_df)){
              if (!identical(custom_signal_weights_m_df@meta_dataframe_name, old_sb_workflow_last_batch$custom_signal_weights_object_name)){
                stop("custom_signal_weights_m_df name does not match the one in old_results.")
              }
            }
            if (!is.null(custom_signal_universe_metrics_m_df)){
              if (!identical(custom_signal_universe_metrics_m_df@meta_dataframe_name, old_sb_workflow_last_batch$custom_signal_universe_metrics_object_name)){
                stop("custom_signal_universe_metrics_m_df name does not match the one in old_results.")
              }
            }


            #######################

            #Update old sb_backtest_config
            #######################
            ##Retrive objects from last period
            .old_backtest_covered_dates <- old_objects_dates_covered_list[["features_m_df"]] %>% sort()
            #New update must happen at last date of last backtest - target_fwd, in order to re-use
            #now populated target_returns_m_df at the last date - target_fwd.
            #Suppose last date was 2023-05-15. If target_fwd = 3, this means that the last populated oos_sb_outputs was 2023-02-15.
            #We are now in 2023-06-15, so we can now populate 2023-03-15, so we will take backtest n_dates + 1 - 3
            #(2023-05-15 + 1 = 2023-06-15 - 3 = 2023-03-15). This should be equal to train + val (cte)
            #The only exception is if new date happens before first date of oos_sb_outputs with NA
            .old_oos_sb_outputs_m_df <- old_results@oos_sb_outputs_m_df@data %>%
              dplyr::filter(dates >= .old_backtest_covered_dates[length(.old_backtest_covered_dates)] -
                              months(old_sb_workflow_last_batch$target_fwd) + months(1))

            ##Get first date with NA
            .old_oos_sb_outputs_m_df_first_NA_date <- .old_oos_sb_outputs_m_df %>%
              dplyr::filter(is.na(target)) %>%
              dplyr::slice_min(dates, n = 1) %>%
              dplyr::pull(dates) %>%
              unique()

            ##Get old model
            .old_sb_model_fit <- old_results@final_sb_model

            ##Adjust new_config
            new_config <- old_results@sb_backtest_config
            ###new_training_date
            new_config@training_sample_size <-
              which(.old_backtest_covered_dates == .old_oos_sb_outputs_m_df_first_NA_date) - #First date with NA
              old_sb_workflow_last_batch$validation_sample_size

            ##Check if new training + validation is equal to length(features_m_df@data$dates)
            if (!(new_config@training_sample_size + old_sb_workflow_last_batch$validation_sample_size) %in%
                c(length(unique(features_m_df@data$dates)) - old_sb_workflow_last_batch$target_fwd,
                  length(unique(features_m_df@data$dates)) - 1)){
              stop("The new training_sample_size + validation_sample_size is not equal to amount of unique dates in features_m_df - target_fwd
                     (general case) or amount of unique dates in features_m_df - 1 (when the former happens before first fitting)")
            }

            ##Get same gsm and winsorization probs
            gsm_algorithm <- old_sb_workflow_last_batch$gsm_algorithm
            winsorization_probs <- sort(c(old_sb_workflow_last_batch$lower_quantile_winsorization, old_sb_workflow_last_batch$upper_quantile_winsorization))


            #######################

            #Re-run!!
            #######################
            updated_sb_backtest_results <- run_sb_backtest(
              ###SB Backtest
              features_m_df = features_m_df, target_m_df = target_m_df, config = new_config,
              ###SS Backtest Results
              ss_backtest_results = updated_ss_backtest_results,
              ###Covariance
              backtest_returns_m_xts = updated_backtest_returns_m_xts,
              benchmark_returns_m_xts = benchmark_returns_m_xts, signal_themes_m_df = signal_themes_m_df,
              ###Custom weights and signal universe metrics
              custom_signal_weights_m_df = custom_signal_weights_m_df, custom_signal_universe_metrics_m_df = custom_signal_universe_metrics_m_df,
              ###Other
              winsorization_probs = winsorization_probs, gsm_algorithm = gsm_algorithm, verbose = verbose, parallel = parallel, .test_seed = .test_seed,
              ###Update
              .update = TRUE, .old_backtest_covered_dates = .old_backtest_covered_dates, .old_oos_sb_outputs_m_df = .old_oos_sb_outputs_m_df,
              .old_sb_model_fit = .old_sb_model_fit
            )

            #######################

            #Consolidate results
            #######################

            ##results objects
            new_sb_backtest_outputs_list <- list(
              oos_sb_outputs_m_df = updated_sb_backtest_results@oos_sb_outputs_m_df,
              oos_testing_eval_metrics_m_xts = updated_sb_backtest_results@oos_testing_eval_metrics_m_xts,
              best_hyperparameters_m_xts = updated_sb_backtest_results@best_hyperparameters_m_xts,
              validation_eval_metrics_hyper_choice_m_xts = updated_sb_backtest_results@validation_eval_metrics_hyper_choice_m_xts,
              feature_importance_m_df = updated_sb_backtest_results@feature_importance_m_df
            )

            ##Consolidate
            ###m_df and m_xts
            ###Erase .old_oos_sb_outputs_m_df_first_NA_date from oos_sb_outputs_m_df
            old_results@oos_sb_outputs_m_df@data <- old_results@oos_sb_outputs_m_df@data %>%
              dplyr::filter(!dates >= .old_oos_sb_outputs_m_df_first_NA_date)

            ###Consolidate m_df
            updated_results_list <- consolidate_backtest_results(new_backtest_outputs_list = new_sb_backtest_outputs_list,
                                                                 old_backtest_results = old_results)

            ###other objs
            ####consolidated_eval_metrics_row
            consolidated_eval_metrics_row <-
              calculate_eval_metrics(pred = updated_results_list[["oos_sb_outputs_m_df"]]@data %>% tidyr::drop_na() %>% dplyr::pull(pred),
                                     target = updated_results_list[["oos_sb_outputs_m_df"]]@data %>% tidyr::drop_na() %>% dplyr::pull(target),
                                     huber_delta = new_config@huber_delta, quantile_tau = new_config@quantile_tau,
                                     chosen_eval_metric = old_sb_workflow_last_batch$chosen_eval_metric #chosen_eval_metric that goes into FUN is after translation
              )[-1] #-1 to eliminate Score
            ####Transform in df
            updated_consolidated_eval_metrics <- data.frame(metric = names(consolidated_eval_metrics_row), cons_oos = as.numeric(consolidated_eval_metrics_row),
                                                            row.names = NULL)

            if (!new_config@sb_algorithm %in% c("ols", "sw", "ew", "rp", "mvo", "custom_weights")){
              ####Get validation row
              avg_validation_eval_metrics_hyper_choice_df <-
                data.frame(metric = colnames(updated_results_list[["validation_eval_metrics_hyper_choice_m_xts"]]@data),
                           avg_val = colMeans(updated_results_list[["validation_eval_metrics_hyper_choice_m_xts"]]@data),
                           row.names = NULL)

              ####Join with updated_consolidated_eval_metrics
              updated_consolidated_eval_metrics <- dplyr::left_join(updated_consolidated_eval_metrics, avg_validation_eval_metrics_hyper_choice_df, by = "metric")

              ####chosen_eval_metric_validation
              updated_chosen_eval_metric_validation <- c(old_results@chosen_eval_metric_validation,
                                                         updated_sb_backtest_results@chosen_eval_metric_validation)
            }

            ##Reassign the updated objects back into 'updated_sb_backtest_results'
            updated_sb_backtest_results@oos_sb_outputs_m_df <- updated_results_list[["oos_sb_outputs_m_df"]]
            updated_sb_backtest_results@oos_testing_eval_metrics_m_xts <- updated_results_list[["oos_testing_eval_metrics_m_xts"]]
            updated_sb_backtest_results@best_hyperparameters_m_xts <- updated_results_list[["best_hyperparameters_m_xts"]]
            updated_sb_backtest_results@validation_eval_metrics_hyper_choice_m_xts <- updated_results_list[["validation_eval_metrics_hyper_choice_m_xts"]]
            updated_sb_backtest_results@feature_importance_m_df <- updated_results_list[["feature_importance_m_df"]]
            updated_sb_backtest_results@consolidated_eval_metrics <- updated_consolidated_eval_metrics
            if (!new_config@sb_algorithm %in% c("ols", "sw", "ew", "rp", "mvo", "custom_weights")){
              updated_sb_backtest_results@chosen_eval_metric_validation <- updated_chosen_eval_metric_validation
            }


            ###In case of an empty update
            if (is.null(updated_sb_backtest_results@final_sb_model)){
              updated_sb_backtest_results@final_sb_model <- old_results@final_sb_model
              updated_sb_backtest_results@final_gsm <- old_results@final_gsm
              updated_sb_backtest_results@feature_importance_m_df <- old_results@feature_importance_m_df
              updated_sb_backtest_results@final_feature_importance_m_d_ref <- old_results@final_feature_importance_m_d_ref
              updated_sb_backtest_results@chosen_eval_metric_validation <- old_results@chosen_eval_metric_validation
              updated_sb_backtest_results@best_hyperparameters_m_xts <- old_results@best_hyperparameters_m_xts
              updated_sb_backtest_results@validation_eval_metrics_hyper_choice_m_xts <- old_results@validation_eval_metrics_hyper_choice_m_xts
            }

            ##Consolidate sb backtest workflow
            updated_sb_backtest_results@sb_backtest_workflow <- c(old_results@sb_backtest_workflow, updated_sb_backtest_results@sb_backtest_workflow)
            names(updated_sb_backtest_results@sb_backtest_workflow)[length(names(updated_sb_backtest_results@sb_backtest_workflow))] <-
              paste0("update_", features_m_df@current_date)

            #######################

            return(updated_sb_backtest_results)


          })


#' @describeIn update_sb_backtest Updates a sb backtest using based on a \code{sb_metabacktest_results} object.
#'
#' This method extracts the parameters from the \code{results} object (of class \code{sb_metabacktest_results}), modifies training_sample_size, performs the
#' new backtest and then binds results
#'
#' @include class_definitions.R
#' @export
setMethod("update_sb_backtest",
          signature(features_m_df = "meta_dataframe", target_m_df = "meta_dataframe", old_results = "sb_metabacktest_results"),

          function(features_m_df, target_m_df, old_results, #sb backtests
                   updated_base_sb_backtest_results, #Base learners updated
                   updated_base_port_backtest_cohort = NULL, updated_base_backtest_returns_m_xts = NULL, base_benchmark_returns_m_xts = NULL, base_signal_themes_m_df = NULL, base_priors_m_df = NULL, #For Base SS Backtest Results
                   base_custom_signal_weights_m_df = NULL, base_custom_signal_universe_metrics_m_df = NULL, #Custom weights and signal universe metrics for base learners
                   updated_meta_port_backtest_cohort = NULL, updated_meta_backtest_returns_m_xts = NULL, meta_benchmark_returns_m_xts = NULL, meta_signal_themes_m_df = NULL, meta_priors_m_df = NULL, #For Meta SS Backtest Results
                   meta_custom_signal_weights_m_df = NULL, meta_custom_signal_universe_metrics_m_df = NULL, #Custom weights for meta learner
                   verbose = TRUE, parallel = TRUE, .test_seed = NULL) {

            #Get old_sb_backtest_workflow
            old_meta_sb_workflow_last_batch <- old_results@meta_sb_backtest_results@sb_backtest_workflow[[length(old_results@meta_sb_backtest_results@sb_backtest_workflow)]]

            #Check adherence between new objects and old object (names and dates)
            #######################
            ##Check if current_date is equal to old_results current_date + 1 months
            if(features_m_df@current_date != ##It will be checked that current_date matches across new objects in run_port_backtest
               lubridate::add_with_rollback(old_meta_sb_workflow_last_batch$current_date, months(1))){
              stop("The current_date in the new features_m_df is not equal to the current_date in the old_results + 1 month")
            }

            ##Updated base sb backtest results
            old_base_sb_backtest_results <- old_results@base_sb_backtest_results_list
            ###Check backtest identifier
            if (!identical(sapply(updated_base_sb_backtest_results, function(x) x@backtest_identifier),
                           unname(sapply(old_base_sb_backtest_results, function(x) x@backtest_identifier)))){
              stop("One or more backtest identifiers in updated_base_sb_backtest_results do not match the ones in old_base_sb_backtest_results")
            }
            ###Check if current date of updated_base_sb_backtest_results is equal to the current date of old_base_sb_backtest_results + 1
            updated_obj_current_date <- as.Date(sapply(updated_base_sb_backtest_results, function(x) x@sb_backtest_workflow[[length(x@sb_backtest_workflow)]]$current_date))
            old_obj_current_date <- as.Date(sapply(old_base_sb_backtest_results, function(x) x@sb_backtest_workflow[[length(x@sb_backtest_workflow)]]$current_date))
              ####Check if the old dates + 1 match current dates
              if (!identical(updated_obj_current_date,
                             lubridate::add_with_rollback(old_obj_current_date, months(1)))){
                stop("current_date in updated_base_sb_backtest_results is not equal to the current_date in the old_base_sb_backtest_results + 1 month")
              }

            #######################

            #Update old sb_backtest_config
            #######################

              ###Get same gsm algo and winsorization
              gsm_algorithm <- old_meta_sb_workflow_last_batch$gsm_algorithm
              winsorization_probs <- sort(c(old_meta_sb_workflow_last_batch$lower_quantile_winsorization, old_meta_sb_workflow_last_batch$upper_quantile_winsorization))

            updated_sb_metabacktest_results <- run_sb_backtest(
              ###SB Metabacktest
              features_m_df = features_m_df, target_m_df = target_m_df, config = old_results@sb_metabacktest_config,
              base_sb_backtest_results_list = updated_base_sb_backtest_results,
              base_port_backtest_cohort = updated_base_port_backtest_cohort, base_backtest_returns_m_xts = updated_base_backtest_returns_m_xts, base_benchmark_returns_m_xts = base_benchmark_returns_m_xts,
              base_signal_themes_m_df = base_signal_themes_m_df, #For RP MVO
              base_custom_signal_weights_m_df = base_custom_signal_weights_m_df, base_custom_signal_universe_metrics_m_df = base_custom_signal_universe_metrics_m_df, #Custom weights and signal universe metrics for base learners
              meta_port_backtest_cohort = updated_meta_port_backtest_cohort, meta_backtest_returns_m_xts = updated_meta_backtest_returns_m_xts, meta_benchmark_returns_m_xts = meta_benchmark_returns_m_xts,
              meta_signal_themes_m_df = meta_signal_themes_m_df, #For RP MVO
              meta_custom_signal_weights_m_df = meta_custom_signal_weights_m_df, meta_custom_signal_universe_metrics_m_df = meta_custom_signal_universe_metrics_m_df, #Custom weights for meta learner
              winsorization_probs = winsorization_probs, gsm_algorithm = gsm_algorithm, verbose = verbose, parallel = parallel, .test_seed = .test_seed,
              .update = TRUE, .old_meta_sb_backtest_results = old_results@meta_sb_backtest_results
              )

            ##Update sb_metabacktest_config
            updated_sb_metabacktest_results@sb_metabacktest_config@meta_sb_backtest_config <-
              updated_sb_metabacktest_results@meta_sb_backtest_results@sb_backtest_config

            #######################


            #Return
            #######################
            return(updated_sb_metabacktest_results)
            #######################

          })


#' Run Signal Blending Backtest
#'
#' Executes a signal blending backtest, supporting both base learners and meta learners in a walk-forward setting. It is capable of handling advanced ML configurations, hyperparameter tuning strategies, custom optimization objectives, and interpretability tools (e.g., global surrogate models). Designed to run flexibly with either `sb_backtest_config` or `sb_metabacktest_config` objects.
#'
#' @details
#' The function has two main methods:
#'
#' ## 1. **Base Learner Backtest (`sb_backtest_config`)**
#'
#' Executes a time-series cross-validation procedure, with refitting at specified rebalancing months. The data is divided into:
#'
#' - **Training window** (fixed size, expanding or rolling)
#' - **Optional validation window** (for tuning)
#' - **Testing window** (evaluated sequentially for each rebalancing date)
#'
#' Key steps:
#' - Signal selection based on `is_eligible` flags from `signal_universe_m_df`.
#' - Correction of signal orientation (e.g., multiply by -1 if `low_` prefixed).
#' - Optionally override signal selection via custom weights.
#' - Hyperparameter tuning via:
#'   - `grid_search`
#'   - `random_search` (with distribution sampling)
#'   - `bayesian_opt` (with `ParBayesianOptimization`)
#' - Refit and predict using ML algorithm (OLS, glmnet, xgboost, rf, nn, etc.).
#' - Global Surrogate Model (`gsm_algorithm`) fitted post-hoc for interpretability.
#' - Walk-forward out-of-sample testing and metric computation.
#'
#' ## 2. **Meta Learner Backtest (`sb_metabacktest_config`)**
#'
#' Iterates over base learners (each with a `sb_backtest_config`), consolidates their predictions into a unified meta feature set, and then:
#' - Winsorizes and/or normalizes predictions (optional)
#' - Adds user-selected pass-through features
#' - Fits a meta learner using a new `sb_backtest_config`
#'
#' The meta learner backtest can be updated via `.update = TRUE` to extend its horizon without re-running all base learners.
#'
#' @section Parallel Execution:
#' By default, tuning_method %in% c("random_search", "grid_search") utilizes furrr::future_pmap, which means they can run according to the built-in backends
#' from the future package. Therefore, if the user does not specify a different evaluation strategy with future::plan(),
#' tuning will be done sequentially by default (equivalent to future::plan(sequential)). In this case, however,
#' random number generator will be set to RNGkind("L'Ecuyer-CMRG"), instead of R default (RNGkind("Mersenne-Twister")), making results
#' not reproducible regarding using purrr:pmap(). In order to run using R's default random number generator, set parallel = FALSE.
#' Using a different evaluation strategy (e.g., future::plan(multisession)) will tune hyperparameters asynchronously (in parallel).
#'
#' For tuning_method = "bayesian_opt", the ParBayesianOptimization::bayesOpt function runs in parallel by using foreach::foreach with the %dopar% operator.
#' Therefore, in this case, the user can either: (i) use doFuture::registerDoFuture(), in order to use the %dofuture% foreach adapter
#' (actually, in this case, doFuture::withDoRNG is used to turn %dopar% into %dorng% in order to use parallel-safe RNG), which allows
#' usage of backends from the future package or (ii) use parallel::makeCluster(), doParallel::registerDoParallel(), doParallel::clusterExport() and
#' doParallel::clusterEvalQ(), as exemplified by ParBayesianOptimization. If parallel = TRUE and neither strategy is being used,
#' code will result in error. Therefore, to run bayesian_opt synchronously, either use doFuture::registerDoFuture() with plan(sequential)
#' or set parallel = FALSE.
#'
#' Keras has some limitations when working in parallel, especially when using bayesian optimization as tuning method.
#'
#' @section Update Workflow:
#' - When `.update = TRUE`, previously computed predictions are reused for existing dates.
#' - Models are only refitted for new rebalancing dates not in `.old_backtest_covered_dates`.
#' - The final object will append results while keeping the full history intact.
#'
#' @param features_m_df A `meta_dataframe` with input features. Must include `id`, `tickers`, and `dates`. Should include signal normalization in its workflow.
#' @param target_m_df A `meta_dataframe` with the target variable. Columns should follow format `targetname_1_m`, etc.
#' @param config Either a `sb_backtest_config` (single backtest) or a `sb_metabacktest_config` (meta learning).
#' @param base_sb_backtest_results_list A list of `sb_backtest_results` objects (only for `sb_metabacktest_config`).
#' @param verbose Logical. Print progress and diagnostic messages.
#' @param parallel Logical. Run tuning and backtest in parallel. See Details.
#' @param winsorization_probs Numeric vector (length 2). Used to winsorize signal or prediction input.
#' @param .test_seed Internal. Used for test reproducibility.
#' @param .update Internal. Set to `TRUE` to update a previously-run backtest object.
#' @param .old_meta_sb_backtest_results Internal. A previously returned `sb_metabacktest_results` object to be updated.
#'
#' @return An S4 object of class:
#' \describe{
#'   \item{\strong{sb_backtest_results}}{For a base learner, containing:}
#'   \itemize{
#'     \item \code{oos_sb_outputs_m_df}: Data frame of predictions, targets, and errors.
#'     \item \code{oos_testing_eval_metrics_m_xts}: Time series of out-of-sample performance metrics.
#'     \item \code{feature_importance_m_df}: Feature importances from surrogate models.
#'     \item \code{final_sb_model}: Final fitted model for last rebalance date.
#'     \item \code{final_gsm}: Final global surrogate model (OLS or tree).
#'     \item \code{validation_eval_metrics_hyper_choice_m_xts}: If tuned, metrics on validation sample.
#'     \item \code{best_hyperparameters_m_xts}: Selected hyperparameters for each rebalancing.
#'     \item \code{consolidated_eval_metrics}: Summary table of performance.
#'     \item \code{sb_backtest_workflow}: Metadata about splits, config, algorithms, and dates.
#'   }
#'
#'   \item{\strong{sb_metabacktest_results}}{For a meta learner, additionally containing:}
#'   \itemize{
#'     \item \code{meta_sb_backtest_results}: A `sb_backtest_results` object fitted to meta features.
#'     \item \code{base_sb_backtest_results_list}: The original list of base learner backtests.
#'     \item \code{oos_predictions_m_df}: The meta features constructed from base learners' predictions.
#'     \item \code{sb_metabacktest_config}: Original configuration object.
#'   }
#' }
#'
#' @seealso
#' \code{\link{sb_backtest_config}}, \code{\link{sb_metabacktest_config}}, \code{\link{time_series_split}}, \code{\link{run_port_backtest}},
#' \code{\link{create_meta_dataframe}}, \code{\link{ParBayesianOptimization::bayesOpt}}, \code{\link{furrr::future_pmap}}
#'
#' @export

setGeneric("run_sb_backtest", function(features_m_df, target_m_df, config, base_sb_backtest_results_list, ...) standardGeneric("run_sb_backtest"))

#' @describeIn run_sb_backtest Runs a signal blending backtest for a single configuration.
#'
#' This method handles a single model configuration defined by an `sb_backtest_config` object.
#' It supports walk-forward validation, optionally with hyperparameter tuning using grid search, random search, or Bayesian optimization.
#' It can also fallback to heuristic models like EW, RP, or MVO.
#'
#' @param features_m_df A `meta_dataframe` containing the features (input variables) used in the backtest. Must include columns: `id`, `tickers`, `dates`, and the features to be used as model inputs.
#' @param target_m_df A `meta_dataframe` containing the target variable(s) to be predicted. Columns should be named using the format `XXXX_number_m`, where `XXXX` is the target name, `number` is the forward prediction horizon, and `m` indicates the period unit (e.g., 1-month forward return).
#' @param config An `sb_backtest_config` object that specifies the entire structure of the backtest, including model algorithm, training/validation/test splitting logic, hyperparameter tuning strategy, sample sizes, and objective functions.
#' @param ss_backtest_results (Optional) An `ss_backtest_results` object that stores the output of a signal selection backtest. If provided, this is used to select eligible signals for blending based on statistical or Bayesian filtering.
#' @param port_backtest_cohort (Optional) A `port_backtest_cohort` object used to extract backtest and benchmark returns in case `backtest_returns_m_xts` or `benchmark_returns_m_xts` are not explicitly provided. Should be used when the user wants the signal blending backtest to be tied to an existing portfolio backtest setup.
#' @param backtest_returns_m_xts (Optional) A `meta_xts` object with historical returns of signals to be blended. Used for covariance estimation in algorithms like `rp` and `mvo`, as well as for constructing heuristic portfolios.
#' @param benchmark_returns_m_xts (Optional) A `meta_xts` object with historical benchmark returns. Required when calculating active returns, constructing heuristic portfolios, or using benchmark-relative constraints.
#' @param signal_themes_m_df (Optional) A `meta_dataframe` mapping signals to groups (e.g., themes, sectors). Required for applying group constraints in signal-based portfolio optimization (e.g., in `mvo` with `max_abs_active_group_weight`).
#' @param custom_signal_weights_m_df (Optional) A `meta_dataframe` containing user-defined signal weights to be used in place of model-generated weights. Required when `sb_algorithm = "custom_weights"`. Weights must be positive and consistent with eligible signals.
#' @param custom_signal_universe_metrics_m_df (Optional) A `meta_dataframe` with additional metrics (e.g., signal volatilities, costs, or liquidity) associated with each signal. These are passed through and can be used for custom diagnostics or constraints.
#' @param winsorization_probs A numeric vector of length 2, specifying the lower and upper quantiles used to winsorize signals (default = `c(0.025, 0.975)`). Helps reduce the influence of extreme outliers in the signal distribution before model fitting.
#' @param gsm_algorithm Character. Specifies the type of Global Surrogate Model used to interpret the fitted model. Options include `"ols"` (default) for linear interpretability or `"tree"` for tree-based importance decomposition. This surrogate is fitted post-hoc on model predictions to extract signal importances.
#' @param verbose Logical. If `TRUE` (default), prints diagnostic messages, sample sizes, fitting progress, model configuration details, and errors encountered throughout the backtest.
#' @param parallel Logical. If `TRUE` (default), enables parallel computation where possible (e.g., hyperparameter tuning, nested configurations, etc.). Uses the `future` ecosystem for `grid_search` and `random_search`, or `foreach` for `bayesian_opt`.
#' @param .test_seed (Internal) Integer or `NULL`. If provided, sets the random seed for model training and hyperparameter search, ensuring reproducibility during unit tests or controlled simulations.
#' @param .update (Internal) Logical. Indicates whether the backtest is being updated with new dates or data. If `TRUE`, skips recomputation of prior results and extends the object with additional periods.
#' @param .old_backtest_covered_dates (Internal) A vector of `Date` objects indicating the periods already covered by a previous backtest. Used in conjunction with `.update = TRUE` to determine which new periods to run.
#' @param .old_oos_sb_outputs_m_df (Internal) A data frame with out-of-sample model predictions from a previous run. Used during update to retrieve past predictions without refitting the model.
#' @param .old_sb_model_fit (Internal) A previously fitted SB model object (e.g., trained glmnet, xgboost, or RP/MVO optimizer). Used when updating the backtest without retraining the model for the same configuration.
#'
#' @return An object of class `sb_backtest_results`.
#'
#' @export
setMethod("run_sb_backtest",
          signature(features_m_df = "meta_dataframe", target_m_df = "meta_dataframe", config = "sb_backtest_config",
                    base_sb_backtest_results_list = "missing"),

          function(features_m_df, target_m_df, config, #SB Backtest
                   ss_backtest_results = NULL, #SS Backtest Results
                   port_backtest_cohort = NULL, backtest_returns_m_xts = NULL, benchmark_returns_m_xts = NULL, signal_themes_m_df = NULL, #Cov Estimation
                   custom_signal_weights_m_df = NULL, custom_signal_universe_metrics_m_df = NULL, #Custom objects
                   winsorization_probs = c(0.025, 0.975), gsm_algorithm = "ols", verbose = TRUE, parallel = TRUE, .test_seed = NULL,
                   .update = FALSE, .old_backtest_covered_dates = NULL, .old_oos_sb_outputs_m_df = NULL, .old_sb_model_fit = NULL
          ) {

            #Assign default values for internal function

            ###########################
            ##Training and splits
            split_method <- "expanding"
            validation_sample_size <- 0

            ##Training algo (loss and eval function)
            custom_objective <- "squared_error"
            chosen_eval_metric <- NULL
            huber_delta <- 1
            quantile_tau <- 0.5
            keras_architecture_parameters <- NULL

            ##Hyper tuning
            hyper_grid_domain_list <- NULL
            tuning_method <- NULL
            n_iter <- NULL
            k_iter <- NULL
            acq <- "ucb"
            init_points <- NULL
            early_stop <- NULL

            ##Heuristic SB
            cov_matrix_sample_size <- 36
            cov_estimation_method <- "sample"
            cov_matrix_benchmark <- NULL
            active_returns <- TRUE
            rp_method <- "cyclical-spinu"
            n_random_ports <- 2000
            random_ports_method <- "sample"
            opt_objective <- "sharpe"
            opt_method <- "random"
            concentration_constraint_policy <- NULL


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

            #Get or Fabricate Signal Universe and market_factor_proxy
            ###########################

            ##Derive signal_universe_m_df
            derive_signal_universe_m_df_results_list <- derive_signal_universe_m_df(
              config = config,
              ss_backtest_results = ss_backtest_results,
              features_m_df = features_m_df,
              backtest_returns_m_xts = backtest_returns_m_xts, benchmark_returns_m_xts = benchmark_returns_m_xts, #Backtest and Benchmark
              priors_m_df = priors_m_df, #Priors
              custom_signal_universe_metrics_m_df = custom_signal_universe_metrics_m_df, #Custom Signal Universe Metrics
              signal_themes_m_df = signal_themes_m_df, #Signal Themes
              verbose = verbose, parallel = parallel, winsorization_probs = winsorization_probs #Misc
            )
            #Extract signal_universe_m_df
            signal_universe_m_df <- derive_signal_universe_m_df_results_list$signal_universe_m_df


            ###########################

            #Get data from S4 objects
            ###########################
            ##features_m_df
            features_workflow <- features_m_df@workflow #Get workflow
            ###Check for normalization
            if (!"normalization" %in% unlist(features_workflow)){
              warning ("Normalization not found in workflow. It is advisable that data is normalized before being fed to run_sb_backtest.")
            }
            features_object_name <- features_m_df@meta_dataframe_name #Get mdf name
            features_current_date <- features_m_df@current_date #Get current date
            features_m_df <- features_m_df@data #Get features_m_df

            ##target_m_df
            target_workflow <- target_m_df@workflow #Get workflow
            target_object_name <- target_m_df@meta_dataframe_name #Get mdf name
            target_current_date <- target_m_df@current_date #Get current date
            target_m_df <- target_m_df@data #Get target_m_df

            ##Get general Information from config
            ###Training and splits
            training_sample_size <- config@training_sample_size #Get training sample size
            rebalancing_months <- config@rebalancing_months #Get rebalancing months
            split_method <- config@split_method #Split method
            target_fwd_name <- config@target_fwd_name #Get target_fwd_name

            ###Training algo (loss and eval function)
            sb_algorithm <- config@sb_algorithm #Get sb_algorithm
            custom_objective <- config@custom_objective #Get custom_objective
            keras_architecture_parameters <- if(sb_algorithm == "nn") as.list(config@keras_architecture_parameters) #Get keras_architecture_parameters
            huber_delta <- config@huber_delta #Get huber_delta
            quantile_tau <- config@quantile_tau #Get quantile_tau

            ###Signal Selection
            chosen_signals_and_positions <- derive_signal_universe_m_df_results_list$chosen_signals_and_positions #Get chosen_signals_and_positions from SB or SS

            ###Tuning Strategy
            if(!sb_algorithm %in% c("ols", "ew", "sw", "rp", "mvo", "custom_weights")){
              tuning_strategy <- config@tuning_strategy #Get tuning strategy
              ###Get objects inside tuning strategy
              tuning_method <- tuning_strategy@tuning_method #Get tuning method
              validation_sample_size <- tuning_strategy@validation_sample_size #Get validation sample size
              chosen_eval_metric <- tuning_strategy@chosen_eval_metric #Get chosen eval metric
              hyper_grid_domain_list <- tuning_strategy@hyper_grid_domain@hyperparameter_list #Get hyper_grid_domain_list
              early_stop <- tuning_strategy@early_stop #Get early_stop

              ###Random search
              if(tuning_method == "random_search"){
                n_iter <- tuning_strategy@n_iter #Get n_iter
              }

              ###Bayesian Opt
              if(tuning_method == "bayesian_opt"){
                n_iter <- tuning_strategy@n_iter #Get n_iter
                acq <- tuning_strategy@acq #Get acq
                k_iter <- tuning_strategy@k_iter #Get k_iter
                init_points <- tuning_strategy@init_points #Get init_points
              }
            }

            ###Signal Port Parameters
            if(sb_algorithm %in% c("rp", "mvo")){
              signal_port_parameters <- config@signal_port_parameters
              ####Covariance
              cov_est_method <- signal_port_parameters@cov_est_method
              cov_estimation_method <- cov_est_method@cov_estimation_method
              cov_matrix_sample_size <- cov_est_method@cov_matrix_sample_size
              active_returns <- cov_est_method@active_returns
              cov_matrix_benchmark <- cov_est_method@cov_matrix_benchmark

              ###RP
              if(sb_algorithm == "rp"){
                rp_parameters <- signal_port_parameters@rp_parameters
                rp_method <- rp_parameters@rp_method
              }

              ###MVO
              if(sb_algorithm == "mvo"){
                mvo_parameters <- signal_port_parameters@mvo_parameters
                random_ports_method <- mvo_parameters@random_ports_method
                n_random_ports <- mvo_parameters@n_random_ports
                opt_method <- mvo_parameters@opt_method
                opt_objective <- mvo_parameters@opt_objective

                if (!is.null(signal_port_parameters@concentration_constraint_policy)){
                  concentration_constraint_policy <-  as.list(signal_port_parameters@concentration_constraint_policy)
                } else {
                  concentration_constraint_policy <- NULL
                }
              }
            }

            ##Get signal themes, backtest returns and so on
            ##Signal themes
            if(!is.null(signal_themes_m_df)){
              signal_themes_object_name <- signal_themes_m_df@meta_dataframe_name #Signal Themes Obj Name
              signal_themes_workflow <- signal_themes_m_df@workflow #Workflow
              signal_themes_current_date <- signal_themes_m_df@current_date #Current Date
              signal_themes_m_df <- signal_themes_m_df@data
            } else {
              if(sb_algorithm == "mvo" & !is.null(concentration_constraint_policy$max_abs_active_group_weight)){
                stop("A signal_themes_m_df must be provided when setting group constraints")
              }
            }
            ##Backtest and benchmark (for heuristic portfolios)
            if(sb_algorithm %in% c("rp", "mvo")){

              ###Check if either backtest_returns_m_xts or port_backtest_cohort is provided
              if(is.null(backtest_returns_m_xts) && is.null(port_backtest_cohort)){
                stop("A backtest_returns_m_xts or a port_backtest_cohort must be provided when using sb_algorithm = 'rp' or 'mvo'")
              }

              ###Check if both backtest_returns_m_xts and port_backtest_cohort are both provided
              if (!is.null(backtest_returns_m_xts) && !is.null(port_backtest_cohort)) {
                stop("Only one of backtest_returns_m_xts or port_backtest_cohort should be provided.")
              }

              ##Extract backtest_returns_m_xts from cohort
              ###If backtest_returns_m_xts is not provided, extract it from port_backtest_cohort
              if (is.null(backtest_returns_m_xts) && !is.null(port_backtest_cohort)) {
                ###Run extraction
                extracted_returns_m_xts <- extract_returns_m_xts(
                  port_backtest_cohort = port_backtest_cohort, #Port Backtest Cohort
                  signals_m_df = suppressMessages(create_meta_dataframe(features_m_df, meta_dataframe_name = features_object_name)),
                  benchmark_returns_m_xts = benchmark_returns_m_xts, #Objects to check consistency
                  verbose = verbose
                )
                ###Assign extracted values
                backtest_returns_m_xts <- extracted_returns_m_xts$backtest_returns_m_xts
                benchmark_returns_m_xts <- extracted_returns_m_xts$benchmark_returns_m_xts

              }

              ###Get objects
              backtest_returns_object_name <- backtest_returns_m_xts@meta_xts_name
              backtest_returns_workflow <- backtest_returns_m_xts@workflow
              backtest_returns_current_date <- backtest_returns_m_xts@current_date
              backtest_returns_m_xts <- backtest_returns_m_xts@data

              if(is.null(benchmark_returns_m_xts)){
                stop("A benchmark_returns_m_xts must be provided when using sb_algorithm = 'rp' or 'mvo'")
              }
              ###Get objects
              benchmark_returns_object_name <- benchmark_returns_m_xts@meta_xts_name
              benchmark_returns_workflow <- benchmark_returns_m_xts@workflow
              benchmark_returns_current_date <- benchmark_returns_m_xts@current_date
                benchmark_returns_m_xts <- benchmark_returns_m_xts@data

                #Check for enable_theme_representativeness when grup constraint is enabled
                if (sb_algorithm == "mvo" & !is.null(concentration_constraint_policy$max_abs_active_group_weight)){
                    if (!is.null(ss_backtest_results)){
                      if (!ss_backtest_results@ss_backtest_workflow[[length(ss_backtest_results@ss_backtest_workflow)]]$enable_theme_representativeness){
                        warning("enable_theme_representativeness should be enabled in alpha_test_strategy when group constraints are enabled,",
                                "to ensure that all themes have representatives when defining group constraints.")
                    }
                  }
                }
              } else {
                if(!is.null(backtest_returns_m_xts)){
                  message("backtest_returns_m_xts assigned as NULL, as it is not used in sb_algorithm choice")
                  backtest_returns_m_xts <- NULL
                }
                if(!is.null(benchmark_returns_m_xts)){
                  message("benchmark_returns_m_xts assigned as NULL, as it is not used in sb_algorithm choice")
                  benchmark_returns_m_xts <- NULL
                }
                if(!is.null(signal_themes_m_df)){
                  message("signal_themes_m_df assigned as NULL, as it is not used in sb_algorithm choice")
                  signal_themes_m_df <- NULL
                }

              }
              ##Custom signal weights
              if(sb_algorithm == "custom_weights"){
                #Check if a custom_signal_weights is provided
                if(is.null(custom_signal_weights_m_df)){
                  stop("A custom_signal_weights_m_df must be provided when using sb_algorithm = 'custom_weights'")
                }
                #Get objects
                custom_signal_weights_object_name <- custom_signal_weights_m_df@meta_dataframe_name
                custom_signal_weights_workflow <- custom_signal_weights_m_df@workflow
                custom_signal_weights_m_df <- custom_signal_weights_m_df@data
              }

             ##Custom signal universe metrics
             if (!is.null(custom_signal_universe_metrics_m_df)){
               custom_signal_universe_metrics_object_name <- custom_signal_universe_metrics_m_df@meta_dataframe_name
               custom_signal_universe_metrics_workflow <- custom_signal_universe_metrics_m_df@workflow
             }

            ###########################

            #Run SB Backtest
            ###########################
              ##Check if all objects current_date match
              check_consistent_dates(list(
                if (!is.null(features_m_df)) features_current_date else NULL,
                if (!is.null(target_m_df)) target_current_date else NULL,
                if (!is.null(backtest_returns_m_xts)) backtest_returns_current_date else NULL,
                if (!is.null(benchmark_returns_m_xts)) benchmark_returns_current_date else NULL,
                if (!is.null(benchmark_returns_m_xts)) benchmark_returns_current_date else NULL,
                if (!is.null(signal_themes_m_df)) signal_themes_current_date else NULL
              ))

              ##Run SB Backtest
              sb_backtest_results <- run_sb_backtest_internal(
                #Basic Obj Inputs
                features_m_df = features_m_df, target_m_df = target_m_df, training_sample_size = training_sample_size, target_fwd_name = target_fwd_name,
                #Splits
                validation_sample_size = validation_sample_size, rebalancing_months = rebalancing_months, split_method = split_method,
                #Heuristic SBs
                signal_universe_m_df = signal_universe_m_df,
                cov_matrix_sample_size = cov_matrix_sample_size, cov_estimation_method = cov_estimation_method, active_returns = active_returns, #Covariance Matrix
                backtest_returns_m_xts = backtest_returns_m_xts, benchmark_returns_m_xts = benchmark_returns_m_xts, cov_matrix_benchmark = cov_matrix_benchmark, #Covariance Matrix
                rp_method = rp_method, n_random_ports = n_random_ports, random_ports_method = random_ports_method, opt_objective = opt_objective,  #RP/MVO
                concentration_constraint_policy = concentration_constraint_policy, signal_themes_m_df = signal_themes_m_df, #MVO (Group constraints) and returns_sample_clean
                custom_signal_weights_m_df = custom_signal_weights_m_df, #Custom Weights
                #Choice of SB algorithm
                sb_algorithm = sb_algorithm, gsm_algorithm = gsm_algorithm,
                #Loss/Eval Functions and Related
                custom_objective = custom_objective, chosen_eval_metric = chosen_eval_metric, huber_delta = huber_delta, quantile_tau = quantile_tau,
                #Hyperparameter tuning
                hyper_grid_domain_list = hyper_grid_domain_list, tuning_method = tuning_method, n_iter = n_iter,
                k_iter = k_iter, acq = acq, init_points = init_points, early_stop = early_stop,
                #Keras architecture parameters
                keras_architecture_parameters = keras_architecture_parameters,
                #Misc
                verbose = verbose, parallel = parallel, lower_quantile_winsorization = lower_quantile_winsorization, upper_quantile_winsorization = upper_quantile_winsorization,
                .test_seed = .test_seed,
                #Update
                .update = .update, .old_backtest_covered_dates = .old_backtest_covered_dates, .old_oos_sb_outputs_m_df = .old_oos_sb_outputs_m_df,
                .old_sb_model_fit = .old_sb_model_fit
              )
            ###########################

            #Adjust SB Backtest Results
            ###########################

            ##Config
            sb_backtest_results@sb_backtest_config <- config

            ##IDs
            sb_backtest_results@sb_backtest_workflow$config_name <- config@config_name
            sb_backtest_results@sb_backtest_workflow$backtest_identifier <-
              paste0("c:",config@config_name, "_f:", features_object_name, "_t:", target_object_name,"-",target_fwd_name)
            sb_backtest_results@backtest_identifier <- sb_backtest_results@sb_backtest_workflow$backtest_identifier
            sb_backtest_results@sb_backtest_workflow$current_date <- features_current_date #already tested if all match

            #Add chosen_signals_and_positions
            sb_backtest_results@sb_backtest_workflow$chosen_signals_and_positions <- chosen_signals_and_positions

            #Add ss identifier
            if (!is.null(ss_backtest_results)){
              sb_backtest_results@sb_backtest_workflow$ss_backtest_identifier <- ss_backtest_results@backtest_identifier
            }

            #Add workflows, config_name and objects for target and features
              ##Target
              sb_backtest_results@sb_backtest_workflow$target_object_name <- target_object_name
              sb_backtest_results@sb_backtest_workflow$target_workflow <- target_workflow

              ##Features
              sb_backtest_results@sb_backtest_workflow$features_object_name <- features_object_name
              sb_backtest_results@sb_backtest_workflow$features_workflow <- features_workflow

              ##Covariance objects
              if (!is.null(signal_themes_m_df)){
              sb_backtest_results@sb_backtest_workflow$signal_themes_object_name <- signal_themes_object_name
              sb_backtest_results@sb_backtest_workflow$signal_themes_workflow <- signal_themes_workflow  #Get workflow
             }


            #Workflow and names for feature_importance_m_df, oos_sb_outputs_m_df and final_sb_model
              ##Workflow/Source/Names
              ###Meta Dataframes
              if (!is.null(sb_backtest_results@feature_importance_m_df) && nrow(sb_backtest_results@feature_importance_m_df@data) > 0){ #If objects are not missing
              sb_backtest_results@feature_importance_m_df@workflow <- list(paste0("feature_importance_m_df result of ", sb_backtest_results@backtest_identifier))
              sb_backtest_results@final_feature_importance_m_d_ref@workflow <- list(paste0("final_feature_importance_m_d_ref result of ", sb_backtest_results@backtest_identifier))
              sb_backtest_results@feature_importance_m_df@meta_dataframe_name <- paste0("sb_backtest__:",sb_backtest_results@sb_backtest_workflow$backtest_identifier)
              sb_backtest_results@final_feature_importance_m_d_ref@meta_dataframe_name <- paste0("sb_backtest__:",sb_backtest_results@sb_backtest_workflow$backtest_identifier)
              if(sb_algorithm %in% c("ew", "sw", "rp", "mvo", "custom_weights")) sb_backtest_results@final_sb_model@model@port_name <- paste0("sb_backtest__:",sb_backtest_results@sb_backtest_workflow$backtest_identifier)
              }
              sb_backtest_results@oos_sb_outputs_m_df@meta_dataframe_name <- paste0("sb_backtest__:",sb_backtest_results@sb_backtest_workflow$backtest_identifier)
              if(sb_algorithm == "custom_weights"){
                sb_backtest_results@sb_backtest_workflow$custom_signal_weights_object_name <- custom_signal_weights_object_name
                sb_backtest_results@sb_backtest_workflow$custom_signal_weights_workflow <- custom_signal_weights_workflow
              }
              if (!is.null(custom_signal_universe_metrics_m_df)){
                sb_backtest_results@sb_backtest_workflow$custom_signal_universe_metrics_object_name <- custom_signal_universe_metrics_object_name
                sb_backtest_results@sb_backtest_workflow$custom_signal_universe_metrics_workflow <- custom_signal_universe_metrics_workflow
              }

              ###Meta xts
              if (!is.null(sb_backtest_results@oos_testing_eval_metrics_m_xts)){
                sb_backtest_results@oos_testing_eval_metrics_m_xts@source <- rep(paste0("sb_backtest__:",sb_backtest_results@sb_backtest_workflow$backtest_identifier), ncol(sb_backtest_results@oos_testing_eval_metrics_m_xts@data))
                sb_backtest_results@oos_testing_eval_metrics_m_xts@meta_xts_name <- paste0("sb_backtest__:",sb_backtest_results@sb_backtest_workflow$backtest_identifier)
              }

              if (!sb_algorithm %in% c("ols", "ew", "sw", "rp", "mvo", "custom_weights")){
                if (!is.null(sb_backtest_results@best_hyperparameters_m_xts) && nrow(sb_backtest_results@best_hyperparameters_m_xts@data) > 0 &&
                    !is.null(sb_backtest_results@validation_eval_metrics_hyper_choice_m_xts) && nrow(sb_backtest_results@validation_eval_metrics_hyper_choice_m_xts@data) > 0){
                sb_backtest_results@best_hyperparameters_m_xts@source <- rep(paste0("sb_backtest__:",sb_backtest_results@sb_backtest_workflow$backtest_identifier), ncol(sb_backtest_results@best_hyperparameters_m_xts@data))
                sb_backtest_results@best_hyperparameters_m_xts@meta_xts_name <- paste0("sb_backtest__:",sb_backtest_results@sb_backtest_workflow$backtest_identifier)
                sb_backtest_results@validation_eval_metrics_hyper_choice_m_xts@source <- rep(paste0("sb_backtest__:",sb_backtest_results@sb_backtest_workflow$backtest_identifier), ncol(sb_backtest_results@validation_eval_metrics_hyper_choice_m_xts@data))
                sb_backtest_results@validation_eval_metrics_hyper_choice_m_xts@meta_xts_name <- paste0("sb_backtest__:",sb_backtest_results@sb_backtest_workflow$backtest_identifier)
                }
              }

              if (sb_algorithm %in% c("rp", "mvo")){
                sb_backtest_results@sb_backtest_workflow$backtest_returns_object_name <- backtest_returns_object_name
                sb_backtest_results@sb_backtest_workflow$backtest_returns_workflow <- backtest_returns_workflow
                sb_backtest_results@sb_backtest_workflow$benchmark_returns_object_name <- benchmark_returns_object_name
                sb_backtest_results@sb_backtest_workflow$benchmark_returns_workflow <- benchmark_returns_workflow
              }

              ###Call
              sb_backtest_results@sb_backtest_workflow$call <- sys.call(-2)

              ###Add date to workflow
              sb_backtest_results@sb_backtest_workflow <- list(sb_backtest_results@sb_backtest_workflow)
              names(sb_backtest_results@sb_backtest_workflow) <- features_current_date

              return(sb_backtest_results)

          }
)



#' @describeIn run_sb_backtest Runs a signal blending meta-backtest using multiple base learners and a meta learner.
#'
#' This method iteratively evaluates several base learners defined in an `sb_metabacktest_config`, then fits a meta learner on top of their predictions.
#' It allows winsorization, normalization, and feature passthrough control when aggregating base learner outputs.
#'
#' @param features_m_df A `meta_dataframe` containing features used for the meta learner. These may include passthrough features and/or out-of-sample predictions from base learners.
#' @param target_m_df A `meta_dataframe` containing the target variable to be predicted by the meta learner. It must be aligned with `features_m_df`.
#' @param config An object of class `sb_metabacktest_config`, which contains the configuration for all base learners and the meta learner.
#' @param base_sb_backtest_results_list A named list of `sb_backtest_results` objects. These are the results of base learners whose predictions will be used as input for the meta learner.
#' @param base_port_backtest_cohort (Optional) A `port_backtest_cohort` object containing results of portfolio backtests associated with the base learners. Used to extract return series when needed.
#' @param base_backtest_returns_m_xts (Optional) A `meta_xts` object with historical returns for the base learner signal portfolios. Used in RP/MVO for covariance estimation.
#' @param base_benchmark_returns_m_xts (Optional) A `meta_xts` object with benchmark returns used alongside base learner portfolios.
#' @param base_signal_themes_m_df (Optional) A `meta_dataframe` mapping base learner signals to groups or themes, needed for group constraints in RP/MVO.
#' @param base_custom_signal_weights_m_df (Optional) A `meta_dataframe` specifying weights for base learner signals. Only used when `sb_algorithm = "custom_weights"` for base learners.
#' @param base_custom_signal_universe_metrics_m_df (Optional) A `meta_dataframe` of evaluation metrics for base learner signals. Can be used as additional features in the meta learner.
#' @param meta_port_backtest_cohort (Optional) A `port_backtest_cohort` object for the meta learner signal portfolio. Used to extract return series when needed.
#' @param meta_backtest_returns_m_xts (Optional) A `meta_xts` object with historical returns for the meta learner’s constructed signals or predictions. Used in RP/MVO.
#' @param meta_benchmark_returns_m_xts (Optional) A `meta_xts` object with benchmark returns for the meta learner.
#' @param meta_signal_themes_m_df (Optional) A `meta_dataframe` with signal group classification for the meta learner. Required if using group constraints.
#' @param meta_custom_signal_weights_m_df (Optional) A `meta_dataframe` with weights for the meta learner signals. Used when `sb_algorithm = "custom_weights"`.
#' @param meta_custom_signal_universe_metrics_m_df (Optional) A `meta_dataframe` of evaluation metrics to be used as custom signal-level features by the meta learner.
#' @param winsorization_probs A numeric vector of length 2 specifying the lower and upper quantiles for winsorizing predictions before training the meta learner. Default is `c(0.025, 0.975)`.
#' @param gsm_algorithm Character string indicating the global surrogate model used for interpretability. Options include `"ols"` and `"tree"`. Default is `"ols"`.
#' @param verbose Logical. If `TRUE`, prints progress messages. Default is `TRUE`.
#' @param parallel Logical. If `TRUE`, runs the backtest and hyperparameter tuning steps in parallel. Default is `TRUE`.
#' @param .test_seed (Internal) A numeric seed used to control randomness for reproducible testing. Default is `NULL`.
#' @param .update (Internal) Logical flag. If `TRUE`, updates a previously computed meta learner backtest instead of running from scratch. Default is `FALSE`.
#' @param .old_meta_sb_backtest_results (Internal) A previously computed `sb_backtest_results` object for the meta learner, used only if `.update = TRUE`.
#'
#' @return An object of class `sb_metabacktest_results`, containing:
#' \itemize{
#'   \item \strong{meta_sb_backtest_results}: The results from the meta learner.
#'   \item \strong{base_sb_backtest_results_list}: List of base learner results.
#'   \item \strong{oos_predictions_m_df}: Out-of-sample predictions used to train the meta learner.
#'   \item \strong{sb_metabacktest_config}: The input configuration object.
#' }
#'
#'
#' @return An object of class `sb_metabacktest_results`.
#'
#' @export
setMethod("run_sb_backtest",
          signature(features_m_df = "meta_dataframe", target_m_df = "meta_dataframe", config = "sb_metabacktest_config",
                    base_sb_backtest_results_list = "list"),

          function(features_m_df, target_m_df, config,
                   base_sb_backtest_results_list,
                   base_port_backtest_cohort = NULL, base_backtest_returns_m_xts = NULL, base_benchmark_returns_m_xts = NULL, base_signal_themes_m_df = NULL, #For RP MVO
                   base_custom_signal_weights_m_df = NULL, base_custom_signal_universe_metrics_m_df = NULL, #Custom weights and signal universe metrics for base learners
                   meta_port_backtest_cohort = NULL, meta_backtest_returns_m_xts = NULL, meta_benchmark_returns_m_xts = NULL, meta_signal_themes_m_df = NULL, #For RP MVO
                   meta_custom_signal_weights_m_df = NULL, meta_custom_signal_universe_metrics_m_df = NULL, #Custom weights for meta learner
                   winsorization_probs = c(0.025, 0.975), gsm_algorithm = "ols", verbose = TRUE, parallel = TRUE, .test_seed = NULL,
                   .update = FALSE, .old_meta_sb_backtest_results = NULL
                   ) {

            ## Initial preparation
            #######################

              ###Check installed packages
              if (verbose){
              if (!requireNamespace("crayon", quietly = TRUE) || !requireNamespace("tictoc", quietly = TRUE)) {
                stop("Packages 'crayon' and 'tictoc' are required to generate logs. Please install them using install.packages() or set verbose as FALSE")
                }
              }

              ###Extract base backtest_returns_m_xts
                ####Check if both backtest_returns_m_xts and base_port_backtest_cohort are provided
                if (!is.null(base_backtest_returns_m_xts) && !is.null(base_port_backtest_cohort)) {
                  stop("Only one of base_backtest_returns_m_xts or base_port_backtest_cohort should be provided.")
                }

                ####If backtest_returns_m_xts is not provided, extract it from base_port_backtest_cohort
                if (is.null(base_backtest_returns_m_xts) && !is.null(base_port_backtest_cohort)) {
                  ###Run extraction
                  extracted_returns_m_xts <- extract_returns_m_xts(
                    port_backtest_cohort = base_port_backtest_cohort, #Port Backtest Cohort
                    signals_m_df = features_m_df, benchmark_returns_m_xts = base_benchmark_returns_m_xts, #Objects to check consistency
                    verbose = verbose
                  )

                  ###Assign extracted returns
                  base_backtest_returns_m_xts <- extracted_returns_m_xts$backtest_returns_m_xts
                  base_benchmark_returns_m_xts <- extracted_returns_m_xts$benchmark_returns_m_xts

                }

              ###Extract meta backtest_returns_m_xts
                ####Check if both backtest_returns_m_xts and meta_port_backtest_cohort are provided
                if (!is.null(meta_backtest_returns_m_xts) && !is.null(meta_port_backtest_cohort)) {
                  stop("Only one of meta_backtest_returns_m_xts or meta_port_backtest_cohort should be provided.")
                }

                ####If backtest_returns_m_xts is not provided, extract it from meta_port_backtest_cohort
                if (is.null(meta_backtest_returns_m_xts) && !is.null(meta_port_backtest_cohort)) {
                  ###Run extraction
                  extracted_returns_m_xts <- extract_returns_m_xts(
                    port_backtest_cohort = meta_port_backtest_cohort, #Port Backtest Cohort
                    signals_m_df = features_m_df, benchmark_returns_m_xts = meta_benchmark_returns_m_xts, #Objects to check consistency
                    verbose = verbose
                  )

                  ###Assign extracted returns
                  meta_backtest_returns_m_xts <- extracted_returns_m_xts$backtest_returns_m_xts
                  meta_benchmark_returns_m_xts <- extracted_returns_m_xts$benchmark_returns_m_xts

                }

              ###Initial checks
              check_inputs_meta_sb_backtest(
                config = config, features_m_df = features_m_df, target_m_df = target_m_df,
                base_sb_backtest_results_list = base_sb_backtest_results_list,
                #Base Objects
                base_backtest_returns_m_xts = base_backtest_returns_m_xts, base_benchmark_returns_m_xts = base_benchmark_returns_m_xts, base_signal_themes_m_df = base_signal_themes_m_df,
                base_custom_signal_weights_m_df = base_custom_signal_weights_m_df, base_custom_signal_universe_metrics_m_df = base_custom_signal_universe_metrics_m_df,
                #Meta Objects
                meta_backtest_returns_m_xts = meta_backtest_returns_m_xts, meta_benchmark_returns_m_xts = meta_benchmark_returns_m_xts, meta_signal_themes_m_df = meta_signal_themes_m_df,
                meta_custom_signal_weights_m_df = meta_custom_signal_weights_m_df, meta_custom_signal_universe_metrics_m_df = meta_custom_signal_universe_metrics_m_df,
                verbose
              )

            #######################

            ## Initial Preparations
            #######################
              ####Ensure base_sb_backtest_results_list is correctly named with backtest ids
              names(base_sb_backtest_results_list) <- sapply(base_sb_backtest_results_list, function(x) x@backtest_identifier)

            #######################

            #Generate oos_predictions_m_df and adapt objects
            #######################


            ##Get features_passthrough_and_positions based on chosen_signals_and_positions_list
              ###Get features_passthrough_and_positions from chosen_signals_and_positions
              features_passthrough_and_positions <- get_features_positions(
                base_sb_backtest_results_list = base_sb_backtest_results_list, #Base SB Backtest Results List
                features_passthrough = config@features_passthrough, #Features to pass through
                features_m_df = features_m_df #Features meta_dataframe
              )

            ###Create oos_predictions_m_df and join with features_m_df according to features_passthrough_and_positions
              ####Create object
              oos_predictions_m_df <- consolidate_oos_sb_outputs_m_df(
                base_sb_backtest_results_list,
                winsorize_predictions = config@winsorize_base_predictions, winsorization_probs = winsorization_probs, # Winsorization
                normalize_predictions = config@normalize_base_predictions, # Normalization
                features_passthrough_and_positions = features_passthrough_and_positions, # Pass-through features
                features_m_df = features_m_df, #Features to be passed
                parallel = parallel, verbose = verbose #Parallel and verbose
              )

              ####Change meta_dataframe name
              oos_predictions_m_df@meta_dataframe_name <- paste0("m_config:", config@config_name, "_", "f_mdf:", features_m_df@meta_dataframe_name)

            ###Adapted chosen_signals_and_positions
              ####Recreate chosen_signals_and_positions based on backtest names (always long) and features_passthrough_and_positions
              backtest_ids <- unname(sapply(base_sb_backtest_results_list, function(x) x@backtest_identifier))
              if (length(features_passthrough_and_positions) == 1 && features_passthrough_and_positions == "none"){
                adapted_chosen_signals_and_positions <- c(rep("long", length(backtest_ids))) #If features_pass is 'none', then pass only backtests
                names(adapted_chosen_signals_and_positions) <- backtest_ids
              } else {
                adapted_chosen_signals_and_positions <- c(rep("long", length(backtest_ids)), features_passthrough_and_positions) #Get all positions
                names(adapted_chosen_signals_and_positions) <- c(backtest_ids, names(features_passthrough_and_positions))
              }

              ####Modify config by inserting adapted_chosen_signals_and_positions
              config@meta_sb_backtest_config@chosen_signals_and_positions <- adapted_chosen_signals_and_positions

            #####Print
            if (verbose){
              cat("Final features and positions for meta backtest:\n")
              print(adapted_chosen_signals_and_positions)
              cat("\n")
            }

            ##Adapt target_m_df
            adapted_target_m_df <- target_m_df
            adapted_target_m_df@data <- target_m_df@data %>% dplyr::filter(id %in% dplyr::pull(oos_predictions_m_df@data, id))
            adapted_target_m_df@meta_dataframe_name <- paste0(adapted_target_m_df@meta_dataframe_name, "_adj")

            ##Join backtest_returns_m_xts
              ###This will bring NULL if meta_backtest_returns_m_xts is NULL or combine the two, returning meta_backtest_returns_m_xts if base_backtest_returns_m_xts is NULL
              consolidated_backtest_returns_m_xts <- consolidate_generic_meta_xts(main_generic_m_xts = meta_backtest_returns_m_xts,
                                                                                  supplemental_generic_m_xts = base_backtest_returns_m_xts,
                                                                                  type = "returns", consolidate_name = TRUE, require_main = TRUE,
                                                                                  operation = "merge"
                                                                                  )

            ##Join benchmark_returns_m_xts
              ###This will bring the not-NULL object or the merged object if both are not NULL. The idea is to serve the inner function with selected_market_factor_proxy or benchmark
              consolidated_benchmark_returns_m_xts <- consolidate_generic_meta_xts(main_generic_m_xts = meta_benchmark_returns_m_xts,
                                                                                  supplemental_generic_m_xts = base_benchmark_returns_m_xts,
                                                                                  type = "returns", consolidate_name = TRUE, require_main = FALSE,
                                                                                  operation = "merge"
                                                                                  )
              ###Ensure conformity with adapted_backtest_returns_m_xts
              if (!is.null(consolidated_backtest_returns_m_xts) && !is.null(consolidated_benchmark_returns_m_xts)){
                ####Get consolidated_backtest_returns_m_xts_dates
                consolidated_backtest_returns_m_xts_dates <- zoo::index(consolidated_backtest_returns_m_xts@data)
                ####Subset to same dates
                consolidated_benchmark_returns_m_xts@data <- consolidated_benchmark_returns_m_xts@data[consolidated_backtest_returns_m_xts_dates,]
              }

            ##Join signal_themes_m_df
            consolidated_signal_themes_m_df <- consolidate_generic_meta_dataframes(main_generic_m_df = meta_signal_themes_m_df,
                                                                                   supplemental_generic_m_df = base_signal_themes_m_df,
                                                                                   type = "groups", consolidate_name = TRUE, require_main = TRUE
                                                                                   )
            ##Join meta_custom_signal_universe_metrics_m_df
            consolidated_custom_signal_weights_m_df <- consolidate_generic_meta_dataframes(main_generic_m_df = meta_custom_signal_weights_m_df,
                                                                                           supplemental_generic_m_df = base_custom_signal_weights_m_df,
                                                                                           type = "weights", consolidate_name = TRUE, require_main = TRUE
                                                                                           )

            ##Derive meta_custom_signal_universe_metrics_m_df as consolidated eval metrics or join
            consolidated_custom_signal_universe_metrics_m_df <- derive_adapted_custom_signal_universe_m_df(
              meta_custom_objective = config@meta_sb_backtest_config@custom_objective,
              base_sb_backtest_results_list = base_sb_backtest_results_list, #Base SB Backtest Results List
              meta_custom_signal_universe_metrics_m_df = meta_custom_signal_universe_metrics_m_df, base_custom_signal_universe_metrics_m_df = base_custom_signal_universe_metrics_m_df
            )

            #######################

            #Run sb_backtest with predictions_m_df
            #######################

            #Fit Meta Model
            meta_learner_backtest_results <- tryCatch({

              ###Print
              if (verbose) {
                if (.update){
                  cat(crayon::cyan("Updating Meta SB backtesting\n"))
                  tictoc::tic(msg = crayon::green("Meta SB backtest updated\n"))
                } else {
                  cat(crayon::cyan("Starting Meta SB backtesting\n"))
                  tictoc::tic(msg = crayon::green("Meta SB backtest finished\n"))
                }
              }
              ###Run or Update SB Backtest
              if (!.update){
              ##Run SB Backtest
              run_sb_backtest(
                config = config@meta_sb_backtest_config, # Meta SB Configuration
                features_m_df = oos_predictions_m_df, # Features are oos predictions for base models
                target_m_df = adapted_target_m_df, # Target is the original target
                backtest_returns_m_xts = consolidated_backtest_returns_m_xts, benchmark_returns_m_xts = consolidated_benchmark_returns_m_xts,
                signal_themes_m_df = consolidated_signal_themes_m_df, #RP/MVO
                custom_signal_weights_m_df = consolidated_custom_signal_weights_m_df, custom_signal_universe_metrics_m_df = consolidated_custom_signal_universe_metrics_m_df, #Custom Weights and Signal Universe Metrics
                winsorization_probs = winsorization_probs, gsm_algorithm = gsm_algorithm, verbose = verbose, parallel = parallel, .test_seed = .test_seed)
              } else {
                ##Update SB Backtest
                update_sb_backtest(
                  old_results = .old_meta_sb_backtest_results, # Meta SB Configuration
                  features_m_df = oos_predictions_m_df, # Features are oos predictions for base models
                  target_m_df = adapted_target_m_df, # Target is the original target
                  updated_backtest_returns_m_xts = consolidated_backtest_returns_m_xts, benchmark_returns_m_xts = consolidated_benchmark_returns_m_xts,
                  signal_themes_m_df = consolidated_signal_themes_m_df, #RP/MVO
                  custom_signal_weights_m_df = consolidated_custom_signal_weights_m_df, custom_signal_universe_metrics_m_df = consolidated_custom_signal_universe_metrics_m_df, #Custom Weights and Signal Universe Metrics
                  verbose = verbose, parallel = parallel, .test_seed = .test_seed)
              }
            }, error = function(e) {
              stop("An error occurred while running the meta SB backtest. Please check the configurations and input data. Details: ", e$message)
            })

            #Change SB Metadata
            ### Features Passthrough And Positions
            meta_learner_backtest_results@sb_backtest_workflow[[length(meta_learner_backtest_results@sb_backtest_workflow)]]$features_passthrough_and_positions <-
              features_passthrough_and_positions
            meta_learner_backtest_results@sb_backtest_workflow[[length(meta_learner_backtest_results@sb_backtest_workflow)]]$winsorize_base_predictions <-
              config@winsorize_base_predictions
            meta_learner_backtest_results@sb_backtest_workflow[[length(meta_learner_backtest_results@sb_backtest_workflow)]]$normalize_base_predictions[[length(meta_learner_backtest_results@sb_backtest_workflow)]] <-
              config@normalize_base_predictions

            ### Type
            meta_learner_backtest_results@sb_backtest_workflow[[length(meta_learner_backtest_results@sb_backtest_workflow)]]$backtest_type <- "meta_learner"

            ### Call
            meta_learner_backtest_results@sb_backtest_workflow[[length(meta_learner_backtest_results@sb_backtest_workflow)]]$call <- sys.call(-2)

            ### Add Meta Config name
            meta_learner_backtest_results@sb_backtest_workflow[[length(meta_learner_backtest_results@sb_backtest_workflow)]]$config_name_meta <-
              config@config_name

            ### Add Base Learners Info
            meta_learner_backtest_results@sb_backtest_workflow[[length(meta_learner_backtest_results@sb_backtest_workflow)]]$config_name_bl <-
              sapply(base_sb_backtest_results_list, function(x) x@sb_backtest_workflow[[length(x@sb_backtest_workflow)]]$config_name)

            meta_learner_backtest_results@sb_backtest_workflow[[length(meta_learner_backtest_results@sb_backtest_workflow)]]$sb_algorithm_bl <-
              sapply(base_sb_backtest_results_list, function(x) x@sb_backtest_workflow[[length(x@sb_backtest_workflow)]]$sb_algorithm)

            meta_learner_backtest_results@sb_backtest_workflow[[length(meta_learner_backtest_results@sb_backtest_workflow)]]$dates_covered_bl <-
              as.data.frame(lapply(base_sb_backtest_results_list, function(x) unique(as.Date(x@sb_backtest_workflow[[length(x@sb_backtest_workflow)]]$dates_covered))))

            meta_learner_backtest_results@sb_backtest_workflow[[length(meta_learner_backtest_results@sb_backtest_workflow)]]$n_dates_bl <-
              length(unique(sapply(base_sb_backtest_results_list, function(x) x@sb_backtest_workflow[[length(x@sb_backtest_workflow)]]$dates_covered)))

            meta_learner_backtest_results@sb_backtest_workflow[[length(meta_learner_backtest_results@sb_backtest_workflow)]]$training_sample_size_bl <-
              unique(sapply(base_sb_backtest_results_list, function(x) x@sb_backtest_workflow[[length(x@sb_backtest_workflow)]]$training_sample_size))

            meta_learner_backtest_results@sb_backtest_workflow[[length(meta_learner_backtest_results@sb_backtest_workflow)]]$validation_sample_size_bl <-
              unique(sapply(base_sb_backtest_results_list, function(x) x@sb_backtest_workflow[[length(x@sb_backtest_workflow)]]$validation_sample_size))

            meta_learner_backtest_results@sb_backtest_workflow[[length(meta_learner_backtest_results@sb_backtest_workflow)]]$testing_sample_size_bl <-
              unique(sapply(base_sb_backtest_results_list, function(x) x@sb_backtest_workflow[[length(x@sb_backtest_workflow)]]$testing_sample_size))

            meta_learner_backtest_results@sb_backtest_workflow[[length(meta_learner_backtest_results@sb_backtest_workflow)]]$dates_testing_sample_bl <-
              as.data.frame(lapply(base_sb_backtest_results_list, function(x) unique(as.Date(x@sb_backtest_workflow[[length(x@sb_backtest_workflow)]]$dates_testing_sample))))

            meta_learner_backtest_results@sb_backtest_workflow[[length(meta_learner_backtest_results@sb_backtest_workflow)]]$rebalance_dates_bl <-
            as.data.frame(lapply(base_sb_backtest_results_list, function(x) unique(as.Date(x@sb_backtest_workflow[[length(x@sb_backtest_workflow)]]$rebalance_dates))))


            # Displays how much time it took
            if (verbose) {
              tictoc::toc()
            }

            ######################
            sb_metabacktest_results <- create_sb_metabacktest_results(
              meta_sb_backtest_results = meta_learner_backtest_results,
              base_sb_backtest_results_list = base_sb_backtest_results_list,
              oos_predictions_m_df = oos_predictions_m_df,
              sb_metabacktest_config = config
            )

            return(sb_metabacktest_results)
          }
)



#' @describeIn run_sb_backtest Run ML Backtest
#' Perform out-of-sample testing for ML Algorithms with walk-forward time series validation
#'
#' This function performs walk-forward validation for time series data using
#' a range of ML models. It supports hyperparameter tuning via random search, grid search,
#' or Bayesian optimization. The function divides the data into training, validation,
#' and testing samples, and iteratively refits the model at specified rebalancing dates.
#'
#' @param features_m_df A meta dataframe or data frame containing features, with columns: id, tickers, dates.
#' @param target_m_df A meta dataframe or data frame containing target variable(s), with corresponding dates. Columns should follow the format XXXX_number_m, where
#' XXXX is the name of the target variable, number is the amount of forward periods and m indicates periods are measured in months.
#' @param training_sample_size Number of observations to include in each training sample.
#' @param validation_sample_size Number of observations to include in each validation sample. If provided a decimal, it will be considered as a percentage of the training sample size.
#' @param rebalancing_months Months (numeric) when model should be rebalanced.
#' @param target_fwd_name Name of the target variable in `target_m_df`.
#' @param sb_algorithm Choice of sb_algorithm: ols (Ordinary Least Squares), glmnet (Elastic Net), rf (Random Forest), xgb (eXtreme Gradient Boosting), and nn (Keras Neural Networks).
#' @param split_method Choice of split method (expanding or rolling).
#' @param hyper_grid_domain A named list containing hyperparameter definitions. The structure of this list depends on the specified tuning method:
#' \itemize{
#'   \item \strong{For grid search:} Must be a list of named vectors:
#'   \item \strong{For random search:} Must be a list of named lists, where each named list contains:
#'     \itemize{
#'       \item \code{distribution_choice}: A character string specifying the distribution (one of "normal", "uniform", "lognormal", "constant").
#'       \item \code{pars}: A named numeric vector of parameters corresponding to the chosen distribution.
#'       \item \code{value}: A numeric value (only present if \code{distribution_choice} is "constant").
#'     }
#'   \item \strong{For Bayesian optimization:} Must be a list of named numeric vectors, each of length 2, representing the boundaries for the hyperparameters.
#' }
#' @examples
#' # Example of creating hyper_grid_domain_list for grid search
#' hyper_grid <- list(
#'    alpha = c(0.2, 0.5),
#'    lambda.min.ratio = c(0.1, 0.5, 0.9)
#'    )
#'
#' # Example of creating hyper_grid_domain_list for random search
#' hyper_grid <- list(
#'   alpha = list(distribution_choice = "uniform", pars = c(min = 0, max = 1), value = NULL),
#'   lambda.min.ratio = list(distribution_choice = "uniform", pars = c(min = 0, max = 0.9), value = NULL)
#' )
#'
#' # Example of creating hyper_grid_domain_list for bayesian optimization
#' hyper_grid <- list(
#'    alpha = c(0.2, 0.9),
#'    lambda.min.ratio = c(0.1, 0.9)
#'    )
#'
#' @param tuning_method Method for hyperparameter tuning: "random_search", "grid_search", or "bayesian_opt".
#' @param n_iter Number of iterations.
#' For grid_search, the value does not make difference, as the number of times the ml algorithm validation error will be evaluated equals the exhaustive combination of unique hyperparameters values provided.
#' For random_search, it should be the number of random draws for each hyperparameter. Random samples of n_iter size will be generated for each hyperparameter and their unique values will be exhaustively combined.
#' Therefore, for n_iter = 5 and 2 hyperparameters, the ml algorithm validation error should be generally evaluated 5² = 25 times.
#' For bayesian_opt, it should be the number of times the ml algorithm will be evaluated after initialization.
#' @param acq Acquisition function for Bayesian optimization: "ucb", "ei", or "poi".
#' @param init_points Number of initial random points for Bayesian optimization.
#' @param k_iter Integer that specifies the number of times to sample eval_function at each Epoch during Bayesian optimization.
#' If running in parallel, set iters.k to a multiple of the number of cores. Must be lower and preferably a multiple of n_iter.
#' @param custom_objective Custom objective (double differentiable loss function) for xgboost and nn algorithms.
#' (current options are squared_error, absolute_error and (pseudo)-huber loss)
#' @param early_stop Sets a halting criteria to prevent overfitting in xgb and nn.
#' @param chosen_eval_metric Metric to optimize during tuning: "rss", "rmse", "cp", "mae", "mphe", "mpe", "mape", "hr", and "mb".
#' @param keras_architecture_parameters A named list containing parameters for configuring the Keras neural network architecture. It includes:
#' \itemize{
#'   \item \strong{units}: A numeric vector specifying the number of neurons in each layer.
#'   \item \strong{n_layers}: An integer indicating the total number of layers in the neural network.
#'   \item \strong{activation}: A character vector listing the activation functions for each layer (e.g., "relu", "sigmoid", "tanh").
#'   \item \strong{nn_optimizer}: A character string specifying the optimizer used for training the model (options: "Adam" or "RMSProp").
#'   \item \strong{batch_norm_option}: A logical vector indicating whether batch normalization should be applied after each respective layer (TRUE or FALSE).
#' }
#' @examples
#' # Example of creating a 3 layers keras_architecture_parameters for neural networks.
#' keras_architecture_parameters = list(units = c(32,16,8), n_layers = 3, activation = c("relu", "relu", "relu"),
#'                                      nn_optimizer = "Adam", batch_norm_option = c(TRUE,TRUE,TRUE)
#' )
#' @param signal_universe_m_d_ref A data frame containing the signal universe. If provided, data in this object will be updated with posteriors.
#' @param backtest_returns_m_xts A xts containing historical backtested returns named according to signals in `signals_universe_m_df`,
#' @param benchmark_returns_m_xts A xts with benchmark returns, named accordingly.
#' @param gsm_algorithm The type of interpretable model to be used as a global surrogate model, which will help interpretate both signal selection and signal belnding. Can be one of
#' 'ols' or 'tree'.
#' @param huber_delta A single numeric value indicating the boundary that separates where the loss function turns from quadratic to linear.
#' @param quantile_tau A single numeric value indicating target quantile when calculating quantile loss.
#' @param verbose Logical, indicating whether to print progress messages (default is TRUE).
#' @param parallel Logical, indicating whether to run hyperparameter tuning in parallel (default is TRUE).
#'
#' @return An object of class sb_backtest_results with various outputs including model predictions, errors, and validation metrics.
#'
#' @details
#' The function ensures all inputs are correctly formatted and performs checks to
#' validate the integrity of input data and parameters. It employs the glmnet package
#' for elastic net regularization, supporting both Lasso and Ridge regression.
#'
#' @section Running in Parallel:
#' By default, tuning_method %in% c("random_search", "grid_search") utilizes furrr::future_pmap, which means they can run according to the built-in backends
#' from the future package. Therefore, if the user does not specify a different evaluation strategy with future::plan(),
#' tuning will be done sequentially by default (equivalent to future::plan(sequential)). In this case, however,
#' random number generator will be set to RNGkind("L'Ecuyer-CMRG"), instead of R default (RNGkind("Mersenne-Twister")), making results
#' not reproducible regarding using purrr:pmap(). In order to run using R's default random number generator, set parallel = FALSE.
#' Using a different evaluation strategy (e.g., future::plan(multisession)) will tune hyperparameters asynchronously (in parallel).
#'
#' For tuning_method = "bayesian_opt", the ParBayesianOptimization::bayesOpt function runs in parallel by using foreach::foreach with the %dopar% operator.
#' Therefore, in this case, the user can either: (i) use doFuture::registerDoFuture(), in order to use the %dofuture% foreach adapter
#' (actually, in this case, doFuture::withDoRNG is used to turn %dopar% into %dorng% in order to use parallel-safe RNG), which allows
#' usage of backends from the future package or (ii) use parallel::makeCluster(), doParallel::registerDoParallel(), doParallel::clusterExport() and
#' doParallel::clusterEvalQ(), as exemplified by ParBayesianOptimization. If parallel = TRUE and neither strategy is being used,
#' code will result in error. Therefore, to run bayesian_opt synchronously, either use doFuture::registerDoFuture() with plan(sequential)
#' or set parallel = FALSE.
#'
#' Keras has some limitations when working in parallel, especially when using bayesian optimization as tuning method.
#'
#' @seealso
#' \code{\link{glmnet}}, \code{\link{ranger}}, \code{\link{xgboost}}, \code{\link{keras}}, \code{\link{time_series_split}}
run_sb_backtest_internal <- function(
  #Basic Objects Inputs
  features_m_df, target_m_df, training_sample_size, target_fwd_name,
  #Splits
  validation_sample_size = 0, rebalancing_months, split_method = "expanding",
  #Heuristic SB
  signal_universe_m_df,
  cov_matrix_sample_size = 36, cov_estimation_method = "sample", active_returns = TRUE, #COV (for RP and MVO)
  backtest_returns_m_xts = NULL, benchmark_returns_m_xts = NULL, cov_matrix_benchmark = "IBOV", #COV (for RP and MVO)
  rp_method = "cyclical-spinu", n_random_ports = 2000, random_ports_method = "sample", opt_objective = "sharpe", opt_method = "random", #RP/MVO
  concentration_constraint_policy = NULL, signal_themes_m_df = NULL, #Group constraints and returns sample clean
  custom_signal_weights_m_df = NULL, #Custom weights meta dataframe
  #Choice of SB algorithm
  sb_algorithm = "ols", gsm_algorithm = "ols",
  #Loss/Eval Functions and Related
  custom_objective = "squared_error", chosen_eval_metric = NULL, huber_delta = 1, quantile_tau = 0.5,
  #Hyperparameter tuning Inputs
  hyper_grid_domain_list = NULL, tuning_method = NULL, n_iter = NULL, k_iter = NULL, acq = "ucb", init_points = NULL, early_stop = NULL,
  #Keras architecture Parameters
  keras_architecture_parameters = NULL,
  #Misc
  verbose = FALSE, parallel = TRUE,
  #Winsorization
  upper_quantile_winsorization = 0.975, lower_quantile_winsorization = 0.025, .test_seed = NULL,
  #Update
  .update = FALSE, .old_backtest_covered_dates = NULL, .old_oos_sb_outputs_m_df = NULL, .old_sb_model_fit = NULL
){

  #Measure time to run and run gc
  elapsed_time <- system.time({

    ################
    ##Check installed packages
    if (verbose){
      if (!requireNamespace("crayon", quietly = TRUE) || !requireNamespace("tictoc", quietly = TRUE)) {
        stop("Packages 'crayon' and 'tictoc' are required to generate logs. Please install them using install.packages() or set verbose as FALSE")
      }
    }

    ##Check Parameters: This function will test whether inputs match format and current functionalities
    check_inputs_sb_backtest(
      features_m_df = features_m_df, target_m_df = target_m_df, training_sample_size = training_sample_size, target_fwd_name = target_fwd_name,
      validation_sample_size = validation_sample_size, rebalancing_months = rebalancing_months, split_method = split_method, signal_universe_m_df = signal_universe_m_df,
      backtest_returns_m_xts = backtest_returns_m_xts, benchmark_returns_m_xts = benchmark_returns_m_xts, cov_matrix_benchmark = cov_matrix_benchmark,
      cov_matrix_sample_size = cov_matrix_sample_size, cov_estimation_method = cov_estimation_method, active_returns = active_returns, signal_themes_m_df = signal_themes_m_df,
      rp_method = rp_method, n_random_ports = n_random_ports, random_ports_method = random_ports_method, opt_objective = opt_objective, concentration_constraint_policy = concentration_constraint_policy,
      custom_signal_weights_m_df = custom_signal_weights_m_df, sb_algorithm = sb_algorithm, gsm_algorithm = gsm_algorithm, custom_objective = custom_objective,
      chosen_eval_metric = chosen_eval_metric, huber_delta = huber_delta, quantile_tau = quantile_tau, hyper_grid_domain_list = hyper_grid_domain_list, tuning_method = tuning_method, n_iter = n_iter, k_iter = k_iter, acq = acq,
      init_points = init_points, early_stop = early_stop, keras_architecture_parameters = keras_architecture_parameters, verbose = verbose, parallel = parallel, .test_seed = .test_seed
    )

    ################

    #Initial Setup: Making some changes to metrics if needed and displaying initial setup
    ################
    ##Adjust custom obj and chosen eval metric
    if(verbose){
      cat("=============================\n")
      cat(crayon::cyan(paste("Signal-Blending Algo:", sb_algorithm)))
      cat("\n")
    }
    adjusted_metrics <- translate_metrics(sb_algorithm = sb_algorithm, chosen_eval_metric = chosen_eval_metric, custom_objective = custom_objective, early_stop = early_stop, huber_delta = huber_delta, verbose = verbose)
    #No tuning algos
    non_tuning_algos <- c("ols", "sw", "ew", "rp", "mvo", "custom_weights")

    #Pass adjusted metrics
    custom_objective_translated <- adjusted_metrics$custom_objective_translated
    chosen_eval_metric_translated <- adjusted_metrics$chosen_eval_metric_translated
    chosen_eval_metric <- adjusted_metrics$chosen_eval_metric

    #Prints for initial setup
    if(verbose){
      cat("\n")
      if(!sb_algorithm %in% c("ew", "rp", "custom_weights")) cat(paste("Custom objective:", custom_objective, "\n"))
      if(!sb_algorithm %in% non_tuning_algos) cat(paste("Eval Metric:", chosen_eval_metric, "\n"))
      cat(paste("Training sample size:", training_sample_size, "\n"))
      if(!sb_algorithm %in% non_tuning_algos) cat(paste("Validation sample size:", validation_sample_size, "\n"))
      cat(paste("Split method:", split_method, "\n"))
    }

    ##################

    ##Init objects
    ##################
      ###Extract dates
      dates_m_vector <- unique(as.Date(features_m_df %>% dplyr::pull(dates), format = "%Y-%m-%d")) #coerce just to be sure
      dates_m_vector <- dates_m_vector[order(dates_m_vector)] #Re-order ascending just to be sure
      ###Takes column corresponding to specific target
      target_vector <- target_m_df %>% dplyr::pull(target_fwd_name)
      target_fwd <- as.numeric(gsub(".*?([0-9]+).*", "\\1", target_fwd_name))

      ####Print for target
      if(verbose)   cat("Predicting a", target_fwd, "months ahead target:", target_fwd_name, "\n")

      ###Testing Sample Size
      testing_sample_size <- length(dates_m_vector) - training_sample_size - validation_sample_size + 1 #calculate testing sample size

      ###Rebalancing Dates
      dates_testing_sample <- dates_m_vector[(training_sample_size + validation_sample_size):
                                             (training_sample_size + validation_sample_size + testing_sample_size - 1)] #These are dates inside testing sample
      if (!.update){
        ####Get first rebalancing date
        first_rebalance_date <- min(dates_testing_sample)
        ####Get all rebalancing dates
        rebalance_dates <- unique( #Unique is to eliminate repeated dates, in case month of first_rebalance_date is a rebalancing month
          c(first_rebalance_date, dates_testing_sample[which(lubridate::month(dates_testing_sample) %in% rebalancing_months)]) #Dates corresponding to rebalancing_months
        )
        ####Re-order ascending just to be sure
        rebalance_dates <- rebalance_dates[order(rebalance_dates)]
        ###Last rebalance date
        last_rebalance_date <- max(rebalance_dates)
      } else {
        rebalance_dates <- dates_testing_sample[which(lubridate::month(dates_testing_sample) %in% rebalancing_months)]
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

      ###Eligible signals dates
      eligible_signals_dates <- signal_universe_m_df %>% dplyr::filter(is_eligible == 1) %>% dplyr::select(dates) %>% unique() %>% dplyr::pull()

      ###Time expanding validation
      if (!sb_algorithm %in% non_tuning_algos && length(rebalance_dates > 0)){
        #Store hyperparameters choice (model complexity)
        #Store validation chosen eval
        chosen_eval_metric_validation <- list()
        #Store validation eval
        validation_eval_metrics_hyper_choice_m_xts <- xts::xts(data.frame(
          rss = as.vector(rep(NA, n_rebalance_months)), #R2
          cp = as.vector(rep(NA, n_rebalance_months)), #CP
          rmse = as.vector(rep(NA, n_rebalance_months)), #Root Mean Squared Error
          mae = as.vector(rep(NA, n_rebalance_months)), #Mean Absolute Error
          mphe = as.vector(rep(NA, n_rebalance_months)), #Mean Pseudo huber
          mpe = as.vector(rep(NA, n_rebalance_months)), #Mean Pinball Error
          mape = as.vector(rep(NA, n_rebalance_months)), #Mean Absolute Percentage Error
          hr = as.vector(rep(NA, n_rebalance_months)), #Hit Rate
          mb = as.vector(rep(NA, n_rebalance_months)) #Mean Bias
        ), order.by = rebalance_dates)

        ###Store hyper_choice_m_xts based on existence of early stop and best_lam
        hyper_choice_m_xts <- xts::xts(as.data.frame(
          matrix(NA, nrow = n_rebalance_months, ncol = length(hyper_grid_domain_list))),
          order.by = rebalance_dates)
        colnames(hyper_choice_m_xts) <- names(hyper_grid_domain_list) #Set colnames as hyperparameters

        ###Add best-lam and best-iteration
        hyper_choice_m_xts$best_lam <- if(sb_algorithm == "glmnet") NA
        hyper_choice_m_xts$best_iteration <- if(!is.null(early_stop)) NA

      }

      ###Time expanding test
      ###Store test eval
      oos_testing_eval_metrics_m_xts<- xts::xts(data.frame(
        rss = as.vector(rep(NA, testing_sample_size)), #+1 bco first date is also a testing date
        cp = as.vector(rep(NA, testing_sample_size)),
        rmse = as.vector(rep(NA, testing_sample_size)),
        mae = as.vector(rep(NA, testing_sample_size)),
        mphe = as.vector(rep(NA, testing_sample_size)),
        mpe = as.vector(rep(NA, testing_sample_size)),
        mape = as.vector(rep(NA, testing_sample_size)),
        hr = as.vector(rep(NA, testing_sample_size)),
        mb = as.vector(rep(NA, testing_sample_size))
      ), order.by = dates_testing_sample
      )

      ###Prediction, error and Y objects
      oos_prediction_list <- list() #initialize prediction list. Each element will be a vector of predictions for that date
      oos_error_list <- list() #Initialize error list.
      oos_y_list <- list() #Initialize y list.

      ###Feature importance
      feature_importance_m_d_ref_list <- list()


      ##################

    ##Start Fitting
    ##################
    ##Loop through
    for(d in (training_sample_size + validation_sample_size):(training_sample_size + validation_sample_size + testing_sample_size - 1)){

      #Get current date
      current_date <- dates_m_vector[d]
      if (verbose) print(current_date)

      ##Rebalance if it's a rebalancing month
      ##############################
      #Define if it is a rebalancing month based on .update
      if (.update){
        ###For an update, don't refit the model at (training_sample_size + validation_sample_size), get last model
        if (current_date %in% .old_backtest_covered_dates){
          is_rebalancing_month <- FALSE #Don't rebuild
          is_update_pickup <- TRUE #Pickup last model
        } else {
          is_rebalancing_month <- (lubridate::month(current_date) %in% rebalancing_months) #Rebalance at rebal months given it is not first d
          is_update_pickup <- FALSE #Don't pickup last model
        }
      } else {
        is_rebalancing_month <- (lubridate::month(current_date) %in% rebalancing_months) || d == (training_sample_size + validation_sample_size)
        is_update_pickup <- FALSE
      }

      if (is_rebalancing_month){
        ###Print refitting message
        if(verbose){
          cat("\n")
          cat(crayon::yellow(paste("Starting model rebalancing at:", current_date)))
          cat("\n")
        }

        ###Select and correct signals
        ##################

        ####Get most recent signal universe (current_eligible_signals)
        most_recent_eligible_signals_date <- eligible_signals_dates[which(eligible_signals_dates <= current_date)] %>% max()
        most_recent_signal_universe_m_d_ref <- signal_universe_m_df %>% dplyr::filter(dates == most_recent_eligible_signals_date)
        current_eligible_signals <- most_recent_signal_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers)

        ####Reconstruct elected_signals_and_positions
        elected_signals_and_positions <- ifelse(stringr::str_detect(current_eligible_signals, pattern = "low_"), "short", "long")
        names(elected_signals_and_positions) <- stringr::str_remove_all(current_eligible_signals, pattern = "low_")

        ####Get most recent custom signal weights if appropriate (not NULL) and overwrite elected_signals_and_positions
        if (!is.null(custom_signal_weights_m_df)){
          ####Get most recent custom signal weights
          most_recent_custom_signal_weights_m_d_ref <- custom_signal_weights_m_df %>% dplyr::filter(dates == most_recent_eligible_signals_date)
          ####Get non-zero weights (allowing for non-eligible signals enables theme_ss predictions)
          current_non_zero_weights_signals <- most_recent_custom_signal_weights_m_d_ref %>% dplyr::filter(weights > 0) %>% dplyr::pull(tickers)
          ####Overwrite elected_signals_and_positions
          elected_signals_and_positions <- ifelse(stringr::str_detect(current_non_zero_weights_signals, pattern = "low_"), "short", "long")
          names(elected_signals_and_positions) <- stringr::str_remove_all(current_non_zero_weights_signals, pattern = "low_")
          #####Print
          if (verbose && d == (training_sample_size + validation_sample_size)){
            cat(crayon::yellow("Custom signal weights detected. Backtest will consider all signals with non-zero custom-weights, regardless of signal selection."))
          }
        } else {
          most_recent_custom_signal_weights_m_d_ref <- NULL
        }

        ####Select and correct signals and backtests
        selected_signals_and_backtest_list <- select_and_correct_signals(
          signals_m_df = features_m_df, #Extract eligible signals from features_m_df and then correct them (multiply short signal by -1)
          chosen_signals_and_positions = elected_signals_and_positions, #Get instruction on what to change features
          backtest_returns_m_xts = backtest_returns_m_xts #Backtest returns to be corrected
        )


        ####Get results
        ####Selected features_m_df with corrected positions
        selected_features_corrected_positions_m_df <- selected_signals_and_backtest_list$selected_signals_corrected_positions_m_df
        ####Selected backtest_returns_corrected_positions_xts
        selected_backtest_returns_corrected_positions_m_xts <- selected_signals_and_backtest_list$selected_backtest_returns_corrected_positions_m_xts
        ####Subset cov_matrix_benchmark
        selected_cov_matrix_benchmark_m_xts <- benchmark_returns_m_xts[, cov_matrix_benchmark]

        ###Check if both are contemplated in signal_themes
        if(!is.null(signal_themes_m_df)){
          if(any(!colnames(dplyr::select(selected_features_corrected_positions_m_df, -id, -tickers, -dates)) %in% unique(signal_themes_m_df %>% dplyr::pull(tickers)))){
            stop("all selected signals (with corrected positions) should have a theme classification in signal_themes_m_df")
          }
          if(any(!colnames(selected_backtest_returns_corrected_positions_m_xts) %in% (signal_themes_m_df%>% dplyr::pull(tickers)))){
            stop("all selected signals in backtests (with corrected positions) should have a theme classification in signal_themes_m_df")
          }
        }

        ###Print message
        if(verbose){
          cat("\n")
          cat(crayon::green("Selecting and correcting signals:"))
          cat("\n")
          cat("Most recent signal universe:\n")
          print(most_recent_signal_universe_m_d_ref %>% dplyr::select(dplyr::any_of(c("id", "theme", "is_eligible"))))
          cat("\n")
        }


        ##################

        ###Time Series Splits
        ##################
        #In case of SB models, there is a validation split to search for hyperparameters. Therefore, the sample is split in three

        ###Features and target split
        ts_splits <- time_series_split(
          #Data
          features_m_df = selected_features_corrected_positions_m_df, target_m_df = target_m_df, target_fwd = target_fwd, target_fwd_name = target_fwd_name,
          #Dates
          current_date = current_date, dates_m_vector = dates_m_vector,
          #Splits
          training_sample_size = training_sample_size, split_method = split_method, validation_sample_size = validation_sample_size
        )

        ####No tuning warning
        if(verbose & sb_algorithm %in% non_tuning_algos){
          cat(sb_algorithm, "chosen as sb_algorithm. Data will be split only in training and test sets.\n")
        }

        ###backtest and selected market factor proxy split (get up to date references)
        selected_backtest_returns_corrected_positions_m_xts_upd_ref <- selected_backtest_returns_corrected_positions_m_xts[which(zoo::index(selected_backtest_returns_corrected_positions_m_xts) <= current_date), ] #Get backtest returns until current date
        selected_cov_matrix_benchmark_m_xts_upd_ref <- selected_cov_matrix_benchmark_m_xts[which(zoo::index(selected_cov_matrix_benchmark_m_xts) <= current_date), ]
        signal_themes_m_d_ref <- if(!is.null(signal_themes_m_df)){signal_themes_m_df %>% dplyr::filter(dates == current_date)} else NULL #Not only selected because we need to compute groups for benchmark

        ##################

        ###Hyperparameter tuning!
        #########################
        if(!sb_algorithm %in% non_tuning_algos){ #If sb_algorithm is OLS or heuristic, one just need to fit model do traininig + validation samples

          #Set seed for testing purposes
          if (!is.null(.test_seed) && is.numeric(.test_seed)){
            if (!sb_algorithm == "nn"){
              set.seed(.test_seed)
            } else {
              set.seed(.test_seed)
              tensorflow::set_random_seed(.test_seed)
            }
          }

          #Training Sample
          #################
          #Get training sample objects
          full_data_training_sample_clean <- ts_splits$training$full_data_training_sample_clean #Clean means no id colms
          ###################

          #Validation Sample
          #################
          #Get validation sample objects
          features_validation_sample <- ts_splits$validation$features_validation_sample
          target_validation_sample <- ts_splits$validation$target_validation_sample
          ##################


          #Set eval_function: function to fit model to training data and calculate eval metrics to validation function
          ##################
          eval_function <- set_eval_function(
            #Choice of ml algorithm
            ml_algorithm = sb_algorithm,
            #Choice of tuning method
            tuning_method = tuning_method
          )
          ##################

          #Hyper tune according to chosen tuning_method
          ##################
          ###Print
          if(verbose){
            cat(paste("Starting", tuning_method, "hyperparameter tuning at:", current_date))
            cat("\n")
          }

          hyper_tune_results <- hyper_tune(
            #General Parameters
            tuning_method = tuning_method, ml_algorithm = sb_algorithm, target_fwd_name = target_fwd_name,
            #Data
            full_data_training_sample_clean = full_data_training_sample_clean,
            features_validation_sample = features_validation_sample, target_validation_sample = target_validation_sample,
            #Eval Function and custom obj
            eval_function = eval_function, custom_objective_translated = custom_objective_translated,
            #Early Stop
            chosen_eval_metric_translated = chosen_eval_metric_translated, early_stop = early_stop,
            #Chosen eval metric
            chosen_eval_metric = chosen_eval_metric, huber_delta = huber_delta, quantile_tau = quantile_tau,
            #Grid/Random Searches
            hyper_grid_domain_list = hyper_grid_domain_list, n_iter = n_iter,
            #Bayesian Optimization
            init_points = init_points, k_iter = k_iter, acq = acq,
            #Keras Parameters
            keras_architecture_parameters = keras_architecture_parameters,
            #Parallelization
            parallel = parallel,
            #Verbose
            verbose = verbose
          )

          ###Store results

          #Fill chosen_eval_metric_validation
          chosen_eval_metric_validation[[which(rebalance_dates == current_date)]] <- #Get correct position for list
            hyper_tune_results$chosen_eval_metric_validation_current_date

          #Get Optimal Hypers and fill hyper_choice_m_xts
          optimal_hyper <- hyper_tune_results$optimal_hyper
          hyper_choice_m_xts[current_date, ] <- optimal_hyper[colnames(hyper_choice_m_xts)] #Get the row corresponding to the rebalancing date and replace hyper_choice_m_xtswith correct order

          #Fill validation_eval_metrics_hyper_choice_m_xts
          validation_eval_metrics_hyper_choice_m_xts[current_date, ] <- hyper_tune_results$validation_eval_metrics_hyper_choice_current_date %>%
            dplyr::select(colnames(validation_eval_metrics_hyper_choice_m_xts)) %>% #Take right columns
            as.numeric() #Turn into numeric

          ###################
        } else {
          optimal_hyper <- NULL #Set optimal hyper as NULL for methods that do not depend on hyper tuning
        }

        #(Re)Fitting
        ###################

        ###Set objects for regression-type algos
        ####Refit new model using data from d - target_fwd
        selected_features_corrected_positions_m_refit <- ts_splits$refit$features_m_refit #Subset -> Signal Ports do not depend on features_m_refit. Therefore, they are fit with most avaiable signal universe data.
        target_m_refit <- ts_splits$refit$target_m_refit #Subset
        selected_full_data_corrected_positions_m_refit_clean <- ts_splits$refit$full_data_m_refit_clean #Full data

        ###Set seed specifically for MVO
        if (!is.null(.test_seed) && is.numeric(.test_seed) && sb_algorithm == "mvo"){
          set.seed(.test_seed)
        }

        #(RE)Fit SB Model
        sb_model_fit <- fit_sb_model(
          #General Parameters
          sb_algorithm = sb_algorithm, target_fwd_name = target_fwd_name,
          #Data
          selected_features_corrected_positions_m_refit = selected_features_corrected_positions_m_refit, target_m_refit = target_m_refit,
          selected_full_data_corrected_positions_m_refit_clean = selected_full_data_corrected_positions_m_refit_clean,
          #Model Params
          custom_objective_translated = custom_objective_translated, huber_delta = huber_delta, quantile_tau = quantile_tau, early_stop = early_stop,
          keras_architecture_parameters = keras_architecture_parameters,
          #Hyperparameters
          optimal_hyper = optimal_hyper, chosen_eval_metric_translated = chosen_eval_metric_translated,
          #Signal Ports Parameters
          most_recent_signal_universe_m_d_ref = most_recent_signal_universe_m_d_ref, most_recent_custom_signal_weights_m_d_ref = most_recent_custom_signal_weights_m_d_ref,
          selected_backtest_returns_corrected_positions_m_xts_upd_ref = selected_backtest_returns_corrected_positions_m_xts_upd_ref, selected_cov_matrix_benchmark_m_xts_upd_ref = selected_cov_matrix_benchmark_m_xts_upd_ref,
          cov_matrix_sample_size = cov_matrix_sample_size, cov_estimation_method = cov_estimation_method, active_returns = active_returns, groups_m_d_ref = signal_themes_m_d_ref,
          rp_method = rp_method, n_random_ports = n_random_ports, random_ports_method = random_ports_method, opt_objective = opt_objective, opt_method = opt_method,
          concentration_constraint_policy = concentration_constraint_policy,
          upper_quantile_winsorization = upper_quantile_winsorization, lower_quantile_winsorization = lower_quantile_winsorization,
          #etc
          verbose = verbose
        )

        ###################

        ##Interpretate
        ###################

        ##Join predictions to design matrix X
        selected_features_corrected_positions_and_predictions_m_refit <- dplyr::mutate(selected_features_corrected_positions_m_refit,
                                                                                       preds = predict(sb_model_fit, selected_features_corrected_positions_m_refit))

        ##Fit Global Surrogate Model
        global_surrogate_model <- switch(gsm_algorithm,
                                         ols = stats::lm(preds ~ ., selected_features_corrected_positions_and_predictions_m_refit %>% dplyr::select(-1:-3)), #Fit OLS Global Surrogate Model
                                         tree = rpart::rpart(preds ~ ., data = selected_features_corrected_positions_and_predictions_m_refit %>% dplyr::select(-1:-3)) #Fit Tree Global Surrogate Model
        )

        ##Get feature importance_m_d_ref
        feature_importance_m_d_ref <- dplyr::full_join(
          # Full join to get both intercept and all signals (eligible or not)
          switch(gsm_algorithm,
                 ols = withCallingHandlers({
                   summary(global_surrogate_model)$coef %>%
                     as.data.frame() %>%
                     dplyr::mutate(tickers = rownames(.), .before = Estimate) %>%
                     dplyr::mutate(tickers = gsub("`", "", tickers)) %>% #Remove ` from tickers to avoid double counting
                     tibble::remove_rownames() %>% # Place rownames as columns
                     dplyr::select(tickers, Estimate) %>% #Get only tickers and coefs
                     dplyr::rename(importance = Estimate) # Rename columns
                 },
                 warning = function(w) {
                   #Supress this warning, which is intrinsic to using ols to explain ols
                   if (grepl("essentially perfect fit: summary may be unreliable", conditionMessage(w))) {
                     invokeRestart("muffleWarning")
                   }
                 }),
                 tree = data.frame(importance = global_surrogate_model$variable.importance) %>%
                   dplyr::mutate(tickers = rownames(.), .before = importance) %>%
                   tibble::remove_rownames()
          ),
          dplyr::select(
            most_recent_signal_universe_m_d_ref,
            # Join with only relevant columns from most_recent_signal_universe_m_d_ref
            dplyr::any_of(
              c("tickers", "theme", "theme_ss_bench_weights", "theme_sb_bench_weights", "is_eligible")
            )
          ),
          by = "tickers"
        ) %>%
          dplyr::mutate(
            normalized_importance = (importance - mean(importance, na.rm = TRUE)) / sd(importance, na.rm = TRUE),
            .after = importance
          ) %>% # Normalize importance
          dplyr::mutate(
            dplyr::across(dplyr::where(is.numeric), ~ tidyr::replace_na(., 0))
          ) %>% # Replace NAs with 0s
          dplyr::mutate(dates = current_date, .before = importance) %>% # Add current date
          dplyr::mutate(id = paste0(tickers, "-", dates), .before = tickers) %>% # Add id
          dplyr::arrange(id) # Arrange by id


        ##Save feature_importance_m_d_ref
        feature_importance_m_d_ref_list[[which(rebalance_dates %in% current_date)]] <- feature_importance_m_d_ref
        ###################
      }

      #Prediction
      ############
      #d stands for the date in which the features are being calculated. Therefore, in d, we have new features, but we still don't have the target, as it is in future.
      d_ref <- which(as.Date(features_m_df %>% dplyr::pull(dates),  format = "%Y-%m-%d") == current_date) #What references correspond to this date?

      #Reference for input in testing list
      testing_lists_ref <- d - training_sample_size - validation_sample_size + 1

      #Subsets for date
      target_vector_ref <- target_vector[d_ref] #targets for new date
      ##In a normal situation, get selected_features from above
                                                         #By 'normal', we mean:
      if ((!is_update_pickup && is_rebalancing_month) || #1) we are in an update and not in an update pickup scenario OR
           !.update){                                    #2) we are running a regular backtest
        selected_features_corrected_positions_m_d_ref <- selected_features_corrected_positions_m_df[d_ref,] #features for new date.
        ##Make predictions
        oos_prediction_list[[testing_lists_ref]] <- predict(sb_model_fit, new_features_m_df = selected_features_corrected_positions_m_d_ref)
        names(oos_prediction_list[[testing_lists_ref]]) <- selected_features_corrected_positions_m_d_ref %>% dplyr::pull(tickers) #Rename

      } else {
      ##In an update pickup, get predictions already made at last rebalancing (IF THEY EXIST)
        if (current_date %in% unique(.old_oos_sb_outputs_m_df$dates)){ ##Check if preds exist
          ####Extract predictions from .old_oos_sb_outputs_m_df
          oos_prediction_list[[testing_lists_ref]] <- .old_oos_sb_outputs_m_df %>% dplyr::filter(dates == current_date) %>% dplyr::pull(pred)
          names(oos_prediction_list[[testing_lists_ref]]) <- .old_oos_sb_outputs_m_df %>% dplyr::filter(dates == current_date) %>% dplyr::pull(tickers) #Rename

        } else {
      ##If there are no predictions, make them
          ####Extract eligible signals from .old_sb_model_fit and change format
          old_eligible_signals <- .old_sb_model_fit@eligible_signals
          old_elected_signals_and_positions <- ifelse(stringr::str_detect(old_eligible_signals, pattern = "low_"), "short", "long")
          names(old_elected_signals_and_positions) <- stringr::str_remove_all(old_eligible_signals, pattern = "low_")
          ####Get features_m_df and then correct them and subset
          selected_features_corrected_positions_m_d_ref <- select_and_correct_signals(
            signals_m_df = features_m_df,
            chosen_signals_and_positions = old_elected_signals_and_positions
          )$selected_signals_corrected_positions_m_df[d_ref,]
          ####Make new predictions
          oos_prediction_list[[testing_lists_ref]] <- predict(.old_sb_model_fit, new_features_m_df = selected_features_corrected_positions_m_d_ref)
          names(oos_prediction_list[[testing_lists_ref]]) <- selected_features_corrected_positions_m_d_ref %>% dplyr::pull(tickers) #Rename
        }
      }

      #Inform targets
      oos_y_list[[testing_lists_ref]] <- as.numeric(target_vector_ref)
      names(oos_y_list[[testing_lists_ref]]) <- names(oos_prediction_list[[testing_lists_ref]])  #Rename

      #Calculate eval metrics and error on testing sample
      testing_metrics <- calculate_eval_metrics(pred = oos_prediction_list[[testing_lists_ref]], target = oos_y_list[[testing_lists_ref]],
                                                huber_delta = huber_delta, quantile_tau = quantile_tau, chosen_eval_metric = chosen_eval_metric, return_error = TRUE)

      #Fill error
      oos_error_list[[testing_lists_ref]] <- as.numeric(testing_metrics$error) #Calculate error
      names(oos_error_list[[testing_lists_ref]]) <- names(oos_prediction_list[[testing_lists_ref]])  #Rename

      #Test Eval Metrics
      oos_testing_eval_metrics_m_xts[testing_lists_ref, ] <- as.numeric(testing_metrics$df_eval_metrics %>%
                                                                        dplyr::select(colnames(oos_testing_eval_metrics_m_xts))) #Eliminate Score

    }

    #################

  })

  #Print elapsed time
  print(elapsed_time)
  if(verbose) cat("=============================\n")

  ###Calculate Final Results
  ##################

  #OOS SB Outputs
  ##Turn all oos_lists to a single meta_dataframe
  ###Rename with testing dates
  names(oos_y_list) <- dates_testing_sample
  names(oos_prediction_list) <- dates_testing_sample
  names(oos_error_list) <- dates_testing_sample

  ##Turn y, preds and error in a oos_sb_outputs_m_df
  oos_y_m_df <- convert_oos_list_to_m_df(oos_y_list) #Convert list to meta dataframe
  colnames(oos_y_m_df)[4] <- "target"
  oos_prediction_m_df <- convert_oos_list_to_m_df(oos_prediction_list) #Convert list to meta dataframe
  colnames(oos_prediction_m_df)[4] <- "pred"
  oos_error_m_df <- convert_oos_list_to_m_df(oos_error_list) #Convert list to meta dataframe
  colnames(oos_error_m_df)[4] <- "error"

  ##Join into a single meta dataframe
  oos_sb_outputs_m_df <- dplyr::left_join(oos_y_m_df, dplyr::select(oos_prediction_m_df, -tickers, -dates), by = "id") %>%
    dplyr::left_join(dplyr::select(oos_error_m_df, -tickers, -dates), by = "id") %>%
    dplyr::arrange(id)


  #Testing Performance Summary
  ##Create consolidated row
  consolidated_eval_metrics_row <- calculate_eval_metrics(pred = oos_sb_outputs_m_df %>% tidyr::drop_na() %>% dplyr::pull(pred),
                                                          target = oos_sb_outputs_m_df %>% tidyr::drop_na() %>% dplyr::pull(target),
                                                          huber_delta = huber_delta, quantile_tau = quantile_tau, chosen_eval_metric = chosen_eval_metric)[-1] #-1 to eliminate Score

  consolidated_eval_metrics_df <- data.frame(metric = names(consolidated_eval_metrics_row), cons_oos = as.numeric(consolidated_eval_metrics_row),
                                             row.names = NULL)

  #Validation metrics
  if (!is_update_pickup && length(rebalance_dates) > 0){
    if (!sb_algorithm %in% non_tuning_algos && length(rebalance_dates) > 0){

      #Validation eval for all hyperparameters
      names(chosen_eval_metric_validation) <- rebalance_dates #Change names

      #Validation Performance Summary
      ##Create average row
      avg_validation_eval_metrics_hyper_choice_df <- data.frame(metric = colnames(validation_eval_metrics_hyper_choice_m_xts),
                                                                avg_val = colMeans(validation_eval_metrics_hyper_choice_m_xts),
                                                                row.names = NULL
      )

      ##Join with consolidated_eval_metrics_df
      consolidated_eval_metrics_df <- dplyr::left_join(consolidated_eval_metrics_df, avg_validation_eval_metrics_hyper_choice_df, by = "metric")
    }

    #Feature Importance
    ##Bind individual feature importance
      feature_importance_m_df <- do.call(rbind, feature_importance_m_d_ref_list) %>% dplyr::arrange(id)
      rownames(feature_importance_m_df) <- NULL
      ###feature_importance_m_df
      feature_importance_m_df <- suppressMessages(create_meta_dataframe(feature_importance_m_df, type = "feature_importance"))
      ###final_feature_importance_m_d_ref
      final_feature_importance_m_d_ref <- suppressMessages(create_meta_dataframe(feature_importance_m_d_ref, type = "feature_importance"))
   } else {
      #In case of empty update
      feature_importance_m_df <- NULL
      final_feature_importance_m_d_ref <- NULL
   }


  #sb_backtest_workflow
  sb_backtest_workflow <- list(
    #Algo
    sb_algorithm = sb_algorithm,
    config_name = "not_identified",
    backtest_identifier = "not_identified",
    custom_objective = custom_objective,
    backtest_type = "base_learner",
    gsm_algorithm = gsm_algorithm,
    #Dates
    dates_covered = dates_m_vector,
    n_dates = length(dates_m_vector),
    training_sample_size = training_sample_size,
    validation_sample_size = validation_sample_size,
    testing_sample_size = testing_sample_size,
    dates_testing_sample = dates_testing_sample,
    first_rebalance_date = first_rebalance_date,
    rebalancing_months = rebalancing_months,
    rebalance_dates = rebalance_dates,
    last_rebalance_date = last_rebalance_date,
    split_method = split_method,
    #Stocks
    ids = features_m_df %>% dplyr::pull(id),
    nobs = length(features_m_df %>% dplyr::pull(id)),
    tickers = unique(features_m_df %>% dplyr::pull(tickers)),
    n_stocks = length(unique(features_m_df %>% dplyr::pull(tickers))),
    #Target
    target_fwd_name = target_fwd_name,
    target_fwd = target_fwd,
    target_workflow = NULL,
    target_object_name = "not_identified",
    target_dates = sort(unique(dplyr::pull(target_m_df, dates))),
    #Features
    features = colnames(features_m_df[,-c(1:3)]),
    features_workflow = NULL,
    features_object_name = "not_identified",
    features_dates = sort(unique(dplyr::pull(features_m_df, dates))),
    #Tuning
    tuning_method = tuning_method,
    n_iter = n_iter,
    k_iter = k_iter,
    acq = acq,
    init_points = init_points,
    hyper_grid_domain_list = hyper_grid_domain_list,
    chosen_eval_metric = chosen_eval_metric,
    huber_delta = huber_delta,
    quantile_tau = quantile_tau,
    early_stop = early_stop,
    #Keras
    keras_architecture_parameters = keras_architecture_parameters,
    #Heuristic SB
    cov_matrix_sample_size = cov_matrix_sample_size,
    cov_estimation_method = cov_estimation_method,
    cov_matrix_benchmark = cov_matrix_benchmark,
    active_returns = active_returns,
    benchmark_returns_object_name = "not_identified",
    benchmark_returns_workflow = NULL,
    benchmark_returns_dates = if (!is.null(benchmark_returns_m_xts)) zoo::index(benchmark_returns_m_xts) else NULL,
    backtest_returns_object_name = "not_identified",
    backtest_returns_workflow = NULL,
    backtest_returns_dates = if (!is.null(backtest_returns_m_xts)) zoo::index(backtest_returns_m_xts) else NULL,
    rp_method = rp_method,
    n_random_ports = n_random_ports,
    random_ports_method = random_ports_method,
    opt_objective = opt_objective,
    concentration_constraint_policy = concentration_constraint_policy,
    signal_themes_object_name = "not_identified",
    signal_themes_workflow = NULL,
    signal_themes_dates = if (!is.null(signal_themes_m_df)) sort(unique(dplyr::pull(signal_themes_m_df, dates))) else NULL,
    lower_quantile_winsorization = lower_quantile_winsorization,
    upper_quantile_winsorization = upper_quantile_winsorization,
    #Performance
    timestamps = c(initialization = Sys.time()),
    elapsed_time = elapsed_time,
    parallel = parallel,
    #Call
    call = match.call()
  )

  #Create meta_dataframes
  ###oos_sb_outputs_m_df
  oos_sb_outputs_m_df <- suppressMessages(create_meta_dataframe(oos_sb_outputs_m_df, type = "oos_sb_outputs", sb_backtest_workflow = sb_backtest_workflow))

  #Create meta_xts
  ###oos_testing_eval_metrics_m_xts
  if (nrow(stats::na.omit(oos_testing_eval_metrics_m_xts)) == 0){
    oos_testing_eval_metrics_m_xts <- NULL
  } else {
    oos_testing_eval_metrics_m_xts <- create_meta_xts(oos_testing_eval_metrics_m_xts %>% stats::na.omit(), type = "metrics",
                                                      source = rep(sb_backtest_workflow$backtest_identifier, ncol(oos_testing_eval_metrics_m_xts)))
  }

  #For tuning algos
  if (!sb_algorithm %in% non_tuning_algos && length(rebalance_dates) > 0){
    ###hyper_choice_m_xts
    hyper_choice_m_xts <- create_meta_xts(hyper_choice_m_xts, type = "metrics",
                                          source = rep(sb_backtest_workflow$backtest_identifier, ncol(hyper_choice_m_xts)))
    ###validation_eval_metrics_hyper_choice_m_xts
    validation_eval_metrics_hyper_choice_m_xts <- create_meta_xts(validation_eval_metrics_hyper_choice_m_xts, type = "metrics",
                                                                  source = rep(sb_backtest_workflow$backtest_identifier, ncol(validation_eval_metrics_hyper_choice_m_xts)))
    } else {
    #In case of empty update or no tuning algos
    hyper_choice_m_xts <- NULL
    validation_eval_metrics_hyper_choice_m_xts <- NULL
    chosen_eval_metric_validation <- NULL
  }



  #Get S4 object
  sb_backtest_results_object <-
    methods::new("sb_backtest_results",
        sb_backtest_config = NULL,
        oos_sb_outputs_m_df = oos_sb_outputs_m_df,
        oos_testing_eval_metrics_m_xts = oos_testing_eval_metrics_m_xts,
        consolidated_eval_metrics = consolidated_eval_metrics_df,
        final_sb_model = if (.update && !exists("sb_model_fit")) NULL else sb_model_fit,
        final_gsm = if (.update && !exists("global_surrogate_model")) NULL else global_surrogate_model,
        chosen_eval_metric_validation = chosen_eval_metric_validation,
        best_hyperparameters_m_xts = hyper_choice_m_xts,
        validation_eval_metrics_hyper_choice_m_xts = validation_eval_metrics_hyper_choice_m_xts,
        feature_importance_m_df = if (.update && !exists("feature_importance_m_df")) NULL else feature_importance_m_df,
        final_feature_importance_m_d_ref = if (.update && !exists("final_feature_importance_m_d_ref")) NULL else final_feature_importance_m_d_ref,
        sb_backtest_workflow = sb_backtest_workflow,
        backtest_identifier = sb_backtest_workflow$backtest_identifier
    )


  #Return List
  return(sb_backtest_results_object)

  #################
}



#' Convert OOS Lists to Meta Data Frame
#' @export
convert_oos_list_to_m_df <- function(oos_obj_list){

  #Convert to list
  m_df <- purrr::map_dfr(names(oos_obj_list), function(date_name) {
    oos_vector <- oos_obj_list[[date_name]]

    # Ensure that oos_vector is a named numeric vector
    if (!is.numeric(oos_vector) || is.null(names(oos_vector))) {
      stop(paste("Each element in 'oos_list' must be a named numeric vector. Issue found in date:", date_name))
    }

    data.frame(
      tickers = names(oos_vector),
      dates = as.Date(date_name),  # Adjust the format if necessary
      value = as.numeric(oos_vector),
      stringsAsFactors = FALSE
    )
  })

  # Create a unique identifier by combining 'tickers' and 'dates'
  m_df <- m_df %>%
    dplyr::mutate(id = paste(tickers, dates, sep = "-")) %>%
    dplyr::select(id, tickers, dates, value)

  return(m_df)
}
