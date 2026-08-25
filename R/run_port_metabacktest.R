#' @describeIn run_port_backtest Allocate across a cohort of already-backtested portfolios
#'
#' Runs a meta-portfolio backtest: at each meta rebalance date, weights are chosen across the base
#' portfolios of a `port_backtest_cohort` using those portfolios' own characteristics, and the
#' resulting allocation is then run as an ordinary stock-level portfolio.
#'
#' @details
#' # The two levels
#'
#' The method works at two levels, and the distinction matters for reading the results.
#'
#' At the **meta level** each base portfolio is an asset. Its characteristics are assembled by
#' [derive_port_universe_m_df()] into a [port_universe_m_df-class], the chosen meta score is turned
#' into an expected-return score by [derive_stock_universe_m_d_ref()], eligibility is set by
#' [classify_investment_universe()], and weights are set by [set_portfolio_weights()]. Covariance,
#' where the construction method needs it, is estimated from the base portfolios' own monthly
#' return series, so `cov_matrix_sample_size` counts months here rather than trading days.
#'
#' At the **stock level** those meta weights are multiplied through each base portfolio's own stock
#' weights by [project_meta_weights_to_stocks()], and the result is run through the ordinary
#' backtest engine as a `custom_weights` portfolio. This is what makes the costs real: base
#' portfolios frequently hold the same names, so a meta rebalance nets off at stock level and
#' trades far less than the portfolio-level turnover would suggest.
#'
#' # What is knowable when
#'
#' Meta weights at date `t` are set from statistics the cohort had produced by `t`. Base portfolio
#' analytics exist only on base rebalance dates, so a meta rebalance may be reading figures formed
#' earlier; that is look-ahead safe and reported by `stats_age_months`, and
#' [check_inputs_meta_port_backtest()] warns when it exceeds `max_stats_age_months`.
#'
#' @param port_backtest_cohort A `port_backtest_cohort` holding the base portfolios to allocate
#'   across. Its backtests must have been run on the same data objects passed here.
#' @param custom_port_metrics_m_df Optional `meta_dataframe` of user-computed per-portfolio metrics,
#'   joined into the port universe and available as a meta score. See
#'   [derive_port_universe_m_df()].
#' @param max_stats_age_months Optional whole number. Age above which carried base statistics raise
#'   a warning at meta rebalance dates. `NULL` reports the observed age without a threshold.
#'
#' @return An object of class [port_metabacktest_results-class].
#'
#' @examples
#' \dontrun{
#'   meta_config <- create_port_metabacktest_config(
#'     meta_port_backtest_config = create_port_backtest_config(
#'       chosen_score_metric_and_position = c(ann_info_ratio = "long"),
#'       eligibility_quantile_range = c(0, 1),
#'       initial_buffer_period = 24, rebalancing_months = c(6, 12),
#'       selected_benchmark = "ibov", main_liquidity_metric = "mean_volfin_3m",
#'       port_construction_method = "sw", config_name = "meta_sw_ir"
#'     ),
#'     config_name = "meta_sw_ir"
#'   )
#'
#'   meta_results <- run_port_backtest(
#'     signals_m_df = signals_m_df, fwd_return_m_df = fwd_return_m_df,
#'     liquidity_m_df = liquidity_m_df, volatility_m_df = volatility_m_df,
#'     config = meta_config, port_backtest_cohort = port_cohort,
#'     benchmark_weights_m_df = benchmark_weights_m_df,
#'     benchmark_returns_m_xts = benchmark_returns_m_xts
#'   )
#' }
#' @export
setMethod("run_port_backtest",
          signature(signals_m_df = "meta_dataframe", fwd_return_m_df = "meta_dataframe",
                    liquidity_m_df = "meta_dataframe", volatility_m_df = "meta_dataframe",
                    config = "port_metabacktest_config"),

          function(signals_m_df, fwd_return_m_df, liquidity_m_df, volatility_m_df, config,
                   port_backtest_cohort,
                   custom_port_metrics_m_df = NULL,
                   stock_groups_m_df = NULL, benchmark_weights_m_df = NULL,
                   daily_stock_returns_m_xts = NULL, daily_bench_returns_m_xts = NULL,
                   benchmark_returns_m_xts = NULL,
                   custom_stock_metrics_m_df = NULL,
                   max_stats_age_months = NULL,
                   winsorization_probs = c(0.025, 0.975),
                   verbose = TRUE, parallel = TRUE, .test_seed = NULL) {

            #Initial preparation
            ###########################
            if (verbose) {
              if (!requireNamespace("crayon", quietly = TRUE) ||
                  !requireNamespace("tictoc", quietly = TRUE)) {
                stop("Packages 'crayon' and 'tictoc' are required to generate logs. ",
                     "Please install them using install.packages() or set verbose as FALSE.")
              }
            }
            if (missing(port_backtest_cohort)) {
              stop("port_backtest_cohort must be provided for a meta portfolio backtest.")
            }

            inner_config <- config@meta_port_backtest_config
            lower_quantile_winsorization <- min(winsorization_probs)
            upper_quantile_winsorization <- max(winsorization_probs)
            ###########################

            #Build the port universe
            ###########################
            if (verbose) {
              cat("\n")
              cat(crayon::cyan("Deriving the meta portfolio universe\n"))
            }

            port_universe_m_df <- derive_port_universe_m_df(
              port_backtest_cohort = port_backtest_cohort,
              return_basis = config@return_basis,
              cost_lookback = config@cost_lookback,
              custom_port_metrics_m_df = custom_port_metrics_m_df,
              verbose = verbose
            )
            ###########################

            #Validate
            ###########################
            validation <- check_inputs_meta_port_backtest(
              config = config,
              port_backtest_cohort = port_backtest_cohort,
              port_universe_m_df = port_universe_m_df,
              signals_m_df = signals_m_df, fwd_return_m_df = fwd_return_m_df,
              liquidity_m_df = liquidity_m_df, volatility_m_df = volatility_m_df,
              benchmark_weights_m_df = benchmark_weights_m_df,
              benchmark_returns_m_xts = benchmark_returns_m_xts,
              daily_stock_returns_m_xts = daily_stock_returns_m_xts,
              daily_bench_returns_m_xts = daily_bench_returns_m_xts,
              stock_groups_m_df = stock_groups_m_df,
              max_stats_age_months = max_stats_age_months,
              verbose = verbose
            )
            meta_rebalance_dates <- validation$meta_rebalance_dates
            ###########################

            #Base portfolio return series for covariance estimation
            ###########################
            ##The assets at meta level are portfolios, so their return series is the cohort's own.
            ##It is monthly, which is why cov_matrix_sample_size counts months here.
            returns_slot <- if (config@return_basis == "net") "net_returns_m_xts" else "raw_returns_m_xts"
            base_returns_xts <- port_backtest_cohort@port_returns_m_xts_list[[returns_slot]]@data
            base_portfolio_names <- sort(unique(port_universe_m_df@data$tickers))

            bench_column <- "selected_bench_return"
            meta_bench_returns_xts <- if (bench_column %in% colnames(base_returns_xts)) {
              base_returns_xts[, bench_column, drop = FALSE]
            } else {
              NULL
            }
            base_returns_xts <- base_returns_xts[, base_portfolio_names, drop = FALSE]

            cov_est_method <- inner_config@cov_est_method
            meta_active_returns <- cov_est_method@active_returns && !is.null(meta_bench_returns_xts)
            ###########################

            #Outer loop: set the meta weights
            ###########################
            if (verbose) {
              cat("\n")
              cat(crayon::cyan(paste0("Setting meta weights over ", length(meta_rebalance_dates),
                                      " rebalance dates\n")))
            }

            meta_port_list <- list()
            meta_weights_list <- list()
            meta_stats_list <- list()

            for (i in seq_along(meta_rebalance_dates)) {

              current_date <- meta_rebalance_dates[i]
              port_universe_m_d_ref <- port_universe_m_df@data %>%
                dplyr::filter(dates == current_date)

              ##Turn the chosen meta score into an expected-return score, exactly as the stock
              ##level turns a chosen characteristic into one
              meta_universe_m_d_ref <- derive_stock_universe_m_d_ref(
                signals_m_d_ref = port_universe_m_d_ref,
                oos_predictions_m_d_ref = NULL,
                chosen_score_metric_and_position = inner_config@chosen_score_metric_and_position,
                chosen_scaler = inner_config@chosen_scaler,
                scaler_m_d_ref = NULL,
                scaler_shrinkage = if (is.null(inner_config@scaler_shrinkage)) 0 else inner_config@scaler_shrinkage,
                lower_quantile_winsorization = lower_quantile_winsorization,
                upper_quantile_winsorization = upper_quantile_winsorization
              )

              ##Rank and select base portfolios. The liquidity, turnover and concentration rules
              ##are rejected by the config's validity, so none of them can reach here.
              meta_universe_m_d_ref <- classify_investment_universe(
                universe_m_d_ref = meta_universe_m_d_ref,
                eligibility_quantile_range = inner_config@eligibility_quantile_range,
                min_eligible_assets_fallback = inner_config@min_eligible_assets_fallback,
                use_raw_for_eligibility = if (is.null(inner_config@use_raw_for_eligibility)) {
                  FALSE
                } else {
                  inner_config@use_raw_for_eligibility
                },
                asset_object = "stocks",
                verbose = verbose
              )

              ##Point-in-time return sample: a return stamped at t was realized over the month
              ##ending at t, so including it uses nothing dated later
              base_returns_upd_ref <- base_returns_xts[
                zoo::index(base_returns_xts) <= current_date, , drop = FALSE]
              meta_bench_upd_ref <- if (!is.null(meta_bench_returns_xts)) {
                meta_bench_returns_xts[zoo::index(meta_bench_returns_xts) <= current_date, ,
                                       drop = FALSE]
              } else {
                NULL
              }

              if (!is.null(.test_seed)) set.seed(.test_seed)

              ##A base portfolio is not a benchmark constituent, so the meta universe carries no
              ##bench weights column and selected_benchmark stays NULL here. The benchmark series
              ##still feeds the covariance estimate when active returns are configured.
              meta_port <- set_portfolio_weights(
                universe_m_d_ref = meta_universe_m_d_ref,
                port_construction_method = inner_config@port_construction_method,
                covariance_matrix = NULL,
                eligible_returns_m_xts_upd_ref = base_returns_upd_ref,
                selected_benchmark_m_xts_upd_ref = meta_bench_upd_ref,
                active_returns = meta_active_returns,
                cov_estimation_method = cov_est_method@cov_estimation_method,
                cov_matrix_sample_size = cov_est_method@cov_matrix_sample_size,
                rp_method = if (!is.null(inner_config@rp_parameters)) inner_config@rp_parameters@rp_method else "cyclical-spinu",
                exp_ret_score_tilt = if (!is.null(inner_config@rp_parameters)) inner_config@rp_parameters@exp_ret_score_tilt else NULL,
                exp_ret_score_tilt_eta = if (!is.null(inner_config@rp_parameters)) inner_config@rp_parameters@exp_ret_score_tilt_eta else NULL,
                linkage = if (!is.null(inner_config@hrp_parameters)) inner_config@hrp_parameters@linkage else "single",
                n_random_ports = if (!is.null(inner_config@mvo_parameters)) inner_config@mvo_parameters@n_random_ports else 2000,
                random_ports_method = if (!is.null(inner_config@mvo_parameters)) inner_config@mvo_parameters@random_ports_method else "sample",
                opt_objective = if (!is.null(inner_config@mvo_parameters)) inner_config@mvo_parameters@opt_objective else "sharpe",
                opt_method = if (!is.null(inner_config@mvo_parameters)) inner_config@mvo_parameters@opt_method else "random",
                ridge_pen = if (!is.null(inner_config@mvo_parameters)) inner_config@mvo_parameters@ridge_pen else NULL,
                n_resamples = if (!is.null(inner_config@mvo_parameters)) inner_config@mvo_parameters@n_resamples else 0,
                exp_ret_score_jitter = if (!is.null(inner_config@mvo_parameters)) inner_config@mvo_parameters@exp_ret_score_jitter else 0,
                cov_eigval_jitter = if (!is.null(inner_config@mvo_parameters)) inner_config@mvo_parameters@cov_eigval_jitter else 0,
                top_down_proxy_port_method = "ew", mmaf_group_col = NULL,
                selected_benchmark = NULL,
                lower_quantile_winsorization = lower_quantile_winsorization,
                upper_quantile_winsorization = upper_quantile_winsorization,
                parallel = parallel, verbose = verbose
              )

              meta_port_list[[i]] <- meta_port

              ##Collect the weights and the meta-level analytics for this date
              meta_weights_list[[i]] <- meta_port@universe_m_d_ref@data %>%
                dplyr::select(id, tickers, dates, weights)

              ##The reported risk here is unambiguously absolute: calculate_port_stats()
              ##re-estimates its own covariance with active returns switched off, so
              ##cov_est_method@active_returns reaches only the covariance used to construct the
              ##weights of the covariance-based methods, never the analytics.
              meta_stats_list[[i]] <- data.frame(
                id = paste0("meta_port-", current_date),
                tickers = "meta_port",
                dates = as.Date(current_date),
                stringsAsFactors = FALSE
              ) %>%
                dplyr::bind_cols(meta_port@port_stats)
            }
            ###########################

            #Consolidate the meta weights
            ###########################
            meta_port_weights_m_df <- do.call(rbind, meta_weights_list) %>%
              dplyr::arrange(id) %>%
              as.data.frame()
            rownames(meta_port_weights_m_df) <- NULL

            meta_port_stats_m_df <- do.call(dplyr::bind_rows, meta_stats_list) %>%
              dplyr::arrange(id) %>%
              as.data.frame()
            rownames(meta_port_stats_m_df) <- NULL
            ###########################

            #Project onto stocks
            ###########################
            if (verbose) {
              cat("\n")
              cat(crayon::cyan("Projecting meta weights onto stocks\n"))
            }

            projected_stock_weights_m_df <- project_meta_weights_to_stocks(
              meta_weights_m_df = meta_port_weights_m_df,
              port_backtest_cohort = port_backtest_cohort,
              signals_m_df = signals_m_df,
              verbose = verbose
            )
            ###########################

            #Run the stock-level backtest
            ###########################
            if (verbose) {
              cat("\n")
              cat(crayon::cyan("Running the stock-level backtest on the projected weights\n"))
            }

            meta_port_backtest_results <- run_port_backtest_internal(
              #Base objects. No score source: the weights are supplied.
              signals_m_df = signals_m_df@data,
              oos_predictions_m_df = NULL,
              chosen_score_metric_and_position = NULL,

              #Backtest scheme, shared with the meta level
              rebalancing_months = inner_config@rebalancing_months,
              initial_buffer_period = inner_config@initial_buffer_period,

              #Construction
              port_construction_method = "custom_weights",
              selected_benchmark = inner_config@selected_benchmark,
              eligibility_quantile_range = inner_config@eligibility_quantile_range,

              #The engine's own defaults assume a score-driven method; a custom_weights run has no
              #tilt to apply, and its validator refuses one for any method other than rp or hrp
              exp_ret_score_tilt = NULL, exp_ret_score_tilt_eta = NULL,
              mmaf_group_col = NULL,

              #Covariance, for the stock-level analytics
              cov_estimation_method = cov_est_method@cov_estimation_method,
              cov_matrix_sample_size = if (is.null(daily_stock_returns_m_xts)) {
                cov_est_method@cov_matrix_sample_size
              } else {
                cov_est_method@cov_matrix_sample_size
              },
              active_returns = cov_est_method@active_returns,
              cov_matrix_benchmark = cov_est_method@cov_matrix_benchmark,
              daily_stock_returns_m_xts = if (!is.null(daily_stock_returns_m_xts)) daily_stock_returns_m_xts@data else NULL,
              daily_bench_returns_m_xts = if (!is.null(daily_bench_returns_m_xts)) daily_bench_returns_m_xts@data else NULL,
              benchmark_returns_m_xts = if (!is.null(benchmark_returns_m_xts)) benchmark_returns_m_xts@data else NULL,

              #Constraints are refused by the config's validity, so all three are absent
              liquidity_constraint_policy = NULL, turnover_constraint_policy = NULL,
              concentration_constraint_policy = NULL,

              #Stock information
              liquidity_m_df = liquidity_m_df@data,
              main_liquidity_metric = inner_config@main_liquidity_metric,
              liquidity_floor_cutoffs = inner_config@liquidity_floor_cutoffs,
              volatility_m_df = volatility_m_df@data,
              fwd_return_m_df = fwd_return_m_df@data,
              stock_groups_m_df = if (!is.null(stock_groups_m_df)) stock_groups_m_df@data else NULL,
              benchmark_weights_m_df = if (!is.null(benchmark_weights_m_df)) benchmark_weights_m_df@data else NULL,
              transaction_costs_parameters = if (!is.null(inner_config@transaction_costs_parameters)) {
                as.list(inner_config@transaction_costs_parameters)
              } else {
                NULL
              },

              #The projected weights
              custom_stock_weights_m_df = projected_stock_weights_m_df@data,
              custom_stock_metrics_m_df = if (!is.null(custom_stock_metrics_m_df)) custom_stock_metrics_m_df@data else NULL,

              #Misc
              lower_quantile_winsorization = lower_quantile_winsorization,
              upper_quantile_winsorization = upper_quantile_winsorization,
              verbose = verbose, parallel = parallel, .test_seed = .test_seed
            )
            ###########################

            #Consolidate
            ###########################
            create_port_metabacktest_results(
              port_metabacktest_config = config,
              meta_port_backtest_results = meta_port_backtest_results,
              port_backtest_cohort = port_backtest_cohort,
              port_universe_m_df = port_universe_m_df,
              meta_port_weights_m_df = meta_port_weights_m_df,
              projected_stock_weights_m_df = projected_stock_weights_m_df,
              meta_port_stats_m_df = meta_port_stats_m_df,
              final_meta_port = meta_port_list[[length(meta_port_list)]]
            )
          }
)
