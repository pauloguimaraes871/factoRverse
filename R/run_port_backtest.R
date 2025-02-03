#' Backtest a porfolio based on signals derived from simple stock characteristics (simple factors) or
#' expected returns from ml model predictions
#'
#' @param signals_m_df A matrix or data frame containing simple features, with columns: id, tickers, dates,
#' to be used for signal-portfolio construction
#' @param oos_predictions_m_df A meta_dataframe object containing predictions from sb models, result of a run_sb_backtest call.
#' @param nmonths_until_first_rebalancing_date The amount of dates in dates_m_vector to be skipped before backtest starts
#' @param rebalancing_months Months (numeric) when portfolio should be rebalanced.
#' @param selected_signal One of ml_model, characteristics or meta_signal IS THIS REALLY NECESSARY????????
#' @param chosen_characteristics A vector of characteristics that indicates which cols in features_m_df should be used as signals
#' @param chosen_ml_algorithms A vector of ml_algorithms that indicate which ml model predictions of ml_walk_forward_validation_results_list
#' should be used as signals. If NULL, all objects of ml_walk_forward_validation_results wil be considered
#' @param liquidity_m_df A data frame (similar to `features_m_df`) containing columns for id, tickers, dates,
#' and one or more market liquidity measures (e.g., inflation-adjusted mean financial volume).
#' @param portfolio_construction_method The type of portfolio construction method for setting weights. Possibilities are heuristics (EW, CW, CS, SW),
#' Mean Tracking-Error (MTO) and Risk-Parity (RP)
#' @param covariance_estimation_method One of SAM (Sample), EWMA, CC (Constant Correlation), PCA1, PCA2, Shrink_ID or Shrink_CC.
#' If NULL, blending_method can only be EW or IR
#' @param custom_target_name A character indicating a custom target to be used in the backtest. Default is to use fwd_return_1m in `target_m_df`. If
#' a different variable is provided, the function will first seek that variable in columns of `target_m_df` and then in `signals_m_df`, in that order. If a variable
#' in target_m_df is chosen, the function will derive its prediction horizon based on naming convention and then name the time series accordingly. Otherwise,
#' ir will refer to current date.
#'
#' @return
#' @export
#'
run_port_backtest_internal <- function(
    #Base Objects
    signals_m_df, oos_predictions_m_df = NULL, exp_ret_score_metric = NULL, #Expected Return Score metric is needed when oos_predictions_m_df is not provided
    #Backtest Scheme
    rebalancing_months, initial_sample_size,
    #Portfolio Construction Method
    port_construction_method = "EW", stock_selection_quantile_range = c(0.9, 1.0),
    #RP/MVO Parameters
    rp_method = "cyclical-spinu", n_random_ports = 2000, random_ports_method = "sample", opt_objective = "sharpe", opt_method = "random", #RP/MVO
    #Covariance Estimation
    cov_estimation_method = "sample", cov_matrix_sample_size = 252, active_returns = FALSE, cov_matrix_benchmark = NULL,
    returns_m_xts = NULL, benchmark_returns_m_xts = NULL,
    #Constraints
    liquidity_constraint_policy, turnover_constraint_policy, concentration_constraint_policy,
    #Liquidity Information (Constraints and Active Returns Calculation)
    liquidity_m_df = NULL, liquidity_floor_cutoffs_list, main_liquidity_metric,
    #Group and benchmark constraints (stock groups also used to fill covariance data)
    stocks_groups_m_df = NULL, benchmark_weights_m_df = NULL,
    #Return calculation (needs also liquidity and vol for net returns)
    volatility_m_df = NULL, target_m_df, transaction_costs_list = NULL,
    #Stock Universe Metrics
    custom_stock_universe_metrics_m_df = NULL,
    #Misc
    lower_quantile_winsorization = 0.025, upper_quantile_winsorization = 0.975,
    verbose = TRUE, parallel = TRUE){

  #Measure time to run and run gc
  elapsed_time <- system.time({

    #####################
    ##Check Parameters: This function will test whether inputs match format and current functionalities
    check_inputs_port_backtest(
      #Base Objects
      signals_m_df = signals_m_df, oos_predictions_m_df = oos_predictions_m_df, exp_ret_score_metric = exp_ret_score_metric,
      #Backtest Scheme
      rebalancing_months = rebalancing_months, initial_buffer_period = initial_buffer_period,
      #Portfolio Construction Method
      port_construction_method = port_construction_method, eligibility_quantile_range = eligibility_quantile_range,
      #RP/MVO Parameters
      rp_method = rp_method, n_random_ports = n_random_ports, random_ports_method = random_ports_method, opt_objective = opt_objective, opt_method = opt_method,
      #Covariance Estimation
      cov_estimation_method = cov_estimation_method, cov_matrix_sample_size = cov_matrix_sample_size, active_returns = active_returns, cov_matrix_benchmark = cov_matrix_benchmark,
      returns_m_xts = returns_m_xts, benchmark_returns_m_xts = benchmark_returns_m_xts,
      #Constraints
      liquidity_constraint_policy = liquidity_constraint_policy, turnover_constraint_policy = turnover_constraint_policy, concentration_constraint_policy = concentration_constraint_policy,
      #Liquidity Information (Constraints and Active Returns Calculation)
      liquidity_m_df = liquidity_m_df, liquidity_floor_cutoffs_list = liquidity_floor_cutoffs_list, main_liquidity_metric = main_liquidity_metric,
      #Group and benchmark constraints (stock groups also used to fill covariance data)
      stocks_groups_m_df = stocks_groups_m_df, benchmark_weights_m_df = benchmark_weights_m_df,
      #Return calculation (needs also liquidity and vol for net returns)
      volatility_m_df = volatility_m_df, target_m_df = target_m_df, transaction_costs_list = transaction_costs_list,
      #Custom Stock Universe Metrics
      custom_stock_universe_metrics_m_df = custom_stock_universe_metrics_m_df,
      #Custom Stock Weights
      custom_stock_weights_m_df = custom_stock_weights_m_df,
      #Misc
      lower_quantile_winsorization = lower_quantile_winsorization, upper_quantile_winsorization = upper_quantile_winsorization,
      verbose = verbose, parallel = parallel
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
        ####Extended dates for port returns according to target_m_df (target_fwd = 1)
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

      ###Port Objects
        ####Create object to store portfolio weights
        port_weights_m_df <- signals_m_df %>% dplyr::select(id, tickers, dates) #These are most up-to-date portfolio weights
        port_weights_m_df$port_weights <- 0 #Initialize portfolio weights

        ####Create object to store portfolio returns
        port_returns_m_xts <- xts::xts(data.frame(
          raw_return = rep(NA, length(dates_port_returns)), #Raw target returns
          raw_active_return = rep(NA, length(dates_port_returns)), #Raw active returns
          net_return = rep(NA, length(dates_port_returns)), #Net target returns
          net_active_return = rep(NA, length(dates_port_returns)), #Net active returns
          direct_cost = rep(NA, length(dates_port_returns)), #Direct cost
          market_impact_cost = rep(NA, length(dates_port_returns)), #Market impact cost
          total_cost = rep(NA, length(dates_port_returns)), #Total cost
          turnover = rep(NA, length(dates_port_returns)), #Turnover
          bench_return = rep(NA, length(dates_port_returns)) #Benchmark returns
        ), order.by = dates_port_returns) #These are most up-to-date portfolio returns

        ###Create object to store portfolio metrics
        port_metrics_m_xts <- xts::xts(as.data.frame(
          lapply(custom_stock_universe_metrics_m_df %>% dplyr::select(-id, -tickers, -dates),
                 function(x) rep(NA, length(dates_port_returns)))
          ), order.by = dates_port_returns) #These are most up-to-date portfolio metrics

       ###Selected benchmark
       selected_benchmark <- if (!is.null(concentration_constraint_policy)) concentration_constraint_policy$benchmark else cov_matrix_benchmark
         ####Insert benchmark name in port_returns_m_xts
         names(port_returns_m_xts)[which(names(port_returns_m_xts) == "bench_return")] <- selected_benchmark
         ####Select benchmark_m_xts
         selected_benchmark_returns_m_xts <- benchmark_returns_m_xts[ ,selected_benchmark]
         ####Create object to store benchmark metrics
         if (!is.null(selected_benchmark)){
           benchmark_metrics_m_xts <- xts::xts(as.data.frame(
             lapply(custom_stock_universe_metrics_m_df %>% dplyr::select(-id, -tickers, -dates),
                    function(x) rep(NA, length(dates_port_returns)))
           ), order.by = dates_port_returns) #These are most up-to-date benchmark metrics
         }

       ###Create stock universe list to get results
       stock_universe_m_d_ref_list <- list()

      #####################

      ##Initial Prints
      #########################
      if (verbose){
        ###Text otherwise
        cat("\n")
        cat(crayon::cyan(paste("Starting portfolio backtest")))
        cat("\n")
        cat(paste("Portfolio Construction Method:", port_construction_method))
        cat("\n")
        cat("Building portfolio based on:")
        cat("\n")
        cat(paste("  Expected Return Score Metric:", if(!is.null(exp_ret_score_metric)) exp_ret_score_metric else "Signal Blend Predictions"))
        cat("\n")
        if (port_construction_method %in% c("rp", "mvo")){
            cat("  Covariance Matrix:")
            cat(paste("   Estimation Method:", cov_estimation_method))
            cat(paste("   Sample Size:", cov_matrix_sample_size))
            cat(paste("   Active Returns:", active_returns))

            if (port_construction_method == "mvo"){
              if (any(!is.null(concentration_constraint_policy), !is.null(liquidity_constraint_policy), !is.null(turnover_constraint_policy))){
                cat("  Constraints:")
                if (!is.null(concentration_constraint_policy)){
                  cat("   Concentration Constraint Policy:")
                  cat(paste("    Benchmark:", concentration_constraint_policy$benchmark))
                  cat(paste("    Individual Constraints:", concentration_constraint_policy$max_abs_active_individual_weight))
                  cat(paste("    Group Constraints:", concentration_constraint_policy$max_abs_active_group_weight))
                }
                if (!is.null(liquidity_constraint_policy)){
                  cat(paste("   Liquidity Constraint Policy:", liquidity_constraint_policy$policy))
                  cat(paste("   Liquidity Constraint Threshold:", liquidity_constraint_policy$threshold))
                  cat(paste("   Liquidity Constraint Benchmark:", liquidity_constraint_policy$benchmark))
                }
                if (!is.null(turnover_constraint_policy)){
                  cat(paste("   Turnover Constraint Policy:", turnover_constraint_policy$policy))
                  cat(paste("   Turnover Constraint Threshold:", turnover_constraint_policy$threshold))
                }
              }
            }
          }
        cat(paste("  Selected Benchmark:", if(!is.null(selected_benchmark)) selected_benchmark else "None"))
        cat("\n")
     }
     #########################

    ##Start Backtest
    #####################
    ##Loop through
    for (d in (initial_buffer_period):(initial_buffer_period + backtest_length - 1)){
      ###Current and last date
      current_date <- dates_m_vector[d]
      last_date <- lubridate::add_with_rollback(current_date, months(-1))
      if (verbose) print(current_date)

      ###Get objects for current date
      ##############################
        ####Meta Dataframes
          #####Base Objects
          signals_m_d_ref <- signal_m_df %>% dplyr::filter(dates == current_date)
          target_m_d_ref <- target_m_df %>% dplyr::filter(dates == current_date)
          oos_predictions_m_d_ref <- if (!is.null(oos_predictions_m_df)) oos_predictions_m_df %>% dplyr::filter(dates == current_date) else NULL

          #####Stock Info
          liquidity_m_d_ref <- if (!is.null(liquidity_m_df)) liquidity_m_df %>% dplyr::filter(dates == current_date) else NULL
          volatility_m_d_ref <- if (!is.null(volatility_m_df)) volatility_m_df %>% dplyr::filter(dates == current_date) else NULL
          benchmark_weights_m_d_ref <- if (!is.null(benchmark_weights_m_df)) benchmark_weights_m_df %>% dplyr::filter(dates == current_date) else NULL
          stock_groups_m_d_ref <- if (!is.null(stock_groups_m_df)) stock_groups_m_df %>% dplyr::filter(dates == current_date) else NULL
          custom_stock_universe_metrics_m_d_ref <- if (!is.null(custom_stock_universe_metrics_m_df)) custom_stock_universe_metrics_m_df %>% dplyr::filter(dates == current_date) else NULL
          custom_stock_weights_m_d_ref <- if (!is.null(custom_stock_weights_m_df)) custom_stock_weights_m_df %>% dplyr::filter(dates == current_date) else NULL

          #####Port Weights
            #####Old Composition Beggining-of-Period Portfolio Weights
            #####This is the old end-of-period portfolio with weights updated by fwd_1m_return (ie. composition from last period, with weights reflecting the current period). Delisted stocks are present at this point.
            if (d > 1){
              updated_port_weights_m_lstd_ref <- fwd_port_weights_m_d_ref %>% dplyr::rename(bop_port_weights = port_weights) #This is eop_port_weights from last period updated by fwd_1m_returns. This means that this carries the composition from last period.
            } else {
              #For first period, just get the composition of last period. Weights are zero by construction.
              updated_port_weights_m_lstd_ref <- port_weights_m_df %>% dplyr::filter(dates == last_date) %>% dplyr::rename(bop_port_weights = port_weights)
            }

            #####End-of-period portfolio weights
              #####At this point, this is a placeholder with zero weights, reflecting all stocks that are currently in the universe.
              #####If it is a rebalancing month, update_port_weights will generate transactions and costs needed to transform the old composition into rebalanced one.
              #####If it is not, update_port_weights will just exclude delisted stocks and rescale weights to sum to 1, also generating a transactions_m_df.
              #####Important: at apply_buffer_rule time, the function will look at current composition through signals_m_df and will check for positions in bop_port_weights_m_d_ref, using left_join. Therefore, delisted stocks will not be considered in the buffer rule.
              #####After update_port_weights, eop_port_weights will then carry weights reflecting the end-of-period port. The object is then submitted to calc_port_returns, originating fwd_port_weights_m_d_ref.
              eop_port_weights_m_d_ref <- port_weights_m_df %>% dplyr::filter(dates == current_date)


        ####Meta xts (up to date references)
        returns_m_xts_upd_ref <- returns_m_xts[which(zoo::index(returns_m_xts) <= current_date), ]
        selected_benchmark_returns_m_xts_upd_ref <- selected_benchmark_returns_m_xts[which(zoo::index(selected_benchmark_returns_m_xts) <= current_date), ]

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
          #####Init object
          stock_universe_m_d_ref <- data.frame(
            id = paste0(current_tickers, "-", current_date),
            tickers =  signals_m_d_ref %>% dplyr::pull(tickers), #Current tickers
            dates = current_date
          )

          #####Add exp_ret_score
          if (!is.null(oos_predictions_m_df)){
            #####Add prediction
            stock_universe_m_d_ref <- stock_universe_m_d_ref %>%
              dplyr::left_join(
                oos_predictions_m_d_ref %>% dplyr::select(id, pred), by = "id"
                ) %>%
              dplyr::rename(exp_ret_score = pred) %>% #Rename prediction to exp_ret_score
              dplyr::mutate(exp_ret_score = signal_transform( #Winsorize, z-score and transform
                lower_quantile_winsorization = lower_quantile_winsorization,
                upper_quantile_winsorization = upper_quantile_winsorization)
              )
          } else {
            #####Add signal
            stock_universe_m_d_ref <- stock_universe_m_d_ref %>%
              dplyr::left_join(
                signals_m_d_ref %>% dplyr::select(id, !!rlang::sym(exp_ret_score)), by = "id"
              ) %>%
              dplyr::rename(exp_ret_score = !!rlang::sym(exp_ret_score)) %>% #Rename signal to exp_ret_score
              dplyr::mutate(exp_ret_score = signal_transform( #Winsorize, z-score and transform
                lower_quantile_winsorization = lower_quantile_winsorization,
                upper_quantile_winsorization = upper_quantile_winsorization)
                )
          }

          #####Classify Stock Universe
          stock_universe_m_d_ref <- classify_investment_universe(
            #Stock Universe
            universe_m_d_ref = stock_universe_m_d_ref,

            #Regular eligibility
            eligibility_quantile_range = eligibility_quantile_range, #Quantile range to elect stocks

            ##Liquidity floor rule and classification
            liquidity_m_d_ref = liquidity_m_d_ref, #Liquidity information to apply liquidity floor rule
            liquidity_constraint_policy =  liquidity_constraint_policy, #Liquidity policy
            liquidity_floor_cutoffs_list = liquidity_floor_cutoffs_list, #Definitions about liquidity

            ##Active concentration eligiblity
            benchmark_weights_m_d_ref = benchmark_weights_m_d_ref, #Benchmark weights information to apply
            groups_m_d_ref = stock_groups_m_d_ref, #Sectors data
            concentration_constraint_policy = concentration_constraint_policy, #Active weights policy

            ##Turnover eligibility
            updated_port_weights_m_lstd_ref = updated_port_weights_m_lstd_ref, #Old portfolio weights to apply buffer rule
            turnover_constraint_policy = turnover_constraint_policy, #Turnover policy

            #User defined rules
            user_defined_AND_rules_list = user_defined_AND_rules_list,
            user_defined_OR_rules_list = user_defined_OR_rules_list
          )

          ##############################

        ####Set Portfolio Weights
        ##############################
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
            groups_m_d_ref = stocks_groups_m_d_ref,
            #Covariance Estimation Method
            cov_estimation_method = cov_estimation_method, cov_matrix_sample_size = cov_matrix_sample_size, #Sample size to estimate cov matrix (NULL => full period)
            active_returns = active_returns,
            #Returns sample for covariance estimation
            returns_m_xts_upd_ref = returns_m_xts_upd_ref, selected_benchmark_returns_m_xts_upd_ref = selected_benchmark_returns_m_xts_upd_ref,
            #Risk-Parity method
            rp_method = rp_method,
            #MVO Optimization
            n_random_ports = n_random_ports, random_ports_method = random_ports_method, opt_objective = opt_objective, opt_method = opt_method,
            #Custom Weights
            custom_weights_m_d_ref = custom_stock_weights_m_d_ref,
            #Winsorization
            lower_quantile_winsorization = lower_quantile_winsorization, upper_quantile_winsorization = upper_quantile_winsorization #Quantiles for winsorization
          )

          ####Get stock_universe_m_d_ref
          stock_universe_m_df_list[[which(rebalance_dates %in% current_date)]] <- stock_port@universe_m_d_ref
          ##############################
      }

        ####Print
        if(verbose){
          cat("\n")
          cat(crayon::green(paste("Portfolio rebalancing completed")))
        }

    ##############################

    ###Calculate Port Returns
    ##############################
      portfolio_returns_results_list <- calculate_portfolio_returns(
        #Date parameters
        current_date = current_date, is_rebalancing_month = is_rebalancing_month,
        #Stock universe object
        stock_universe_m_d_ref = stock_universe_m_d_ref,
        #Returns to calculate portfolio returns
        fwd_returns_m_d_ref = fwd_returns_m_d_ref,
        #Returns DF
        portfolio_targets_df = portfolio_targets_df, selected_benchmark_returns_df = selected_benchmark_returns_df,
        #Portfolio weights objects
        portfolio_weights_m_d_ref = portfolio_weights_m_d_ref, portfolio_weights_m_lstd_ref = portfolio_weights_m_lstd_ref,
        #Transaction cost calculation
        ##Liquidity data
        liquidity_m_d_ref = liquidity_m_d_ref, main_liquidity_metric = main_liquidity_metric,
        ##Daily vol data
        volatility_m_d_ref = volatility_m_d_ref,
        ##Transaction cost info
        transaction_costs_list = transaction_costs_list
      )

        #Fill
        ###Portfolio weights
        portfolio_weights_m_df[d_ref, "portfolio_weights"] <- portfolio_returns_results_list$portfolio_weights_m_d_ref$portfolio_weights
        ###Updated Port weights (current portfolio with weights updated to next period)
        updated_portfolio_weights <- portfolio_returns_results_list$updated_portfolio_weights #Updated weights
        ##Portfolio Returns
        portfolio_targets_df <- portfolio_returns_results_list$portfolio_targets_df
        ###Transactions
        transactions_m_df_list[[d]] <- portfolio_returns_results_list$transactions_m_d_ref








  }

  })

}










