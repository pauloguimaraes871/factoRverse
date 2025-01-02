#' Backtest a porfolio based on signals derived from simple stock characteristics (simple factors) or
#' expected returns from ml model predictions
#'
#' @param signals_m_df A matrix or data frame containing simple features, with columns: id, tickers, dates,
#' to be used for signal-portfolio construction
#' @param ml_walk_forward_validation_results Optional. A list of one or more objects of class ml_walk_forward_validation_results
#' whose oos_prediction_list is to be used as signals and chosen_eval_metric_validation can be used to produce ml ensembles
#' @param dates_m_vector A vector of dates corresponding to the data.
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
#' @examples
metabacktest <- function(signals_m_df,
                         rebalancing_months, initial_buffer_period,
                         liquidity_m_df, benchmark_weights_m_df, volatility_m_df, target_m_df, custom_target_name = "fwd_return_1m"

                         ){


  #Measure time to run and run gc
  elapsed_time <- system.time({

    #Get data frame in case of meta_df object
    if(class(signals_m_df) == "meta_dataframe"){
      signals_m_df <- signals_m_df@data
      workflow <- signals_m_df@workflow
    }

    ################
    ##Check Parameters: This function will test whether inputs match format and current functionalities
    check_inputs_backtest_portfolio()


    ###Init objects###
    ##################

    ###Get Custom Target
    if (custom_target_name %in% names(target_m_df)) {
      ###If custom target name is in target_m_df, use it
      custom_target_m_df <- target_m_df[, c("id", "tickers", "dates", custom_target_name)] ###Build m_df
      custom_target_fwd <- as.numeric(gsub(".*?([0-9]+).*", "\\1", custom_target_name)) ###Infer prediction horizon
      custom_target_type <- "return"

    } else if (custom_target_name %in% names(signals_m_df)) {
      ###Otherwise, if custom target name is in signals_m_df, use it
      if(verbose) warning("Custom target not found in target_m_df. Searching in signals_m_df")
      custom_target_m_df <- signals_m_df[, c("id", "tickers", "dates", custom_target_name)]
      custom_target_fwd <- 0
      custom_target_type <- "signal"

    } else {
      stop("Custom target not found in signals_m_df.")
    }

      ###Print
      if(verbose) message("Custom target: ", custom_target_name, " of type: ", custom_target_type, " with forward horizon of: ", custom_target_fwd)

    ###Dates Related
        #Coerce dates
        dates_m_vector <- as.Date(unique(signals_m_df$dates), format = "%Y-%m-%d") #coerce just to be sure
        dates_m_vector <- dates_m_vector[order(dates_m_vector)] #Re-order ascending just to be sure

        #Backtest length
        backtest_length <- length(dates_m_vector) - initial_buffer_period + 1 #Backtest length follows signals_m_df format, varying accordingly to dates_m_vector.
        #Backtest dates
        dates_backtest <- dates_m_vector[(initial_buffer_period):(initial_buffer_period + backtest_length - 1)] #These are dates of backtest

        #Backtest target calculation dates
        if (custom_target_fwd == 0) {
          # No need to add extra dates or remove any
          dates_target_calculation <- dates_backtest
        } else {
          # Add extra months at the end
          extended_dates <- c(
            dates_backtest,
            lubridate::add_with_rollback(dates_backtest[backtest_length], months(seq_len(custom_target_fwd)))
          )
          # Remove the first 'custom_target_fwd' dates
          dates_target_calculation <- extended_dates[-seq_len(custom_target_fwd)]
        }

        #Rebalancing Dates
        first_rebalance_date <- min(dates_backtest) #Get first rebalancing date
        rebalance_dates <- unique( #Unique is to eliminate repeated dates, in case month of first_rebalance_date is a rebalancing month
          c(first_rebalance_date, dates_backtest[which(lubridate::month(dates_backtest) %in% rebalancing_months)]) #Dates corresponding to rebalancing_months
        )
        rebalance_dates <- rebalance_dates[order(rebalance_dates)] #Re-order

        #Number of rebalancing months
        n_rebalance_months <- length(rebalance_dates)

        #Last rebalance date
        last_rebalance_date <- max(rebalance_dates)

    ###Portfolio objects
        #Create object to store portfolio weights
        portfolio_weights_m_df <- signals_m_df %>% dplyr::select(id, tickers, dates) #These are most up-to-date portfolio weights

          ##Initialize
          portfolio_weights_m_df$portfolio_weights <- 0

        #Create object to store portfolio target
            ###In case custom target type is a return, it makes sense to calculate net_return and net_active_return.
            ###Otherwise, net and net_active targets should be eliiminated
            portfolio_targets_df <- data.frame(dates = dates_target_calculation,
                                               raw_target = NA, raw_active_target = NA, net_target = NA, net_active_target = NA,
                                               direct_cost = NA, market_impact_cost = NA, total_cost = NA, turnover = NA,
                                               placeholder = NA  # Temporary placeholder
            )

           ###Rename the last column dynamically to get benchmark name
           names(portfolio_targets_df)[names(portfolio_targets_df) == "placeholder"] <- concentration_constraint_policy$benchmark


    ###Main liquidity metric
        if(is.null(main_liquidity_metric)){
         warning("main_liquidity_metric is missing and mean_volfin_3m will be used")
        }
        main_liquidity_metric <- ifelse(is.null(main_liquidity_metric), "mean_volfin_3m", main_liquidity_metric)

    ###Selected Benchmark Returns
        selected_benchmark_returns_df <- benchmark_returns_df[, c("dates", concentration_constraint_policy$benchmark)]


    ###Results objects
        #stock_universe_list
        stock_universe_m_df_list <- list()
        #signal_universe_list
        signal_universe_m_df_list <- list()
        #rebalancing
        rebalancing_m_df_list <- list()
        #signal_blend_m_df
        signal_blend_m_df <- signals_m_df %>% dplyr::select(id, tickers, dates)
        signal_blend_m_df$final_signal <- NA

    ###ML objects
        if(signal_selection_policy$signal_blending_method == "ML"){
          if(signal_selection_policy$ml_parameters$ml_algorithm != "ols"){

          #Store hyperparameters choice (model complexity)
          #Store validation chosen eval
          chosen_eval_metric_validation <- list()
          #Store validation eval
          validation_eval_metrics_hyper_choice <- data.frame(
            rss = as.vector(rep(NA, n_rebalance_months)), #R2
            cp = as.vector(rep(NA, n_rebalance_months)), #CP
            rmse = as.vector(rep(NA, n_rebalance_months)), #Root Mean Squared Error
            mae = as.vector(rep(NA, n_rebalance_months)), #Mean Absolute Error
            mphe = as.vector(rep(NA, n_rebalance_months)), #Mean Pseudo huber
            mpe = as.vector(rep(NA, n_rebalance_months)), #Mean Pinball Error
            mape = as.vector(rep(NA, n_rebalance_months)), #Mean Absolute Percentage Error
            hr = as.vector(rep(NA, n_rebalance_months)), #Hit Rate
            mb = as.vector(rep(NA, n_rebalance_months)), #Mean Bias
            row.names = rebalance_dates
          )

          #Store hyper_choice_df based on existence of early stop and best_lam
          hyper_choice_df <- as.data.frame(matrix(NA, nrow = n_rebalance_months, ncol = length(signal_selection_policy$ml_parameters$hyper_grid_domain_list)))
          rownames(hyper_choice_df) <- rebalance_dates #Set rownames as rebalance dates
          colnames(hyper_choice_df) <- names(signal_selection_policy$ml_parameters$hyper_grid_domain_list) #Set colnames as hyperparameters
          #Add best-lam and best-iteration
          hyper_choice_df$best_lam <- if(signal_selection_policy$ml_parameters$ml_algorithm == "glmnet") NA
          hyper_choice_df$best_iteration <- if(!is.null(signal_selection_policy$ml_parameters$early_stop)) NA

          }

          #Store test eval
          oos_testing_eval_metrics <- data.frame(
            rss = as.vector(rep(NA, backtest_length + 1)), #+1 bco first date is also a testing date
            cp = as.vector(rep(NA, backtest_length + 1)),
            rmse = as.vector(rep(NA, backtest_length + 1)),
            mae = as.vector(rep(NA, backtest_length + 1)),
            mphe = as.vector(rep(NA, backtest_length + 1)),
            mpe = as.vector(rep(NA, backtest_length + 1)),
            mape = as.vector(rep(NA, backtest_length + 1)),
            hr = as.vector(rep(NA, backtest_length + 1)),
            mb = as.vector(rep(NA, backtest_length + 1))
          )
          #Change names
          rownames(oos_testing_eval_metrics) <- dates_backtest


          #Prediction, error and Y objects
          oos_prediction_list <- list() #initialize prediction list. Each element will be a vector of predictions for that date
          oos_error_list <- list() #Initialize error list.
          oos_y_list <- list() #Initialize y list.

        }


    ##################

    ###Backtest###
    ##################
    #Loop through
    for(d in (initial_buffer_period):(initial_buffer_period + backtest_length)){

      #Get current date
      current_date <- dates_m_vector[d]

      #Is rebalancing month?
      is_rebalancing_month <- (lubridate::month(current_date) %in% rebalancing_months) || d == (initial_buffer_period)

      ###Set date_reference objects
      ##########################
      d_ref <- which(as.Date(signals_m_df$dates,  format = "%Y-%m-%d") == current_date) #What references correspond to this date?

      ##Get current date references
      #Targets
      #target_m_df
      target_m_d_ref <- target_m_df[d_ref,]

      #Transaction cost calculation data
      ###liquidity_m_df
      liquidity_m_d_ref <- liquidity_m_df[d_ref, ]
      ###volatility_m_df
      volatility_m_d_ref <- volatility_m_df[d_ref, ]

      #Benchmarks data
      ###benchmark_weights_m_d_ref
      benchmark_weights_m_d_ref <- benchmark_weights_m_df[d_ref, ]

      ##Get up to date references for daily stock returns and create adequate sample
      try(daily_active_returns_upd_ref <- daily_active_returns_df[which(daily_returns_df$dates <= current_date),], silent = TRUE) #Get dates sequence

      #Groups data
      ###groups_list_d_ref
      try(groups_m_d_ref_list <- lapply(groups_m_df_list, function(x){if(is.data.frame(x)) return(x[d_ref,]) else x}))
      ###Get sectors
      stocks_groups_m_d_ref <- groups_m_d_ref_list$stocks
      ###Get themes
      signals_groups_m_d_ref <- groups_m_d_ref_list$signals


      #Portfolio Weights
      ###portfolio_weights_m_d_ref
      portfolio_weights_m_d_ref <- portfolio_weights_m_df[d_ref, ]
      ###Set updated weights calculated with future returns from last period
      if(d > 1){
        portfolio_weights_m_lstd_ref <- updated_portfolio_weights #Sets updated weights from old portfolio
      } else {
        #If portfolio_weights_m_lstd_ref is pre-backtest
        portfolio_weights_m_lstd_ref <- portfolio_weights_m_d_ref
        portfolio_weights_m_lstd_ref$dates <- "pre-backtest"
        portfolio_weights_m_lstd_ref$weights <- 0
        portfolio_weights_m_lstd_ref$id <- paste0(portfolio_weights_m_lstd_ref$tickers, "-", "pre-backtest")
      }
        #Rename
        colnames(portfolio_weights_m_lstd_ref)[4] <- "old_portfolio_weights" #rename

        ##Check if it's a rebalancing month
        if(is_rebalancing_month){
          #Print refitting message
          if(verbose == TRUE){
            cat("\n")
            cat(crayon::yellow(paste("Starting portfolio rebalancing at:", current_date)))
            cat("\n")
          } else {}

          #Get reference of rebalance_dates vector
          rebalance_date_ref <- which(rebalance_dates %in% current_date)

              ###Check for no variation in signals
              #no_variation_characteristics <- as.data.frame(signals_m_upd_ref[,-1]) %>% apply(2, function(x) length(unique(x)) == 1) #check condition
              #if(any(no_variation_characteristics)){
              #  warning(paste("No variation observed in column",
              #                colnames(signals_m_upd_ref[,-1])[which(no_variation_characteristics == TRUE)],
              #                "of signals_m_upd_ref in date",
              #                current_date)
              #  )
              #}

        ##########################


        ###Create selected signal with blend
        #####################################################
          ####Compute signal matrix
          signal_results_list <- blend_signals(
            #Current date for up to date reference
            current_date = current_date,
            #Signals
            signals_m_df = signals_m_df, #Note that this contain full period information
            #Target (for ML processing)
            target_m_df = target_m_df,

            #Signal Selection Policy
            signal_selection_policy = signal_selection_policy, #How to blend signals

            #Themes
            signals_groups_m_d_ref = signals_groups_m_d_ref,

            #Backtests and benchmark returns
            selected_benchmark_returns_df = selected_benchmark_returns_df,
            backtest_returns_df = backtest_returns_df,

            #Priors data
            priors_m_df_list = priors_m_df_list,

            #Covariance estimation method
            covariance_estimation_method = covariance_estimation_method,

            #Portfolio Optimization methos
            n_random_portfolios = n_random_portfolios, rp_method = rp_method, mto_port_objective = mto_port_objective,

            #Winsorization
            upper_quantile_winsorization = upper_quantile_winsorization, lower_quantile_winsorization = lower_quantile_winsorization
          )

          ####Fill Rebalancing Information
            ##General objects
            signal_universe_m_df_list[[rebalance_date_ref]] <- signal_results_list$signal_universe_m_d_ref

            ##ML objects
            if(signal_selection_policy$signal_blending_method == "ML"){
              ###Chosen Eval Metric Variation Accross a Given Validation
              chosen_eval_metric_validation[[which(rebalance_dates == current_date)]] <-
                signal_results_list$ml_walk_forward_validation_results@chosen_eval_metric_validation[[1]]
              ###Hyperparameters choice
              hyper_choice_df[paste(current_date), ] <-
                signal_results_list$ml_walk_forward_validation_results@best_hyperparameters[names(hyper_choice_df)]
              ###All Evaluation Metrics for that hyperparameter choice
              validation_eval_metrics_hyper_choice[paste(current_date), ] <-
                signal_results_list$ml_walk_forward_validation_results@validation_eval_metrics_hyper_choice[, colnames(validation_eval_metrics_hyper_choice)]
            }

        #################################################

        ###Filter stock universe based on signal, liquidity and benchmark_position
        ##########################################################################
        stock_universe_m_d_ref <- classify_investment_universe(
          signals_m_d_ref = signal_results_list$stock_universe_m_d_ref, #Selected signal DF with final signal column

          #Regular eligibility
          top_assets_quantile = top_assets_quantile, #Quantile for top stock selection

              ##Liquidity floor rule and classification
              liquidity_m_d_ref = liquidity_m_d_ref, #Liquidity information to apply liquidity floor rule
              liquidity_constraint_policy =  liquidity_constraint_policy, #Liquidity policy
              liquidity_floor_cutoffs_list = liquidity_floor_cutoffs_list, #Definitions about liquidity

          ##Active concentration eligiblity
          benchmark_weights_m_d_ref = benchmark_weights_m_d_ref, #Benchmark weights information to apply
          groups_m_d_ref = stocks_groups_m_d_ref, #Sectors data
          concentration_constraint_policy = concentration_constraint_policy, #Active weights policy

          ##Turnover eligibility
          portfolio_weights_m_lstd_ref = portfolio_weights_m_lstd_ref, #Old portfolio weights to apply buffer rule
          turnover_constraint_policy = turnover_constraint_policy, #Turnover policy

        #User defined rules
        user_defined_AND_rules_list = user_defined_AND_rules_list,
        user_defined_OR_rules_list = user_defined_OR_rules_list
      )


      ##########################################################################


      ###Define Portfolio Weights
      #############################
          stock_universe_m_d_ref <-  set_portfolio_weights(
            #Stock universe object
            universe_m_d_ref = stock_universe_m_d_ref,
            #Stock Portfolio Construction method
            portfolio_construction_method = portfolio_construction_method,
            #Groups
            groups_m_d_ref = stocks_groups_m_d_ref,
            #Returns sample
            returns_upd_ref = daily_active_returns_upd_ref,
            #Covariance Estimation Method
            covariance_estimation_method = covariance_estimation_method, covariance_matrix_sample_size = covariance_matrix_sample_size, #Sample size to estimate cov matrix (NULL => full perior)
            #Number of random portfolios to generate for numeric optimization
            n_random_portfolios = n_random_portfolios,
            #RandomPorts method
            rp_method = rp_method,
            #What to optimize? #Objective for MTO Optimization
            mto_port_objective = mto_port_objective,
            #Cap weighting metric
            cap_weighting_metric = main_liquidity_metric,
            #Set concentration constraint policy for stocks
            concentration_constraint_policy = concentration_constraint_policy,
            #Set turnover constraint policy for stocks
            turnover_constraint_policy = turnover_constraint_policy,
            #Set liquidity constraint policy for stocks
            liquidity_constraint_policy = liquidity_constraint_policy,
            #Liquidity Obj
            liquidity_m_d_ref = liquidity_m_d_ref,
            #Winsorization
            lower_quantile_winsorization = lower_quantile_winsorization, #Quantiles for winsorization
            upper_quantile_winsorization = upper_quantile_winsorization #Quantiles for winsorization
          )

        #Fill
        stock_universe_m_df_list[[rebalance_date_ref]] <- stock_universe_m_d_ref

      #############################

        } else {} #Rebalancing Completed
        if(verbose){
          cat("\n")
          cat(crayon::green(paste("Portfolio rebalancing completed")))
        }
        #Update signal_blend_m_df and calculate portfolio returns

          #Get final signal
          ###########################
          if(is_rebalancing_month){
            #If it is a rebalancing month, final signal has already been calculated
            signal_blend_m_df[d_ref, "final_signal"] <- signal_results_list$selected_signals_corrected_positions_m_d_ref$final_signal
            ml_predictions <- signal_results_list$ml_predictions #Machine-Learning Predictions
            new_features_m_d_ref <- signal_results_list$new_features_m_d_ref #New Features

          } else {
            #Otherwise, it must be calculated for current date
            signal_results_list <- blend_signals(
              #Current date for up to date reference
              current_date = current_date,

              #Objects to calculate final signal
              eligible_signals = signal_results_list$eligible_signals,
              signal_weights = signal_results_list$signal_weights,
              ml_walk_forward_validation_results = signal_results_list$ml_walk_forward_validation_results,
              #Signals
              signals_m_df = signals_m_df, signal_selection_policy = signal_selection_policy, #How to blend signals

              #Winsorization
              upper_quantile_winsorization = upper_quantile_winsorization, lower_quantile_winsorization = lower_quantile_winsorization,

              #Set everything else to NULL
              signals_groups_m_d_ref = NULL, selected_benchmark_returns_df = NULL, backtest_returns_df = NULL, priors_m_df_list = NULL, covariance_estimation_method = NULL,
              n_random_portfolios = NULL, rp_method = NULL, mto_port_objective = NULL
            )
            #Fill
            signal_blend_m_df[d_ref, "final_signal"] <-  signal_results_list$stock_universe_m_d_ref$final_signal
            ml_predictions <- signal_results_list$ml_predictions #Machine-Learning Predictions
            new_features_m_d_ref <- signal_results_list$new_features_m_d_ref #New Features
          }
          #############################

          #Get ML objects if it's the case
          #################################
          if(signal_selection_policy$signal_blending_method == "ML"){
            #Predictions
              ##Reference for input in testing list
              testing_lists_ref <- d - initial_buffer_period + 1
              ##Target object
              target_vector_ref <- target_m_df[which(target_m_df$dates == current_date), signal_selection_policy$ml_parameters$target_fwd_name]

              ##OOS Predictions
              oos_prediction_list[[testing_lists_ref]] <- ml_predictions
              names(oos_prediction_list[[testing_lists_ref]]) <- new_features_m_d_ref$tickers

              ##Inform targets
              oos_y_list[[testing_lists_ref]] <- as.numeric(target_vector_ref)
              names(oos_y_list[[testing_lists_ref]]) <- new_features_m_d_ref$tickers

              ##Calculate eval metrics and error on testing sample
              testing_metrics <- calculate_eval_metrics(
                pred = oos_prediction_list[[testing_lists_ref]], target = oos_y_list[[testing_lists_ref]],
                huber_delta = signal_selection_policy$ml_parameters$huber_delta,
                quantile_tau = signal_selection_policy$ml_parameters$quantile_tau,
                chosen_eval_metric = signal_selection_policy$ml_parameters$chosen_eval_metric, return_error = TRUE)
                ###Fill error
                oos_error_list[[testing_lists_ref]] <- as.numeric(testing_metrics$error) #Calculate error
                names(oos_error_list[[testing_lists_ref]]) <- new_features_m_d_ref$tickers #Rename
                ###Test eval metrics
                oos_testing_eval_metrics[testing_lists_ref,] <- testing_metrics$df_eval_metrics[,colnames(oos_testing_eval_metrics)]

           } else {}
           ###############################

          #Calculate Portfolio Returns
          #############################
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

    } #End of backtest

    ##################

    ###Results###
    ##################
      #Rename
      names(stock_universe_m_df_list) <- rebalance_dates
      names(signal_universe_m_df_list) <- rebalance_dates
      names(rebalancing_m_df_list) <- rebalance_dates
      names(oos_prediction_list) <- rebalance_dates
      names(oos_y_list) <- rebalance_dates

      #Metadata
      metadata <- list()

      #Add benchmark weights to portfolio_weights_m_df
      portfolio_weights_m_df <- dplyr::left_join(portfolio_weights_m_df, dplyr::select(benchmark_weights_m_df, -id, -dates), by = "tickers")

      #Results list
      results_list <- list()
        ##Stock Universe List
        results_list$stock_universe_m_df_list <- stock_universe_m_df_list

        ##Signal Universe List
        results_list$signal_universe_m_df_list <- signal_universe_m_df_list

        ##Rebalancing List
        results_list$transactions_m_df_list <- transactions_m_df_list

        ##Bayesian Fit list
        results_list$bayesian_fit_list <- ifelse(!is.null(signal_results_list$bayesian_fit_list), signal_results_list$bayesian_fit_list, NULL)

        ##Portfolio Weights
        results_list$portfolio_weights_m_df <- portfolio_weights_m_df

        ##Portfolio Returns
        results_list$portfolio_targets_df <- portfolio_targets_df

        ##Signal Blend
        results_list$signal_blend_m_df <- signal_blend_m_df


          ###ML Model Results
          if(signal_selection_policy$signal_blending_method == "ML"){
            ####ML WF Validation Results
            ml_walk_forward_validation_result_list <- list()

            ####OOS Prediction List
            ml_walk_forward_validation_result_list$oos_prediction_list <- oos_prediction_list

            ####OOS Error List
            ml_walk_forward_validation_result_list$oos_error_list <- oos_error_list

            ####OOS Y List
            ml_walk_forward_validation_result_list$oos_y_list <- oos_y_list

            ####OOS Testing Eval Metrics
            ml_walk_forward_validation_result_list$oos_testing_eval_metrics <- oos_testing_eval_metrics

            ####Final Model
            ml_walk_forward_validation_result_list$final_model <- signal_results_list$ml_walk_forward_validation_results@final_model

            ####Different composition if ML algorithm is OLS
            if(!signal_selection_policy$ml_parameters$ml_algorithm == "ols"){
              #####Chosen Eval Metric Validation
              names(chosen_eval_metric_validation) <- dates_backtest
              ml_walk_forward_validation_result_list$chosen_eval_metric_validation <- chosen_eval_metric_validation

              #####Hyper Choice
              ml_walk_forward_validation_result_list$hyper_choice_df <- hyper_choice_df

              #####Validation Eval Metrics for Hyper Choice
              ml_walk_forward_validation_result_list$validation_eval_metrics_hyper_choice <- validation_eval_metrics_hyper_choice

              #####Fill names for ML Algo =! OLS
              names(ml_walk_forward_validation_result_list) <- c("oos_prediction_list", "oos_error_list", "oos_y_list", "oos_testing_eval_metrics", "final_model",
                                                                "chosen_eval_metric_validation", "best_hyperparameters", "validation_eval_metrics_hyper_choice")

              #ML Specific
              names(result_list) <- c("stock_universe_list", "signal_universe_list", "transaction_list", "bayesian_fit",
                                      "portfolio_weights", "portfolio_returns", "signal_blend", "ml_wf_val_results")

            } else {
              #####Fill names for ML Algo = OLS
              names(ml_walk_forward_validation_result_list) <- c("oos_prediction_list", "oos_error_list", "oos_y_list", "oos_testing_eval_metrics", "final_model")

            }

            #####Get S4 Obj
            result_list$ml_wf_val_results <-
              new("ml_wf_val_results",
                  oos_prediction_list = ml_walk_forward_validation_result_list$oos_prediction_list,
                  oos_error_list = ml_walk_forward_validation_result_list$oos_error_list,
                  oos_y_list = ml_walk_forward_validation_result_list$oos_y_list,
                  oos_testing_eval_metrics = ml_walk_forward_validation_result_list$oos_testing_eval_metrics,
                  final_model = ml_walk_forward_validation_result_list$final_model,
                  chosen_eval_metric_validation = ml_walk_forward_validation_result_list$chosen_eval_metric_validation,
                  best_hyperparameters = ml_walk_forward_validation_result_list$best_hyperparameters,
                  validation_eval_metrics_hyper_choice = ml_walk_forward_validation_result_list$validation_eval_metrics_hyper_choice,
                  metadata = signal_selection_policy$ml_parameters
              )

             #Name results list
              names(results_list) <- c("stock_universe_list", "signal_universe_list", "transaction_list", "bayesian_fit",
                "portfolio_weights", "portfolio_returns", "signal_blend", "ml_wf_val_results")

          } else {

            #####Fill names for other Signal Blending Method != ML
            names(results_list) <- c("stock_universe_list", "signal_universe_list", "transaction_list", "bayesian_fit",
                                    "portfolio_weights", "portfolio_returns", "signal_blend")

            }


        })

        ##GET S4 Object
        #################
          ###New S4

          ###Portfolio Plots
          metabacktest_plots_list <- plot_metabacktest()

  #Just return
  return(results_list)

}
