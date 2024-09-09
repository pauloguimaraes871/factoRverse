#' Backtest a porfolio based on signals derived from simple stock characteristics (simple factors) or
#' expected returns from ml model predictions
#'
#' @param features_m_df A matrix or data frame containing simple features, with columns: id, tickers, dates,
#' to be used for signal-portfolio construction
#' @param ml_walk_forward_validation_results_list Optional. A list of one or more objects of class ml_walk_forward_validation_results
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
#' @return
#' @export
#'
#' @examples
metabacktest <- function(signals_m_df, ml_walk_forward_validation_results_list,
                         dates_m_vector, rebalancing_months, n_months_until_first_rebalancing_date,
                         selected_signal, chosen_characteristics, chosen_ml_algorithms,
                         liquidity_m_df, benchmark_weights_m_df, volatility_m_df, target_m_df){


  #Measure time to run and run gc
  elapsed_time <- system.time({

    #Visible binding for global variables

    ################
    ##Check Parameters: This function will test whether inputs match format and current functionalities
    check_inputs_backtest_portfolio()


    ###Init objects###
    ##################

    ###Dates Related
        #Coerce dates
        dates_m_vector <- as.Date(dates_m_vector, format = "%Y-%m-%d")

        #Backtest length
        backtest_length <- length(dates_m_vector) - nmonths_until_first_rebalancing_dates

        #Backtest dates
        dates_backtest <- dates_m_vector[(nmonths_until_first_rebalancing_dates):(nmonths_until_first_rebalancing_dates + backtest_length)] #These are dates of backtest

        #Return calculation dates
        dates_return_calculation <- c(dates_backtest, lubridate::add_with_rollback(dates_backtest[backtest_length], months(1))) #Add extra month bco of target_fwd_1m
        dates_return_calculation <- dates_return_calculation[-1] #Remove first date

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
        portfolio_weights_m_df <- signals_m_df %>% select(id, tickers, dates) #These are most up-to-date portfolio weights

          ##Initialize
          portfolio_weights_m_df$portfolio_weights <- 0

        #Create object to store portfolio returns
          portfolio_returns_df <- data.frame(dates = dates_return_calculation, raw_return = NA, net_return = NA, turnover = NA, total_cost = NA, net_active_return = NA)

    ###Main liquidity metric
          main_liquidity_metric <- ifelse(is.null(main_liquidity_metric), "mean_volfin_3m", main_liquidity_metric)

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

          } else {}

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
    for(d in (n_months_until_first_rebalancing_date):(n_months_until_first_rebalancing_date + backtest_length)){

      #Get current date
      current_date <- dates_m_vector[d]

      #Is rebalancing month?
      is_rebalancing_month <- (lubridate::month(current_date) %in% rebalancing_months) || d == (n_months_until_first_rebalancing_date)

      ###Set date_reference objects
      ##########################
      d_ref <- which(as.Date(signals_m_df$dates,  format = "%Y-%m-%d") == current_date) #What references correspond to this date?

      ##Get current date references
      #Targets
      #target_m_df
      target_m_d_ref <- target_m_df[d_ref,]

      #Transaction cost calculation data
      ###transaction_cost_list_d_ref
      ###liquidity_m_df
      liquidity_m_d_ref <- liquidity_m_df[d_ref, ]
      ###volatility_m_df
      volatility_m_d_ref <- volatility_m_df[d_ref, ]

      #Benchmarks data
      ###benchmark_weights_m_d_ref
      benchmark_weights_m_d_ref <- benchmark_weights_m_df[d_ref, ]
      ##selected_benchmark_returns
      selected_benchmark_returns_df <- benchmark_returns_df[, c("dates", concentration_constraint_policy$benchmark)]

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
      tickers_from_last_port <- updated_portfolio_weights_m_d_ref$tickers
      portfolio_weights_m_d_ref[which(portfolio_weights_m_d_ref$tickers %in% tickers_from_last_port), "portfolio_weights"] <- updated_portfolio_weights_m_d_ref$portfolio_weights

      ###portfolio_weights_m_lstd_ref
      if(d > 1){
        last_date <- dates_m_vector[d-1]
        last_d_ref <- which(as.Date(signals_m_df$dates,  format = "%Y-%m-%d") == last_date) #What references correspond to this date?
        portfolio_weights_m_lstd_ref <- portfolio_weights_m_df[last_d_ref, ]
      } else {
        #If portfolio_weights_m_lstd_ref is pre-backtest
        portfolio_weights_m_lstd_ref <- portfolio_weights_m_d_ref
        portfolio_weights_m_lstd_ref$dates <- "pre-backtest"
        portfolio_weights_m_lstd_ref$weights <- 0
        portfolio_weights_m_lstd_ref$id <- paste0(portfolio_weights_m_lstd_ref$tickers, "-", "pre-backtest")
      }
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
                signal_results_list$ml_walk_forward_validation_results$chosen_eval_metric_validation[[1]]
              ###Hyperparameters choice
              hyper_choice_df[paste(current_date), ] <-
                signal_results_list$ml_walk_forward_validation_results$best_hyperparameters[names(hyper_choice_df)]
              ###All Evaluation Metrics for that hyperparameter choice
              validation_eval_metrics_hyper_choice[paste(current_date), ] <-
                signal_results_list$ml_walk_forward_validation_results$validation_eval_metrics_hyper_choice[, colnames(validation_eval_metrics_hyper_choice)]
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
              testing_lists_ref <- d - nmonths_until_first_rebalancing_dates + 1
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
              target_m_d_ref = target_m_d_ref,
              #Portfolio returns df
              portfolio_returns_df = portfolio_returns_df,
              #Portfolio weights object
              portfolio_weights_m_d_ref = portfolio_weights_m_d_ref,
              #Old Portfolio weights
              portfolio_weights_m_lstd_ref = portfolio_weights_m_lstd_ref,
              #Transaction cost calculation
              ##Liquidity data
              liquidity_m_d_ref = liquidity_m_d_ref,
              ##Daily vol data
              volatility_m_d_ref = volatility_m_d_ref,
              ##Transaction cost info
              transaction_costs_list = NULL
            )

          #Fill portfolio_weights_m_df
            ##For rebalancing months, update portfolio weight to include new rebalanced weights and get rebalancing obj
            if(is_rebalancing_month){
              ###Rebalancing weights
              portfolio_weights_m_df[d_ref, "portfolio_weights"] <- portfolio_returns_results_list$portfolio_weights_m_d_ref$portfolio_weights
              ###Rebalancing DF
              rebalancing_m_df_list[[rebalance_date_ref]] <- portfolio_returns_results_list$rebalancing_m_d_ref
            }

            ##Updated Port weights
            updated_portfolio_weights_m_d_ref <- portfolio_returns_results_list$updated_portfolio_weights_m_d_ref #Updated weights
            ##Portfolio Returns
            portfolio_returns_df <- portfolio_returns_results_list$portfolio_returns_df


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
        results_list$rebalancing_m_df_list <- rebalancing_m_df_list

        ##Bayesian Fit list
        results_list$bayesian_fit_list <- ifelse(!is.null(signal_results_list$bayesian_fit_list), signal_results_list$bayesian_fit_list, NULL)

        ##Portfolio Weights
        results_list$portfolio_weights_m_df <- portfolio_weights_m_df

        ##Portfolio Returns
        results_list$portfolio_returns_df <- portfolio_returns_df

        ##Signal Blend
        results_list$signal_blend_m_df <- signal_blend_m_df


          ###ML Model Results
          if(signal_selection_policy$signal_blending_method == "ML"){
            ####ML WF Validation Results
            results_list$ml_walk_forward_validation_results <- list()

            ####OOS Prediction List
            results_list$ml_walk_forward_validation_results$oos_prediction_list <- oos_prediction_list

            ####OOS Error List
            results_list$ml_walk_forward_validation_results$oos_error_list <- oos_error_list

            ####OOS Y List
            results_list$ml_walk_forward_validation_results$oos_y_list <- oos_y_list

            ####OOS Testing Eval Metrics
            results_list$ml_walk_forward_validation_results$oos_testing_eval_metrics <- oos_testing_eval_metrics

            ####Final Model
            results_list$ml_walk_forward_validation_results$final_model <- signal_results_list$ml_walk_forward_validation_results$final_model

            ####Different composition if ML algorithm is OLS
            if(!signal_selection_policy$ml_parameters$ml_algorithm == "ols"){
              #####Chosen Eval Metric Validation
              names(chosen_eval_metric_validation) <- dates_backtest
              results_list$ml_walk_forward_validation_results$chosen_eval_metric_validation <- chosen_eval_metric_validation

              #####Hyper Choice
              results_list$hyper_choice_df <- hyper_choice_df

              #####Validation Eval Metrics for Hyper Choice
              results_list$validation_eval_metrics_hyper_choice <- validation_eval_metrics_hyper_choice

              #####Fill names for ML Algo =! OLS
              names(result_list) <- c("stock_universe_list", "signal_universe_list", "rebalancing_list", "bayesian_fit",
                "portfolio_weights", "portfolio_returns","signal_blend", "oos_prediction_list",
                "oos_error_list", "oos_y_list", "oos_testing_eval_metrics", "final_model",
                "chosen_eval_metric_validation","best_hyperparameters", "validation_eval_metrics_hyper_choice") #ML Specific

            } else {
              #####Fill names for ML Algo = OLS
              names(result_list) <- c("stock_universe_list", "signal_universe_list", "rebalancing_list", "bayesian_fit",
                "portfolio_weights", "portfolio_returns","signal_blend", "oos_prediction_list",
                "oos_error_list", "oos_y_list", "oos_testing_eval_metrics", "final_model")
            }

            #####Fill names for other Signal Blending Method != ML
            names(result_list) <- c("stock_universe_list", "signal_universe_list", "rebalancing_list", "bayesian_fit",
                                    "portfolio_weights", "portfolio_returns", "signal_blend")
          }

        ##Plots
        #################
          ###ML plots
          ml_plots_list <- plot_ml_walk_forward_validation(
            #Eval metrics
            oos_testing_eval_metrics = oos_testing_eval_metrics, validation_eval_metrics_hyper_choice = validation_eval_metrics_hyper_choice,
            #Hyper choice and chosen eval metric
            hyper_choice_df = hyper_choice_df, chosen_eval_metric = signal_selection_policy$ml_parameters$chosen_eval_metric, chosen_eval_metric_validation = chosen_eval_metric_validation,
            #Backtest parameters
            ml_algorithm = signal_selection_policy$ml_parameters$ml_algorithm, rebalance_dates = rebalance_dates, show_plots = show_plots
          )

          ###Portfolio Plots
          metabacktest_plots_list <- plot_metabacktest()



    })

  #Just return
  return(results_list)

}
