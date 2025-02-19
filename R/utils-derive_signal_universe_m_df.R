#' Derive Signal Universe for Systematic Backtest
#'
#' @description
#' Generates or extracts a signal universe for systematic backtesting, with flexible input methods.
#'
#' @param config SB Config
#' @param features_m_df Feature matrix dataframe
#' @param backtest_returns_m_xts XTS of backtest returns
#' @param benchmark_returns_m_xts XTS of benchmark returns
#' @param signal_themes_m_df Dataframe of signal themes (optional)
#' @param priors_m_df Priors dataframe (optional)
#' @param custom_signal_universe_metrics_m_df Custom metrics dataframe (optional)
#' @param verbose Logical to enable verbose output
#' @param parallel Logical to enable parallel processing
#' @param winsorization_probs Winsorization probabilities
#'
#' @return A dataframe representing the signal universe with eligibility and metadata
#'
#' @details
#' This function can derive a signal universe through two primary methods:
#' 1. Using provided signal selection backtest results
#' 2. Generating an artificial signal universe based on input configurations
#'
#' @note
#' - Requires careful input matching and validation
#' - Supports full and partial signal selection
#' - Handles both long and short signal positions
derive_signal_universe_m_df <- function(config,
                                        features_m_df,
                                        backtest_returns_m_xts, benchmark_returns_m_xts, signal_themes_m_df, priors_m_df, custom_signal_universe_metrics_m_df,
                                        verbose, parallel, winsorization_probs
                                        ){

  #Get objects from config and features_m_df
  ################################
  ss_backtest_results <- config@ss_backtest_results
  ss_backtest_config <- config@ss_backtest_config
  chosen_signals_and_positions <- config@chosen_signals_and_positions
  features_object_name = features_m_df@meta_dataframe_name
  if (config@sb_algorithm %in% c("rp", "mvo")){
    cov_matrix_benchmark <- config@signal_port_parameters@cov_est_method@cov_matrix_benchmark
  } else {
    cov_matrix_benchmark <- NULL
  }


  ################################

  if(is.null(ss_backtest_results)){
    #Run a Signal Selection Backtest based on ss_backtest_config
    ###########################
    if(!is.null(ss_backtest_config)){
      ##Print
      if(verbose){
        cat(crayon::cyan("Running Signal Selection Backtest based on ss_backtest_config \n"))
      }
      ##Checks
      ###Objects being provided
      if(is.null(backtest_returns_m_xts)){
        stop("A backtest_returns_m_xts must be provided when providing a ss_backtest_config")
      }
      if(is.null(benchmark_returns_m_xts)){
        stop("A benchmark_returns_m_xts must be provided when providing a ss_backtest_config")
      }
      if(is.null(signal_themes_m_df)){
        stop("A signal_themes_m_df must be provided when providing a ss_backtest_config")
      }
      ##Conformity
      if(!is.null(cov_matrix_benchmark) && ss_backtest_config@alpha_test_strategy@market_factor_proxy != cov_matrix_benchmark){
        message(crayon::yellow("market_factor_proxy and cov_matrix_benchmark in ss_backtest_config differ."))
      }

      #Run Signal Selection Backtest
      ss_backtest_results <- run_ss_backtest(
        config = ss_backtest_config, #Configuration
        signals_m_df = features_m_df, #Data
        backtest_returns_m_xts = backtest_returns_m_xts, benchmark_returns_m_xts = benchmark_returns_m_xts, signal_themes_m_df = signal_themes_m_df, #SS Main Objs
        priors_m_df = priors_m_df, #Priors derivation
        custom_signal_universe_metrics_m_df = custom_signal_universe_metrics_m_df, #Custom Metrics
        verbose = verbose, parallel = parallel, winsorization_probs = winsorization_probs #Misc
      )
      #Extract signal_universe_m_df
      signal_universe_m_df <- ss_backtest_results@signal_universe_m_df@data
      cat(crayon::green("Signal Selection Backtest completed sucessfully \n"))
      #Extract chosen_signals_and_positions
      chosen_signals_and_positions <- ss_backtest_results@ss_backtest_workflow$chosen_signals_and_positions

    }
    ###########################
    else {
    #Generate an artificial signal_universe_m_df
    ###########################

      ##Get columns values
      dates <- unique(features_m_df@data %>% dplyr::pull(dates))
      tickers <- colnames(features_m_df@data[,-c(1:3)])

      ##Adjust tickers based on chosen_signals_and_positions
      if (length(chosen_signals_and_positions) == 1 && chosen_signals_and_positions == "all"){
        chosen_signals <- tickers #Get all signals in signals_m_df
        chosen_signals_and_positions <- rep("long", length(chosen_signals)) #Set all positions as 'long'
        names(chosen_signals_and_positions) <- chosen_signals
          ###Print
          if (verbose) cat("According to user choice, SB backtest will contemplate all signals in features_m_df, assuming a 'long' position.")

      } else {
        ##Some checks
        ###Check if all chosen_signals_and_positions are inside features_m_df
        if (any(!names(chosen_signals_and_positions) %in% colnames(features_m_df@data))){
          stop("chosen_signals_and_positions must be match the columns of features_m_df")
        }
          ####Print
          if (verbose) cat("According to user choice, SB backtest will contemplate the following features in features_m_df:")
          if (verbose) print(chosen_signals_and_positions)
      }

      ##Create signal_universe_m_df
      signal_universe_m_df <- expand.grid(tickers, dates, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE) %>%
        dplyr::rename(tickers = Var1, dates = Var2) %>%
        dplyr::mutate(
          is_eligible = dplyr::if_else(
            is.na(chosen_signals_and_positions[tickers]),
            0L,
            1L
          ),
          tickers = dplyr::if_else(
            chosen_signals_and_positions[tickers] == "short",
            paste0("low_", tickers),
            tickers,
            missing = tickers
          ),
          id = paste0(tickers, "-", dates),
        ) %>%
        dplyr::select(id, tickers, dates, is_eligible) %>%
        dplyr::arrange(id)

      ##Add a signal themes
      if(!is.null(signal_themes_m_df)){
        if(!any(dplyr::pull(signal_themes_m_df@data, id) %in% dplyr::pull(signal_universe_m_df, id))){
          message("signal_themes_m_df does not contain all the id's in signal_universe_m_df.")
          #Join
          signal_universe_m_df <- signal_universe_m_df %>%
            dplyr::left_join(signal_themes_m_df@data %>% dplyr::select(-tickers, -dates), by = "id")
        }
      }
      ##Add custom signal_universe_metrics_m_df
        ###Check for adequacy of custom_signal_universe_metrics_m_df
        if(!is.null(custom_signal_universe_metrics_m_df)){
          custom_signal_universe_metrics_m_df <- custom_signal_universe_metrics_m_df@data
          ####Coercibility
          if (!is_coercible_to_meta_dataframe(custom_signal_universe_metrics_m_df)){
            stop("custom_signal_universe_metrics_m_df not coercible to meta_dataframe.")
          }
          ####Only numeric
          if (!all(sapply(custom_signal_universe_metrics_m_df %>% dplyr::select(-id, -tickers, -dates), is.numeric))){
            stop("custom_signal_universe_metrics_m_df should only contain numeric values.")
          }
          ####all signal_universe_m_df tickers in custom_signal_universe_metrics
          if (any(!dplyr::pull(signal_universe_m_df, tickers) %in% dplyr::pull(custom_signal_universe_metrics_m_df, tickers))){
            stop ("all signal_universe_m_df tickers should be contemplated in custom_signal_universe_metrics_m_df")
          }
          ####Any NA in custom_signal_universe_m_df
          if (any(is.na(custom_signal_universe_metrics_m_df))){
            stop("custom_signal_universe_metrics_m_df should not contain NA's")
          }
          ####Check if nrows match tickers * dates
          if (custom_signal_universe_metrics_m_df %>% nrow() != length(unique(dplyr::pull(custom_signal_universe_metrics_m_df, tickers))) * length(unique(dplyr::pull(custom_signal_universe_metrics_m_df, dates)))){
            stop("custom_signal_universe_metrics_m_df should have nrows equal to tickers * dates")
          }
          ####Check if first training data is contemplated by custom_signal_universe
          first_training_date <- unique(dplyr::pull(features_m_df@data, dates))[config@training_sample_size + if (!config@sb_algorithm %in% c("ols", "ew", "sw", "mvo", "rp")) config@tuning_strategy@validation_sample_size else 0]
          if (all(first_training_date < unique(dplyr::pull(custom_signal_universe_metrics_m_df, dates)))){
            stop("custom_signal_universe_metrics_m_df should have at least one date before first_training_date")
          }
          ####Check if custom objective is contemplated in custom_signal_universe_metrics
          if (!stringr::str_remove(stringr::str_remove(config@custom_objective, "min_"), "max_") %in% colnames(custom_signal_universe_metrics_m_df)){
            stop("custom_objective not contemplated in custom_signal_universe_metrics_m_df")
          }
          ###Add custom_signal_universe_metrics_m_df to signal_universe_m_df
          signal_universe_m_df <- signal_universe_m_df %>%
            dplyr::left_join(custom_signal_universe_metrics_m_df %>% dplyr::select(-tickers, -dates), by = "id")

            #####Check for NAs and warn the user
            if (any(sapply(signal_universe_m_df, function(x) any(is.na(x))))){
              message("custom_signal_universe_metrics_m_df does not contain data for all dates in features_m_df.\n")
            }

          ###Drop NA rows
          signal_universe_m_df <- signal_universe_m_df %>% tidyr::drop_na()

        }

      if(!is_coercible_to_meta_dataframe(signal_universe_m_df)) stop("signal_universe_m_df not coercible to meta_dataframe.")
    }
  ###########################
  }

  #Get from Signal Selection Results
  ###########################
  else {
    #Check for compatibility between ss and sb objects
    if(!is.null(cov_matrix_benchmark) && ss_backtest_results@ss_backtest_workflow$market_factor_proxy != cov_matrix_benchmark){
      stop("market_factor_proxy and cov_matrix_benchmark in ss_backtest_results differ.")
    }
    if(ss_backtest_results@ss_backtest_workflow$signals_object_name != features_object_name){
      stop("Object names of signals_m_df (ss_backtest) and of features_m_df (sb_backtest) differ.")
    }
    if(!is.null(backtest_returns_m_xts) && ss_backtest_results@ss_backtest_workflow$backtest_returns_object_name != backtest_returns_m_xts@meta_xts_name){
      stop("Object names of backtest_returns_m_xts differ accross ss_backtest_results and sb_backtest.")
    }
    if(!is.null(benchmark_returns_m_xts) && ss_backtest_results@ss_backtest_workflow$benchmark_returns_object_name != benchmark_returns_m_xts@meta_xts_name){
      stop("Object names of benchmark_returns_m_xts differ accross ss_backtest_results and sb_backtest.")
    }
    if(!is.null(signal_themes_m_df) && ss_backtest_results@ss_backtest_workflow$signal_themes_object_name != signal_themes_m_df@meta_dataframe_name){
      stop("Object names of signal_themes_m_df differ accross ss_backtest_results and sb_backtest.")
    }
    if (!is.null(custom_signal_universe_metrics_m_df)){
     warning("custom_signal_universe_metrics_m_df should only be provided when a ss_backtest_results is not given. Ignoring custom_signal_universe_metrics_m_df.")
    }

    ###Extract signal_universe_m_df
    signal_universe_m_df <- ss_backtest_results@signal_universe_m_df@data
    #Extract chosen_signals_and_positions
    chosen_signals_and_positions <- ss_backtest_results@ss_backtest_workflow$chosen_signals_and_positions
  }

  ###########################
  return(list(signal_universe_m_df = signal_universe_m_df,
              chosen_signals_and_positions = chosen_signals_and_positions
              ))


}
