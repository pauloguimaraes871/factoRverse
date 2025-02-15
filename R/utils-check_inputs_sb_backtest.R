#' Perform validation checks on inputs for SB workflow
#'
#' This function validates and checks various inputs required for a signal blending workflow.
#'
#' @param features_m_df A data frame or matrix containing features data.
#' @param target_m_df A data frame or matrix containing target variable data.
#' @param training_sample_size Numeric, size of the training sample.
#' @param target_fwd_name Character, name of the forward looking target.
#' @param validation_sample_size Numeric, size of the validation sample.
#' @param rebalancing_months Numeric, number of months for rebalancing.
#' @param split_method Character, method of data splitting (currently only "expanding" is supported).
#' @param sb_algorithm Character, choice of signal blending algorithm ("ols", "glmnet", "rf", "xgb", "nn", "ew", "sw", "rp", "mvo").
#' @param custom_objective Character, custom objective function for loss.
#' @param chosen_eval_metric Character, chosen evaluation metric ("rmse", "mae", "cp", "rss", "mphe", "mpe", "hr", "mape").
#' @param huber_delta Numeric, delta parameter for Huber loss (for "pseudo_huber_error" custom objective).
#' @param quantile_tau Numeric, tau parameter for quantile loss (for "quantile_error" custom objective).
#' @param hyper_grid_domain_list List, domain list of hyperparameters for tuning.
#' @param tuning_method Character, method of hyperparameter tuning ("random_search", "grid_search", "bayesian_opt").
#' @param n_iter number of iterations for tuning.
#' @param k_iter number of iterations for k-fold cross-validation.
#' @param acq Character, acquisition function for Bayesian optimization ("ucb", "ei", "poi").
#' @param init_points Numeric, number of initial points for Bayesian optimization.
#' @param early_stop Numeric, number of epochs for early stopping (for "xgb" and "nn" algorithms).
#' @param keras_architecture_parameters List, containing units (numeric), n_layers (numeric between 1 and 5), activation_function and nn_optimizer ("Adam" or "RMSProp")
#' @param signal_universe_m_d_ref A data frame containing the signal universe. If provided, data in this object will be updated with posteriors.
#' @param backtest_returns_m_xts A xts containing historical backtested returns named according to signals in `signals_m_df`,
#' @param benchmark_returns_m_xts A xts with benchmark returns, named accordingly.
#' @param show_plots Logical, whether to show diagnostic plots.
#' @param verbose Logical, whether to print verbose output.
#' @param parallel Logical, whether to use parallel computation.
#'
#' @return NULL. This function is used for validation and does not return a value; it stops on errors.
#'
#' @details
#' This function performs comprehensive validation checks on various inputs required for a signal blending workflow.
#' It validates data formats, correctness of hyperparameters, consistency of dates, and other specific requirements for
#' different signal blending algorithms.
#'
#'
#' @export
#'
#' @references
#' For more information on signal blending algorithms and their usage, refer to appropriate documentation.
#'
#' @keywords internal validation machine-learning
#'
#'
check_inputs_sb_backtest <- function(
    #Data
  features_m_df, target_m_df, training_sample_size, target_fwd_name,
  #Time
  validation_sample_size, rebalancing_months, split_method,
  #SB heuristic
  signal_universe_m_df, backtest_returns_m_xts, benchmark_returns_m_xts, cov_matrix_benchmark,
  cov_matrix_sample_size, cov_estimation_method, active_returns, signal_themes_m_df,
  rp_method, n_random_ports, random_ports_method, opt_objective, concentration_constraint_policy,
  custom_signal_weights_m_df,
  #SB algorithm
  sb_algorithm, gsm_algorithm, custom_objective, chosen_eval_metric, huber_delta, quantile_tau,
  #Tuning
  hyper_grid_domain_list, tuning_method, n_iter, k_iter, acq, init_points,
  #Etc
  early_stop, keras_architecture_parameters, parallel, verbose = TRUE, .test_seed
){

  ###Initial Checks###
  ################

  #Structures
  ############################################################################################

  #Check for correct format in features_m_df
  if(!(is_coercible_to_meta_dataframe(features_m_df))){
    stop("features_m_df should be coercible to meta_dataframe object")
  }

  if(!all(sapply(features_m_df[,-c(1:3)], function(x) is.numeric(x) && all(!is.na(x))))){
    stop("features_m_df should contain only numeric columns with non-NAs.")
  }

  if(all(any(!lubridate::is.Date(features_m_df %>% dplyr::pull(dates))) ||
         any(is.na(as.Date(features_m_df %>% dplyr::pull(dates), format = "%Y-%m-%d", tryFormats = c("%Y-%m-%d")))))){
    stop("dates in features_m_df must be a date object with format %Y-%m-%d.")
  }

  #Check for presence of low
  if(any(grepl("low_", colnames(features_m_df)))){
    stop("features_m_df column names should not contain 'low_'.")
  }

  #Check structure of rebalancing_months
  if(!is.numeric(rebalancing_months)){
    stop("rebalancing_months should be numeric.")
  }

  if(rebalancing_months < 0 || rebalancing_months > 12){
    stop("rebalancing_months should be between 1 and 12.")
  }

  #Check structure of target_fwd_name
  target_fwd_name_right_pattern <- "^[A-Za-z_]+_[0-9]{1,2}m$"

  if(!(is.character(target_fwd_name))){
    stop("target_fwd_name should be character.")
  }

  if(!grepl(target_fwd_name_right_pattern, target_fwd_name)){
    stop("target_fwd_name is not in the right pattern")
  }

  #Check structure of training_sample_size and validation_sample_size
  assumed_target_fwd <- as.numeric(gsub(".*?([0-9]+).*", "\\1", target_fwd_name))

  if(!(is.numeric(training_sample_size))){
    stop("training_sample_size should be numeric.")
  }

  if((training_sample_size <= assumed_target_fwd)){
    stop("training_sample_size should be bigger than target_fwd")
  }

  if((training_sample_size < 0)){
    stop("training_sample_size should be positive.")
  }

  if(!(is.numeric(validation_sample_size))){
    stop("validation_sample_size should be numeric.")
  }

  if(validation_sample_size < 0){
    stop("validation_sample_size should be positive.")
  }

  if (!sb_algorithm %in% c("ols", "ew", "sw", "rp", "mvo", "custom_weights") && ((training_sample_size + validation_sample_size - assumed_target_fwd) < training_sample_size)){
    stop("training_sample_size should be bigger than training_sample_size + validation_sample_size - target_fwd")
  }

  if(sb_algorithm %in% c("ols", "ew", "sw", "rp", "mvo") & validation_sample_size != 0){
    stop("ols and heuristic sb algorithms do not support validation split.")
  }

  dates_m_vector <- unique(as.Date(features_m_df %>% dplyr::pull(dates), format = "%Y-%m-%d"))
  if(length(dates_m_vector) < (training_sample_size + validation_sample_size)){
    stop("training_sample_size plus validation_sample_size should be less than the number of unique dates in features_m_df.")
  }

  #Check structure of split_method
  if(split_method != "expanding"){
    stop("split_method should be expanding.")
  }

  #gsm
  if(!gsm_algorithm %in% c("ols", "tree")){
    stop("gsm_algorithm should be either 'ols' or 'tree'.")
  }


  #Check for correct format in target_m_df
  if(!(is_coercible_to_meta_dataframe(target_m_df))){
    stop("target_m_df should be coercible to meta_dataframe object")
  }

  #Check if target_fwd_name is in target_m_df
  if(!(target_fwd_name %in% colnames(target_m_df))){
    stop("target_fwd_name is not in target_m_df")
  }

  if(any(!grepl(target_fwd_name_right_pattern, colnames(target_m_df[,-c(1:3)])))){
    stop("target_m_df colnames should follow the format XXXX_number_m, where ' XXXX is the name of the target variable, number is the amount of forward periods and m indicates periods are measured in months.")
  }

  if(all(any(!lubridate::is.Date(target_m_df %>% dplyr::pull(dates))) ||
         any(is.na(as.Date(target_m_df %>% dplyr::pull(dates), format = "%Y-%m-%d", tryFormats = c("%Y-%m-%d")))))){
    stop("dates in target_m_df must be a date object with format %Y-%m-%d.")
  }

  #Check for only NAs in first rebalancing period
  if (all(is.na(target_m_df %>% dplyr::filter(dates %in% unique(target_m_df %>% dplyr::pull(dates))[1:training_sample_size+validation_sample_size]) %>%
                dplyr::pull(target_fwd_name)))){
    stop("target_m_df can't have only NAs in the first rebalancing period")
  }

  #Get dates allowed to be NA
  dates_allowed_to_be_NA_in_target_m_df <- unique(target_m_df %>% dplyr::pull(dates))[(length(unique(target_m_df %>% dplyr::pull(dates))) - assumed_target_fwd + 1):length(unique(target_m_df %>% dplyr::pull(dates)))]
  if(length(dates_allowed_to_be_NA_in_target_m_df) > assumed_target_fwd){
    stop("number of dates in target_m_df with NAs should be at most equal to prediction horizon")
  }
  if(any(is.na(target_m_df[-which(target_m_df %>% dplyr::pull(dates) %in% dates_allowed_to_be_NA_in_target_m_df),target_fwd_name]))){
    stop("target_m_df can't have NAs until the last target_fwd periods")
  }

  #Get dates with effective NAs
  dates_allowed_to_be_NA_but_are_not_na <- target_m_df %>%
    dplyr::filter(dates %in% dates_allowed_to_be_NA_in_target_m_df, !is.na(!!rlang::sym(target_fwd_name))) %>%
    dplyr::pull(dates) %>%
    unique()

  dates_allowed_to_be_NA_and_really_are_na <- as.Date(setdiff(dates_allowed_to_be_NA_in_target_m_df, dates_allowed_to_be_NA_but_are_not_na))

  if(all(length(dates_allowed_to_be_NA_and_really_are_na) != 0, verbose)){
    message("The following dates from features_m_df contemplate NA rows in target_m_df: ", paste(dates_allowed_to_be_NA_and_really_are_na, collapse = " "))
  }
  if(all(length(dates_allowed_to_be_NA_but_are_not_na) != 0, verbose)){
    message("The following final dates from target_m_df are expected to be NA in an up-to-date backtest, but are not: ", paste(dates_allowed_to_be_NA_but_are_not_na, collapse = " "))
  }

  #Check structure between target_m_df and feature_m_df
  if(nrow(target_m_df) != nrow(features_m_df)){
    stop("features_m_df and target_m_df must possess same number of rows.")
  }

  if(any(target_m_df %>% dplyr::pull(id) != features_m_df %>% dplyr::pull(id))){
    stop("id in features_m_df and in target_m_df must match.")
  }

  if(any(target_m_df %>% dplyr::pull(tickers) != features_m_df %>% dplyr::pull(tickers))){
    stop("tickers in features_m_df and in target_m_df must match.")
  }

  if(any(target_m_df %>% dplyr::pull(dates) != features_m_df %>% dplyr::pull(dates))){
    stop("dates in features_m_df and in target_m_df must match.")
  }

  if(nrow(target_m_df) < assumed_target_fwd || nrow(features_m_df) < assumed_target_fwd){
    stop("target_m_df and features_m_df should have more dates than the prediction horizon")
  }

  if(length(target_m_df %>% dplyr::pull(dates) %>% unique()) <= assumed_target_fwd || length(features_m_df %>% dplyr::pull(dates) %>% unique()) <= assumed_target_fwd){
    stop("target_m_df and features_m_df should have more dates than the prediction horizon")
  }


  #Check structure of signal_universe_m_df
  if(!(is_coercible_to_meta_dataframe(signal_universe_m_df))){
    stop("signal_universe_m_df should be coercible to meta_dataframe object")
  }
  ##Check for existence and NAs in heuristic sb metric
  if (sb_algorithm %in% c("sw", "mvo")){
    if (!stringr::str_remove_all(custom_objective, "max_") %>% stringr::str_remove_all("min_") %in% colnames(signal_universe_m_df)){
      stop("Heuristic Signal Blending Metric not found in signal_universe_m_df")
    }
    heuristic_sb_metric <- signal_universe_m_df %>% dplyr::pull(stringr::str_remove_all(custom_objective, "max_") %>% stringr::str_remove_all("min_"))
    if (any(is.na(heuristic_sb_metric))){
      stop("Heuristic Signal Blending Metric contains NAs")
    }
  }

  #backtest_returns_m_xts
  if(sb_algorithm %in% c("rp", "mvo") && is.null(backtest_returns_m_xts)){
    stop("backtest_returns_m_xts are strictly needed when sb_algorithm is either rp or mvo.")
  }
  if(!is.null(backtest_returns_m_xts)){
    if(!xts::is.xts(backtest_returns_m_xts)){
      stop("backtest_returns_m_xts must be a xts object")
    }
    #get dates
    backtest_returns_dates <- zoo::index(backtest_returns_m_xts)

    if(class(backtest_returns_dates) != "Date"){
      stop("dates in backtest_returns_m_xts must be of class Date")
    }

    if(any(apply(backtest_returns_m_xts, 2, function(x) any(is.na(x))))){
      stop("backtest_returns_m_xts must not have any NA")
    }

    if(nrow(backtest_returns_m_xts) < (training_sample_size + validation_sample_size)){
      stop("backtest_returns_m_xts must have at least training_sample_size + validation_sample_size rows")
    }

    if(any(!signal_universe_m_df %>% dplyr::pull(dates) %in% backtest_returns_dates)){
      stop("all dates in signal_universe_m_df must be present in backtest_returns_m_xts")
    }

    if(any(!unique(dplyr::pull(features_m_df, dates))[-c(1:(training_sample_size + validation_sample_size))] %in% backtest_returns_dates)){
      stop("all backtest_dates derived from features_m_df must be present in backtest_returns_m_xts")
    }

    backtest_returns_dates_before_first_training <- backtest_returns_dates[which(backtest_returns_dates < unique(dplyr::pull(features_m_df, dates))[training_sample_size + validation_sample_size])]

    if (length(backtest_returns_dates_before_first_training) < 2) {
      stop("There is only one date in backtest_returns_m_xts before the first training date")
    }

    if(!all(diff(as.numeric(format(backtest_returns_dates, "%Y")) * 12 + as.numeric(format(backtest_returns_dates, "%m"))) == 1)){
      stop("backtest_returns_m_xts must have consecutive dates")
    }

    if(any(seq.Date(from = backtest_returns_dates[1], to = backtest_returns_dates[length(backtest_returns_dates)], by = "month") != backtest_returns_dates)){
      stop("backtest_returns_m_xts must have sequential monthly dates")
    }

    if(sb_algorithm %in% c("rp", "mvo") && length(backtest_returns_dates) <= cov_matrix_sample_size){
      stop("backtest_returns_m_xts must have more dates than cov_matrix_sample_size")
    }
  }

  #benchmark_returns_m_xts
  if(sb_algorithm %in% c("rp", "mvo") && active_returns && is.null(benchmark_returns_m_xts)){
    stop("benchmark_returns_m_xts are strictly needed when sb_algorithm is either rp or mvo and active_returns is set to TRUE.")
  }
  if(!is.null(benchmark_returns_m_xts)){
    if(!xts::is.xts(benchmark_returns_m_xts)){
      stop("benchmark_returns_m_xts must be a xts object")
    }
    #get dates
    benchmark_returns_dates <- zoo::index(benchmark_returns_m_xts)
    if(class(benchmark_returns_dates) != "Date"){
      stop("dates in benchmark_returns_m_xts must be of class Date")
    }

    if(any(!benchmark_returns_dates %in% backtest_returns_dates)){
      stop("dates in benchmark_returns_m_xts and backtest_returns_m_xts must be the same")
    }

    if(any(!backtest_returns_dates %in% benchmark_returns_dates)){
      stop("dates in benchmark_returns_m_xts and backtest_returns_m_xts must be the same")
    }

    if(any(apply(benchmark_returns_m_xts, 2, function(x) all(is.na(x))))){
      stop("benchmark_returns_m_xts must not have any NA values")
    }

    if(!all(diff(as.numeric(format(benchmark_returns_dates, "%Y")) * 12 +
                 as.numeric(format(benchmark_returns_dates, "%m"))) == 1)){
      stop("benchmark_returns_m_xts must have consecutive dates")
    }

    if(sb_algorithm %in% c("rp", "mvo") && !cov_matrix_benchmark %in% colnames(benchmark_returns_m_xts)){
      stop("cov_matrix_benchmark must be present in benchmark_returns_m_xts")
    }
  }

  #signal_themes_m_df
  if(!sb_algorithm %in% c("rp", "mvo") && !is.null(signal_themes_m_df)){
    stop("signal_themes_m_df is only needed when sb_algorithm is either rp or mvo.")
  }

  #concentration_constraint_policy validity
  if (!is.null(concentration_constraint_policy)){
    validate_concentration_constraint_policy(concentration_constraint_policy)
  }

  ##Accordance with concentration_constraint_policy
  if(!is.null(concentration_constraint_policy$max_abs_active_group_weight)){
    ##null
    if(is.null(signal_themes_m_df)){
      stop("signal_themes_m_df must be provided if max_abs_active_group_weight is given.")
    }
    #Check if all themes are contenmplated by eligible signals
    ##Get eligible signals and themes
    eligible_signals <- signal_universe_m_df %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers) %>% unique()
    eligible_themes <- signal_themes_m_df %>% dplyr::filter(tickers %in% eligible_signals) %>% dplyr::pull(theme) %>% unique()

    ##Check if all themes are present in eligible signals
    if (length(setdiff(unique(signal_themes_m_df$theme), eligible_themes)) > 0) {
      stop("All themes in signal_themes_m_df must be present in eligible signals when max_abs_active_group_weight is given.
                 Running run_ss_backtest with enable_theme_representativeness as TRUE may help.")
    }
  }


  if(!is.null(signal_themes_m_df)){
    ##Check if signal_themes_m_df contemplates theme column
    if(any(!colnames(signal_themes_m_df) == c("id", "tickers", "dates", "theme"))){
      stop("signal_themes_m_df must have columns 'id', 'tickers', 'dates' and 'theme'")
    }

    ##Check if theme column is character
    if(!is.character(signal_themes_m_df %>% dplyr::pull(theme))){
      stop("theme column in signal_themes_m_df must be character")
    }

    ##Check format in signal_themes_m_df
    if(any(grepl("_", signal_themes_m_df %>% dplyr::pull(theme)))){
      stop("No underscores allowed in signal_themes_m_df theme names")
    }

    ##Check if dates in signal_themes_m_df and signals_m_df are the same
    dates_m_vector <- as.Date(unique(features_m_df %>% dplyr::pull(dates)))
    signal_themes_dates_m_vector <- as.Date(unique(signal_themes_m_df %>% dplyr::pull(dates)))
    if(any(!dates_m_vector %in% signal_themes_dates_m_vector)){
      stop("dates in signal_themes_m_df and features_m_df must be the same")
    }

    ##Check if there is a theme classification for every date
    if(any(!signal_universe_m_df$id %in% signal_themes_m_df$id)){
      stop("all ids in signal_universe_m_df must have a theme classification")
    }

    ###Check for NAs
    if (any(is.na(signal_themes_m_df))){
      stop("signal_themes_m_df should not have NAs")
    }
  }

  #custom_signal_weights_m_df
  if(sb_algorithm == "custom_weights" && is.null(custom_signal_weights_m_df)){
    stop("custom_signal_weights_m_df must be provided if sb_algorithm is custom_weights.")
  }
  if(!is.null(custom_signal_weights_m_df)){
    ##Check coercibility
    if(!is_coercible_to_meta_dataframe(custom_signal_weights_m_df)){
      stop("custom_signal_weights_m_df is not coercible to meta dataframe")
    }

    ##Check if there is a weight column
    if(!"weights" == colnames(custom_signal_weights_m_df)[4]){
      stop("custom_signal_weights_m_df must have a 'weights' column")
    }
    ##Check if weight column is numeric
    if(!is.numeric(custom_signal_weights_m_df %>% dplyr::pull(weights))){
      stop("weights column in custom_signal_weights_m_df must be numeric")
    }
    ##Check if ids in custom_signal_weights_m_df and signal_universe_m_df are the same
    if(any(!(signal_universe_m_df %>% dplyr::pull(id)) %in% (custom_signal_weights_m_df %>% dplyr::pull(id)))){
      stop("all ids in signal_universe_m_df should have a correspondence in custom_signal_weights_m_df")
    }
    ##Check if any weight belong to a non-eligible ticker
    non_zero_weight_id <- custom_signal_weights_m_df %>% dplyr::filter(weights != 0) %>% dplyr::pull(id)
    non_eligible_id <- signal_universe_m_df %>% dplyr::filter(is_eligible == 0) %>% dplyr::pull(id)
    if(any(non_zero_weight_id %in% non_eligible_id)){
      message("Some ids in custom_signal_weights_m_df are not eligible: ",
              paste(non_zero_weight_id[non_zero_weight_id %in% non_eligible_id], collapse = ", "))
    }

    ##Check if non_zero_weight_tickers match features_m_df
    non_zero_weight_signals <- custom_signal_weights_m_df %>% dplyr::filter(weights != 0) %>% dplyr::pull(tickers)
    check_signal_presence <- !stringr::str_remove_all(non_zero_weight_signals, pattern = "low_") %in% colnames(features_m_df)
    if (any(check_signal_presence)) {
      stop("There is a signal mismatch between non zero-weight signals in custom_signal_weights_m_df and features_m_df: ",
           paste(non_zero_weight_signals[check_signal_presence], collapse = ", ")
      )
    }

    #Check if weights sum to 1
    custom_signal_weights_m_df %>%
      dplyr::group_by(dates) %>%
      dplyr::summarise(sum_w = sum(weights)) %>%
      dplyr::mutate(check_sum_1 = abs(sum_w - 1) < 0.02)
    if(any(custom_signal_weights_m_df$check_sum_1 == FALSE)){
      stop(paste("Weights do not sum to 1 at dates:", custom_signal_weights_m_df$dates[which(custom_signal_weights_m_df$check_sum_1 == FALSE)], collapse = ", "))
    }

  }





  #####################################################################################

  #Check for eligible signals
  signal_universe_m_df_dates <- signal_universe_m_df %>% dplyr::pull(dates) %>% unique()
  first_training_date <- dates_m_vector[training_sample_size]

  ##First date of eligible_signals should be before first training date
  if(first_training_date < min(signal_universe_m_df_dates)){
    stop("First date of signal_universe_m_df should be before first training date.")
  }

  ###Get elected signals
  eligible_signals <- signal_universe_m_df %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(tickers) %>% unique()
  ##Check signal presence in features_m_df
  check_eligible_signals_presence_in_features_m_df <- !stringr::str_remove_all(eligible_signals, pattern = "low_") %in% colnames(features_m_df)
  if (any(check_eligible_signals_presence_in_features_m_df)) {
    stop("There are eligible signals not present in features_m_df: ",
         paste(stringr::str_remove_all(eligible_signals, pattern = "low_")[check_eligible_signals_presence_in_features_m_df], collapse = ", ")
    )
  }

  ##Get elected ids
  eligible_ids <- signal_universe_m_df %>% dplyr::filter(is_eligible == 1) %>% dplyr::pull(id)
  ##Check signal presence in signal_themes_m_df
  if(!is.null(signal_themes_m_df)){
    check_id_presence <- !eligible_ids %in% (signal_themes_m_df %>% dplyr::pull(id))
    if (any(check_id_presence)) {
      stop("There is a id mismatch between eligible_ids and signal_themes_m_df: ",
           paste(eligible_ids[check_id_presence], collapse = ", ")
      )
    }

    ###Get all ids
    #For signal_themes, it is important for one to have all signals in signal_themes_m_df. This is because of benchmark group weights calculation.
    all_ids <- signal_universe_m_df %>% dplyr::pull(id)
    check_id_presence <- !all_ids %in% (signal_themes_m_df %>% dplyr::pull(id))
    if (any(check_id_presence)) {
      stop("There is a signal mismatch between ids (eligible or not) and signal_themes_m_df: ",
           paste(all_ids[check_id_presence], collapse = ", ")
      )
    }
  }

  ##Check signal presence in backtest_returns_m_xts
  if(!is.null(backtest_returns_m_xts)){
    check_signal_presence <- !eligible_signals %in% colnames(backtest_returns_m_xts)
    if (any(check_signal_presence)) {
      stop("There is a signal mismatch between eligible_signals and backtest_returns_m_xts: ",
           paste(eligible_signals[check_signal_presence], collapse = ", ")
      )
    }
  }

  ##Check signal presence in custom signal weights
  if(!is.null(custom_signal_weights_m_df)){
    all_ids <- signal_universe_m_df %>% dplyr::pull(id)
    check_id_presence <- !all_ids %in% (custom_signal_weights_m_df %>% dplyr::pull(id))
    if (any(check_id_presence)) {
      stop("There is a signal mismatch between ids (eligible or not) and custom_signal_weights_m_df: ",
           paste(all_ids[check_id_presence], collapse = ", ")
      )
    }
  }

  ##Check if all is eligible while method is ew
  if(sb_algorithm == "ew" && !any(stringr::str_detect(signal_universe_m_df$tickers, "low_"))){
    warning("All signals are 'long' and sb_algorithm is 'ew'. Please check if this is intended.")
  }

  #Validation Schema
  #Check for correct choice in chosen_eval_metric
  if(!is.null(chosen_eval_metric)){
    if(!chosen_eval_metric %in% c("rmse", "mae", "cp", "rss", "mphe", "mpe", "hr", "mape")){
      stop("chosen_eval_metric choice not supported.")
    }
  }

  #Check for correct format of custom_eval/loss parameters
  if(quantile_tau <= 0 || quantile_tau >= 1){
    stop("quantile_tau should be > 0 and less than 1.")
  }

  if(!is.numeric(huber_delta)){
    stop("huber_delta should be numeric.")
  }



  #Check for correct hyperparameters names in hyper_grid_domain_list
  if(sb_algorithm %in% c("ols", "ew", "sw", "rp", "mvo", "custom_weights") & !is.null(hyper_grid_domain_list)){
    stop("ols and heuristic sb algorithms do not support hyperparameters.")
  }


  if(!sb_algorithm %in% c("ols", "ew", "sw", "rp", "mvo", "custom_weights") & is.null(hyper_grid_domain_list)){
    stop("hyper_grid_domain must be set when sb_algorithm is different from ols.")
  }


  #GLMNET
  if(sb_algorithm == "glmnet" && !all(names(hyper_grid_domain_list) == c("alpha", "lambda.min.ratio"))){
    stop("hyperparameters do not match sb_algorithm choice")
  }

  #RF
  if(sb_algorithm == "rf" && !all(names(hyper_grid_domain_list) == c("mtry", "num.trees", "max.depth", "min.bucket"))){
    stop("hyperparameters do not match sb_algorithm choice")
  }

  #XGB
  if(sb_algorithm == "xgb" && !all(names(hyper_grid_domain_list) == c("min_child_weight", "max_depth", "subsample", "colsample_bytree",
                                                                      "eta", "alpha", "gamma", "nrounds"))){
    stop("hyperparameters do not match sb_algorithm choice")
  } else {}

  #NN
  if(sb_algorithm == "nn" && !all(names(hyper_grid_domain_list) == c("regularizer_l1", "regularizer_l2", "droprate", "lr", "size_of_batch", "number_of_epochs"))){
    stop("hyperparameters do not match sb_algorithm choice")
  } else {}



  #Check for valid format in tuning method
  if(!sb_algorithm %in% c("ols", "ew", "sw", "rp", "mvo", "custom_weights") && !tuning_method %in% c("random_search", "grid_search", "bayesian_opt")){
    stop("tuning_method should be one of random_search, grid_search or bayesian_opt.")
  }

  #Check for correct format in case tuning method is grid_search
  if(!sb_algorithm %in% c("ols", "ew", "sw", "rp", "mvo", "custom_weights") && tuning_method == c("grid_search")){
    if(any(
      #Check if hyper_grid_domain_list is a list
      !(class(hyper_grid_domain_list) == "list"),
      #Check if hyper_grid_domain_list is a list of vectors
      !all(sapply(hyper_grid_domain_list, function(x) is.vector(x))),
      #Check if hyper_grid_domain_list contains numeric values
      !all(sapply(hyper_grid_domain_list, function(x) is.numeric(x)))
    )
    ){
      stop("hyper_grid_domain_list not in correct format for grid_search tuning.")
    }
  }

  if(all(!sb_algorithm %in% c("ols", "ew", "sw", "rp", "mvo", "custom_weights"), tuning_method == "grid_search",!is.null(n_iter))){
    warning("When tuning_method is grid_search, hyperparameters are combined exhaustively. Ignoring any user set n_iter value")
  }

  #Check for correct format in case tuning method is random_search
  if(!sb_algorithm %in% c("ols", "ew", "sw", "rp", "mvo", "custom_weights") && tuning_method == c("random_search")){


    tryCatch({
      if(any(
        #Check if hyper_grid_domain_list is a list
        !(class(hyper_grid_domain_list) == "list"),
        #Check if hyper_grid_domain_list is a list of lists
        !all(sapply(hyper_grid_domain_list, function(x) is.list(x))),
        #Check if every element contains data for distribution choice and pars
        !all(sapply(hyper_grid_domain_list, function(x) names(x) %in% c("distribution_choice", "pars", "value"))),
        #Check if distribution choices match allowed choices
        !all(sapply(hyper_grid_domain_list, function(x) all(x$distribution_choice %in% c("normal", "uniform", "lognormal", "constant")))),
        #Check if pars are numeric and not NA
        !all(sapply(hyper_grid_domain_list, function(x) all(is.numeric(x$pars) | is.numeric(x$value), !is.na(x$pars) | !is.na(x$value)))),
        #Check if pars are named
        !all(sapply(hyper_grid_domain_list, function(x) ifelse(x$distribution_choice != "constant", all(!is.null(names(x$pars))), any(names(x) %in% c("value"))))),
        #Check if pars match each possible distribution choice
        !all(sapply(hyper_grid_domain_list, function(x) ifelse(x$distribution_choice == "uniform", all(names(x$pars) == c("min", "max")),
                                                               ifelse(x$distribution_choice == "normal", all(names(x$pars) == c("mean", "sd")),
                                                                      ifelse(x$distribution_choice == "lognormal", all(names(x$pars) == c("meanlog", "sdlog")),
                                                                             is.numeric(x$value))))))
      )
      ){
        stop("hyper_grid_domain_list not in correct format for random_search tuning.")
      }

    }, error = function(e){
      stop("hyper_grid_domain_list not in correct format for random_search tuning.")
    })


    if(!is.numeric(n_iter)){
      stop("n_iter must be numeric.")
    }

  }



  #Check for correct format in case tuning method is Bayesian Optimization
  if(!sb_algorithm %in% c("ols", "ew", "sw", "rp", "mvo", "custom_weights") && tuning_method == c("bayesian_opt")){
    if(any(
      #Check if hyper_grid_domain_list is a list
      !is.list(hyper_grid_domain_list),
      #Check if hyper_grid_domain_list elements have length of 2 (boundaries)
      !all(sapply(hyper_grid_domain_list, function(x) length(x) == 2)),
      #Check if hyper_grid_domain_list elements are vectors
      !all(sapply(hyper_grid_domain_list, function(x) is.vector(x))),
      #Check if hyper_grid_domain_list contains numeric values
      !all(sapply(hyper_grid_domain_list, function(x) is.numeric(x)))
    )
    ){
      stop("hyper_grid_domain_list not in correct format for bayesian_opt tuning.")
    }

    if(!acq %in% c("ucb", "ei", "poi")){
      stop("acq should be one of ucb, ei or poi")
    }
    if(any(!is.numeric(init_points), !is.numeric(n_iter), !is.numeric(k_iter))){
      stop("n_iter, k_iter and init_points must be numeric.")
    }
    if(init_points <= length(hyper_grid_domain_list)){
      stop("init_points must be greater than number of hyperparameters")
    }
    if(n_iter < k_iter){
      stop("n_iter must be greater than k_iter")
    }

  }

  #SB algorithms
  ################
  #Check for correct choice in sb_algorithm
  if(!sb_algorithm %in% c("ols", "glmnet", "rf", "xgb", "nn", "ew", "sw", "rp", "mvo", "custom_weights")){
    stop("sb_algorithm choice not supported.")
  }

  #Check for correct choice in custom_objective
  if(all(!sb_algorithm %in% c("xgb", "nn", "sw", "mvo", "custom_weights") && custom_objective != "squared_error")){
    stop("Custom objective functions are only allowed for xgb, nn, sw or mvo sb_algorithm choices")
  }

  #Check for custom objective
  if(sb_algorithm %in% c("sw", "mvo")){
    if (!custom_objective %>% stringr::str_remove("max_") %>% stringr::str_remove("min_") %in% colnames(signal_universe_m_df)){
      stop("Invalid custom_objective. Should be 'max_' or 'min_' + one of columns in signal_universe_m_df (heuristic performance metrics).
                 To see complete list of base valid heuristic performance metrics, run 'display_valid_custom_objectives()'.")
    }
  } else {
    if (!is.null(custom_objective) && !(custom_objective %in% c("squared_error", "pseudo_huber_error", "absolute_error"))) {
      stop("Invalid custom_objective. Choose from 'squared_error', 'pseudo_huber_error', or 'absolute_error'.")
    }
  }


  #Check for correct choice in early_stop
  if(all(!is.null(early_stop), !sb_algorithm %in% c("xgb", "nn"))){
    stop("Early stop only allowed for xgb or nn sb_algorithm choices")
  }

  #Check for NN
  if(sb_algorithm == "nn"){

    if(is.data.frame(keras_architecture_parameters) || !is.list(keras_architecture_parameters) ||
       !all(names(keras_architecture_parameters) == c("units", "n_layers", "activation", "nn_optimizer", "batch_norm_option"))){
      stop("keras_architecture_parameters should be a list with units, n_layers, activation, nn_optimizer and batch_norm_option elements")
    }

    if(!all(is.numeric(keras_architecture_parameters$units))){
      stop("units should be numeric")
    }

    if(!keras_architecture_parameters$n_layers %in% c(1,2,3,4,5) || length(keras_architecture_parameters$n_layers) > 1){
      stop("n_layers should be an integer between 1 and 5.")
    }

    if(!all(keras_architecture_parameters$activation %in% c("relu", "sigmoid", "softmax", "softplus", "tanh", "leaky_relu"))){
      stop("activation should be one of relu, sigmoid, softmax, softplus, tanh or leaky_relu.")
    }

    if(length(keras_architecture_parameters$units) != keras_architecture_parameters$n_layers ||
       length(keras_architecture_parameters$activation) != keras_architecture_parameters$n_layers ||
       length(keras_architecture_parameters$batch_norm_option) != keras_architecture_parameters$n_layers
    ){
      stop("length of units, activation and batch_norm_option should match n_layers")
    }

    if(!keras_architecture_parameters$nn_optimizer %in% c("Adam", "RMSProp")){
      stop("nn_optimizer should be Adam or RMSProp.")
    }


    if(!all(is.logical(keras_architecture_parameters$batch_norm_option))){
      stop("batch_norm_option should be logical")
    }

    if(parallel){
      warning("keras models have some limitations regarding parallel computations. Use with care.")
    }

  }

  #Check for RP and MVO
  if(sb_algorithm %in% c("rp", "mvo")){

    if(is.null(cov_estimation_method)){
      stop("cov_estimation_method should be set for rp and mvo algorithms")
    }
    if(!is.numeric(cov_matrix_sample_size)){
      stop("cov_matrix_sample_size should be numeric")
    }

    if(cov_matrix_sample_size > training_sample_size){
      stop("cov_matrix_sample_size should be smaller than or equal to training_sample_size")
    }

    if(!is.logical(active_returns)){
      stop("active_returns should be logical")
    }
    if(active_returns & is.null(cov_matrix_benchmark)){
      stop("cov_matrix_benchmark should be set if active_returns is TRUE")
    }

    if(sb_algorithm == "rp"){
      if(!rp_method ==  "cyclical-spinu"){
        stop("rp_method should be set to cyclical-spinu for rp algorithm")
      }
    } else {
      if(!is.numeric(n_random_ports)){
        stop("n_random_ports should be numeric")
      }
      if(!random_ports_method %in% c("sample", "simplex", "grid")){
        stop("random_ports_method should be set to sample, simplex or grid")
      }
      if(!opt_objective %in% c("return", "risk", "sharpe")){
        stop("opt_objective should be set to return, risk or sharpe")
      }
      if(length(concentration_constraint_policy) > 0 && !concentration_constraint_policy$benchmark %in% c("theme_sb", "theme_ss")){
        stop("concentration_constraint_policy's benchmark should be set to theme_sb or theme_ss")
      }
      if(length(concentration_constraint_policy) > 0 && !concentration_constraint_policy$benchmark %in% stringr::str_remove(colnames(signal_universe_m_df), "_bench_weights")){
        stop("concentration_constraint_policy's benchmark should be present in signal_universe_m_df")
      }

    }


  }

  ################


  #Hyper domain
  ##################
  #Check for correct domains in hyper_grid_domain_list

  #GLMNET
  ###############
  if(sb_algorithm == "glmnet"){
    #alpha
    ##########
    if(tuning_method == "random_search"){
      if(hyper_grid_domain_list$alpha$distribution_choice == "constant"){
        hyper_domain <- hyper_grid_domain_list$alpha$value
      } else {
        #in case of random
        hyper_domain <- range(hyper_grid_domain_list$alpha$pars)
      }
    } else {
      #bayesian opt or grid search
      hyper_domain <- hyper_grid_domain_list$alpha
    }
    #Check domain
    if(!all(0 <= hyper_domain, hyper_domain <= 1)){
      stop("alpha should be set in interval [0,1]")
    }
    ##########

    #lambda.min.ratio
    ##########
    if(tuning_method == "random_search"){
      if(hyper_grid_domain_list$lambda.min.ratio$distribution_choice == "constant"){
        hyper_domain <- hyper_grid_domain_list$lambda.min.ratio$value
      } else {
        #in case of random
        hyper_domain <- range(hyper_grid_domain_list$lambda.min.ratio$pars)
      }
    } else {
      #bayesian opt or grid search
      hyper_domain <- hyper_grid_domain_list$lambda.min.ratio
    }
    #Check domain
    if(!all(0 <= hyper_domain, hyper_domain < 1)){
      stop("lambda.min.ratio should be set in interval [0,1)")
    }
    ##########
  }
  ###############

  #RF
  ###############
  if(sb_algorithm == "rf"){
    #num.trees
    ##########
    if(tuning_method == "random_search"){
      if(hyper_grid_domain_list$num.trees$distribution_choice == "constant"){
        hyper_domain <- hyper_grid_domain_list$num.trees$value
      } else {
        #in case of random
        hyper_domain <- range(hyper_grid_domain_list$num.trees$pars)
      }
    } else {
      #bayesian opt or grid search
      hyper_domain <- hyper_grid_domain_list$num.trees
    }
    #Check domain
    if(tuning_method == "grid_search"){
      if(!all(hyper_domain == floor(hyper_domain))){
        stop("num.trees should have no decimals")
      }
    } else {
      if(!all(is.integer(hyper_domain))){
        stop("num.trees should be integer")
      }
    }

    if(!all(hyper_domain > 0)){
      stop("num.trees should be positive")
    }
    ##########

    #mtry
    ##########
    if(tuning_method == "random_search"){
      if(hyper_grid_domain_list$mtry$distribution_choice == "constant"){
        hyper_domain <- hyper_grid_domain_list$mtry$value
      } else {
        #in case of random
        hyper_domain <- range(hyper_grid_domain_list$mtry$pars)
      }
    } else {
      #bayesian opt or grid search
      hyper_domain <- hyper_grid_domain_list$mtry
    }
    #Check domain
    if(!all(0 <= hyper_domain, hyper_domain <= 1)){
      stop("mtry should be set in interval [0,1]")
    } else {}
    ##########

    #max.depth
    ##########
    if(tuning_method == "random_search"){
      if(hyper_grid_domain_list$max.depth$distribution_choice == "constant"){
        hyper_domain <- hyper_grid_domain_list$max.depth$value
      } else {
        #in case of random
        hyper_domain <- range(hyper_grid_domain_list$max.depth$pars)
      }
    } else {
      #bayesian opt or grid search
      hyper_domain <- hyper_grid_domain_list$max.depth
    }
    #Check domain
    if(tuning_method == "grid_search"){
      if(!all(hyper_domain == floor(hyper_domain))){
        stop("max.depth should have no decimals")
      }
    } else {
      if(!all(is.integer(hyper_domain))){
        stop("max.depth should be integer")
      } else {}
    }
    if(!all(hyper_domain > 0)){
      stop("max.depth should be positive")
    } else {}
    ##########

  }
  ###############

  #XGB
  ###############
  if(sb_algorithm == "xgb"){
    #eta
    ##########
    if(tuning_method == "random_search"){
      if(hyper_grid_domain_list$eta$distribution_choice == "constant"){
        hyper_domain <- hyper_grid_domain_list$eta$value
      } else {
        #in case of random
        hyper_domain <- range(hyper_grid_domain_list$eta$pars)
      }
    } else {
      #bayesian opt or grid search
      hyper_domain <- hyper_grid_domain_list$eta
    }
    #Check domain
    if(!all(0 <= hyper_domain, hyper_domain <= 1)){
      stop("eta should be set in interval [0,1]")
    } else {}
    ##########

    #max_depth
    ##########
    if(tuning_method == "random_search"){
      if(hyper_grid_domain_list$max_depth$distribution_choice == "constant"){
        hyper_domain <- hyper_grid_domain_list$max_depth$value
      } else {
        #in case of random
        hyper_domain <- range(hyper_grid_domain_list$max_depth$pars)
      }
    } else {
      #bayesian opt or grid search
      hyper_domain <- hyper_grid_domain_list$max_depth
    }
    #Check domain
    if(tuning_method == "grid_search"){
      if(!all(hyper_domain == floor(hyper_domain))){
        stop("max_depth should have no decimals")
      }
    } else {
      if(!all(is.integer(hyper_domain))){
        stop("max_depth should be integer")
      } else {}
    }
    if(!all(hyper_domain > 0)){
      stop("max_depth should be positive")
    } else {}
    ##########

    #colsample_bytree
    ##########
    if(tuning_method == "random_search"){
      if(hyper_grid_domain_list$colsample_bytree$distribution_choice == "constant"){
        hyper_domain <- hyper_grid_domain_list$colsample_bytree$value
      } else {
        #in case of random
        hyper_domain <- range(hyper_grid_domain_list$colsample_bytree$pars)
      }
    } else {
      #bayesian opt or grid search
      hyper_domain <- hyper_grid_domain_list$colsample_bytree
    }
    #Check domain
    if(!all(0 <= hyper_domain, hyper_domain <= 1)){
      stop("colsample_bytree should be set in interval [0,1]")
    } else {}
    ##########

    #subsample
    ##########
    if(tuning_method == "random_search"){
      if(hyper_grid_domain_list$subsample$distribution_choice == "constant"){
        hyper_domain <- hyper_grid_domain_list$subsample$value
      } else {
        #in case of random
        hyper_domain <- range(hyper_grid_domain_list$subsample$pars)
      }
    } else {
      #bayesian opt or grid search
      hyper_domain <- hyper_grid_domain_list$subsample
    }
    #Check domain
    if(!all(0 <= hyper_domain, hyper_domain <= 1)){
      stop("subsample should be set in interval [0,1]")
    } else {}
    ##########

  }
  ###############

  #NN
  ###############

  if(sb_algorithm == "nn"){
    #droprate
    ##########
    if(tuning_method == "random_search"){
      if(hyper_grid_domain_list$droprate$distribution_choice == "constant"){
        hyper_domain <- hyper_grid_domain_list$droprate$value
      } else {
        #in case of random
        hyper_domain <- range(hyper_grid_domain_list$droprate$pars)
      }
    } else {
      #bayesian opt or grid search
      hyper_domain <- hyper_grid_domain_list$droprate
    }
    #Check domain
    if(!all(0 <= hyper_domain, hyper_domain < 1)){
      stop("droprate should be set in interval [0,1)")
    } else {}
    ##########

    #number_of_epochs
    ##########
    if(tuning_method == "random_search"){
      if(hyper_grid_domain_list$number_of_epochs$distribution_choice == "constant"){
        hyper_domain <- hyper_grid_domain_list$number_of_epochs$value
      } else {
        #in case of random
        hyper_domain <- range(hyper_grid_domain_list$number_of_epochs$pars)
      }
    } else {
      #bayesian opt or grid search
      hyper_domain <- hyper_grid_domain_list$number_of_epochs
    }
    #Check domain
    if(tuning_method == "grid_search"){
      if(!all(hyper_domain == floor(hyper_domain))){
        stop("number_of_epochs should have no decimals")
      }
    } else {
      if(!all(is.integer(hyper_domain))){
        stop("number_of_epochs should be integer")
      } else {}
    }
    if(!all(hyper_domain > 0)){
      stop("number_of_epochs should be positive")
    } else {}
    ##########

    #size_of_batch
    ##########
    if(tuning_method == "random_search"){
      if(hyper_grid_domain_list$size_of_batch$distribution_choice == "constant"){
        hyper_domain <- hyper_grid_domain_list$size_of_batch$value
      } else {
        #in case of random
        hyper_domain <- range(hyper_grid_domain_list$size_of_batch$pars)
      }
    } else {
      #bayesian opt or grid search
      hyper_domain <- hyper_grid_domain_list$size_of_batch
    }
    #Check domain
    if(tuning_method == "grid_search"){
      if(!all(hyper_domain == floor(hyper_domain))){
        stop("size_of_batch should have no decimals")
      }
    } else {
      if(!all(is.integer(hyper_domain))){
        stop("size_of_batch should be integer")
      } else {}
    }
    if(!all(hyper_domain > 0)){
      stop("size_of_batch should be positive")
    } else {}
    ##########


  }
  ###############

}
