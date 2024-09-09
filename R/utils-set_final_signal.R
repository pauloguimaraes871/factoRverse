#' Compute and Set Final Signal
#'
#' This function calculates and assigns a final signal value to a set of financial signals based on a specified
#' blending method. The blending method can either be a non-machine-learning approach or a machine learning
#' approach. The function handles signal weighting, quantile winsorization, and, if applicable, fits and uses
#' a machine learning model to generate predictions.
#'
#' @param selected_signals_corrected_positions_m_upd_ref A data frame containing selected signals with corrected positions,
#'   including date information. This data frame is updated with the final signal values after computation.
#' @param signals_positions A data frame that specifies the positions (long or short) of the signals.
#' @param signal_weights A vector of weights for each signal.
#' @param ml_walk_forward_validation_results
#' @param signal_blending_method A character string specifying the method used to blend signals. Possible values are
#'   "ML" for machine learning or any other value for non-ML methods (EW, SW, RP or MTO).
#' @param target_m_upd_ref A data frame containing the target variables used for machine learning model fitting,
#'   including date information.
#' @param upper_quantile_winsorization Numeric value for upper winsorization when creating signals
#' @param lower_quantile_winsorization Numeric value for lower winsorization when creating signals
#' @export
set_final_signal <- function(selected_signals_corrected_positions_m_d_ref,
                             eligible_signals, #Signals deemed eligible
                             signal_weights, #For no ML approach
                             ml_walk_forward_validation_results, #For ML approach
                             #Winsorize
                             upper_quantile_winsorization = 0.975, lower_quantile_winsorization = 0.025){


  #Compute final_signal
  ##########################
  if(!is.null(signal_weights) & !is.null(ml_walk_forward_validation_results)){
    stop("Only one of signal_weights and ml_walk_forward_validation_results should be provided")
  }

  ###For blending method different from ML
  ########################################
  if(!is.null(signal_weights)){

    ##Calculate final signal
    selected_signals_corrected_positions_m_d_ref[, "final_signal"] <- as.data.frame(selected_signals_corrected_positions_m_d_ref[,-c(1:3)]) %>% #don't take id, tickers, dates
      apply(1, function(row){ #To each row
        sum(row * signal_weights) #Multiply by corresponding weights and sum
      }) %>% #Signal transform
      signal_transform(upper_quantile_winsorization = upper_quantile_winsorization, lower_quantile_winsorization = lower_quantile_winsorization)
    ############################################
  } else {}

  ###For ML models
  #############################
  if(!is.null(ml_walk_forward_validation_results)){

    #Predict to new data
    ###Subset
    new_features_m_d_ref <- selected_signals_corrected_positions_m_d_ref %>% dplyr::select(id, tickers, dates, all_of(eligible_signals$tickers))

    ###ML Predict
    ml_predictions <- predict_ml_model(ml_walk_forward_validation_results = ml_walk_forward_validation_results, new_features_m_d_ref = new_features_m_d_ref)

    ###Attach to selected_signals_corrected_positions_m_d_ref
    ##Calculate final signal
    selected_signals_corrected_positions_m_d_ref[, "final_signal"] <- ml_predictions %>% #Signal transform
      signal_transform(upper_quantile_winsorization = upper_quantile_winsorization, lower_quantile_winsorization = lower_quantile_winsorization)

  } else {}

  #Get results
  final_signal_results_list <- list()
  final_signal_results_list$selected_signals_corrected_positions_m_d_ref <- selected_signals_corrected_positions_m_d_ref
  try(final_signal_results_list$ml_predictions <- ml_predictions, silent = TRUE)
  try(final_signal_results_list$new_features_m_d_ref <- new_features_m_d_ref, silent = TRUE)

  return(final_signal_results_list)

}
