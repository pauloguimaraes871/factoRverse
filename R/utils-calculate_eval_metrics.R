#' Calculate Evaluation Metrics
#'
#' Calculate various evaluation metrics for model prediction performance.
#'
#' @param pred Numeric vector of predicted values.
#' @param target Numeric vector of actual target values.
#' @param huber_delta Numeric scalar, delta parameter for Pseudo-Huber loss calculation.
#' @param quantile_tau Numeric scalar, quantile parameter for Pinball loss calculation.
#' @param chosen_eval_metric Metric to optimize during tuning: "rss", "rmse", "cp", "mae", "mphe", "mpe", "mape", "hr" and "mb"
#' @param early_stop If numeric, will include Halting criteria to prevent overfitting in xgb and nn
#' @param best_iteration Best iteration according to early stopping criteria implemented
#' @param return_error Should residuals be returned?
#'
#' @return A data frame containing the calculated evaluation metrics:
#'   - `rss`: R-squared (coefficient of determination).
#'   - `cp`: Cross-product of predicted and actual values.
#'   - `rmse`: Root Mean Squared Error.
#'   - `mae`: Mean Absolute Error.
#'   - `mphe`: Mean Pseudo-Huber Error.
#'   - `mpe`: Mean Pinball Error.
#'   - `mape`: Mean Absolute Percentage Error.
#'   - `hr`: Hit Rate (percentage of predictions with correct sign).
#'   - `mb`: Mean Bias (mean of prediction errors).
#'
#' @examples
#' pred <- c(1.1, 2.2, 1.05)
#' target <- c(1.0, 2.0, 1.0)
#' calculate_eval_metrics(pred, target, chosen_eval_metric = "mphe", huber_delta = 1, quantile_tau = 0.5)
#'
#' @export
calculate_eval_metrics <- function(pred, target, huber_delta, quantile_tau, chosen_eval_metric,
                                   early_stop = NULL, best_iteration = NULL, return_error = FALSE
                                   ){

  if(any(is.na(pred))){
    stop("Error in calculate_eval_metrics: NAs present in pred")
  }

  if(!is.numeric(best_iteration) & !is.null(best_iteration)){
    stop("best_iteration should either be NULL or numeric.")
  }

  if(!chosen_eval_metric %in% c("rmse", "rss", "cp", "mae", "mphe", "mpe", "mape", "hr")){
    stop("chosen_eval_metric should be one of rmse, rss, cp, mae, mphe, mpe, mape, hr")
  }

  #Error
  error <- target - pred


 #Calculate eval metrics
  validation_sample_rss <- 1 - sum(error^2)/sum(target^2) #R2
  validation_sample_cp <- mean(pred*target) #Cross-Product
  validation_sample_rmse <- sqrt(mean(error^2)) #RMSE
  validation_sample_mae <- mean(abs(error)) #mae
  validation_sample_mphe <- mean(huber_delta^2 * (sqrt(1 + (error / huber_delta)^2) - 1)) #Pseudo-Huber
  validation_sample_mpe <- mean(ifelse(error>=0, quantile_tau * (error), (1-quantile_tau)*(-error))) #Pinball
  validation_sample_mape <- mean(abs(error/target)) #MAPE
  validation_sample_hr <- length(which(sign(pred) == sign(target)))/length(target)
  validation_sample_mb <- mean(error)

  #Return DF
  df_eval_metrics <- data.frame(
    Score = switch(chosen_eval_metric,
                   rss = validation_sample_rss, #RSS
                   cp = validation_sample_cp, #CP
                   rmse = -validation_sample_rmse, #RMSE
                   mae = -validation_sample_mae, #MAE
                   mphe = -validation_sample_mphe, #MPHE
                   mpe = -validation_sample_mpe, #Pinball
                   mape = -validation_sample_mape, #MAPE
                   hr = validation_sample_hr #Hit Rate
                   ),
    rss = validation_sample_rss,
    cp = validation_sample_cp,
    rmse = validation_sample_rmse,
    mae = validation_sample_mae,
    mphe = validation_sample_mphe,
    mpe = validation_sample_mpe,
    mape = validation_sample_mape,
    hr = validation_sample_hr,
    mb = validation_sample_mb
  )

  #Include best iteration from early_stop
  if(!is.null(early_stop)){
    df_eval_metrics$best_iteration <- best_iteration
  }

  if(return_error){
    return(list(df_eval_metrics = df_eval_metrics,
                error = error))
  }

  return(df_eval_metrics)


}
