#' Calculate Out-of-Sample Evaluation Metrics
#'
#' @description
#' Computes a panel of out-of-sample (OOS) prediction-quality metrics comparing a
#' vector of predictions against realized targets. Used inside the signal-blending
#' tuning loop (see \code{\link{hyper_tune}}) to score each hyperparameter candidate,
#' and again to report the metrics of the chosen model.
#'
#' @details
#' The error convention is \code{error = target - pred}. If every element of
#' \code{error}/\code{target} is \code{NA}, or any prediction is \code{NA}, all metrics
#' are returned as \code{NA} rather than propagating silently. The \code{Score} column
#' is the single scalar the tuner maximizes: the \code{chosen_eval_metric} re-signed so
#' that larger is always better (error-type metrics \code{rmse}, \code{mae},
#' \code{mphe}, \code{mpe}, \code{mape} are negated; \code{rss}, \code{cp}, \code{hr}
#' are kept as-is).
#'
#' @param pred Numeric vector of predicted values.
#' @param target Numeric vector of realized target values (same length as \code{pred}).
#' @param huber_delta Numeric scalar, delta for the Pseudo-Huber error (\code{mphe}). Default \code{1}.
#' @param quantile_tau Numeric scalar in \code{(0, 1)}, tau for the Pinball error (\code{mpe}). Default \code{0.5}.
#' @param chosen_eval_metric Character, metric used to build \code{Score}. One of
#'   \code{"rss"}, \code{"rmse"}, \code{"cp"}, \code{"mae"}, \code{"mphe"}, \code{"mpe"},
#'   \code{"mape"}, \code{"hr"}. Default \code{"rmse"}. Note \code{"mb"} is reported but
#'   not selectable here.
#' @param early_stop Numeric or \code{NULL}. When numeric, a \code{best_iteration}
#'   column is appended (for \code{xgb}/\code{nn} early stopping).
#' @param best_iteration Numeric or \code{NULL}. Best iteration index to record when \code{early_stop} is set.
#' @param return_error Logical. If \code{TRUE}, returns a list of the metrics data frame plus the raw \code{error} vector. Default \code{FALSE}.
#'
#' @return A one-row \code{data.frame} of metrics (or a list of that plus \code{error}
#'   when \code{return_error = TRUE}). Columns:
#'   \itemize{
#'     \item \code{Score}: \code{chosen_eval_metric} re-signed so higher is better (tuning target).
#'     \item \code{rss}: Out-of-sample R-squared, \eqn{1 - \sum error^2 / \sum target^2}.
#'     \item \code{cp}: Mean cross-product, \code{mean(pred * target)}.
#'     \item \code{rmse}: Root Mean Squared Error.
#'     \item \code{mae}: Mean Absolute Error.
#'     \item \code{mphe}: Mean Pseudo-Huber Error (uses \code{huber_delta}).
#'     \item \code{mpe}: Mean Pinball Error (uses \code{quantile_tau}).
#'     \item \code{mape}: Mean Absolute Percentage Error.
#'     \item \code{hr}: Hit Rate (share of predictions whose sign matches the target).
#'     \item \code{mb}: Mean Bias, \code{mean(error)} (reported only).
#'     \item \code{best_iteration}: Present only when \code{early_stop} is numeric.
#'   }
#'
#'
#' @export
calculate_eval_metrics <- function(pred, target, huber_delta = 1, quantile_tau = 0.5, chosen_eval_metric = "rmse",
                                   early_stop = NULL, best_iteration = NULL, return_error = FALSE
                                   ){

  if(!is.numeric(best_iteration) && !is.null(best_iteration)){
    stop("best_iteration should either be NULL or numeric.")
  }

  if(!chosen_eval_metric %in% c("rmse", "rss", "cp", "mae", "mphe", "mpe", "mape", "hr")){
    stop("chosen_eval_metric should be one of rmse, rss, cp, mae, mphe, mpe, mape, hr")
  }

  #Error
  error <- target - pred


 #Calculate eval metrics
  if(all(is.na(error), is.na(target)) || any(is.na(pred))){
    validation_sample_rss <- NA
    validation_sample_cp <- NA
    validation_sample_rmse <- NA
    validation_sample_mae <- NA
    validation_sample_mphe <- NA
    validation_sample_mpe <- NA
    validation_sample_mape <- NA
    validation_sample_hr <- NA
    validation_sample_mb <- NA
  } else {
    validation_sample_rss <- 1 - sum(error^2)/sum(target^2) #R2
    validation_sample_cp <- mean(pred*target) #Cross-Product
    validation_sample_rmse <- sqrt(mean(error^2)) #RMSE
    validation_sample_mae <- mean(abs(error)) #mae
    validation_sample_mphe <- mean(huber_delta^2 * (sqrt(1 + (error / huber_delta)^2) - 1)) #Pseudo-Huber
    validation_sample_mpe <- mean(ifelse(error>=0, quantile_tau * (error), (1-quantile_tau)*(-error))) #Pinball
    validation_sample_mape <- mean(abs(error/target)) #MAPE
    validation_sample_hr <- length(which(sign(pred) == sign(target)))/length(target)
    validation_sample_mb <- mean(error)
  }

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
