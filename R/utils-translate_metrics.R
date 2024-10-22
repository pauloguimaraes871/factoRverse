#' Translate metrics based on machine learning algorithm and chosen evaluation metric.
#'
#' This function adapts the chosen evaluation metric based on the provided parameters.
#'
#' @param ml_algorithm Character string specifying the machine learning algorithm.
#' @param chosen_eval_metric Character string specifying the chosen evaluation metric.
#' @param custom_objective Character string specifying the custom objective function.
#' @param early_stop Numeric or null, indicating whether early stopping is enabled.
#' @param huber_delta Numeric specifying the delta parameter for Huber loss if applicable.
#' @param verbose Logical indicating whether to display verbose messages.
#'
#' @return A list containing the adapted chosen evaluation metric and custom objective function.
#'
#' @details
#' - If \code{chosen_eval_metric} is \code{NULL}, it selects an appropriate metric based on \code{custom_objective}.
#' - Provides commentary if the selected evaluation metric is not supported for early stopping.
#' - Adjusts \code{custom_objective} based on the specified \code{ml_algorithm}.
#'
#' @examples
#' translate_metrics("xgb", NULL, "squared_error", early_stop = FALSE, verbose = TRUE)
#'
#' @export
#' @importFrom crayon yellow
#' @import keras
translate_metrics <- function(ml_algorithm, chosen_eval_metric, custom_objective, early_stop, huber_delta, verbose){

  #Adapt chosen_eval_metric if needed
  if(is.null(chosen_eval_metric)){

    #Set chosen_eval_metric based on custom_objective in case it's not previosly set
    chosen_eval_metric <- switch(
      custom_objective,
      squared_error = "rmse",
      absolute_error = "mae",
      pseudo_huber_error = "mphe",
      quantile_error = "mpe",
      "rmse"
    )
    if(verbose == TRUE){
      cat(crayon::yellow("chosen_eval_metric not declared. Choice will be based on custom_objective"))
      cat("\n")
    }
  } else {}


  #Translate custom_objective and chosen_eval_metric for early stop
  if(ml_algorithm %in% c("ols","glmnet","rf")){
    custom_objective_translated <- NULL
    chosen_eval_metric_translated <- NULL
  } else {}

  if(ml_algorithm == "xgb"){
    custom_objective_translated <- switch(custom_objective,
                                          squared_error = "reg:squarederror",
                                          absolute_error = "reg:absoluteerror",
                                          pseudo_huber_error = "reg:pseudohubererror",
                                          "reg:squarederror"
    )

    chosen_eval_metric_translated <- switch(chosen_eval_metric,
                                            rmse = "rmse",
                                            mae = "mae",
                                            mphe = "mphe",
                                            #mpe = mpe_xgb(quantile_tau = quantile_tau),
                                            mape = "mape",
                                            #rss = rss_xgb, MAX
                                            #hr = hr_xgb, MAX
                                            #mb = mb_xgb,
                                            #cp = cp_xgb, MAX
                                            "rmse"
    )

  } else {}
  if(ml_algorithm == "nn"){
    custom_objective_translated <- switch(custom_objective,
                                          squared_error = "mean_squared_error",
                                          absolute_error = "mean_absolute_error",
                                          pseudo_huber_error = keras::loss_huber(delta = huber_delta),
                                          "mean_squared_error"
    )

    chosen_eval_metric_translated <- switch(chosen_eval_metric,
           rmse = list(metric = "mean_squared_error", name = "val_mean_squared_error", mode = "min"),
           mae =  list(metric = "mean_absolute_error", name = "val_mean_absolute_error", mode = "min"),
           mphe = list(metric = keras::loss_huber(delta = huber_delta),
                    name = "val_huber_loss", mode = "min"),#Pseudo huber with custom delta
           #mpe = mpe_keras(quantile_tau = quantile_tau),
           mape = list(metric = "mean_absolute_percentage_error", name = "val_mean_absolute_percentage_error",
                    mode = "min"),
           #rss = rss_keras,
           #cp = cp_keras,
           list(metric = "mean_squared_error", name = "val_mean_squared_error", mode = "min")
          )



  } else {}



  #Commentary about early_stop and using a eval metric not supported
  if(verbose == TRUE){
    if(all(!is.null(early_stop), ml_algorithm %in% c("xgb", "nn"), !chosen_eval_metric %in% c("rmse", "mae", "mphe", "mape"))){
      cat(crayon::yellow(
        "This eval_metric is not supported by early stop. Applying rmse as criteria for early_stop instead."))
      cat("\n")
      cat(paste("However", chosen_eval_metric, "will still be applied in hyperparameter tuning."))
    } else {}

    #Commentary about pseudo_huber_error in nn
    if(all(ml_algorithm == "nn", #If Neural Network AND
           custom_objective == "pseudo_huber_error")){#pseudo_huber_error
      cat(crayon::yellow(
        "Internal keras operations do not handle pseudo huber metric, applying huber metric instead."))
      cat("\n")

      if(chosen_eval_metric == "mphe"){ #If custom_obj is pseudo_huber and chosen_eval is mphe
      cat(paste("However, mphe will still be applied in hyperparameter tuning."))
      cat("\n")
      }
    }
  }



  return(list(chosen_eval_metric = chosen_eval_metric, custom_objective_translated = custom_objective_translated, chosen_eval_metric_translated = chosen_eval_metric_translated))

}
