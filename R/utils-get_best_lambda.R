#' Get Best Lambda from Glmnet Model Based on Training/Test Sets
#'
#' Given a glmnet model fit and a lambda sequence, returns the lambda (regularization parameter)
#' that yields the best performance metric on a validation sample.
#'
#' @param glmnet_fit A glmnet object obtained from fitting the model using glmnet.
#' @param lambda_seq Numeric vector of lambda values used in the glmnet model.
#' @param features_validation_sample_clean Matrix or data frame of validation set features.
#' @param target_validation_sample Vector of true values for the validation set.
#' @param huber_delta Huber loss parameter (optional, default is 1.345).
#' @param quantile_tau Quantile loss parameter (optional, default is 0.5).
#' @param chosen_eval_metric Character string specifying the evaluation metric to optimize (e.g., "mse", "mae", "quantile").
#'
#' @return The lambda value from lambda_seq that maximizes the chosen evaluation metric.
#'
#'
#'
get_best_lambda <- function(glmnet_fit, lambda_seq, features_validation_sample_clean, target_validation_sample, huber_delta, quantile_tau, chosen_eval_metric){
  lambda_seq[which.max( #Which max score?
    sapply(
      apply(stats::predict(glmnet_fit, newx = as.matrix(features_validation_sample_clean)) #Predict to find best_lam
            , 2, function(x) calculate_eval_metrics(pred = x, target = target_validation_sample,
                                                    huber_delta = huber_delta, quantile_tau = quantile_tau,
                                                    chosen_eval_metric = chosen_eval_metric)), #Calculate eval metrics for all lambdas
      function(x) x$Score #Takes only score value

    )
  )]
}
