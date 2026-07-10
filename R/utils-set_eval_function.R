#' Build the Evaluation Function for Hyperparameter Tuning
#'
#' @description
#' Factory returning the closure \code{\link{hyper_tune}} uses to score one
#' hyperparameter candidate for a given ML algorithm. The shape of the returned
#' function depends on \code{tuning_method}, because grid/random search and Bayesian
#' optimization consume it differently.
#'
#' @details
#' \itemize{
#'   \item \strong{\code{"grid_search"} / \code{"random_search"}}: returns a function
#'     whose formals are the hyperparameters (plus data and eval-metric arguments),
#'     suitable for \code{purrr::pmap()} / \code{furrr::future_pmap()} over an expanded
#'     grid. It fits on the training sample, predicts on the validation sample, and
#'     returns the \code{\link{calculate_eval_metrics}} data frame (or the fitted model
#'     when \code{return_all_info = TRUE}).
#'   \item \strong{\code{"bayesian_opt"}}: returns a \emph{wrapper} that captures data /
#'     eval-metric arguments via \code{...}, then exposes an inner \code{fit()} taking
#'     only the hyperparameters and returning the named list of scalar scores expected
#'     by \code{ParBayesianOptimization::bayesOpt()}.
#' }
#' Supported algorithms: \code{"glmnet"} (\code{glmnet::glmnet}), \code{"rf"}
#' (\code{ranger::ranger}; \code{mtry} is treated as a proportion of predictors),
#' \code{"xgb"} (\code{xgboost::xgb.train}), \code{"nn"} (\code{\link{fit_keras_model}}).
#'
#' @param ml_algorithm Character, algorithm to build an evaluator for
#'   (\code{"glmnet"}, \code{"rf"}, \code{"xgb"}, \code{"nn"}).
#' @param tuning_method Character, one of \code{"grid_search"}, \code{"random_search"},
#'   \code{"bayesian_opt"}; selects the calling convention.
#'
#' @return A closure passed to \code{\link{hyper_tune}}: a direct evaluator for
#'   grid/random search, or a data-capturing wrapper for Bayesian optimization.
#'
#' @seealso \code{\link{hyper_tune}}, \code{\link{calculate_eval_metrics}}, \code{\link{fit_keras_model}}
#' @export
#'
 set_eval_function <- function(ml_algorithm, tuning_method){ #General Parameters

  #If tuning == grid or random, return a function to be used by purrr or furrr functionals
  #If tuning == bayesian_opt, return a function to be passed to ParBayesianOptimization package

  if(tuning_method %in% c("grid_search", "random_search")){
    #Set evaluate_hyper_objective_function according to grid_search or random_search
    eval_function <-
      switch(
        #GLMNET (Elastic Net)
        ml_algorithm,
        glmnet = function(alpha, lambda.min.ratio, #Hyperparameters
                          huber_delta = 1, quantile_tau = 0.5, #Eval Functions Parameters
                          full_data_training_sample_clean, features_validation_sample, target_validation_sample, target_fwd_name, #Data
                          chosen_eval_metric,
                          verbose, #Verbose
                          return_all_info = FALSE,
                          ...
        ){

          #Set objects in GLM format
          features_matrix_train_clean <- full_data_training_sample_clean %>% dplyr::select(-dplyr::all_of(target_fwd_name)) #Get training features matrix
          target_vector_train <- full_data_training_sample_clean %>% dplyr::pull(target_fwd_name) #Get training target vector
          features_validation_sample_clean <- features_validation_sample %>% dplyr::select(-1:-3)


          #Fit GLM model
          glmnet_fit <- glmnet::glmnet(as.matrix(features_matrix_train_clean), #train matrix
                                       target_vector_train, #target vector
                                       alpha = alpha, #alpha hyperparameter
                                       lambda.min.ratio = lambda.min.ratio) #lambda.min.ratio hyperparameter


          #Get best lambda
          best_lam <- get_best_lambda(glmnet_fit = glmnet_fit, lambda_seq = glmnet_fit$lambda, #Glmnet Specific
                                      features_validation_sample_clean = features_validation_sample_clean, target_validation_sample = target_validation_sample,  #Val Data
                                      huber_delta = huber_delta, quantile_tau = quantile_tau, chosen_eval_metric = chosen_eval_metric) #Eval Metrics Parameters

          #Predict with best lam
          pred <- stats::predict(glmnet_fit,#GLM model
                                 newx = as.matrix(features_validation_sample_clean),  #Features test
                                 s = best_lam) #Predict with best_lam


          #Calculate eval metrics
          df_eval_metrics <- calculate_eval_metrics(pred = pred, target = target_validation_sample,
                                                    huber_delta = huber_delta, quantile_tau = quantile_tau,
                                                    chosen_eval_metric = chosen_eval_metric)


          #Rename rows
          if(return_all_info == FALSE){
            return(cbind(df_eval_metrics, best_lam = best_lam))
          } else {
            return(list(df_eval_metrics = df_eval_metrics, ml_model = glmnet_fit,
                        pred = pred,
                        target = target_validation_sample,
                        best_lam = best_lam))
          }


        },
        #Random Forest
        rf = function(mtry, num.trees, max.depth, min.bucket, #Hyperparameters
                      huber_delta = 1, quantile_tau = 0.5, #Eval Functions Parameters
                      full_data_training_sample_clean, features_validation_sample, target_validation_sample, target_fwd_name, #Data
                      chosen_eval_metric,
                      verbose, #Verbose
                      return_all_info = FALSE, #Should model be returned (useful for testing)
                      ...
        ){

          #Fit RF model
          rf_fit <- ranger::ranger(paste(target_fwd_name,'~.'), data = janitor::clean_names(full_data_training_sample_clean), #Names need to be clean
                                   mtry = mtry * (ncol(full_data_training_sample_clean) - 1), #Proportion of variables used to forecast
                                   num.trees = num.trees, #Number of trees
                                   max.depth = max.depth, #Max Depth of tree
                                   min.bucket = min.bucket, #Min Size of Terminal Node
                                   verbose = FALSE
                                   )
          #Format
          features_validation_sample_clean <- features_validation_sample %>% dplyr::select(-1:-3)

          #Predict
          pred <- stats::predict(rf_fit,#RF model
                                 data = janitor::clean_names(features_validation_sample_clean) #Features val
          )$predictions


          #Calculate eval metrics
          df_eval_metrics <- calculate_eval_metrics(pred = pred, target = target_validation_sample,
                                                    huber_delta = huber_delta, quantile_tau = quantile_tau,
                                                    chosen_eval_metric = chosen_eval_metric)

          if(return_all_info == FALSE){
            return(df_eval_metrics)
          } else {
            return(list(df_eval_metrics = df_eval_metrics, ml_model = rf_fit,
                        pred = pred,
                        target = target_validation_sample))
          }


        },

        #XGBoost
        xgb = function(min_child_weight, max_depth, #Group 1 of Hyperparameters
                       subsample, colsample_bytree, #Group 2 of Hyperparameters
                       eta, alpha, gamma, nrounds, #Group 3 of Hyperparameters
                       huber_delta = 1, quantile_tau = 0.5, #Eval Functions Parameters
                       full_data_training_sample_clean, features_validation_sample, target_validation_sample, target_fwd_name, #Data
                       chosen_eval_metric,
                       verbose, #Verbose
                       return_all_info = FALSE, #Should model be returned (useful for testing)
                       ...
        ){

          #Additional XGB arguments
          args <- list(...)
          #Training
          early_stop <- args$early_stop #Early halting
          chosen_eval_metric_translated <- args$chosen_eval_metric_translated #Chosen eval metric for ealy halting

          #Loss Function
          custom_objective_translated <- args$custom_objective_translated

          #Set objects in XGB Format
          features_matrix_train_clean <- full_data_training_sample_clean %>% dplyr::select(-dplyr::all_of(target_fwd_name)) #Get training features matrix
          target_vector_train <- full_data_training_sample_clean %>% dplyr::pull(target_fwd_name) #Get training target vector
          features_validation_sample_clean <- features_validation_sample %>% dplyr::select(-1:-3)

          full_data_training_sample_clean_xgb <- xgboost::xgb.DMatrix(data = as.matrix(features_matrix_train_clean), #Already withou 3 first columns
                                                      label = target_vector_train)

          full_data_val_clean_xgb <- xgboost::xgb.DMatrix(data = as.matrix(features_validation_sample_clean),
                                                    label = target_validation_sample)


          #Fit XGB model
          xgb_fit <- xgboost::xgb.train(data = full_data_training_sample_clean_xgb,
                                        eta = eta, #Learning Rate
                                        early_stopping_rounds = early_stop, #Number of rounds to early stop
                                        min_child_weight = min_child_weight, #Minimum sum of instance weight (hessian) needed in a child
                                        max_depth = round(max_depth, 0), #Max tree depth
                                        nrounds = nrounds, #Number of trees (boosting interations)
                                        subsample = subsample, #Subsample ratio of training instance
                                        colsample_bytree = colsample_bytree, #Col subsample
                                        alpha = alpha, #L1 regularization on weights
                                        gamma = gamma, #Min loss reduction to make a further partition
                                        print_every_n = 25,
                                        verbose = FALSE,
                                        eval_metric = chosen_eval_metric_translated, #Set eval metric for ealy stop
                                        #Set custom objective
                                        objective = custom_objective_translated,
                                        #Watchlist,
                                        watchlist = list(train = full_data_training_sample_clean_xgb,
                                                         validation = full_data_val_clean_xgb),

                                        huber_slope = huber_delta #Huber delta
                                        #quantile_alpha = quantile_tau #Tau for quantile regression

          )


          #Predict
          pred <- stats::predict(xgb_fit,#XGB model
                                 newdata = as.matrix(features_validation_sample_clean) #Features val
          )

          #Calculate eval metrics
          df_eval_metrics <- calculate_eval_metrics(pred = pred, target = target_validation_sample,
                                                    huber_delta = huber_delta, quantile_tau = quantile_tau,
                                                    chosen_eval_metric = chosen_eval_metric,
                                                    early_stop = early_stop,
                                                    best_iteration = xgb_fit$best_iteration
                                                    )


          #Return Results
          if(return_all_info == FALSE){
            return(df_eval_metrics)
          } else {
            return(list(df_eval_metrics = df_eval_metrics, ml_model = xgb_fit,
                        pred = pred,
                        target = target_validation_sample))
          }


        },

        nn = function(regularizer_l1, regularizer_l2, droprate, lr, number_of_epochs, size_of_batch, #Hyperparameters
                      huber_delta = 1, quantile_tau = 0.5,  #Eval Functions Parameters
                      full_data_training_sample_clean, features_validation_sample, target_validation_sample, target_fwd_name, #Data
                      chosen_eval_metric,
                      verbose, #Verbose
                      return_all_info = FALSE,
                      ...
        ){


          #Additional NN arguments
          args <- list(...) #Get extra aguments

          #Network
          keras_architecture_parameters <- args$keras_architecture_parameters

          #Training
          early_stop <- args$early_stop #Early Halting
          chosen_eval_metric_translated <- args$chosen_eval_metric_translated #Chosen eval metric for ealy halting

          #Loss Function
          custom_objective_translated <- args$custom_objective_translated

          #Format
          features_matrix_train_clean <- full_data_training_sample_clean %>% dplyr::select(-dplyr::all_of(target_fwd_name)) #Get training features matrix
          target_vector_train <- full_data_training_sample_clean %>% dplyr::pull(target_fwd_name) #Get training target vector
          features_validation_sample_clean <- features_validation_sample %>% dplyr::select(-1:-3)

          #Fit keras model
          keras_results <- fit_keras_model(
            #Hyperparameters
            regularizer_l1 = regularizer_l1, regularizer_l2 = regularizer_l2, droprate = droprate, #Hyperparameters Part 1
            lr = lr, number_of_epochs = number_of_epochs, size_of_batch = size_of_batch, #Hyperparameters Part 2

            #Architecture choices
            keras_architecture_parameters = keras_architecture_parameters,

            #Early Stop
            early_stop = early_stop, chosen_eval_metric_translated = chosen_eval_metric_translated,

            #Loss Function
            custom_objective_translated = custom_objective_translated,  huber_delta = huber_delta,

            #Data
            features_matrix_train_clean = features_matrix_train_clean, target_vector_train = target_vector_train, #Data Part I
            features_validation_sample_clean = features_validation_sample_clean, target_validation_sample = target_validation_sample, #Data Part II

            verbose = verbose #Verbose
          )

          model_nn <- keras_results$model_nn #Neural network models
          fit_nn <- keras_results$fit_nn #Training history

          #Predict
          pred <- stats::predict(model_nn,#NN model
                                 as.matrix(features_validation_sample_clean) #Features val
          )

          #Calculate eval metrics
          df_eval_metrics <- calculate_eval_metrics(pred = pred, target = target_validation_sample,
                                                    huber_delta = huber_delta, quantile_tau = quantile_tau,
                                                    chosen_eval_metric = chosen_eval_metric,
                                                    early_stop = early_stop,
                                                    best_iteration = which.min(fit_nn$metrics[[chosen_eval_metric_translated$name]])
                                                    )


          #Return Results
          if(return_all_info == FALSE){

            #Improve memory usage
            rm(features_matrix_train_clean, target_vector_train, features_validation_sample_clean,
               model_nn, fit_nn)
            gc()

            return(df_eval_metrics)
          } else {
            return(list(df_eval_metrics = df_eval_metrics, model_nn = model_nn, fit_nn = fit_nn,
                        pred = pred,
                        target = target_validation_sample))


          }


        }

      )

    } else {}
    #If tuning method is bayesian_opt, calls to eval function should be made through a wrapper
  if(tuning_method == "bayesian_opt"){
    eval_function <-
      switch(ml_algorithm,
             #GLMNET
             glmnet = function(...){ #Wrapper function
               #Get args
               #######################
               args <- list(...)

               #Data arguments
               full_data_training_sample_clean <- args$full_data_training_sample_clean #full data
               features_validation_sample <- args$features_validation_sample #validation features
               target_validation_sample <- args$target_validation_sample #validation target
               target_fwd_name <- args$target_fwd_name #target

               #Eval Function Parameters
               chosen_eval_metric <- args$chosen_eval_metric #Chosen Eval
               chosen_eval_metric_translated <- args$chosen_eval_metric_translated #Chosen Eval Metric for Early Stop
               huber_delta <- args$huber_delta #Huber delta
               quantile_tau <- args$quantile_tau #Quantile tau

               #Early Stop
               early_stop <- args$early_stop #Eartly Stop

               #Custom Loss
               custom_objective_translated <- args$custom_objective_translated

               #Keras Network Parameters
               keras_architecture_parameters <- args$keras_architecture_parameters #Chosen eval metric

               verbose <- args$verbose
               #######################

               fit <- function(alpha, lambda.min.ratio){ #Hyperparameters

                 #Set objects in GLM format
                 features_matrix_train_clean <- full_data_training_sample_clean %>% dplyr::select(-dplyr::all_of(target_fwd_name)) #Get training features matrix
                 target_vector_train <- full_data_training_sample_clean %>% dplyr::pull(target_fwd_name) #Get training target vector
                 features_validation_sample_clean <- features_validation_sample %>% dplyr::select(-1:-3)


                 #Fit GLM model
                 glmnet_fit <- glmnet::glmnet(as.matrix(features_matrix_train_clean), #train matrix
                                              target_vector_train, #target vector
                                              alpha = alpha, #alpha hyperparameter
                                              lambda.min.ratio = lambda.min.ratio) #lambda hyperparameter


                 #Get best lambda
                 best_lam <- get_best_lambda(glmnet_fit = glmnet_fit, lambda_seq = glmnet_fit$lambda, #Glmnet Specific
                                             features_validation_sample_clean = features_validation_sample_clean, target_validation_sample = target_validation_sample,  #Val Data
                                             huber_delta = huber_delta, quantile_tau = quantile_tau, chosen_eval_metric = chosen_eval_metric) #Eval Metrics Parameters

                 #Predict with best lam
                 pred <- stats::predict(glmnet_fit,#GLM model
                                        newx = as.matrix(features_validation_sample_clean),  #Features test
                                        s = best_lam) #Predict with best_lam


                 #Calculate eval metrics
                 df_eval_metrics <- calculate_eval_metrics(pred = pred, target = target_validation_sample,
                                                           huber_delta = huber_delta, quantile_tau = quantile_tau,
                                                           chosen_eval_metric = chosen_eval_metric)

                 #Return List
                 return(list(Score = df_eval_metrics$Score,
                             rss = df_eval_metrics$rss,
                             cp = df_eval_metrics$cp,
                             rmse = df_eval_metrics$rmse,
                             mae = df_eval_metrics$mae,
                             mphe = df_eval_metrics$mphe,
                             mpe = df_eval_metrics$mpe,
                             mape = df_eval_metrics$mape,
                             hr = df_eval_metrics$hr,
                             mb = df_eval_metrics$mb,
                             best_lam = best_lam)
                 )

               }

             },
             #Random Forest
             rf = function(...){ #Wrapper function

               #Get args
               #######################
               args <- list(...)

               #Data arguments
               full_data_training_sample_clean <- args$full_data_training_sample_clean #full data
               features_validation_sample <- args$features_validation_sample #validation features
               target_validation_sample <- args$target_validation_sample #validation target
               target_fwd_name <- args$target_fwd_name #target

               #Eval Function Parameters
               chosen_eval_metric <- args$chosen_eval_metric #Chosen Eval
               chosen_eval_metric_translated <- args$chosen_eval_metric_translated #Chosen Eval Metric for Early Stop
               huber_delta <- args$huber_delta #Huber delta
               quantile_tau <- args$quantile_tau #Quantile tau

               #Early Stop
               early_stop <- args$early_stop #Eartly Stop

               #Custom Loss
               custom_objective_translated <- args$custom_objective_translated

               #Keras Network Parameters
               keras_architecture_parameters <- args$keras_architecture_parameters #Chosen eval metric

               verbose <- args$verbose
               #######################

               fit <- function(mtry, num.trees, max.depth, min.bucket){ #Hyperparameters

                 #Fit RF model
                 rf_fit <- ranger::ranger(paste(target_fwd_name,'~.'), data = janitor::clean_names(full_data_training_sample_clean), #Names need to be clean
                                          mtry = mtry * (ncol(full_data_training_sample_clean) - 1), #Proportion of variables used to forecast
                                          num.trees = num.trees, #Number of trees
                                          max.depth = max.depth, #Max Depth of tree
                                          min.bucket = min.bucket) #Min Size of Terminal Node

                 #Format
                 features_validation_sample_clean <- features_validation_sample %>% dplyr::select(-1:-3)


                 #Predict
                 pred <- stats::predict(rf_fit,#RF model
                                        data = janitor::clean_names(features_validation_sample_clean) #Features val
                 )$predictions

                 #Calculate eval metrics
                 df_eval_metrics <- calculate_eval_metrics(pred = pred, target = target_validation_sample,
                                                           huber_delta = huber_delta, quantile_tau = quantile_tau,
                                                           chosen_eval_metric = chosen_eval_metric)

                 #Return List
                 return(list(Score = df_eval_metrics$Score,
                             rss = df_eval_metrics$rss,
                             cp = df_eval_metrics$cp,
                             rmse = df_eval_metrics$rmse,
                             mae = df_eval_metrics$mae,
                             mphe = df_eval_metrics$mphe,
                             mpe = df_eval_metrics$mpe,
                             mape = df_eval_metrics$mape,
                             hr = df_eval_metrics$hr,
                             mb = df_eval_metrics$mb)
                        )

               }

             },
             #XGB
             xgb = function(...){ #Wrapper function


               #Get args
               ########################
               args <- list(...)

               #Data arguments
               full_data_training_sample_clean <- args$full_data_training_sample_clean #full data
               features_validation_sample <- args$features_validation_sample #validation features
               target_validation_sample <- args$target_validation_sample #validation target
               target_fwd_name <- args$target_fwd_name #target

               #Eval Function Parameters
               chosen_eval_metric <- args$chosen_eval_metric #Chosen Eval
               chosen_eval_metric_translated <- args$chosen_eval_metric_translated #Chosen Eval Metric for Early Stop
               huber_delta <- args$huber_delta #Huber delta
               quantile_tau <- args$quantile_tau #Quantile tau

               #Early Stop
               early_stop <- args$early_stop #Eartly Stop

               #Custom Loss
               custom_objective_translated <- args$custom_objective_translated

               #Keras Network Parameters
               keras_architecture_parameters <- args$keras_architecture_parameters #Chosen eval metric

               verbose <- args$verbose
               ########################

               fit <- function(min_child_weight, max_depth, subsample, colsample_bytree, eta, alpha, gamma, nrounds){ #Hyperparameters

               #Set objects in XGB Format
               features_matrix_train_clean <- full_data_training_sample_clean %>% dplyr::select(-dplyr::all_of(target_fwd_name)) #Get training features matrix
               target_vector_train <- full_data_training_sample_clean %>% dplyr::pull(target_fwd_name) #Get training target vector
               features_validation_sample_clean <- features_validation_sample %>% dplyr::select(-1:-3)

               full_data_training_sample_clean_xgb <- xgboost::xgb.DMatrix(data = as.matrix(features_matrix_train_clean), #Already withou 3 first columns
                                                                           label = target_vector_train)

               full_data_val_clean_xgb <- xgboost::xgb.DMatrix(data = as.matrix(features_validation_sample_clean),
                                                               label = target_validation_sample)


               #Fit XGB model
               xgb_fit <- xgboost::xgb.train(data = full_data_training_sample_clean_xgb,
                                             eta = eta, #Learning Rate
                                             early_stopping_rounds = early_stop, #Number of rounds to early stop
                                             min_child_weight = min_child_weight, #Minimum sum of instance weight (hessian) needed in a child
                                             max_depth = round(max_depth, 0), #Max tree depth
                                             nrounds = nrounds, #Number of trees (boosting interations)
                                             subsample = subsample, #Subsample ratio of training instance
                                             colsample_bytree = colsample_bytree, #Col subsample
                                             alpha = alpha, #L1 regularization on weights
                                             gamma = gamma, #Min loss reduction to make a further partition
                                             print_every_n = 25,
                                             verbose = FALSE,
                                             eval_metric = chosen_eval_metric_translated, #Set eval metric for ealy stop
                                             #Set custom objective
                                             objective = custom_objective_translated,
                                             #Watchlist,
                                             watchlist = list(train = full_data_training_sample_clean_xgb,
                                                              validation = full_data_val_clean_xgb),

                                             huber_slope = huber_delta #Huber delta
                                             #quantile_alpha = quantile_tau #Tau for quantile regression

               )


               #Predict
               pred <- stats::predict(xgb_fit,#XGB model
                                      newdata = as.matrix(features_validation_sample_clean) #Features val
               )

               #Calculate eval metrics
               df_eval_metrics <- calculate_eval_metrics(pred = pred, target = target_validation_sample,
                                                         huber_delta = huber_delta, quantile_tau = quantile_tau,
                                                         chosen_eval_metric = chosen_eval_metric,
                                                         early_stop = early_stop,
                                                         best_iteration = xgb_fit$best_iteration
               )

               #Return List
               if(is.null(early_stop)){
               return(list(Score = df_eval_metrics$Score,
                           rss = df_eval_metrics$rss,
                           cp = df_eval_metrics$cp,
                           rmse = df_eval_metrics$rmse,
                           mae = df_eval_metrics$mae,
                           mphe = df_eval_metrics$mphe,
                           mpe = df_eval_metrics$mpe,
                           mape = df_eval_metrics$mape,
                           hr = df_eval_metrics$hr,
                           mb = df_eval_metrics$mb)
               )

              } else {
              return(list(Score = df_eval_metrics$Score,
                             rss = df_eval_metrics$rss,
                             cp = df_eval_metrics$cp,
                             rmse = df_eval_metrics$rmse,
                             mae = df_eval_metrics$mae,
                             mphe = df_eval_metrics$mphe,
                             mpe = df_eval_metrics$mpe,
                             mape = df_eval_metrics$mape,
                             hr = df_eval_metrics$hr,
                             mb = df_eval_metrics$mb,
                             best_iteration = df_eval_metrics$best_iteration)
              )


              }


           }

       },
             #NN
             nn = function(...){ #Wrapper function


               #Get args
               ########################
               args <- list(...)

               #Data arguments
               full_data_training_sample_clean <- args$full_data_training_sample_clean #full data
               features_validation_sample <- args$features_validation_sample #validation features
               target_validation_sample <- args$target_validation_sample #validation target
               target_fwd_name <- args$target_fwd_name #target

               #Eval Function Parameters
               chosen_eval_metric <- args$chosen_eval_metric #Chosen Eval
               chosen_eval_metric_translated <- args$chosen_eval_metric_translated #Chosen Eval Metric for Early Stop
               huber_delta <- args$huber_delta #Huber delta
               quantile_tau <- args$quantile_tau #Quantile tau

               #Early Stop
               early_stop <- args$early_stop #Eartly Stop

               #Custom Loss
               custom_objective_translated <- args$custom_objective_translated

               #Keras Network Parameters
               keras_architecture_parameters <- args$keras_architecture_parameters #Chosen eval metric

               verbose <- args$verbose
               ########################

               fit <- function(regularizer_l1, regularizer_l2, droprate, lr, number_of_epochs, size_of_batch){ #Hyperparameters

                 #Format
                 features_matrix_train_clean <- full_data_training_sample_clean %>% dplyr::select(-dplyr::all_of(target_fwd_name)) #Get training features matrix
                 target_vector_train <- full_data_training_sample_clean %>% dplyr::pull(target_fwd_name) #Get training target vector
                 features_validation_sample_clean <- features_validation_sample %>% dplyr::select(-1:-3)

                 #Fit keras model
                 keras_results <- fit_keras_model(
                   #Hyperparameters
                   regularizer_l1 = regularizer_l1, regularizer_l2 = regularizer_l2, droprate = droprate, #Hyperparameters Part 1
                   lr = lr, number_of_epochs = number_of_epochs, size_of_batch = size_of_batch, #Hyperparameters Part 2

                   #Architecture choices
                   keras_architecture_parameters = keras_architecture_parameters,

                   #Early Stop
                   early_stop = early_stop, chosen_eval_metric_translated = chosen_eval_metric_translated,

                   #Loss Function
                   custom_objective_translated = custom_objective_translated,  huber_delta = huber_delta,

                   #Data
                   features_matrix_train_clean = features_matrix_train_clean, target_vector_train = target_vector_train, #Data Part I
                   features_validation_sample_clean = features_validation_sample_clean, target_validation_sample = target_validation_sample, #Data Part II

                   verbose = verbose #Verbose
                 )

                 model_nn <- keras_results$model_nn #Neural network models
                 fit_nn <- keras_results$fit_nn #Training history

                 #Predict
                 pred <- stats::predict(model_nn,#NN model
                                        as.matrix(features_validation_sample_clean) #Features val
                 )

                 #Calculate eval metrics
                 df_eval_metrics <- calculate_eval_metrics(pred = pred, target = target_validation_sample,
                                                           huber_delta = huber_delta, quantile_tau = quantile_tau,
                                                           chosen_eval_metric = chosen_eval_metric,
                                                           early_stop = early_stop,
                                                           best_iteration = which.min(fit_nn$metrics[[chosen_eval_metric_translated$name]])
                 )

                 #Improve memory usage
                 rm(features_matrix_train_clean, target_vector_train, features_validation_sample_clean,
                    model_nn, fit_nn)
                 gc()


                 #Return List
                 if(is.null(early_stop)){
                   return(list(Score = df_eval_metrics$Score,
                               rss = df_eval_metrics$rss,
                               cp = df_eval_metrics$cp,
                               rmse = df_eval_metrics$rmse,
                               mae = df_eval_metrics$mae,
                               mphe = df_eval_metrics$mphe,
                               mpe = df_eval_metrics$mpe,
                               mape = df_eval_metrics$mape,
                               hr = df_eval_metrics$hr,
                               mb = df_eval_metrics$mb)
                   )

                 } else {
                   return(list(Score = df_eval_metrics$Score,
                               rss = df_eval_metrics$rss,
                               cp = df_eval_metrics$cp,
                               rmse = df_eval_metrics$rmse,
                               mae = df_eval_metrics$mae,
                               mphe = df_eval_metrics$mphe,
                               mpe = df_eval_metrics$mpe,
                               mape = df_eval_metrics$mape,
                               hr = df_eval_metrics$hr,
                               mb = df_eval_metrics$mb,
                               best_iteration = df_eval_metrics$best_iteration)
                   )


                 }


               }

       }


        )

    } else {}

  return(eval_function)






  }



################################################################




###############Custom Losses and Eval Metrics (in development)
###############################################################

#Pinball Loss is not differentiable
#https://stackoverflow.com/questions/73804076/how-to-compute-a-custom-loss-function-in-r-using-keras-with-tensorflow
#Pinball Loss - Keras: https://github.com/rstudio/keras3/issues/451
#pinball_loss_keras <- function(quantile_tau, y_true, y_pred){
#  err <- y_true - y_pred #Define error
#  keras::k_mean(keras::k_maximum(quantile_tau*err,
#                                 (quantile_tau - 1)*err)
#                ) #Keras representation
#}

#mpe_keras <- keras::custom_metric("mpe", function(quantile_tau, y_true, y_pred){
#  err <- y_true - y_pred #Define error
#  keras::k_mean(keras::k_maximum(quantile_tau*err,
#                                 (quantile_tau - 1)*err)
#                ) #Keras representation
#})

#OOS R²
#rss_keras <- keras::custom_metric("rss", function(y_true, y_pred){
#  err <- y_true - y_pred #Define error
#  (1 - (keras::k_sum(keras::k_square(err))/keras::k_sum(keras::k_square(y_true))
#        ))
#})

#CP
#cp_keras <- keras::custom_metric("cp", function(y_true, y_pred){
#  keras::k_mean(y_true*y_pred)
#})

#Pinball Loss - XGBOOST https://gist.github.com/Nikolay-Lysenko/06769d701c1d9c9acb9a66f2f9d7a6c7
pinball_loss_xgb <- function(preds, full_data_train_xgb, quantile_tau){
  # Check if quantile is within valid range
  if (quantile_tau < 0 || quantile_tau > 1) {
    stop("Quantile value must be numeric between 0 and 1.")
  }

  labels <- xgboost::getinfo(full_data_train_xgb, "label")
  err = preds - labels

  left_mask <- err < 0
  right_mask <- err > 0

  grad <- -quantile_tau * left_mask + (1 - quantile_tau) * right_mask
  hess <- rep(1, length(preds))  # or use 'ones_like(preds)' if it's defined

  return(list(grad = grad, hess = hess))
}


mpe_xgb <- function(preds, full_data_train_xgb, quantile_tau){
  labels <- xgboost::getinfo(full_data_train_xgb, "label")
  # Calculate
  mpe <- mean(
    (preds >= labels) * (1 - quantile_tau) * (preds - labels) +
      (preds < labels) * quantile_tau * (labels - preds),
    na.rm = TRUE
  )

  # Return the loss value along with a string identifier
  return(list(
    metric = "mpe",
    value = mpe)
  )

}

#rss XGB
rss_xgb <- function(preds, full_data_train_xgb){
  labels <- xgboost::getinfo(full_data_train_xgb, "label")
  # Calculate
  oos_rss <- (1 - (sum((labels - preds)^2)/sum(labels^2)))
  # Return the loss value along with a string identifier
  return(list(
    metric = "rss",
    value = oos_rss)
  )

}

#CP XGB
cp_xgb <- function(preds, full_data_train_xgb){
  labels <- xgboost::getinfo(full_data_train_xgb, "label")
  # Calculate
  cp <- mean(labels * preds)
  # Return the loss value along with a string identifier
  return(list(
    metric = "cp",
    value = cp)
  )

}


#Custom Loss Functions - XGB
assym_loss_xgb <- function(preds, full_data_train_xgb, gamma){
  labels <- xgboost::getinfo(full_data_train_xgb, "label")
  grad <- ifelse((labels - preds) < 0, -2 * (labels - preds), -2*gamma*(labels - preds))
  hess <- ifelse((labels - preds) < 0, 2, 2*gamma)
return(list(grad = grad, hess = hess))
}

assym_eval_xgb <- function(preds, full_data_train_xgb, gamma){
  labels <- xgboost::getinfo(full_data_train_xgb, "label")
  err <- as.numeric(
    ifelse((labels - preds) < 0, mean((labels - preds)^2), gamma*mean((labels - preds)^2))
    )
}






