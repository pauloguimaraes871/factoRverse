#' Fit a Keras Neural Network Model
#'
#' This function fits a Keras neural network model based on the specified hyperparameters,
#' architecture choices, training configurations, and loss function parameters.
#'
#' @param regularizer_l1 Numeric. L1 regularization parameter.
#' @param regularizer_l2 Numeric. L2 regularization parameter.
#' @param droprate Numeric. Dropout rate.
#' @param lr Numeric. Learning rate.
#' @param keras_architecture_parameters List, containing n_layers, units, activation, nn_optimizer and batch_norm_option
#' @param number_of_epochs Integer. Maximum number of training epochs.
#' @param size_of_batch Integer. Batch size for training.
#' @param early_stop Integer or NULL. Number of epochs with no improvement to stop early, or NULL for no early stopping.
#' @param custom_objective_translated Custom objective in keras format
#' @param huber_delta Numeric. Delta parameter for Huber loss function.
#' @param features_matrix_train_clean Matrix. Training features matrix.
#' @param target_vector_train Vector. Training target vector.
#' @param verbose Integer. Verbosity level during training.
#' @param ... Additional arguments. Not currently used.
#'
#' @return A list containing:
#'   \item{model_nn}{The Keras model object.}
#'   \item{fit_nn}{The fitted Keras model object.}
#'
#'
#' @import keras
#' @export

fit_keras_model <- function(regularizer_l1, regularizer_l2, droprate, lr, number_of_epochs, size_of_batch, #Hyperparameters
                            keras_architecture_parameters, #Network
                            early_stop = NULL, #Training
                            custom_objective_translated, huber_delta, #Loss Function Parameters
                            features_matrix_train_clean, target_vector_train, #Data
                            verbose,
                            ...
){

  #Clear the session after each model training
  on.exit({
    keras::k_clear_session()
    gc()
  }, add = TRUE)
  . <- NULL
  #Validation arguments necessary only for early stop on validation set
  args <- list(...)
  try({ #early_stop: Can either be NULL (not set by user, which is a refit), NULL (set by user, which is do not apply early stop) and NUMBER (set by user, for tuning only)
    features_validation_sample_clean <- args$features_validation_sample_clean
    target_validation_sample <- args$target_validation_sample
  })
  chosen_eval_metric_translated<- args$chosen_eval_metric_translated

  #Define the structure of the network (how layers are organized)
  #Typical NN1 Architecture
  if(keras_architecture_parameters$n_layers == 1){
    model_nn <- keras::keras_model_sequential()
    tryCatch(
      {#Try to create keras network
        model_nn %>%
          keras::layer_dense(units = keras_architecture_parameters$units[1],
                             activation = keras_architecture_parameters$activation[1], #Units and activation may vary by layer
                             input_shape =  ncol(features_matrix_train_clean), #Shape = # of features
                             kernel_regularizer = keras::regularizer_l1_l2(l1 = regularizer_l1, l2 = regularizer_l2)) %>% #L1 and L2 Regularization
          {if (keras_architecture_parameters$batch_norm_option[1]) keras::layer_batch_normalization() else .}() %>% #Batch normalization
          keras::layer_dropout(rate = droprate) %>% #Adds dropout

          keras::layer_dense(units = 1) #No activation means linear: f(x) = x
      },
      error = function(e){ #In case of error
        message("Failure in creating keras network. Please check if input parameters units, activation, input_shape, kernel_regularizer,
                batch_norm_option, droprate are appropriate.")
        message("Here is the original error message:")
        message(conditionMessage(e))
      }
    )
  } else {}
  #Typical NN2 Architecture
  if(keras_architecture_parameters$n_layers == 2){
    model_nn <- keras::keras_model_sequential()
    tryCatch(
      {#Try to create keras network
        model_nn %>%
          keras::layer_dense(units = keras_architecture_parameters$units[1],
                             activation = keras_architecture_parameters$activation[1], #Units and activation may vary by layer
                             input_shape =  ncol(features_matrix_train_clean), #Shape = # of features
                             kernel_regularizer = keras::regularizer_l1_l2(l1 = regularizer_l1, l2 = regularizer_l2)) %>% #L1 and L2 Regularization
          {if (keras_architecture_parameters$batch_norm_option[1]) keras::layer_batch_normalization() else .}() %>% #Batch normalization
          keras::layer_dropout(rate = droprate) %>% #Adds dropout

          keras::layer_dense(units = keras_architecture_parameters$units[2],
                             activation = keras_architecture_parameters$activation[2], #Units and activation may vary by layer
                             kernel_regularizer = keras::regularizer_l1_l2(l1 = regularizer_l1, l2 = regularizer_l2)) %>%
          {if (keras_architecture_parameters$batch_norm_option[2]) keras::layer_batch_normalization() else .}() %>% #Batch normalization
          keras::layer_dropout(rate = droprate) %>% #Adds dropout

          keras::layer_dense(units = 1) #No activation means linear: f(x) = x
      },
      error = function(e){ #In case of error
        message("Failure in creating keras network. Please check if input parameters units, activation, input_shape, kernel_regularizer,
                batch_norm_option, droprate are appropriate.")
        message("Here is the original error message:")
        message(conditionMessage(e))
      }
    )
  } else {}
  #Typical NN3 Architecture
  if(keras_architecture_parameters$n_layers == 3){
    model_nn <- keras::keras_model_sequential()
    tryCatch(
      {#Try to create keras network
        model_nn %>%
          keras::layer_dense(units = keras_architecture_parameters$units[1],
                             activation = keras_architecture_parameters$activation[1], #Units and activation may vary by layer
                             input_shape =  ncol(features_matrix_train_clean), #Shape = # of features
                             kernel_regularizer = keras::regularizer_l1_l2(l1 = regularizer_l1, l2 = regularizer_l2)) %>% #L1 and L2 Regularization
          {if (keras_architecture_parameters$batch_norm_option[1]) keras::layer_batch_normalization() else .}() %>% #Batch normalization
          keras::layer_dropout(rate = droprate) %>% #Adds dropout

          keras::layer_dense(units = keras_architecture_parameters$units[2],
                             activation = keras_architecture_parameters$activation[2], #Units and activation may vary by layer
                             kernel_regularizer = keras::regularizer_l1_l2(l1 = regularizer_l1, l2 = regularizer_l2)) %>%
          {if (keras_architecture_parameters$batch_norm_option[2]) keras::layer_batch_normalization() else .}() %>% #Batch normalization
          keras::layer_dropout(rate = droprate) %>% #Adds dropout

          keras::layer_dense(units = keras_architecture_parameters$units[3],
                             activation = keras_architecture_parameters$activation[3], #Units and activation may vary by layer
                             kernel_regularizer = keras::regularizer_l1_l2(l1 = regularizer_l1, l2 = regularizer_l2)) %>%
          {if (keras_architecture_parameters$batch_norm_option[3]) keras::layer_batch_normalization() else .}() %>% #Batch normalization
          keras::layer_dropout(rate = droprate) %>% #Adds dropout

          keras::layer_dense(units = 1) #No activation means linear: f(x) = x
      },
      error = function(e){ #In case of error
        message("Failure in creating keras network. Please check if input parameters units, activation, input_shape, kernel_regularizer,
                batch_norm_option, droprate are appropriate.")
        message("Here is the original error message:")
        message(conditionMessage(e))
      }
    )
  } else {}
  #Typical NN4 Architecture
  if(keras_architecture_parameters$n_layers == 4){
    model_nn <- keras::keras_model_sequential()
    tryCatch(
      {#Try to create keras network
        model_nn %>%
          keras::layer_dense(units = keras_architecture_parameters$units[1],
                             activation = keras_architecture_parameters$activation[1], #Units and activation may vary by layer
                             input_shape =  ncol(features_matrix_train_clean), #Shape = # of features
                             kernel_regularizer = keras::regularizer_l1_l2(l1 = regularizer_l1, l2 = regularizer_l2)) %>% #L1 and L2 Regularization
          {if (keras_architecture_parameters$batch_norm_option[1]) keras::layer_batch_normalization() else .}() %>% #Batch normalization
          keras::layer_dropout(rate = droprate) %>% #Adds dropout

          keras::layer_dense(units = keras_architecture_parameters$units[2],
                             activation = keras_architecture_parameters$activation[2], #Units and activation may vary by layer
                             kernel_regularizer = keras::regularizer_l1_l2(l1 = regularizer_l1, l2 = regularizer_l2)) %>%
          {if (keras_architecture_parameters$batch_norm_option[2]) keras::layer_batch_normalization() else .}() %>% #Batch normalization
          keras::layer_dropout(rate = droprate) %>% #Adds dropout

          keras::layer_dense(units = keras_architecture_parameters$units[3],
                             activation = keras_architecture_parameters$activation[3], #Units and activation may vary by layer
                             kernel_regularizer = keras::regularizer_l1_l2(l1 = regularizer_l1, l2 = regularizer_l2)) %>%
          {if (keras_architecture_parameters$batch_norm_option[3]) keras::layer_batch_normalization() else .}() %>% #Batch normalization
          keras::layer_dropout(rate = droprate) %>% #Adds dropout

          keras::layer_dense(units = keras_architecture_parameters$units[4],
                             activation = keras_architecture_parameters$activation[4], #Units and activation may vary by layer
                             kernel_regularizer = keras::regularizer_l1_l2(l1 = regularizer_l1, l2 = regularizer_l2)) %>%
          {if (keras_architecture_parameters$batch_norm_option[4]) keras::layer_batch_normalization() else .}() %>% #Batch normalization
          keras::layer_dropout(rate = droprate) %>% #Adds dropout

          keras::layer_dense(units = 1) #No activation means linear: f(x) = x
      },
      error = function(e){ #In case of error
        message("Failure in creating keras network. Please check if input parameters units, activation, input_shape, kernel_regularizer,
                batch_norm_option, droprate are appropriate.")
        message("Here is the original error message:")
        message(conditionMessage(e))
      }
    )
  } else {}
  #Typical NN5 Architecture
  if(keras_architecture_parameters$n_layers == 5){
    model_nn <- keras::keras_model_sequential()
    tryCatch(
      {#Try to create keras network
        model_nn %>%
          keras::layer_dense(units = keras_architecture_parameters$units[1],
                             activation = keras_architecture_parameters$activation[1], #Units and activation may vary by layer
                             input_shape =  ncol(features_matrix_train_clean), #Shape = # of features
                             kernel_regularizer = keras::regularizer_l1_l2(l1 = regularizer_l1, l2 = regularizer_l2)) %>% #L1 and L2 Regularization
          {if (keras_architecture_parameters$batch_norm_option[1]) keras::layer_batch_normalization() else .}() %>% #Batch normalization
          keras::layer_dropout(rate = droprate) %>% #Adds dropout

          keras::layer_dense(units = keras_architecture_parameters$units[2],
                             activation = keras_architecture_parameters$activation[2], #Units and activation may vary by layer
                             kernel_regularizer = keras::regularizer_l1_l2(l1 = regularizer_l1, l2 = regularizer_l2)) %>%
          {if (keras_architecture_parameters$batch_norm_option[2]) keras::layer_batch_normalization() else .}() %>% #Batch normalization
          keras::layer_dropout(rate = droprate) %>% #Adds dropout

          keras::layer_dense(units = keras_architecture_parameters$units[3],
                             activation = keras_architecture_parameters$activation[3], #Units and activation may vary by layer
                             kernel_regularizer = keras::regularizer_l1_l2(l1 = regularizer_l1, l2 = regularizer_l2)) %>%
          {if (keras_architecture_parameters$batch_norm_option[3]) keras::layer_batch_normalization() else .}() %>% #Batch normalization
          keras::layer_dropout(rate = droprate) %>% #Adds dropout

          keras::layer_dense(units = keras_architecture_parameters$units[4],
                             activation = keras_architecture_parameters$activation[4], #Units and activation may vary by layer
                             kernel_regularizer = keras::regularizer_l1_l2(l1 = regularizer_l1, l2 = regularizer_l2)) %>%
          {if (keras_architecture_parameters$batch_norm_option[4]) keras::layer_batch_normalization() else .}() %>% #Batch normalization
          keras::layer_dropout(rate = droprate) %>% #Adds dropout

          keras::layer_dense(units = keras_architecture_parameters$units[5],
                             activation = keras_architecture_parameters$activation[5], #Units and activation may vary by layer
                             kernel_regularizer = keras::regularizer_l1_l2(l1 = regularizer_l1, l2 = regularizer_l2)) %>%
          {if (keras_architecture_parameters$batch_norm_option[5]) keras::layer_batch_normalization() else .}() %>% #Batch normalization
          keras::layer_dropout(rate = droprate) %>% #Adds dropout

          keras::layer_dense(units = 1) #No activation means linear: f(x) = x
      },
      error = function(e){ #In case of error
        message("Failure in creating keras network. Please check if input parameters units, activation, input_shape, kernel_regularizer,
                batch_norm_option, droprate are appropriate.")
        message("Here is the original error message:")
        message(conditionMessage(e))
      }
    )
  } else {}


  #Backpropagation
  tryCatch(
    {#Try to compile keras model
      model_nn %>% keras::compile( #Model Specification
        #Loss function
        loss = custom_objective_translated,
        #Optimization method and learning rate
        optimizer = switch(keras_architecture_parameters$nn_optimizer,
                           "Adam" = keras::optimizer_adam(learning_rate = lr),
                           "RMSProp" = keras::optimizer_rmsprop(learning_rate = lr),
                           keras::optimizer_adam(learning_rate = lr)
        ),
        #Custom eval metric translated
        metrics = chosen_eval_metric_translated$metric
      )
    },
    error = function(e){ #In case of error
      message("Failure in compiling keras model. Please check if input parameters custom_objective, nn_optimizer, chosen_eval_metric and huber_delta are appropriate.")
      message("Here is the original error message:")
      message(conditionMessage(e))
    }
  )

  #Fit
  tryCatch(
    {
      if(is.null(early_stop)){
        #In case no early_stop
        fit_nn <- model_nn %>% #Keras models, unlike many R objects, are mutable objects. Piping after calling a model will alter it. Sucessive trainings then do not start from scratch.
          keras::fit(x = as.matrix(features_matrix_train_clean), #Training features
                     y = target_vector_train, #Training label
                     epochs = number_of_epochs, #Number of epochs
                     batch_size = size_of_batch, #Batch size (should be a multiple of 2)
                     verbose = FALSE
          )


      } else {
        #In case of early_stop
        fit_nn <- model_nn %>% #Keras models, unlike many R objects, are mutable objects. Piping after calling a model will alter it. Sucessive trainings then do not start from scratch.
          keras::fit(x = as.matrix(features_matrix_train_clean), #Training features
                     y = target_vector_train, #Training label
                     epochs = number_of_epochs, #Number of epochs
                     batch_size = size_of_batch, #Batch size (should be a multiple of 2)
                     verbose = FALSE,
                     callbacks = list(keras::callback_early_stopping(monitor = chosen_eval_metric_translated$name,
                                                                     patience = early_stop, #Early stop (nº epochs with no improvement)
                                                                     restore_best_weights = TRUE, #Restore best weights after stopping
                                                                     mode = chosen_eval_metric_translated$mode)), #Min for RMSE, MAE and HUBER
                     validation_data = list(as.matrix(features_validation_sample_clean), target_validation_sample) #Validation data
          )

      }
    },
    error = function(e){ #In case of error
      message("Failure in fitting keras model. Please check if input parameters features, targets,
              number_of_epochs, batch_size and early_stop are appropriate.")
      message("Here is the original error message:")
      message(conditionMessage(e))
    }
  )



  return(list(model_nn = model_nn,
              fit_nn = fit_nn))

}


