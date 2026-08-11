#' Fit a Keras Neural Network Model
#'
#' @description
#' Builds and trains a feed-forward Keras neural network (1--5 dense layers) for signal
#' blending, given hyperparameters, an architecture specification, and loss /
#' early-stopping settings. Used both during tuning (with a validation set for early
#' stopping) and at refit time (no early stopping).
#'
#' @details
#' Each hidden layer applies L1/L2 kernel regularization, optional batch normalization,
#' and dropout; the output layer is a single linear unit (regression). The Keras
#' session is cleared via \code{on.exit()} after each call to bound memory growth across
#' the many refits of a walk-forward backtest. Note that Keras models are mutable:
#' re-fitting the same object continues training rather than starting fresh.
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
#' @param n_ensembles Integer >= 1. Number of independently initialised networks to train
#'   and average. Each member sees the same data and the same hyperparameters and differs
#'   only in its random weight initialisation and its own early-stopping decision;
#'   forecasts are averaged, never weights. Defaults to 1, which trains a single network
#'   and returns exactly what earlier versions returned. Values above 1 follow Gu, Kelly
#'   and Xiu (2020), who average 10 networks per topology, and Rubesam (2021), who
#'   averages 50, both to remove the variance a single random initialisation injects into
#'   the forecast.
#'
#'   This is a deliberate function argument rather than a field read off
#'   \code{keras_architecture_parameters}: the tuning path reaches this function with the
#'   same architecture specification as the refit, so reading it from there would make
#'   every candidate evaluation ensemble too and multiply the search cost by
#'   \code{n_ensembles}. Only the refit passes it.
#' @param ... Additional arguments consumed only when early stopping is active:
#'   \code{features_validation_sample_clean}, \code{target_validation_sample}, and
#'   \code{chosen_eval_metric_translated} (its \code{$name}/\code{$mode} configure
#'   \code{keras::callback_early_stopping()}).
#'
#' @return A list containing:
#'   \item{model_nn}{When \code{n_ensembles == 1}, the trained Keras model object. When
#'     \code{n_ensembles > 1}, a \code{\linkS4class{keras_ensemble}} holding the trained members,
#'     which predicts as their average.}
#'   \item{fit_nn}{When \code{n_ensembles == 1}, the Keras training \code{history} object
#'     (per-epoch metrics). When \code{n_ensembles > 1}, a list of one such history per
#'     member, since members may stop at different epochs.}
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
                            n_ensembles = 1, #Initialisation ensembling (refit only)
                            ...
){

  #Clear the session after each model training
  ###Registered once, at the level of the whole call, so that the session is
  ###cleared only after every ensemble member has been trained. Clearing between
  ###members would tear down state while earlier members are still referenced.
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

  #Validate n_ensembles
  ###Absent means the historical single-network behaviour. A fractional or
  ###vector-valued count would otherwise be rounded or recycled silently inside
  ###the training loop.
  if (is.null(n_ensembles)) {
    n_ensembles <- 1
  }
  if (!is.numeric(n_ensembles) || length(n_ensembles) != 1L || is.na(n_ensembles) ||
      !is.finite(n_ensembles) || n_ensembles < 1 || n_ensembles != round(n_ensembles)) {
    stop("n_ensembles should be a single finite integer >= 1.")
  }

  #Train one independently initialised member per iteration
  ###With n_ensembles == 1 this runs the body exactly once and returns the same
  ###objects, in the same order, as before this argument existed. Nothing about
  ###the single-network path is conditional on the ensembling code.
  ensemble_members <- vector("list", n_ensembles)
  ensemble_histories <- vector("list", n_ensembles)

  for (ensemble_index in seq_len(n_ensembles)) {

  ###Reset per member so that a failed build or fit cannot silently carry the
  ###previous member's model or history forward into this member's slot.
  model_nn <- NULL
  fit_nn <- NULL

  ###The block below is the original single-network body, left at its previous
  ###indentation so the change reads as a wrapper rather than a rewrite. It runs
  ###once per member; each pass builds a fresh network, so weights are drawn
  ###independently and each member early-stops on its own.

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
          {if (keras_architecture_parameters$batch_norm_option[1]) keras::layer_batch_normalization(.) else .} %>% #Batch normalization
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
          {if (keras_architecture_parameters$batch_norm_option[1]) keras::layer_batch_normalization(.) else .} %>% #Batch normalization
          keras::layer_dropout(rate = droprate) %>% #Adds dropout

          keras::layer_dense(units = keras_architecture_parameters$units[2],
                             activation = keras_architecture_parameters$activation[2], #Units and activation may vary by layer
                             kernel_regularizer = keras::regularizer_l1_l2(l1 = regularizer_l1, l2 = regularizer_l2)) %>%
          {if (keras_architecture_parameters$batch_norm_option[2]) keras::layer_batch_normalization(.) else .} %>% #Batch normalization
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
          {if (keras_architecture_parameters$batch_norm_option[1]) keras::layer_batch_normalization(.) else .} %>% #Batch normalization
          keras::layer_dropout(rate = droprate) %>% #Adds dropout

          keras::layer_dense(units = keras_architecture_parameters$units[2],
                             activation = keras_architecture_parameters$activation[2], #Units and activation may vary by layer
                             kernel_regularizer = keras::regularizer_l1_l2(l1 = regularizer_l1, l2 = regularizer_l2)) %>%
          {if (keras_architecture_parameters$batch_norm_option[2]) keras::layer_batch_normalization(.) else .} %>% #Batch normalization
          keras::layer_dropout(rate = droprate) %>% #Adds dropout

          keras::layer_dense(units = keras_architecture_parameters$units[3],
                             activation = keras_architecture_parameters$activation[3], #Units and activation may vary by layer
                             kernel_regularizer = keras::regularizer_l1_l2(l1 = regularizer_l1, l2 = regularizer_l2)) %>%
          {if (keras_architecture_parameters$batch_norm_option[3]) keras::layer_batch_normalization(.) else .} %>% #Batch normalization
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
          {if (keras_architecture_parameters$batch_norm_option[1]) keras::layer_batch_normalization(.) else .} %>% #Batch normalization
          keras::layer_dropout(rate = droprate) %>% #Adds dropout

          keras::layer_dense(units = keras_architecture_parameters$units[2],
                             activation = keras_architecture_parameters$activation[2], #Units and activation may vary by layer
                             kernel_regularizer = keras::regularizer_l1_l2(l1 = regularizer_l1, l2 = regularizer_l2)) %>%
          {if (keras_architecture_parameters$batch_norm_option[2]) keras::layer_batch_normalization(.) else .} %>% #Batch normalization
          keras::layer_dropout(rate = droprate) %>% #Adds dropout

          keras::layer_dense(units = keras_architecture_parameters$units[3],
                             activation = keras_architecture_parameters$activation[3], #Units and activation may vary by layer
                             kernel_regularizer = keras::regularizer_l1_l2(l1 = regularizer_l1, l2 = regularizer_l2)) %>%
          {if (keras_architecture_parameters$batch_norm_option[3]) keras::layer_batch_normalization(.) else .} %>% #Batch normalization
          keras::layer_dropout(rate = droprate) %>% #Adds dropout

          keras::layer_dense(units = keras_architecture_parameters$units[4],
                             activation = keras_architecture_parameters$activation[4], #Units and activation may vary by layer
                             kernel_regularizer = keras::regularizer_l1_l2(l1 = regularizer_l1, l2 = regularizer_l2)) %>%
          {if (keras_architecture_parameters$batch_norm_option[4]) keras::layer_batch_normalization(.) else .} %>% #Batch normalization
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
          {if (keras_architecture_parameters$batch_norm_option[1]) keras::layer_batch_normalization(.) else .} %>% #Batch normalization
          keras::layer_dropout(rate = droprate) %>% #Adds dropout

          keras::layer_dense(units = keras_architecture_parameters$units[2],
                             activation = keras_architecture_parameters$activation[2], #Units and activation may vary by layer
                             kernel_regularizer = keras::regularizer_l1_l2(l1 = regularizer_l1, l2 = regularizer_l2)) %>%
          {if (keras_architecture_parameters$batch_norm_option[2]) keras::layer_batch_normalization(.) else .} %>% #Batch normalization
          keras::layer_dropout(rate = droprate) %>% #Adds dropout

          keras::layer_dense(units = keras_architecture_parameters$units[3],
                             activation = keras_architecture_parameters$activation[3], #Units and activation may vary by layer
                             kernel_regularizer = keras::regularizer_l1_l2(l1 = regularizer_l1, l2 = regularizer_l2)) %>%
          {if (keras_architecture_parameters$batch_norm_option[3]) keras::layer_batch_normalization(.) else .} %>% #Batch normalization
          keras::layer_dropout(rate = droprate) %>% #Adds dropout

          keras::layer_dense(units = keras_architecture_parameters$units[4],
                             activation = keras_architecture_parameters$activation[4], #Units and activation may vary by layer
                             kernel_regularizer = keras::regularizer_l1_l2(l1 = regularizer_l1, l2 = regularizer_l2)) %>%
          {if (keras_architecture_parameters$batch_norm_option[4]) keras::layer_batch_normalization(.) else .} %>% #Batch normalization
          keras::layer_dropout(rate = droprate) %>% #Adds dropout

          keras::layer_dense(units = keras_architecture_parameters$units[5],
                             activation = keras_architecture_parameters$activation[5], #Units and activation may vary by layer
                             kernel_regularizer = keras::regularizer_l1_l2(l1 = regularizer_l1, l2 = regularizer_l2)) %>%
          {if (keras_architecture_parameters$batch_norm_option[5]) keras::layer_batch_normalization(.) else .} %>% #Batch normalization
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



  ###Collect this member and move on to the next initialisation.
  ###Single-bracket assignment with a list() wrapper, because `[[<-` with a NULL
  ###value deletes the element instead of storing it, which would silently
  ###shorten the ensemble when a member fails to build or fit.
  ensemble_members[ensemble_index] <- list(model_nn)
  ensemble_histories[ensemble_index] <- list(fit_nn)

  ###The build, compile and fit steps above report failures through message()
  ###and carry on, so a member can reach this point untrained. Before this loop
  ###existed such a failure surfaced as an unassigned `fit_nn` and the function
  ###aborted; failing here keeps that loud behaviour and says which member and
  ###which stage broke, instead of returning a silently unusable model.
  if (is.null(model_nn)) {
    stop("Network construction failed for ensemble member ", ensemble_index,
         " of ", n_ensembles, ". See the messages above for the original Keras error.")
  }
  if (is.null(fit_nn)) {
    stop("Training failed for ensemble member ", ensemble_index,
         " of ", n_ensembles, ". See the messages above for the original Keras error.")
  }

  } #End of the per-member loop


  #Return
  ##Single network: the historical return value, unchanged
  if (n_ensembles == 1) {
    return(list(model_nn = ensemble_members[[1]],
                fit_nn = ensemble_histories[[1]]))
  }

  ##Ensemble: same two-element shape, so every existing caller that reads
  ##`$model_nn` keeps working; only the class of the model changes, and the
  ##keras_ensemble predict method averages the members' forecasts.
  return(list(model_nn = methods::new("keras_ensemble", members = ensemble_members),
              fit_nn = ensemble_histories))

}


