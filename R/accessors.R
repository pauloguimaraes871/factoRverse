# Define generic accessor methods for ml_wf_val_results
##########################################################
setGeneric("get_oos_prediction_list", function(object) standardGeneric("get_oos_prediction_list"))
setGeneric("get_oos_error_list", function(object) standardGeneric("get_oos_error_list"))
setGeneric("get_oos_y_list", function(object) standardGeneric("get_oos_y_list"))
setGeneric("get_oos_testing_eval_metrics", function(object) standardGeneric("get_oos_testing_eval_metrics"))
setGeneric("get_final_model", function(object) standardGeneric("get_final_model"))
setGeneric("get_chosen_eval_metric_validation", function(object) standardGeneric("get_chosen_eval_metric_validation"))
setGeneric("get_best_hyperparameters", function(object) standardGeneric("get_best_hyperparameters"))
setGeneric("get_validation_eval_metrics_hyper_choice", function(object) standardGeneric("get_validation_eval_metrics_hyper_choice"))
setGeneric("get_metadata", function(object) standardGeneric("get_metadata"))

# Define methods for accessing the slots
setMethod("get_oos_prediction_list", "ml_wf_val_results", function(object) {
  return(object@oos_prediction_list)
})

setMethod("get_oos_error_list", "ml_wf_val_results", function(object) {
  return(object@oos_error_list)
})

setMethod("get_oos_y_list", "ml_wf_val_results", function(object) {
  return(object@oos_y_list)
})

setMethod("get_oos_testing_eval_metrics", "ml_wf_val_results", function(object) {
  return(object@oos_testing_eval_metrics)
})

setMethod("get_final_model", "ml_wf_val_results", function(object) {
  return(object@final_model)
})

setMethod("get_chosen_eval_metric_validation", "ml_wf_val_results", function(object) {
  return(object@chosen_eval_metric_validation)
})

setMethod("get_best_hyperparameters", "ml_wf_val_results", function(object) {
  return(object@best_hyperparameters)
})

setMethod("get_validation_eval_metrics_hyper_choice", "ml_wf_val_results", function(object) {
  return(object@validation_eval_metrics_hyper_choice)
})

setMethod("get_metadata", "ml_wf_val_results", function(object) {
  return(object@metadata)
})
##########################################################


# Define generic accessor methods for meta_dataframe
#########################################################
setGeneric("get_data", function(object) standardGeneric("get_data"))
setGeneric("get_workflow", function(object) standardGeneric("get_workflow"))

# Define methods for accessing the slots
setMethod("get_data", "meta_dataframe", function(object) {
  return(object@data)
})

setMethod("get_workflow", "meta_dataframe", function(object) {
  return(object@workflow)
})
#########################################################




# Define generic accessor methods for refit_ml_model
#########################################################
setGeneric("get_ml_algorithm", function(object) standardGeneric("get_ml_algorithm"))
setGeneric("get_best_hyperparameters", function(object) standardGeneric("get_best_hyperparameters"))
setGeneric("get_model", function(object) standardGeneric("get_model"))

# Define methods for accessing the slots
setMethod("get_ml_algorithm", "refit_ml_model", function(object) {
  return(object@ml_algorithm)
})

setMethod("get_best_hyperparameters", "refit_ml_model", function(object) {
  return(object@best_hyperparameters)
})

setMethod("get_model", "refit_ml_model", function(object) {
  return(object@model)
})
#########################################################
