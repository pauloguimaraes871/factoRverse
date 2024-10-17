#' Create a meta_dataframe Object
#'
#' This function creates an object of class \code{meta_dataframe} from a provided data frame.
#' The data frame must meet specific requirements: it must contain 'id', 'tickers', and 'dates' columns,
#' where 'dates' must be of class \code{Date} and sorted in ascending order. The 'id' column should
#' be constructed as \code{paste0(tickers, "-", dates)}. The function also validates that there are no
#' missing dates, duplicated IDs, or NA values in the required columns.
#'
#' @param data A \code{data.frame} containing the data to be converted to a \code{meta_dataframe}.
#'
#' @return An object of class \code{meta_dataframe} if the input data frame meets all validation criteria.
#' The returned object includes metadata such as the number of unique dates, unique tickers, and
#' total number of observations.
#'
#' @details
#' - The 'id' column is expected to be in the format of \code{paste0(tickers, "-", dates)}.
#' - The 'dates' column must be of class \code{Date} and in ascending chronological order.
#' - The function checks for NA values in the 'id', 'tickers', and 'dates' columns.
#' - The function ensures that there are no gaps in the dates sequence and no duplicated IDs.
#' - The metadata includes the number of unique dates, unique tickers, and total observations.
#'
#' @examples
#' # Create a sample data frame
#' df <- data.frame(
#'   id = c("A-2024-01-01", "B-2024-02-01"),
#'   tickers = c("A", "B"),
#'   dates = as.Date(c("2024-01-01", "2024-02-01")),
#'   value = c(10, 20)
#' )
#'
#' # Create a meta_dataframe object
#' meta_df <- create_meta_dataframe(df)
#'
#' @export
create_meta_dataframe <- function(data) {
  if (!is.data.frame(data)) {
    stop("Input must be a data.frame")
  }

  required_columns <- c("id", "tickers", "dates")
  if (!all(required_columns %in% names(data))) {
    stop("Data must contain 'id', 'tickers', and 'dates' columns")
  }

  # Check for NA values in the required columns
  if (any(is.na(data[required_columns]))) {
    stop("Columns 'id', 'tickers', or 'dates' contain NA values")
  }

  #Check dates format
  if (!inherits(data$dates, "Date")) {
    stop("The 'dates' column must be of class 'Date'")
  }

  if (!all(diff(unique(data$dates)[order(unique(data$dates))]) >= 0)) {
    stop("Dates must be in ascending chronological order")
  }

  #Check tickers format
  if(any(!is.character(data$tickers))){
    stop("Tickers must be of class character")
  }

  # Check for NA values in remaining columns and report them
  remaining_columns <- setdiff(names(data), required_columns)
  na_remaining <- sapply(data[, remaining_columns], function(col) any(is.na(col)))
  if (any(na_remaining)) {
    message("The following columns contain NA values: ",
            paste(remaining_columns[na_remaining], collapse = ", "))
  }

  # Ensure the 'id' column matches paste0(tickers, "-", dates)
  expected_id <- paste0(data$tickers, "-", data$dates)
  if (!all(data$id == expected_id)) {
    stop("The 'id' column does not match 'tickers-dates')")
  }
  # Ensure no gaps in the dates sequence
  unique_dates <- unique(data$dates)
  full_dates <- seq(min(unique_dates), max(unique_dates), by = "month")
  missing_dates <- setdiff(full_dates, unique_dates)

  if (length(missing_dates) > 0) {
    warning("There are gaps in the dates sequence. Missing dates: ", paste(as.Date(missing_dates), collapse = ", "))
  }

  if (any(duplicated(data$id))) {
    stop("ID column contains duplicated values")
  }

  # Check for NA values in remaining columns and report them
  remaining_columns <- setdiff(names(data), required_columns)
  na_remaining <- sapply(obj[, remaining_columns], function(col) any(is.na(col)))
  if (any(duplicated(remaining_columns))) {
    stop("Column names for variables must be unique")
  }

  # Calculate metadata
  unique_dates_count <- length(unique(data$dates))
  unique_tickers_count <- length(unique(data$tickers))
  total_observations_count <- nrow(data)

  # Initialize workflow slot as an empty list
  # Store metadata and column names
  new("meta_dataframe",
      data = data,
      workflow = list(),
      signals = names(data)[-c(1:3)],
      unique_dates = unique_dates_count,
      unique_tickers = unique_tickers_count,
      n_obs = total_observations_count)
}




#' Create a `hyper_grid_domain` Object
#'
#' This function creates an instance of the `hyper_grid_domain` S4 class.
#' It allows users to specify the tuning method, machine learning algorithm, and relevant hyperparameters.
#'
#' @param tuning_method A character string specifying the tuning method to be applied.
#'                     Must be one of "grid_search", "random_search", or "bayesian_optimization".
#' @param ml_algorithm A character string specifying the machine-learning algorithm to be used.
#'                     Must be one of "glmnet", "rf", "xgb", "nn", or "ols".
#' @param hyperparameters A named list representing the hyperparameter to be added.
#'  #' @seealso \code{\link{add_hyperparameter}} for more details.
#'
#' @return An instance of the `hyper_grid_domain` S4 class.
#'
#' @examples
#' # Creating a hyper_grid_domain object for a random forest model
#' hyper_grid_rf <- create_hyper_grid_domain(
#'   tuning_method = "grid_search",
#'   ml_algorithm = "rf"
#' )
#'
#'
#' @export
create_hyper_grid_domain <- function(tuning_method, ml_algorithm, hyperparameters = NULL) {

  hyperparameter_list <- list()


  # Ensure hyperparameters is a list
  if (!is.list(hyperparameters) & !is.null(hyperparameters)) {
    stop("hyperparameters must be a list.")
  }

  # Validate hyperparameters based on tuning_method
  if (tuning_method == "grid_search") {
    if (!all(sapply(hyperparameters, function(x) is.numeric(x) && is.vector(x)))) {
      stop("For 'grid_search', hyperparameters must be a list of numeric vectors.")
    }
  } else if (tuning_method == "random_search") {
    for (name in names(hyperparameters)) {
      if (!is.list(hyperparameters[[name]]) || !all(c("distribution_choice") %in% names(hyperparameters[[name]]))) {
        stop("For 'random_search', each hyperparameters must be a list with 'distribution_choice'.")
      }

      distribution_choice <- hyperparameters[[name]]$distribution_choice

      if (is.null(distribution_choice) || !(distribution_choice %in% c("normal", "uniform", "lognormal", "constant"))) {
        stop("distribution_choice must be one of 'normal', 'uniform', 'lognormal', or 'constant'.")
      }

      if (distribution_choice == "constant") {
        if (is.null(hyperparameters[[name]]$value) || !is.numeric(hyperparameters[[name]]$value)) {
          stop("For 'constant', the second argument must be a numeric vector named 'value'.")
        }
      } else {
        if (!is.null(hyperparameters[[name]]$value)) {
          stop("For distributions other than 'constant', do not specify 'value'.")
        }
        pars <- hyperparameters[[name]]$pars
        if (is.null(pars) || !is.numeric(pars) || !is.vector(pars)) {
          stop("For distributions, the second argument must be a numeric vector named 'pars'.")
        }

        # Additional checks based on distribution_choice
        if (distribution_choice == "normal") {
          if (!all(names(pars) %in% c("mean", "sd"))) {
            stop("For 'normal', 'pars' must have names 'mean' and 'sd'.")
          }
        } else if (distribution_choice == "uniform") {
          if (!all(names(pars) %in% c("min", "max"))) {
            stop("For 'uniform', 'pars' must have names 'min' and 'max'.");
          }
        } else if (distribution_choice == "lognormal") {
          if (!all(names(pars) %in% c("meanlog", "sdlog"))) {
            stop("For 'lognormal', 'pars' must have names 'meanlog' and 'sdlog'.");
          }
        }
      }
    }
  } else if (tuning_method == "bayesian_opt") {
    if (any(sapply(hyperparameters, function(x) !is.numeric(x) || length(x) != 2))) {
      stop("For 'bayesian_opt', each hyperparameters must be a numeric vector of length 2 representing the bounds.")
    }
  } else {
    stop("Invalid tuning_method. Only 'grid_search', 'random_search', and 'bayesian_opt' are supported.")
  }


  # Check hyperparameters validity based on ml_algorithm
  hyperparameters_names <- names(hyperparameters)

  # GLMNET
  if (ml_algorithm == "glmnet" && !all(hyperparameters_names %in% c("alpha", "lambda.min.ratio"))) {
    stop("new_hyperparameters do not match ml_algorithm choice for 'glmnet'")
  }

  # RF
  if (ml_algorithm == "rf" && !all(hyperparameters_names %in% c("mtry", "num.trees", "max.depth", "min.bucket"))) {
    stop("new_hyperparameters do not match ml_algorithm choice for 'rf'")
  }

  # XGB
  if (ml_algorithm == "xgb" && !all(hyperparameters_names %in% c("min_child_weight", "max_depth", "subsample", "colsample_bytree",
                                                                     "eta", "alpha", "gamma", "nrounds"))) {
    stop("new_hyperparameters do not match ml_algorithm choice for 'xgb'")
  }

  # NN
  if (ml_algorithm == "nn" && !all(hyperparameters_names %in% c("regularizer_l1", "regularizer_l2", "droprate", "lr",
                                                                    "size_of_batch", "number_of_epochs"))) {
    stop("new_hyperparameters do not match ml_algorithm choice for 'nn'")
  }

  if(!is.null(hyperparameters)){
  # Overwrite existing hyperparameters if they are duplicates
  for (l in 1:length(hyperparameters)) {
    hyperparameter_list[[l]] <- hyperparameters[[l]]
  }}

  names(hyperparameter_list) <- hyperparameters_names

  new("hyper_grid_domain",
      tuning_method = tuning_method,
      ml_algorithm = ml_algorithm,
      hyperparameter_list = hyperparameter_list)
}


#' Add a Hyperparameter to a `hyper_grid_domain`
#'
#' This method adds a new hyperparameter to an existing `hyper_grid_domain` object.
#'
#' @param hyper_grid_domain A `hyper_grid_domain` object to which the hyperparameter will be added.
#' @param new_hyperparameters A named list representing the hyperparameter to be added.
#'
#' @return A new `hyper_grid_domain` object with the updated hyperparameters.
#'
#' @examples
#' # Create an initial hyper_grid_domain object for grid-search
#' hyper_grid <- create_hyper_grid_domain(
#'   tuning_method = "grid_search",
#'   ml_algorithm = "glmnet"
#' )
#'
#' #Add hyperparameter for grid_search
#' hyper_grid <- add_hyperparameter(
#' hyper_grid_domain = hyper_grid,
#' new_hyperparameters = list(alpha = c(0.2, 0.5)) #Hyperparameters may be added one by one or all at once (eg.: list(alpha = c(0.2, 0.5), lambda.min.ratio = c(0.5, 0.2, 0.3)))
#' )
#'
#' random_search and xgb algorithm
#' hyper_grid <- create_hyper_grid_domain(
#'   tuning_method = "random_search",
#'   ml_algorithm = "xgb"
#' )
#'
#' hyper_grid <- add_hyperparameter(
#' hyper_grid_domain = hyper_grid,
#' new_hyperparameters =
#' list(min_child_weight = list(distribution_choice = "constant", value = 3),
#'      max_depth = list(distribution_choice = "uniform", pars = c(min = 1L, max = 2L)),
#'      subsample = list(distribution_choice = "uniform", pars = c(min = 0.25, max = 0.50)),
#'      colsample_bytree = list(distribution_choice = "uniform", pars = c(min = 0.25, max = 0.50)),
#'      eta = list(distribution_choice = "uniform", pars = c(min = 0.1, max = 0.2)),
#'      alpha = list(distribution_choice = "uniform", pars = c(min = 2, max = 5)),
#'      gamma = list(distribution_choice = "constant", value = 0),
#'      nrounds = list(distribution_choice = "uniform", pars = c(min = 200L, max = 500L)))
#' )
#'
#' bayesian_opt and rf algorithm
#' #' hyper_grid <- create_hyper_grid_domain(
#'   tuning_method = "bayesian_opt",
#'   ml_algorithm = "rf"
#' )
#'
#'
#' hyper_grid <- add_hyperparameter(
#' hyper_grid_domain = hyper_grid,
#' new_hyperparameters =
#' list(mtry = c(0,1),
#'      num.trees = c(100L, 1000L),
#'      max.depth = c(2L, 8L),
#'      min.bucket = c(1, 5))
#' )
#'
#'
#' @export
setGeneric("add_hyperparameter", function(hyper_grid_domain, new_hyperparameters) standardGeneric("add_hyperparameter"))

#' @export
setMethod("add_hyperparameter", "hyper_grid_domain", function(hyper_grid_domain, new_hyperparameters) {

  # Ensure new_hyperparameters is a list
  if (!is.list(new_hyperparameters)) {
    stop("new_hyperparameters must be a list.")
  }

  # Get tuning_method and ml_algorithm
  tuning_method <- hyper_grid_domain@tuning_method
  ml_algorithm <- hyper_grid_domain@ml_algorithm

  # Validate new_hyperparameters based on tuning_method
  if (tuning_method == "grid_search") {
    if (!all(sapply(new_hyperparameters, function(x) is.numeric(x) && is.vector(x)))) {
      stop("For 'grid_search', new_hyperparameters must be a list of numeric vectors.")
    }
  } else if (tuning_method == "random_search") {
    for (name in names(new_hyperparameters)) {
      if (!is.list(new_hyperparameters[[name]]) || !all(c("distribution_choice") %in% names(new_hyperparameters[[name]]))) {
        stop("For 'random_search', each new_hyperparameters must be a list with 'distribution_choice'.")
      }

      distribution_choice <- new_hyperparameters[[name]]$distribution_choice

      if (is.null(distribution_choice) || !(distribution_choice %in% c("normal", "uniform", "lognormal", "constant"))) {
        stop("distribution_choice must be one of 'normal', 'uniform', 'lognormal', or 'constant'.")
      }

      if (distribution_choice == "constant") {
        if (is.null(new_hyperparameters[[name]]$value) || !is.numeric(new_hyperparameters[[name]]$value)) {
          stop("For 'constant', the second argument must be a numeric vector named 'value'.")
        }
      } else {
        if (!is.null(new_hyperparameters[[name]]$value)) {
          stop("For distributions other than 'constant', do not specify 'value'.")
        }
        pars <- new_hyperparameters[[name]]$pars
        if (is.null(pars) || !is.numeric(pars) || !is.vector(pars)) {
          stop("For distributions, the second argument must be a numeric vector named 'pars'.")
        }

        # Additional checks based on distribution_choice
        if (distribution_choice == "normal") {
          if (!all(names(pars) %in% c("mean", "sd"))) {
            stop("For 'normal', 'pars' must have names 'mean' and 'sd'.")
          }
        } else if (distribution_choice == "uniform") {
          if (!all(names(pars) %in% c("min", "max"))) {
            stop("For 'uniform', 'pars' must have names 'min' and 'max'.");
          }
        } else if (distribution_choice == "lognormal") {
          if (!all(names(pars) %in% c("meanlog", "sdlog"))) {
            stop("For 'lognormal', 'pars' must have names 'meanlog' and 'sdlog'.");
          }
        }
      }
    }
  } else if (tuning_method == "bayesian_opt") {
    if (any(sapply(new_hyperparameters, function(x) !is.numeric(x) || length(x) != 2))) {
      stop("For 'bayesian_opt', each new_hyperparameters must be a numeric vector of length 2 representing the bounds.")
    }
  } else {
    stop("Invalid tuning_method. Only 'grid_search', 'random_search', and 'bayesian_opt' are supported.")
  }


  # Check new_hyperparameters validity based on ml_algorithm
  new_hyperparameters_names <- names(new_hyperparameters)

  # GLMNET
  if (ml_algorithm == "glmnet" && !all(new_hyperparameters_names %in% c("alpha", "lambda.min.ratio"))) {
    stop("new_hyperparameters do not match ml_algorithm choice for 'glmnet'")
  }

  # RF
  if (ml_algorithm == "rf" && !all(new_hyperparameters_names %in% c("mtry", "num.trees", "max.depth", "min.bucket"))) {
    stop("new_hyperparameters do not match ml_algorithm choice for 'rf'")
  }

  # XGB
  if (ml_algorithm == "xgb" && !all(new_hyperparameters_names %in% c("min_child_weight", "max_depth", "subsample", "colsample_bytree",
                                                                     "eta", "alpha", "gamma", "nrounds"))) {
    stop("new_hyperparameters do not match ml_algorithm choice for 'xgb'")
  }

  # NN
  if (ml_algorithm == "nn" && !all(new_hyperparameters_names %in% c("regularizer_l1", "regularizer_l2", "droprate", "lr",
                                                                    "size_of_batch", "number_of_epochs"))) {
    stop("new_hyperparameters do not match ml_algorithm choice for 'nn'")
  }

  # Get current hyperparameter_list
  hyperparameter_list <- hyper_grid_domain@hyperparameter_list

  # Overwrite existing hyperparameters if they are duplicates
  for (name in names(new_hyperparameters)) {
    hyperparameter_list[[name]] <- new_hyperparameters[[name]]
  }

  # Create a new hyper_grid_domain object with the updated hyperparameter_list
  hyper_grid_domain_new <- new("hyper_grid_domain",
                               tuning_method = hyper_grid_domain@tuning_method,
                               ml_algorithm = hyper_grid_domain@ml_algorithm,
                               hyperparameter_list = hyperparameter_list
  )

  return(hyper_grid_domain_new) # Return the new object
})




#' @title Create Keras Architecture
#' @description Constructor for creating an instance of `keras_architecture_parameters`.
#'
#' @param nn_optimizer A character string specifying the optimizer to use (e.g., "adam").
#'
#' @return An object of class `keras_architecture_parameters`.
#'
#' @export
create_keras_architecture <- function(nn_optimizer, units, activation, batch_norm_option) {

  # Check nn_optimizer is valid
  valid_optimizers <- c("Adam", "RMSProp")
  if (!nn_optimizer %in% valid_optimizers) {
    stop("nn_optimizer should be Adam or RMSProp.")
  }

  # Check that units are numeric
  if (!all(is.numeric(units))) {
    stop("units should be numeric")
  }

  # Check activation functions
  valid_activations <- c("relu", "sigmoid", "softmax", "softplus", "tanh", "leaky_relu")
  if (!all(activation %in% valid_activations)) {
    stop("activation should be one of relu, sigmoid, softmax, softplus, tanh or leaky_relu.")
  }

  # Check batch_norm_option is logical
  if (!all(batch_norm_option %in% c(TRUE, FALSE))) {
    stop("batch_norm_option should be logical (TRUE or FALSE)")
  }

  if(length(units) > 5){
    warning("factoRverse only supports up to 5 layers currently")
  }

  new("keras_architecture_parameters",
      units = units,
      n_layers = length(units),
      activation = activation,
      nn_optimizer = nn_optimizer,
      batch_norm_option = batch_norm_option
  )

}



#' @title Add Layer to Keras Architecture
#' @description Method to add a new layer to the Keras architecture.
#'
#' @param object An object of class `keras_architecture_parameters`.
#' @param units A numeric value for the number of units in the new layer.
#' @param activation A character string specifying the activation function for the new layer (e.g., "relu").
#' @param batch_norm_option A character string indicating whether to apply batch normalization for the new layer (e.g., "yes").
#'
#' @return An updated object of class `keras_architecture_parameters`.
#'
#' @export
setGeneric("add_layer", function(object, units, activation, batch_norm_option) {
  standardGeneric("add_layer")
})

setMethod(
  "add_layer",
  "keras_architecture_parameters",
  function(object, units, activation, batch_norm_option) {

    # Check that units are numeric
    if (!all(is.numeric(units))) {
      stop("units should be numeric")
    }

    # Check activation functions
    valid_activations <- c("relu", "sigmoid", "softmax", "softplus", "tanh", "leaky_relu")
    if (!all(activation %in% valid_activations)) {
      stop("activation should be one of relu, sigmoid, softmax, softplus, tanh or leaky_relu.")
    }

    # Check batch_norm_option is logical
    if (!all(batch_norm_option %in% c(TRUE, FALSE))) {
      stop("batch_norm_option should be logical (TRUE or FALSE)")
    }

    # Update the layers
    object@units <- c(object@units, units)
    object@n_layers <- length(object@units)  # Update the number of layers
    object@activation <- c(object@activation, activation)
    object@batch_norm_option <- c(object@batch_norm_option, batch_norm_option)

    if(length(object@units) > 5){
      warning("factoRverse only supports up to 5 layers currently")
    }

    return(object)  # Return the updated object
  }
)


#' @title Create Portfolio Policies
#' @description Constructs a `portfolio_policies` object.
#' portfolio <- create_portfolio_policies()
create_portfolio_policies <- function() {

  # Create the S4 object
  new("portfolio_policies",
      liquidity_constraint_policy = list(),
      signal_selection_policy = list(),
      turnover_constraint_policy = list(),
      concentration_constraint_policy = list(),
      liquidity_floor_cutoffs = list())
}


#' @title Add Liquidity Constraint Policy
#' @description Adds a liquidity constraint policy to a `portfolio_policies` object.
#'
#' @param portfolio_policies_obj A `portfolio_policies` object to which the liquidity constraint policy will be added.
#' @param liquidity_floor_rule A character string representing the liquidity floor rule (optional).
#' @param liquidity_cap_rules A named numeric vector where names represent liquidity classifications and values represent liquidity caps (optional).
#'
#' @return The updated `portfolio_policies` object with the added liquidity constraint policy.
#'
#' @examples
#' portfolio <- create_portfolio_policies()
#' portfolio <- add_liquidity_constraint_policy(portfolio, liquidity_floor_rule = "micro_caps")
#' portfolio <- add_liquidity_constraint_policy(portfolio, liquidity_cap_rules = c(micro_caps = 0.01))
#' portfolio <- add_liquidity_constraint_policy(portfolio, liquidity_floor_rule = "small_caps") # This should update the liquidity_floor_rule
#'
#' @export
setGeneric("add_liquidity_constraint_policy", function(portfolio_policies_obj, liquidity_floor_rule = NULL, liquidity_cap_rules = NULL) {
  standardGeneric("add_liquidity_constraint_policy")
})

#' @export
setMethod("add_liquidity_constraint_policy", "portfolio_policies", function(portfolio_policies_obj, liquidity_floor_rule = NULL, liquidity_cap_rules = NULL) {
  # Allowed classes
  allowed_classes <- c("micro_caps", "small_caps", "mid_caps", "large_caps", "mega_caps")

  # Validate liquidity_floor_rule
  if (!is.null(liquidity_floor_rule)) {
    if (!is.character(liquidity_floor_rule) || length(liquidity_floor_rule) != 1) {
      stop("Error: liquidity_floor_rule must be a single character string.")
    }
    if (!liquidity_floor_rule %in% allowed_classes) {
      stop(sprintf("Error: liquidity_floor_rule must be one of: %s.", paste(allowed_classes, collapse = ", ")))
    }
  }

  # Validate liquidity_cap_rules
  if (!is.null(liquidity_cap_rules)) {
    if (!is.numeric(liquidity_cap_rules) || length(liquidity_cap_rules) == 0) {
      stop("Error: liquidity_cap_rules must be a non-empty named numeric vector.")
    }
    if (!all(names(liquidity_cap_rules) %in% allowed_classes)) {
      stop("Error: All names in liquidity_cap_rules must be one of: micro_caps, small_caps, mid_caps, large_caps, mega_caps.")
    }
  }

  # Initialize new liquidity_constraint_policy
  new_liquidity_constraint_policy <- list(liquidity_floor_rule = NULL)

  # Add liquidity_floor_rule
  if(is.null(portfolio_policies_obj@liquidity_constraint_policy$liquidity_floor_rule)){ #If there is not existing rule
    if(!is.null(liquidity_floor_rule)){
      new_liquidity_constraint_policy$liquidity_floor_rule <- liquidity_floor_rule #Atribute liquidity_floor-rule
    }
  } else {
    new_liquidity_constraint_policy$liquidity_floor_rule <- portfolio_policies_obj@liquidity_constraint_policy$liquidity_floor_rule #If not, keep past one
  }

  # Add liquidity_cap_rules
  if (!is.null(liquidity_cap_rules)) {
    existing_rules <- portfolio_policies_obj@liquidity_constraint_policy
    existing_liquidity_floor_cap_rules <- existing_rules[!(names(existing_rules) %in% "liquidity_floor_rule")]

    existing_liquidity_floor_cap_rules_vector <- vapply(existing_liquidity_floor_cap_rules, function(x) x$liquidity_cap, numeric(1))
    names(existing_liquidity_floor_cap_rules_vector) <- sapply(existing_liquidity_floor_cap_rules, function(x) x$liquidity_classification)

    # Combine with new rules
    final_liquidity_floor_cap_rules_vector <- c(existing_liquidity_floor_cap_rules_vector[!(names(existing_liquidity_floor_cap_rules_vector) %in% names(liquidity_cap_rules))],
                                                liquidity_cap_rules)

    # Transform to the right format
    new_liquidity_cap_rules <- setNames(
      lapply(seq_along(final_liquidity_floor_cap_rules_vector), function(i) {
        list(
          liquidity_classification = names(final_liquidity_floor_cap_rules_vector)[i],
          liquidity_cap = final_liquidity_floor_cap_rules_vector[[i]]
        )
      }),
      paste0("liquidity_cap_rule_", seq_along(final_liquidity_floor_cap_rules_vector))
    )


    # Add liquidity_cap_rules to the new policy
    new_liquidity_constraint_policy <- c(new_liquidity_constraint_policy, new_liquidity_cap_rules)
  }

  # Create the S4 object
  new_portfolio_policies_obj <- new("portfolio_policies",
                                    liquidity_constraint_policy = new_liquidity_constraint_policy,
                                    signal_selection_policy = portfolio_policies_obj@signal_selection_policy,
                                    turnover_constraint_policy = portfolio_policies_obj@turnover_constraint_policy,
                                    concentration_constraint_policy = portfolio_policies_obj@concentration_constraint_policy,
                                    liquidity_floor_cutoffs = portfolio_policies_obj@liquidity_floor_cutoffs)

  return(new_portfolio_policies_obj)
})


#' @title Add Turnover Constraint Policy
#' @description Adds a turnover constraint policy to a `portfolio_policies` object.
#'
#' @param portfolio_policies_obj A `portfolio_policies` object to which the liquidity constraint policy will be added.
#' @param turnover_rule A list with the following structure, specifying the turnover rule:
#' #' \itemize{
#'   \item \strong{liquidity_classification:} One of "micro_caps", "small_caps", "mid_caps", "large_caps" or "mega_caps"
#'   \item \strong{turnover_cap:} A number indicating the turnover_cap
#'   \item \strong{top_stock_quantile_buffer:} A number indicating the minimum signal quantile
#'   }
#'
#' @return The updated `portfolio_policies` object with the added liquidity constraint policy.
#'
#' @examples
#' portfolio <- create_portfolio_policies()
#' portfolio <- add_turnover_constraint_policy(portfolio, turnover_rule = list(liquidity_classification = "micro_caps", turnover_cap = 0.01, top_stock_quantile_buffer = 0.66))
#'
#' @export
setGeneric("add_turnover_constraint_policy", function(portfolio_policies_obj, turnover_rules) {
  standardGeneric("add_turnover_constraint_policy")
})

#' @export
setMethod("add_turnover_constraint_policy", "portfolio_policies", function(portfolio_policies_obj, turnover_rules) {

  # Allowed classes
  allowed_classes <- c("micro_caps", "small_caps", "mid_caps", "large_caps", "mega_caps")


  # Validate turnover_rules
  if(!is.list(turnover_rules)){
    stop("turnover_rules must be a list or list of lists")
  }

  ##Create function to extract vector dependengin on nesting structure
  unnest_turnover_rules <- function(argument){
    if(all(sapply(turnover_rules, function(x) is.list(x)))){
      return(turnover_rules %>% sapply(function(x) x[[argument]]))
    } else {
      return(turnover_rules[[argument]])
    }
  }

  ##liquidity classification
  liquidity_classification_vector <- unnest_turnover_rules("liquidity_classification")

  if (!all(is.character(liquidity_classification_vector))) {
    stop("Error: liquidity_classification must be a character string.")
  }
  if (!all(liquidity_classification_vector %in% allowed_classes)) {
    stop(sprintf("Error: liquidity_classification must be one of: %s.", paste(allowed_classes, collapse = ", ")))
  }

  ##turnover_cap
  turnover_cap_vector <- unnest_turnover_rules("turnover_cap")
  if(!all(is.numeric(turnover_cap_vector))){
    stop("Error: turnover_cap must be numeric")
  }

  ##top_stock_quantile_buffer
  top_stock_quantile_buffer_vector <- unnest_turnover_rules("top_stock_quantile_buffer")
  if(!all(is.numeric(top_stock_quantile_buffer_vector))){
    stop("Error: top_stock_quantile_buffer must be numeric")
  }

  # Add turnover_cap_rules
  ##Get existing rules
  existing_rules <- portfolio_policies_obj@turnover_constraint_policy
  existing_rules_names <- sapply(existing_rules, function(x) x$liquidity_classification)

  #Combine old and new rules
  if(length(existing_rules) != 0){
    #Extract existing rules not to be overwritten
    unique_existing_rules <- existing_rules[[which(!existing_rules_names %in% sapply(turnover_rules, function(x) x$liquidity_classification))]]

      ###Check if unique_existing_rules is not list of lists
      if(!all(sapply(unique_existing_rules, function(x) is.list(x)))){
        final_rules <- list(unique_existing_rules) #Create list of lists obj
      } else {
        final_rules <- unique_existing_rules #do noth
      }

    #Combine new and old
    if(all(sapply(turnover_rules, function(x) is.list(x)))){

       #If new object is a list of lists, extract individually and add
      for(new_rule in turnover_rules){
          final_rules[[length(final_rules) + 1]] <- new_rule
      }
    }
      else {
        #In case it is not a list of lists
        final_rules[[length(final_rules) + 1]] <- new_rule
      }
    }

  ###In case there are no old rules

  else {

    if(all(sapply(turnover_rules, function(x) is.list(x)))){
      final_rules <- turnover_rules
    } else {
      final_rules <- list(turnover_rules)
    }
  }

    # Transform to the right format
    n <- length(final_rules)

    # Rename
    names(final_rules) <- paste0("buffer_zone_", seq_len(n))


    # Create the S4 object
    new_portfolio_policies_obj <- new("portfolio_policies",
                                      liquidity_constraint_policy = portfolio_policies_obj@liquidity_constraint_policy,
                                      signal_selection_policy = portfolio_policies_obj@signal_selection_policy,
                                      turnover_constraint_policy = final_rules,
                                      concentration_constraint_policy = portfolio_policies_obj@concentration_constraint_policy,
                                      liquidity_floor_cutoffs = portfolio_policies_obj@liquidity_floor_cutoffs)

    return(new_portfolio_policies_obj)
  })


#' @title Add Concentration Constraint Policy
#' @description Adds a concentration constraint policy to a `portfolio_policies` object.
#'
#' @param portfolio_policies_obj A `portfolio_policies` object to which the concentration constraint policy will be added.
#' @param benchmark A character string representing benchmark based on which concentration policy will be applied.
#' @param max_abs_active_individual_weight A number indicating the absolute weight differential from benchmark weights.
#' @param max_abs_active_group_weight A named vector indicating absolute group (sector) weight differentials in relation to the benchmark.
#'
#' @return The updated `portfolio_policies` object with the added liquidity constraint policy.
#'
#' @examples
#' portfolio <- create_portfolio_policies()
#' portfolio <- add_concentration_constraint_policy(portfolio, benchmark = "IBOV", max_abs_active_individual_weight = 0.05, max_abs_active_group_weight = c(Sector = 0.1, Subsector = 0.5))
#'
#' @export
setGeneric("add_concentration_constraint_policy", function(portfolio_policies_obj, benchmark = NULL, max_abs_active_individual_weight = NULL, max_abs_active_group_weight = NULL) {
  standardGeneric("add_concentration_constraint_policy")
})

#' @export
setMethod("add_concentration_constraint_policy",  "portfolio_policies",
          function(portfolio_policies_obj, benchmark = NULL, max_abs_active_individual_weight = NULL, max_abs_active_group_weight = NULL) {

  #Validate arguments
  ##Benchmark
  if(!is.null(benchmark)){
    if(!is.character(benchmark)){
      stop("benchmark should be a character")
    }
  }

  ##Max Absolute Active Individual Weight
  if(!is.null(max_abs_active_individual_weight)){
    if(!is.numeric(max_abs_active_individual_weight)){
      stop("max_abs_active_individual_weight should be numeric")
    }
  }

  ##Max Absolute Active Group Weight
  if(!is.null(max_abs_active_group_weight)){
    if(!all(is.numeric(max_abs_active_group_weight))){
      stop("max_abs_active_group_weight should be a numeric vector")
    }
    if(is.null(names(max_abs_active_group_weight))){
      stop("max_abs_active_group_weight should include names")
    }
  }

  new_concentration_constraint_policy <- list()

  #Set benchmark
  if(!is.null(benchmark)){
    new_concentration_constraint_policy$benchmark <- benchmark
  } else {
    if(!is.null(portfolio_policies_obj@benchmark)){
      new_concentration_constraint_policy$benchmark <- portfolio_policies_obj@benchmark
    } else {
      new_concentration_constraint_policy$benchmark <- NULL
    }
  }

  #Set max_abs_active_individual_weight
  if(!is.null(max_abs_active_individual_weight)){
    new_concentration_constraint_policy$max_abs_active_individual_weight <- max_abs_active_individual_weight
  } else {
    if(!is.null(portfolio_policies_obj@max_abs_active_individual_weight)){
      new_concentration_constraint_policy$max_abs_active_individual_weight <- portfolio_policies_obj@max_abs_active_individual_weight
    } else {
      new_concentration_constraint_policy$max_abs_active_individual_weight <- NULL
    }
  }

  #Set max_abs_active_group_weight
  if(!is.null(max_abs_active_group_weight)){
    new_concentration_constraint_policy$max_abs_active_group_weight <- max_abs_active_group_weight
  } else {
    if(!is.null(portfolio_policies_obj@max_abs_active_group_weight)){
      new_concentration_constraint_policy$max_abs_active_group_weight <- portfolio_policies_obj@max_abs_active_group_weight
    } else {
      new_concentration_constraint_policy$max_abs_active_group_weight <- NULL
    }
  }

  # Create the S4 object
  new_portfolio_policies_obj <- new("portfolio_policies",
                                    liquidity_constraint_policy = portfolio_policies_obj@liquidity_constraint_policy,
                                    signal_selection_policy = portfolio_policies_obj@signal_selection_policy,
                                    turnover_constraint_policy = portfolio_policies_obj@turnover_constraint_policy,
                                    concentration_constraint_policy = new_concentration_constraint_policy,
                                    liquidity_floor_cutoffs = portfolio_policies_obj@liquidity_floor_cutoffs)

  return(new_portfolio_policies_obj)

})




#' @title Add Signal Selection Policy
#' @description Adds a signal selection policy to a `portfolio_policies` object.
#'
#' @param portfolio_policies_obj A `portfolio_policies` object to which the signal selection policy will be added.
#' @param signal_blending_method A method for blending signals.
#' @param sb_benchmark_weighting A weighting strategy for the benchmark.
#' @param max_abs_active_individual_weight A number indicating the absolute weight differential from benchmark weights.
#' @param max_abs_active_group_weight A named vector indicating absolute group (sector) weight differentials in relation to the benchmark.
#' @param p_correction_method Method for p-value correction.
#' @param chosen_signals A vector of chosen signals.
#' @param signal_positions A data structure indicating positions of signals.
#' @param signal_significance_threshold A threshold for signal significance.
#' @param chosen_informative_data A data structure with informative data.
#' @param chosen_sb_metric A metric chosen for signal blending.
#' @param priors_type Type of priors to use.
#' @param data_availability_cutoff A cutoff for data availability.
#'
#' @return The updated `portfolio_policies` object with the added signal selection policy.
#'
#' @examples
#' portfolio <- create_portfolio_policies()
#' portfolio <- add_signal_selection_policy(portfolio, signal_blending_method = "method1", ...)
#'
#' @export
setGeneric("add_signal_selection_policy", function(portfolio_policies_obj,
                                                   #Signal Selection
                                                   chosen_signals = NULL, signal_positions = NULL,
                                                   #How to blend signals?
                                                   signal_blending_method = NULL, chosen_sb_metric = NULL,
                                                   #Restrictions on weights
                                                   sb_benchmark_weighting_method = NULL, max_abs_active_individual_weight = NULL, max_abs_active_group_weight = NULL,
                                                   #How to select signals
                                                   p_correction_method = "none", signal_significance_threshold = 0.05, data_availability_cutoff = 60,
                                                   priors_type = NULL, priors_informative_data = NULL) {

  standardGeneric("add_signal_selection_policy")
})

#' @export
setMethod("add_signal_selection_policy", "portfolio_policies", function(portfolio_policies_obj,
                                                                        #Signal Selection
                                                                        chosen_signals = NULL, signal_positions = NULL,
                                                                        #How to blend signals?
                                                                        signal_blending_method = NULL, chosen_sb_metric = NULL,
                                                                        #Restrictions on weights
                                                                        sb_benchmark_weighting_method = "theme_sb", max_abs_active_individual_weight = NULL, max_abs_active_group_weight = NULL,
                                                                        #How to select signals
                                                                        p_correction_method = "none", signal_significance_threshold = 0.05, data_availability_cutoff = 60,
                                                                        priors_type = NULL, priors_informative_data = NULL
                                                                       ) {

  # Validate arguments
    ## chosen_signals
    if(!is.null(chosen_signals)){
      if (!all(is.character(chosen_signals))){
        stop("chosen_signals should be a character vector.")
      }
      #Check if signal positions is also updated
      if(is.null(signal_positions)){
        stop("please always update chosen_signals with signal_positions to ensure consistency")
      }

      ## signal_position
      if (!all(is.character(signal_positions))){
        stop("signal_position should be a character vector.")
      }
      if(length(signal_positions) != length(chosen_signals)){
        stop("lengths of signal_positions and chosen_signals should match.")
      }
      if(length(signal_positions) != length(chosen_signals)){
        stop("lengths of signal_positions and chosen_signals should match.")
      }

      ## check for appropriate signal_blending_method
      if(length(chosen_signals) > 1){
        #If more than one signal is chosen, signal_blending_method can't be NULL
        if(any(!is.null(signal_blending_method), !is.null(portfolio_policies_obj@signal_selection_policy))){
          #Either the new signal_blending_method or the old one must be different from NULL
        }
      }
    }

    ## Signal Blending Method
    if (!is.null(signal_blending_method)){
      ###Character
      if(!is.character(signal_blending_method)) {
        stop("signal_blending_method should be a character")
      }
      ###Correct choice
      if(!signal_blending_method %in% c("EW", "SW", "RP", "MTO", "ML")){
        stop("signal_blending_method should be one of EW, SW, RP, MTO or ML")
      }
    }

    ##chosen_sb_metric
    if (!is.null(chosen_sb_metric)){
      ###Character
      if(!is.character(chosen_sb_metric)) {
        stop("chosen_sb_metric should be a character")
      }
      ###Correct choice
      if(!chosen_sb_metric %in% c("mean_active_return", "IR", "alpha", "AP", "treynor")){
        stop("chosen_sb_metric should be one of mean_active_return, IR, alpha, AP or treynor")
      }
    }

    ## Signal Blending Benchmark Weighting Method
    if (!is.null(sb_benchmark_weighting_method)){
      ###Character
      if(!is.character(sb_benchmark_weighting_method)) {
        stop("sb_benchmark_weighting_method should be a character")
      }
      ###Correct choice
      if(!sb_benchmark_weighting_method %in% c("individual_sb", "theme_sb")){
        stop("sb_benchmark_weighting_method should be one of individual_sb or theme_sb")
      }
    }

    ##max_abs_active_individual_weight
    if(!is.null(max_abs_active_individual_weight)){
      if(!is.numeric(max_abs_active_individual_weight)){
        stop("max_abs_active_individual_weight should be numeric")
      }
      if(is.null(signal_blending_method)){
        if(portfolio_policies_obj@signal_selection_policy$signal_blending_method != "MTO"){
          stop("signal_blending_method must be equal to MTO if max_abs_active_individual_weight is set")
        }
      } else {
        if(signal_blending_method != "MTO"){
          stop("signal_blending_method must be equal to MTO if max_abs_active_individual_weight is set")
        }
      }
    }

    ##max_abs_active_group_weight
    if(!is.null(max_abs_active_group_weight)){
      if(!is.numeric(max_abs_active_group_weight)){
        stop("max_abs_active_group_weight should be numeric")
      }
      if(is.null(signal_blending_method)){
        if(portfolio_policies_obj@signal_selection_policy$signal_blending_method != "MTO"){
          stop("signal_blending_method must be equal to MTO if max_abs_active_group_weight is set")
        }
      } else {
        if(signal_blending_method != "MTO"){
          stop("signal_blending_method must be equal to MTO if max_abs_active_group_weight is set")
        }
      }
    }




    ## p_correction_method
    if (!is.null(p_correction_method)){
      ###Character
      if(!is.character(p_correction_method)) {
        stop("p_correction_method should be a character")
      }
      ###Correct choice
      if(!p_correction_method %in% c("bayesian", "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")){
        stop("p_correction_method should be one of bayesian, holm, hochberg, hommel, bonferroni, BH, BY, fdr or none")
      }
    }

    ##signal_significance_threshold
    if(!is.numeric(signal_significance_threshold)){
      stop("signal_significance_threshold should be numeric")
    }

    ##data_availability_cutoff
    if(!is.numeric(data_availability_cutoff)){
      stop("data_availability_cutoff should be numeric")
    }

    ##priors_type
    if (!is.null(priors_type)){
      ###Character
      if(!is.character(priors_type)) {
        stop("priors_type should be a character")
      }
      ###Correct choice
      if(!priors_type %in% c("uninformative", "all", "mean", "user")){
        stop("priors_type should be one of uninformative, all, mean or user")
      }
    }

    ##priors_informative_data
    if (!is.null(priors_informative_data)){
      ###Character
      if(!is.character(priors_informative_data)) {
        stop("priors_informative_data should be a character")
      }
      ###Correct choice
      if(!priors_informative_data %in% c("jkp_emerging", "cz_global")){
        stop("priors_type should be one of jkp_emerging or cz_global")
      }
    }


  # Construct new_signal_selection_policy list
  new_signal_selection_policy <- list(
    chosen_signals = if (!is.null(chosen_signals)) chosen_signals else if (!is.null(portfolio_policies_obj@signal_selection_policy$chosen_signals)) portfolio_policies_obj@signal_selection_policy$chosen_signals else NULL,
    signal_positions = if (!is.null(signal_positions)) signal_positions else if (!is.null(portfolio_policies_obj@signal_selection_policy$signal_positions)) portfolio_policies_obj@signal_selection_policy$signal_positions else NULL,
    signal_blending_method = if (!is.null(signal_blending_method)) signal_blending_method else if (!is.null(portfolio_policies_obj@signal_selection_policy$signal_blending_method)) portfolio_policies_obj@signal_selection_policy$signal_blending_method else NULL,
    chosen_sb_metric = if (!is.null(chosen_sb_metric)) chosen_sb_metric else if (!is.null(portfolio_policies_obj@signal_selection_policy$chosen_sb_metric)) portfolio_policies_obj@signal_selection_policy$chosen_sb_metric else NULL,
    sb_benchmark_weighting_method = if (!is.null(sb_benchmark_weighting_method)) sb_benchmark_weighting_method else if (!is.null(portfolio_policies_obj@signal_selection_policy$sb_benchmark_weighting_method)) portfolio_policies_obj@signal_selection_policy$sb_benchmark_weighting_method else NULL,
    max_abs_active_individual_weight = if (!is.null(max_abs_active_individual_weight)) max_abs_active_individual_weight else if (!is.null(portfolio_policies_obj@signal_selection_policy$max_abs_active_individual_weight)) portfolio_policies_obj@signal_selection_policy$max_abs_active_individual_weight else NULL,
    max_abs_active_group_weight = if (!is.null(max_abs_active_group_weight)) max_abs_active_group_weight else if (!is.null(portfolio_policies_obj@signal_selection_policy$max_abs_active_group_weight)) portfolio_policies_obj@signal_selection_policy$max_abs_active_group_weight else NULL,
    p_correction_method = if (!is.null(p_correction_method)) p_correction_method else if (!is.null(portfolio_policies_obj@signal_selection_policy$p_correction_method)) portfolio_policies_obj@signal_selection_policy$p_correction_method else NULL,
    signal_significance_threshold = if (!is.null(signal_significance_threshold)) signal_significance_threshold else if (!is.null(portfolio_policies_obj@signal_selection_policy$signal_significance_threshold)) portfolio_policies_obj@signal_selection_policy$signal_significance_threshold else NULL,
    chosen_informative_data = if (!is.null(chosen_informative_data)) chosen_informative_data else if (!is.null(portfolio_policies_obj@signal_selection_policy$chosen_informative_data)) portfolio_policies_obj@signal_selection_policy$chosen_informative_data else NULL,
    priors_type = if (!is.null(priors_type)) priors_type else if (!is.null(portfolio_policies_obj@signal_selection_policy$priors_type)) portfolio_policies_obj@signal_selection_policy$priors_type else NULL,
    data_availability_cutoff = if (!is.null(data_availability_cutoff)) data_availability_cutoff else if (!is.null(portfolio_policies_obj@signal_selection_policy$data_availability_cutoff)) portfolio_policies_obj@signal_selection_policy$data_availability_cutoff else NULL
  )

  # Create the S4 object
  new_portfolio_policies_obj <- new("portfolio_policies",
                                    liquidity_constraint_policy = portfolio_policies_obj@liquidity_constraint_policy,
                                    signal_selection_policy = new_signal_selection_policy,
                                    turnover_constraint_policy = portfolio_policies_obj@turnover_constraint_policy,
                                    concentration_constraint_policy = portfolio_policies_obj@concentration_constraint_policy,
                                    liquidity_floor_cutoffs = portfolio_policies_obj@liquidity_floor_cutoffs)

  return(new_portfolio_policies_obj)
})

