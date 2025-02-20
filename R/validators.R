#' Check if an object is coercible to meta_dataframe
#'
#' This function checks if an object can be converted to a \code{meta_dataframe} class
#' by verifying the required columns, data types, and other constraints. It provides
#' detailed messages explaining why the object is not coercible.
#'
#' @param obj An R object to be checked for coercibility.
#' @return A logical value indicating whether the object can be coerced to \code{meta_dataframe}.
#' @export
is_coercible_to_meta_dataframe <- function(obj) {

  #If the object is already meta_dataframe, return TRUE
  if(is_meta_dataframe(obj)){
    return(TRUE)
  } else {
  #Otherwise...
    if (!is.data.frame(obj)){
      message("The object is not a data frame.")
      return(FALSE)
    }

    required_columns <- c("id", "tickers", "dates")

    if (!all(required_columns == names(obj)[1:3])) {
      message("The data frame must contain the following columns: 'id', 'tickers', 'dates', exactly in this order.")
      return(FALSE)
    }

    if(any(!is.character(obj$tickers))){
      stop("Tickers must be of class character")
    }

    if (any(is.na(obj[required_columns]))) {
      message("Columns 'id', 'tickers', or 'dates' contain NA values.")
      return(FALSE)
    }

    if (!inherits(obj$dates, "Date")) {
      message("The 'dates' column must be of class 'Date'.")
      return(FALSE)
    }

    if(any(obj$id != obj$id[order(obj$id)])){
      message("Object must be ordered alphabetically according to id.")
      return(FALSE)
    }

    expected_id <- paste0(obj$tickers, "-", obj$dates)
    if (!all(obj$id == expected_id)) {
      message("The 'id' column does not match the expected format 'tickers-dates'.")
      return(FALSE)
    }


    if (!all(diff(unique(obj$dates)[order(unique(obj$dates))]) >= 0)) {
      message("Dates must be in ascending chronological order")
      return(FALSE)
    }

    if (any(duplicated(obj$id))) {
      message("The 'id' column contains duplicated values.")
      return(FALSE)
    }



    # Check for NA values in remaining columns
    remaining_columns <- setdiff(names(obj), required_columns)
    na_remaining <- sapply(obj[, remaining_columns, drop = FALSE], function(col) any(is.na(col)))
    if (any(na_remaining)) {
      message("The following columns contain NA values: ",
              paste(remaining_columns[na_remaining], collapse = ", "))
    }

    if (any(duplicated(remaining_columns))) {
      stop("Column names for variables must be unique")
    }



    # All checks passed
    return(TRUE)

  }


}



# Define the is_meta_dataframe function
#' Check if an object is a meta_dataframe
#'
#' @param x The object to check.
#' @return TRUE if x is of class "meta_dataframe", FALSE otherwise.
#' @export
is_meta_dataframe <- function(x) {
  inherits(x, "meta_dataframe")
}


#' Check if an object is of class hyper_grid_domain
#'
#' This function checks whether a given object is of class 'hyper_grid_domain'.
#'
#' @param x The object to check.
#'
#' @return Logical value: TRUE if the object is of class 'hyper_grid_domain', FALSE otherwise.
#'
#' @export
is_hyper_grid_domain <- function(x) {
  inherits(x, "hyper_grid_domain")
}

#' Define the is_hyperparameter_tuning_strategy function
#' @description Function to check if an object is of class `hyperparameter_tuning_strategy`.
#'
#' @param x An object to check.
#'
#' @return A logical value indicating whether the object is of class `hyperparameter_tuning_strategy`.
#'
#' @export
is_tuning_strategy <- function(x) {
  is(x, "tuning_strategy")
}

#' Define the is keras_architecture_parameters function
#' @description Function to check if an object is of class `keras_architecture_parameters`.
#'
#' @param x An object to check.
#'
#' @return A logical value indicating whether the object is of class `keras_architecture_parameters`.
#'
#' @export
is_keras_architecture_parameters <- function(x) {
  is(x, "keras_architecture_parameters")
}


#' Define the is portfolio_policies function
#' @description Function to check if an object is of class `portfolio_policies`.
#'
#' @param x An object to check.
#'
#' @return A logical value indicating whether the object is of class `portfolio_policies`.
#'
#' @export
is_portfolio_policies <- function(x) {
  is(x, "portfolio_policies")
}

#' Get Expected Hyperparameters for a Machine Learning Algorithm or Configuration
#'
#' The `hyperparameters` function returns a character vector of expected hyperparameters for a given machine learning algorithm or configuration.
#'
#' @param object An `sb_algorithm` character string or an `sb_backtest_config` object.
#' @return A character vector containing the names of the expected hyperparameters for the specified algorithm.
#' @export
setGeneric("hyperparameters", function(object) standardGeneric("hyperparameters"))


#' @describeIn hyperparameters Get expected hyperparameters for a given machine learning algorithm.
#'
#' @param object A character string specifying the machine learning algorithm.
#' Valid options are "glmnet", "rf", "xgb", "nn", or "ols".
#'
#' @return A character vector of expected hyperparameters.
#' If the algorithm is "ols" or not recognized, it returns an empty character vector.
#'
#' @examples
#' # Get hyperparameters for glmnet
#' hyperparameters("glmnet")
#' # Get hyperparameters for random forest
#' hyperparameters("rf")
#'
#' @export
setMethod("hyperparameters", signature(object = "character"),
          function(object) {
            sb_algorithm <- object
            expected_hyperparameters <- switch(
              sb_algorithm,
              "glmnet" = c("alpha", "lambda.min.ratio"),
              "rf" = c("mtry", "num.trees", "max.depth", "min.bucket"),
              "xgb" = c("min_child_weight", "max_depth", "subsample", "colsample_bytree",
                        "eta", "alpha", "gamma", "nrounds"),
              "nn" = c("regularizer_l1", "regularizer_l2", "droprate", "lr",
                       "size_of_batch", "number_of_epochs"),
              character(0) # default for unrecognized algorithms or 'ols'
            )
            return(expected_hyperparameters)
          })

#' @describeIn hyperparameters Get expected hyperparameters from an `sb_backtest_config` object.
#'
#' @param object An `sb_backtest_config` object.
#'
#' @return A character vector of expected hyperparameters for the algorithm specified in the configuration.
#' If the algorithm is "ols" or not recognized, it returns an empty character vector.
#'
#' @examples
#' # Assuming you have an sb_backtest_config object named config
#' hyperparameters(config)
#'
#' @export
setMethod("hyperparameters", signature(object = "sb_backtest_config"),
          function(object) {
            sb_algorithm <- object@sb_algorithm
            expected_hyperparameters <- hyperparameters(sb_algorithm)
            return(expected_hyperparameters)
          })



#' Validate Concentration Constraint Policy
#'
#' Internal function to validate the structure and values of the concentration constraint policy.
#'
#' @param concentration_constraint_policy A list with the following possible elements:
#'   \describe{
#'     \item{benchmark}{Benchmark weights (not validated by this function).}
#'     \item{max_abs_active_individual_weight}{A numeric value in (0, 1] representing the maximum absolute active weight for an individual asset.}
#'     \item{max_abs_active_group_weight}{A named numeric vector in (0, 1] representing the maximum absolute active weight for groups of assets. Names must be unique.}
#'   }
#'
#' @return Invisibly returns TRUE if the policy is valid.
#' @keywords internal
validate_concentration_constraint_policy <- function(concentration_constraint_policy){

  ## Check if the input is a list
  if (!is.list(concentration_constraint_policy)) {
    stop("Error in concentration_constraint_policy: must be a list")
  }

  ##Check if names in concentration_constraint_policy match possible options
  if(any(!names(concentration_constraint_policy) %in%
         c("benchmark", "max_abs_active_individual_weight", "max_abs_active_group_weight"))){
    stop("Error in concentration_constraint_policy: elements of concentration_constraint_policy should be one of benchmark, max_abs_active_individual_weight or max_abs_active_group_weight.")
  }

  ##Check if benchmark is always set
  if(is.null(concentration_constraint_policy$benchmark)){
    stop("Error in concentration_constraint_policy: benchmark must be set")
  }

  ##Check if one of max_abs_active_individual_weight or max_abs_active_group_weight is set
  if(is.null(concentration_constraint_policy$max_abs_active_individual_weight) &
     is.null(concentration_constraint_policy$max_abs_active_group_weight)){
    stop("Error in concentration_constraint_policy: either max_abs_active_individual_weight or max_abs_active_group_weight must be set")
  }

  ##Check if max_abs_active_individual_weight is numeric
  if(!is.null(concentration_constraint_policy$max_abs_active_individual_weight) &
     !is.numeric(concentration_constraint_policy$max_abs_active_individual_weight)){
    stop("Error in concentration_constraint_policy: max_abs_active_individual_weight must be numeric")
  }

  ##Check if max_abs_active_group_weight is numeric
  if(!is.null(concentration_constraint_policy$max_abs_active_group_weight) &
     !is.numeric(concentration_constraint_policy$max_abs_active_group_weight)){
    stop("Error in concentration_constraint_policy: max_abs_active_group_weight must be numeric")
  }

  ##Constraints are bounded in (0,1]
  if (!is.null(concentration_constraint_policy$max_abs_active_individual_weight) &&
      (concentration_constraint_policy$max_abs_active_individual_weight <= 0 || concentration_constraint_policy$max_abs_active_individual_weight > 1)){
    stop("Error in concentration_constraint_policy: max_abs_active_individual_weight must be in (0,1]")
  }

  ##Constraints are bounded in (0,1]
  if (!is.null(concentration_constraint_policy$max_abs_active_group_weight) &&
      (any(concentration_constraint_policy$max_abs_active_group_weight <= 0) ||
       any(concentration_constraint_policy$max_abs_active_group_weight > 1))){
    stop("Error in concentration_constraint_policy: max_abs_active_group_weight must be in (0,1]")
  }

  ## Check if max_abs_active_group_weight contains duplicated names
  if (!is.null(concentration_constraint_policy$max_abs_active_group_weight)) {
    grp_names <- names(concentration_constraint_policy$max_abs_active_group_weight)
    if (!is.null(grp_names) && any(duplicated(grp_names))) {
      stop("Error in concentration_constraint_policy: max_abs_active_group_weight can't contain duplicated names")
    }
  }


}



#' Validate Liquidity Constraint Policy
#'
#' Internal function to validate the liquidity constraint policy.
#'
#' The policy is provided as a list that may contain two elements:
#' \describe{
#'   \item{liquidity_floor_rule}{(Optional) A character string indicating the liquidity floor.
#'      Must be one of "micro_caps", "small_caps", "mid_caps", "large_caps", or "mega_caps".}
#'   \item{liquidity_cap_rules}{(Optional) A named numeric vector containing liquidity caps.
#'      The names must be valid liquidity categories (see above), there must be no duplicated names,
#'      and the numeric values must lie in the interval (0, 1]. If liquidity_floor_rule is provided,
#'      no liquidity cap can be set for a category that is less liquid than the floor.
#'      Moreover, monotonicity is enforced: for any two categories, if one category is less liquid than
#'      another, its cap must not exceed that of the more liquid category.}
#' }
#'
#' @param liquidity_constraint_policy A list with the elements described above.
#'
#' @return Invisibly returns TRUE if the policy is valid.
#'
#' @keywords internal
validate_liquidity_constraint_policy <- function(liquidity_constraint_policy) {
  ## Check that input is a list
  if (!is.list(liquidity_constraint_policy)) {
    stop("Error in liquidity_constraint_policy: must be a list")
  }

  ##Either liquidity_floor_rule or liquidity_cap_rules must be set
  if (is.null(liquidity_constraint_policy$liquidity_floor_rule) &&
      is.null(liquidity_constraint_policy$liquidity_cap_rules)) {
    stop("Error in liquidity_constraint_policy: either liquidity_floor_rule or liquidity_cap_rules must be set")
  }

  allowed_elements <- c("liquidity_floor_rule", "liquidity_cap_rules")
  if (!is.null(names(liquidity_constraint_policy)) &&
      !all(names(liquidity_constraint_policy) %in% allowed_elements)) {
    stop("Error in liquidity_constraint_policy: elements of liquidity_constraint_policy should be one of liquidity_floor_rule or liquidity_cap_rules")
  }

  ## Define allowed liquidity categories (from less liquid to more liquid)
  allowed_categories <- c("micro_caps", "small_caps", "mid_caps", "large_caps", "mega_caps")

  ## Validate liquidity_floor_rule if provided
  if (!is.null(liquidity_constraint_policy$liquidity_floor_rule)) {
    if (!is.character(liquidity_constraint_policy$liquidity_floor_rule) ||
        length(liquidity_constraint_policy$liquidity_floor_rule) != 1) {
      stop("Error in liquidity_constraint_policy: liquidity_floor_rule must be a character string")
    }
    if (!liquidity_constraint_policy$liquidity_floor_rule %in% allowed_categories) {
      stop("Error in liquidity_constraint_policy: liquidity_floor_rule must be one of 'micro_caps', 'small_caps', 'mid_caps', 'large_caps' or 'mega_caps'")
    }
  }

  ## Validate liquidity_cap_rules if provided
  if (!is.null(liquidity_constraint_policy$liquidity_cap_rules)) {
    cap_rules <- liquidity_constraint_policy$liquidity_cap_rules

    ## Must be numeric
    if (!is.numeric(cap_rules)) {
      stop("Error in liquidity_constraint_policy: liquidity_cap_rules must be numeric")
    }
    ## Must have names
    if (is.null(names(cap_rules))) {
      stop("Error in liquidity_constraint_policy: liquidity_cap_rules must have names")
    }
    ## All names must be valid liquidity categories
    if (!all(names(cap_rules) %in% allowed_categories)) {
      stop("Error in liquidity_constraint_policy: liquidity_cap_rules names must be one of 'micro_caps', 'small_caps', 'mid_caps', 'large_caps' or 'mega_caps'")
    }
    ## Check for duplicated names
    if (any(duplicated(names(cap_rules)))) {
      stop("Error in liquidity_constraint_policy: liquidity_cap_rules can't have duplicated names")
    }
    ## Check that liquidity_cap_rules are bounded in (0,1]
    if (any(cap_rules <= 0 | cap_rules > 1)) {
      stop("Error in liquidity_constraint_policy: liquidity_cap_rules must be in (0,1]")
    }

    ## If liquidity_floor_rule is provided, ensure that no liquidity cap rule is defined
    ## for a category that is less liquid than the floor.
    if (!is.null(liquidity_constraint_policy$liquidity_floor_rule)) {
      floor_rule <- liquidity_constraint_policy$liquidity_floor_rule
      floor_index <- match(floor_rule, allowed_categories)
      for (cat in names(cap_rules)) {
        cat_index <- match(cat, allowed_categories)
        if (cat_index < floor_index) {
          stop(
            paste0("Error in liquidity_constraint_policy: Liquidity cap rule for '", cat,
                   "' is less liquid than the liquidity_floor_rule '", floor_rule, "'")
          )
        }
      }
    }

    ## Enforce monotonicity in liquidity_cap_rules.
    ## For any two categories in liquidity_cap_rules, if one category is less liquid than another then its
    ## cap must not exceed that of the more liquid category.
    for (i in seq_along(cap_rules)) {
      for (j in seq_along(cap_rules)) {
        if (match(names(cap_rules)[i], allowed_categories) <
            match(names(cap_rules)[j], allowed_categories)) {
          if (cap_rules[i] > cap_rules[j]) {
            stop(
              paste0("Error in liquidity_constraint_policy: Cap for '", names(cap_rules)[i],
                     "' of ", cap_rules[i],
                     " cannot be greater than cap for '", names(cap_rules)[j],
                     "' of ", cap_rules[j])
            )
          }
        }
      }
    }
  }

  invisible(TRUE)
}


#' Validate Turnover Constraint Policy
#'
#' Internal function to validate the turnover constraint policy.
#'
#' The policy is provided as a list that must contain the following elements:
#' \describe{
#'   \item{quantile_range_buffer}{A numeric value between 0 and 1 that defines the buffer for turnover quantiles.}
#'   \item{turnover_cap_rules}{A named numeric vector whose names correspond to valid liquidity categories
#'      ("micro_caps", "small_caps", "mid_caps", "large_caps", "mega_caps"). Each value must be between 0 and 1.
#'      Additionally, for any two categories, if one category is less liquid than another then its cap must not exceed
#'      that of the more liquid category.}
#' }
#'
#' @param turnover_constraint_policy A list with the elements described above.
#'
#' @return Invisibly returns TRUE if the policy is valid.
#'
#' @keywords internal
validate_turnover_constraint_policy <- function(turnover_constraint_policy) {
  # Check that the input is a list
  if (!is.list(turnover_constraint_policy)) {
    stop("Error in turnover_constraint_policy: must be a list")
  }

  allowed_elements <- c("quantile_range_buffer", "turnover_cap_rules")
  if (!is.null(names(turnover_constraint_policy)) &&
      !all(names(turnover_constraint_policy) %in% allowed_elements)) {
    stop("Error in turnover_constraint_policy: elements of turnover_constraint_policy should be one of quantile_range_buffer or turnover_cap_rules")
  }

  # Validate quantile_range_buffer
  if (is.null(turnover_constraint_policy$quantile_range_buffer)) {
    stop("Error in turnover_constraint_policy: quantile_range_buffer can't be missing")
  }
  if (!is.numeric(turnover_constraint_policy$quantile_range_buffer) ||
      length(turnover_constraint_policy$quantile_range_buffer) != 1) {
    stop("Error in turnover_constraint_policy: quantile_range_buffer must be a number")
  }
  if (turnover_constraint_policy$quantile_range_buffer < 0 ||
      turnover_constraint_policy$quantile_range_buffer > 1) {
    stop("Error in turnover_constraint_policy: quantile_range_buffer must be a number between 0 and 1")
  }

  # Validate turnover_cap_rules
  if (is.null(turnover_constraint_policy$turnover_cap_rules)) {
    stop("Error in turnover_constraint_policy: turnover_cap_rules can't be missing")
  }
  cap_rules <- turnover_constraint_policy$turnover_cap_rules
  if (!is.numeric(cap_rules)) {
    stop("Error in turnover_constraint_policy: turnover_cap_rules must be numeric")
  }
  if (is.null(names(cap_rules))) {
    stop("Error in turnover_constraint_policy: turnover_cap_rules must have names")
  }

  allowed_categories <- c("micro_caps", "small_caps", "mid_caps", "large_caps", "mega_caps")
  if (!all(names(cap_rules) %in% allowed_categories)) {
    stop("Error in turnover_constraint_policy: names of turnover_cap_rules must be in accordance to micro_caps, small_caps, mid_caps, large_caps, mega_caps")
  }
  if (any(duplicated(names(cap_rules)))) {
    stop("Error in turnover_constraint_policy: names of turnover_cap_rules must not be duplicated")
  }
  if (!all(cap_rules >= 0 & cap_rules <= 1)) {
    stop("Error in turnover_constraint_policy: turnover_cap_rules must be bounded between 0 and 1")
  }

  # Enforce monotonicity in turnover_cap_rules.
  # For any two categories, if one category is less liquid than another then its cap must not exceed that of the more liquid category.
  for (i in seq_along(cap_rules)) {
    for (j in seq_along(cap_rules)) {
      if (match(names(cap_rules)[i], allowed_categories) < match(names(cap_rules)[j], allowed_categories)) {
        if (cap_rules[i] > cap_rules[j]) {
          stop(paste0("Error in turnover_constraint_policy: Cap for '", names(cap_rules)[i],
                      "' of", cap_rules[i],
                      " cannot be greater than cap for '", names(cap_rules)[j],
                      "' of ", cap_rules[j]))
        }
      }
    }
  }

  invisible(TRUE)
}


#' Validate Transaction Cost Parameters
#'
#' This function validates a list of transaction cost parameters based on the BARRA model.
#' It checks that the list contains the names: "direct_transaction_cost", "strategy_aum",
#' "alpha", and "lambda". Additionally, it ensures that:
#' \itemize{
#'   \item \code{direct_transaction_cost} is a single numeric value and positive.
#'   \item \code{strategy_aum} is a single numeric value and positive.
#'   \item \code{alpha} is a single numeric value and positive.
#'   \item \code{lambda} is a single value that is either numeric or exactly the string "dynamic".
#' }
#'
#' @param transaction_costs_parameters A list containing transaction cost parameters.
#'
#' @return TRUE if all validations pass; otherwise, the function stops with an error.
#'
#' @examples
#' params <- list(
#'   direct_transaction_cost = 0.01,
#'   strategy_aum = 1000000,
#'   alpha = 0.05,
#'   lambda = 0.5
#' )
#' validate_transaction_cost_parameters(params)
#'
#' @export
validate_transaction_costs_parameters <- function(transaction_costs_parameters) {
  # Check if all required names are present
  required_names <- c("direct_transaction_cost", "strategy_aum", "alpha", "lambda")
  if (!all(required_names %in% names(transaction_costs_parameters))) {
    stop("transaction_costs_parameters should have names 'direct_transaction_cost', 'strategy_aum', 'alpha' and 'lambda'")
  }

  # Check if direct_transaction_cost is numeric of length 1
  if (!is.numeric(transaction_costs_parameters$direct_transaction_cost) ||
      length(transaction_costs_parameters$direct_transaction_cost) != 1 ||
      (transaction_costs_parameters$direct_transaction_cost <= 0.0001 || transaction_costs_parameters$direct_transaction_cost >= 0.1)){
    stop("direct_transaction_cost should be a single numeric between 0.0001 and 0.1")
  }

  # Check if strategy_aum is numeric of length 1
  if (!is.numeric(transaction_costs_parameters$strategy_aum) ||
      length(transaction_costs_parameters$strategy_aum) != 1 ||
      transaction_costs_parameters$strategy_aum <= 0){
    stop("strategy_aum should be a single positive numeric")
  }

  # Check if alpha is numeric of length 1
  if (!is.numeric(transaction_costs_parameters$alpha) ||
      length(transaction_costs_parameters$alpha) != 1 ||
      (transaction_costs_parameters$alpha <= 0 || transaction_costs_parameters$alpha > 1)
  ) {
    stop("alpha should be a single numeric between 0 and 1")
  }

  # Check if lambda is of length 1
  if (length(transaction_costs_parameters$lambda) != 1 ||
      (!is.numeric(transaction_costs_parameters$lambda) && transaction_costs_parameters$lambda != "dynamic")){
    stop("lambda should be a single numeric value or 'dynamic'")
  }

  if(is.numeric(transaction_costs_parameters$lambda) && (transaction_costs_parameters$lambda <= 0 || transaction_costs_parameters$lambda > 1)){
    stop("lambda should be a single numeric value between 0 and 1")
  }



  TRUE
}

#' Validate Liquidity Floor Cutoffs
#'
#' Internal function to validate a liquidity_floor_cutoffs data frame.
#'
#' This function checks that the liquidity_floor_cutoffs data frame meets the following requirements:
#' \itemize{
#'   \item It is a data.frame with at least two columns.
#'   \item The first column is named \code{"liquidity_classification"}.
#'   \item It has at most 5 rows.
#'   \item The values in the \code{liquidity_classification} column are among
#'         \code{"micro_caps"}, \code{"small_caps"}, \code{"mid_caps"}, \code{"large_caps"}, or \code{"mega_caps"}
#'         and contain no duplicates.
#'   \item All other columns are numeric and contain no missing values.
#'   \item If \code{main_liquidity_metric} is provided, then it must be one of the non-classification
#'         column names, the data frame must be arranged in ascending order by that metric, and the
#'         ranking (order) of the values across all liquidity metric columns must be identical.
#'   \item The liquidity metric values must not be normalized; that is, it is an error if all values in each
#'         liquidity metric column (all but the first) are between -1 and 1.
#' }
#'
#' @param liquidity_floor_cutoffs A data.frame containing liquidity floor cutoffs.
#' @param main_liquidity_metric Optional character string specifying the column (other than
#'   \code{"liquidity_classification"}) that should be used to check ascending order.
#'
#' @return Invisibly returns \code{TRUE} if the data frame passes all validations.
#'
#' @keywords internal
validate_liquidity_floor_cutoffs <- function(liquidity_floor_cutoffs, main_liquidity_metric = NULL) {

  # Must be a data.frame
  if (!is.data.frame(liquidity_floor_cutoffs)) {
    stop("liquidity_floor_cutoffs must be a data.frame")
  }

  # Must have at least 2 columns
  if (ncol(liquidity_floor_cutoffs) < 2) {
    stop("liquidity_floor_cutoffs must have at least 2 columns")
  }

  # First column must be "liquidity_classification"
  if (colnames(liquidity_floor_cutoffs)[1] != "liquidity_classification") {
    stop("liquidity_floor_cutoffs must have liquidity_classification as the first column")
  }

  # Must have at most 5 rows
  if (nrow(liquidity_floor_cutoffs) > 5) {
    stop("liquidity_floor_cutoffs must have at most 5 rows")
  }

  # liquidity_classification values must have no duplicates
  if (any(duplicated(liquidity_floor_cutoffs$liquidity_classification))) {
    stop("liquidity_classification must not have duplicates")
  }

  # Check that liquidity_classification values are supported
  allowed_categories <- c("micro_caps", "small_caps", "mid_caps", "large_caps", "mega_caps")
  if (any(!liquidity_floor_cutoffs$liquidity_classification %in% allowed_categories)) {
    stop("liquidity_classification must be one of micro_caps, small_caps, mid_caps, large_caps or mega_caps")
  }

  # All columns except the first must be numeric
  if (!all(sapply(liquidity_floor_cutoffs[, -1, drop = FALSE], is.numeric))) {
    stop("liquidity_floor_cutoffs elements (except liquidity_classification) must be numeric")
  }

  # There must be no missing values
  if (any(is.na(liquidity_floor_cutoffs))) {
    stop("liquidity_floor_cutoffs must not contain NAs")
  }

  # If a main liquidity metric is provided, check that:
  if (!is.null(main_liquidity_metric)) {
    # (1) It is present among the non-classification columns
    if (!main_liquidity_metric %in% colnames(liquidity_floor_cutoffs)[-1]) {
      stop("main_liquidity_metric must be present in liquidity_floor_cutoffs")
    }
    # (2) The data.frame is arranged in ascending order according to that metric
    ordered_df <- dplyr::arrange(liquidity_floor_cutoffs, !!rlang::sym(main_liquidity_metric))
    if (!identical(ordered_df, liquidity_floor_cutoffs)) {
      stop("liquidity_floor_cutoffs is not in ascending order according to main_liquidity_metric")
    }
    # (3) Check that the ranking order across all liquidity metric columns is identical.
    #       Compute, for each liquidity metric column, the order (i.e. ranking of row indices)
    order_matrix <- sapply(liquidity_floor_cutoffs[, -1, drop = FALSE], order)
    # For each row in the order matrix, the ordered indices should be the same across columns.
    if (!all(apply(order_matrix, 1, function(x) length(unique(x)) == 1))) {
      stop("liquidity metrics orders in liquidity_floor_cutoffs are conflicting")
    }
  }

  # Check if the liquidity metric values appear normalized.
  #     If every non-classification column has all values between -1 and 1, assume they are normalized.
  normalized <- sapply(liquidity_floor_cutoffs[, -1, drop = FALSE], function(x) all(x >= -1 & x <= 1))
  if (any(normalized)) {
    stop("liquidity_floor_cutoffs values must not be normalized")
  }

  invisible(TRUE)
}

#' Validate update_backtest_inputs
#'
#' @description
#' Checks that each object in a named list of `_m_df` or `_m_xts` objects:
#' \itemize{
#'   \item Matches the expected name in the \code{results@port_backtest_workflow}.
#'   \item Has new dates beyond those in \code{dates_covered}.
#'   \item Starts exactly one month after the last date in \code{dates_covered}.
#'   \item Contains exactly \code{n_update} new dates, as specified in
#'         \code{results@port_backtest_config@n_update}.
#' }
#'
#' @param new_objects_list A named list of `_m_df` or `_m_xts` objects, each object having
#'                         a \code{meta_dataframe_name} or \code{meta_xts_name} slot,
#'                         plus a \code{@data} slot with actual data.
#' @param results          A backtest results object that must contain:
#'   \itemize{
#'     \item \code{@port_backtest_workflow}, where each object name is stored in a field
#'           named \code{<prefix>_obj_name}.
#'     \item \code{@port_backtest_config}, which must have \code{n_update}.
#'   }
#' @param dates_covered    A vector of dates that have already been covered by the backtest.
#'
#' @return
#' \code{TRUE} (invisibly) if all checks pass, otherwise raises an error via \code{stop()}.
#'
#' @examples
#' \dontrun{
#'   # Example usage (pseudo-code):
#'   new_list <- list(
#'     signals_m_df = my_signals_m_df,
#'     daily_stock_returns_m_xts = my_daily_stock_returns_m_xts
#'   )
#'   check_backtest_objects(
#'     new_objects_list = new_list,
#'     results = my_results,
#'     dates_covered = my_dates_covered
#'   )
#' }
check_update_backtest_objects <- function(new_objects_list, old_objects_names, dates_covered) {

  ##Initial Prep
  ###############
    ###Get last_dates_covered
    last_date_covered <- dates_covered[length(dates_covered)]

    ###Helper function: remove "_m_df" or "_m_xts" from the name
    remove_suffix <- function(x) {
      sub("(_m_df|_m_xts)$", "", x)
    }

  ###############

  ##Loop through each object, detect suffix, check object name and dates
  ###############
  for (arg_name in names(new_objects_list)) {
    obj <- new_objects_list[[arg_name]]
    old_obj <- old_results[[arg_name]]

    ###If NULL, skip
    if (is.null(obj)) next

    ###Identify whether argument ends in _m_df or _m_xts
    if (grepl("_m_df$", arg_name)) {
      ####Extract meta name
      meta_name <- obj@meta_dataframe_name #Suffix is _m_df => check meta_dataframe_name
      expected_name <- old_obj@meta_dataframe_name
      ####Extract dates for m_df
      new_obj_dates <- obj@data %>% dplyr::pull(dates) %>% unique() %>% sort() #Suffix is _m_df => acess underlying data.frame
    } else if (grepl("_m_xts$", arg_name)) {
      ####Extract meta name
      meta_name <- obj@meta_xts_name #Suffix is _m_xts => check meta_xts_name
      ####Extract dates for m_xts
      new_obj_dates <- zoo::index(obj@data)
    } else {
      # If object name doesn't end in _m_df or _m_xts, skip
      next
    }

    ###Check meta_name vs workflow
    ####Derive the prefix by removing the suffix
    prefix <- remove_suffix(arg_name)  # e.g. "signals_m_df" => "signals"
    workflow_field <- paste0(prefix, "_obj_name") #The workflow field is "<prefix>_obj_name". E.g. "signals_obj_name"

    ####Compare
    if (meta_name != expected_name) {
      stop(sprintf(
        "Updated %s does not match object name in port_backtest_workflow.\nExpected: %s, got: %s",
        arg_name, expected_name, meta_name
      ))
    }

    ###Check dates
    ####Check if new object is somehow shorter than the last date covered
    if (length(new_obj_dates) <= length(dates_covered)) {
      stop(sprintf("No new dates to cover for %s!", arg_name))
    }

    ####Check if new date is within dates covered
    first_new_date <- new_obj_dates[length(dates_covered) + 1]
    if (first_new_date %in% dates_covered) {
      stop(sprintf(
        "The first new date of %s is already covered by the existing backtest results.",
        arg_name
      ))
    }

    ####Check if new date is within expected range
    expected_next_date <- lubridate::add_with_rollback(last_date_covered, months(1))
    if (first_new_date != expected_next_date) {
      stop(sprintf(
        paste0("The first new date of %s does not match the expected date.\n",
               "Expected: %s\nGot: %s"),
        arg_name, expected_next_date, first_new_date
      ))
    }

    ####Check if total new # of dates is consistent with n_update
    if ((length(new_obj_dates) - length(dates_covered)) != config@n_update) {
      stop(sprintf(
        paste0("The new dates in %s do not conform to the expected n_update.\n",
               "Expected total length: %s\nGot total length: %s"),
        arg_name,
        length(dates_covered) + config@n_update,
        length(new_obj_dates)
      ))
    }
  }
  ###############

  return(invisible(TRUE))

}



