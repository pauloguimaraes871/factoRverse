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

    if (!all(required_columns %in% names(obj))) {
      message("The data frame must contain the following columns: 'id', 'tickers', 'dates'.")
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

    unique_dates <- unique(obj$dates)
    full_dates <- seq(min(unique_dates), max(unique_dates), by = "month")
    missing_dates <- setdiff(full_dates, unique_dates)
    if (length(missing_dates) > 0) {
      message("There are gaps in the dates sequence. Missing dates: ", paste(as.Date(missing_dates), collapse = ", "))
      return(FALSE)
    }

    if (any(duplicated(obj$id))) {
      message("The 'id' column contains duplicated values.")
      return(FALSE)
    }

    # Check for NA values in remaining columns
    remaining_columns <- setdiff(names(obj), required_columns)
    na_remaining <- sapply(obj[, remaining_columns], function(col) any(is.na(col)))
    if (any(na_remaining)) {
      message("The following columns contain NA values: ",
              paste(remaining_columns[na_remaining], collapse = ", "))
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
is_hyperparameter_tuning_strategy <- function(x) {
  is(x, "hyperparameter_tuning_strategy")
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
