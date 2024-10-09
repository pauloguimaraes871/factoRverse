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

    if (any(is.na(obj[required_columns]))) {
      message("Columns 'id', 'tickers', or 'dates' contain NA values.")
      return(FALSE)
    }

    if (!inherits(obj$dates, "Date")) {
      message("The 'dates' column must be of class 'Date'.")
      return(FALSE)
    }

    if (!all(diff(unique(obj$dates)) >= 0)) {
      message("Dates must be in ascending chronological order.")
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
      message("There are gaps in the dates sequence. Missing dates: ", paste(missing_dates, collapse = ", "))
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
      warning("The following columns contain NA values: ",
              paste(remaining_columns[na_remaining], collapse = ", "))
    }
    # All checks passed
    return(TRUE)

  }


}



