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
  if (!is.data.frame(obj)) {
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

  if (!all(diff(obj$dates) >= 0)) {
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
  if (any(is.na(obj[, remaining_columns]))) {
    message("Some columns contain NA values.")
    return(FALSE)
  }

  # All checks passed
  return(TRUE)
}

#' Check if an Object is Coercible to `fwd_meta_dataframe`
#'
#' This function checks if an object can be coerced into a `fwd_meta_dataframe` class.
#' It ensures that the object meets the structural requirements of `fwd_meta_dataframe`,
#' including correct column names and formats.
#'
#' @param obj The object to be checked.
#'
#' @return A logical value indicating whether the object is coercible to `fwd_meta_dataframe`.
#' Additionally, if the object is not coercible, a message will be displayed explaining why.
#'
#' @examples
#' df <- data.frame(
#'   id = c("A-2024-01-01", "B-2024-02-01"),
#'   tickers = c("A", "B"),
#'   dates = as.Date(c("2024-01-01", "2024-02-01")),
#'   value_01 = c(10, 20),
#'   value_02 = c(15, 25)
#' )
#' is_coercible_to_fwd_meta_dataframe(df)
#'
#' @export
is_coercible_to_fwd_meta_dataframe <- function(obj) {
  if (!is.data.frame(obj)) {
    message("Object must be a data frame.")
    return(FALSE)
  }


  # Check if it has the required columns
  required_columns <- c("id", "tickers", "dates")
  if (!all(required_columns %in% names(obj))) {
    message("Data frame must contain 'id', 'tickers', and 'dates' columns.")
    return(FALSE)
  }

  # Validate the 'dates' column
  if (!inherits(obj$dates, "Date")) {
    message("The 'dates' column must be of class 'Date'.")
    return(FALSE)
  }

  # Validate the 'id' column
  expected_id <- paste0(obj$tickers, "-", obj$dates)
  if (!all(obj$id == expected_id)) {
    message("The 'id' column does not match 'tickers-dates'.")
    return(FALSE)
  }

  # Check non-required columns for the forward format
  non_required_columns <- setdiff(names(obj), required_columns)
  valid_format <- "^fwd_.*_(1m|3m|6m)$"  # Adjusted regex to match fwd_variable_1m, fwd_variable_3m, etc.

  if (any(!grepl(valid_format, non_required_columns))) {
    message("Non-required columns must follow the format fwd_variable_ym where 'ym' is the forward month (1m, 3m, or 6m).")
    return(FALSE)
  }

  if(!is_coercible_to_meta_dataframe(obj)){
    return(FALSE)
  }

  # If all checks pass, the object is coercible to fwd_meta_dataframe
  return(TRUE)
}


