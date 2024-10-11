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



