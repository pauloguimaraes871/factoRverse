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

  if (!inherits(data$dates, "Date")) {
    stop("The 'dates' column must be of class 'Date'")
  }

  if (!all(diff(data$dates) >= 0)) {
    stop("Dates must be in ascending chronological order")
  }

  # Check for NA values in remaining columns
  if (any(is.na(data[, setdiff(names(data), required_columns)]))) {
    warning("Some columns contain NA values")
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
    warning("There are gaps in the dates sequence. Missing dates: ", paste(missing_dates, collapse = ", "))
  }

  if (any(duplicated(data$id))) {
    stop("ID column contains duplicated values")
  }

  remaining_columns <- setdiff(names(data), required_columns)
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


#' Create a Forward MetaDataFrame Object
#'
#' This function creates an object of class \code{fwd_meta_dataframe} from a provided data frame.
#' The data frame must meet the same requirements as \code{meta_dataframe}, with additional
#' constraints on the non-required columns. These columns must follow the format \code{XXXXX_ym},
#' where \code{XXXXX} represents any string of length 5, and \code{ym} indicates the number of
#' forward months (01 to 12). The \code{fwd_target} slot will be populated with the names of these
#' forward-looking columns.
#'
#' @param data A \code{data.frame} containing the data to be converted to a \code{fwd_meta_dataframe}.
#'
#' @return An object of class \code{fwd_meta_dataframe} if the input data frame meets all validation criteria.
#' The returned object includes metadata such as the number of unique dates, unique tickers, total number
#' of observations, and the forward-looking target variables.
#'
#' @details
#' - The 'id' column should be in the format \code{paste0(tickers, "-", dates)}.
#' - The 'dates' column must be of class \code{Date} and in ascending chronological order.
#' - Non-required columns must follow the format \code{XXXXX_ym}, where \code{ym} represents the number
#'   of forward months (01 to 12).
#'
#' @examples
#' # Create a sample data frame
#' df <- data.frame(
#'   id = c("A-2024-01-01", "B-2024-02-01"),
#'   tickers = c("A", "B"),
#'   dates = as.Date(c("2024-01-01", "2024-02-01")),
#'   value_01 = c(10, 20),
#'   value_02 = c(15, 25)
#' )
#'
#' # Create a fwd_meta_dataframe object
#' fwd_meta_df <- create_fwd_meta_dataframe(df)
#'
#' @export
create_fwd_meta_dataframe <- function(data) {
  m_df <- create_meta_dataframe(data)  # Call the meta_dataframe constructor

  # Validate the format of non-required columns
  required_columns <- c("id", "tickers", "dates")
  non_required_columns <- setdiff(names(data), required_columns)

  # Regular expression to match the format XXXXX_ym
  valid_format <- "^fwd_.*_(1m|3m|6m)$"

  if (any(!grepl(valid_format, non_required_columns))) {
    stop("Non-required columns must follow the format fwd_XXXXX_ym where 'ym' is the forward month (01 to 12)")
  }

  # Create fwd_target slot
  fwd_target <- non_required_columns

  # Create the fwd_meta_dataframe object
  new("fwd_meta_dataframe", m_df, fwd_target = fwd_target)
}


