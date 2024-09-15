#' Define the `meta_dataframe` S4 Class
#'
#' This class represents a metadata-enhanced data frame. It extends the functionality
#' of a standard data frame by including additional metadata slots. The class is designed
#' to ensure that the input data frame adheres to specific structural requirements, including
#' unique identifiers, valid date formats, and unique column names.
#'
#' @slot data A \code{data.frame} containing the actual data.
#' @slot workflow A \code{list} for storing metadata about the data manipulation workflow.
#' @slot signals A \code{character} vector containing the names of columns that represent signals.
#' @slot unique_dates A \code{numeric} value representing the count of unique dates in the data.
#' @slot unique_tickers A \code{numeric} value representing the count of unique tickers in the data.
#' @slot n_obs A \code{numeric} value representing the total number of observations in the data.
#'
#' @details
#' The \code{meta_dataframe} class ensures that the data frame is structured correctly with the required columns,
#' and includes metadata about the data. The \code{signals} slot holds the names of columns representing various signals.
#' The \code{unique_dates}, \code{unique_tickers}, and \code{n_obs} slots store the metadata related to the number of unique dates,
#' tickers, and total observations respectively.
#'
#' @examples
#' # Define a sample data frame
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
#' # Print the meta_dataframe object
#' print(meta_df)
#'
#' @export
setClass("meta_dataframe",
         slots = c(
           data = "data.frame",        # Slot for the data frame
           workflow = "list",          # Slot for storing metadata about the data manipulation workflow
           signals = "character",      # Slot for storing column names
           unique_dates = "numeric",   # Slot for storing count of unique dates
           unique_tickers = "numeric", # Slot for storing count of unique tickers
           n_obs = "numeric"           # Slot for storing total number of observations
         ))


#' Define the `fwd_meta_dataframe` S4 Class
#'
#' This class extends the `meta_dataframe` class to include additional metadata for forward-looking data frames.
#' It is specifically designed to handle data frames where columns follow the format XXXXX_ym, where 'ym'
#' represents the forward month (01 to 12). This class inherits all properties of `meta_dataframe` and
#' adds a slot for specifying forward-looking targets.
#'
#' @slot data A \code{data.frame} containing the actual data.
#' @slot workflow A \code{list} for storing metadata about the data manipulation workflow.
#' @slot signals A \code{character} vector containing the names of columns that represent signals.
#' @slot unique_dates A \code{numeric} value representing the count of unique dates in the data.
#' @slot unique_tickers A \code{numeric} value representing the count of unique tickers in the data.
#' @slot n_obs A \code{numeric} value representing the total number of observations in the data.
#' @slot fwd_target A \code{character} vector containing the names of columns that represent forward-looking targets.
#'
#' @details
#' The \code{fwd_meta_dataframe} class inherits from the \code{meta_dataframe} class and adds an additional
#' slot \code{fwd_target} for forward-looking target variables. The columns in \code{fwd_target} must follow the
#' format XXXXX_ym, where 'ym' denotes the forward month (01 to 12).
#'
#' @examples
#' # Define a sample data frame
#' df <- data.frame(
#'   id = c("A-2024-01-01", "B-2024-02-01"),
#'   tickers = c("A", "B"),
#'   dates = as.Date(c("2024-01-01", "2024-02-01")),
#'   `value_01` = c(10, 20),
#'   `value_02` = c(15, 25)
#' )
#'
#' # Create a fwd_meta_dataframe object
#' fwd_meta_df <- create_fwd_meta_dataframe(df)
#'
#' # Print the fwd_meta_dataframe object
#' print(fwd_meta_df)
#'
#' @export
setClass("fwd_meta_dataframe",
         contains = "meta_dataframe",
         slots = c(fwd_target = "character")
)

