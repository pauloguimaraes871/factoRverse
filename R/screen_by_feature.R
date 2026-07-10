#' Screen (select) features from a meta_dataframe
#'
#' Selects columns (features) from a `meta_dataframe`, always retaining the `id`, `tickers` and `dates`
#' identifier columns regardless of the selection. This is a thin, workflow-aware wrapper around
#' `dplyr::select()`: the requested columns are chosen, the three identifier columns are guaranteed to be
#' present and ordered first, and a screening step is appended to the object's `workflow` log.
#'
#' @param meta_dataframe A `meta_dataframe` object.
#' @param ... One or more column names or tidyselect helpers (e.g. `dplyr::starts_with("mom")`) passed to
#'   `dplyr::select()` to choose feature columns.
#'
#' @return A new `meta_dataframe` with the selected features (plus `id`, `tickers`, `dates`). Errors if the
#'   selection would leave only the identifier columns.
#'
#' @examples
#' \dontrun{
#' # Keep only momentum features (id/tickers/dates are always retained)
#' screen_by_feature(features_m_df, dplyr::starts_with("mom"))
#' }
#'
#' @export
setGeneric("screen_by_feature", function(meta_dataframe, ...) {
  standardGeneric("screen_by_feature")
})

#' Method for selecting features from a meta_dataframe
#'
#' @inheritParams screen_by_feature
#' @return A new `meta_dataframe` object with selected features (always including id, tickers, dates).
#' @export
setMethod("screen_by_feature", "meta_dataframe", function(meta_dataframe, ...) {

  # Columns that must always be retained
  required_cols <- c("id", "tickers", "dates")

  # Select user-requested columns
  user_selected <- dplyr::select(meta_dataframe@data, ...)

  # Identify which required columns are missing and add them back
  missing_required <- setdiff(required_cols, names(user_selected))
  selected_features_m_df <- dplyr::bind_cols(
    meta_dataframe@data[required_cols[required_cols %in% missing_required]],
    user_selected
  ) %>%
    # Make sure the required columns are ordered first, without duplication
    dplyr::select(dplyr::any_of(required_cols), dplyr::everything())

  # Safety check
  if (ncol(selected_features_m_df) <= length(required_cols)) {
    stop("No features (beyond id, tickers, and dates) were selected. Please check expression.")
  }

  # Add new workflow step
  new_workflow <- list(
    list(current_date = meta_dataframe@current_date,
         timestamp = Sys.time(),
         selection = match.call(expand.dots = FALSE)$...,
         screening_call = match.call())
  )

  # Recreate meta_dataframe object
  selected_features_m_df <- create_meta_dataframe(selected_features_m_df,
                                                  meta_dataframe_name = meta_dataframe@meta_dataframe_name,
                                                  workflow = c(meta_dataframe@workflow, new_workflow))

  # Name the workflow step
  names(selected_features_m_df@workflow)[length(selected_features_m_df@workflow)] <-
    paste0("screen_features_", selected_features_m_df@current_date)

  return(selected_features_m_df)
})
