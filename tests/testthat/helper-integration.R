#' @noRd
skip_if_not_integration <- function() {
  testthat::skip_if_not(
    Sys.getenv("RUN_INTEGRATION_TESTS") == "true",
    "Skipping integration test: RUN_INTEGRATION_TESTS != true"
  )
}
