#' Retrieve and Validate Chosen Feature Positions
#'
#' This function accepts a list of named character vectors representing chosen signals
#' and their corresponding positions (e.g., \code{"long"}, \code{"short"}, or \code{"force"}).
#' It ensures consistency across the list, reconstructs missing positions as \code{"long"}
#' if an element is \code{NULL}, verifies subsets against \code{features_passthrough} and
#' \code{features_m_df}, and finally returns a single named character vector of positions.
#'
#' @param chosen_signals_and_positions_list A list of named character vectors, where the names
#'   are feature names (e.g., \code{"FeatureA", "FeatureB"}) and values are positions
#'   (e.g., \code{"long", "short", "force"}). If any element in the list is \code{NULL}, the
#'   function will reconstruct it using all features (from \code{features_m_df}) set to
#'   \code{"long"}.
#'
#' @param features_passthrough A character vector indicating which features (by name) should
#'   be passed through to the meta-learner. Must be a subset of both \code{chosen_signals_and_positions_list[[1]]}
#'   and the columns of \code{features_m_df} (excluding the first 3 columns). If
#'   \code{"all"}, all available features are included. If \code{"none"}, an empty
#'   vector (or custom behavior) is returned. Defaults to \code{"none"}.
#'
#' @param features_m_df A \code{meta_dataframe} (or similar) object containing at least three
#'   columns (often \code{date, symbol, target}), with all remaining columns considered
#'   valid features. The function checks that \code{features_passthrough} is a valid subset
#'   of these remaining columns.
#'
#' @details
#' \enumerate{
#'   \item **Consistency Check**: The function verifies that all non-\code{NULL} elements
#'         of \code{chosen_signals_and_positions_list} are identical in terms of their
#'         \strong{named positions}. If they differ, an error is raised.
#'
#'   \item **Reconstruction**: If an element is \code{NULL}, the function reconstructs
#'         its chosen signals and positions by assigning \code{"long"} to each feature
#'         in \code{features_m_df} (except the first 3 columns).
#'
#'   \item **Subset Checks**: If \code{features_passthrough} is neither \code{"all"} nor
#'         \code{"none"}, it must be a subset of the positions' names (i.e., the features)
#'         and a subset of the columns in \code{features_m_df}.
#'
#'   \item **Force to Long**: Any position labeled \code{"force"} is mapped to \code{"long"}
#'         in the final output.
#' }
#'
#' @return A named character vector of positions. The names correspond to features, and
#'   the values are \code{"long"} or \code{"short"}, with any \code{"force"} replaced by
#'   \code{"long"}.
#'
#' @examples
#' \dontrun{
#'   # Suppose we have a list of chosen signals/positions
#'   csp_list <- list(
#'     c(FeatureA = "long", FeatureB = "short"),
#'     NULL  # This will be reconstructed as all "long"
#'   )
#'
#'   # And a meta_dataframe with columns: date, symbol, target, FeatureA, FeatureB
#'   features_mdf <- my_meta_dataframe  # Should have 'data' slot with columns
#'
#'   # If we want all features:
#'   get_features_positions(
#'     chosen_signals_and_positions_list = csp_list,
#'     features_passthrough = "all",
#'     features_m_df = features_mdf
#'   )
#'
#'   # If we want only specific features:
#'   get_features_positions(
#'     chosen_signals_and_positions_list = csp_list,
#'     features_passthrough = c("FeatureB"),
#'     features_m_df = features_mdf
#'   )
#' }
#'
get_features_positions <- function(chosen_signals_and_positions_list, features_passthrough, features_m_df) {

  #Checks
  ############################
    ##Verify all vectors in chosen_signals_and_positions_list are identical
    if (length(chosen_signals_and_positions_list) > 1) {
      first_vec <- chosen_signals_and_positions_list[[1]]
      for (i in seq_along(chosen_signals_and_positions_list)) {
        if (!identical(chosen_signals_and_positions_list[[i]], first_vec)) {
          stop("chosen_signals_and_positions differ at object with position number: ", i)
        }
      }
    }

    if (!(length(features_passthrough) == 1 && features_passthrough %in% c("none", "all"))) {
      ###Check conformity between features_passthrough and chosen_signals_and_positions
      if (any(!features_passthrough %in% names(chosen_signals_and_positions_list[[1]]))) {
        stop("features_passthrough must be a subset of chosen_signals_and_positions.")
      }
    }

  ############################

  ##Get positions
  ############################
  ###Get positions according to features_passthrough
  if (length(features_passthrough) == 1 && features_passthrough == "all") {
    features_passthrough_and_positions <- chosen_signals_and_positions_list[[1]]
  } else if (length(features_passthrough) == 1 && features_passthrough == "none") {
    features_passthrough_and_positions <- "none"
  } else {
    features_passthrough_and_positions <- chosen_signals_and_positions_list[[1]][features_passthrough]
  }

  ##treat "force" as "long"
  features_passthrough_and_positions[features_passthrough_and_positions == "force"] <- "long"


  return(features_passthrough_and_positions)
  ############################

}
