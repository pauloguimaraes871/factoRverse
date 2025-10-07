#' Create a Hierarchical Risk Parity (HRP) portfolio
#'
#' Constructs a portfolio using the Hierarchical Risk Parity algorithm
#' (López de Prado, 2016). Assets are recursively grouped by hierarchical
#' clustering, and risk is allocated inversely to cluster variances.
#' Optionally, final weights can be tilted by expected return scores.
#'
#' @param universe_m_d_ref A data.frame with at least columns \code{tickers},
#'   \code{is_eligible}, and \code{exp_ret_score}. Only rows with
#'   \code{is_eligible == 1} are considered.
#' @param covariance_matrix A covariance matrix of eligible assets, with row
#'   and column names matching \code{tickers}.
#' @param linkage Linkage method for hierarchical clustering. Passed to
#'   \code{\link[stats]{hclust}}. Default is \code{"single"}.
#' @param exp_ret_score_tilt character or NULL. If "inner", tilt is applied at each split;
#'   if "final", a post overlay is applied; if NULL, no tilt.
#' @param exp_ret_score_tilt_eta numeric or NULL. Tilt intensity used for BOTH "inner" and "final".
#' @param verbose Logical, whether to print progress messages. Default is TRUE.
#'
#' @return A list with:
#' \item{universe_m_d_ref}{Input data.frame merged with portfolio weights.}
#' \item{weights}{Final portfolio weights (numeric vector).}
#' \item{dist_matrix}{Distance matrix used in clustering.}
#' \item{clusters}{\code{hclust} object with the dendrogram.}
#'
#' @references López de Prado, M. (2016). Building Diversified Portfolios that
#' Perform Well Out-of-Sample. The Journal of Portfolio Management, 42(4), 59–69.
#'
#' @export
create_hrp_portfolio <- function(universe_m_d_ref, covariance_matrix, linkage = "single",
                                 exp_ret_score_tilt_eta = NULL, exp_ret_score_tilt = NULL, #Alpha Tilt
                                 verbose = TRUE){

  # Initial Setup---------------------------------------------------------------
    ## Message
    if (isTRUE(verbose)) {
      tictoc::tic()
      cat("\n")
      cat("Deriving weights through HRP...")
      cat("\n")
      if (!is.null(exp_ret_score_tilt)) {
        cat(paste0("Exp Ret Score Tilt: ", exp_ret_score_tilt))
        cat("\n")
        cat(paste0("Exp Ret Score Tilt Eta: ", exp_ret_score_tilt_eta))
        cat("\n")
      }
    }
    ## Eligible tickers
    eligible_universe_m_d_ref <- universe_m_d_ref %>% dplyr::filter(is_eligible == 1)
    eligible_tickers <- eligible_universe_m_d_ref %>% dplyr::pull(tickers)

      ### If there is only one eligible tickers, return universe_m_d_ref with weight 1
      ### for the eligible tickers and 0 for other stocks
      if (length(eligible_tickers) == 1) {
        universe_m_d_ref <- universe_m_d_ref %>%
          dplyr::mutate(weights = ifelse(tickers == eligible_tickers, 1, 0))
        if (isTRUE(verbose)) {
          cat("\n")
          cat(crayon::green(paste("Only one eligible ticker. Weight set to 1.")))
          cat("\n")
          tictoc::toc()
        }
        hrp_results_list <- list(
          universe_m_d_ref = universe_m_d_ref,
          weights = universe_m_d_ref$weights,
          dist_matrix = NULL,
          clusters = NULL
        )
        return(hrp_results_list)
      }

      ### Defensively check if covariance is ordered according to eligible tickers
      if (!all(rownames(covariance_matrix) == colnames(covariance_matrix) &
               rownames(covariance_matrix) == eligible_tickers)) {
        stop("Covariance matrix rownames/colnames do not match eligible tickers.")
      }

    ## IVP Helper function
    ## "IVP" (Inverse-Variance Portfolio) inside a group
    ivp_in_cluster <- function(cov_slice){

      ### Inverse-variance portfolio
      d <- diag(cov_slice)

      ### Treat ill-conditioned
      if (any(d <= 0 | !is.finite(d))) d <- pmax(d, 1e-10)

      w <- 1 / d
      w / sum(w) # normalize to 1 inside the cluster

    }

    # Clustering----------------------------------------------------------------

      ## Calculate correlation matrix between assets
      correlation_matrix <- stats::cov2cor(covariance_matrix)

      ## Distance matrix D^G_ij = sqrt(0.5 * (1 - rho_ij))
      dist_matrix <- sqrt(0.5 * (1 - correlation_matrix))

        ### Correct possible problems ( < 0 -> 0; > 1 -> 1)
        dist_matrix[dist_matrix < 0] <- 0
        dist_matrix[dist_matrix > 1] <- 1

      ## Distance-of-distances (Euclidean distance between rows of dist_matrix
      dist_of_dist_matrix <- stats::dist(dist_matrix, method = "euclidean",
                                         diag = TRUE, upper = TRUE)

      ## Quasi-Diagonalization via hierarchical clustering
      hc <- stats::hclust(dist_of_dist_matrix, method = linkage)
        ### dendrogram leaf order (mirrors allowed;
        ### HRP is flip-invariant)
        eligible_tickers_order <- eligible_tickers[hc$order]

      ## Score in dendrogram order
      if (!is.null(exp_ret_score_tilt) &&
          exp_ret_score_tilt != "none"){

        ### Get exp ret scores and name them
        exp_ret_scores <- setNames(
          eligible_universe_m_d_ref$exp_ret_score,
          eligible_universe_m_d_ref$tickers
        )

        exp_ret_scores_ord <- exp_ret_scores[eligible_tickers_order]

      }

    # Recursive bisection-------------------------------------------------------

      ## Top-down split: For ordered assets, split into halvers and allocate
      weights_sorted <- rep(1, length(eligible_tickers_order)) # Start with w = 1
      names(weights_sorted) <- eligible_tickers_order # Name by cluster
      idx_list <- list(seq_along(hc$order)) # Start with all assets in one range

      ## Keep splitting untill all ranges are singletons
      while (length(idx_list) > 0){

        ### Collect next generation of sub-ranges
        new_idx_list <- list()

        ### Process each current range of contiguous indices
        for (idx in idx_list){

          #### Base case: singletons (stop splitting)
          if (length(idx) == 1) next

          #### Split this range into left and right halves
          #### For odd lenghts, mid floors, so left gets one fewer
          mid <- floor(length(idx) / 2)
          L <- idx[1:mid]
          R <- idx[(mid + 1):length(idx)]

          #### Extract each cluster covariance submatrices
          #### rows/cols are sector *names* in dendrogram order.
          cov_L <- covariance_matrix[
            eligible_tickers_order[L], eligible_tickers_order[L], drop = FALSE
          ]
          cov_R <- covariance_matrix[
            eligible_tickers_order[R], eligible_tickers_order[R], drop = FALSE
          ]

          #### Compute each half's cluster variance using IVP
          #### Inside the cluster, weights are proportional to 1 / diag(cov)
          #### Variance is w^T * cov * w
          #### IVP is stable (no matrix inversion)
          weights_L <- ivp_in_cluster(cov_L)
          weights_R <- ivp_in_cluster(cov_R)

          var_L <- as.numeric(t(weights_L) %*% cov_L %*% weights_L)
          var_R <- as.numeric(t(weights_R) %*% cov_R %*% weights_R)

          ##### Defensively check for non-positive variance
          if (var_L <= 0 | var_R <= 0){
            stop("Non-positive cluster variance encountered.")
          }

          #### Allocate weights to left and right halves inversely proportional to variance
          #### Alpha = 1 - var_L / (var_L + var_R)
          #### The left half gets weight alpha, the right half 1 - alpha ie
          #### the half with lower variance gets higher weight
          alpha <- 1 - var_L / (var_L + var_R)

          #### Apply exp_ret_score tilt within cluster if the case
          if (!is.null(exp_ret_score_tilt) && exp_ret_score_tilt == "inner" &&
              !is.null(exp_ret_score_tilt_eta) && exp_ret_score_tilt_eta > 0) {

            ##### IVP-weighted cluster-level alpha signals
            mu_L <- as.numeric(sum(weights_L * exp_ret_scores_ord[L]))
            mu_R <- as.numeric(sum(weights_R * exp_ret_scores_ord[R]))

            ##### Positive, scale-free mapping via ranks
            r <- rank(c(mu_L, mu_R), ties.method = "average")
            gL <- r[1]/2
            gR <- r[2]/2

            ##### Tilted allocators: risk * alpha_signal^eta
            A_L <- (1 / var_L) * (gL ^ exp_ret_score_tilt_eta)
            A_R <- (1 / var_R) * (gR ^ exp_ret_score_tilt_eta)
            alpha <- A_L / (A_L + A_R)

          }

          #### Downscale existing mass in each half by its allocation factor
          weights_sorted[L] <- weights_sorted[L] * alpha
          weights_sorted[R] <- weights_sorted[R] * (1 - alpha)

          #### Queue sub-ranges with more than one element to be split next
          if (length(L) > 1) new_idx_list <- c(new_idx_list, list(L))
          if (length(R) > 1) new_idx_list <- c(new_idx_list, list(R))

        }

        ### Replace current ranges with new generation
        idx_list <- new_idx_list

      }

      ## Map back from dendrogram order to the *original* group name order used elsewhere,
      ## then normalize
      weights <- weights_sorted[eligible_tickers] # map back to original group order
      weights <- weights / sum(weights) # normalize to 1

      # Apply alpha tilt overlay
      if (!is.null(exp_ret_score_tilt) && exp_ret_score_tilt == "final" &&
          !is.null(exp_ret_score_tilt_eta) && exp_ret_score_tilt_eta > 0) {

        ### Get exp_ret_scores vector in original order
        exp_ret_scores <- setNames(
          eligible_universe_m_d_ref$exp_ret_score,
          eligible_universe_m_d_ref$tickers
        )

        exp_ret_scores_vec <- exp_ret_scores[eligible_tickers]

        weights <- weights * (exp_ret_scores_vec ^ exp_ret_score_tilt_eta)
        weights <- weights / sum(weights)
      }

    # Merge back and return-----------------------------------------------------

      #Get weights and relative risk contribution
      hrp_weights <- data.frame(tickers = names(weights), weights = weights)

      #Merge with current_stock_universe
      universe_m_d_ref <- universe_m_d_ref %>%
        dplyr::left_join(hrp_weights, by = "tickers")

      #Replace NAs with zeros
      universe_m_d_ref[which(is.na(universe_m_d_ref$weights)),"weights"] <- 0

      #Check for weights different from 1
      if (abs(sum(universe_m_d_ref$weights) - 1) > 1e-6){
        stop("Weights do not sum to 1")
      }

      #Message
      if(verbose){
        cat("\n")
        cat(crayon::green(paste("HRP weights succesfully defined")))
        cat("\n")
        tictoc::toc()
      }

      #Return
      hc$call <- NULL
      hrp_results_list <- list(
        universe_m_d_ref = universe_m_d_ref,
        weights = universe_m_d_ref$weights,
        dist_matrix = dist_matrix,
        clusters = hc
      )

      return(hrp_results_list)
}





