#' Compute Short-Leg Underweights for the SLSAF Portfolio
#'
#' @description
#' Converts short-leg scores into the actual underweights of a Simulated Long-Short
#' Allocation Framework (`slsaf`) portfolio, and reports the active budget those
#' underweights release for the long leg.
#'
#' @details
#' The short budget is endogenous: it is the benchmark mass sitting outside the eligible
#' set, \eqn{T = \sum_{i \in S} b_i}. Desired underweights distribute that budget
#' according to the scores, and each one is then capped by the position actually held in
#' the benchmark:
#'
#' \deqn{u_i = \min(T \times s_i, b_i), \quad s_i = short\_score_i / \sum_j short\_score_j}
#'
#' \deqn{U = \sum_i u_i}
#'
#' **The cap is the mechanism, not a defect.** When \eqn{T s_i} exceeds \eqn{b_i} the
#' excess is discarded rather than redistributed to other short-block names. That is
#' deliberate: full redistribution has a fixed point where every ineligible constituent
#' goes to zero weight, which is precisely the ungraded behaviour this construction
#' exists to avoid. The binding cap is what produces a graded outcome, where mega caps
#' stay close to benchmark weight and small disliked names are eliminated.
#'
#' **Only uncapped names convert budget into underweight.** Since capped names
#' contribute a fixed \eqn{b_i} regardless of their score, \eqn{U} rises only when score
#' mass moves toward names whose cap is not binding. This is why
#' \code{bench_weight_tilt_eta} in \code{\link{compute_short_leg_scores}} raises the
#' realized budget while \code{badness_tilt_eta} does not.
#'
#' **The ceiling rescales proportionally.** When \code{max_short_budget} is supplied and
#' \eqn{U} exceeds it, every underweight is scaled by the same factor. Scaling down can
#' never violate \eqn{u_i \le b_i}, so the result stays feasible and \eqn{U} lands
#' exactly on the ceiling in one step, with no iteration. The reduction is spread evenly
#' across all names rather than concentrated on the uncapped ones.
#'
#' @param short_scores Numeric vector of strictly positive short-leg scores, typically
#'   produced by \code{\link{compute_short_leg_scores}}. Not required to be normalized.
#' @param bench_weights Numeric vector of benchmark weights for the same assets, in the
#'   same order. Must be strictly positive and finite.
#' @param max_short_budget Optional numeric scalar in (0, 1]. Hard ceiling on the
#'   realized active budget. \code{NULL} (default) leaves the budget fully endogenous.
#'
#' @return A named list with:
#' \describe{
#'   \item{\code{underweights}}{Named numeric vector of underweights \eqn{u_i}, each in
#'     \eqn{[0, b_i]}, in the input order.}
#'   \item{\code{short_budget}}{The available budget \eqn{T}, the benchmark mass of the
#'     short block.}
#'   \item{\code{active_budget}}{The realized budget \eqn{U = \sum u_i}, which is what
#'     the long leg receives. Always at most \code{short_budget}.}
#'   \item{\code{n_zeroed}}{Number of assets driven to a zero portfolio weight, i.e.
#'     whose underweight reached their full benchmark weight.}
#' }
#'
#' @seealso \code{\link{compute_short_leg_scores}}
compute_short_leg_underweights <- function(short_scores,
                                           bench_weights,
                                           max_short_budget = NULL){

  ## Tolerance for the feasibility assertions below
  tol <- 1e-10

  # Validate inputs-------------------------------------------------------------

    ## Scores
    if (!is.numeric(short_scores)){
      stop("short_scores must be numeric.")
    }
    if (any(is.na(short_scores))){
      stop("short_scores must not contain NAs.")
    }
    if (any(!is.finite(short_scores)) || any(short_scores <= 0)){
      stop("short_scores must be strictly positive and finite.")
    }

    ## Benchmark weights
    if (!is.numeric(bench_weights)){
      stop("bench_weights must be numeric.")
    }
    if (length(bench_weights) != length(short_scores)){
      stop("bench_weights must have the same length as short_scores.")
    }
    if (any(is.na(bench_weights))){
      stop("bench_weights must not contain NAs.")
    }
    if (any(!is.finite(bench_weights)) || any(bench_weights <= 0) || any(bench_weights > 1)){
      stop("bench_weights must be strictly positive, finite and not greater than 1.")
    }

    ## Ceiling
    if (!is.null(max_short_budget)){
      if (!is.numeric(max_short_budget) || length(max_short_budget) != 1 ||
          !is.finite(max_short_budget) || max_short_budget <= 0 || max_short_budget > 1){
        stop("max_short_budget must be NULL or a single numeric value in (0, 1].")
      }
    }

    ## Empty short block: no benchmark mass to release, so nothing to underweight
    if (length(short_scores) == 0){
      return(list(
        underweights  = stats::setNames(numeric(0), names(short_scores)),
        short_budget  = 0,
        active_budget = 0,
        n_zeroed      = 0L
      ))
    }

  # Distribute the budget-------------------------------------------------------

    ## Available budget: the benchmark mass held by the short block
    short_budget <- sum(bench_weights)

    ## Normalized short weights
    short_weights <- short_scores / sum(short_scores)

    ## Desired underweight, then capped by the position actually held
    ### Anything above b_i is discarded on purpose: see Details
    desired_underweights <- short_budget * short_weights
    underweights <- pmin(desired_underweights, bench_weights)

  # Apply the ceiling-----------------------------------------------------------

    ## Realized active budget before any ceiling
    active_budget <- sum(underweights)

    if (!is.null(max_short_budget) && active_budget > max_short_budget){
      ### Proportional rescaling: cannot break u_i <= b_i, and lands on the ceiling
      ### exactly, so no iteration is needed
      underweights <- underweights * (max_short_budget / active_budget)
      active_budget <- sum(underweights)
    }

  # Verify feasibility and return-----------------------------------------------

    ## The invariants the rest of the construction depends on
    if (any(underweights < -tol)){
      stop("Computed underweights must be non-negative.")
    }
    if (any(underweights > bench_weights + tol)){
      stop("Computed underweights must never exceed benchmark weights.")
    }
    if (active_budget > short_budget + tol){
      stop("Realized active budget must never exceed the short budget.")
    }

    ## Preserve identifiers when the caller supplied them
    names(underweights) <- names(short_scores)

    ## Assets whose underweight consumed their whole benchmark position
    n_zeroed <- sum(underweights >= bench_weights - tol)

    return(list(
      underweights  = underweights,
      short_budget  = short_budget,
      active_budget = active_budget,
      n_zeroed      = as.integer(n_zeroed)
    ))
}
