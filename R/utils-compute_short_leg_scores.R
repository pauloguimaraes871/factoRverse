#' Compute Short-Leg Scores for the SLSAF Portfolio
#'
#' @description
#' Builds the score that drives the underweight (short) leg of a Simulated Long-Short
#' Allocation Framework (`slsaf`) portfolio. The short leg is applied to benchmark
#' constituents that failed regular eligibility, and its score answers a different
#' question from `exp_ret_score`: not "how much do I want to own this", but "how much
#' conviction do I have to hold less of it than the benchmark does".
#'
#' @details
#' The score is the product of two strictly positive terms, each with its own exponent:
#'
#' \deqn{short\_score_i = b_i ^ {bench\_weight\_tilt\_eta} \times badness_i ^ {badness\_tilt\_eta}}
#'
#' \describe{
#'   \item{\code{badness_i = 1 / exp_ret_score_raw_i}}{The reciprocal is the canonical
#'     inversion in this package rather than an ad hoc choice: \code{signal_transform()}
#'     maps \eqn{z > 0} to \eqn{1 + z} and \eqn{z < 0} to \eqn{1 / (1 - z)}, so
#'     \eqn{f(-z) = 1 / f(z)} identically. The reciprocal of a score is therefore exactly
#'     the score the same signal would produce under the opposite position.}
#'   \item{\code{b_i}}{The raw benchmark weight, which anchors the short leg on the
#'     position actually held. The raw weight is used rather than a cross-sectional
#'     transform of it because only the raw weight makes the budget-maximizing point
#'     exactly representable, see below.}
#' }
#'
#' **The scaler is deliberately absent.** The long leg uses
#' \code{exp_ret_score = exp_ret_score_raw * scaler}, but the short leg is reconstructed
#' from \code{exp_ret_score_raw} only. Inverting a scaler would invert its economic
#' meaning: a scaler such as \code{1 / idio_vol} would make the short leg underweight
#' low-volatility stocks the most, silently shorting a documented return premium as a
#' side effect of a plumbing decision. Any scaler that is itself return-predictive must
#' not reach the short leg.
#'
#' **What each exponent actually does.** Downstream,
#' \code{\link{compute_short_leg_underweights}} spends a budget \eqn{T = \sum_i b_i}
#' subject to \eqn{u_i \le b_i}. Because \eqn{\sum_i T s_i = T = \sum_i b_i}, the budget
#' is fully spent if and only if the normalized short weights are benchmark-proportional
#' (\eqn{s \propto b}), and any departure from proportionality strictly loses budget.
#' That gives both exponents a clean reading:
#'
#' \itemize{
#'   \item \code{bench_weight_tilt_eta} sets the *basis*. At 1 the benchmark term alone
#'         is exactly proportional to \eqn{b}, which is the budget-maximizing anchor. At
#'         0 the benchmark is ignored entirely and the score is pure badness.
#'   \item \code{badness_tilt_eta} then walks the *budget-versus-grading frontier*. At 0
#'         (with \code{bench_weight_tilt_eta = 1}) the whole budget is spent, which is
#'         also the ungraded case where every ineligible constituent is sold in full.
#'         Raising it concentrates underweight on the worst names and monotonically
#'         gives up budget in exchange, which is the graded behaviour this construction
#'         exists to produce.
#' }
#'
#' Maximum budget and graded underweights are therefore mutually exclusive by
#' construction, and \code{badness_tilt_eta} is the parameter that prices the trade.
#'
#' @param exp_ret_score_raw Numeric vector of unscaled expected return scores
#'   (the \code{exp_ret_score_raw} column, never the scaled \code{exp_ret_score}).
#'   Must be strictly positive and finite, which \code{\link{signal_transform}}
#'   guarantees by construction.
#' @param bench_weights Numeric vector of benchmark weights for the same assets, in the
#'   same order. Must be strictly positive and finite: every asset in the short block is
#'   by definition a benchmark constituent.
#' @param badness_tilt_eta Numeric scalar exponent applied to the badness score.
#'   Defaults to 1. Values above 1 concentrate underweight on the worst names at the cost
#'   of budget; 0 removes the conviction tilt entirely.
#' @param bench_weight_tilt_eta Numeric scalar exponent applied to the benchmark weight.
#'   Defaults to 1, the benchmark-proportional anchor. 0 removes the benchmark term
#'   (\eqn{b^0 = 1}), leaving a pure badness score.
#'
#' @return A named numeric vector of strictly positive short-leg scores, with the same
#'   length and names as \code{exp_ret_score_raw}. The scores are not normalized: the
#'   downstream signal-weighted call normalizes them to sum to 1.
#'
#' @seealso \code{\link{compute_short_leg_underweights}}, \code{\link{signal_transform}}
compute_short_leg_scores <- function(exp_ret_score_raw,
                                     bench_weights,
                                     badness_tilt_eta = 1,
                                     bench_weight_tilt_eta = 1){

  # Validate inputs-------------------------------------------------------------

    ## Scores
    if (!is.numeric(exp_ret_score_raw)){
      stop("exp_ret_score_raw must be numeric.")
    }
    if (length(exp_ret_score_raw) == 0){
      stop("exp_ret_score_raw must have at least one element.")
    }
    if (any(is.na(exp_ret_score_raw))){
      stop("exp_ret_score_raw must not contain NAs.")
    }
    ### signal_transform() guarantees strict positivity, so a non-positive value means
    ### the input did not come from it and the reciprocal would be meaningless.
    if (any(!is.finite(exp_ret_score_raw)) || any(exp_ret_score_raw <= 0)){
      stop("exp_ret_score_raw must be strictly positive and finite.")
    }

    ## Benchmark weights
    if (!is.numeric(bench_weights)){
      stop("bench_weights must be numeric.")
    }
    if (length(bench_weights) != length(exp_ret_score_raw)){
      stop("bench_weights must have the same length as exp_ret_score_raw.")
    }
    if (any(is.na(bench_weights))){
      stop("bench_weights must not contain NAs.")
    }
    ### Every short-block asset is a benchmark constituent by construction
    if (any(!is.finite(bench_weights)) || any(bench_weights <= 0) || any(bench_weights > 1)){
      stop("bench_weights must be strictly positive, finite and not greater than 1.")
    }

    ## Exponents
    if (!is.numeric(badness_tilt_eta) || length(badness_tilt_eta) != 1 ||
        !is.finite(badness_tilt_eta)){
      stop("badness_tilt_eta must be a single finite numeric value.")
    }
    if (!is.numeric(bench_weight_tilt_eta) || length(bench_weight_tilt_eta) != 1 ||
        !is.finite(bench_weight_tilt_eta)){
      stop("bench_weight_tilt_eta must be a single finite numeric value.")
    }

  # Build the two components----------------------------------------------------

    ## Badness: the score the same signal would give under the opposite position
    badness <- 1 / exp_ret_score_raw

    ## Benchmark anchor: the raw weight, so that an exponent of 1 reproduces
    ## benchmark proportionality exactly, which is the budget-maximizing point
    bench_weight_score <- bench_weights

  # Combine and return----------------------------------------------------------

    ## Two positive scores, two exponents
    short_scores <- (bench_weight_score ^ bench_weight_tilt_eta) * (badness ^ badness_tilt_eta)

    ## Preserve identifiers when the caller supplied them
    names(short_scores) <- names(exp_ret_score_raw)

    ## Defensively confirm the result is usable as a weighting score
    if (any(!is.finite(short_scores)) || any(short_scores <= 0)){
      stop("Computed short-leg scores must be strictly positive and finite.")
    }

    return(short_scores)
}
