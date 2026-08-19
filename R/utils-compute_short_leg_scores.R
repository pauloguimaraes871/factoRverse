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
#' \deqn{short\_score_i = badness_i ^ {badness\_tilt\_eta} \times g_i ^ {bench\_weight\_tilt\_eta}}
#'
#' \describe{
#'   \item{\code{badness_i = 1 / exp_ret_score_raw_i}}{The reciprocal is the canonical
#'     inversion in this package rather than an ad hoc choice: \code{signal_transform()}
#'     maps \eqn{z > 0} to \eqn{1 + z} and \eqn{z < 0} to \eqn{1 / (1 - z)}, so
#'     \eqn{f(-z) = 1 / f(z)} identically. The reciprocal of a score is therefore exactly
#'     the score the same signal would produce under the opposite position.}
#'   \item{\code{g_i = signal_transform(bench_weights)}}{The winsorized cross-sectional
#'     transform of the benchmark weight, computed within the short block. Using the
#'     transform rather than the raw weight compresses the spread between a mega cap and
#'     a small constituent, which is what makes \code{bench_weight_tilt_eta} a smooth
#'     dial instead of a switch, and which naturally mutes the effect on benchmarks that
#'     are not concentrated.}
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
#' **What each exponent actually does.** \code{bench_weight_tilt_eta} is the *budget*
#' dial: it moves desired underweight toward high-benchmark-weight names, where the
#' \eqn{u_i \le b_i} cap applied downstream by
#' \code{\link{compute_short_leg_underweights}} is far from binding, so more of the
#' available budget converts into realized underweight. \code{badness_tilt_eta} is the
#' *conviction-concentration* dial: it punishes the worst names disproportionately and
#' borderline names disproportionately less, but because it concentrates the score
#' distribution it pushes more names past their cap, so it does not increase (and
#' usually mildly decreases) the realized active budget.
#'
#' @param exp_ret_score_raw Numeric vector of unscaled expected return scores
#'   (the \code{exp_ret_score_raw} column, never the scaled \code{exp_ret_score}).
#'   Must be strictly positive and finite, which \code{\link{signal_transform}}
#'   guarantees by construction.
#' @param bench_weights Numeric vector of benchmark weights for the same assets, in the
#'   same order. Must be strictly positive and finite: every asset in the short block is
#'   by definition a benchmark constituent.
#' @param badness_tilt_eta Numeric scalar exponent applied to the badness score.
#'   Defaults to 1 (no extra tilt). Values above 1 concentrate underweight on the worst
#'   names; values below 1 flatten it toward an equal underweight.
#' @param bench_weight_tilt_eta Numeric scalar exponent applied to the transformed
#'   benchmark weight. Defaults to 0, which switches the benchmark tilt off entirely
#'   (\eqn{g^0 = 1}). Positive values shift underweight toward large index names.
#' @param lower_quantile_winsorization Numeric in (0, 1). Lower winsorization quantile
#'   passed to \code{\link{signal_transform}} when transforming benchmark weights.
#' @param upper_quantile_winsorization Numeric in (0, 1). Upper winsorization quantile
#'   passed to \code{\link{signal_transform}} when transforming benchmark weights.
#'
#' @return A named numeric vector of strictly positive short-leg scores, with the same
#'   length and names as \code{exp_ret_score_raw}. The scores are not normalized: the
#'   downstream signal-weighted call normalizes them to sum to 1.
#'
#' @seealso \code{\link{compute_short_leg_underweights}}, \code{\link{signal_transform}}
compute_short_leg_scores <- function(exp_ret_score_raw,
                                     bench_weights,
                                     badness_tilt_eta = 1,
                                     bench_weight_tilt_eta = 0,
                                     lower_quantile_winsorization = 0.025,
                                     upper_quantile_winsorization = 0.975){

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

    ## Benchmark weight tilt, computed within the short block
    ### signal_transform() already handles the degenerate cases: a single element
    ### returns 1, and a zero-variance vector returns all ones. In both cases the
    ### tilt correctly becomes inert rather than undefined.
    if (bench_weight_tilt_eta == 0){
      ### Skip the transform entirely when the tilt is off: g^0 = 1 regardless
      bench_weight_score <- rep(1, length(bench_weights))
    } else {
      bench_weight_score <- signal_transform(
        bench_weights,
        lower_quantile_winsorization = lower_quantile_winsorization,
        upper_quantile_winsorization = upper_quantile_winsorization
      )
    }

  # Combine and return----------------------------------------------------------

    ## Two positive scores, two exponents
    short_scores <- (badness ^ badness_tilt_eta) * (bench_weight_score ^ bench_weight_tilt_eta)

    ## Preserve identifiers when the caller supplied them
    names(short_scores) <- names(exp_ret_score_raw)

    ## Defensively confirm the result is usable as a weighting score
    if (any(!is.finite(short_scores)) || any(short_scores <= 0)){
      stop("Computed short-leg scores must be strictly positive and finite.")
    }

    return(short_scores)
}
