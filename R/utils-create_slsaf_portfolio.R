#' Create a Simulated Long-Short Allocation Framework (SLSAF) portfolio
#'
#' @description
#' Builds a long-only, benchmark-relative portfolio as a benchmark position plus a
#' self-financing active overlay. The universe is split into two conviction blocks by
#' \code{\link{classify_investment_universe}}: a long block (what the eligibility cascade
#' is willing to buy) and a short block (benchmark constituents the cascade rejected,
#' which may only be underweighted). The overlay underweights the short block according
#' to conviction, and spends exactly the budget it releases on the long block.
#'
#' @details
#' For benchmark weights \eqn{b_i}, long block \eqn{L} and short block \eqn{S}:
#'
#' \enumerate{
#'   \item \strong{Short budget.} \eqn{T = \sum_{i \in S} b_i}, the benchmark mass sitting
#'     outside the eligible set. It is endogenous by design: when the index heavyweights
#'     score well they are eligible, \eqn{T} shrinks and the portfolio hugs the benchmark.
#'   \item \strong{Short leg.} A signal-weighted sub-portfolio over \eqn{S} on the score
#'     from \code{\link{compute_short_leg_scores}}, giving \eqn{s}.
#'   \item \strong{Underweights.} \eqn{u_i = \min(T s_i, b_i)}, optionally rescaled to a
#'     ceiling, via \code{\link{compute_short_leg_underweights}}. The realized active
#'     budget is \eqn{U = \sum_i u_i}.
#'   \item \strong{Long leg.} A sub-portfolio over \eqn{L} built with
#'     \code{long_port_config}, giving \eqn{l}.
#'   \item \strong{Combine.} \eqn{w_i = b_i - u_i} on \eqn{S}, \eqn{w_i = b_i + U l_i} on
#'     \eqn{L}, and 0 elsewhere.
#' }
#'
#' The construction guarantees, and asserts before returning:
#' \itemize{
#'   \item active weights sum to zero exactly, so weights sum to 1 with no renormalization;
#'   \item \eqn{0 \le w_i \le b_i} on the short block, so a disliked constituent is never
#'     overweighted and never shorted;
#'   \item \eqn{w_i \ge b_i} on the long block, so an eligible name is never structurally
#'     underweighted, which is the problem this method exists to solve;
#'   \item long-only throughout.
#' }
#'
#' Note that the long leg weights \eqn{l} are \emph{active} weights, not total weights.
#' The sub-portfolios are therefore built with \code{selected_benchmark = NULL}: a
#' benchmark-relative statistic computed on an active-weight vector would be misleading.
#' The parent portfolio still reports full active statistics against the real benchmark.
#'
#' @param universe_m_d_ref A single-date data frame carrying `is_eligible`,
#'   `is_long_candidate`, `is_short_candidate`, `exp_ret_score`, the benchmark weight
#'   column, and ideally `exp_ret_score_raw`. Produced by
#'   \code{\link{classify_investment_universe}} with `include_benchmark_in_universe = TRUE`.
#' @param selected_benchmark Character scalar naming the benchmark. The universe must
#'   carry a `<selected_benchmark>_bench_weights` column.
#' @param long_port_config A `sub_port_config` describing how to build the long leg.
#' @param bench_weight_tilt_eta,badness_tilt_eta Numeric exponents of the short-leg score,
#'   passed to \code{\link{compute_short_leg_scores}}.
#' @param max_short_budget Optional numeric in (0, 1]. Ceiling on the realized active
#'   budget, passed to \code{\link{compute_short_leg_underweights}}.
#' @param covariance_matrix Optional covariance matrix of the eligible assets, in the
#'   eligible universe order. Sub-portfolios receive the relevant submatrix rather than
#'   re-estimating, so both legs share the parent risk model.
#' @param eligible_returns_m_xts_upd_ref,selected_benchmark_m_xts_upd_ref,active_returns,cov_estimation_method,cov_matrix_sample_size
#'   Covariance estimation inputs, forwarded to the long leg when it needs them.
#' @param groups_m_d_ref,liquidity_m_d_ref,cap_weighting_metric Optional data forwarded to
#'   the long leg.
#' @param lower_quantile_winsorization,upper_quantile_winsorization Numerics in (0, 1),
#'   forwarded to the long leg.
#' @param parallel Logical, forwarded to the long leg.
#' @param verbose Logical, print progress and timing via `tictoc`.
#'
#' @return A list with:
#' \describe{
#'   \item{universe_m_d_ref}{Input universe joined with final `weights`.}
#'   \item{weights}{Final portfolio weights.}
#'   \item{underweights}{Named numeric vector of short-block underweights.}
#'   \item{short_budget}{The available budget \eqn{T}.}
#'   \item{active_budget}{The realized budget \eqn{U}.}
#'   \item{n_long,n_short,n_zeroed}{Block sizes and the number of fully sold constituents.}
#'   \item{micro}{`list(long = <port or NULL>, short = <port or NULL>)`.}
#' }
#'
#' @seealso \code{\link{compute_short_leg_scores}},
#'   \code{\link{compute_short_leg_underweights}}, \code{\link{set_portfolio_weights}}
create_slsaf_portfolio <- function(universe_m_d_ref,
                                   selected_benchmark,
                                   long_port_config,
                                   bench_weight_tilt_eta = 1,
                                   badness_tilt_eta = 1,
                                   max_short_budget = NULL,
                                   covariance_matrix = NULL,
                                   eligible_returns_m_xts_upd_ref = NULL,
                                   selected_benchmark_m_xts_upd_ref = NULL,
                                   active_returns = if (is.null(selected_benchmark_m_xts_upd_ref)) FALSE else TRUE,
                                   cov_estimation_method = "sample",
                                   cov_matrix_sample_size = if (is.null(eligible_returns_m_xts_upd_ref)) NULL else nrow(eligible_returns_m_xts_upd_ref),
                                   groups_m_d_ref = NULL,
                                   liquidity_m_d_ref = NULL,
                                   cap_weighting_metric = NULL,
                                   lower_quantile_winsorization = 0.025,
                                   upper_quantile_winsorization = 0.975,
                                   parallel = FALSE,
                                   verbose = TRUE){

  ## Tolerances: one for declaring a budget economically empty, one for assertions, and
  ## one bounding how far the benchmark may be renormalized before the input is refused.
  ## The last two answer different questions and must not be conflated. tol_bench_gap
  ## judges input data quality, and is the loose one because an index that is 0.2% short
  ## of full coverage is still that index. tol_check judges this function's own
  ## arithmetic, and is tight because nothing here should be off by more than float
  ## noise; it also bounds the long-only and never-overweight invariants, which would
  ## become meaningless at a data-quality tolerance.
  tol_empty <- 1e-8
  tol_check <- 1e-6
  tol_bench_gap <- 2e-3

  # Initial Setup---------------------------------------------------------------

    ## Message
    if (isTRUE(verbose)) {
      tictoc::tic()
      cat("\n")
      cat("Deriving weights through SLSAF (Simulated Long-Short Allocation Framework)...")
      cat("\n")
      cat("Long leg method: ", long_port_config@port_construction_method)
      cat("\n")
    }

    ## Validate the block split produced upstream
    required_cols <- c("is_eligible", "is_long_candidate", "is_short_candidate", "exp_ret_score")
    missing_cols <- setdiff(required_cols, colnames(universe_m_d_ref))
    if (length(missing_cols) > 0){
      stop(paste0("universe_m_d_ref is missing column(s): ", paste(missing_cols, collapse = ", "),
                  ". Run classify_investment_universe() with include_benchmark_in_universe = TRUE."))
    }

    ## Validate the benchmark
    if (is.null(selected_benchmark) || length(selected_benchmark) != 1 ||
        !is.character(selected_benchmark)){
      stop("selected_benchmark must be a single character string for slsaf.")
    }
    bench_weights_col <- paste0(selected_benchmark, "_bench_weights")
    if (!bench_weights_col %in% colnames(universe_m_d_ref)){
      stop(paste0(bench_weights_col, " not found in universe_m_d_ref."))
    }
    if (any(is.na(universe_m_d_ref[[bench_weights_col]]))){
      stop("Benchmark weights must not contain NAs.")
    }

    ## Validate the long leg configuration
    if (!methods::is(long_port_config, "sub_port_config")){
      stop("long_port_config must be an object of class 'sub_port_config'.")
    }

    ## Blocks
    eligible_universe_m_d_ref <- universe_m_d_ref %>% dplyr::filter(is_eligible == 1)
    long_universe_m_d_ref     <- universe_m_d_ref %>% dplyr::filter(is_long_candidate == 1L)
    short_universe_m_d_ref    <- universe_m_d_ref %>% dplyr::filter(is_short_candidate == 1L)

    eligible_tickers <- eligible_universe_m_d_ref %>% dplyr::pull(tickers)
    long_tickers     <- long_universe_m_d_ref %>% dplyr::pull(tickers)
    short_tickers    <- short_universe_m_d_ref %>% dplyr::pull(tickers)

      ### A long block is what the released budget is spent on
      if (length(long_tickers) == 0){
        stop("slsaf requires at least one long candidate.")
      }

      ### Defensively check covariance alignment, as every caller relies on it
      if (!is.null(covariance_matrix)){
        if (!identical(eligible_tickers, rownames(covariance_matrix)) ||
            !identical(eligible_tickers, colnames(covariance_matrix))){
          stop("Covariance matrix rownames/colnames do not match eligible tickers.")
        }
      }

    ## Benchmark weights
    ### Constituents absent from the universe cannot be represented, so their weight is
    ### unallocated and the identity sum(w) = sum(b) = 1 would silently fail. Small gaps
    ### are repaired by renormalizing over what the universe actually covers; large ones
    ### are refused.
    bench_weights_all <- universe_m_d_ref[[bench_weights_col]]
    bench_weights_total <- sum(bench_weights_all)
    bench_weights_gap <- abs(bench_weights_total - 1)

    if (bench_weights_total <= tol_empty){
      stop("Benchmark weights in universe_m_d_ref sum to zero.")
    }

    ### Past this point renormalization stops being a numerical repair and becomes the
    ### substitution of a different index: every surviving weight is inflated by the gap,
    ### so active weights, tracking error and the whole benchmark-relative premise would
    ### be measured against a benchmark that exists nowhere, announced only by a warning
    ### buried in backtest output. Refuse instead. The check is symmetric because a sum
    ### above 1 is not a coverage gap at all but duplicated or overstated weights, which
    ### is a corrupted input rather than an incomplete one.
    if (bench_weights_gap > tol_bench_gap){
      stop(paste0("Benchmark weights in universe_m_d_ref sum to ",
                  round(bench_weights_total, 6), ", which is further from 1 than the ",
                  tol_bench_gap, " renormalization allowance. ",
                  if (bench_weights_total < 1){
                    "Too much of the index is missing from the universe to track it: renormalizing would silently redefine the benchmark."
                  } else {
                    "Benchmark weights above 1 indicate duplicated or overstated constituents rather than an incomplete universe."
                  }))
    }

    ### Below the refusal threshold, normalization must still cover every gap the final
    ### sum-to-one assertion would reject. Gating it at a looser tolerance than the
    ### assertion is non-monotone in data quality: sum(w) = sum(b) exactly, because the
    ### overlay is self-financing, so a benchmark file rounded to four decimals (0.99995)
    ### would skip normalization and then die at the assertion, while a worse file would
    ### normalize and pass. Gating it at the same tolerance is no better, because it
    ### leaves the assertion boundary itself unrepaired: index files are published
    ### rounded, so a gap arrives as an exact multiple of the quantum, and a gap of one
    ### quantum is then inherited whole by the portfolio and compared against the very
    ### tolerance it equals. Whether it passes is decided by the float noise of b - u on
    ### the short block and b + U*l on the long block, which is to say by which long-leg
    ### method happened to run. Repairing every gap removes the boundary rather than
    ### moving it, and costs one division by a number within tol_bench_gap of 1.
    ###
    ### The acceptance decision is not made here. tol_bench_gap above is what refuses an
    ### input, and the warning below is what reports one, so this gate changes only
    ### whether an already-accepted gap is carried or closed.
    if (bench_weights_gap > 0){

      if (bench_weights_gap > 1e-4){
        warning(paste0("Benchmark weights in universe_m_d_ref sum to ",
                       round(bench_weights_total, 6),
                       ", not 1. Renormalizing: some constituents are absent from the universe."))
      }

      bench_weights_all <- bench_weights_all / bench_weights_total

      #### Persist the normalization. Downstream, set_portfolio_weights() rebuilds the
      #### benchmark portfolio and every active statistic from this column, and the leg
      #### diagnostics read it back as bench_weight. Leaving the original in place would
      #### report active weights measured against a different benchmark from the one the
      #### weights were actually constructed against. The refusal above is what keeps this
      #### persistence bounded: the stored benchmark can differ from the source file by at
      #### most tol_bench_gap, which cannot move a conclusion.
      universe_m_d_ref[[bench_weights_col]] <- bench_weights_all
    }
    names(bench_weights_all) <- universe_m_d_ref %>% dplyr::pull(tickers)

  # Short leg-------------------------------------------------------------------

    ## An empty short block releases no budget, so the portfolio is the benchmark itself
    if (length(short_tickers) == 0){

      if (isTRUE(verbose)){
        cat(crayon::yellow("\nNo short candidates: every constituent is eligible. Portfolio equals the benchmark.\n"))
      }

      underweights  <- stats::setNames(numeric(0), character(0))
      short_budget  <- 0
      active_budget <- 0
      n_zeroed      <- 0L
      short_port    <- NULL

    } else {

      ### Score the short block
      #### The short leg is always reconstructed from the unscaled score. Falling back to
      #### exp_ret_score when exp_ret_score_raw is absent would be safe only if a scaler
      #### could be reliably detected, and a universe that was scaled and then had its
      #### scaler column dropped would silently invert it. The reciprocal identity
      #### 1/f(z) = f(-z) is also a property of the raw score specifically, so requiring
      #### it keeps the contract aligned with its justification.
      if (!"exp_ret_score_raw" %in% colnames(short_universe_m_d_ref)){
        stop("exp_ret_score_raw is required for slsaf: the short leg is built from the unscaled score so that a scaler is never inverted.")
      }
      short_exp_ret_score_raw <- short_universe_m_d_ref %>% dplyr::pull(exp_ret_score_raw)

      short_bench_weights <- bench_weights_all[short_tickers]

      short_scores <- compute_short_leg_scores(
        exp_ret_score_raw     = stats::setNames(short_exp_ret_score_raw, short_tickers),
        bench_weights         = short_bench_weights,
        badness_tilt_eta      = badness_tilt_eta,
        bench_weight_tilt_eta = bench_weight_tilt_eta
      )

      ### Build the short sub-portfolio through the shared machinery. Its weights are the
      ### normalized short scores; the port object is kept for diagnostics.
      #### The score column is substituted, so this port's exp_ret_score slot holds
      #### badness, not expected return.
      short_sub_universe_m_d_ref <- short_universe_m_d_ref %>%
        dplyr::mutate(exp_ret_score = as.numeric(short_scores[tickers]))

      if (isTRUE(verbose)){
        cat("\nBuilding the short leg...\n")
      }

      short_port <- set_portfolio_weights(
        universe_m_d_ref = short_sub_universe_m_d_ref,
        #### Always signal weighted: the underweight must follow conviction, and a
        #### risk-based method would grant the largest underweight to the name
        #### contributing least to active risk, which is backwards.
        port_construction_method = "sw",
        covariance_matrix = if (is.null(covariance_matrix)) NULL else
          covariance_matrix[short_tickers, short_tickers, drop = FALSE],
        groups_m_d_ref = NULL,
        selected_benchmark = NULL,
        level = "sub_port",
        verbose = FALSE
      )

      short_weights <- short_port@universe_m_d_ref@data %>%
        dplyr::arrange(match(tickers, short_tickers)) %>%
        dplyr::pull(weights)
      names(short_weights) <- short_tickers

      ### Convert scores into underweights, capped by the position actually held
      short_leg_results <- compute_short_leg_underweights(
        short_scores     = short_weights,
        bench_weights    = short_bench_weights,
        max_short_budget = max_short_budget
      )

      underweights  <- short_leg_results$underweights
      short_budget  <- short_leg_results$short_budget
      active_budget <- short_leg_results$active_budget
      n_zeroed      <- short_leg_results$n_zeroed
    }

    ## Message
    if (isTRUE(verbose)){
      cat(paste0("\nShort budget: ", round(short_budget * 100, 2), "% | ",
                 "Realized active budget: ", round(active_budget * 100, 2), "% | ",
                 "Constituents sold in full: ", n_zeroed, "\n"))
    }

  # Long leg--------------------------------------------------------------------

    ## With no released budget there is nothing to allocate, and the portfolio is the
    ## benchmark restricted to the universe
    if (active_budget <= tol_empty){

      if (isTRUE(verbose)){
        cat(crayon::yellow("\nNo active budget released. Long leg skipped.\n"))
      }

      long_weights <- stats::setNames(rep(0, length(long_tickers)), long_tickers)
      long_port    <- NULL

    } else {

      if (isTRUE(verbose)){
        cat("\nBuilding the long leg...\n")
      }

      ### Parameterize the inner call from its own configuration
      long_args <- expand_sub_port_config(long_port_config)

      long_port <- do.call(
        set_portfolio_weights,
        c(
          list(
            universe_m_d_ref = long_universe_m_d_ref,
            #### The parent risk model, subset. Re-estimating would give the two legs
            #### inconsistent covariances for no benefit.
            covariance_matrix = if (is.null(covariance_matrix)) NULL else
              covariance_matrix[long_tickers, long_tickers, drop = FALSE],
            eligible_returns_m_xts_upd_ref = if (is.null(eligible_returns_m_xts_upd_ref)) NULL else
              eligible_returns_m_xts_upd_ref[, col_match(eligible_returns_m_xts_upd_ref, long_tickers), drop = FALSE],
            selected_benchmark_m_xts_upd_ref = selected_benchmark_m_xts_upd_ref,
            active_returns = active_returns,
            cov_estimation_method = cov_estimation_method,
            cov_matrix_sample_size = cov_matrix_sample_size,
            groups_m_d_ref = groups_m_d_ref,
            liquidity_m_d_ref = liquidity_m_d_ref,
            cap_weighting_metric = cap_weighting_metric,
            #### The long leg produces active weights, so a benchmark-relative statistic
            #### on them would be misleading. The parent reports the real ones.
            selected_benchmark = NULL,
            lower_quantile_winsorization = lower_quantile_winsorization,
            upper_quantile_winsorization = upper_quantile_winsorization,
            parallel = parallel,
            level = "sub_port",
            verbose = FALSE
          ),
          long_args
        )
      )

      long_weights <- long_port@universe_m_d_ref@data %>%
        dplyr::arrange(match(tickers, long_tickers)) %>%
        dplyr::pull(weights)
      names(long_weights) <- long_tickers
    }

  # Combine---------------------------------------------------------------------

    ## Active weight: released on the short block, spent on the long block
    active_weights <- stats::setNames(rep(0, nrow(universe_m_d_ref)),
                                      universe_m_d_ref %>% dplyr::pull(tickers))

    if (length(short_tickers) > 0){
      active_weights[short_tickers] <- -underweights[short_tickers]
    }
    active_weights[long_tickers] <- active_budget * long_weights[long_tickers]

    ## Final weights are the benchmark plus the overlay, and zero outside both blocks
    final_weights <- bench_weights_all + active_weights
    final_weights[setdiff(names(final_weights), c(long_tickers, short_tickers))] <- 0

  # Diagnose the leg score ordering---------------------------------------------

    ## The construction never depends on the two blocks being ordered by score, but the
    ## economics do. Eligibility governs what may be bought, so a rule that excludes a
    ## high-scoring name for reasons other than its score, most often a strict liquidity
    ## floor, pushes it into the short block while it remains an index position. On a
    ## narrow universe that can invert the ordering outright, and the portfolio then
    ## underweights precisely the names the signal likes. Nothing else in the pipeline
    ## would surface that, so compare what is being sold against what is being bought.
    if (length(short_tickers) > 0 && active_budget > tol_empty){

      short_exp_ret_score <- short_universe_m_d_ref %>% dplyr::pull(exp_ret_score)
      long_exp_ret_score  <- long_universe_m_d_ref %>% dplyr::pull(exp_ret_score)

      ### Weight each leg by the active exposure actually taken in it
      sold_weights   <- underweights[short_tickers]
      bought_weights <- long_weights[long_tickers]

      if (sum(sold_weights) > tol_empty && sum(bought_weights) > tol_empty){

        sold_score   <- stats::weighted.mean(short_exp_ret_score, sold_weights)
        bought_score <- stats::weighted.mean(long_exp_ret_score, bought_weights)

        if (isTRUE(sold_score > bought_score)){
          warning(paste0(
            "slsaf: the underweighted names score better on average than the overweighted ones (",
            round(sold_score, 4), " sold vs ", round(bought_score, 4), " bought). ",
            "This usually means an eligibility rule other than the score, such as a liquidity floor, ",
            "is pushing high-scoring constituents into the short block."
          ))
        }
      }
    }

  # Verify the construction-----------------------------------------------------

    ## Clean tiny numerical crumbs left by the arithmetic above, before anything is
    ## asserted. Mutating the vector afterwards would leave the invariants describing a
    ## portfolio other than the one returned: zeroing enough positions can move the sum
    ## by more than the tolerance the sum-to-one check just accepted.
    final_weights[abs(final_weights) < tol_empty] <- 0

    ### Re-derive the overlay from the cleaned weights, so the self-financing check below
    ### describes the portfolio being returned rather than the pre-clean one
    active_weights <- final_weights - bench_weights_all

    ## The overlay must be self-financing
    if (abs(sum(active_weights)) > tol_check){
      stop(paste0("Active weights must sum to zero, got ", sum(active_weights), "."))
    }

    ## Long-only and fully invested
    if (any(final_weights < -tol_check)){
      stop("Final weights must be non-negative.")
    }
    if (abs(sum(final_weights) - 1) > tol_check){
      stop(paste0("Weights do not sum to 1, got ", sum(final_weights), "."))
    }

    ## A disliked constituent is never overweighted, an eligible name never underweighted
    if (length(short_tickers) > 0 &&
        any(final_weights[short_tickers] > bench_weights_all[short_tickers] + tol_check)){
      stop("Short-block weights must never exceed benchmark weights.")
    }
    if (any(final_weights[long_tickers] < bench_weights_all[long_tickers] - tol_check)){
      stop("Long-block weights must never fall below benchmark weights.")
    }

  # Merge back and return-------------------------------------------------------

    ## Attach weights to the universe
    universe_m_d_ref <- universe_m_d_ref %>%
      dplyr::left_join(
        data.frame(tickers = names(final_weights),
                   weights = as.numeric(final_weights),
                   stringsAsFactors = FALSE),
        by = "tickers"
      )
    universe_m_d_ref$weights[which(is.na(universe_m_d_ref$weights))] <- 0

    ## Assert the identities on the object that actually leaves this function, not on the
    ## local vectors. Everything downstream reads the benchmark back out of this column,
    ## so this is the check that catches a normalization that failed to persist.
    returned_bench_total <- sum(universe_m_d_ref[[bench_weights_col]])
    if (abs(returned_bench_total - 1) > tol_check){
      stop(paste0("Returned benchmark weights must sum to 1, got ", returned_bench_total, "."))
    }
    returned_active_total <- sum(universe_m_d_ref$weights - universe_m_d_ref[[bench_weights_col]])
    if (abs(returned_active_total) > tol_check){
      stop(paste0("Returned active weights must sum to zero, got ", returned_active_total, "."))
    }

    ## Message
    if (isTRUE(verbose)) {
      cat("\n")
      cat(crayon::green("SLSAF weights successfully defined"))
      cat("\n")
      tictoc::toc()
    }

    return(list(
      universe_m_d_ref = universe_m_d_ref,
      weights          = universe_m_d_ref$weights,
      underweights     = underweights,
      short_budget     = short_budget,
      active_budget    = active_budget,
      n_long           = length(long_tickers),
      n_short          = length(short_tickers),
      n_zeroed         = n_zeroed,
      micro            = list(long = long_port, short = short_port)
    ))
}
