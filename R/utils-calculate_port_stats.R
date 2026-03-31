#' Compute portfolio statistics (portfolio- and group-level)
#'
#' @description
#' Computes portfolio metrics (HHI, effective N, entropy, Gini, top-k concentration,
#' diversification ratio, weighted average pairwise correlation, gross/net exposure,
#' RRC-based metrics) PLUS portfolio expected return, risk (stdev), and Sharpe (rf = 0).
#' Optionally, also computes a parallel set of metrics for a provided **group universe**
#' (e.g., sector or sleeve portfolio), with column names prefixed by \code{group_}.
#'
#' @param universe_m_d_ref A \code{data.frame} with at least columns:
#'   \code{tickers} (character), \code{is_eligible} (integer/logical),
#'   \code{weights} (numeric), and optionally \code{exp_ret_score} (numeric),
#'   \code{rel_risk_contr} (numeric).
#'   Only rows with \code{is_eligible == 1} are used for the **portfolio**.
#' @param covariance_matrix Optional covariance matrix for the eligible tickers when benchmark is not provided
#' @param all_returns_m_xts_upd_ref An optional `xts` object containing return data for all tickers, used in covariance matrix estimation for Risk-Parity and MVO methods.
#' @param cov_estimation_method Character. Covariance matrix estimation method to use.
#' @param cov_matrix_sample_size Integer. Number of time periods (rows in `all_returns_m_xts_upd_ref`) used to estimate the covariance matrix. If `NULL`, uses all available observations.
#' @param selected_benchmark Character indicating the benchmark to use
#' @param bench_universe_m_d_ref Optional \code{data.frame} in the same format as
#'   \code{universe_m_d_ref} for the **benchmark**; if provided, active weights are used
#'   (\code{w_port - w_bench}) on the union of tickers (missing side = 0).
#' @param group_universe_m_d_ref Optional \code{data.frame} in the same format but
#'   representing a **group portfolio** (e.g., pre-aggregated sector/sleeve weights).
#'   Only rows with \code{is_eligible == 1} are used. A separate metrics block is returned,
#'   prefixed with \code{group_}.
#' @param group_cov_matrix Optional covariance matrix for the **group** universe
#'   (row/column names must match \code{group} tickers provided).
#' @param groups_m_d_ref An optional data frame used for group constraints and covariance matrix estimation. Should include group information if used. Defaults to \code{NULL}.
#' @return A one-row \code{data.frame} with portfolio metrics (and group metrics if provided).
#' @rdname calculate_port_stats
calculate_port_stats <-  function(universe_m_d_ref,
                                  covariance_matrix = NULL,
                                  group_universe_m_d_ref = NULL,
                                  group_cov_matrix = NULL,
                                  selected_benchmark = NULL,
                                  bench_universe_m_d_ref = NULL,
                                  all_returns_m_xts_upd_ref = NULL,
                                  cov_estimation_method = "sample",
                                  cov_matrix_sample_size = if(is.null(all_returns_m_xts_upd_ref)) NULL else nrow(all_returns_m_xts_upd_ref),
                                  groups_m_d_ref = NULL
                                  ) {

  # Validate Inputs-------------------------------------------------------------

    ## No NAs anywhere (no cols in df_use)
    if (any(is.na(universe_m_d_ref$weights))) {
      stop("NA values found in portfolio weights.")
    }
    if (!is.null(bench_universe_m_d_ref) && any(is.na(bench_universe_m_d_ref$weights))) {
      stop("NA values found in bench weights.")
    }
    ## Expected Return Score
    if (!is.null(universe_m_d_ref$exp_ret_score) && any(is.na(universe_m_d_ref$exp_ret_score))) {
      stop("NA values found in exp ret scores.")
    }
    if (!is.null(bench_universe_m_d_ref) && !is.null(bench_universe_m_d_ref$exp_ret_score) &&
        any(is.na(bench_universe_m_d_ref$exp_ret_score))) {
      stop("NA values found in bench exp ret scores.")
    }
    ## RRC
    if (!is.null(universe_m_d_ref$rel_risk_contr) && any(is.na(universe_m_d_ref$rel_risk_contr))) {
      stop("NA values found in portfolio rel_risk_contr.")
    }
    if (!is.null(bench_universe_m_d_ref) && !is.null(bench_universe_m_d_ref$rel_risk_contr) &&
        any(is.na(bench_universe_m_d_ref$rel_risk_contr))) {
      stop("NA values found in bench rel_risk_contr.")
    }
    ## Both selected_benchmark and bench_universe_m_d_ref must be provided together
    if (is.null(selected_benchmark) != is.null(bench_universe_m_d_ref)){
      stop("Both selected_benchmark and bench_universe_m_d_ref must be provided together.")
    }
  ## Covariance matrix
  if (!is.null(all_returns_m_xts_upd_ref)){

    ### All eligible tickers must be present in all_returns_m_xts_upd_ref
    eligible_tickers <- universe_m_d_ref %>%
      dplyr::filter(is_eligible == 1) %>%
      dplyr::pull(tickers)

    if (!all(base::make.names(eligible_tickers, FALSE) %in%
             base::make.names(base::colnames(all_returns_m_xts_upd_ref), FALSE))) {
      stop("Row/column names of all_returns_m_xts_upd_ref must match eligible tickers in portfolio.")
    }
  }
  if (!is.null(covariance_matrix)){
    ### Row/column names of covariance_matrix must match eligible tickers
    eligible_tickers <- universe_m_d_ref %>%
      dplyr::filter(is_eligible == 1) %>%
      dplyr::pull(tickers)
    if (!identical(eligible_tickers, rownames(covariance_matrix))) {
      stop("Row/column names of covariance_matrix must match eligible tickers in portfolio.")
    }
    ### If a benchmark is being provided, covariance_matrix should be NULL
    if (!is.null(bench_universe_m_d_ref)){
      stop("When a benchmark is provided, covariance_matrix must be NULL.")
    }
  }
  ## Bench covariance matrix
  if (!is.null(bench_universe_m_d_ref) && !is.null(all_returns_m_xts_upd_ref)){
    ### All eligible tickers must be present in all_returns_m_xts_upd_ref
    bench_eligible_tickers <- bench_universe_m_d_ref %>%
      dplyr::filter(weights > 0) %>%
      dplyr::pull(tickers)

    if (!all(base::make.names(bench_eligible_tickers, FALSE) %in%
             base::make.names(base::colnames(all_returns_m_xts_upd_ref), FALSE))) {
      stop("Row/column names of all_returns_m_xts_upd_ref must match eligible tickers in benchmark.")
    }

    ### Weights should sum to 1
    if (abs(sum(bench_universe_m_d_ref$weights) - 1) > 0.05){
      stop("Weights in bench_universe_m_d_ref should sum to 1.")
    }
  }
  ## Group
  if (!is.null(group_universe_m_d_ref)){
    if (any(is.na(group_universe_m_d_ref$weights))) {
      stop("NA values found in group weights.")
    }
    if (!is.null(group_universe_m_d_ref$exp_ret_score) && any(is.na(group_universe_m_d_ref$exp_ret_score))) {
      stop("NA values found in group exp_ret_score.")
    }
    if (!is.null(group_universe_m_d_ref$rel_risk_contr) && any(is.na(group_universe_m_d_ref$rel_risk_contr))) {
      stop("NA values found in group rel_risk_contr.")
    }
    if (!is.null(group_cov_matrix)){
      if (!identical(group_universe_m_d_ref$tickers, rownames(group_cov_matrix))) {
        stop("Row/column names of group_cov_matrix must match eligible tickers in group_universe_m_d_ref.")
      }
    }
    ### If selected_benchmark is provided, selected_benchmark_bench_weights should be present
    if (!is.null(selected_benchmark)){
      benchmark_col <- paste0(selected_benchmark, "_bench_weights")
      if (!(benchmark_col %in% colnames(group_universe_m_d_ref))){
        stop(paste0("When selected_benchmark is provided, group_universe_m_d_ref must contain a column named ",
                    benchmark_col))
      }
    }
  }

  # Create data.frames for use--------------------------------------------------

    ## Extract weights, exp_ret_score and rrc
    df_use <- universe_m_d_ref %>%
      dplyr::select(dplyr::any_of(c("id", "tickers", "is_eligible", "weights",
                                    "exp_ret_score")))

    ## Weights should sum to 1
    if (abs(sum(df_use$weights) - 1) > 0.05){
      stop("Weights in universe_m_d_ref should sum to 1.")
    }

    ## Join Portfolio + Benchmark
    if (!is.null(bench_universe_m_d_ref)) {
      ### Check that, for benchmark, is_eligible matches weights > 0 stocks
      if (any(bench_universe_m_d_ref %>% dplyr::filter(is_eligible == 1) %>%
              dplyr::pull(weights) <= 0)){
        stop("In bench_universe_m_d_ref, is_eligible must match weights > 0 stocks.")
      }
      if (any(bench_universe_m_d_ref %>% dplyr::filter(weights > 0) %>%
              dplyr::pull(is_eligible) <= 0)){
        stop("In bench_universe_m_d_ref, is_eligible must match weights > 0 stocks.")
      }
      ### Check that all tickers in benchmark universe are in port universe and vice-versa
      missing_in_port <- setdiff(bench_universe_m_d_ref$tickers,
                                 universe_m_d_ref$tickers)
      if (length(missing_in_port) > 0){
        stop(paste0("The following tickers are in bench_universe_m_d_ref but missing in universe_m_d_ref: ",
                    paste(missing_in_port, collapse = ", ")))
      }
      missing_in_bench <- setdiff(universe_m_d_ref$tickers,
                                  bench_universe_m_d_ref$tickers)
      if (length(missing_in_bench) > 0){
        stop(paste0("The following tickers are in universe_m_d_ref but missing in bench_universe_m_d_ref: ",
                    paste(missing_in_bench, collapse = ", ")))
      }

      ### Join benchmark weights
      df_use <- df_use %>%
        dplyr::left_join(
          bench_universe_m_d_ref %>% dplyr::select(id, weights) %>%
            dplyr::rename(bench_w = weights),
          by = "id"
        )
       ### Check that no NA in bench_w
       if (any(is.na(df_use$bench_w))){
         stop("After joining benchmark weights, some tickers have NA bench_w.")
       }
    }

    ## Set eligibles and w_use
    if (!is.null(bench_universe_m_d_ref)){
      ### Filter only is_eligible == 1 OR bench_w > 0
      df_use <- df_use %>%
        dplyr::filter(is_eligible == 1 | bench_w > 0)
      ### Active weights
      df_use <- df_use %>%
        dplyr::mutate(w_use = weights - bench_w)
    } else {
      ### Filter only is_eligible == 1
      df_use <- df_use %>%
        dplyr::filter(is_eligible == 1)
      df_use <- df_use %>%
        dplyr::mutate(w_use = weights)
    }

    ## Extract
      ### Tickers to use
      tickers_use <- df_use %>% dplyr::pull(tickers)

      ### Weights to use
      w_use <- df_use %>% dplyr::pull(w_use)
      names(w_use) <- tickers_use

      ### Expected Return Score to use
      if (!is.null(df_use$exp_ret_score)){
        exp_ret_score_use <- df_use %>%
          dplyr::pull(exp_ret_score)
        names(exp_ret_score_use) <- tickers_use
      } else {
        exp_ret_score_use <- NULL
      }

    ## Group Weights
    if (!is.null(group_universe_m_d_ref)){
      ### Check for groups that all are eligible
      if ((group_universe_m_d_ref %>% dplyr::filter(is_eligible == 0) %>% nrow()) > 0){
        stop("In group_universe_m_d_ref, all groups must be eligible.")
      }
      df_group_use <- group_universe_m_d_ref

      ### Set w_use
      if (!is.null(bench_universe_m_d_ref)){
        ### Active weights
        df_group_use <- df_group_use %>%
          dplyr::mutate(w_use = weights - !!rlang::sym(paste0(selected_benchmark, "_bench_weights")))
      } else {
        df_group_use <- df_group_use %>%
          dplyr::mutate(w_use = weights)
      }

      ### Extract
        #### Tickers
        group_tickers_use <- df_group_use %>% dplyr::pull(tickers)

        #### Weights
        group_w_use <- df_group_use %>%
          dplyr::pull(w_use)
        names(group_w_use) <- group_tickers_use

        ### Extract group_exp_ret_score
        if (!is.null(df_group_use$exp_ret_score)){
          group_exp_ret_score_use <- df_group_use %>%
            dplyr::pull(exp_ret_score)
          names(group_exp_ret_score_use) <- group_tickers_use
        } else {
          group_exp_ret_score_use <- NULL
        }
    }

  # Subset covariance matrix----------------------------------------------------

      ### Covariance / correlation_matrix
      if (!is.null(all_returns_m_xts_upd_ref) && is.null(covariance_matrix)){

        #### Run estimation function
        cov_matrix_use <- estimate_covariance_matrix(
          tickers = tickers_use, #Eligible universe
          returns_m_xts_upd_ref = all_returns_m_xts_upd_ref, #Return sample
          cov_matrix_sample_size = cov_matrix_sample_size,
          cov_estimation_method = cov_estimation_method,
          active_returns = FALSE, #This avoids double counting for benchmark
          groups_m_d_ref = groups_m_d_ref #Groups for correcting NAs
        )
        cor_matrix_use <- stats::cov2cor(cov_matrix_use)
      } else if (!is.null(covariance_matrix)){
        #### Use provided covariance matrix
        cov_matrix_use <- covariance_matrix[tickers_use, tickers_use, drop = FALSE]
        cor_matrix_use <- stats::cov2cor(cov_matrix_use)
      } else {
        cov_matrix_use <- NULL
        cor_matrix_use <- NULL
      }

      ### Compute RRC if a cov_matrix_use exists
      if (!is.null(cov_matrix_use)){
        #### Compute RRC
        rrc <- relative_risk_contribution(
          weights = w_use,
          covariance_matrix = cov_matrix_use
        )
        rrc_use <- rrc$rel_risk_contr
        names(rrc_use) <- rrc$tickers
      } else {
        rrc_use <- NULL
      }

      if (!is.null(group_cov_matrix)){

        #### Use provided group covariance matrix
        group_cov_matrix_use <- group_cov_matrix
        group_cor_matrix_use <-  stats::cov2cor(group_cov_matrix)

        #### Compute RRC
        group_rrc <- relative_risk_contribution(
          weights = group_w_use,
          covariance_matrix = group_cov_matrix_use
        )
        group_rrc_use <- group_rrc$rel_risk_contr
        names(group_rrc_use) <- group_rrc$tickers
      } else {
        group_cov_matrix_use <- NULL
        group_cor_matrix_use <- NULL
        group_rrc_use <- NULL

      }

  # Compute Port Stats----------------------------------------------------------

      ### Portfolio universe
      port_stats <- calculate_port_stats_internal(
        w = w_use,
        covariance_matrix = cov_matrix_use,
        correlation_matrix = cor_matrix_use,
        exp_ret_score = exp_ret_score_use,
        rrc = rrc_use
      )
        #### If benchmark is passed, add a act_ prefix
        if (!is.null(bench_universe_m_d_ref)){
          port_stats <- port_stats %>%
            dplyr::rename_with(.cols = dplyr::everything(),
                               .fn = ~ paste0("act_", .x))
          #### For 'act_sharp', rename with info_ratio
          port_stats <- port_stats %>%
            dplyr::rename(info_ratio = act_sharpe)
        }

      final_stats <- port_stats

      ### Groups Universe (if exists)
      if (!is.null(group_universe_m_d_ref)){
        group_stats <- calculate_port_stats_internal(
          w = group_w_use,
          covariance_matrix = group_cov_matrix_use,
          correlation_matrix = group_cor_matrix_use,
          exp_ret_score = group_exp_ret_score_use,
          rrc = group_rrc_use
        )
        #### Rename to add group_prefix to metrics
        group_stats <- group_stats %>%
          ##### Remove top_10 and top_25 concentration
          dplyr::select(-c("top_5_concentration", "top_10_concentration",
                           "top_25_concentration")) %>%
          ##### Add group concentration metrics (top1, top3, top5)
          dplyr::mutate(
            top_1_concentration = top_k_concentration(group_w_use, k = 1L),
            top_3_concentration = top_k_concentration(group_w_use, k = 3L),
            top_5_concentration = top_k_concentration(group_w_use, k = 5L)
          ) %>%
          ##### Add group prefix to all names
          dplyr::rename_with(.cols = dplyr::everything(),
                             .fn = ~ paste0("group_", .x)) %>%
          ##### Add number of groups
          dplyr::mutate(n_groups = length(group_w_use))

      #### If benchmark is passed, add a act_ prefix
      if (!is.null(bench_universe_m_d_ref)){
        group_stats <- group_stats %>%
          dplyr::rename_with(.cols = dplyr::everything(),
                             .fn = ~ paste0("act_", .x))
        #### For 'act_sharp', rename with info_ratio
        group_stats <- group_stats %>%
          dplyr::rename(group_info_ratio = act_group_sharpe,
                        n_groups = act_n_groups)
      }

      #### Combine and return
      final_stats <- cbind(final_stats, group_stats)

      }

  # Return----------------------------------------------------------------------
      ### Deliver w_use, covariance and final_stats
      if (nrow(df_use) == 0L){
        assets_stats <- data.frame()
      } else {
        assets_stats <- data.frame(
          tickers = tickers_use,
          weights = w_use,
          rel_risk_contr = if (!is.null(rrc_use)) rrc_use else NA_real_,
          stringsAsFactors = FALSE
        )
      }

      return(list(
        port_stats = final_stats,
        assets_stats = assets_stats,
        covariance_matrix = cov_matrix_use
      ))

}



calculate_port_stats_internal <- function(w,
                                          covariance_matrix = NULL,
                                          correlation_matrix = NULL,
                                          exp_ret_score = NULL,
                                          rrc = NULL) {

    ## Check if names of w, covariance, correltion, exp_ret_score and rrc are all alligned
    if (!is.null(covariance_matrix)) {
      if (!base::identical(base::make.names(names(w), FALSE),
                           base::make.names(base::rownames(covariance_matrix), FALSE))) {
        stop("Names of weights must match row/column names of covariance_matrix.")
      }
    }

    if (!is.null(correlation_matrix)) {
      if (!base::identical(base::make.names(names(w), FALSE),
                           base::make.names(base::rownames(correlation_matrix), FALSE))) {
        stop("Names of weights must match row/column names of correlation_matrix.")
      }
    }
    if (!is.null(exp_ret_score)) {
      if (!identical(names(w), names(exp_ret_score))) {
        stop("Names of weights must match names of exp_ret_score.")
      }
    }
    if (!is.null(rrc)) {
      if (!base::identical(
        base::make.names(base::names(w),   FALSE),
        base::make.names(base::names(rrc), FALSE)
      )) {
        stop("Names of weights must match names of rrc.")
      }
    }

    ## Select weights: regular or active (weights - benchmark)
    n <- length(w)

    ## Portfolio exp. return, risk, Sharpe (rf = 0)
    port_exp_ret <- if (!is.null(exp_ret_score) && any(w > 0)) sum(w * exp_ret_score) else NA_real_
    port_var     <- if (!is.null(covariance_matrix)) as.numeric(t(w) %*% covariance_matrix %*% w) else NA_real_
    port_risk    <- if (!is.na(port_var)) sqrt(port_var) else NA_real_
    port_sharpe  <- if (!is.na(port_exp_ret) && !is.na(port_risk) && !isTRUE(all.equal(port_risk, 0))) {
      port_exp_ret / port_risk
    } else {
      NA_real_
    }

    ## Metrics via helpers
    hhi_w   <- hhi_weights(w)
    n_eff_w <- effective_n_bets(w)
    ent_w   <- entropy_weights(w)
    ent_ne  <- entropy_effective_n(w)
    gini_w_ <- gini_weights(w)
    top5    <- top_k_concentration(w, k = 5L)
    top10   <- top_k_concentration(w, k = 10L)
    top25   <- top_k_concentration(w, k = 25L)
    dratio  <- if (!is.null(covariance_matrix)) diversification_ratio(w, covariance_matrix) else NA_real_
    wapc    <- if (!is.null(correlation_matrix)) weighted_avg_pairwise_corr(w, correlation_matrix) else NA_real_
    ge      <- gross_exposure(w)
    ne      <- net_exposure(w)

    if (is.null(rrc) || all(is.na(rrc))) {
      hhi_r    <- NA_real_
      n_eff_r  <- NA_real_
      dist_erc <- NA_real_
    } else {
      hhi_r    <- hhi_rrc(rrc)
      n_eff_r  <- effective_n_rrc(rrc)
      dist_erc <- rrc_distance_to_erc(rrc)
    }

    ## Output
    data.frame(
      # Portfolio-level
      exp_ret               = port_exp_ret,
      risk                  = port_risk,
      sharpe                = port_sharpe,
      # Weight-based
      hhi_weights           = hhi_w,
      n_eff_weights         = n_eff_w,
      entropy_weights       = ent_w,
      entropy_effective_n   = ent_ne,
      gini_weights          = gini_w_,
      top_5_concentration   = top5,
      top_10_concentration  = top10,
      top_25_concentration  = top25,
      diversification_ratio = dratio,
      wavg_pairwise_corr    = wapc,
      gross_exposure        = ge,
      net_exposure          = ne,
      # RRC
      hhi_rrc               = hhi_r,
      n_eff_rrc             = n_eff_r,
      rrc_dist_to_erc       = dist_erc,
      stringsAsFactors = FALSE
    )

  }



#Helpers-----------------------------------------------
# Helpers to calculate metrics
hhi_weights <- function(w){
  ww <- abs(w)
  s  <- sum(ww)
  if (isTRUE(all.equal(s, 0))) return(NA_real_)
  sum((ww / s)^2)
}
hhi_rrc     <- function(rrc) sum(rrc^2)
effective_n_bets <- function(w) {
  h <- hhi_weights(w)
  if (isTRUE(all.equal(h, 0))) NA_real_ else 1 / h
}

effective_n_rrc  <- function(rrc) {
  h <- hhi_rrc(rrc)
  if (isTRUE(all.equal(h, 0))) NA_real_ else 1 / h
}

entropy_weights  <- function(w){
  ww <- abs(w)
  s  <- sum(ww)
  if (isTRUE(all.equal(s, 0))) return(NA_real_)   # no exposure

  p <- ww / s                 # normalize to sum 1
  p <- p[p > 0]               # 0*log(0) := 0, drop zeros for stability
  -sum(p * log(p))
  }

entropy_effective_n <- function(w){
  H <- entropy_weights(w)
  if (is.na(H)) NA_real_ else exp(H)
}

gini_weights <- function(w){
  if (any(w < 0)) {
    ww <- abs(w)
    s  <- sum(ww)
    if (isTRUE(all.equal(s, 0))) return(NA_real_)
    w <- ww / s                 # exposure Gini for long+short
  } else {
    s <- sum(w)
    if (isTRUE(all.equal(s, 0))) return(NA_real_)
    w <- w / s                  # long-only case: ensure sum=1
  }
  w <- w[w > 0]
  n <- length(w)
  if (n <= 1L) return(0)

  w <- sort(w)
  2 * sum(w * seq_len(n)) / n - (n + 1) / n
}


top_k_concentration <- function(w, k = 10L) {
  if (length(w) == 0L) return(NA_real_)
  ww <- abs(w)
  s  <- sum(ww)
  if (isTRUE(all.equal(s, 0))) return(NA_real_)
  ww <- sort(ww, decreasing = TRUE)
  sum(ww[seq_len(min(k, length(ww)))]) / s  # fraction of gross exposure
}

diversification_ratio <- function(w, covmat){
  sig_i <- sqrt(diag(covmat))
  top <- sum(abs(w) * sig_i)                              # gross exposure
  bot <- sqrt(as.numeric(t(w) %*% covmat %*% w))          # actual variance of w
  if (isTRUE(all.equal(bot, 0))) NA_real_ else top / bot
}

weighted_avg_pairwise_corr <- function(w, corr, tol = 1e-12){
  n <- length(w)
  if (is.null(corr) || n <= 1) return(NA_real_)

  #align by names if available
  if (!is.null(names(w)) && !is.null(rownames(corr)) && !is.null(colnames(corr))) {
    common <- intersect(names(w), rownames(corr))
    common <- intersect(common, colnames(corr))
    if (length(common) < 2L) return(NA_real_)
    w    <- w[common]
    corr <- corr[common, common, drop = FALSE]
    n    <- length(w)
  } else {
    # Fallback: basic shape checks
    if (!is.matrix(corr) || any(dim(corr) != n)) return(NA_real_)
  }

  # Drop zero-weight names entirely (treat them as non-participants)
  keep <- which(abs(w) > tol)
  if (length(keep) < 2L) return(NA_real_)  # no pair to average
  w    <- w[keep]
  corr <- corr[keep, keep, drop = FALSE]

  # Compute weighted average over positive-weight pairs
  idx <- which(upper.tri(corr), arr.ind = TRUE)
  if (nrow(idx) == 0) return(NA_real_)
  wpair <- abs(w[idx[,1]] * w[idx[,2]])   # exposure weights
  cvals <- corr[idx]

  # Handle NA correlations robustly
  ok    <- !is.na(cvals) & (wpair > 0)
  if (!any(ok)) return(NA_real_)

  num <- sum(wpair[ok] * cvals[ok])
  den <- sum(wpair[ok])

  if (isTRUE(all.equal(den, 0))) NA_real_ else num / den
}
rrc_distance_to_erc <- function(rrc){
  n <- length(rrc)
  if (n == 0) return(NA_real_)
  erc <- rep(1/n, n)
  sqrt(sum((rrc - erc)^2))
}

gross_exposure <- function(w) sum(abs(w))
net_exposure <- function(w) sum(w)

