#' Create a Micro–Macro Allocation Framework (MMAF) portfolio
#'
#' @description
#' Implements a two-layer portfolio construction framework where the behavior is
#' controlled by `mmaf_method`:
#'
#' - **"top_down"**: Build *proxy* intra-sector portfolios first (no sector budgets),
#'   aggregate to sector-level metrics, construct sector (macro) weights, then
#'   re-optimize **inside each sector** under its sector budget (constraints scaled
#'   by the sector budget), and finally reconcile to final stock weights
#'   (sector weight × intra-sector weight).
#'
#' - **"bottom_up"**: Build a single micro-level portfolio across all stocks,
#'   aggregate the resulting weights to sector-level metrics, construct sector
#'   weights (macro), then reconcile stock weights so sector totals match macro weights.
#'   (Micro-level concentration constraints may not hold globally after reconciliation.)
#'
#' @param universe_m_d_ref A data.frame/tibble with (at least) columns:
#'   `id`, `tickers`, `dates`, `is_eligible`, `exp_ret_score`, and any optional
#'   columns used by the micro methods (e.g., benchmark/target weights, liquidity features).
#'   Should refer to a **single date**.
#' @param mmaf_method Character scalar: `"top_down"` or `"bottom_up"`.
#' @param covariance_matrix Numeric covariance matrix with row/col names that
#'   exactly match the `tickers` of eligible names (same ordering is enforced).
#' @param groups_m_d_ref A data.frame/tibble with group (sector) membership for tickers.
#'   Must include `id`, `tickers`, `dates`, and the `mmaf_group_col` column.
#' @param mmaf_group_col Character scalar naming the column in `groups_m_d_ref` that carries
#'   the group/sector classification. (Your wrapper enforces this as the 4th column.)
#' @param liquidity_m_d_ref Optional data.frame/tibble with liquidity features aligned by
#'   `id`, `tickers`, `dates`. If provided, group-level liquidity metrics are aggregated
#'   as weighted means of intra-sector micro weights.
#' @param top_down_proxy_port_method Character: micro method to build *proxy* intra-sector
#'   portfolios in the initial pass of `"top_down"` (e.g., `"rp"`, `"hrp"`, `"ew"`).
#'
#' @param micro_port_construction_method Character micro method used for the actual
#'   intra-sector optimization (e.g., `"mvo"`, `"rp"`, `"hrp"`, `"ew"`, or custom handled
#'   by `set_portfolio_weights()`).
#' @param linkage Character, passed to hierarchical methods when applicable.
#' @param liquidity_constraint_policy Optional list describing liquidity caps at micro level.
#' @param turnover_constraint_policy Optional list describing turnover caps at micro level.
#' @param concentration_constraint_policy Optional list with e.g.
#'   `max_abs_active_individual_weight` for micro level.
#' @param cap_weighting_metric Optional character for capitalization weighting metric (if used).
#' @param n_random_ports Integer, number of random portfolios (for MVO/random search).
#' @param random_ports_method Character, sampling method for random portfolios.
#' @param opt_objective Character, optimization objective (e.g., `"sharpe"`).
#' @param opt_method Character, optimizer selector for MVO/random search.
#' @param ridge_pen Numeric or `NULL`, ridge penalty for MVO (micro).
#' @param n_resamples Integer, number of resamples for robust MVO (micro).
#' @param exp_ret_score_jitter Numeric, jitter on expected return score (micro MVO).
#' @param cov_eigval_jitter Numeric, jitter on covariance eigenvalues (micro MVO).
#' @param rp_method Character, risk parity method at micro level (e.g., `"cyclical-spinu"`).
#' @param exp_ret_score_tilt Optional numeric vector/column name for RP tilt (micro).
#' @param exp_ret_score_tilt_eta Optional numeric, tilt intensity for micro RP.
#' @param macro_port_construction_method Character macro method used to allocate across groups
#'   (e.g., `"ew"`, `"rp"`, `"hrp"`, `"mvo"`). For a strictly *neutral* top-down
#'   sector allocation, prefer `"ew"`, `"rp"` or `"hrp"` and keep `macro_exp_ret_score_tilt = NULL`.
#' @param macro_linkage Character, passed to macro hierarchical methods when applicable.
#' @param macro_concentration_constraint_policy Optional list with group-level weight caps.
#' @param macro_cap_weighting_metric Optional character for cap-based macro weighting (if used).
#' @param macro_n_random_ports Integer, number of random portfolios at macro level.
#' @param macro_random_ports_method Character, sampling method (macro).
#' @param macro_opt_objective Character, optimization objective at macro.
#' @param macro_opt_method Character, optimizer selector at macro.
#' @param macro_ridge_pen Numeric or `NULL`, ridge penalty for macro MVO.
#' @param macro_n_resamples Integer, number of resamples for macro MVO.
#' @param macro_exp_ret_score_jitter Numeric, jitter on sector ER (macro MVO).
#' @param macro_cov_eigval_jitter Numeric, jitter on macro covariance eigenvalues.
#' @param macro_rp_method Character, risk parity method at macro.
#' @param macro_exp_ret_score_tilt Optional numeric vector/column name for RP tilt (macro).
#' @param macro_exp_ret_score_tilt_eta Optional numeric, tilt intensity for macro RP.
#'
#' @param lower_quantile_winsorization,upper_quantile_winsorization Numerics in (0,1),
#'   passed through to micro and macro `set_portfolio_weights()`.
#' @param parallel Logical, whether to parallelize micro-level sector runs via `furrr::future_map()`.
#' @param verbose Logical, print progress and timing via `tictoc`.
#'
#' @details
#' **Top-down** emphasizes sector allocation: sectors are first represented by proxy
#' intra-sector portfolios to estimate sector ER/liquidity and the sector-by-sector
#' covariance. After optimizing sector weights (macro), intra-sector portfolios are
#' re-optimized under sector budgets. Constraint policies (concentration/turnover/liquidity)
#' are **scaled by the sector budget** when applicable.
#'
#' **Bottom-up** emphasizes stock selection: a single micro portfolio is built across
#' all names. Sector metrics are aggregated from this solution, macro sector weights
#' are computed, and final stock weights are reconciled to match macro sector totals.
#' Micro-level concentration constraints may no longer hold exactly after reconciliation.
#'
#' The function assumes (by your upstream contract) that:
#' - `universe_m_d_ref` / `groups_m_d_ref` / `liquidity_m_d_ref` are all single-date.
#' - `covariance_matrix` row/col names match the eligible tickers (same ordering).
#' - `port@universe_m_d_ref@data` carries liquidity columns when those are used.
#' - `mmaf_group_col` is the 4th column of `groups_m_d_ref` (enforced by a wrapper).
#'
#' @return A list with:
#' \describe{
#'   \item{universe_m_d_ref}{Input universe joined with final `weights` (stock-level).}
#'   \item{group_weights}{Named numeric vector of sector weights (macro).}
#'   \item{macro}{Return object from `set_portfolio_weights()` at macro level.}
#'   \item{micro}{Top-down: list of per-sector micro return objects. Bottom-up:
#'                a single-element list `list(consolidated = <micro_object>)`.}
#'   \item{group_cov_matrix}{Sector-by-sector covariance matrix implied by micro solutions.}
#' }
#'
#' @examples
#' \dontrun{
#' # Minimal sketch (objects must be prepared consistently upstream):
#' res_td <- create_mmaf_portfolio(
#'   universe_m_d_ref = universe_df,
#'   mmaf_method = "top_down",
#'   covariance_matrix = Sigma,
#'   groups_m_d_ref = groups_df,
#'   mmaf_group_col = "sector",
#'   liquidity_m_d_ref = liq_df,
#'   top_down_proxy_port_method = "rp",
#'   micro_port_construction_method = "mvo",
#'   macro_port_construction_method = "hrp",
#'   verbose = TRUE
#' )
#'
#' res_bu <- create_mmaf_portfolio(
#'   universe_m_d_ref = universe_df,
#'   mmaf_method = "bottom_up",
#'   covariance_matrix = Sigma,
#'   groups_m_d_ref = groups_df,
#'   mmaf_group_col = "sector",
#'   liquidity_m_d_ref = liq_df,
#'   micro_port_construction_method = "mvo",
#'   macro_port_construction_method = "rp",
#'   verbose = TRUE
#' )
#'
#' # Check reconciliation by sector (example):
#' # dplyr::summarise sum of final weights by sector should match res_td$group_weights
#' }
#'
#' @export
create_mmaf_portfolio <- function(universe_m_d_ref, mmaf_method = "bottom_up",
                                  covariance_matrix, groups_m_d_ref, mmaf_group_col,
                                  liquidity_m_d_ref, top_down_proxy_port_method = "rp",
                                  # Micro Level
                                  micro_port_construction_method, linkage = "single",
                                  liquidity_constraint_policy = NULL, #Policies
                                  turnover_constraint_policy = NULL,
                                  concentration_constraint_policy = NULL,
                                  cap_weighting_metric = NULL,
                                  n_random_ports = 2000, random_ports_method = "sample",
                                  opt_objective = "sharpe", opt_method = "random", ridge_pen = NULL,
                                  n_resamples = 0, exp_ret_score_jitter = 0, cov_eigval_jitter = 0, #MVO
                                  rp_method = "cyclical-spinu", exp_ret_score_tilt = NULL, #Risk Parity
                                  exp_ret_score_tilt_eta = NULL,
                                  # Macro Level
                                  macro_port_construction_method,
                                  macro_concentration_constraint_policy = NULL,
                                  macro_cap_weighting_metric = NULL,
                                  macro_n_random_ports = 2000, macro_random_ports_method = "sample",
                                  macro_opt_objective = "sharpe", macro_opt_method = "random", macro_ridge_pen = NULL,
                                  macro_n_resamples = 0, macro_exp_ret_score_jitter = 0, macro_cov_eigval_jitter = 0, #MVO
                                  macro_rp_method = "cyclical-spinu", macro_exp_ret_score_tilt = NULL, #Risk Parity
                                  macro_exp_ret_score_tilt_eta = NULL, macro_linkage = "single",
                                  lower_quantile_winsorization = 0.025, upper_quantile_winsorization = 0.975,
                                  parallel = FALSE, verbose = TRUE
){

  # Initial Setup---------------------------------------------------------------
    ## Message
    if (isTRUE(verbose)) {
      tictoc::tic()
      cat("\n")
      cat("Deriving weights through MMAF (Macro-Micro Allocation Framework)...")
      cat("\n")
      cat("MMAF method: ", mmaf_method)
      cat("\n")
    }

    ## Eligible tickers
    eligible_universe_m_d_ref <- universe_m_d_ref %>% dplyr::filter(is_eligible == 1)
    eligible_tickers <- eligible_universe_m_d_ref %>% dplyr::pull(tickers)

      ### Defensive checks
      if (!all(rownames(covariance_matrix) == colnames(covariance_matrix) &
               rownames(covariance_matrix) == eligible_tickers)) {
        stop("Covariance matrix rownames/colnames do not match eligible tickers.")
      }
      if (mmaf_method == "bottom_up" && !is.null(concentration_constraint_policy)){
        warning("For bottom_up, micro-level concentration constraints might not hold globally.")
      }
      if (mmaf_method == "top_down" && is.null(top_down_proxy_port_method)){
        stop("For top_down, top_down_proxy_port_method must be specified.")
      }
      if (mmaf_method == "top_down" && !is.null(top_down_proxy_port_method) &&
          !top_down_proxy_port_method %in% c("ew", "rp", "hrp", "cs", "sw")){
        stop("top_down_proxy_port_method must be one of 'ew', 'rp', 'hrp', 'cs', 'sw'.")
      }
      if (!mmaf_group_col %in% names(eligible_universe_m_d_ref)){
        stop("mmaf_group_col not found in eligible_universe_m_d_ref.")
      }
      if (!mmaf_group_col %in% names(groups_m_d_ref)){
        stop("mmaf_group_col not found in groups_m_d_ref")
      }

    ## Get groups
      ### If mmaf_group_col is NULL, assign the first after id, tickers and dates as mmaf_group_col
      if (is.null(mmaf_group_col)){
        mmaf_group_col <- names(groups_m_d_ref)[4]
        message(paste0("mmaf_group_col not specified. Using ", mmaf_group_col, " as mmaf_group_col."))
      }

      ### Get unique groups
      groups <- unique(eligible_universe_m_d_ref[[mmaf_group_col]])

        ### Re-order groups alphabetically
        groups <- sort(groups)

        #### Define members
        group_members <- lapply(groups, function(g) {
          eligible_universe_m_d_ref %>%
            dplyr::filter(!!rlang::sym(mmaf_group_col) == g) %>%
            dplyr::pull(tickers)
        })
        names(group_members) <- groups

        #### If any group does not contain any eligible ticker, throw error
        empty_groups <- sapply(group_members, length) == 0
        if (any(empty_groups)) {
          stop(paste0("Some groups have no eligible tickers: ",
                       paste(groups[empty_groups], collapse = ", ")))
        }
        #### If any group is NA or '', throw error
        if (any(is.na(groups)) || any(groups == "")) {
          stop("Some groups are NA or empty strings.")
        }

        #### Display the number of members in each group
        if (isTRUE(verbose)) {
          cat("\nNumber of groups and members:\n")
          for (g in seq_along(groups)) {
            ### Use crayon to color number of members
            ### (red if less than 2, orange if less than 5, green otherwise)
            n_members <- length(group_members[[g]])
            color <- if (n_members < 2) {
              crayon::red
            } else if (n_members < 5) {
              crayon::yellow
            } else {
              crayon::green
            }
            cat(" - ", groups[g], ": ", color(n_members), " members\n", sep = "")
          }
        }

        #### Number of groups
        n_groups <- length(groups)

      ### Check if at least two groups
      if(n_groups < 2){
        stop("At least two groups are required for MMAF portfolio construction.")
      }


  # Micro-level-----------------------------------------------------------------

    ## Bottom-up
    if (mmaf_method == "bottom_up"){

      ### Message
      if (verbose){
        cat("\nApplying bottom-up micro-level portfolio method...")
        cat("\n")
        cat("Portfolio construction method: ", micro_port_construction_method)
        cat("\n")
      }

      ### Apply set_portfolio_weights to entire universe
      micro_port <- set_portfolio_weights(
        # Stock Universe object
        universe_m_d_ref = eligible_universe_m_d_ref, #To match process_micro logic
        # Intra Group Port Construction Method
        port_construction_method = micro_port_construction_method,
        linkage = linkage,
        # Liquidity Constraint Policy for stocks
        liquidity_constraint_policy = liquidity_constraint_policy,
        liquidity_m_d_ref = liquidity_m_d_ref,
        cap_weighting_metric = cap_weighting_metric,
        #Concentration Constraint Policy for stocks
        concentration_constraint_policy = concentration_constraint_policy,
        # Turnover Constraint Policy for stocks
        turnover_constraint_policy = turnover_constraint_policy,
        # Groups
        groups_m_d_ref = groups_m_d_ref,
        # Covariance Matrix
        covariance_matrix = covariance_matrix,
        # Intra-portfolio parameters
        ## Risk Parity
        rp_method = rp_method,
        exp_ret_score_tilt = exp_ret_score_tilt,
        exp_ret_score_tilt_eta = exp_ret_score_tilt_eta,
        ## MVO
        n_random_ports = n_random_ports, random_ports_method = random_ports_method,
        opt_objective = opt_objective, opt_method = opt_method, ridge_pen = ridge_pen,
        n_resamples = n_resamples, exp_ret_score_jitter = exp_ret_score_jitter,
        cov_eigval_jitter = cov_eigval_jitter,
        ## Custom Weights
        custom_weights_m_d_ref = custom_weights_m_d_ref,
        # Winsorization
        lower_quantile_winsorization = lower_quantile_winsorization,
        upper_quantile_winsorization = upper_quantile_winsorization,
        parallel = parallel
      )

      ### Break up into micro_universe_m_d_ref_list
      micro_universe_m_d_ref_list <- purrr::map(groups, function(g) {

        #### Check that all assets are eligible
        if (micro_port@universe_m_d_ref@data %>%
              dplyr::filter(is_eligible == 0) %>%
              nrow() > 0) {
          stop("Micro portfolio contains ineligible assets.")
        }

        #### Subset universe and normalize weights
          ##### If sum of weights is 0, return a vector of 0 weights
          if (sum(micro_port@universe_m_d_ref@data %>%
                  dplyr::filter(!!rlang::sym(mmaf_group_col) == g) %>%
                  dplyr::pull(weights)) < .Machine$double.eps) {
            return(
              micro_port@universe_m_d_ref@data %>%
                dplyr::filter(!!rlang::sym(mmaf_group_col) == g) %>%
                dplyr::mutate(weights = 0)
            )
          } else {
          ##### Otherwise, normalize weights to sum to 1
          micro_port@universe_m_d_ref@data %>%
            dplyr::filter(!!rlang::sym(mmaf_group_col) == g) %>%
            dplyr::mutate(weights = weights / sum(weights, na.rm = TRUE))
          }
      })
      names(micro_universe_m_d_ref_list) <- groups

    }

    ## Top-down Proxies
    if (mmaf_method == "top_down"){

      ### Message
      if (verbose){
        cat("\nBuilding top-down micro-level proxy portfolios...")
        cat("\n")
        cat("Portfolio construction method: ", top_down_proxy_port_method)
        cat("\n")
      }

      ### For each group, apply micro portfolio method
      micro_port_list <- process_micro_portfolios(
        universe_m_d_ref = eligible_universe_m_d_ref,
        parallel = parallel, groups = groups,
        group_members = group_members,
        group_weights = NULL, # No group weights at this stage
        micro_port_construction_method = top_down_proxy_port_method,
        linkage = linkage,
        rp_method = rp_method,
        cap_weighting_metric = cap_weighting_metric,
        liquidity_m_d_ref = liquidity_m_d_ref,
        covariance_matrix = covariance_matrix,
        ## In the context of top_down_proxy, we want to enforce
        ## a portfolio that is well diversified in order to get \
        ## a valid proxy for the group. This makes mvo a bit tricky
        ## because it can often lead to underdiversified portfolios,
        ## requiring contraints and penalties that must be tuned.
        ## Therefore, we force some arguments to be NULL
        liquidity_constraint_policy = NULL,
        turnover_constraint_policy = NULL,
        concentration_constraint_policy = NULL,
        n_random_ports = NULL,
        random_ports_method = NULL,
        opt_objective = NULL, opt_method = NULL, ridge_pen = NULL,
        n_resamples = NULL, exp_ret_score_jitter = NULL, cov_eigval_jitter = NULL,
        exp_ret_score_tilt = NULL, exp_ret_score_tilt_eta = NULL,
        lower_quantile_winsorization = lower_quantile_winsorization,
        upper_quantile_winsorization = upper_quantile_winsorization
      )

      ### Break up into micro_universe_m_d_ref
      micro_universe_m_d_ref_list <- purrr::map(
        micro_port_list, function(port) port@universe_m_d_ref@data
      )

    }

  # Macro Metrics --------------------------------------------------------------

    ## Compute aggregate objects
    agg_macro_objects <- compute_agg_macro_objects(
      eligible_universe_m_d_ref = eligible_universe_m_d_ref,
      covariance_matrix = covariance_matrix,
      group_col = mmaf_group_col,
      micro_universe_m_d_ref_list = micro_universe_m_d_ref_list,
      liquidity_m_d_ref = liquidity_m_d_ref
    )

    group_universe_m_d_ref   <- agg_macro_objects$group_universe_m_d_ref
    group_liquidity_m_d_ref  <- agg_macro_objects$group_liquidity_m_d_ref
    group_covariance_matrix  <- agg_macro_objects$group_covariance_matrix


  # Macro-level ----------------------------------------------------------------

    ## Set Portfolio Weights
    if (verbose){
      cat("\nApplying macro-level portfolio method...")
      cat("\n")
      cat("Portfolio construction method: ", macro_port_construction_method)
      cat("\n")
    }

    ## Set macro portfolio
    macro_port <- set_portfolio_weights(
      # Stock Universe object
      universe_m_d_ref = group_universe_m_d_ref,
      # Macro Port Construction Method
      port_construction_method = macro_port_construction_method,
      linkage = macro_linkage,
      # Liquidity Constraint Policy for groups
      liquidity_constraint_policy = NULL,
      liquidity_m_d_ref = group_liquidity_m_d_ref,
      cap_weighting_metric = macro_cap_weighting_metric,
      #Concentration Constraint Policy for groups
      concentration_constraint_policy = macro_concentration_constraint_policy,
      # Turnover Constraint Policy for groups
      turnover_constraint_policy = NULL, # Not implemented at macro level
      # Groups
      groups_m_d_ref = NULL, # Not needed at macro level
      # Covariance Matrix
      covariance_matrix = group_covariance_matrix,
      # Intra-portfolio parameters
      ## Risk Parity
      rp_method = macro_rp_method,
      exp_ret_score_tilt = macro_exp_ret_score_tilt,
      exp_ret_score_tilt_eta = macro_exp_ret_score_tilt_eta,
      ## MVO
      n_random_ports = macro_n_random_ports, random_ports_method = macro_random_ports_method,
      opt_objective = macro_opt_objective, opt_method = macro_opt_method, ridge_pen = macro_ridge_pen,
      n_resamples = macro_n_resamples, exp_ret_score_jitter = macro_exp_ret_score_jitter,
      cov_eigval_jitter = macro_cov_eigval_jitter,
      ## Custom Weights
      custom_weights_m_d_ref = NULL,
      # Winsorization
      lower_quantile_winsorization = lower_quantile_winsorization,
      upper_quantile_winsorization = upper_quantile_winsorization,
      parallel = FALSE # Macro level is small, no need for parallel
    )

    group_weights <- macro_port@universe_m_d_ref@data %>%
      dplyr::pull(weights) %>%
      setNames(macro_port@universe_m_d_ref@data$tickers)

  # Produce micro_port_list-----------------------------------------------------

    ##Overwrite intra-group allocation for top_down
    if (mmaf_method == "top_down"){

      ### For top_down method, re-optimize inside sectors

      ### Message
      if (verbose){
        cat("\nApplying top-down micro-level portfolio method...")
        cat("\n")
        cat("Portfolio construction method: ", micro_port_construction_method)
        cat("\n")
      }


      ### For each group, apply micro portfolio method
      micro_port_list <- process_micro_portfolios(
        universe_m_d_ref = eligible_universe_m_d_ref,
        parallel = parallel, groups = groups,
        group_members = group_members,
        group_weights = group_weights,
        micro_port_construction_method = micro_port_construction_method,
        linkage = linkage,
        rp_method = rp_method,
        cap_weighting_metric = cap_weighting_metric,
        liquidity_m_d_ref = liquidity_m_d_ref,
        covariance_matrix = covariance_matrix,
        liquidity_constraint_policy = liquidity_constraint_policy,
        turnover_constraint_policy = turnover_constraint_policy,
        concentration_constraint_policy = concentration_constraint_policy,
        n_random_ports = n_random_ports,
        random_ports_method = random_ports_method,
        opt_objective = opt_objective, opt_method = opt_method, ridge_pen = ridge_pen,
        n_resamples = n_resamples, exp_ret_score_jitter = exp_ret_score_jitter, cov_eigval_jitter = cov_eigval_jitter,
        exp_ret_score_tilt = exp_ret_score_tilt, exp_ret_score_tilt_eta = exp_ret_score_tilt_eta,
        lower_quantile_winsorization = lower_quantile_winsorization,
        upper_quantile_winsorization = upper_quantile_winsorization
      )

      ### Break up into micro_universe_m_d_ref
      micro_universe_m_d_ref_list <- purrr::map(
        micro_port_list, function(port){
          if (!is.null(port) && inherits(port, "port")){
            port@universe_m_d_ref@data
          } else {
            NULL #This is for group_weights == 0
          }
        })

    } else {

      micro_port_list = NULL

    }


  # Reconcile at portfolio level------------------------------------------------

    ## For each group, multiply stock_weight by group weight
    final_weights_m_d_ref <- purrr::map_dfr(groups, function(g){

      ### Current micro universe
        #### For groups with zero weight
        if (group_weights[g] < 1e-8){
          current_micro_universe_m_d_ref <- universe_m_d_ref %>%
            dplyr::filter(tickers %in% group_members[g]) %>%
            dplyr::mutate(weights = 0) %>%
            dplyr::select(id, weights)
        } else {
          current_micro_universe_m_d_ref <- micro_universe_m_d_ref_list[[g]] %>%
            dplyr::select(id, weights)
        }

     ### Multiply weights by group weight
      current_micro_universe_m_d_ref %>%
        dplyr::mutate(weights = weights * group_weights[g])

    }) %>%
      dplyr::arrange(id)

    ## Left join to universe_m_d_ref
    universe_m_d_ref <- universe_m_d_ref %>%
      dplyr::left_join(final_weights_m_d_ref, by = "id")

    ## Replace NAs with 0
    universe_m_d_ref <- universe_m_d_ref %>%
      dplyr::mutate(weights = dplyr::if_else(is.na(weights), 0, weights))


  # Return---------------------------------------------------------------------
  if (isTRUE(verbose)) {
    cat("\n")
    cat(crayon::green("MMAF weights successfully defined"))
    cat("\n")
    tictoc::toc()
  }

    ### Remove any stray names from numeric weight vectors
    if ("weights" %in% names(universe_m_d_ref)) {
      names(universe_m_d_ref$weights) <- NULL
    }

      return(list(
        universe_m_d_ref           = universe_m_d_ref,
        group_weights              = group_weights,
        macro                      = macro_port,
        micro                      = if (mmaf_method == "top_down"){
          micro_port_list
        }  else {
          list("bottom_up"         = micro_port)
        },
        group_cov_matrix           = group_covariance_matrix,
        mmaf_group_col             = mmaf_group_col,
        mmaf_method                = mmaf_method
      ))


}


#Helpers------------------------------------------------------------------------

#' @title Set Top-Down Micro Weights
#'
#' @description
#' Constructs a micro-level portfolio within a given group as part of a
#' hierarchical risk parity allocation. The function subsets the universe,
#' covariance matrix, and (optionally) liquidity data for the group, scales
#' constraints relative to group weights, and calls
#' `set_portfolio_weights()` to compute intra-group allocations.
#'
#' @param group_name Character. Name of the group being processed.
#' @param group_weights Named numeric vector of group weights. Required if
#'   any constraint or ridge penalty is defined.
#' @param micro_port_construction_method Character. Method used for portfolio
#'   construction within the group (e.g., `"mvo"`, `"risk_parity"`).
#' @param group_members List mapping group names to member tickers.
#' @param universe_m_d_ref Data frame with stock-level reference data, including
#'   tickers and optional benchmark/target weights.
#' @param covariance_matrix Numeric covariance matrix covering all tickers.
#' @param liquidity_m_d_ref (Optional) Data frame with liquidity metrics for
#'   the full universe.
#' @param concentration_constraint_policy (Optional) Policy object controlling
#'   max active weights at stock level.
#' @param turnover_constraint_policy (Optional) Policy object controlling stock
#'   or group-level turnover caps.
#' @param liquidity_constraint_policy (Optional) Policy object controlling stock
#'   or group-level liquidity caps.
#' @param cap_weighting_metric (Optional) Market cap or related metric for
#'   cap-weighted allocations.
#' @param rp_method (Optional) Risk parity method.
#' @param exp_ret_score_tilt (Optional) Expected return tilt applied under risk parity.
#' @param exp_ret_score_tilt_eta (Optional) Tilt intensity for expected return tilt.
#' @param n_random_ports (Optional) Number of random portfolios used for resampling.
#' @param random_ports_method (Optional) Sampling method for random portfolios.
#' @param opt_objective (Optional) Optimization objective (e.g., `"min_var"`, `"sharpe"`).
#' @param opt_method (Optional) Numerical optimization routine.
#' @param ridge_pen (Optional) Ridge penalty term for regularized optimization.
#' @param n_resamples (Optional) Number of resamples used in MVO.
#' @param exp_ret_score_jitter (Optional) Jitter applied to expected return scores.
#' @param cov_eigval_jitter (Optional) Jitter applied to covariance eigenvalues.
#' @param linkage (Optional) Linkage method for hierarchical clustering in risk parity.
#' @param lower_quantile_winsorization (Optional) Lower quantile cutoff.
#' @param upper_quantile_winsorization (Optional) Upper quantile cutoff.
#'
#' @return A portfolio object returned by `set_portfolio_weights()` with weights
#'   for tickers in the specified group.
#'
#' @details
#' - Scales benchmark, BOP, and target weights by the group allocation if constraints are active.
#' - Normalizes scaled weights to sum to 1 if needed.
#' - Scales concentration, turnover, and liquidity constraints relative to
#'   the group weight.
#' - Removes pre-existing weight columns if no constraints are active.
#'
#' @seealso [process_micro_portfolios()], [set_portfolio_weights()]
#'
#' @keywords portfolio optimization risk-parity
set_top_down_micro_weights <- function(group_name, group_weights = NULL,
                                       micro_port_construction_method,

                                       # Core data
                                       group_members, universe_m_d_ref,
                                       covariance_matrix, liquidity_m_d_ref = NULL,

                                       # Constraint policies
                                       concentration_constraint_policy = NULL,
                                       turnover_constraint_policy = NULL,
                                       liquidity_constraint_policy = NULL,

                                       # Portfolio parameters
                                       cap_weighting_metric = NULL,

                                       # Risk parity
                                       rp_method = "cyclical-spinu",
                                       exp_ret_score_tilt = NULL,
                                       exp_ret_score_tilt_eta = NULL,
                                       linkage = "single",

                                       # MVO
                                       n_random_ports = 2000,
                                       random_ports_method = "sample",
                                       opt_objective = "sharpe",
                                       opt_method = "random",
                                       ridge_pen = NULL,
                                       n_resamples = 0,
                                       exp_ret_score_jitter = 0,
                                       cov_eigval_jitter = 0,

                                       # Winsorization
                                       lower_quantile_winsorization = 0.025,
                                       upper_quantile_winsorization = 0.975
                                       ){

  ### Print message
  cat("\n")
  cat("Processing group:", group_name, "...\n")

  ### If group weight is 0, return NULL
  if (!is.null(group_weights) && group_weights[group_name] < .Machine$double.eps){
    cat("      Group weight is 0, skipping portfolio construction...\n")
    return(NULL)
  }

  ### If group_members list is not named, error and exit
  if (is.null(names(group_members)) || any(names(group_members) == "")){
    stop("group_members must be a named list with group names as names.")
  }

  ### Get group members
  idx <- group_members[[group_name]]
  sub_universe_m_d_ref <- universe_m_d_ref %>%
    dplyr::filter(tickers %in% idx)
  sub_tickers <- sub_universe_m_d_ref %>% dplyr::pull(tickers)

    #### Check if subtickers is empty or duplicated
    if (length(sub_tickers) == 0){
      stop(paste0("Group ", group_name, " has no eligible tickers."))
    }
    if (any(duplicated(idx))){
      stop(paste0("Group ", group_name, " has duplicated tickers."))
    }

  ### Get subcovariance
  if (!all(sub_tickers %in% rownames(covariance_matrix))){
    stop(paste0("Some tickers in group ", group_name, " are not in covariance matrix."))
  }
  sub_covariance_matrix <- covariance_matrix[sub_tickers, sub_tickers, drop = FALSE]

  ### Subset liquidity_m_d_ref
  if (is.null(liquidity_m_d_ref)){
    sub_liquidity_m_d_ref <- NULL
  } else {
    sub_liquidity_m_d_ref <- liquidity_m_d_ref %>% dplyr::filter(tickers %in% idx)
  }

  ### If they exist, scale weight columns and constraints (divide weight by group weight)
  #### Initialize
  sub_concentration_constraint_policy <- NULL
  sub_turnover_constraint_policy      <- NULL
  sub_liquidity_constraint_policy     <- NULL

  #### Check if any constraint is defined
  if (!is.null(concentration_constraint_policy) ||
      !is.null(turnover_constraint_policy) ||
      !is.null(liquidity_constraint_policy) ||
      !is.null(ridge_pen)
  ){
    needs_group_weights <- TRUE
  } else {
    needs_group_weights <- FALSE
  }

  #### If has_constraint and group_weights is NULL, throw error
  if (isTRUE(needs_group_weights) && is.null(group_weights)){
    stop("group_weights must be provided if any constraint or ridge pen are defined.")
  }

  if (isTRUE(needs_group_weights) && micro_port_construction_method == "mvo"){
    #### columns that contain 'bench_weights'
    if(ncol(sub_universe_m_d_ref %>% dplyr::select(dplyr::contains("bench_weights"))) > 0){
      weight_cols <- colnames(sub_universe_m_d_ref)[grepl("bench_weights", colnames(sub_universe_m_d_ref))]
      sub_universe_m_d_ref <- sub_universe_m_d_ref %>%
        dplyr::mutate(dplyr::across(dplyr::all_of(weight_cols), ~ .x / group_weights[group_name]))
      ##### For each weight column, if weights sum >= 1, normalize weights so that they sum 1
      for (col in weight_cols){
        if (sum(sub_universe_m_d_ref[[col]], na.rm = TRUE) >= 1){
          ###### Warn that weights are being normalized, which might indicate constraints not holding
          warning(paste0("For concentration constraint: after scaling, ", col, " in group ", group_name,
                         " sums to more than 1. Normalizing to sum to 1.",
                         "This might indicate that overall constraints do not hold because of this group."))
          ##### Normalize weights
          sub_universe_m_d_ref <- sub_universe_m_d_ref %>%
            dplyr::mutate(!!rlang::sym(col) := !!rlang::sym(col) / sum(!!rlang::sym(col), na.rm = TRUE))
        }
      }
    }

    #### columns that contain bop_port_weights
    if(ncol(sub_universe_m_d_ref %>% dplyr::select(dplyr::contains("bop_port_weights"))) > 0){
      weight_cols <- colnames(sub_universe_m_d_ref)[grepl("bop_port_weights", colnames(sub_universe_m_d_ref))]
      sub_universe_m_d_ref <- sub_universe_m_d_ref %>%
        dplyr::mutate(dplyr::across(dplyr::all_of(weight_cols), ~ .x / group_weights[group_name]))
      ##### For each weight column, if weights sum >= 1, normalize weights so that they sum 1
      for (col in weight_cols){
        if (sum(sub_universe_m_d_ref[[col]], na.rm = TRUE) >= 1){
          ###### Warn that weights are being normalized, which might indicate constraints not holding
          warning(paste0("For turnover constraint: after scaling, ", col, " in group ", group_name,
                         " sums to more than 1. Normalizing to sum to 1.",
                         "This might indicate that overall constraints do not hold because of this group."))
          ##### Normalize weights
          sub_universe_m_d_ref <- sub_universe_m_d_ref %>%
            dplyr::mutate(!!rlang::sym(col) := !!rlang::sym(col) / sum(!!rlang::sym(col), na.rm = TRUE))
        }
      }
    }

    #### target_weights column
    if(ncol(sub_universe_m_d_ref %>% dplyr::select(dplyr::contains("target_weights"))) > 0){
      weight_cols <- colnames(sub_universe_m_d_ref)[grepl("target_weights", colnames(sub_universe_m_d_ref))]
      sub_universe_m_d_ref <- sub_universe_m_d_ref %>%
        dplyr::mutate(dplyr::across(dplyr::all_of(weight_cols), ~ .x / group_weights[group_name]))
      ##### For each weight column, if weights sum >= 1, normalize weights so that they sum 1
      for (col in weight_cols){
        if (sum(sub_universe_m_d_ref[[col]], na.rm = TRUE) >= 1){
          ###### Warn that weights are being normalized, which might indicate constraints not holding
          warning(paste0("For target weights: after scaling, ", col, " in group ", group_name,
                         " sums to more than 1. Normalizing to sum to 1.",
                         "This might indicate that overall constraints do not hold because of this group."))
          ##### Normalize weights
          sub_universe_m_d_ref <- sub_universe_m_d_ref %>%
            dplyr::mutate(!!rlang::sym(col) := !!rlang::sym(col) / sum(!!rlang::sym(col), na.rm = TRUE))
        }
      }
    }

    #### Apply a defensive check. If any weight column sums more than 1, stop execution
    weight_cols <- colnames(sub_universe_m_d_ref)[grepl("weights", colnames(sub_universe_m_d_ref))]
    for (col in weight_cols){
      if (sum(sub_universe_m_d_ref[[col]], na.rm = TRUE) > 1 + .Machine$double.eps){
        stop(paste0("After scaling, ", col, " in group ", group_name, " sums to more than 1."))
      }
    }

    #### associated constraint

    ##### Scale constraints
    if (!is.null(concentration_constraint_policy) &&
        !is.null(concentration_constraint_policy$max_abs_active_individual_weight)){
      sub_concentration_constraint_policy <- concentration_constraint_policy
      ##### Scale max_abs_active_individual_weight if group_weights > .Machine$double.eps
      if (group_weights[group_name] > .Machine$double.eps){
        sub_concentration_constraint_policy$max_abs_active_individual_weight <-
          sub_concentration_constraint_policy$max_abs_active_individual_weight / group_weights[group_name]
      } else {
        sub_concentration_constraint_policy$max_abs_active_individual_weight <- NULL
      }
    }

      ##### If a group constraint is being provided, drop it and warn
      if (!is.null(concentration_constraint_policy) &&
          !is.null(concentration_constraint_policy$max_abs_active_group_weight)){
        warning(paste0("Group constraints is being ignored inside group ", group_name, "."))
        sub_concentration_constraint_policy$max_abs_active_group_weight <- NULL
      }

    if (!is.null(turnover_constraint_policy) &&
        !is.null(turnover_constraint_policy$turnover_cap_rules)){
      sub_turnover_constraint_policy <- turnover_constraint_policy
      ##### Scale each turnover_cap_rule if group_weights > .Machine$double.eps
      if (group_weights[group_name] > .Machine$double.eps){
        sub_turnover_constraint_policy$turnover_cap_rules <-
          purrr::map_dbl(sub_turnover_constraint_policy$turnover_cap_rules, function(stock_cap){
            rule <- stock_cap / group_weights[group_name]
            return(rule)
          })
      } else {
        sub_turnover_constraint_policy$turnover_cap_rules <- NULL
      }
    }
    if (!is.null(liquidity_constraint_policy) &&
        !is.null(liquidity_constraint_policy$liquidity_cap_rules)){
      sub_liquidity_constraint_policy <- liquidity_constraint_policy
      ##### Scale each liquidity_cap_rule if group_weights > .Machine$double.eps
      if (group_weights[group_name] > .Machine$double.eps){
        sub_liquidity_constraint_policy$liquidity_cap_rules <-
          purrr::map_dbl(sub_liquidity_constraint_policy$liquidity_cap_rules, function(liq_cap){
            rule <- liq_cap / group_weights[group_name]
            return(rule)
          })
      } else {
        sub_liquidity_constraint_policy$liquidity_cap_rules <- NULL
      }
    }

  } else {

    ## Defensively remove weight columns from subuniverse_m_df
    weight_cols <- colnames(sub_universe_m_d_ref)[grepl("weights", colnames(sub_universe_m_d_ref))]
    if (length(weight_cols) > 0){
      sub_universe_m_d_ref <- sub_universe_m_d_ref %>%
        dplyr::select(-dplyr::all_of(weight_cols))
    }

  }

  ### Call set_portfolio_weights

    #### # Collect warnings here (per call)
    .warnings <- character(0)

    #### Call
    port <- withCallingHandlers(
      suppressMessages(
        tryCatch(
          {
            port <- set_portfolio_weights(
              # Stock Universe object
              universe_m_d_ref = sub_universe_m_d_ref,
              # Intra Group Port Construction Method
              port_construction_method = micro_port_construction_method,
              # Liquidity Constraint Policy for stocks
              liquidity_constraint_policy = sub_liquidity_constraint_policy,
              liquidity_m_d_ref = sub_liquidity_m_d_ref,
              cap_weighting_metric = cap_weighting_metric,
              #Concentration Constraint Policy for stocks
              concentration_constraint_policy = sub_concentration_constraint_policy,
              # Turnover Constraint Policy for stocks
              turnover_constraint_policy = sub_turnover_constraint_policy,
              # Groups
              groups_m_d_ref = NULL, #Not needed inside a group
              # Covariance Matrix
              covariance_matrix = sub_covariance_matrix,
              # Intra-portfolio parameters
              ## Risk Parity
              rp_method = rp_method, exp_ret_score_tilt = exp_ret_score_tilt,
              exp_ret_score_tilt_eta = exp_ret_score_tilt_eta,
              ## HRP
              linkage = linkage,
              ## MVO
              n_random_ports = n_random_ports, random_ports_method = random_ports_method,
              opt_objective = opt_objective, opt_method = opt_method, ridge_pen = ridge_pen,
              n_resamples = n_resamples, exp_ret_score_jitter = exp_ret_score_jitter,
              cov_eigval_jitter = cov_eigval_jitter,
              ## Custom Weights
              custom_weights_m_d_ref = NULL, #Custom Weights
              # Winsorization
              lower_quantile_winsorization = lower_quantile_winsorization,
              upper_quantile_winsorization = upper_quantile_winsorization
            )
          },
          error = function(e) {
            # Flush any captured warnings before erroring
            if (length(.warnings)) {
              for (w in .warnings) warning(sprintf("[%s] %s", group_name, w), call. = FALSE, immediate. = TRUE)
            }
            stop(sprintf("Error in set_portfolio_weights for group %s: %s",
                         group_name, conditionMessage(e)),
                 call. = FALSE)
          }
        )
      ),
      warning = function(w) {
        .warnings <<- c(.warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      },
      message = function(m) {
        # Redundant with suppressMessages, but belt & suspenders:
        invokeRestart("muffleMessage")
      }
    )

    # Re-emit accumulated warnings after a successful run
    if (length(.warnings)) {
      for (w in .warnings) warning(sprintf("[%s] %s", group_name, w),
                                   call. = FALSE, immediate. = TRUE)
    }



  port

}

#' @title Process Micro Portfolios
#'
#' @description
#' Applies micro-level portfolio construction across multiple groups, either
#' sequentially or in parallel, by calling [set_top_down_micro_weights()] for
#' each group. This is the orchestration function that builds all
#' intra-group portfolios as part of the hierarchical risk parity process.
#'
#' @param parallel Logical. If `TRUE`, compute in parallel using `furrr`,
#'   otherwise sequentially with `purrr`.
#' @param groups Character vector of group names.
#' @param group_weights Named numeric vector of group weights. Required if
#'   constraints or ridge penalty are active.
#' @param micro_port_construction_method Character. Method for constructing
#'   intra-group portfolios (e.g., `"mvo"`, `"risk_parity"`).
#'
#' @inheritParams set_top_down_micro_weights
#'
#' @param verbose Logical. If `TRUE`, prints progress messages.
#'
#' @return A named list of portfolio objects, with one element per group.
#'
#' @details
#' - Subsets each group’s members, covariance matrix, and liquidity data.
#' - Calls [set_top_down_micro_weights()] with the full argument set.
#' - Supports parallel computation with reproducible seeds via `furrr`.
#'
#' @seealso [set_top_down_micro_weights()], [set_portfolio_weights()]
#'
#' @keywords portfolio optimization hierarchical risk-parity
process_micro_portfolios <- function(parallel, groups, group_weights,
                                     micro_port_construction_method,
                                     # Core data
                                     group_members,
                                     universe_m_d_ref,
                                     covariance_matrix,
                                     liquidity_m_d_ref = NULL,

                                     # Constraint policies
                                     concentration_constraint_policy = NULL,
                                     turnover_constraint_policy = NULL,
                                     liquidity_constraint_policy = NULL,

                                     # Portfolio parameters
                                     cap_weighting_metric = NULL,

                                     # Risk parity
                                     rp_method = "cyclical-spinu",
                                     exp_ret_score_tilt = NULL,
                                     exp_ret_score_tilt_eta = NULL,

                                     # HRP
                                     linkage = "single",

                                     # MVO
                                     n_random_ports = 2000,
                                     random_ports_method = "sample",
                                     opt_objective = "sharpe",
                                     opt_method = "random",
                                     ridge_pen = NULL,
                                     n_resamples = 0,
                                     exp_ret_score_jitter = 0,
                                     cov_eigval_jitter = 0,

                                     # Winsorization
                                     lower_quantile_winsorization =  0.025,
                                     upper_quantile_winsorization = 0.975,

                                     # Misc
                                     verbose = FALSE){

  ### Use furrr or purrr to iterate through groups
  if (isTRUE(parallel)) {
    if (isTRUE(verbose)) cat("\nApplying micro level portfolio method in parallel...")
    micro_port_list <- furrr::future_map(
      groups, function(group_name){
        set_top_down_micro_weights(
          group_name = group_name,
          micro_port_construction_method = micro_port_construction_method,
          group_weights = group_weights,
          # Core data
          group_members = group_members,
          universe_m_d_ref = universe_m_d_ref,
          covariance_matrix = covariance_matrix,
          liquidity_m_d_ref = liquidity_m_d_ref,

          # Constraint policies
          concentration_constraint_policy = concentration_constraint_policy,
          turnover_constraint_policy = turnover_constraint_policy,
          liquidity_constraint_policy = liquidity_constraint_policy,

          # Portfolio parameters
          cap_weighting_metric = cap_weighting_metric,

          # Risk parity
          rp_method = rp_method,
          exp_ret_score_tilt = exp_ret_score_tilt,
          exp_ret_score_tilt_eta = exp_ret_score_tilt_eta,

          # MVO
          n_random_ports = n_random_ports,
          random_ports_method = random_ports_method,
          opt_objective = opt_objective,
          opt_method = opt_method,
          ridge_pen = ridge_pen,
          n_resamples = n_resamples,
          exp_ret_score_jitter = exp_ret_score_jitter,
          cov_eigval_jitter = cov_eigval_jitter,

          # Winsorization
          lower_quantile_winsorization = lower_quantile_winsorization,
          upper_quantile_winsorization = upper_quantile_winsorization
        )
      },
      .options = furrr::furrr_options(seed = TRUE)
    )
  } else {
    if (isTRUE(verbose)) cat("\nApplying micro level portfolio method sequentially...")
    micro_port_list <- purrr::map(
      groups, function(group_name){
        set_top_down_micro_weights(
          group_name = group_name,
          micro_port_construction_method = micro_port_construction_method,
          group_weights = group_weights,
          # Core data
          group_members = group_members,
          universe_m_d_ref = universe_m_d_ref,
          covariance_matrix = covariance_matrix,
          liquidity_m_d_ref = liquidity_m_d_ref,

          # Constraint policies
          concentration_constraint_policy = concentration_constraint_policy,
          turnover_constraint_policy = turnover_constraint_policy,
          liquidity_constraint_policy = liquidity_constraint_policy,

          # Portfolio parameters
          cap_weighting_metric = cap_weighting_metric,

          # Risk parity
          rp_method = rp_method,
          exp_ret_score_tilt = exp_ret_score_tilt,
          exp_ret_score_tilt_eta = exp_ret_score_tilt_eta,

          # MVO
          n_random_ports = n_random_ports,
          random_ports_method = random_ports_method,
          opt_objective = opt_objective,
          opt_method = opt_method,
          ridge_pen = ridge_pen,
          n_resamples = n_resamples,
          exp_ret_score_jitter = exp_ret_score_jitter,
          cov_eigval_jitter = cov_eigval_jitter,

          # Winsorization
          lower_quantile_winsorization = lower_quantile_winsorization,
          upper_quantile_winsorization = upper_quantile_winsorization
        )
      }
    )
  }

  #### Rename
  names(micro_port_list) <- groups

  micro_port_list
}








