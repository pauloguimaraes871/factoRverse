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
#'   Should refer to a **single date** (the \dQuote{d\_ref} convention).
#' @param mmaf_method Character scalar: `"top_down"` or `"bottom_up"`.
#' @param covariance_matrix Numeric covariance matrix with row/col names that
#'   exactly match the `tickers` of eligible names (same ordering is enforced).
#' @param groups_m_d_ref A data.frame/tibble with group (sector) membership for tickers.
#'   Must include `id`, `tickers`, `dates`, and the `group_col` column.
#' @param group_col Character scalar naming the column in `groups_m_d_ref` that carries
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
#' @param custom_weights_m_d_ref Optional custom weights descriptor for micro.
#'
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
#' @param macro_custom_weights_m_d_ref Optional custom weights descriptor for macro.
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
#' - `universe_m_d_ref` / `groups_m_d_ref` / `liquidity_m_d_ref` are all single-date (\dQuote{d\_ref}).
#' - `covariance_matrix` row/col names match the eligible tickers (same ordering).
#' - `port@universe_m_d_ref@data` carries liquidity columns when those are used.
#' - `group_col` is the 4th column of `groups_m_d_ref` (enforced by a wrapper).
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
#'   group_col = "sector",
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
#'   group_col = "sector",
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
                                  covariance_matrix, groups_m_d_ref, group_col,
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
                                  custom_weights_m_d_ref = NULL, #Custom Weights
                                  # Macro Level
                                  macro_port_construction_method, macro_linkage = "single",
                                  macro_concentration_constraint_policy = NULL,
                                  macro_cap_weighting_metric = NULL,
                                  macro_n_random_ports = 2000, macro_random_ports_method = "sample",
                                  macro_opt_objective = "sharpe", macro_opt_method = "random", macro_ridge_pen = NULL,
                                  macro_n_resamples = 0, macro_exp_ret_score_jitter = 0, macro_cov_eigval_jitter = 0, #MVO
                                  macro_rp_method = "cyclical-spinu", macro_exp_ret_score_tilt = NULL, #Risk Parity
                                  macro_custom_weights_m_d_ref = NULL, #Custom Weights
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

      ### Defensively check if covariance is ordered according to eligible tickers
      if (!all(rownames(covariance_matrix) == colnames(covariance_matrix) &
               rownames(covariance_matrix) == eligible_tickers)) {
        stop("Covariance matrix rownames/colnames do not match eligible tickers.")
      }

      ### Other checks to be passed to check_port_inputs later
      if (mmaf_method == "bottom_up" && !is.null(concentration_constraint_policy)){
        warning("For bottom_up, micro-level concentration constraints might not hold globally.")
      }

    ## Get groups
      ### If group_col is NULL, assign the first after id, tickers and dates as group_col
      if (is.null(group_col)){
        group_col <- names(groups_m_d_ref)[4]
        message(paste0("group_col not specified. Using ", group_col, " as group_col."))
      }

      ### Get unique groups
      groups <- unique(groups_m_d_ref[[group_col]])
        #### Define members
        group_members <- lapply(groups, function(g) {
          eligible_universe_m_d_ref %>%
            dplyr::filter(!!rlang::sym(group_col) == g) %>%
            dplyr::pull(tickers)
        })
        names(group_members) <- groups

        #### If any group does not contain any eligible ticker, remove it
        empty_groups <- sapply(group_members, length) == 0
        if (any(empty_groups)) {
          warning(paste0("Some groups have no eligible tickers and will be removed: ",
                         paste(groups[empty_groups], collapse = ", ")))

          groups <- groups[!empty_groups]
          group_members <- group_members[!empty_groups]
        }

        #### Number of groups
        n_groups <- length(groups)

      ### Check if at least two groups
      if(n_groups < 2){
        stop("At least two groups are required for MMAF portfolio construction.")
      }

    ## Helpers
    set_top_down_micro_weights <- function(group_name, group_weights = NULL,
                                           micro_port_construction_method){

      ### Get group members
      idx <- group_members[[group_name]]
      sub_universe_m_d_ref <- universe_m_d_ref %>%
        dplyr::filter(tickers %in% idx)

      ### Get subcovariance
      sub_covariance_matrix <- covariance_matrix[idx, idx, drop = FALSE]

      ### Subset liquidity_m_df
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
            sub_universe_m_d_ref <- sub_universe_m_d_ref %>%
              dplyr::mutate(!!rlang::sym(col) := !!rlang::sym(col) / sum(!!rlang::sym(col), na.rm = TRUE))
          }
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
      if (!is.null(turnover_constraint_policy) &&
          !is.null(turnover_constraint_policy$turnover_cap_rules)){
          sub_turnover_constraint_policy <- turnover_constraint_policy
          ##### Scale each turnover_cap_rule if group_weights > .Machine$double.eps
          if (group_weights[group_name] > .Machine$double.eps){
          sub_turnover_constraint_policy$turnover_cap_rules <-
            lapply(sub_turnover_constraint_policy$turnover_cap_rules, function(stock_cap){
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
            lapply(sub_liquidity_constraint_policy$liquidity_cap_rules, function(liq_cap){
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

      port

    }

    ## Process subportfolios
    process_micro_portfolios <- function(parallel, groups, group_weights,
                                         micro_port_construction_method){

      ### Use furrr or purrr to iterate through groups
      if (isTRUE(parallel)) {
        if (isTRUE(verbose)) cat("\n  Applying micro level portfolio method in parallel...")
        micro_port_list <- furrr::future_map(
          groups, function(group_name){
            set_top_down_micro_weights(
              group_name = group_name,
              micro_port_construction_method = micro_port_construction_method,
              group_weights = group_weights
            )
          },
          .options = furrr::furrr_options(seed = TRUE)
        )
      } else {
        if (isTRUE(verbose)) cat("\n  Applying micro level portfolio method sequentially...")
        micro_port_list <- purrr::map(
          groups, function(group_name){
            set_top_down_micro_weights(
              group_name = group_name,
              micro_port_construction_method = micro_port_construction_method,
              group_weights = group_weights
            )
          }
        )
      }

      #### Rename
      names(micro_port_list) <- groups

      micro_port_list
    }


  # Micro-level-----------------------------------------------------------------

    ## Bottom-up
    if (mmaf_method == "bottom_up"){

      ### Message
      if (verbose){
        cat("\n  Applying bottom-up micro-level portfolio method...")
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
        rp_method = rp_method, exp_ret_score_tilt = exp_ret_score_tilt,
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
        #### Subset universe and normalize weights
          ##### If sum of weights is 0, return a vector of 0 weights
          if (sum(micro_port@universe_m_d_ref@data %>%
                  dplyr::filter(!!rlang::sym(group_col) == g) %>%
                  dplyr::pull(weights)) < .Machine$double.eps) {
            return(
              micro_port@universe_m_d_ref@data %>%
                dplyr::filter(!!rlang::sym(group_col) == g) %>%
                dplyr::mutate(weights = 0)
            )
          } else {
          ##### Otherwise, normalize weights to sum to 1
          micro_port@universe_m_d_ref@data %>%
            dplyr::filter(!!rlang::sym(group_col) == g) %>%
            dplyr::mutate(weights = weights / sum(weights, na.rm = TRUE))
          }
      })
      names(micro_universe_m_d_ref_list) <- groups

    }

    ## Top-down Proxies
    if (mmaf_method == "top_down"){

      ### Message
      if (verbose){
        cat("\n  Building top-down micro-level proxy portfolios...")
        cat("\n")
        cat("Portfolio construction method: ", top_down_proxy_port_method)
        cat("\n")
      }

      ### For each group, apply micro portfolio method
      micro_port_list <- process_micro_portfolios(
        parallel = parallel, groups = groups,
        group_weights = NULL, # No group weights at this stage
        micro_port_construction_method = top_down_proxy_port_method
      )

      ### Break up into micro_universe_m_d_ref
      micro_universe_m_d_ref_list <- purrr::map(
        micro_port_list, function(port) port@universe_m_d_ref@data
      )

    }

  # Macro Metrics --------------------------------------------------------------

  ## group_universe_m_d_ref
  group_universe_m_d_ref <- purrr::map_dfr(groups, function(g){

    ### Current universe_m_d_ref
    current_micro_universe_m_d_ref <- micro_universe_m_d_ref_list[[g]]
    current_date <- unique(current_micro_universe_m_d_ref$dates)

    ### Calculate weighted average of exp_ret_score
    group_exp_ret_score <- stats::weighted.mean(
      current_micro_universe_m_d_ref$exp_ret_score,
      current_micro_universe_m_d_ref$weights,
      na.rm = TRUE
    )

    ### Calculate weighted average of liquidity_m_df colnames and liquidity_m_df
    group_liq_cols <- list()
    if (!is.null(liquidity_m_df)){
      liquidity_colnames <- names(liquidity_m_df[,-c(1:3)]) #Exclude id, tickers, dates

      if (length(liquidity_colnames) > 0){
        for (liq_col in liquidity_colnames){
          group_liq_cols[[liq_col]] <- stats::weighted.mean(
            current_micro_universe_m_d_ref[[liq_col]],
            current_micro_universe_m_d_ref$weights,
            na.rm = TRUE
          )
        }
      }
    }

    ### Data frame
    data.frame(
      id = paste0(g, "-", current_date),
      tickers = g,
      dates = current_date,
      exp_ret_score = group_exp_ret_score,
      is_eligible = 1
    ) %>%
      #### Add group liquidity columns if any
      dplyr::bind_cols(as.data.frame(group_liq_cols)) %>%
      #### Relocate exp_ret_score and is_eligible to end
      dplyr::relocate(exp_ret_score, .after = dplyr::last_col()) %>%
      dplyr::relocate(is_eligible, .after = dplyr::last_col())

  })

  ## Construct group_liquidity_m_df if liquidity_m_df exists
  group_liquidity_m_df <- NULL
  if (!is.null(liquidity_m_df)){
    group_liquidity_m_df <- group_universe_m_d_ref %>%
      dplyr::select(dplyr::all_of(names(liquidity_m_df)))
  }

  ## Compute sector-by-sector covariance
  group_covariance_matrix <- matrix(NA_real_, n_groups, n_groups,
                                    dimnames = list(groups, groups))

  ## Fill group covariance matrix
  for (g1 in seq_len(n_groups)){

    ## Get group members for first group
    group_g1 <- groups[g1]
    group_g1_tickers <- eligible_universe_m_d_ref %>%
      dplyr::filter(!is.na(!!rlang::sym(group_col)) &
                      !!rlang::sym(group_col) == group_g1) %>%
      dplyr::pull(tickers)

    ## Get weights for group g1
    w_g1_df <- micro_universe_m_d_ref_list[[group_g1]] %>%
      dplyr::filter(tickers %in% group_g1_tickers) %>%
      dplyr::slice(match(group_g1_tickers, tickers)) # Ensure order matches
    w_g1 <- w_g1_df %>% dplyr::pull(weights) %>% setNames(group_g1_tickers)

    ## Fill group covariance matrix
    for (g2 in seq_len(n_groups)){

      ### Get group members for second group
      group_g2 <- groups[g2]
      group_g2_tickers <- eligible_universe_m_d_ref %>%
        dplyr::filter(!is.na(!!rlang::sym(group_col)) &
                        !!rlang::sym(group_col) == group_g2) %>%
        dplyr::pull(tickers)

      ### Get weights for group g2
      w_g2_df <- micro_universe_m_d_ref_list[[group_g2]] %>%
        dplyr::filter(tickers %in% group_g2_tickers) %>%
        dplyr::slice(match(group_g2_tickers, tickers)) # Ensure order matches
      w_g2 <- w_g2_df %>% dplyr::pull(weights) %>% setNames(group_g2_tickers)

      ### Calculate covariance between groups
      cov_between_groups <- as.numeric(
        t(w_g1) %*% covariance_matrix[group_g1_tickers, group_g2_tickers,
                                      drop = FALSE] %*% w_g2
      )

      ### Fill group covariance matrix
      group_covariance_matrix[g1, g2] <- cov_between_groups

    }
  }

  # Macro-level ----------------------------------------------------------------

    ## Set Portfolio Weights
    if (verbose){
      cat("\n  Applying macro-level portfolio method...")
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
      liquidity_m_d_ref = group_liquidity_m_df,
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
      rp_method = macro_rp_method, exp_ret_score_tilt = macro_exp_ret_score_tilt,
      ## MVO
      n_random_ports = macro_n_random_ports, random_ports_method = macro_random_ports_method,
      opt_objective = macro_opt_objective, opt_method = macro_opt_method, ridge_pen = macro_ridge_pen,
      n_resamples = macro_n_resamples, exp_ret_score_jitter = macro_exp_ret_score_jitter,
      cov_eigval_jitter = macro_cov_eigval_jitter,
      ## Custom Weights
      custom_weights_m_d_ref = macro_custom_weights_m_d_ref,
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
        cat("\n  Applying top-down micro-level portfolio method...")
        cat("\n")
        cat("Portfolio construction method: ", micro_port_construction_method)
        cat("\n")
      }

      ### For each group, apply micro portfolio method
      micro_port_list <- process_micro_portfolios(
        parallel = parallel, groups = groups,
        group_weights = group_weights,
        micro_port_construction_method = micro_port_construction_method
      )

      ### Break up into micro_universe_m_d_ref
      micro_universe_m_d_ref_list <- purrr::map(
        micro_port_list, function(port) port@universe_m_d_ref@data
      )

    } else {

      micro_port_list = NULL

    }


  # Reconcile at portfolio level------------------------------------------------

    ## For each group, multiply stock_weight by group weight
    final_weights_m_df <- purrr::map_dfr(groups, function(g){

      ### Current micro universe
      current_micro_universe_m_d_ref <- micro_universe_m_d_ref_list[[g]] %>%
        dplyr::select(id, weights)

      ### Multiply weights by group weight
      current_micro_universe_m_d_ref %>%
        dplyr::mutate(weights = weights * group_weights[g])

    }) %>%
      dplyr::arrange(id)

    ## Left join to universe_m_d_ref
    universe_m_d_ref <- universe_m_d_ref %>%
      dplyr::left_join(final_weights_m_df, by = "id")

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

      return(list(
        universe_m_d_ref  = universe_m_d_ref,
        group_weights     = group_weights,
        macro             = macro_port,
        micro             = if (mmaf_method == "top_down"){
          micro_port_list
        }  else {
          list("consolidated" = micro_port)
        },
        group_cov_matrix  = group_covariance_matrix
      ))


}
