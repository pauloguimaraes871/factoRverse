#' Build aggregated group-level universe objects (and optional group covariance)
#'
#' @description
#' Aggregates stock-level information into a **group-level universe** (e.g., sectors)
#' for a single date snapshot, optionally computing:
#' 1) weighted group summaries (expected return score and liquidity metrics), and
#' 2) a group-by-group covariance matrix using the asset-level covariance.
#'
#' Internally, within each group the function builds a per-group weight vector
#' (from `eligible_universe_m_d_ref`). If a group's weights sum to zero, it **falls
#' back to equal-weights** for that group and warns once. The final outputs are:
#' - `group_universe_m_d_ref`: one row per group with aggregated metrics;
#' - `group_covariance_matrix`: optional \eqn{G \times G} covariance between groups;
#' - `group_liquidity_m_d_ref`: optional group-level liquidity frame aligned to
#'   `liquidity_m_d_ref`'s columns;
#' - `micro_universe_m_d_ref_list`: the per-group weights actually used.
#'
#' @param universe_m_d_ref `data.frame`. Must contain at least:
#'   - `tickers` (`character`): asset identifiers;
#'   - `dates` (`Date`): snapshot date;
#'   - `is_eligible` (`integer`/`logical`): must be 1 or 0;
#'   - `exp_ret_score` (`numeric`): stock-level expected return score (used in aggregation);
#'   - `weights` (`numeric`): portfolio weights for the snapshot (used to aggregate within group);
#'   - `{group_col}` (`character`): grouping column name provided via `group_col`.
#'
#' @param covariance_matrix `matrix` or `NULL`. Optional **asset-level** covariance
#'   \eqn{\Sigma} whose row/column names are exactly `eligible_universe_m_d_ref$tickers`.
#'   If provided, a **group covariance matrix** is computed via
#'   \eqn{\Sigma_G[g_1,g_2] = w_{g_1}^\top \Sigma_{g_1,g_2} w_{g_2}} using the
#'   per-group weights in `micro_universe_m_d_ref_list`.
#'
#' @param group_col `character(1)`. Column name in `eligible_universe_m_d_ref` that
#'   defines group membership (e.g., `"Sector"`).
#'
#' @param micro_universe_m_d_ref_list `NULL` or `named list`. Optional override for
#'   within-group weights. Names must cover **all** groups present in
#'   `eligible_universe_m_d_ref[[group_col]]`. Each element is a `data.frame`
#'   with columns:
#'   - `tickers` (`character`);
#'   - `weights` (`numeric`) — expected to be **within-group** weights (the function
#'     will normalize if the sum is nonzero; if the sum is zero, it falls back to EW with warning).
#'   When `NULL`, the function constructs this list from `eligible_universe_m_d_ref`.
#'
#' @param liquidity_m_d_ref `NULL` or `data.frame`. Optional stock-level liquidity
#'   metrics aligned by `id`/`tickers`/`dates`. All columns **after** the first three
#'   (id, tickers, dates) are aggregated by **weighted mean** using the group's
#'   within-group weights. If `NULL`, no liquidity aggregation is produced.
#'
#' @return `list` with components:
#' \itemize{
#'   \item `group_universe_m_d_ref` (`data.frame`): one row per group containing:
#'     \itemize{
#'       \item `id` (`character`): `"group-current_date"`;
#'       \item `tickers` (`character`): group identifier (the group name);
#'       \item `dates` (`Date`): snapshot date;
#'       \item `{liquidity columns}` (`numeric`, optional): weighted means of each
#'         liquidity metric provided in `liquidity_m_d_ref` (if any);
#'       \item `{*_bench_weights}/{target_weights}` (`numeric`, optional): group-level
#'         sums of matching columns found in `eligible_universe_m_d_ref`;
#'       \item `exp_ret_score` (`numeric`): **weighted mean** of stock `exp_ret_score`
#'         using within-group weights;
#'       \item `is_eligible` (`integer`): set to 1 for groups retained.
#'     }
#'   \item `group_covariance_matrix` (`matrix` or `NULL`): group-by-group covariance
#'     matrix (dimnames = groups), or `NULL` when `covariance_matrix` is `NULL`.
#'   \item `group_liquidity_m_d_ref` (`data.frame` or `NULL`): group-level liquidity
#'     frame containing the same columns as `liquidity_m_d_ref` (if provided), else `NULL`.
#'   \item `micro_universe_m_d_ref_list` (`named list`): the within-group weights used
#'     for aggregation and for the group covariance computation.
#' }
#'
#' @details
#' **Groups** are inferred from `eligible_universe_m_d_ref[[group_col]]` for the
#' provided snapshot. The function enforces:
#' \enumerate{
#'   \item `eligible_universe_m_d_ref` contains **only** eligible rows (`is_eligible == 1`);
#'   \item when `covariance_matrix` is provided, its row/column names are exactly
#'     `eligible_universe_m_d_ref$tickers`;
#'   \item when `micro_universe_m_d_ref_list` is `NULL`, per-group weights are
#'     built from `eligible_universe_m_d_ref$weights` and normalized within group;
#'     if a group's sum of weights is zero, it **falls back to equal-weights** with a warning.
#' }
#'
#' **Aggregation rules**
#' \itemize{
#'   \item `exp_ret_score`: `stats::weighted.mean()` within each group using
#'     the group's weights.
#'   \item Liquidity columns: each column in `liquidity_m_d_ref` (except the first
#'     three identifier columns) is aggregated by `stats::weighted.mean()` using
#'     the same within-group weights.
#'   \item Group `{*_bench_weights}`/`target_weights`: computed by `dplyr::summarise()`
#'     as **sums across constituent stocks** for each group.
#' }
#'
#' **Group covariance (optional)**
#' If `covariance_matrix` is provided, the function calls
#' `calculate_group_covariance_matrix()` with the per-group weights list and returns
#' \eqn{\Sigma_G}. This is coherent with ex-ante risk decomposition when the group
#' weights reflect the portfolio's within-group exposures.
#'
#' @section Errors and Warnings:
#' \itemize{
#'   \item **Errors:** missing `group_col`; non-matching ticker sets between
#'     `eligible_universe_m_d_ref$tickers` and `covariance_matrix` (when provided);
#'     invalid `micro_universe_m_d_ref_list` structure; presence of ineligible rows.
#'   \item **Warnings:** when a group's weights sum to zero, the function falls back
#'     to equal-weights for that group (message: *"Weights for group 'g' sum to zero. Fallback to equal weights."*).
#' }
#'
#' @seealso
#' `calculate_group_covariance_matrix()`, `dplyr::group_by()`, `dplyr::summarise()`,
#' `stats::weighted.mean()`, `purrr::map()` / `purrr::map_dfr()`.
#'
#' @examples NULL
compute_agg_macro_objects <- function(universe_m_d_ref, covariance_matrix = NULL,
                                      group_col, micro_universe_m_d_ref_list = NULL,
                                      liquidity_m_d_ref = NULL
                                      ){
  ## Get eligible universe---------------------------------------------------
  eligible_universe_m_d_ref <- universe_m_d_ref %>%
    dplyr::filter(is_eligible == 1)

  ## Define groups--------------------------------------------------------------
  if (!group_col %in% names(eligible_universe_m_d_ref)){
    stop(sprintf("group_col '%s' not found in eligible_universe_m_d_ref.",
                 group_col))
  }

  groups <- eligible_universe_m_d_ref %>%
    dplyr::pull(!!rlang::sym(group_col)) %>%
    unique() %>%
    sort()

  ## Basic checks---------------------------------------------------------------
  if (!is.null(covariance_matrix)){
    if (!is.matrix(covariance_matrix)){
      stop("covariance_matrix must be a numeric matrix.")
    }
    if (is.null(rownames(covariance_matrix)) ||
        is.null(colnames(covariance_matrix))){
      stop("covariance_matrix must have row and column names (tickers).")
    }
    if (!identical(rownames(covariance_matrix),
                   eligible_universe_m_d_ref$tickers)){
      stop("Row names of covariance_matrix must match eligible tickers.")
    }
  }
  if (!is.vector(groups) || !is.character(groups) || length(groups) == 0L){
    stop("groups must be a non-empty character vector.")
  }
  if (!is.null(micro_universe_m_d_ref_list) &&
      (!is.list(micro_universe_m_d_ref_list) || !all(groups %in% names(micro_universe_m_d_ref_list)))){
    stop("micro_universe_m_d_ref_list must be a named list with names covering all groups.")
  }
  if (eligible_universe_m_d_ref %>% dplyr::filter(is_eligible == 0) %>% nrow() > 0){
    stop("eligible_universe_m_d_ref must only contain eligible tickers.")
  }

  req_cols <- c("tickers", "dates", "is_eligible", group_col, "id")
  miss <- setdiff(req_cols, names(eligible_universe_m_d_ref))
  if (length(miss)) stop(sprintf("eligible_universe_m_d_ref missing columns: %s", paste(miss, collapse=", ")))

  has_weights_col <- "weights" %in% names(eligible_universe_m_d_ref)
  has_exp_ret_score_col <- "exp_ret_score" %in% names(eligible_universe_m_d_ref)

  ## Define micro_universe_m_d_ref_list if needed-----------------------------

    ### If micro_universe_m_d_ref_list is NULL, create from eligible_universe_m_d_ref
    if (is.null(micro_universe_m_d_ref_list)){

      ### Check weights column is present
      if (!isTRUE(has_weights_col)){
        stop("eligible_universe_m_d_ref must contain a 'weights' column to compute micro_universe_m_d_ref_list.")
      }

      ### Compute group internal weights
      micro_universe_m_d_ref_list <- purrr::map(groups, function(g){

        #### Get group members
        group_members <- eligible_universe_m_d_ref %>%
          dplyr::filter(!!rlang::sym(group_col) == g) %>%
          dplyr::pull(tickers)

        ##### If no members, stop
        if (length(group_members) == 0) stop(paste0("Group '", g, "' has no tickers."))

        #### Filter universe for group
        by_group_eligible_universe_m_d_ref <- eligible_universe_m_d_ref %>%
          dplyr::filter(tickers %in% group_members)

        ##### Calculate Sector Proxy
        ##### If weights sum to 0, revert to equal weights
        if (by_group_eligible_universe_m_d_ref %>% dplyr::pull(weights) %>% sum() < .Machine$double.eps){
          warning(paste0("Weights for group '", g, "' sum to zero. Fallback to equal weights."))
          by_group_eligible_universe_m_d_ref <- by_group_eligible_universe_m_d_ref %>%
            dplyr::mutate(weights = 1/nrow(by_group_eligible_universe_m_d_ref))
        } else {
          ##### Normalize
          by_group_eligible_universe_m_d_ref <- by_group_eligible_universe_m_d_ref %>%
            dplyr::mutate(weights = weights / sum(weights))
        }

        #### Return weights data.frame
        by_group_eligible_universe_m_d_ref

      }) %>%
        setNames(groups)

    }

  ## Compute aggregate metrics--------------------------------------------------

    ### group_universe_m_d_ref
    group_universe_m_d_ref <- purrr::map_dfr(groups, function(g){

      ### Current universe_m_d_ref
      current_group_universe_m_d_ref <- micro_universe_m_d_ref_list[[g]]
      current_date <- unique(current_group_universe_m_d_ref$dates)

      ### If weights in current_group_universe_m_d_ref sum to 0, revert to equal weights
      if (sum(current_group_universe_m_d_ref$weights) < .Machine$double.eps){
        warning(paste0("Weights for group '", g, "' sum to zero. Fallback to equal weights."))
        current_group_universe_m_d_ref <- current_group_universe_m_d_ref %>%
          dplyr::mutate(weights = 1/nrow(current_group_universe_m_d_ref))
      }

      ### Calculate weighted average of exp_ret_score
      if (isTRUE(has_exp_ret_score_col)){
        group_exp_ret_score <- stats::weighted.mean(
          current_group_universe_m_d_ref$exp_ret_score,
          current_group_universe_m_d_ref$weights,
          na.rm = TRUE
        )
      }

      ### Calculate weighted average of liquidity_m_d_ref colnames and liquidity_m_d_ref
      group_liq_cols <- list()
      if (!is.null(liquidity_m_d_ref)){
        liquidity_colnames <- names(liquidity_m_d_ref[,-c(1:3)]) #Exclude id, tickers, dates

        if (length(liquidity_colnames) > 0){
          for (liq_col in liquidity_colnames){
            group_liq_cols[[liq_col]] <- stats::weighted.mean(
              current_group_universe_m_d_ref[[liq_col]],
              current_group_universe_m_d_ref$weights,
              na.rm = TRUE
            )
          }
        }
      }

      ### Calculate sum of weights if 'weights' column is present
      if (isTRUE(has_weights_col)){
        total_group_weight <- sum(
          eligible_universe_m_d_ref %>%
            dplyr::filter(!!rlang::sym(group_col) == g) %>%
            dplyr::pull(weights)
        )
      } else {
        total_group_weight <- NULL
      }

      ### Data frame
      if (isTRUE(has_weights_col)){
        if (isTRUE(has_exp_ret_score_col)){
          group_row <- data.frame(
            id = paste0(g, "-", current_date),
            tickers = g,
            dates = as.Date(current_date),
            exp_ret_score = group_exp_ret_score,
            is_eligible = 1,
            weights = total_group_weight
          )
        } else {
          group_row <- data.frame(
            id = paste0(g, "-", current_date),
            tickers = g,
            dates = as.Date(current_date),
            is_eligible = 1,
            weights = total_group_weight
          )
        }
      } else {
        if (isTRUE(has_exp_ret_score_col)){
          group_row <- data.frame(
            id = paste0(g, "-", current_date),
            tickers = g,
            dates = as.Date(current_date),
            exp_ret_score = group_exp_ret_score,
            is_eligible = 1
          )
        } else {
          group_row <- data.frame(
            id = paste0(g, "-", current_date),
            tickers = g,
            dates = as.Date(current_date),
            is_eligible = 1
          )
        }
      }


      #### Add group liquidity columns if any
      if (length(group_liq_cols) > 0){
        group_row <- group_row %>%
          dplyr::bind_cols(as.data.frame(group_liq_cols))
      }

      group_row

    })

    ### Add group bench_weights and target_weights
    group_weight_colnames <- names(eligible_universe_m_d_ref)[
      stringr::str_detect(names(eligible_universe_m_d_ref), "_bench_weights") |
        stringr::str_detect(names(eligible_universe_m_d_ref), "target_weights")
    ]

    if (length(group_weight_colnames) > 0){
      #### Build total weights per sector, considering all stocks (universe_m_d_ref)
      #### and sum them by sector
      group_weights_m_d_ref <- universe_m_d_ref %>% #Here we need all stocks so that weights sum to 1
        dplyr::group_by(!!rlang::sym(group_col)) %>%
        dplyr::summarise(dplyr::across(
          dplyr::all_of(group_weight_colnames),
          \(x) sum(x, na.rm = TRUE)
        ),
        .groups = "drop"
        ) %>%
        dplyr::rename(sector = !!rlang::sym(group_col))

        ##### Defensively check that weight columns sum to 1
        for (col in group_weight_colnames){
          total_weight_sum <- sum(universe_m_d_ref[[col]], na.rm = TRUE)
          if (abs(total_weight_sum - 1) > 0.02){
            stop(paste0("Total sum of '", col, "' in universe_m_d_ref is ", round(total_weight_sum, 2),
                           ", which deviates from 1 by more than 0.02."))
          }
        }

      #### Join to group_universe_m_d_ref
      group_universe_m_d_ref <- group_universe_m_d_ref %>%
        dplyr::left_join(group_weights_m_d_ref,
                         by = c("tickers" = "sector"))
    }

    ### Relocate exp_ret_score and is_eligible to end
    if (isTRUE(has_exp_ret_score_col)){
      group_universe_m_d_ref <- group_universe_m_d_ref %>%
        dplyr::relocate(exp_ret_score, .after = dplyr::last_col())
    }
    group_universe_m_d_ref <- group_universe_m_d_ref %>%
      dplyr::relocate(is_eligible, .after = dplyr::last_col())
    if (isTRUE(has_weights_col)){
      group_universe_m_d_ref <- group_universe_m_d_ref %>%
        dplyr::relocate(weights, .after = dplyr::last_col())
    }

    ### Construct group_liquidity_m_d_ref if liquidity_m_d_ref exists
    group_liquidity_m_d_ref <- NULL
    if (!is.null(liquidity_m_d_ref)){
      group_liquidity_m_d_ref <- group_universe_m_d_ref %>%
        dplyr::select(dplyr::all_of(names(liquidity_m_d_ref)))
    }


  ## Compute group_covariance_matrix--------------------------------------------
  if (!is.null(covariance_matrix)){

    ### Compute sector-by-sector covariance
    group_covariance_matrix <- calculate_group_covariance_matrix(
      eligible_universe_m_d_ref = eligible_universe_m_d_ref,
      groups = groups,
      covariance_matrix = covariance_matrix,
      group_col = group_col,
      micro_universe_m_d_ref_list = micro_universe_m_d_ref_list
    )

  } else {
    group_covariance_matrix <- NULL
  }

  ## Return----------------------------------------------------------------------
  return(list(
    group_universe_m_d_ref  = group_universe_m_d_ref,
    group_covariance_matrix = group_covariance_matrix,
    group_liquidity_m_d_ref = group_liquidity_m_d_ref,
    micro_universe_m_d_ref_list = micro_universe_m_d_ref_list
  ))

}



#' Compute a group-by-group (block) covariance matrix
#'
#' @description
#' Builds a \eqn{G \times G} matrix of covariances between groups (e.g., sectors)
#' using a full asset-level covariance matrix \eqn{\Sigma} and per-group weight
#' vectors. Each entry \eqn{(g_1, g_2)} is computed as
#' \deqn{w_{g_1}^\top \, \Sigma_{g_1,g_2} \, w_{g_2},}
#' where \eqn{\Sigma_{g_1,g_2}} is the block of \eqn{\Sigma} whose rows are the
#' tickers in group \eqn{g_1} and columns are the tickers in group \eqn{g_2}.
#'
#' @param eligible_universe_m_d_ref A data.frame containing at least the
#'   columns `tickers` (character) and the grouping column indicated by
#'   `group_col`. Only rows with non-`NA` `group_col` values are used.
#' @param groups Character vector of group names (e.g., sectors) for which to
#'   compute the covariance matrix. Must match values in
#'   `eligible_universe_m_d_ref[[group_col]]`.
#' @param covariance_matrix Numeric covariance matrix \eqn{\Sigma} with row and
#'   column names equal to asset tickers. Must be square and cover all tickers
#'   present in `eligible_universe_m_d_ref$tickers`.
#' @param group_col Character scalar with the name of the column in
#'   `eligible_universe_m_d_ref` that defines group membership (e.g., `"sector"`).
#' @param micro_universe_m_d_ref_list Named list whose names are the elements of
#'   `groups`. Each element is a data.frame with columns:
#'   - `tickers` (character): tickers belonging to that group; and
#'   - `weights` (numeric): corresponding weights for the group-level
#'     aggregation.
#'
#' @return A numeric \eqn{G \times G} matrix with dimnames set to `groups`,
#'   where entry \eqn{(g_1, g_2)} equals
#'   \eqn{w_{g_1}^\top \, \Sigma_{g_1,g_2} \, w_{g_2}}.
#'
#' @details
#' **Input requirements and alignment**
#' * All tickers in `eligible_universe_m_d_ref$tickers` must be present as both
#'   row and column names of `covariance_matrix`.
#' * For each `g` in `groups`, `micro_universe_m_d_ref_list[[g]]` must include
#'   `tickers` and `weights` for **all** tickers that belong to group `g`
#'   according to `eligible_universe_m_d_ref[[group_col]]`. Internally, weights
#'   are aligned to the ticker order used to extract the \eqn{\Sigma} blocks.
#'
#' **Normalization**
#' The function does **not** renormalize group weights; results therefore reflect
#' the absolute scaling implied by the provided weights. If you need pure
#' within-group aggregation (i.e., each group’s weights summing to 1), normalize
#' the weights in `micro_universe_m_d_ref_list` beforehand.
#'
#' **Complexity**
#' The computation is \eqn{O(G^2)} block multiplications, where \eqn{G = } `length(groups)`,
#' plus the cost of extracting each block from \eqn{\Sigma}.
#'
#' @section Errors and Validation:
#' The function raises errors when:
#' * `covariance_matrix` is not a numeric matrix with named rows/columns.
#' * `groups` is empty or not a character vector.
#' * `micro_universe_m_d_ref_list` is not a named list covering all `groups`.
#' * Any ticker in `eligible_universe_m_d_ref$tickers` is missing from
#'   `covariance_matrix` row/column names.
#' * For any group, some required tickers are missing in the corresponding
#'   `micro_universe_m_d_ref_list[[g]]$tickers`, or the weights frame lacks the
#'   required `tickers`/`weights` columns.
#'
#' @seealso
#' * `stats::cov` for covariance computation at the asset level (if needed upstream).
#' * `dplyr::filter`, `dplyr::pull`, `dplyr::slice` and `rlang::sym` used here for data handling.
#'
#' @note
#' This function assumes `eligible_universe_m_d_ref$tickers` are unique within
#' each group for the given snapshot. If duplicates may occur, deduplicate or
#' aggregate beforehand.
calculate_group_covariance_matrix <- function(eligible_universe_m_d_ref,
                                              groups,
                                              covariance_matrix,
                                              group_col,
                                              micro_universe_m_d_ref_list = NULL){

  ## Checks----------------------------------------------------------------------
  eligible_tickers <- unique(eligible_universe_m_d_ref$tickers)
  sigma_tickers <- intersect(rownames(covariance_matrix), colnames(covariance_matrix))
  missing_in_sigma <- setdiff(eligible_tickers, sigma_tickers)
  if (length(missing_in_sigma) > 0L) {
    stop(sprintf("Tickers in eligible_universe_m_d_ref missing from covariance_matrix: %s",
                 paste(missing_in_sigma, collapse = ", ")))
  }

    ### Check weights coverage per group
    for (g in groups) {
      group_tickers <- eligible_universe_m_d_ref %>%
        dplyr::filter(!is.na(!!rlang::sym(group_col)) & !!rlang::sym(group_col) == g) %>%
        dplyr::pull(tickers) %>%
        unique()

      if (length(group_tickers) == 0L) next

      if (!g %in% names(micro_universe_m_d_ref_list)) {
        stop(sprintf("Group '%s' not found in micro_universe_m_d_ref_list names.", g))
      }

      w_df <- micro_universe_m_d_ref_list[[g]]
      if (!all(c("tickers", "weights") %in% colnames(w_df))) {
        stop(sprintf("micro_universe_m_d_ref_list[['%s']] must have columns 'tickers' and 'weights'.", g))
      }

      missing_in_weights <- setdiff(group_tickers, w_df$tickers)
      if (length(missing_in_weights) > 0L) {
        stop(sprintf(
          "Weights missing for some tickers in group '%s': %s",
          g, paste(missing_in_weights, collapse = ", ")
        ))
      }
    }

  ## Get number of groups
  n_groups <- length(groups)

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

      #### Conservatively check that both group of tickers are non-empty and are present
      if (length(group_g1_tickers) == 0 || length(group_g2_tickers) == 0){
        stop(paste0("One of the groups '", group_g1, "' or '", group_g2,
                    "' has no eligible tickers for covariance calculation."))
      }
      if (!all(group_g1_tickers %in% rownames(covariance_matrix)) ||
          !all(group_g2_tickers %in% colnames(covariance_matrix))){
        stop(paste0("Some tickers in groups '", group_g1, "' or '", group_g2,
                    "' are not in covariance matrix for covariance calculation."))
      }

      ### Calculate covariance between groups
      if (sum(w_g1) <= .Machine$double.eps){
        #### Fall back to EW
        w_g1 <- rep(1/length(group_g1_tickers), length(group_g1_tickers))
        names(w_g1) <- group_g1_tickers
      }
      if (sum(w_g2) <= .Machine$double.eps){
        #### Fall back to EW
        w_g2 <- rep(1/length(group_g2_tickers), length(group_g2_tickers))
        names(w_g2) <- group_g2_tickers
      }
      cov_between_groups <- as.numeric(
        t(w_g1) %*% covariance_matrix[group_g1_tickers, group_g2_tickers, drop = FALSE] %*% w_g2
      )

      ### Fill group covariance matrix
      group_covariance_matrix[g1, g2] <- cov_between_groups

    }
  }

  return(group_covariance_matrix)


}







