#' Prepare inputs for a hierarchical model.
#'
#' This functions takes wide-formar backtest and benchmark returns data, and transforms it into a long format.
#' It also adds a market factor proxy to the data frame. The function then merges the data with signal themes and prepares a formula for the Bayesian hierarchical model.
#'
#' @param selected_backtest_returns_corrected_positions_xts_upd_ref A xts containing the backtest returns data for various signals.
#'   - The first column should include dates.
#'   - Remaining columns represent signals (e.g., tickers) and their respective active returns.
#'
#' @param selected_market_factor_proxy_xts_upd_ref A xts containing benchmark returns data.
#'
#' @param signal_themes_m_d_ref A data frame containing metadata about signals. This data frame should include:
#'   - `tickers`: Signal identifiers matching those in `selected_backtest_returns_corrected_positions_xts_upd_ref`.
#'   - `theme`: Group membership for each signal, defining the clusters for the Bayesian hierarchical model.
#'   - `dates`: Dates corresponding to the backtest data.
#'   This input ensures proper alignment between signals and their associated themes.
#'
#' @param selected_backtest_returns_corrected_positions_m_upd_ref An already processed `selected_backtest_returns_corrected_positions_xts_upd_ref`.
#' This data.frame is already in long format and contemplates both the `selected_market_factor_proxy_xts_upd_ref` and the
#' `signal_themes_m_d_ref` theme data.
#'
#' @param model_spec_theme_level A character string specifying the desired Bayesian model structure.
#'   Options include:
#'   - `"random_intercept_fixed_slope"`: Includes random effects for the intercept at the theme level.
#'   - `"theme_specific_intercept_fixed_slope"`: Uses fixed intercepts for each theme.
#'   - `"theme_specific_intercept_theme_specific_slope"`: Includes fixed intercepts and slopes for each theme.
#'   - `"fixed_intercept_fixed_slope"`: Omits theme-level intercepts but includes random effects at the theme:signal level.


prepare_hierarchical_model_inputs <- function(selected_backtest_returns_corrected_positions_xts_upd_ref, selected_market_factor_proxy_xts_upd_ref, #Data
                                              signal_themes_m_d_ref, selected_backtest_returns_corrected_positions_m_upd_ref = NULL, model_spec_theme_level){

  #Prepare objects
  ######################
  ##Check if selected_backtest_returns_corrected_positions_m_upd_ref is alread being provided
  ##########################
  if(is.null(selected_backtest_returns_corrected_positions_m_upd_ref)){
    ###Check if selected_backtest_returns_corrected_positions_m_upd_ref can be produced
    if(any(is.null(selected_backtest_returns_corrected_positions_xts_upd_ref), is.null(selected_market_factor_proxy_xts_upd_ref),
           is.null(signal_themes_m_d_ref))){
      stop("selected_backtest_returns_corrected_positions_xts_upd_ref, selected_market_factor_proxy_xts_upd_ref and
           signal_themes_m_d_ref must be provided when selected_backtest_returns_corrected_positions_m_upd_ref is not given.")
    }

    ##Add market_factor_proxy
    selected_backtest_returns_corrected_positions_xts_upd_ref$market_factor_proxy <- selected_market_factor_proxy_xts_upd_ref

    ##Melt
    selected_backtest_returns_corrected_positions_m_upd_ref <- tidyr::pivot_longer(
      tibble::rownames_to_column(as.data.frame(selected_backtest_returns_corrected_positions_xts_upd_ref), var = "dates"),
      cols = -c(dates, market_factor_proxy),
      names_to = "tickers", #Rename to match priors
      values_to = "return"
    ) %>%
      dplyr::mutate(id = paste0(tickers, "-", dates)) %>% #Create id
      dplyr::select(id, tickers, dates, return, market_factor_proxy) %>% as.data.frame()  #Reorder columns


    ##Add theme
    selected_backtest_returns_corrected_positions_m_upd_ref <- dplyr::left_join(
      selected_backtest_returns_corrected_positions_m_upd_ref,
      signal_themes_m_d_ref %>% dplyr::select(tickers, theme), by = "tickers")
  }

  ###Create formula
  formula <-
    switch(model_spec_theme_level,
           # Random effects on intercept on theme level
           "random_intercept_fixed_slope" = formula(return ~ market_factor_proxy + (1 | theme) + (1 + market_factor_proxy | theme:tickers)),
           "theme_specific_intercept_fixed_slope" = formula(return ~ 0 + theme + market_factor_proxy + (1 + market_factor_proxy | theme:tickers)),
           "theme_specific_intercept_theme_specific_slope" = formula(return ~ 0 + theme + theme:market_factor_proxy + (1 + market_factor_proxy | theme:tickers)),
           "fixed_intercept_fixed_slope" = formula(return ~ market_factor_proxy + (1 + market_factor_proxy | theme:tickers))
    )

  hierarchical_model_inputs_list <- list(
    selected_backtest_returns_corrected_positions_m_upd_ref = selected_backtest_returns_corrected_positions_m_upd_ref,
    formula = formula
  )

  return(hierarchical_model_inputs_list)

}
