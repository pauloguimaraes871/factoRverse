#' Derive Leg Diagnostics for a SLSAF Portfolio Backtest
#'
#' @description
#' Turns the stock universe of a Simulated Long-Short Allocation Framework backtest into
#' tidy per-date, per-leg aggregates. Every `slsaf` plot and summary is built from this
#' one function, so the leg accounting is defined in a single place and can be tested
#' without any plotting machinery.
#'
#' @details
#' The framework splits the universe into a long block (names the eligibility cascade is
#' willing to buy) and a short block (index constituents it rejected, which may only be
#' underweighted). Almost every question worth asking of such a portfolio is a contrast
#' between those two blocks over time: how much of the benchmark sits in each, how their
#' scores compare, which one drives tracking error, and how each is composed by sector
#' and by capitalization.
#'
#' The aggregates are computed on rebalance dates only, since those are the dates at
#' which the universe is rebuilt and the blocks are defined.
#'
#' @param stock_universe_m_df A `stock_universe_m_df`, `meta_dataframe` or `data.frame`
#'   carrying the backtest universe. Must contain `dates`, `tickers`, `weights`,
#'   `is_long_candidate`, `is_short_candidate` and the benchmark weight column.
#'   Optional columns enrich the output: `exp_ret_score`, `exp_ret_score_raw`,
#'   `act_rel_risk_contr`, `act_weights`, `liquidity_classification`, and any group
#'   column. The underweight profile's `badness` is graded from `exp_ret_score_raw`,
#'   the column the short leg is built from, and is `NA` when that column is absent.
#' @param selected_benchmark Character scalar naming the benchmark, used to locate the
#'   `<selected_benchmark>_bench_weights` column.
#' @param group_col Optional character naming the column to use for the sector
#'   breakdown. Defaults to `"sectors"` when present.
#'
#' @return A named list of `data.frame`s:
#' \describe{
#'   \item{leg_summary}{One row per date and leg: asset counts, benchmark mass,
#'     portfolio mass, active weight, weighted and unweighted mean expected return
#'     score, and share of active risk contribution.}
#'   \item{leg_budget}{One row per date: the four components of the weight
#'     decomposition, which sum to 1 by construction.}
#'   \item{leg_sector}{One row per date, leg and group, when a group column exists.}
#'   \item{leg_liquidity}{One row per date, leg and liquidity classification, when that
#'     column exists.}
#'   \item{underweight_profile}{One row per date and short-block asset: benchmark
#'     weight, retained weight, relative trim, badness score and whether the cap bound.}
#' }
#'
#' @seealso \code{\link{create_slsaf_portfolio}}
derive_slsaf_leg_diagnostics <- function(stock_universe_m_df,
                                         selected_benchmark,
                                         group_col = NULL){

  # Validate and normalize inputs-----------------------------------------------

    ## Accept the S4 wrappers as well as a plain data.frame
    if (methods::is(stock_universe_m_df, "meta_dataframe")){
      universe_m_df <- stock_universe_m_df@data
    } else {
      universe_m_df <- stock_universe_m_df
    }

    if (!is.data.frame(universe_m_df)){
      stop("stock_universe_m_df must be a data.frame or a meta_dataframe.")
    }

    ## Benchmark
    if (is.null(selected_benchmark) || length(selected_benchmark) != 1 ||
        !is.character(selected_benchmark)){
      stop("selected_benchmark must be a single character string.")
    }
    bench_weights_col <- paste0(selected_benchmark, "_bench_weights")

    ## Required structure
    required_cols <- c("dates", "tickers", "weights", "is_long_candidate",
                       "is_short_candidate", bench_weights_col)
    missing_cols <- setdiff(required_cols, colnames(universe_m_df))
    if (length(missing_cols) > 0){
      stop(paste0("stock_universe_m_df is missing column(s): ",
                  paste(missing_cols, collapse = ", "),
                  ". Leg diagnostics are only defined for 'slsaf' backtests."))
    }

    ## Optional enrichments
    has_exp_ret_score <- "exp_ret_score" %in% colnames(universe_m_df)
    has_liquidity     <- "liquidity_classification" %in% colnames(universe_m_df)

    ### The short leg is built from the unscaled score, so its badness must be diagnosed
    ### from the same column. Reporting 1 / exp_ret_score instead would invert a scaler:
    ### under a return-predictive scaler such as 1 / idio_vol the raw and scaled rankings
    ### differ, and the underweight profile would then show an ordering that is not the
    ### one the underweights were graded by. There is no reliable way to detect after the
    ### fact whether a scaler was applied, so when the raw column is absent the badness is
    ### reported as unavailable rather than guessed from the scaled score.
    has_exp_ret_score_raw <- "exp_ret_score_raw" %in% colnames(universe_m_df)

    ### Active risk contributions are filled with zeros when the backtest ran without a
    ### covariance matrix, so an all-zero column means "no risk model" rather than "no
    ### risk". Treat it as absent, otherwise every risk plot would show a flat zero and
    ### look like a finding.
    has_rrc <- "act_rel_risk_contr" %in% colnames(universe_m_df) &&
      any(abs(universe_m_df$act_rel_risk_contr) > 1e-12, na.rm = TRUE)

    ### Group column defaults to the conventional sector column when present
    if (is.null(group_col) && "sectors" %in% colnames(universe_m_df)){
      group_col <- "sectors"
    }
    has_group <- !is.null(group_col) && group_col %in% colnames(universe_m_df)

  # Label the legs--------------------------------------------------------------

    ## Non-eligible names carry no weight and belong to neither block
    universe_m_df <- universe_m_df %>%
      dplyr::mutate(
        bench_weight = !!rlang::sym(bench_weights_col),
        leg = dplyr::case_when(
          is_long_candidate == 1L  ~ "Long",
          is_short_candidate == 1L ~ "Short",
          TRUE                     ~ NA_character_
        ),
        ### Active weight is the object of interest throughout: the long leg only ever
        ### adds to a benchmark position and the short leg only ever subtracts.
        ### Named distinctly from the aggregates below, since a summarised column would
        ### otherwise shadow this one inside the same summarise() call.
        asset_active_weight = weights - bench_weight
      ) %>%
      dplyr::filter(!is.na(leg))

    if (nrow(universe_m_df) == 0){
      stop("No long or short candidates found: is this a 'slsaf' backtest?")
    }

  # Per-date, per-leg summary---------------------------------------------------

    leg_summary <- universe_m_df %>%
      dplyr::group_by(dates, leg) %>%
      dplyr::summarise(
        n_assets       = dplyr::n(),
        bench_mass     = sum(bench_weight),
        port_mass      = sum(weights),
        active_weight  = sum(asset_active_weight),
        ## Gross active exposure taken in the leg, the natural weighting for the
        ## score comparison below
        gross_active   = sum(abs(asset_active_weight)),
        n_zeroed       = sum(weights <= 1e-10 & bench_weight > 1e-10),
        mean_exp_ret_score = if (has_exp_ret_score) mean(exp_ret_score) else NA_real_,
        ## Weighted by the active exposure actually taken, so this answers "what am I
        ## buying" against "what am I selling" rather than "what is in the block"
        wmean_exp_ret_score = if (has_exp_ret_score){
          if (sum(abs(asset_active_weight)) > 1e-12){
            stats::weighted.mean(exp_ret_score, abs(asset_active_weight))
          } else NA_real_
        } else NA_real_,
        ## Active risk contribution is signed and sums to 1 across the portfolio, so a
        ## leg's share is directly comparable with its share of gross active weight
        rrc_share = if (has_rrc) sum(act_rel_risk_contr) else NA_real_,
        .groups = "drop"
      ) %>%
      dplyr::arrange(dates, leg)

    ## Share of gross active exposure, so risk share can be read against weight share
    leg_summary <- leg_summary %>%
      dplyr::group_by(dates) %>%
      dplyr::mutate(
        active_share = if (sum(gross_active) > 1e-12) gross_active / sum(gross_active) else NA_real_
      ) %>%
      dplyr::ungroup() %>%
      as.data.frame()

  # Per-date budget decomposition-----------------------------------------------

    ## Three components account for every unit of portfolio weight: what the short block
    ## keeps, what the long block already held in the benchmark, and what it received on
    ## top. The released budget is reported alongside them but is NOT a fourth component,
    ## since it is the same money as long_active seen from the other side. Stacking all
    ## four would double-count it.
    leg_budget <- universe_m_df %>%
      dplyr::group_by(dates) %>%
      dplyr::summarise(
        short_retained     = sum(weights[leg == "Short"]),
        short_underweight  = sum(bench_weight[leg == "Short"]) - sum(weights[leg == "Short"]),
        long_benchmark     = sum(bench_weight[leg == "Long"]),
        long_active        = sum(weights[leg == "Long"]) - sum(bench_weight[leg == "Long"]),
        short_budget       = sum(bench_weight[leg == "Short"]),
        benchmark_coverage = sum(bench_weight[leg == "Long"]) /
          dplyr::if_else(sum(bench_weight) > 1e-12, sum(bench_weight), NA_real_),
        .groups = "drop"
      ) %>%
      ## The three stackable components must account for the whole portfolio
      dplyr::mutate(
        port_total = short_retained + long_benchmark + long_active
      ) %>%
      dplyr::arrange(dates) %>%
      as.data.frame()

    ### Guard the identities the plots rely on
    if (any(abs(leg_budget$port_total - 1) > 1e-6)){
      stop("slsaf budget decomposition does not account for the whole portfolio.")
    }
    if (any(abs(leg_budget$short_underweight - leg_budget$long_active) > 1e-6)){
      stop("Released budget and long-leg active weight must coincide.")
    }

  # Composition breakdowns------------------------------------------------------

    ## Sector composition within each leg
    if (isTRUE(has_group)){
      leg_sector <- universe_m_df %>%
        dplyr::group_by(dates, leg, group = !!rlang::sym(group_col)) %>%
        dplyr::summarise(
          n_assets      = dplyr::n(),
          bench_mass    = sum(bench_weight),
          port_mass     = sum(weights),
          active_weight = sum(asset_active_weight),
          .groups = "drop"
        ) %>%
        ### Share within the leg, so the two legs are comparable despite different sizes
        dplyr::group_by(dates, leg) %>%
        dplyr::mutate(
          leg_share = if (sum(bench_mass) > 1e-12) bench_mass / sum(bench_mass) else NA_real_
        ) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(dates, leg, group) %>%
        as.data.frame()
    } else {
      leg_sector <- NULL
    }

    ## Capitalization profile within each leg
    if (isTRUE(has_liquidity)){
      leg_liquidity <- universe_m_df %>%
        dplyr::group_by(dates, leg, liquidity_classification) %>%
        dplyr::summarise(
          n_assets      = dplyr::n(),
          bench_mass    = sum(bench_weight),
          port_mass     = sum(weights),
          active_weight = sum(asset_active_weight),
          .groups = "drop"
        ) %>%
        dplyr::group_by(dates, leg) %>%
        dplyr::mutate(
          leg_share = if (sum(bench_mass) > 1e-12) bench_mass / sum(bench_mass) else NA_real_
        ) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(dates, leg, liquidity_classification) %>%
        as.data.frame()
    } else {
      leg_liquidity <- NULL
    }

  # Underweight intensity profile-----------------------------------------------

    ## Per short-block asset, how much of its index position was given up and how bad
    ## the signal said it was. This is what makes the exponents and the cap visible.
    underweight_profile <- universe_m_df %>%
      dplyr::filter(leg == "Short", bench_weight > 1e-12) %>%
      dplyr::mutate(
        underweight   = bench_weight - weights,
        relative_trim = (bench_weight - weights) / bench_weight,
        ### The cap binds exactly when the whole index position was sold
        is_capped     = weights <= 1e-10,
        ### Graded from the unscaled score, exactly as the short leg was built
        badness       = if (has_exp_ret_score_raw) 1 / exp_ret_score_raw else NA_real_
      ) %>%
      dplyr::select(dplyr::any_of(c("dates", "tickers", "bench_weight", "weights",
                                    "underweight", "relative_trim", "is_capped",
                                    "badness", "exp_ret_score_raw", "exp_ret_score",
                                    "liquidity_classification"))) %>%
      dplyr::arrange(dates, dplyr::desc(relative_trim)) %>%
      as.data.frame()

  # Return----------------------------------------------------------------------

    return(list(
      leg_summary         = leg_summary,
      leg_budget          = leg_budget,
      leg_sector          = leg_sector,
      leg_liquidity       = leg_liquidity,
      underweight_profile = underweight_profile
    ))
}


#' Build Plot-Ready Objects for a SLSAF Portfolio Backtest
#'
#' @description
#' Wraps the aggregates from \code{\link{derive_slsaf_leg_diagnostics}} into
#' `meta_dataframe` objects so the existing plot methods can render them directly. Every
#' `slsaf` plot is therefore an ordinary `meta_dataframe` plot over a purpose-built
#' frame, rather than bespoke plotting code.
#'
#' @details
#' The `meta_dataframe` contract requires an `id`, `tickers` and `dates` triple, with
#' `id` equal to `tickers-dates`. The aggregates are not per-asset, so the `tickers`
#' slot is used to carry whatever the series is keyed by: the leg name, the budget
#' component, or the asset itself for the per-name profile. This is the same device the
#' macro-level plots use to render group series.
#'
#' @param stock_universe_m_df The backtest universe, as produced by an `slsaf` run.
#' @param selected_benchmark Character scalar naming the benchmark.
#' @param group_col Optional character naming the sector column.
#'
#' @return A named list of `meta_dataframe` objects (`budget`, `coverage`, `leg_score`,
#'   `leg_risk`, `underweight`) plus `universe_with_leg`, the input universe augmented
#'   with a `leg` label, and `diagnostics`, the raw aggregates.
build_slsaf_plot_m_dfs <- function(stock_universe_m_df,
                                   selected_benchmark,
                                   group_col = NULL){

  # Aggregate--------------------------------------------------------------------

    diagnostics <- derive_slsaf_leg_diagnostics(
      stock_universe_m_df = stock_universe_m_df,
      selected_benchmark = selected_benchmark,
      group_col = group_col
    )

    ## Small helper: the meta_dataframe contract keyed by an arbitrary series name
    as_series_m_df <- function(df, series_col, value_cols){
      out <- df %>%
        dplyr::rename(tickers = !!rlang::sym(series_col)) %>%
        dplyr::mutate(
          tickers = as.character(tickers),
          dates = as.Date(dates),
          id = paste0(tickers, "-", dates)
        ) %>%
        dplyr::select(dplyr::all_of(c("id", "tickers", "dates", value_cols))) %>%
        dplyr::arrange(id)
      suppressMessages(create_meta_dataframe(out))
    }

  # Weight decomposition---------------------------------------------------------

    ## Three stackable components accounting for the whole portfolio. The released
    ## budget is deliberately excluded: it equals the long leg's active weight, so
    ## stacking it too would double-count.
    budget_long_df <- diagnostics$leg_budget %>%
      dplyr::select(dates, short_retained, long_benchmark, long_active) %>%
      tidyr::pivot_longer(cols = -dates, names_to = "component", values_to = "weight") %>%
      dplyr::mutate(
        component = dplyr::recode(component,
                                  short_retained = "Short leg retained",
                                  long_benchmark = "Long leg benchmark",
                                  long_active    = "Long leg active")
      ) %>%
      as.data.frame()

  # Benchmark coverage-----------------------------------------------------------

    coverage_df <- diagnostics$leg_summary %>%
      dplyr::select(dates, leg, bench_mass, port_mass, n_assets) %>%
      as.data.frame()

  # Leg score and risk-----------------------------------------------------------

    leg_score_df <- diagnostics$leg_summary %>%
      dplyr::select(dates, leg, wmean_exp_ret_score, mean_exp_ret_score) %>%
      as.data.frame()

    leg_risk_df <- diagnostics$leg_summary %>%
      dplyr::select(dates, leg, rrc_share, active_share) %>%
      as.data.frame()

  # Underweight profile----------------------------------------------------------

    underweight_df <- diagnostics$underweight_profile %>%
      dplyr::mutate(is_capped = as.numeric(is_capped)) %>%
      dplyr::select(dplyr::any_of(c("dates", "tickers", "relative_trim", "badness",
                                    "bench_weight", "underweight", "is_capped"))) %>%
      as.data.frame()

  # Universe labelled by leg-----------------------------------------------------

    ## Adding the label to the universe itself lets every per-asset plot be sliced by
    ## leg through the existing clustering and filtering arguments
    universe_with_leg <- stock_universe_m_df
    universe_with_leg@data <- universe_with_leg@data %>%
      dplyr::mutate(
        leg = dplyr::case_when(
          is_long_candidate == 1L  ~ "Long leg",
          is_short_candidate == 1L ~ "Short leg",
          TRUE                     ~ "Not represented"
        )
      )

  # Return-----------------------------------------------------------------------

    return(list(
      budget            = as_series_m_df(budget_long_df, "component", "weight"),
      coverage          = as_series_m_df(coverage_df, "leg", c("bench_mass", "port_mass", "n_assets")),
      leg_score         = as_series_m_df(leg_score_df, "leg", c("wmean_exp_ret_score", "mean_exp_ret_score")),
      leg_risk          = as_series_m_df(leg_risk_df, "leg", c("rrc_share", "active_share")),
      underweight       = suppressMessages(create_meta_dataframe(
        underweight_df %>%
          dplyr::mutate(dates = as.Date(dates), id = paste0(tickers, "-", dates)) %>%
          dplyr::relocate(id, tickers, dates) %>%
          dplyr::arrange(id)
      )),
      universe_with_leg = universe_with_leg,
      diagnostics       = diagnostics
    ))
}
