# A risk-targeted meta backtest on real B3 data
# =============================================================================
# Builds a single risky sleeve from a characteristic portfolio, blends it against an
# index-tracking residual, and lets the targeting rule set the split so the combination aims at a
# stated tracking error.
#
# The data is the package's B3 panel: 462 real tickers, monthly signals from 2022-10 to 2023-04,
# daily stock returns from 2022-06. Signal values are synthetic, the ticker universe and the
# calendar are not.
#
# Run with: source(system.file("examples", "risk_targeted_meta_backtest.R", package = "factoRverse"))

library(factoRverse)
library(magrittr)


# 1. Load the panel -----------------------------------------------------------

data_path <- system.file("../tests/testthat/testdata/toy_preprocessed_port_obj.RData",
                         package = "factoRverse")
if (!nzchar(data_path) || !file.exists(data_path)) {
  ## Falls back to the source-tree location when the package is loaded with devtools
  data_path <- file.path("tests", "testthat", "testdata", "toy_preprocessed_port_obj.RData")
}
load(data_path)

residual_ticker <- "BOVA11"


# 2. Build the residual sleeve ------------------------------------------------
# The residual has to track the benchmark for a tracking-error target to mean anything: only then
# does tracking error scale linearly with the weight, so that halving the sleeve halves the
# tracking error and a fully residual portfolio has none. Blending toward cash instead would raise
# tracking error past a point, since a cash portfolio is maximally far from the index.
#
# There is no index ETF in this panel, so one is replicated from it: the benchmark weights applied
# to the daily stock returns. In production this would be the actual BOVA11 series, which carries
# its own tracking difference and management fee.

replicate_index <- function(daily_returns, benchmark_weights) {

  weight_dates <- sort(unique(benchmark_weights$dates))
  daily_dates <- zoo::index(daily_returns)
  replicated <- rep(NA_real_, length(daily_dates))

  for (i in seq_along(daily_dates)) {
    ## The most recent weights knowable at this day, so the replication carries no look-ahead
    usable <- weight_dates[weight_dates <= daily_dates[i]]
    if (length(usable) == 0) next

    weights_now <- benchmark_weights[benchmark_weights$dates == max(usable), ]
    weights_now <- weights_now[!is.na(weights_now$ibov) & weights_now$ibov > 0, ]
    held <- intersect(weights_now$tickers, colnames(daily_returns))
    if (length(held) == 0) next

    weight_vector <- weights_now$ibov[match(held, weights_now$tickers)]
    weight_vector <- weight_vector / sum(weight_vector)
    day_returns <- as.numeric(daily_returns[i, held])

    ## A name without a quote that day contributes nothing rather than dragging the index to NA
    usable_returns <- !is.na(day_returns)
    if (!any(usable_returns)) next
    replicated[i] <- sum(weight_vector[usable_returns] * day_returns[usable_returns]) /
      sum(weight_vector[usable_returns])
  }

  replicated[is.na(replicated)] <- 0
  replicated
}

index_daily <- replicate_index(daily_stock_returns_m_xts, benchmark_weights_m_df)

## A real index ETF does not track perfectly: sampling, fees and cash drag leave a small daily
## tracking difference. Adding one matters beyond realism here. A residual whose active return is
## identically zero has zero variance in active space, which makes the active-return correlation
## matrix singular and breaks the clustering the stock-level analytics run. Two basis points a day
## is about 0.3 percentage points of annualised tracking error, which is typical for a large ETF.
set.seed(20260827)
residual_daily <- index_daily + stats::rnorm(length(index_daily), 0, 0.02)

cat("Residual sleeve replicated from the benchmark weights.\n")
cat("  Index annualised volatility: ",
    round(stats::sd(index_daily) * sqrt(252), 2), "%\n", sep = "")
cat("  Residual tracking error vs the index: ",
    round(stats::sd(residual_daily - index_daily) * sqrt(252), 2), "%\n", sep = "")


# 3. Add the residual to every stock-level object ------------------------------
# It is a tradable holding, not a special case, so it is priced and traded like any other row and
# pays the same direct and indirect costs.

add_residual_row <- function(df, values) {
  own_dates <- sort(unique(df$dates))
  extra <- data.frame(id = paste0(residual_ticker, "-", own_dates),
                      tickers = residual_ticker, dates = own_dates,
                      stringsAsFactors = FALSE)
  for (column in setdiff(names(df), names(extra))) {
    extra[[column]] <- if (!is.null(values[[column]])) values[[column]] else values[["default"]]
  }
  out <- rbind(df, extra[, names(df)])
  out[order(out$id), ]
}

## The residual's forward return is the index's, and its last date has none, exactly as the stocks
monthly_index <- as.numeric(benchmark_returns_m_xts[, "ibov"])
index_dates <- zoo::index(benchmark_returns_m_xts)
fwd_dates <- sort(unique(fwd_return_m_df$dates))
residual_fwd <- c(monthly_index[-1], NA_real_)[match(fwd_dates, index_dates)]

fwd_augmented <- add_residual_row(fwd_return_m_df, list(default = NA_real_))
fwd_augmented$fwd_return_1m[fwd_augmented$tickers == residual_ticker] <- residual_fwd
## Stock groups are deliberately not passed to either run. compute_agg_macro_objects() reads the
## liquidity column NAMES from liquidity_m_d_ref but reads their VALUES out of the group universe,
## which is built from eligible_universe_m_d_ref and does not carry them, so the aggregation fails
## on a length mismatch. This reproduces on the pristine panel with no residual involved, so it is
## a defect in the group path rather than anything to do with the meta backtest.


## Highly liquid and low volatility, which is what an index ETF is
liquidity_augmented <- add_residual_row(liquidity_m_df,
                                        list(default = 1e9, presence = 100))
volatility_augmented <- add_residual_row(
  volatility_m_df,
  list(default = stats::sd(residual_daily) * sqrt(252),
       daily_vol = stats::sd(residual_daily)))

## It is not a constituent of the index it tracks, so it carries no benchmark weight
benchmark_weights_augmented <- add_residual_row(benchmark_weights_m_df, list(default = 0))
signals_augmented <- add_residual_row(signals_m_df, list(default = 0))

daily_augmented <- cbind(daily_stock_returns_m_xts, residual_daily)
colnames(daily_augmented)[ncol(daily_augmented)] <- residual_ticker

## A day without a quote is treated as a zero return. The package would normally fill these from
## group medians inside clean_returns_sample(), but that path enters on any NA without checking
## that stock groups were actually supplied, and errors when they were not. Filling here keeps the
## example on the risk-targeting question. It mildly understates volatility for the least liquid
## names, which matters little since the sleeve holds liquid ones.
daily_augmented[is.na(daily_augmented)] <- 0


# 4. Wrap into meta objects ----------------------------------------------------

signals_meta <- create_meta_dataframe(signals_augmented, type = "signals")
fwd_meta <- create_meta_dataframe(fwd_augmented, type = "target")
liquidity_meta <- create_meta_dataframe(liquidity_augmented)
volatility_meta <- create_meta_dataframe(volatility_augmented)
benchmark_weights_meta <- create_meta_dataframe(benchmark_weights_augmented, type = "weights")
benchmark_returns_meta <- create_meta_xts(benchmark_returns_m_xts)
daily_returns_meta <- create_meta_xts(daily_augmented, type = "returns",
                                      asset_type = "stocks", meta_xts_name = "B3")

## The daily benchmark is the same replication, since that is what the index is here. Active
## returns at stock level are formed against it, and without a daily series aligned to the daily
## stock returns the active-return covariance cannot be built at all.
##
## The benchmark is the pure index; the residual is that index plus its tracking difference, which
## is what puts a small but non-zero floor under the achievable tracking error.
daily_bench_meta <- create_meta_xts(
  xts::xts(data.frame(ibov = index_daily), order.by = zoo::index(daily_augmented)),
  type = "returns", asset_type = "benchmark", meta_xts_name = "B3")


# 5. The risky sleeve ----------------------------------------------------------
# An ordinary characteristic portfolio: the cheapest third of the universe by book yield,
# signal-weighted. This is the portfolio the targeting rule will scale.

sleeve_config <- create_port_backtest_config(
  chosen_score_metric_and_position = c(book_yield = "long"),
  eligibility_quantile_range = c(0.67, 1.0),
  selected_benchmark = "ibov",
  initial_buffer_period = 2,
  rebalancing_months = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
  port_construction_method = "sw",
  main_liquidity_metric = "mean_volfin_3m",
  config_name = "book_yield_sleeve") %>%
  add_transaction_costs_parameters(direct_transaction_cost = 0.07, alpha = 1,
                                   lambda = "dynamic", strategy_aum = 25000)

cat("\nRunning the risky sleeve...\n")
sleeve <- run_port_backtest(
  signals_m_df = signals_meta, fwd_return_m_df = fwd_meta,
  liquidity_m_df = liquidity_meta, volatility_m_df = volatility_meta,
  config = sleeve_config,
  benchmark_weights_m_df = benchmark_weights_meta,
  benchmark_returns_m_xts = benchmark_returns_meta,
  verbose = FALSE, parallel = FALSE)

cohort <- create_port_backtest_cohort(list(sleeve), cohort_name = "risk_targeted_cohort")


# 6. The meta configuration ----------------------------------------------------
# port_construction_method is custom_weights because the weight comes from the targeting rule
# rather than from ranking a cross-section, and that value is what says so.

meta_inner <- create_port_backtest_config(
  chosen_score_metric_and_position = NULL,
  eligibility_quantile_range = c(0, 1),
  selected_benchmark = "ibov",
  initial_buffer_period = 2,
  rebalancing_months = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
  port_construction_method = "custom_weights",
  main_liquidity_metric = "mean_volfin_3m",
  ## This one prices the stock-level analytics off the daily returns, so its sample size counts
  ## days. The default carried by create_port_backtest_config is 252, which is more daily history
  ## than this panel has by the first rebalance date, so it is set explicitly.
  cov_est_method = create_cov_est_method("sample", 90, TRUE, "ibov"),
  config_name = "te_managed_inner") %>%
  add_transaction_costs_parameters(direct_transaction_cost = 0.07, alpha = 1,
                                   lambda = "dynamic", strategy_aum = 25000)

meta_config <- create_port_metabacktest_config(
  meta_inner, type = "risk_targeted", return_basis = "net",
  config_name = "te_managed", verbose = FALSE) %>%
  add_risk_target_parameters(
    residual_ticker = residual_ticker,
    ## Four annualised percentage points of tracking error against the IBOV
    target = 4,
    target_metric = "tracking_error",
    ## p = 1 is ordinary risk targeting; p = 2 would give the inverse-variance response of a
    ## volatility-managed portfolio
    p = 1,
    ## Risk re-estimated from daily stock returns under the sleeve's current weights, rather than
    ## inherited from the base backtest, which would be stale between rebalances
    vol_source = "ex_ante",
    vol_cov_est_method = create_cov_est_method("ewma", 60, FALSE, NULL),
    ## Never fully out of the sleeve, never levered
    min_weight = 0.2, max_weight = 1)


# 7. Run the meta backtest -----------------------------------------------------

cat("Running the risk-targeted meta backtest...\n")
meta_results <- run_port_backtest(
  signals_m_df = signals_meta, fwd_return_m_df = fwd_meta,
  liquidity_m_df = liquidity_meta, volatility_m_df = volatility_meta,
  config = meta_config, port_backtest_cohort = cohort,
  benchmark_weights_m_df = benchmark_weights_meta,
  benchmark_returns_m_xts = benchmark_returns_meta,
  daily_stock_returns_m_xts = daily_returns_meta,
  daily_bench_returns_m_xts = daily_bench_meta,
  verbose = FALSE, parallel = FALSE)


# 8. Inspect -------------------------------------------------------------------

methods::show(meta_results)

## The targeting rule and the capital-market-line view
# plot(meta_results, plot_id = "Capital Market Line")
# plot(meta_results, plot_id = "Risky Weight vs Sleeve Risk")
# plot(meta_results, plot_id = "Realised vs Target Risk")
# plot(meta_results, plot_id = "Meta Weights Over Time")

invisible(meta_results)
