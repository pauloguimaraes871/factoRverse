#Fixture: a two-date slsaf universe with both blocks populated
build_slsaf_diagnostics_universe <- function(){

  make_date <- function(current_date, short_weights, long_weights, short_scores, long_scores){
    tickers <- c(paste0("S", seq_along(short_weights)), paste0("L", seq_along(long_weights)))
    data.frame(
      id      = paste0(tickers, "-", current_date),
      tickers = tickers,
      dates   = rep(as.Date(current_date), length(tickers)),
      exp_ret_score = c(short_scores, long_scores),
      ##No scaler in this fixture, so the raw and scaled scores coincide
      exp_ret_score_raw = c(short_scores, long_scores),
      ibov_bench_weights = c(short_weights, long_weights),
      is_long_candidate  = c(rep(0L, length(short_weights)), rep(1L, length(long_weights))),
      is_short_candidate = c(rep(1L, length(short_weights)), rep(0L, length(long_weights))),
      is_eligible = 1,
      sectors = c("Mining", "Retail", "Banks", "Mining"),
      liquidity_classification = c("small_caps", "mid_caps", "mega_caps", "large_caps"),
      act_rel_risk_contr = c(0.30, 0.10, 0.35, 0.25),
      stringsAsFactors = FALSE
    )
  }

  ##Weights are chosen so the arithmetic is checkable by hand: the short block gives up
  ##half of its benchmark mass and the long block receives exactly that
  first <- make_date("2020-01-15",
                     short_weights = c(0.30, 0.10), long_weights = c(0.40, 0.20),
                     short_scores = c(0.5, 0.8), long_scores = c(2.0, 3.0))
  first$weights <- c(0.10, 0.10, 0.55, 0.25)

  second <- make_date("2020-02-15",
                      short_weights = c(0.20, 0.20), long_weights = c(0.35, 0.25),
                      short_scores = c(0.4, 0.9), long_scores = c(1.8, 2.6))
  second$weights <- c(0.00, 0.10, 0.55, 0.35)

  suppressMessages(create_meta_dataframe(rbind(first, second) %>% dplyr::arrange(id)))
}

#Leg summary
test_that("derive_slsaf_leg_diagnostics summarises each leg per date", {

  universe_m_df <- build_slsaf_diagnostics_universe()
  diagnostics <- derive_slsaf_leg_diagnostics(universe_m_df, "ibov")

  expect_named(diagnostics, c("leg_summary", "leg_budget", "leg_sector",
                              "leg_liquidity", "underweight_profile"))

  leg_summary <- diagnostics$leg_summary
  expect_equal(nrow(leg_summary), 4)
  expect_setequal(leg_summary$leg, c("Long", "Short"))

  first_short <- leg_summary[leg_summary$dates == as.Date("2020-01-15") &
                               leg_summary$leg == "Short", ]
  first_long  <- leg_summary[leg_summary$dates == as.Date("2020-01-15") &
                               leg_summary$leg == "Long", ]

  #Benchmark mass and portfolio mass are simple sums
  expect_equal(first_short$bench_mass, 0.40)
  expect_equal(first_short$port_mass, 0.20)
  expect_equal(first_long$bench_mass, 0.60)
  expect_equal(first_long$port_mass, 0.80)

  #The short leg gave up exactly what the long leg received
  expect_equal(first_short$active_weight, -0.20)
  expect_equal(first_long$active_weight, 0.20)

  #Each leg carries half the gross active exposure by construction here
  expect_equal(first_long$active_share, 0.5)
  expect_equal(first_short$active_share, 0.5)
})

test_that("the leg score is weighted by the active exposure actually taken", {

  universe_m_df <- build_slsaf_diagnostics_universe()
  diagnostics <- derive_slsaf_leg_diagnostics(universe_m_df, "ibov")

  first_short <- diagnostics$leg_summary[
    diagnostics$leg_summary$dates == as.Date("2020-01-15") &
      diagnostics$leg_summary$leg == "Short", ]

  #S1 gives up 0.20 and S2 gives up nothing, so the weighted score is S1's alone,
  #while the unweighted mean would be the midpoint. The distinction matters: it is
  #what makes the score comparison answer "what am I selling" rather than "what is
  #in the block".
  expect_equal(first_short$wmean_exp_ret_score, 0.5)
  expect_equal(first_short$mean_exp_ret_score, mean(c(0.5, 0.8)))
})

#Budget decomposition
test_that("the budget decomposition accounts for the whole portfolio", {

  universe_m_df <- build_slsaf_diagnostics_universe()
  leg_budget <- derive_slsaf_leg_diagnostics(universe_m_df, "ibov")$leg_budget

  #Three components, not four: the released budget is the same money as the long leg's
  #active weight seen from the other side, so stacking it too would double-count
  expect_true(all(abs(leg_budget$port_total - 1) < 1e-10))
  expect_equal(leg_budget$short_underweight, leg_budget$long_active)

  first <- leg_budget[leg_budget$dates == as.Date("2020-01-15"), ]
  expect_equal(first$short_retained, 0.20)
  expect_equal(first$long_benchmark, 0.60)
  expect_equal(first$long_active, 0.20)
  expect_equal(first$short_budget, 0.40)
  expect_equal(first$benchmark_coverage, 0.60)
})

test_that("the budget decomposition accepts every portfolio the constructor accepted", {

  #Regression. port_total is algebraically just sum(weights), but it was recomposed from
  #three separately accumulated group sums and asserted against its own hardcoded 1e-6,
  #so its floating-point residual differed from the single sum the constructor checked.
  #A portfolio that built cleanly could therefore be rejected here, taking every slsaf
  #plot down with it while the portfolio itself was fine. The numbers below are the ones
  #a real build shipped: quantised benchmark weights one micro-unit above 1, and the
  #weights the overlay produced from them.
  units <- c(40713L, 50026L, 67578L, 96902L, 35123L, 96043L, 100091L, 75268L,
             72498L, 22890L, 35498L, 32926L, 77561L, 51074L, 84803L, 61007L)
  weights <- c(0.010815256971524433, 0.020892709090913841, 0, 0.029574363440984644,
               0.013745975399163644, 0, 0.021667940246693912, 0,
               0.13062896935633994, 0.081020969356339942, 0.093628969356339936,
               0.091056969356339945, 0.13569196935633995, 0.10920496935633994,
               0.14293396935633995, 0.11913796935633994)
  scores <- c(0.945857, 1.192715, 0.642032, 0.999701, 1.141235, 0.490928, 0.886506,
              0.413000, 1.974109, 2.140560, 1.618746, 2.135343, 2.817567, 2.076489,
              2.274912, 2.439392)

  tickers <- c(paste0("S", 1:8), paste0("L", 1:8))
  universe_m_df <- data.frame(
    id      = paste0(tickers, "-2020-01-15"),
    tickers = tickers,
    dates   = rep(as.Date("2020-01-15"), 16L),
    weights = weights,
    exp_ret_score      = scores,
    exp_ret_score_raw  = scores,
    ibov_bench_weights = units / 1e6,
    is_long_candidate  = c(rep(0L, 8), rep(1L, 8)),
    is_short_candidate = c(rep(1L, 8), rep(0L, 8)),
    is_eligible        = rep(1, 16L),
    stringsAsFactors = FALSE
  )

  #The premise of the regression: this portfolio is one the constructor accepts, so the
  #diagnostics have no standing to refuse it
  expect_lte(abs(sum(universe_m_df$weights) - 1), 1e-6)

  expect_no_error(
    leg_budget <- derive_slsaf_leg_diagnostics(universe_m_df, "ibov")$leg_budget
  )

  #The displayed decomposition still adds up to the total it decomposes, which is what
  #the stacked budget plot relies on. This is checked here rather than inside the
  #function: the two are the same quantity by construction, so asserting it there would
  #only reintroduce the second accumulation path that caused the disagreement.
  expect_equal(leg_budget$port_total, sum(universe_m_df$weights), tolerance = 1e-12)
})

#Composition breakdowns
test_that("derive_slsaf_leg_diagnostics breaks each leg down by sector and capitalization", {

  universe_m_df <- build_slsaf_diagnostics_universe()
  diagnostics <- derive_slsaf_leg_diagnostics(universe_m_df, "ibov")

  expect_false(is.null(diagnostics$leg_sector))
  expect_false(is.null(diagnostics$leg_liquidity))

  #Shares are computed within a leg, so the two legs stay comparable despite differing
  #in size
  sector_shares <- diagnostics$leg_sector %>%
    dplyr::group_by(dates, leg) %>%
    dplyr::summarise(total = sum(leg_share), .groups = "drop")
  expect_true(all(abs(sector_shares$total - 1) < 1e-10))

  liquidity_shares <- diagnostics$leg_liquidity %>%
    dplyr::group_by(dates, leg) %>%
    dplyr::summarise(total = sum(leg_share), .groups = "drop")
  expect_true(all(abs(liquidity_shares$total - 1) < 1e-10))
})

#Underweight profile
test_that("the underweight profile reports relative trim and capping per constituent", {

  universe_m_df <- build_slsaf_diagnostics_universe()
  profile <- derive_slsaf_leg_diagnostics(universe_m_df, "ibov")$underweight_profile

  #Only short-block constituents appear
  expect_setequal(unique(profile$tickers), c("S1", "S2"))

  first_s1 <- profile[profile$dates == as.Date("2020-01-15") & profile$tickers == "S1", ]
  expect_equal(first_s1$underweight, 0.20)
  expect_equal(first_s1$relative_trim, 0.20 / 0.30)
  expect_false(first_s1$is_capped)
  expect_equal(first_s1$badness, 1 / 0.5)

  #On the second date S1 is sold in full, which is exactly what the cap flag marks
  second_s1 <- profile[profile$dates == as.Date("2020-02-15") & profile$tickers == "S1", ]
  expect_equal(second_s1$relative_trim, 1)
  expect_true(second_s1$is_capped)
})

test_that("badness is graded from the unscaled score, never from the scaled one", {

  universe_m_df <- build_slsaf_diagnostics_universe()

  #A return-predictive scaler such as 1 / idio_vol reorders the two short names: S1 is
  #the worse name on the raw score but the better one once scaled. The short leg is built
  #from the raw score, so the profile must follow it. Reading the scaled column instead
  #would draw an underweight chart whose badness ordering is the reverse of the ordering
  #the underweights were actually graded by, contradicting the guarantee the method makes.
  is_s1 <- universe_m_df@data$tickers == "S1"
  is_s2 <- universe_m_df@data$tickers == "S2"
  universe_m_df@data$exp_ret_score[is_s1] <- 4.0
  universe_m_df@data$exp_ret_score[is_s2] <- 0.1

  profile <- derive_slsaf_leg_diagnostics(universe_m_df, "ibov")$underweight_profile

  first_s1 <- profile[profile$dates == as.Date("2020-01-15") & profile$tickers == "S1", ]
  first_s2 <- profile[profile$dates == as.Date("2020-01-15") & profile$tickers == "S2", ]

  #Raw scores are 0.5 and 0.8, so S1 stays the worse name
  expect_equal(first_s1$badness, 1 / 0.5)
  expect_equal(first_s2$badness, 1 / 0.8)
  expect_gt(first_s1$badness, first_s2$badness)
})

test_that("badness is unavailable rather than guessed when the raw score is absent", {

  universe_m_df <- build_slsaf_diagnostics_universe()
  universe_m_df@data$exp_ret_score_raw <- NULL

  profile <- derive_slsaf_leg_diagnostics(universe_m_df, "ibov")$underweight_profile

  #There is no reliable way to tell after the fact whether a scaler was applied, so
  #falling back to the scaled score could silently invert the ordering
  expect_true(all(is.na(profile$badness)))
})

#Absent risk model
test_that("an all-zero risk contribution column is treated as absent, not as zero risk", {

  universe_m_df <- build_slsaf_diagnostics_universe()

  #This is what a backtest run without a covariance matrix produces
  universe_m_df@data$act_rel_risk_contr <- 0

  diagnostics <- derive_slsaf_leg_diagnostics(universe_m_df, "ibov")

  #Reporting zero would render as a flat line and read like a finding
  expect_true(all(is.na(diagnostics$leg_summary$rrc_share)))
})

#Validation
test_that("derive_slsaf_leg_diagnostics rejects universes it cannot interpret", {

  universe_m_df <- build_slsaf_diagnostics_universe()

  expect_error(derive_slsaf_leg_diagnostics(universe_m_df, NULL),
               "selected_benchmark must be a single character string")
  expect_error(derive_slsaf_leg_diagnostics(universe_m_df, "smll"),
               "is missing column")

  #A universe from another method has no block split
  no_blocks_m_df <- universe_m_df
  no_blocks_m_df@data$is_long_candidate <- NULL
  expect_error(derive_slsaf_leg_diagnostics(no_blocks_m_df, "ibov"),
               "only defined for 'slsaf' backtests")
})

#Plot objects
test_that("build_slsaf_plot_m_dfs wraps the aggregates as meta_dataframes", {

  universe_m_df <- build_slsaf_diagnostics_universe()
  plot_objects <- build_slsaf_plot_m_dfs(universe_m_df, "ibov")

  for (object_name in c("budget", "coverage", "leg_score", "leg_risk", "underweight")){
    expect_s4_class(plot_objects[[object_name]], "meta_dataframe")
  }

  #The budget frame carries the three stackable components, keyed by component name
  expect_setequal(unique(plot_objects$budget@data$tickers),
                  c("Short leg retained", "Long leg benchmark", "Long leg active"))

  #The universe gains a readable leg label so per-asset plots can be sliced by it
  expect_true("leg" %in% names(plot_objects$universe_with_leg@data))
  expect_setequal(unique(plot_objects$universe_with_leg@data$leg),
                  c("Long leg", "Short leg"))
})
