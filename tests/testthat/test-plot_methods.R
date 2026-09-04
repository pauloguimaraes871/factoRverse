#Fixture: a portfolio carrying populated micro legs, built through the cheapest method
#that produces them
build_micro_port <- function(){

  load(paste(test_path(), "/testdata/", "artificial_port_obj.RData", sep = ""))

  current_date <- "2001-04-15"
  signals_m_d_ref <- signals_m_df[which(signals_m_df$dates == current_date), ]

  universe_m_d_ref <- signals_m_d_ref[, c("id", "tickers", "dates")]
  universe_m_d_ref$exp_ret_score_raw <- c(1.2, 0.2, 0.05, 0.04, 3)
  universe_m_d_ref$exp_ret_score <- universe_m_d_ref$exp_ret_score_raw

  benchmark_weights_m_d_ref <- benchmark_weights_m_df[
    which(benchmark_weights_m_df$dates == current_date), ]

  universe_m_d_ref <- classify_investment_universe(
    universe_m_d_ref = universe_m_d_ref,
    eligibility_quantile_range = c(0.6, 1),
    selected_benchmark = "ibov",
    benchmark_weights_m_d_ref = benchmark_weights_m_d_ref,
    include_benchmark_in_universe = TRUE,
    verbose = FALSE
  )

  set_portfolio_weights(
    universe_m_d_ref = universe_m_d_ref,
    port_construction_method = "slsaf",
    sub_port_configs = list(long = create_sub_port_config("sw")),
    selected_benchmark = "ibov",
    verbose = FALSE
  )
}

#Skip whenever the plot method's own dependencies are absent, since it refuses to run at
#all without them and the failure would say nothing about the level gate
skip_without_plot_pkgs <- function(){
  for (pkg in c("gridExtra", "scales", "ggdist", "ggraph", "ggrepel", "igraph",
                "RColorBrewer")){
    skip_if_not_installed(pkg)
  }
}

#The assertions below never render anything. Each one drives plot() with a deliberately
#invalid plot type, which is validated only after the level has been resolved and the
#micro leg selected. The resulting message therefore reports how far the call travelled:
#"Specified micro_port name not found." means the Micro level was reached and its
#dispatch ran, while "Invalid plot type specified." means the call never left the
#Portfolio level. This keeps the tests deterministic and free of interactive prompts.
bogus_type <- "not a real plot type"

#Plot level options
test_that("the Micro level is reachable whenever legs exist, whatever the method is called", {

  skip_without_plot_pkgs()

  port_obj <- build_micro_port()

  #The premise: the object really does carry both legs
  expect_named(port_obj@micro, c("long", "short"))

  #The level was gated on the method name rather than on the slot, so slsaf portfolios
  #were offered only the Portfolio level even though the dispatch below is already
  #generic and works unchanged for them. Both names must now reach it.
  for (method_name in c("slsaf", "mmaf")){

    probe_port <- port_obj
    probe_port@port_construction_method <- method_name

    expect_error(
      plot(probe_port, level = "Micro", micro_port = "nonexistent_leg",
           type = bogus_type),
      "Specified micro_port name not found.",
      fixed = TRUE
    )
  }
})

test_that("a micro leg is selected by name or by index", {

  skip_without_plot_pkgs()

  port_obj <- build_micro_port()

  #A valid selector gets past the dispatch and hands the call to the leg itself, which is
  #the object that then rejects the type. Contrast with the previous test, where an
  #unknown name is refused by the dispatch before any delegation happens.
  for (selector in list("long", "short", 1L, 2L)){
    expect_error(
      plot(port_obj, level = "Micro", micro_port = selector, type = bogus_type),
      "Invalid plot type specified.",
      fixed = TRUE
    )
  }

  #Exactly two legs are on offer, so anything outside that range is refused
  for (selector in list(0L, 3L)){
    expect_error(
      plot(port_obj, level = "Micro", micro_port = selector, type = bogus_type),
      "Invalid micro_port index.",
      fixed = TRUE
    )
  }
})

test_that("the Micro level is withheld when no leg is populated", {

  skip_without_plot_pkgs()

  port_obj <- build_micro_port()

  #Gating on the slot must not become gating on nothing. slsaf leaves the short leg NULL
  #when every constituent is eligible, and the long leg NULL when no budget is released,
  #so a list of NULLs has to count as empty rather than as two entries. Reaching the
  #Micro dispatch here would refuse the unknown name; staying at the Portfolio level
  #rejects the type instead, which is what must happen.
  empty_micro_port <- port_obj
  empty_micro_port@micro <- list(long = NULL, short = NULL)
  expect_error(
    plot(empty_micro_port, level = "Micro", micro_port = "nonexistent_leg",
         type = bogus_type),
    "Invalid plot type specified.",
    fixed = TRUE
  )

  null_micro_port <- port_obj
  null_micro_port@micro <- NULL
  expect_error(
    plot(null_micro_port, level = "Micro", micro_port = "nonexistent_leg",
         type = bogus_type),
    "Invalid plot type specified.",
    fixed = TRUE
  )
})
