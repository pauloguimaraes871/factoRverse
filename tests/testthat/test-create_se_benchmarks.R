test_that("create_se_benchmarks correctly builds benchmarks when all signals are significant", {

  #Create signal_universe_m_d_ref
  signal_universe_m_d_ref <- data.frame(id = c("Alpha-2001-07-15", "low_Beta-2001-07-15", "Gamma-2001-07-15"),
                                        tickers = c("Alpha", "low_Beta", "Gamma"),
                                        dates = c("2001-07-15", "2001-07-15", "2001-07-15"),
                                        mean_active_return = rnorm(3, 0, 1),
                                        tracking_error = runif(3, 0, 1),
                                        IR = rnorm(3,0,1),
                                        alpha = rnorm(3,0,1),
                                        AP = rnorm(3,0,1),
                                        beta = rnorm(3,0,1),
                                        treynor = rnorm(3,0,1),
                                        p_value = c(0.05,0.02,0.03)
  )

  signal_themes_m_d_ref <- data.frame(id = c("Alpha-2001-07-15", "low_Beta-2001-07-15", "Gamma-2001-07-15"),
                                       tickers = c("Alpha", "low_Beta", "Gamma"),
                                       dates = c("2001-07-15", "2001-07-15", "2001-07-15"),
                                       theme = c("Value", "Momentum", "Value")
  )

  signal_universe_m_d_ref$adjusted_p_value <- p.adjust(signal_universe_m_d_ref$p_value, "none")
  signal_universe_m_d_ref$final_signal <- signal_transform(signal_universe_m_d_ref$alpha, 0.99, 0.01)
  signal_universe_m_d_ref$top_assets <- c(1,1,1)

  expected_results <- signal_universe_m_d_ref
  expected_results$theme_ss <- c(0.25, 0.50, 0.25)
  expected_results$theme_sb <- c(0.25, 0.50, 0.25)

  expect_equal(dplyr::select(expected_results, id, tickers, dates, theme_ss, theme_sb),
               create_se_benchmarks(signal_universe_m_d_ref, signal_themes_m_d_ref = signal_themes_m_d_ref),
               tolerance = 1e-3)


})

test_that("create_se_benchmarks correctly builds benchmarks when one signal is not significant", {

  #Create signal_universe_m_d_ref
  signal_universe_m_d_ref <- data.frame(id = c("Alpha-2001-07-15", "low_Beta-2001-07-15", "Gamma-2001-07-15"),
                                        tickers = c("Alpha", "low_Beta", "Gamma"),
                                        dates = c("2001-07-15", "2001-07-15", "2001-07-15"),
                                        mean_active_return = rnorm(3, 0, 1),
                                        tracking_error = runif(3, 0, 1),
                                        IR = rnorm(3,0,1),
                                        alpha = rnorm(3,0,1),
                                        AP = rnorm(3,0,1),
                                        beta = rnorm(3,0,1),
                                        treynor = rnorm(3,0,1),
                                        p_value = c(0.05,0.02,0.03)
  )

  signal_themes_m_d_ref <- data.frame(id = c("Alpha-2001-07-15", "low_Beta-2001-07-15", "Gamma-2001-07-15"),
                                       tickers = c("Alpha", "low_Beta", "Gamma"),
                                       dates = c("2001-07-15", "2001-07-15", "2001-07-15"),
                                       theme = c("Value", "Momentum", "Value")
  )

  signal_universe_m_d_ref$adjusted_p_value <- p.adjust(signal_universe_m_d_ref$p_value, "none")
  signal_universe_m_d_ref$final_signal <- signal_transform(signal_universe_m_d_ref$alpha, 0.99, 0.01)
  signal_universe_m_d_ref$top_assets <- c(1,1,0)

  expected_results <- signal_universe_m_d_ref
  expected_results$theme_ss <- c(0.25, 0.50, 0.25)
  expected_results$theme_sb <- c(0.50, 0.50, 0)

  expect_equal(dplyr::select(expected_results, id, tickers, dates, theme_ss, theme_sb),
               create_se_benchmarks(signal_universe_m_d_ref, signal_themes_m_d_ref = signal_themes_m_d_ref),
               tolerance = 1e-3)


})

test_that("create_se_benchmarks correctly builds benchmarks when one theme has no significant signal", {

  #Create signals_m_d_ref_test
  signal_universe_m_d_ref <- data.frame(id = c("Alpha-2001-07-15", "low_Beta-2001-07-15", "Gamma-2001-07-15"),
                                        tickers = c("Alpha", "low_Beta", "Gamma"),
                                        dates = c("2001-07-15", "2001-07-15", "2001-07-15"),
                                        mean_active_return = rnorm(3, 0, 1),
                                        tracking_error = runif(3, 0, 1),
                                        IR = rnorm(3,0,1),
                                        alpha = rnorm(3,0,1),
                                        AP = rnorm(3,0,1),
                                        beta = rnorm(3,0,1),
                                        treynor = rnorm(3,0,1),
                                        p_value = c(0.05,0.2,0.03)
  )

  signal_universe_m_d_ref$adjusted_p_value <- p.adjust(signal_universe_m_d_ref$p_value, "none")
  signal_universe_m_d_ref$final_signal <- signal_transform(signal_universe_m_d_ref$alpha, 0.99, 0.01)
  signal_universe_m_d_ref$top_assets <- c(1,0,1)
  signal_themes_m_d_ref <- data.frame(id = c("Alpha-2001-07-15", "low_Beta-2001-07-15", "Gamma-2001-07-15"),
                                      tickers = c("Alpha", "low_Beta", "Gamma"),
                                      dates = c("2001-07-15", "2001-07-15", "2001-07-15"),
                                      theme = c("value", "momentum", "value")
  )


  expected_results <- signal_universe_m_d_ref
  expected_results$theme_ss <- c(0.25,0.50,0.25)
  expected_results$theme_sb <- c(0.50, 0, 0.50)

  expect_equal(create_se_benchmarks(signal_universe_m_d_ref, signal_themes_m_d_ref = signal_themes_m_d_ref),
               dplyr::select(expected_results, id, tickers, dates, theme_ss, theme_sb),
               tolerance = 1e-3)


})
