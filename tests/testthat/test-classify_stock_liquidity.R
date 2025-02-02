test_that("classify_stock_liquidity adequately classifies stocks and applies liquidity_floor_rule", {

  #Create cutoff
  liquidity_floor_cutoffs_test <- data.frame(
    liquidity_classification = c("nano_caps", "micro_caps", "small_caps", "mid_caps", "large_caps", "mega_caps"),
    mean_volfin_3m = c(0, 1000, 5000, 25000, 100000, 500000),
    presence = c(0, 97.5, 99, 100, 100, 100)
  )

  #Create liquidity_m_df_test
  liquidity_m_df_test <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15", "Stock C-2020-05-15", "Stock A-2020-06-15", "Stock B-2020-06-15", "Stock C-2020-06-15"),
    tickers = c("Stock A", "Stock B", "Stock C", "Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2020-05-15", "2020-05-15", "2020-05-15", "2020-06-15", "2020-06-15", "2020-06-15"), format = "%Y-%m-%d"),
    mean_volfin_3m = c(500, 6000, 25500, 1000, 6000, 25500),
    presence = c(95, 99, 100, 100, 100, 100)
  )

  #Floor rule
  liquidity_floor_rule_test = "micro_caps"
  apply_liquidity_floor_rule_test = TRUE

  #Expected
  expected_results <- liquidity_m_df_test
  expected_results$liquidity_classification <- c("nano_caps", "small_caps", "mid_caps", "micro_caps", "small_caps", "mid_caps")
  expected_results$liquidity_floor <- c(0, 1, 1, 1, 1, 1)

  #Check
  expect_equal(classify_stock_liquidity(liquidity_floor_cutoffs = liquidity_floor_cutoffs_test, liquidity_m_df = liquidity_m_df_test,
                                        liquidity_floor_rule = liquidity_floor_rule_test, apply_liquidity_floor_rule = apply_liquidity_floor_rule_test,
                                        filter_out_liquidity_floor_rule = FALSE),
               expected_results)

})

test_that("classify_stock_liquidity adequately classifies stocks and applies liquidity_floor_rule - 2", {

  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Floor rule
  liquidity_floor_rule_test = "small_caps"
  apply_liquidity_floor_rule_test = TRUE

  #Expected
  expected_results <- liquidity_m_df[which(liquidity_m_df$dates == "2001-07-15"),]
  expected_results$liquidity_classification <- c("mid_caps", "micro_caps", "mid_caps", "mid_caps")
  expected_results$liquidity_floor <- c(1, 0, 1, 1)

  #Check
  results <- classify_stock_liquidity(liquidity_floor_cutoffs = liquidity_floor_cutoffs_df, liquidity_m_df = liquidity_m_df[which(liquidity_m_df$dates == "2001-07-15"),],
                                      liquidity_floor_rule = liquidity_floor_rule_test, apply_liquidity_floor_rule = apply_liquidity_floor_rule_test,
                                      filter_out_liquidity_floor_rule = FALSE)
  rownames(expected_results) <- NULL
  rownames(results) <- NULL


  expect_equal(results, expected_results)

})

test_that("classify_stock_liquidity adequately classifies stocks and applies liquidity_floor_rule when setting decimals", {

  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Floor rule
  liquidity_floor_rule_test = "small_caps"
  apply_liquidity_floor_rule_test = TRUE

  #Create cutoff
  liquidity_floor_cutoffs_test <- data.frame(
    liquidity_classification = c("micro_caps", "small_caps", "mid_caps", "large_caps", "mega_caps"),
    mean_volfin_3m = c(0.1, 0.25, 0.5, 0.75, 0.9),
    presence = c(0.1, 0.25, 0.5, 0.75, 0.9)
  )

  #liquidity_floor_cutoffs_test$mean_volfin_3m <- purrr::pmap_vec(list(liquidity_floor_cutoffs_test$mean_volfin_3m), ~quantile(liquidity_m_df$mean_volfin_3m, probs = .x))
  #liquidity_floor_cutoffs_test$presence <- purrr::pmap_vec(list(liquidity_floor_cutoffs_test$presence), ~quantile(liquidity_m_df$presence, probs = .x))

  #Expected
  expected_results <- liquidity_m_df[which(liquidity_m_df$dates == "2001-07-15"),]
  expected_results$liquidity_classification <- c("nano_caps", "nano_caps", "mid_caps", "small_caps")
  expected_results$liquidity_floor <- c(0, 0, 1, 1)

  #Check
  results <- classify_stock_liquidity(liquidity_floor_cutoffs = liquidity_floor_cutoffs_test, liquidity_m_df = liquidity_m_df[which(liquidity_m_df$dates == "2001-07-15"),],
                                      liquidity_floor_rule = liquidity_floor_rule_test, apply_liquidity_floor_rule = apply_liquidity_floor_rule_test,
                                      filter_out_liquidity_floor_rule = FALSE)
  rownames(expected_results) <- NULL
  rownames(results) <- NULL

  expect_equal(results, expected_results)

})

test_that("classify_stock_liquidity only set quantiles when both metrics are set as decimals", {

  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Floor rule
  liquidity_floor_rule_test = "small_caps"
  apply_liquidity_floor_rule_test = TRUE

  #Create cutoff
  liquidity_floor_cutoffs_test <- data.frame(
    liquidity_classification = c("micro_caps", "small_caps", "mid_caps", "large_caps", "mega_caps"),
    mean_volfin_3m = c(0.1, 0.25, 0.5, 0.75, 0.9),
    presence = c(10, 25, 50, 75, 90)
  )


  #Check
  expect_no_message(classify_stock_liquidity(liquidity_floor_cutoffs = liquidity_floor_cutoffs_test, liquidity_m_df = liquidity_m_df[which(liquidity_m_df$dates == "2001-07-15"),],
                                             liquidity_floor_rule = liquidity_floor_rule_test, apply_liquidity_floor_rule = apply_liquidity_floor_rule_test,
                                             filter_out_liquidity_floor_rule = FALSE))

})

test_that("classify_stock_liquidity adequately classifies stocks and applies liquidity_floor_rule when there are conflicting metrics", {


  #Create cutoff
  liquidity_floor_cutoffs_test <- data.frame(
    liquidity_classification = c("micro_caps", "small_caps", "mid_caps", "large_caps", "mega_caps"),
    mean_volfin_3m = c(1000, 5000, 25000, 100000, 500000),
    presence = c(97.5, 99, 100, 100, 100)
  )

  #Create liquidity_m_df_test
  liquidity_m_df_test <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15", "Stock C-2020-05-15", "Stock A-2020-06-15", "Stock B-2020-06-15", "Stock C-2020-06-15"),
    tickers = c("Stock A", "Stock B", "Stock C", "Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2020-05-15", "2020-05-15", "2020-05-15", "2020-06-15", "2020-06-15", "2020-06-15"), format = "%Y-%m-%d"),
    mean_volfin_3m = c(500, 6000, 25500, 1000, 6000, 25500),
    presence = c(95, 98, 100, 100, 100, 100)
  )

  #Floor rule
  liquidity_floor_rule_test = "small_caps"
  apply_liquidity_floor_rule_test = TRUE

  #Expected
  expected_results <- liquidity_m_df_test
  expected_results$liquidity_classification <- c("nano_caps", "micro_caps", "mid_caps", "micro_caps", "small_caps", "mid_caps")
  expected_results$liquidity_floor <- c(0, 0, 1, 0, 1, 1)

  #Check
  expect_equal(classify_stock_liquidity(liquidity_floor_cutoffs = liquidity_floor_cutoffs_test, liquidity_m_df = liquidity_m_df_test,
                                        liquidity_floor_rule = liquidity_floor_rule_test, apply_liquidity_floor_rule = apply_liquidity_floor_rule_test,
                                        filter_out_liquidity_floor_rule = FALSE),
               expected_results)

})

test_that("classify_stock_liquidity adequately classifies stocks, applies liquidity_floor_rule and filters out stocks under liquidity_floor_rule", {

  #Create cutoff
  liquidity_floor_cutoffs_test <- data.frame(
    liquidity_classification = c("micro_caps", "small_caps", "mid_caps", "large_caps", "mega_caps"),
    mean_volfin_3m = c(1000, 5000, 25000, 100000, 500000),
    presence = c(97.5, 99, 100, 100, 100)
  )

  #Create liquidity_m_df_test
  liquidity_m_df_test <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15", "Stock C-2020-05-15", "Stock A-2020-06-15", "Stock B-2020-06-15", "Stock C-2020-06-15"),
    tickers = c("Stock A", "Stock B", "Stock C", "Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2020-05-15", "2020-05-15", "2020-05-15", "2020-06-15", "2020-06-15", "2020-06-15"), format = "%Y-%m-%d"),
    mean_volfin_3m = c(500, 6000, 25500, 1000, 6000, 25500),
    presence = c(95, 98, 100, 100, 100, 100)
  )

  #Floor rule
  liquidity_floor_rule_test = "small_caps"
  apply_liquidity_floor_rule_test = TRUE

  #Expected
  expected_results <- liquidity_m_df_test
  expected_results$liquidity_classification <- c("nano_caps", "micro_caps", "mid_caps", "micro_caps", "small_caps", "mid_caps")
  expected_results$liquidity_floor <- c(0, 0, 1, 0, 1, 1)
  expected_results <- expected_results %>% dplyr::filter(liquidity_floor == 1)

  #Check
  expect_equal(classify_stock_liquidity(liquidity_floor_cutoffs = liquidity_floor_cutoffs_test, liquidity_m_df = liquidity_m_df_test,
                                        liquidity_floor_rule = liquidity_floor_rule_test, apply_liquidity_floor_rule = apply_liquidity_floor_rule_test,
                                        filter_out_liquidity_floor_rule = TRUE, verbose = TRUE),
               expected_results)

})

test_that("classify_stock_liquidity throws an error when liquidity_floor_cutoffs_test not in correct order", {

  #Create cutoff
  liquidity_floor_cutoffs_test <- list(
    mega_caps = c(mean_volfin_3m = 500000, presence = 100),
    large_caps = c(mean_volfin_3m = 100000, presence = 100),
    mid_caps = c(mean_volfin_3m = 25000, presence = 100),
    small_caps = c(mean_volfin_3m = 5000, presence = 99),
    micro_caps = c(mean_volfin_3m = 1000, presence = 97.5)
  )
  liquidity_floor_cutoffs_test <- data.frame(
    liquidity_classification = c("mega_caps", "large_caps", "mid_caps", "small_caps", "micro_caps"),
    mean_volfin_3m = c(500000, 100000, 25000, 5000, 1000),
    presence = c(100, 100, 100, 99, 97.5)
  )

  #Create liquidity_m_df_test
  liquidity_m_df_test <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15", "Stock C-2020-05-15", "Stock A-2020-06-15", "Stock B-2020-06-15", "Stock C-2020-06-15"),
    tickers = c("Stock A", "Stock B", "Stock C", "Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2020-05-15", "2020-05-15", "2020-05-15", "2020-06-15", "2020-06-15", "2020-06-15"), format = "%Y-%m-%d"),
    mean_volfin_3m = c(500, 6000, 25500, 1000, 6000, 25500),
    presence = c(95, 98, 100, 100, 100, 100)
  )

  #Floor rule
  liquidity_floor_rule_test = "small_caps"
  apply_liquidity_floor_rule_test = TRUE

  #Check
  expect_error(classify_stock_liquidity(liquidity_floor_cutoffs = liquidity_floor_cutoffs_test, liquidity_m_df = liquidity_m_df_test,
                                        liquidity_floor_rule = liquidity_floor_rule_test, apply_liquidity_floor_rule = apply_liquidity_floor_rule_test,
                                        filter_out_liquidity_floor_rule = FALSE),
               "liquidity_floor_cutoffs is not in ascending order")

})

test_that("classify_stock_liquidity throws an error when liquidity_floor_cutoffs_test orders are conflicting", {

  #Create cutoff
  liquidity_floor_cutoffs_test <- data.frame(
    liquidity_classification = c("micro_caps", "small_caps", "mid_caps", "large_caps", "mega_caps"),
    mean_volfin_3m = c(1000, 5000, 25000, 100000, 500000),
    presence = c(99, 97.5, 100, 100, 100)
  )


  #Create liquidity_m_df_test
  liquidity_m_df_test <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15", "Stock C-2020-05-15", "Stock A-2020-06-15", "Stock B-2020-06-15", "Stock C-2020-06-15"),
    tickers = c("Stock A", "Stock B", "Stock C", "Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2020-05-15", "2020-05-15", "2020-05-15", "2020-06-15", "2020-06-15", "2020-06-15"), format = "%Y-%m-%d"),
    mean_volfin_3m = c(500, 6000, 25500, 1000, 6000, 25500),
    presence = c(95, 98, 100, 100, 100, 100)
  )

  #Floor rule
  liquidity_floor_rule_test = "small_caps"
  apply_liquidity_floor_rule_test = TRUE

  #Expected
  expected_results <- liquidity_m_df_test
  expected_results$liquidity_classification <- c("nano_caps", "micro_caps", "mid_caps", "micro_caps", "small_caps", "mid_caps")
  expected_results$liquidity_floor <- c(0, 0, 1, 0, 1, 1)

  #Check
  expect_error(classify_stock_liquidity(liquidity_floor_cutoffs = liquidity_floor_cutoffs_test, liquidity_m_df = liquidity_m_df_test,
                                        liquidity_floor_rule = liquidity_floor_rule_test, apply_liquidity_floor_rule = apply_liquidity_floor_rule_test,
                                        filter_out_liquidity_floor_rule = FALSE),
               "liquidity metrics orders in liquidity_floor_cutoffs are conflicting"
               )

})

test_that("classify_stock_liquidity throws an error when liquidity_floor_rule not included liquidity_floor_cutoffs", {

  #Create cutoff
  liquidity_floor_cutoffs_test <- data.frame(
    liquidity_classification = c("micro_caps", "small_caps", "mid_caps", "large_caps", "mega_caps"),
    mean_volfin_3m = c(1000, 1000, 1000, 100000, 500000),
    presence = c(99, 99, 100, 100, 100)
  )

  #Create liquidity_m_df_test
  liquidity_m_df_test <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15", "Stock C-2020-05-15", "Stock A-2020-06-15", "Stock B-2020-06-15", "Stock C-2020-06-15"),
    tickers = c("Stock A", "Stock B", "Stock C", "Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2020-05-15", "2020-05-15", "2020-05-15", "2020-06-15", "2020-06-15", "2020-06-15"), format = "%Y-%m-%d"),
    mean_volfin_3m = c(500, 6000, 25500, 1000, 6000, 25500),
    presence = c(95, 98, 100, 100, 100, 100)
  )

  #Floor rule
  liquidity_floor_rule_test = "nano_caps"
  apply_liquidity_floor_rule_test = TRUE

  #Expected
  expected_results <- liquidity_m_df_test
  expected_results$liquidity_classification <- c("nano_caps", "micro_caps", "mid_caps", "micro_caps", "small_caps", "mid_caps")
  expected_results$liquidity_floor <- c(0, 0, 1, 0, 1, 1)

  #Check
  expect_error(classify_stock_liquidity(liquidity_floor_cutoffs = liquidity_floor_cutoffs_test, liquidity_m_df = liquidity_m_df_test,
                                        liquidity_floor_rule = liquidity_floor_rule_test, apply_liquidity_floor_rule = apply_liquidity_floor_rule_test,
                                        filter_out_liquidity_floor_rule = FALSE),
               "liquidity_floor_rule must be contemplated in liquidity_floor_cutoffs"
  )

})

test_that("classify_stock_liquidity throws an error when liquidity_floor_rule is not set and apply_liquidity_floor_rule is TRUE", {

  #Create cutoff
  liquidity_floor_cutoffs_test <- data.frame(
    liquidity_classification = c("micro_caps", "small_caps", "mid_caps", "large_caps", "mega_caps"),
    mean_volfin_3m = c(1000, 1000, 1000, 100000, 500000),
    presence = c(99, 99, 100, 100, 100)
  )

  #Create liquidity_m_df_test
  liquidity_m_df_test <- data.frame(
    id = c("Stock A-2020-05-15", "Stock B-2020-05-15", "Stock C-2020-05-15", "Stock A-2020-06-15", "Stock B-2020-06-15", "Stock C-2020-06-15"),
    tickers = c("Stock A", "Stock B", "Stock C", "Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2020-05-15", "2020-05-15", "2020-05-15", "2020-06-15", "2020-06-15", "2020-06-15"), format = "%Y-%m-%d"),
    mean_volfin_3m = c(500, 6000, 25500, 1000, 6000, 25500),
    presence = c(95, 98, 100, 100, 100, 100)
  )

  #Floor rule
  apply_liquidity_floor_rule_test = TRUE

  #Check
  expect_error(classify_stock_liquidity(liquidity_floor_cutoffs = liquidity_floor_cutoffs_test, liquidity_m_df = liquidity_m_df_test,
                                        liquidity_floor_rule = NULL, apply_liquidity_floor_rule = apply_liquidity_floor_rule_test,
                                        filter_out_liquidity_floor_rule = FALSE),
               "liquidity_floor_rule can't be missing if apply_liquidity_floor_rule is TRUE"
  )

})

test_that("classify_stock_liquidity throws an error when decimals are being set and liquidity_m_df has more than one date", {

  load(paste(test_path(),"/testdata/","artificial_port_obj.RData", sep =""))

  #Floor rule
  liquidity_floor_rule_test = "small_caps"
  apply_liquidity_floor_rule_test = TRUE

  liquidity_floor_cutoffs_test <- data.frame(
    liquidity_classification = c("micro_caps", "small_caps", "mid_caps", "large_caps", "mega_caps"),
    mean_volfin_3m = c(0.1, 0.25, 0.5, 0.75, 0.9),
    presence = c(0.1, 0.25, 0.5, 0.75, 0.9)
  )

  #Check
  expect_error(classify_stock_liquidity(liquidity_floor_cutoffs = liquidity_floor_cutoffs_test, liquidity_m_df = liquidity_m_df,
                                      liquidity_floor_rule = liquidity_floor_rule_test, apply_liquidity_floor_rule = apply_liquidity_floor_rule_test,
                                      filter_out_liquidity_floor_rule = FALSE),
  "For working with decimals, there should be onl one date in liquidity_m_df"
  )

})
