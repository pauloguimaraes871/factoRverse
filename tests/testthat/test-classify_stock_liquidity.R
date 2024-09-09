test_that("classify_stock_liquidity adequately classifies stocks and applies liquidity_floor_rule", {

  #Create cutoff
  liquidity_floor_cutoffs_list_test <- list(
    micro_caps = c(mean_volfin_3m = 1000, presence = 97.5),
    small_caps = c(mean_volfin_3m = 5000, presence = 99),
    mid_caps = c(mean_volfin_3m = 25000, presence = 100),
    large_caps = c(mean_volfin_3m = 100000, presence = 100),
    mega_caps = c(mean_volfin_3m = 500000, presence = 100)
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
  expect_equal(classify_stock_liquidity(liquidity_floor_cutoffs_list = liquidity_floor_cutoffs_list_test, liquidity_m_df = liquidity_m_df_test,
                                        liquidity_floor_rule = liquidity_floor_rule_test, apply_liquidity_floor_rule = apply_liquidity_floor_rule_test,
                                        filter_out_liquidity_floor_rule = FALSE),
               expected_results)

})

test_that("classify_stock_liquidity adequately classifies stocks and applies liquidity_floor_rule - 2", {

  load(paste(test_path(),"/testdata/","artificial_metabacktest_obj.RData", sep =""))

  #Floor rule
  liquidity_floor_rule_test = "small_caps"
  apply_liquidity_floor_rule_test = TRUE

  #Expected
  expected_results <- liquidity_m_df[which(liquidity_m_df$dates == "2001-07-15"),]
  expected_results$liquidity_classification <- c("mid_caps", "micro_caps", "mid_caps", "mid_caps")
  expected_results$liquidity_floor <- c(1, 0, 1, 1)

  #Check
  expect_equal(classify_stock_liquidity(liquidity_floor_cutoffs_list = liquidity_floor_cutoffs_list, liquidity_m_df = liquidity_m_df[which(liquidity_m_df$dates == "2001-07-15"),],
                                        liquidity_floor_rule = liquidity_floor_rule_test, apply_liquidity_floor_rule = apply_liquidity_floor_rule_test,
                                        filter_out_liquidity_floor_rule = FALSE),
               expected_results)

})

test_that("classify_stock_liquidity adequately classifies stocks and applies liquidity_floor_rule when setting decimals", {

  load(paste(test_path(),"/testdata/","artificial_metabacktest_obj.RData", sep =""))

  #Floor rule
  liquidity_floor_rule_test = "small_caps"
  apply_liquidity_floor_rule_test = TRUE

  #Create cutoff
  liquidity_floor_cutoffs_list_test <- list(
    micro_caps = c(mean_volfin_3m = 0.1, presence = 0.1),
    small_caps = c(mean_volfin_3m = 0.25, presence = 0.25),
    mid_caps = c(mean_volfin_3m = 0.50, presence = 0.5),
    large_caps = c(mean_volfin_3m = 0.75, presence = 0.75),
    mega_caps = c(mean_volfin_3m = 0.90, presence = 0.90)
  )

  liquidity_floor_cutoff_list_transformed_meanvolfin3m <-
    pmap(list(lapply(liquidity_floor_cutoffs_list_test, function(x) x[1])), function(x) quantile(liquidity_m_df[which(liquidity_m_df$dates == "2001-07-15"),]$mean_volfin_3m, probs = x)) %>% unlist()

  liquidity_floor_cutoff_list_transformed_presence <-
    pmap(list(lapply(liquidity_floor_cutoffs_list_test, function(x) x[2])), function(x) quantile(liquidity_m_df[which(liquidity_m_df$dates == "2001-07-15"),]$presence, probs = x)) %>% unlist()


  #Expected
  expected_results <- liquidity_m_df[which(liquidity_m_df$dates == "2001-07-15"),]
  expected_results$liquidity_classification <- c("nano_caps", "nano_caps", "mid_caps", "small_caps")
  expected_results$liquidity_floor <- c(0, 0, 1, 1)

  #Check
  expect_equal(
    suppressWarnings(classify_stock_liquidity(liquidity_floor_cutoffs_list = liquidity_floor_cutoffs_list_test, liquidity_m_df = liquidity_m_df[which(liquidity_m_df$dates == "2001-07-15"),],
                                        liquidity_floor_rule = liquidity_floor_rule_test, apply_liquidity_floor_rule = apply_liquidity_floor_rule_test,
                                        filter_out_liquidity_floor_rule = FALSE)),
               expected_results)

})

test_that("classify_stock_liquidity only set quantiles when both metrics are set as decimals", {

  load(paste(test_path(),"/testdata/","artificial_metabacktest_obj.RData", sep =""))

  #Floor rule
  liquidity_floor_rule_test = "small_caps"
  apply_liquidity_floor_rule_test = TRUE

  #Create cutoff
  liquidity_floor_cutoffs_list_test <- list(
    micro_caps = c(mean_volfin_3m = 0.1, presence = 10),
    small_caps = c(mean_volfin_3m = 0.25, presence = 25),
    mid_caps = c(mean_volfin_3m = 0.50, presence = 50),
    large_caps = c(mean_volfin_3m = 0.75, presence = 75),
    mega_caps = c(mean_volfin_3m = 0.90, presence = 90)
  )


  #Check
  expect_no_warning(classify_stock_liquidity(liquidity_floor_cutoffs_list = liquidity_floor_cutoffs_list_test, liquidity_m_df = liquidity_m_df[which(liquidity_m_df$dates == "2001-07-15"),],
                                              liquidity_floor_rule = liquidity_floor_rule_test, apply_liquidity_floor_rule = apply_liquidity_floor_rule_test,
                                              filter_out_liquidity_floor_rule = FALSE))

})

test_that("classify_stock_liquidity adequately classifies stocks and applies liquidity_floor_rule when there are conflicting metrics", {

  #Create cutoff
  liquidity_floor_cutoffs_list_test <- list(
    micro_caps = c(mean_volfin_3m = 1000, presence = 97.5),
    small_caps = c(mean_volfin_3m = 5000, presence = 99),
    mid_caps = c(mean_volfin_3m = 25000, presence = 100),
    large_caps = c(mean_volfin_3m = 100000, presence = 100),
    mega_caps = c(mean_volfin_3m = 500000, presence = 100)
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
  expect_equal(classify_stock_liquidity(liquidity_floor_cutoffs_list = liquidity_floor_cutoffs_list_test, liquidity_m_df = liquidity_m_df_test,
                                        liquidity_floor_rule = liquidity_floor_rule_test, apply_liquidity_floor_rule = apply_liquidity_floor_rule_test,
                                        filter_out_liquidity_floor_rule = FALSE),
               expected_results)

})

test_that("classify_stock_liquidity adequately classifies stocks, applies liquidity_floor_rule and filters out stocks under liquidity_floor_rule", {

  #Create cutoff
  liquidity_floor_cutoffs_list_test <- list(
    micro_caps = c(mean_volfin_3m = 1000, presence = 97.5),
    small_caps = c(mean_volfin_3m = 5000, presence = 99),
    mid_caps = c(mean_volfin_3m = 25000, presence = 100),
    large_caps = c(mean_volfin_3m = 100000, presence = 100),
    mega_caps = c(mean_volfin_3m = 500000, presence = 100)
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
  expect_equal(classify_stock_liquidity(liquidity_floor_cutoffs_list = liquidity_floor_cutoffs_list_test, liquidity_m_df = liquidity_m_df_test,
                                        liquidity_floor_rule = liquidity_floor_rule_test, apply_liquidity_floor_rule = apply_liquidity_floor_rule_test,
                                        filter_out_liquidity_floor_rule = TRUE, verbose = TRUE),
               expected_results)

})

test_that("classify_stock_liquidity throws an error when liquidity_floor_cutoffs_list_test not in correct order", {

  #Create cutoff
  liquidity_floor_cutoffs_list_test <- list(
    mega_caps = c(mean_volfin_3m = 500000, presence = 100),
    large_caps = c(mean_volfin_3m = 100000, presence = 100),
    mid_caps = c(mean_volfin_3m = 25000, presence = 100),
    small_caps = c(mean_volfin_3m = 5000, presence = 99),
    micro_caps = c(mean_volfin_3m = 1000, presence = 97.5)
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
  expect_error(classify_stock_liquidity(liquidity_floor_cutoffs_list = liquidity_floor_cutoffs_list_test, liquidity_m_df = liquidity_m_df_test,
                                        liquidity_floor_rule = liquidity_floor_rule_test, apply_liquidity_floor_rule = apply_liquidity_floor_rule_test,
                                        filter_out_liquidity_floor_rule = FALSE),
               "liquidity_floor_cutoffs_list is not in ascending order")

})

test_that("classify_stock_liquidity throws an error when liquidity_floor_cutoffs_list_test orders are conflicting", {

  #Create cutoff
  liquidity_floor_cutoffs_list_test <- list(
    micro_caps = c(mean_volfin_3m = 1000, presence = 99),
    small_caps = c(mean_volfin_3m = 5000, presence = 97.5),
    mid_caps = c(mean_volfin_3m = 25000, presence = 100),
    large_caps = c(mean_volfin_3m = 100000, presence = 100),
    mega_caps = c(mean_volfin_3m = 500000, presence = 100)
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
  expect_error(classify_stock_liquidity(liquidity_floor_cutoffs_list = liquidity_floor_cutoffs_list_test, liquidity_m_df = liquidity_m_df_test,
                                        liquidity_floor_rule = liquidity_floor_rule_test, apply_liquidity_floor_rule = apply_liquidity_floor_rule_test,
                                        filter_out_liquidity_floor_rule = FALSE),
               "liquidity metrics orders in liquidity_floor_cutoffs_list are conflicting"
               )

})

test_that("classify_stock_liquidity throws an error when liquidity_floor_rule not included liquidity_floor_cutoffs_list", {

  #Create cutoff
  liquidity_floor_cutoffs_list_test <- list(
    micro_caps = c(mean_volfin_3m = 1000, presence = 99),
    small_caps = c(mean_volfin_3m = 1000, presence = 99),
    mid_caps = c(mean_volfin_3m = 1000, presence = 100),
    large_caps = c(mean_volfin_3m = 100000, presence = 100),
    mega_caps = c(mean_volfin_3m = 500000, presence = 100)
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
  expect_error(classify_stock_liquidity(liquidity_floor_cutoffs_list = liquidity_floor_cutoffs_list_test, liquidity_m_df = liquidity_m_df_test,
                                        liquidity_floor_rule = liquidity_floor_rule_test, apply_liquidity_floor_rule = apply_liquidity_floor_rule_test,
                                        filter_out_liquidity_floor_rule = FALSE),
               "liquidity_floor_rule not included in liquidity_floor_cutoffs_list"
  )

})

test_that("classify_stock_liquidity throws an error when liquidity_floor_rule is not set and apply_liquidity_floor_rule is TRUE", {

  #Create cutoff
  liquidity_floor_cutoffs_list_test <- list(
    micro_caps = c(mean_volfin_3m = 1000, presence = 99),
    small_caps = c(mean_volfin_3m = 1000, presence = 99),
    mid_caps = c(mean_volfin_3m = 1000, presence = 100),
    large_caps = c(mean_volfin_3m = 100000, presence = 100),
    mega_caps = c(mean_volfin_3m = 500000, presence = 100)
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

  #Expected
  expected_results <- liquidity_m_df_test
  expected_results$liquidity_classification <- c("nano_caps", "micro_caps", "mid_caps", "micro_caps", "small_caps", "mid_caps")
  expected_results$liquidity_floor <- c(0, 0, 1, 0, 1, 1)

  #Check
  expect_error(classify_stock_liquidity(liquidity_floor_cutoffs_list = liquidity_floor_cutoffs_list_test, liquidity_m_df = liquidity_m_df_test,
                                        liquidity_floor_rule = NULL, apply_liquidity_floor_rule = apply_liquidity_floor_rule_test,
                                        filter_out_liquidity_floor_rule = FALSE),
               "liquidity_floor_rule can't be missing if apply_liquidity_floor_rule is TRUE"
  )

})
