test_that("classify_stock_liquidity throws an error when liquidity_floor_cutoffs_test not in correct order", {

  #Create cutoff
  liquidity_floor_cutoffs_test <- list(
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
  expect_error(classify_stock_liquidity(liquidity_floor_cutoffs = liquidity_floor_cutoffs_test, liquidity_m_df = liquidity_m_df_test,
                                        liquidity_floor_rule = liquidity_floor_rule_test, apply_liquidity_floor_rule = apply_liquidity_floor_rule_test,
                                        filter_out_liquidity_floor_rule = FALSE),
               "liquidity_floor_cutoffs is not in ascending order")

})
