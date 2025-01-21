test_that("Test 1: Identical vectors in chosen_signals_and_positions_list pass", {
  chosen_signals <- list(
    c(a = "long", b = "short"),
    c(a = "long", b = "short")
  )
  features <- c("a", "b")

  features_m_df <- data.frame(id = c("A-2001-05-15", "A-2001-06-15", "B-2001-05-15", "B-2001-06-15"),
                              tickers = c("A", "A", "B", "B"),
                              dates = as.Date(c("2001-05-15", "2001-06-15", "2001-05-15", "2001-06-15")),
                              a = c(1, 2, 3, 4),
                              b = c(5, 6, 7, 8)
                              )

  expect_silent(
    get_features_positions(chosen_signals, features, features_m_df)
  )

})

test_that("Test 2: Mismatch in chosen_signals_and_positions_list throws error", {
  chosen_signals <- list(
    c(a = "long", b = "short"),
    c(a = "long", b = "long") # mismatch
  )
  features <- c("a", "b")

  features_m_df <- data.frame(id = c("A-2001-05-15", "A-2001-06-15", "B-2001-05-15", "B-2001-06-15"),
                              tickers = c("A", "A", "B", "B"),
                              dates = as.Date(c("2001-05-15", "2001-06-15", "2001-05-15", "2001-06-15")),
                              a = c(1, 2, 3, 4),
                              b = c(5, 6, 7, 8)
  )

  expect_error(
    get_features_positions(chosen_signals, features, features_m_df),
    "chosen_signals_and_positions differ at object with position number: 2"
  )
})

test_that("Test 3: Force is treated as long, and no error thrown if consistent", {
  chosen_signals <- list(
    c(a = "long", b = "short", c = "force"),
    c(a = "long", b = "short", c = "force") # identical
  )
  features <- c("a", "b", "c") # c is 'long'

  features_m_df <- data.frame(id = c("A-2001-05-15", "A-2001-06-15", "B-2001-05-15", "B-2001-06-15"),
                              tickers = c("A", "A", "B", "B"),
                              dates = as.Date(c("2001-05-15", "2001-06-15", "2001-05-15", "2001-06-15")),
                              a = c(1, 2, 3, 4),
                              b = c(5, 6, 7, 8),
                              c = c(9, 10, 11, 12)
  )


  expect_equal(
    get_features_positions(chosen_signals, features, features_m_df),
    c(a = "long", b = "short", c = "long")
  )

})

test_that("Test 4: Missing names in chosen_signals cause error", {
  chosen_signals <- list(
    c(a = "long", b = "short")
  )
  # 'c' does not exist in chosen_signals
  features <- c("a", "b", "c")

  features_m_df <- data.frame(id = c("A-2001-05-15", "A-2001-06-15", "B-2001-05-15", "B-2001-06-15"),
                              tickers = c("A", "A", "B", "B"),
                              dates = as.Date(c("2001-05-15", "2001-06-15", "2001-05-15", "2001-06-15")),
                              a = c(1, 2, 3, 4),
                              b = c(5, 6, 7, 8),
                              c = c(9, 10, 11, 12)
  )

  expect_error(
    get_features_positions(chosen_signals, features, features_m_df),
    "features_passthrough must be a subset of chosen_signals_and_positions."
  )
})

test_that("Test 5: Missing names in features_m_df cause error", {
  chosen_signals <- list(
    c(a = "long", b = "short", d = "long")
  )
  # 'c' does not exist in chosen_signals
  features <- c("a", "b", "d")

  features_m_df <- data.frame(id = c("A-2001-05-15", "A-2001-06-15", "B-2001-05-15", "B-2001-06-15"),
                              tickers = c("A", "A", "B", "B"),
                              dates = as.Date(c("2001-05-15", "2001-06-15", "2001-05-15", "2001-06-15")),
                              a = c(1, 2, 3, 4),
                              b = c(5, 6, 7, 8),
                              c = c(9, 10, 11, 12)
  )

  expect_error(
    get_features_positions(chosen_signals, features, features_m_df),
    "features_passthrough must be contained in features_m_df"
  )
})

test_that("Test 6: Single value of 'none' or 'all' work as expected", {
  chosen_signals <- list(
    c(a = "long", b = "long")
  )

  features_m_df <- data.frame(id = c("A-2001-05-15", "A-2001-06-15", "B-2001-05-15", "B-2001-06-15"),
                              tickers = c("A", "A", "B", "B"),
                              dates = as.Date(c("2001-05-15", "2001-06-15", "2001-05-15", "2001-06-15")),
                              a = c(1, 2, 3, 4),
                              b = c(5, 6, 7, 8),
                              c = c(9, 10, 11, 12)
  )


  # Single string 'none'
  expect_equal(
    get_features_positions(
      chosen_signals, "none", features_m_df
    ), "none"
  )
  # Single string 'all'
  expect_equal(
    get_features_positions(
      chosen_signals, "all", features_m_df
    ), c(a = "long", b = "long")
  )
})

test_that("Test 7: Zero-length or minimal length edge cases", {

  # If the list has exactly one vector
  chosen_signals <- list(c(a = "force"))
  features <- c("a") # 'force' is 'long'
  features_m_df <- data.frame(id = c("A-2001-05-15", "A-2001-06-15", "B-2001-05-15", "B-2001-06-15"),
                              tickers = c("A", "A", "B", "B"),
                              dates = as.Date(c("2001-05-15", "2001-06-15", "2001-05-15", "2001-06-15")),
                              a = c(1, 2, 3, 4),
                              b = c(5, 6, 7, 8),
                              c = c(9, 10, 11, 12)
  )

  expect_equal(
    get_features_positions(chosen_signals, features, features_m_df),
    c(a = "long")
    )

})
