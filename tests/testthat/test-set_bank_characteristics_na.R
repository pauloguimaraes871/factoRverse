test_that("set_bank_characteristics_na sets correct characteristics to NA for banks", {

  # Create test meta_dataframe with bank and non-bank firms
  features_m_df <- create_meta_dataframe(
    list(
      # inventory
      matrix(c(100, 200, 300,
               150, 250, 350,
               120, 220, 320),
             nrow = 3, ncol = 3),
      # ppe
      matrix(c(1000, 2000, 3000,
               1500, 2500, 3500,
               1200, 2200, 3200),
             nrow = 3, ncol = 3),
      # capex_int_12m
      matrix(c(50, 100, 150,
               60, 110, 160,
               55, 105, 155),
             nrow = 3, ncol = 3),
      # assets (should NOT be set to NA)
      matrix(c(10000, 20000, 30000,
               15000, 25000, 35000,
               12000, 22000, 32000),
             nrow = 3, ncol = 3)
    ),
    tickers = c("Stock A", "Stock B", "Stock C"),
    dates = as.Date(c("2001-03-15", "2001-04-15", "2001-05-15")),
    features_names = c("inventory", "ppe", "capex_int_12m", "assets")
  )

  # Add sector column
  features_m_df@data$sector <- c(
    "Bank", "Bank", "Bank",           # Stock A is a bank
    "Manufacturing", "Manufacturing", "Manufacturing",  # Stock B is manufacturing
    "Financial", "Financial", "Financial"  # Stock C is also financial
  )

  # Apply function
  result_m_df <- set_bank_characteristics_na(
    features_m_df,
    sector_column = "sector",
    bank_values = c("Bank", "Financial"),
    insurance_values = c("Insurance")
  )

  # Extract data
  result_data <- result_m_df@data

  # Check that inventory is NA for banks (Stock A and C) but not for manufacturing (Stock B)
  expect_true(all(is.na(result_data$inventory[result_data$tickers == "Stock A"])))
  expect_true(all(is.na(result_data$inventory[result_data$tickers == "Stock C"])))
  expect_false(any(is.na(result_data$inventory[result_data$tickers == "Stock B"])))

  # Check that ppe is NA for banks but not for manufacturing
  expect_true(all(is.na(result_data$ppe[result_data$tickers == "Stock A"])))
  expect_true(all(is.na(result_data$ppe[result_data$tickers == "Stock C"])))
  expect_false(any(is.na(result_data$ppe[result_data$tickers == "Stock B"])))

  # Check that capex_int_12m is NA for banks but not for manufacturing
  expect_true(all(is.na(result_data$capex_int_12m[result_data$tickers == "Stock A"])))
  expect_true(all(is.na(result_data$capex_int_12m[result_data$tickers == "Stock C"])))
  expect_false(any(is.na(result_data$capex_int_12m[result_data$tickers == "Stock B"])))

  # Check that assets is NOT set to NA (universal metric)
  expect_false(any(is.na(result_data$assets)))

  # Verify original values for manufacturing firm remain unchanged
  stock_b_inventory <- result_data$inventory[result_data$tickers == "Stock B"]
  expect_equal(stock_b_inventory, c(200, 250, 220))

  stock_b_ppe <- result_data$ppe[result_data$tickers == "Stock B"]
  expect_equal(stock_b_ppe, c(2000, 2500, 2200))
})


test_that("set_bank_characteristics_na handles insurance companies", {

  # Create test meta_dataframe
  features_m_df <- create_meta_dataframe(
    list(
      # inventory
      matrix(c(100, 200,
               150, 250),
             nrow = 2, ncol = 2),
      # sga_12m
      matrix(c(50, 100,
               60, 110),
             nrow = 2, ncol = 2)
    ),
    tickers = c("Stock A", "Stock B"),
    dates = as.Date(c("2001-03-15", "2001-04-15")),
    features_names = c("inventory", "sga_12m")
  )

  # Add sector column
  features_m_df@data$sector <- c(
    "Insurance", "Insurance",  # Stock A is insurance
    "Retail", "Retail"  # Stock B is retail
  )

  # Apply function
  result_m_df <- set_bank_characteristics_na(
    features_m_df,
    sector_column = "sector",
    bank_values = c("Bank"),
    insurance_values = c("Insurance", "Insurer")
  )

  # Extract data
  result_data <- result_m_df@data

  # Check that inventory is NA for insurance but not for retail
  expect_true(all(is.na(result_data$inventory[result_data$tickers == "Stock A"])))
  expect_false(any(is.na(result_data$inventory[result_data$tickers == "Stock B"])))

  # Check that sga_12m is NA for insurance but not for retail
  expect_true(all(is.na(result_data$sga_12m[result_data$tickers == "Stock A"])))
  expect_false(any(is.na(result_data$sga_12m[result_data$tickers == "Stock B"])))
})


test_that("set_bank_characteristics_na with custom characteristics list", {

  # Create test meta_dataframe
  features_m_df <- create_meta_dataframe(
    list(
      # inventory
      matrix(c(100, 200,
               150, 250),
             nrow = 2, ncol = 2),
      # ppe
      matrix(c(1000, 2000,
               1500, 2500),
             nrow = 2, ncol = 2),
      # depreciation_12m
      matrix(c(50, 100,
               60, 110),
             nrow = 2, ncol = 2)
    ),
    tickers = c("Stock A", "Stock B"),
    dates = as.Date(c("2001-03-15", "2001-04-15")),
    features_names = c("inventory", "ppe", "depreciation_12m")
  )

  # Add sector column
  features_m_df@data$sector <- c(
    "Bank", "Bank",  # Stock A is a bank
    "Manufacturing", "Manufacturing"  # Stock B is manufacturing
  )

  # Apply function with only inventory in custom characteristics
  result_m_df <- set_bank_characteristics_na(
    features_m_df,
    sector_column = "sector",
    bank_values = c("Bank"),
    characteristics = c("inventory")  # Only set inventory to NA
  )

  # Extract data
  result_data <- result_m_df@data

  # Check that only inventory is NA for banks
  expect_true(all(is.na(result_data$inventory[result_data$tickers == "Stock A"])))
  # ppe and depreciation_12m should NOT be NA since they're not in custom list
  expect_false(any(is.na(result_data$ppe[result_data$tickers == "Stock A"])))
  expect_false(any(is.na(result_data$depreciation_12m[result_data$tickers == "Stock A"])))
})


test_that("set_bank_characteristics_na handles non-existent characteristics gracefully", {

  # Create test meta_dataframe
  features_m_df <- create_meta_dataframe(
    list(
      # assets
      matrix(c(10000, 20000,
               15000, 25000),
             nrow = 2, ncol = 2)
    ),
    tickers = c("Stock A", "Stock B"),
    dates = as.Date(c("2001-03-15", "2001-04-15")),
    features_names = c("assets")
  )

  # Add sector column
  features_m_df@data$sector <- c("Bank", "Bank", "Manufacturing", "Manufacturing")

  # Apply function with non-existent characteristics
  expect_warning(
    result_m_df <- set_bank_characteristics_na(
      features_m_df,
      sector_column = "sector",
      characteristics = c("inventory", "ppe", "nonexistent_column")
    ),
    "None of the specified characteristics exist"
  )

  # Data should be unchanged
  expect_equal(result_m_df@data$assets, features_m_df@data$assets)
})


test_that("set_bank_characteristics_na errors on invalid sector_column", {

  # Create test meta_dataframe
  features_m_df <- create_meta_dataframe(
    list(
      # inventory
      matrix(c(100, 200),
             nrow = 2, ncol = 1)
    ),
    tickers = c("Stock A", "Stock B"),
    dates = as.Date(c("2001-03-15")),
    features_names = c("inventory")
  )

  # Expect error when sector_column doesn't exist
  expect_error(
    set_bank_characteristics_na(
      features_m_df,
      sector_column = "nonexistent_sector",
      bank_values = c("Bank")
    ),
    "does not exist in the meta_dataframe"
  )
})


test_that("set_bank_characteristics_na updates workflow correctly", {

  # Create test meta_dataframe
  features_m_df <- create_meta_dataframe(
    list(
      # inventory
      matrix(c(100, 200),
             nrow = 2, ncol = 1)
    ),
    tickers = c("Stock A", "Stock B"),
    dates = as.Date(c("2001-03-15")),
    features_names = c("inventory")
  )

  # Add sector column
  features_m_df@data$sector <- c("Bank", "Manufacturing")

  # Apply function
  result_m_df <- set_bank_characteristics_na(
    features_m_df,
    sector_column = "sector",
    bank_values = c("Bank", "Financial"),
    insurance_values = c("Insurance")
  )

  # Check that workflow was updated
  expect_true(length(result_m_df@workflow) > length(features_m_df@workflow))
  expect_true(any(grepl("set_bank_characteristics_na", result_m_df@workflow)))
})


test_that("set_bank_characteristics_na handles all default characteristics", {

  # Create test meta_dataframe with all default characteristics
  default_chars <- c(
    "inventory", "st_inventory",
    "cogs_nonbanks_3m", "cogs_nonbanks_12m",
    "sales_nonbanks_3m", "sales_nonbanks_12m",
    "capex_int_12m", "capex_int_3m", "capex_ppe_12m", "capex_ppe_3m",
    "ppe", "sga_12m", "sga_3m",
    "depreciation_12m", "depreciation_3m"
  )

  # Create matrices for each characteristic
  matrices <- lapply(seq_along(default_chars), function(i) {
    matrix(c(i*100, i*200,
             i*150, i*250),
           nrow = 2, ncol = 2)
  })

  features_m_df <- create_meta_dataframe(
    matrices,
    tickers = c("Stock A", "Stock B"),
    dates = as.Date(c("2001-03-15", "2001-04-15")),
    features_names = default_chars
  )

  # Add sector column
  features_m_df@data$sector <- c(
    "Bank", "Bank",  # Stock A is a bank
    "Manufacturing", "Manufacturing"  # Stock B is manufacturing
  )

  # Apply function with defaults
  result_m_df <- set_bank_characteristics_na(
    features_m_df,
    sector_column = "sector",
    bank_values = c("Bank")
  )

  # Check that all default characteristics are NA for banks
  result_data <- result_m_df@data
  for (char in default_chars) {
    expect_true(
      all(is.na(result_data[[char]][result_data$tickers == "Stock A"])),
      info = paste("Characteristic", char, "should be NA for banks")
    )
    expect_false(
      any(is.na(result_data[[char]][result_data$tickers == "Stock B"])),
      info = paste("Characteristic", char, "should NOT be NA for manufacturing")
    )
  }
})
