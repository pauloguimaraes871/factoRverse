#' Set Non-Meaningful Characteristics to NA for Financial Firms
#'
#' @description
#' Sets specific financial characteristics to NA for banks and insurance companies,
#' following standard practices in asset pricing research. Certain characteristics
#' that are meaningful for manufacturing and retail firms (e.g., inventory, COGS,
#' PPE, capex) are not applicable to financial institutions.
#'
#' @param meta_dataframe A `meta_dataframe` object containing firm characteristics.
#' @param sector_column A `character` specifying the column name that identifies
#'   sector classification (e.g., "sectors_to_treat_as_banks", "sectors_dynamic").
#' @param bank_values A `character` vector of values in `sector_column` that
#'   identify banks. Default is `c("Bank", "Financial", "Banking")`.
#' @param insurance_values A `character` vector of values in `sector_column` that
#'   identify insurance companies. Default is `c("Insurance", "Insurer")`.
#' @param characteristics A `character` vector of characteristic names to set to NA
#'   for financial firms. If NULL (default), uses a standard list based on asset
#'   pricing literature.
#'
#' @return A modified `meta_dataframe` object with specified characteristics set to NA
#'   for financial firms.
#'
#' @details
#' The function identifies firms classified as banks or insurance companies based on
#' the `sector_column` and sets non-applicable characteristics to NA. This follows
#' the standard practice in asset pricing research (e.g., Fama-French methodology)
#' where certain characteristics are excluded for financial firms.
#'
#' **Default characteristics set to NA for banks and insurers:**
#'
#' - **Inventory-related**: `inventory`, `st_inventory`
#' - **Non-bank COGS**: `cogs_nonbanks_3m`, `cogs_nonbanks_12m`
#' - **Non-bank Sales**: `sales_nonbanks_3m`, `sales_nonbanks_12m`
#' - **Capital Expenditures**: `capex_int_12m`, `capex_int_3m`, `capex_ppe_12m`, `capex_ppe_3m`
#' - **Property, Plant & Equipment**: `ppe`
#' - **SG&A**: `sga_12m`, `sga_3m`
#' - **Depreciation**: `depreciation_12m`, `depreciation_3m`
#'
#' **Characteristics that remain meaningful for banks:**
#'
#' - Bank-specific: `cogs_banks_*`, `sales_banks_*`, `newloans_*`, `int_inc_*`, `serv_rev_*`
#' - Universal metrics: assets, book equity, cash, debt, prices, returns, volatility, beta
#'
#' @references
#' Fama, E. F., & French, K. R. (1992). The cross-section of expected stock returns.
#' The Journal of Finance, 47(2), 427-465.
#'
#' Fama, E. F., & French, K. R. (1993). Common risk factors in the returns on stocks
#' and bonds. Journal of Financial Economics, 33(1), 3-56.
#'
#' @examples
#' \dontrun{
#' # Create sample data
#' features_m_df <- create_meta_dataframe(...)
#'
#' # Set non-meaningful characteristics to NA for banks
#' features_m_df <- set_bank_characteristics_na(
#'   features_m_df,
#'   sector_column = "sectors_to_treat_as_banks",
#'   bank_values = c("Bank", "Financial"),
#'   insurance_values = c("Insurance")
#' )
#'
#' # Custom characteristics list
#' features_m_df <- set_bank_characteristics_na(
#'   features_m_df,
#'   sector_column = "sectors_dynamic",
#'   characteristics = c("inventory", "ppe", "capex_int_12m")
#' )
#' }
#'
#' @export
setGeneric("set_bank_characteristics_na", function(meta_dataframe,
                                                     sector_column,
                                                     bank_values = c("Bank", "Financial", "Banking"),
                                                     insurance_values = c("Insurance", "Insurer"),
                                                     characteristics = NULL) {
  standardGeneric("set_bank_characteristics_na")
})

#' @rdname set_bank_characteristics_na
#' @export
setMethod("set_bank_characteristics_na",
          signature(meta_dataframe = "meta_dataframe",
                    sector_column = "character"),
          function(meta_dataframe,
                   sector_column,
                   bank_values = c("Bank", "Financial", "Banking"),
                   insurance_values = c("Insurance", "Insurer"),
                   characteristics = NULL) {

  # Extract components
  meta_dataframe_name <- meta_dataframe@meta_dataframe_name
  meta_dataframe_current_date <- meta_dataframe@current_date
  meta_dataframe_workflow <- meta_dataframe@workflow
  data <- meta_dataframe@data

  # Validation
  if (!(sector_column %in% colnames(data))) {
    stop("The specified sector_column '", sector_column, "' does not exist in the meta_dataframe.")
  }

  # Define default characteristics if not provided
  if (is.null(characteristics)) {
    characteristics <- c(
      # Inventory-related
      "inventory",
      "st_inventory",
      # Non-bank COGS
      "cogs_nonbanks_3m",
      "cogs_nonbanks_12m",
      # Non-bank Sales
      "sales_nonbanks_3m",
      "sales_nonbanks_12m",
      # Capital Expenditures
      "capex_int_12m",
      "capex_int_3m",
      "capex_ppe_12m",
      "capex_ppe_3m",
      # Property, Plant & Equipment
      "ppe",
      # SG&A
      "sga_12m",
      "sga_3m",
      # Depreciation
      "depreciation_12m",
      "depreciation_3m"
    )
  }

  # Filter to only existing characteristics
  existing_characteristics <- characteristics[characteristics %in% colnames(data)]

  if (length(existing_characteristics) == 0) {
    warning("None of the specified characteristics exist in the meta_dataframe. No changes made.")
    return(meta_dataframe)
  }

  # Identify financial firm rows
  financial_values <- c(bank_values, insurance_values)
  is_financial <- data[[sector_column]] %in% financial_values

  # Set characteristics to NA for financial firms
  for (char in existing_characteristics) {
    data[[char]][is_financial] <- NA
  }

  # Update workflow
  workflow_step <- paste0(
    "set_bank_characteristics_na(",
    "sector_column = '", sector_column, "', ",
    "bank_values = ", paste0("c('", paste(bank_values, collapse = "', '"), "')"), ", ",
    "insurance_values = ", paste0("c('", paste(insurance_values, collapse = "', '"), "')"),
    ")"
  )
  meta_dataframe_workflow <- c(meta_dataframe_workflow, workflow_step)

  # Return updated meta_dataframe
  meta_dataframe@data <- data
  meta_dataframe@workflow <- meta_dataframe_workflow

  return(meta_dataframe)
})
