# Bank Characteristics Treatment in Factor Portfolio Construction

## Overview

This document explains the rationale for setting certain firm characteristics to NA for banks and insurance companies in factor portfolio construction and machine learning applications for cross-sectional return prediction.

## Research Background

In asset pricing research, particularly following the seminal work of Fama and French (1992, 1993), it is standard practice to exclude or differently treat financial firms (banks, insurance companies) when computing certain firm characteristics. This is because the financial statements and business models of financial institutions differ fundamentally from those of non-financial firms.

## Characteristics Not Meaningful for Banks

The following characteristics should be set to NA for banks and insurance companies:

### 1. Inventory-Related Metrics
- **Variables**: `inventory`, `st_inventory`
- **Rationale**: Banks do not hold inventory in the traditional manufacturing/retail sense. Their "inventory" would be loans and securities, which are already captured in their asset composition.

### 2. Cost of Goods Sold (COGS) - Non-Bank
- **Variables**: `cogs_nonbanks_3m`, `cogs_nonbanks_12m`
- **Rationale**: Banks do not have traditional COGS. Their primary costs are interest expenses and operating costs, not the cost of producing goods.

### 3. Sales (Non-Bank)
- **Variables**: `sales_nonbanks_3m`, `sales_nonbanks_12m`
- **Rationale**: Banks do not have traditional sales revenue. Their revenue comes from interest income, fee income, and service charges, which should be captured in bank-specific variables.

### 4. Capital Expenditures (Capex)
- **Variables**: `capex_int_12m`, `capex_int_3m`, `capex_ppe_12m`, `capex_ppe_3m`
- **Rationale**: Capital intensity metrics are less meaningful for banks, whose assets are primarily financial rather than physical. Banks invest in loans and securities, not machinery and equipment.

### 5. Property, Plant & Equipment (PPE)
- **Variables**: `ppe`
- **Rationale**: Banks have minimal physical assets compared to manufacturing firms. Their asset base consists primarily of loans, securities, and other financial instruments. PPE as a percentage of total assets is typically very small and not a meaningful characteristic.

### 6. Selling, General & Administrative Expenses (SG&A)
- **Variables**: `sga_12m`, `sga_3m`
- **Rationale**: Banks do not report SG&A in the same format as non-financial firms. Their operating expenses are structured differently and are better captured through bank-specific metrics.

### 7. Depreciation
- **Variables**: `depreciation_12m`, `depreciation_3m`
- **Rationale**: Depreciation is not a significant expense for banks since they hold few depreciable assets. This metric is more relevant for capital-intensive industries.

## Characteristics That ARE Meaningful for Banks

The following characteristics should be retained and are meaningful for banks:

### Bank-Specific Metrics
- `cogs_banks_3m`, `cogs_banks_12m`: Bank-specific cost measures
- `sales_banks_3m`, `sales_banks_12m`: Bank-specific revenue measures
- `newloans_12m`, `newloans_3m`: New loan origination (growth indicator)
- `int_inc_12m`, `int_inc_3m`: Interest income
- `fin_int_12m`, `fin_int_3m`: Financial interest expenses
- `serv_rev_12m`, `serv_rev_3m`: Service revenue (fee income)

### Universal Financial Metrics
These are meaningful for both banks and non-banks:
- **Balance Sheet**: `assets`, `be` (book equity), `cash`, `debt` measures
- **Market Data**: `price`, `price_adj`, returns (`ret_*`), volatility (`vol_*`)
- **Risk Metrics**: `beta_mrkt_*`, `beta_inf`, `beta_ry_*`, `beta_usd_*`
- **Profitability**: `net_inc_*`, `ebit_*`, `ebitda_*`
- **Valuation**: `mkt_cap`, `ev`, `ev_not_adj`
- **Liquidity**: `presence_1m`, `qtt_*`, trading volume
- **Other**: `dps_*`, `retention_rate`, weights in indices

## Implementation

The `set_bank_characteristics_na()` function implements this standard treatment:

```r
# Apply standard treatment
features_m_df <- set_bank_characteristics_na(
  features_m_df,
  sector_column = "sectors_to_treat_as_banks",
  bank_values = c("Bank", "Financial", "Banking"),
  insurance_values = c("Insurance", "Insurer")
)
```

### Custom Treatment

For custom analysis, you can specify exactly which characteristics to set to NA:

```r
# Custom list of characteristics
features_m_df <- set_bank_characteristics_na(
  features_m_df,
  sector_column = "sectors_dynamic",
  characteristics = c("inventory", "ppe", "capex_int_12m")
)
```

## Best Practices

1. **Apply Early in Pipeline**: Set bank characteristics to NA early in your data preprocessing pipeline, before computing ratios or performing imputation.

2. **Sector Classification**: Ensure your sector classification correctly identifies financial institutions. The function supports custom sector values through the `bank_values` and `insurance_values` parameters.

3. **Ratio Calculations**: When computing ratios (e.g., `capex/assets`), computing them after setting inappropriate characteristics to NA will naturally propagate the NA, avoiding spurious ratios.

4. **Machine Learning**: For ML models, consider:
   - Using bank-specific features for financial firms
   - Training separate models for financial vs non-financial firms
   - Using sector dummy variables to allow models to learn sector-specific patterns

5. **Factor Construction**: When constructing factor portfolios:
   - Rank firms within industry groups when possible
   - Consider excluding financial firms entirely from certain factors (e.g., asset growth, investment factors)
   - Use bank-specific factors for financial firms

## Academic References

1. **Fama, E. F., & French, K. R. (1992).** "The cross-section of expected stock returns." 
   *The Journal of Finance*, 47(2), 427-465.

2. **Fama, E. F., & French, K. R. (1993).** "Common risk factors in the returns on stocks and bonds." 
   *Journal of Financial Economics*, 33(1), 3-56.

3. **Fama, E. F., & French, K. R. (2015).** "A five-factor asset pricing model." 
   *Journal of Financial Economics*, 116(1), 1-22.

4. **Hou, K., Xue, C., & Zhang, L. (2015).** "Digesting anomalies: An investment approach." 
   *Review of Financial Studies*, 28(3), 650-705.

## Conclusion

Properly handling bank characteristics is crucial for:
1. Avoiding misleading factor exposures
2. Preventing data quality issues in ML models
3. Following established academic practices
4. Ensuring comparability with published research

The `set_bank_characteristics_na()` function provides a standardized, research-backed approach to this important data preprocessing step.
