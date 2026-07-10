# Display Eligibility Criteria with Colors

This function prints out the eligibility criteria for classifying the
universe based on signals and other custom and user-defined rules, using
colors to facilitate reading. It uses cyan and magenta colors for
headings and yellow to highlight important words.

## Usage

``` r
display_eligibility_criteria()
```

## Value

Prints the colored eligibility criteria to the console.

## Examples

``` r
display_eligibility_criteria()
#> Classify the universe based on signals and other custom and user-defined rules.
#> 
#> The eligibility of a stock/signal portfolio depends on a series of criteria, as explained in Details. Default behavior is to apply only the Only Top Assets Rule, in which case assets are promoted based on their signal being above a given quantile.
#> 
#> The function provides additional custom rules and also accepts user-defined rules.
#> 
#> ## Eligibility Criteria
#> To be promoted as eligible, assets must meet one of the following criteria:
#> 
#> 1. Regular Eligibility
#>    - Only Top Assets Rule: Asset must be in the top quantile as specified by top_quantile.
#>      - To ignore this behavior, set top_quantile to 0.
#>    - Liquidity Floor Rule (exclusive for stocks): must meet minimum liquidity requirements as defined by the liquidity floor rule.
#> 
#> 2. OR Active Weights Constraint Policy Eligibility:
#>    - Maximum Absolute Individual Active Weight Rule: Benchmark weight must exceed the maximum absolute individual active weight threshold.
#> 
#> 3. OR Turnover Policy Eligibility: (exclusive for stocks)
#>    - Stock must be in one of the buffer zones. For this to happen:
#>      - Stock must be in the top quantile buffer (signal >= top_quantile_buffer).
#>      - Stock must be in the pre-rebalancing portfolio.
#>      - Stock must meet the liquidity classification of the buffer zone.
#> 
#> 4. OR user_defined_OR_rules Eligibility
#> 
#> 5. OR Group Representativeness Eligibility:
#>    - If there are no stocks or signal portfolios in one of the groups specified in concentration_constraint_policy, a representative will be included according to the best quantile.
#> 
#> 6. AND user_defined_AND_rules
#> 
#> ## Dominance of Rules
#> - The Active Weights Constraint Policy Eligibility is dominant; assets meeting this rule will always be eligible.
#> - The Turnover Policy Eligibility takes precedence over the Liquidity Floor Rule; thus, a stock in the buffer zone will be included even if the liquidity floor rule suggests otherwise.
#> - Assets that meet user_defined_OR_rules will always be promoted.
#> - Assets that fail to meet user_defined_AND_rules will always be excluded.
```
