#' Display Eligibility Criteria with Colors
#'
#' This function prints out the eligibility criteria for classifying the universe based on signals and other custom and user-defined rules, using colors to facilitate reading.
#' It uses cyan and magenta colors for headings and yellow to highlight important words.
#'
#' @return Prints the colored eligibility criteria to the console.
#' @export
#'
#' @examples
#' eligibility_criteria()
#'
eligibility_criteria <- function(){
  # Ensure the crayon package is available
  if (!requireNamespace("crayon", quietly = TRUE)) {
    stop("The 'crayon' package is required but is not installed. Please install it using install.packages('crayon').")
  }

  cat(
    crayon::cyan$bold("Classify the universe based on signals and other custom and user-defined rules.\n\n"),

    "The eligibility of a stock/signal portfolio depends on a series of criteria, as explained in ",
    crayon::yellow$bold("Details"), ". Default behavior is to apply only the ",
    crayon::yellow$bold("Only Top Assets Rule"), ", in which case assets are promoted based on their signal being above a given quantile.\n\n",

    "The function provides additional custom rules and also accepts user-defined rules.\n\n",

    crayon::magenta$bold("## Eligibility Criteria\n"),
    "To be promoted as eligible, assets must meet one of the following criteria:\n\n",

    crayon::cyan$bold("1. Regular Eligibility\n"),
    "   - ", crayon::yellow$bold("Only Top Assets Rule"), ": Asset must be in the top quantile as specified by ", crayon::yellow("top_quantile"), ".\n",
    "     - To ignore this behavior, set ", crayon::yellow("top_quantile"), " to 0.\n",
    "   - ", crayon::yellow$bold("Liquidity Floor Rule"), " (exclusive for stocks): must meet minimum liquidity requirements as defined by the liquidity floor rule.\n\n",

    crayon::cyan$bold("2. OR Active Weights Constraint Policy Eligibility:\n"),
    "   - ", crayon::yellow$bold("Maximum Absolute Individual Active Weight Rule"), ": Benchmark weight must exceed the maximum absolute individual active weight threshold.\n\n",

    crayon::cyan$bold("3. OR Turnover Policy Eligibility: (exclusive for stocks)\n"),
    "   - Stock must be in one of the buffer zones. For this to happen:\n",
    "     - Stock must be in the top quantile buffer (", crayon::yellow("signal >= top_quantile_buffer"), ").\n",
    "     - Stock must be in the pre-rebalancing portfolio.\n",
    "     - Stock must meet the liquidity classification of the buffer zone.\n\n",

    crayon::cyan$bold("4. OR user_defined_OR_rules Eligibility\n\n"),

    crayon::cyan$bold("5. OR Group Representativeness Eligibility:\n"),
    "   - If there are no stocks or signal portfolios in one of the groups specified in ", crayon::yellow("concentration_constraint_policy"), ", a representative will be included according to the best quantile.\n\n",

    crayon::cyan$bold("6. AND user_defined_AND_rules\n\n"),

    crayon::magenta$bold("## Dominance of Rules\n"),
    "- The ", crayon::yellow$bold("Active Weights Constraint Policy Eligibility"), " is dominant; assets meeting this rule will always be eligible.\n",
    "- The ", crayon::yellow$bold("Turnover Policy Eligibility"), " takes precedence over the ", crayon::yellow$bold("Liquidity Floor Rule"), "; thus, a stock in the buffer zone will be included even if the liquidity floor rule suggests otherwise.\n",
    "- Assets that meet ", crayon::yellow$bold("user_defined_OR_rules"), " will always be promoted.\n",
    "- Assets that fail to meet ", crayon::yellow$bold("user_defined_AND_rules"), " will always be excluded.\n",

    sep = ""
  )
}

