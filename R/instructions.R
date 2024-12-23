#' Display Eligibility Criteria with Colors
#'
#' This function prints out the eligibility criteria for classifying the universe based on signals and other custom and user-defined rules, using colors to facilitate reading.
#' It uses cyan and magenta colors for headings and yellow to highlight important words.
#'
#' @return Prints the colored eligibility criteria to the console.
#' @export
#'
#' @examples
#' display_eligibility_criteria()
#'
display_eligibility_criteria <- function(){

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


#' Display Valid Custom Objectives
#'
#' This function prints out the list of valid custom objectives, categorizing them based on the requirements
#' for the `sb_algorithm` slot in the `sb_backtest_config` class. The metrics are needed when the `sb_algorithm`
#' is set to either `sw` or `markowitz`, and some of them are specific to the `active_returns` setting or the `p_correction_method`.
#'
#' Metrics containing either "act_", "info_ratio", or "track_err" are active performance metrics, which are used
#' when the `active_returns` slot of `sb_backtest_config` is TRUE. Metrics with "posterior" relate to a Bayesian
#' correction method when the `p_correction_method` slot is set to `bayesian`. Raw return metrics are used when
#' `active_returns` is FALSE. Frequentist CAPM metrics apply when `p_correction_method` is not `bayesian`.
#'
#' Additionally, the error metrics 'squared_error', 'pseudo_huber_error', or 'absolute_error' are valid only when
#' `sb_algorithm` is either `xgb` or `nn`.
#'
#' @return Prints the valid heuristic SB metrics to the console, categorizing them as needed.
#' @export
#'
#' @examples
#' display_valid_custom_objectives()
#'
display_valid_custom_objectives <- function(){

  # Print the function output with colorful headings and important notes
  cat(
    crayon::cyan$bold("Valid Heuristic SB Metrics List\n\n"),

    crayon::magenta$bold("## Required Metrics for sb_algorithm 'sw' or 'markowitz'\n"),
    "These metrics are needed when the `sb_algorithm` slot in the `sb_backtest_config` class is set to either ",
    crayon::yellow$bold("'sw'"), " or ", crayon::yellow$bold("'markowitz'"), ".\n\n",

    crayon::cyan$bold("### Active Performance Metrics\n"),
    "The following metrics are considered active performance metrics and are used when the `active_returns` slot of the `ss_backtest_config` object is TRUE. These metrics include any metrics containing 'act_', 'info_ratio', or 'track_err'.\n\n",

    crayon::cyan$bold("1. Active Arithmetic Mean Return"),
    "   - ", crayon::yellow$bold("act_arith_mean_ret"), "\n",
    crayon::cyan$bold("2. Active Geometric Mean Return"),
    "   - ", crayon::yellow$bold("act_geom_mean_ret"), "\n",
    crayon::cyan$bold("3. Active Annualized Return"),
    "   - ", crayon::yellow$bold("act_ann_ret"), "\n",
    crayon::cyan$bold("4. Tracking Error"),
    "   - ", crayon::yellow$bold("track_err"), "\n",
    crayon::cyan$bold("5. Annualized Tracking Error"),
    "   - ", crayon::yellow$bold("ann_track_err"), "\n",
    crayon::cyan$bold("6. Active Semi Deviation"),
    "   - ", crayon::yellow$bold("act_semi_dev"), "\n",
    crayon::cyan$bold("7. Active Downside Deviation"),
    "   - ", crayon::yellow$bold("act_down_dev"), "\n",
    crayon::cyan$bold("8. Active Drawdown Deviation"),
    "   - ", crayon::yellow$bold("act_dd_dev"), "\n",
    crayon::cyan$bold("9. Active Downside Frequency"),
    "   - ", crayon::yellow$bold("act_down_freq"), "\n",
    crayon::cyan$bold("10. Active Expected Shortfall"),
    "   - ", crayon::yellow$bold("act_exp_short"), "\n",
    crayon::cyan$bold("11. Active Pain"),
    "   - ", crayon::yellow$bold("act_pain"), "\n",
    crayon::cyan$bold("12. Active Ulcer"),
    "   - ", crayon::yellow$bold("act_ulcer"), "\n",
    crayon::cyan$bold("13. Active Maximum Drawdown"),
    "   - ", crayon::yellow$bold("act_max_dd"), "\n",
    crayon::cyan$bold("14. Active Skewness"),
    "   - ", crayon::yellow$bold("act_skew"), "\n",
    crayon::cyan$bold("15. Active Kurtosis"),
    "   - ", crayon::yellow$bold("act_kurt"), "\n",
    crayon::cyan$bold("16. Information Ratio"),
    "   - ", crayon::yellow$bold("info_ratio"), "\n",
    crayon::cyan$bold("17. Annualized Information Ratio"),
    "   - ", crayon::yellow$bold("ann_info_ratio"), "\n",
    crayon::cyan$bold("18. Information Ratio (Semi Deviation)"),
    "   - ", crayon::yellow$bold("info_ratio_semi_dev"), "\n",
    crayon::cyan$bold("19. Active Sortino Ratio"),
    "   - ", crayon::yellow$bold("act_sortino_ratio"), "\n",
    crayon::cyan$bold("20. Active Annualized Burke Ratio"),
    "   - ", crayon::yellow$bold("act_ann_burke_ratio"), "\n",
    crayon::cyan$bold("21. Active Inverted D-Ratio"),
    "   - ", crayon::yellow$bold("act_inv_d_ratio"), "\n",
    crayon::cyan$bold("22. Information Ratio (Expected Shortfall)"),
    "   - ", crayon::yellow$bold("info_ratio_exp_short"), "\n",
    crayon::cyan$bold("23. Active Annualized Pain Ratio"),
    "   - ", crayon::yellow$bold("act_ann_pain_ratio"), "\n",
    crayon::cyan$bold("24. Active Annualized Martin Ratio"),
    "   - ", crayon::yellow$bold("act_ann_martin_ratio"), "\n",
    crayon::cyan$bold("25. Active Annualized Calmar Ratio"),
    "   - ", crayon::yellow$bold("act_ann_calmar_ratio"), "\n",
    crayon::cyan$bold("26. Active Omega Ratio"),
    "   - ", crayon::yellow$bold("act_omega"), "\n",
    crayon::cyan$bold("27. Active Rachev Ratio"),
    "   - ", crayon::yellow$bold("act_rachev_ratio"), "\n",
    crayon::cyan$bold("28. Active Average Drawdown Recovery"),
    "   - ", crayon::yellow$bold("act_avg_dd_rec"), "\n",
    crayon::cyan$bold("29. Active Average Drawdown Length"),
    "   - ", crayon::yellow$bold("act_avg_dd_length"), "\n",
    crayon::cyan$bold("30. Active Hurst Index"),
    "   - ", crayon::yellow$bold("act_hurst"), "\n",
    crayon::cyan$bold("31. Active Minimum Track Record"),
    "   - ", crayon::yellow$bold("act_min_track_record"), "\n",
    crayon::cyan$bold("32. Probabilistic Info Ratio"),
    "   - ", crayon::yellow$bold("prob_info_ratio"), "\n",
    crayon::cyan$bold("33. Active Modigliani Ratio"),
    "   - ", crayon::yellow$bold("act_modigliani"), "\n",
    crayon::cyan$bold("34. Active Annualized Modigliani Ratio"),
    "   - ", crayon::yellow$bold("act_ann_modigliani"), "\n\n",

    crayon::cyan$bold("### Raw Performance Metrics\n"),
    "The following metrics are used when the `active_returns` slot of the `ss_backtest_config` object is FALSE.\n\n",

    crayon::cyan$bold("1. Arithmetic Mean Return"),
    "   - ", crayon::yellow$bold("arith_mean_ret"), "\n",
    crayon::cyan$bold("2. Geometric Mean Return"),
    "   - ", crayon::yellow$bold("geom_mean_ret"), "\n",
    crayon::cyan$bold("3. Annualized Return"),
    "   - ", crayon::yellow$bold("ann_ret"), "\n",
    crayon::cyan$bold("4. Standard Deviation"),
    "   - ", crayon::yellow$bold("std_dev"), "\n",
    crayon::cyan$bold("5. Annualized Standard Deviation"),
    "   - ", crayon::yellow$bold("ann_std_dev"), "\n",
    crayon::cyan$bold("6. Semi Deviation"),
    "   - ", crayon::yellow$bold("semi_dev"), "\n",
    crayon::cyan$bold("7. Downside Deviation"),
    "   - ", crayon::yellow$bold("down_dev"), "\n",
    crayon::cyan$bold("8. Drawdown Deviation"),
    "   - ", crayon::yellow$bold("dd_dev"), "\n",
    crayon::cyan$bold("9. Downside Frequency"),
    "   - ", crayon::yellow$bold("down_freq"), "\n",
    crayon::cyan$bold("10. Expected Shortfall"),
    "   - ", crayon::yellow$bold("exp_short"), "\n",
    crayon::cyan$bold("11. Pain"),
    "   - ", crayon::yellow$bold("pain"), "\n",
    crayon::cyan$bold("12. Ulcer"),
    "   - ", crayon::yellow$bold("ulcer"), "\n",
    crayon::cyan$bold("13. Maximum Drawdown"),
    "   - ", crayon::yellow$bold("max_dd"), "\n",
    crayon::cyan$bold("14. Skewness"),
    "   - ", crayon::yellow$bold("skew"), "\n",
    crayon::cyan$bold("15. Kurtosis"),
    "   - ", crayon::yellow$bold("kurt"), "\n",
    crayon::cyan$bold("16. Sharpe Ratio"),
    "   - ", crayon::yellow$bold("sharpe_ratio"), "\n",
    crayon::cyan$bold("17. Annualized Sharpe Ratio"),
    "   - ", crayon::yellow$bold("ann_sharpe_ratio"), "\n",
    crayon::cyan$bold("18. Sortino Ratio"),
    "   - ", crayon::yellow$bold("sortino_ratio"), "\n",
    crayon::cyan$bold("19. Annualized Burke Ratio"),
    "   - ", crayon::yellow$bold("ann_burke_ratio"), "\n",
    crayon::cyan$bold("20. Inverted D-Ratio"),
    "   - ", crayon::yellow$bold("inv_d_ratio"), "\n",
    crayon::cyan$bold("21. Omega Ratio"),
    "   - ", crayon::yellow$bold("omega"), "\n",
    crayon::cyan$bold("22. Rachev Ratio"),
    "   - ", crayon::yellow$bold("rachev_ratio"), "\n",
    crayon::cyan$bold("23. Average Drawdown Recovery"),
    "   - ", crayon::yellow$bold("avg_dd_rec"), "\n",
    crayon::cyan$bold("24. Average Drawdown Length"),
    "   - ", crayon::yellow$bold("avg_dd_length"), "\n",
    crayon::cyan$bold("25. Hurst Index"),
    "   - ", crayon::yellow$bold("hurst"), "\n",
    crayon::cyan$bold("26. Minimum Track Record"),
    "   - ", crayon::yellow$bold("min_track_record"), "\n",
    crayon::cyan$bold("27. Modigliani Ratio"),
    "   - ", crayon::yellow$bold("modigliani"), "\n",
    crayon::cyan$bold("28. Annualized Modigliani Ratio"),
    "   - ", crayon::yellow$bold("ann_modigliani"), "\n\n",

    crayon::magenta$bold("### Frequentist CAPM Metrics\n"),
    "The following metrics are used when the `p_correction_method` slot of the `ss_backtest_config` object is NOT `bayesian`.
    Therefore, these metrics include the Bayesian metrics without the 'posterior' prefix.
    When the 'model_stucture' in ss_backtest_config is 'partial_pooled', one can choose between 'theme_alpha', 'individual_alpha',
    'theme_beta' and 'individual_beta'. When the 'model_stucture' in ss_backtest_config is 'no_pooled', one can only choose between
    'alpha' and 'beta'.\n\n",

    crayon::cyan$bold("1. Theme Alpha"),
    "   - ", crayon::yellow$bold("theme_alpha"), "\n",
    crayon::cyan$bold("2. Individual Alpha"),
    "   - ", crayon::yellow$bold("individual_alpha"), "\n",
    crayon::cyan$bold("3. Alpha Standard Error"),
    "   - ", crayon::yellow$bold("alpha_se"), "\n",
    crayon::cyan$bold("4. Theme Beta"),
    "   - ", crayon::yellow$bold("theme_beta"), "\n",
    crayon::cyan$bold("5. Individual Beta"),
    "   - ", crayon::yellow$bold("individual_beta"), "\n",
    crayon::cyan$bold("6. Specific Risk"),
    "   - ", crayon::yellow$bold("specific_risk"), "\n",
    crayon::cyan$bold("7. Alpha T-Statistic"),
    "   - ", crayon::yellow$bold("alpha_t_stat"), "\n",
    crayon::cyan$bold("8. Treynor Ratio"),
    "   - ", crayon::yellow$bold("treynor_ratio"), "\n",
    crayon::cyan$bold("9. Appraisal Ratio"),
    "   - ", crayon::yellow$bold("appraisal_ratio"), "\n\n",

    crayon::magenta$bold("### For no_pooled model_structures, the theme and individual alpha and beta are just 'alpha' and 'beta'. The remaining metrics stay the same: \n"),

    crayon::cyan$bold("10. Alpha"),
    "   - ", crayon::yellow$bold("alpha"), "\n",
    crayon::cyan$bold("11. Beta"),
    "   - ", crayon::yellow$bold("beta"), "\n\n",


    crayon::magenta$bold("### Posterior Metrics (Bayesian Correction Method)\n"),
    "The following metrics are used when the `p_correction_method` slot of the `sb_backtest_config` is set to `bayesian`.\n\n",

    crayon::cyan$bold("1. Posterior Theme Alpha"),
    "   - ", crayon::yellow$bold("posterior_theme_alpha"), "\n",
    crayon::cyan$bold("2. Posterior Individual Alpha"),
    "   - ", crayon::yellow$bold("posterior_individual_alpha"), "\n",
    crayon::cyan$bold("3. Posterior Alpha Standard Error"),
    "   - ", crayon::yellow$bold("posterior_alpha_se"), "\n",
    crayon::cyan$bold("4. Posterior Theme Beta"),
    "   - ", crayon::yellow$bold("posterior_theme_beta"), "\n",
    crayon::cyan$bold("5. Posterior Individual Beta"),
    "   - ", crayon::yellow$bold("posterior_individual_beta"), "\n",
    crayon::cyan$bold("6. Posterior Specific Risk"),
    "   - ", crayon::yellow$bold("posterior_specific_risk"), "\n",
    crayon::cyan$bold("7. Posterior Alpha T-Statistic"),
    "   - ", crayon::yellow$bold("posterior_alpha_t_stat"), "\n",
    crayon::cyan$bold("8. Posterior Treynor Ratio"),
    "   - ", crayon::yellow$bold("posterior_treynor_ratio"), "\n",
    crayon::cyan$bold("9. Posterior Appraisal Ratio"),
    "   - ", crayon::yellow$bold("posterior_appraisal_ratio"), "\n\n",

    crayon::magenta$bold("### Error Metrics for XGBoost or Neural Networks (sb_algorithm = 'xgb' or 'nn')\n"),
    "These error metrics are valid only when the `sb_algorithm` is set to 'xgb' or 'nn'.\n\n",

    "1. Squared Error - ", crayon::yellow$bold("squared_error"), "\n",
    "2. Pseudo-Huber Error - ", crayon::yellow$bold("pseudo_huber_error"), "\n",
    "3. Absolute Error - ", crayon::yellow$bold("absolute_error"), "\n",

    sep = ""
  )
}

