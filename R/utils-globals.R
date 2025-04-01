#' Global variables
utils::globalVariables(c("id", "tickers", "untraded", "delisted", "listed", "old",
                         "old_tickers_first_quote", "old_tickers_last_quote", "new_tickers_first_quote", "new_tickers_last_quote",
                         "tickers_first_quote", "tickers_last_quote",
                         "perm_id", "dates", "new_ticker", "old_ticker", "old_perm_id", "change_date",
                         "has_not_na", "count", "untrd_not_only_NA_TRUE", "untrd_not_only_NA_FALSE",
                         "out_trd_rg_not_only_NA_FALSE", "out_trd_rg_not_only_NA_TRUE", ":=",
                         "pred", "updated_port_weights", "is_eligible", "total_cost", "n_assets",
                         "fwd_return_1m", "liquidity_classification", "exp_ret_score", "individual_alpha",
                         ".", "bop_port_weights", "is_in_buffered_quantile_range", "was_in_old_portfolio",
                         "does_liquidity_meets_turnover_cap_rule", "turnover_cap_rule", "liquidity_floor",
                         "rel_risk_contr", "max_weight", "min_weight", "cap_score", "pre_eligible_assets",
                         "theme", "theme_sb", "row_id", "period_return", "eop_port_weights", "daily_vol",
                         "obs", "delta", "relative_order_size", "theme_id", "market_factor_proxy", "t value",
                         "b_Intercept", "r_theme", "theme_term", "r_theme:tickers", "tickers_term",
                         "r_theme:tickers", "fixed_effect_name", "theme_fixed_effect", "fixed_effect_value",
                         "posterior_theme_alpha", "posterior_theme_alpha.lower",  "posterior_theme_alpha.upper",
                         "posterior_individual_alpha", "posterior_individual_alpha.lower", "posterior_individual_alpha.upper",
                         "b_market_factor_proxy", "posterior_theme_beta", "posterior_theme_beta.lower", "posterior_theme_beta.upper",
                         "posterior_individual_beta", "posterior_individual_beta.lower",  "posterior_individual_beta.upper",
                         "sd_theme__Intercept", "sd_theme:tickers__Intercept", "sd_theme:tickers__market_factor_proxy",
                         "cor_theme:tickers__Intercept__market_factor_proxy", ".width", ".point", ".interval", ".prediction",
                         "theme_tickers", "posterior_geom_mean_ret", "posterior_specific_risk", "sum_w",
                         "squared_error", "pseudo_huber_error", "quantile_error", "early_stopping_rounds",
                         "objective", "Var1", "Var2"



))
