# Display Valid Custom Objectives

This function prints out the list of valid custom objectives,
categorizing them based on the requirements for the `sb_algorithm` slot
in the `sb_backtest_config` class. The metrics are needed when the
`sb_algorithm` is set to either `sw` or `markowitz`, and some of them
are specific to the `active_returns` setting or the
`p_correction_method`.

## Usage

``` r
display_valid_custom_objectives()
```

## Value

Prints the valid heuristic SB metrics to the console, categorizing them
as needed.

## Details

Metrics containing either "act\_", "info_ratio", or "track_err" are
active performance metrics, which are used when the `active_returns`
slot of `sb_backtest_config` is TRUE. Metrics with "posterior" relate to
a Bayesian correction method when the `p_correction_method` slot is set
to `bayesian`. Raw return metrics are used when `active_returns` is
FALSE. Frequentist CAPM metrics apply when `p_correction_method` is not
`bayesian`.

Additionally, the error metrics 'squared_error', 'pseudo_huber_error',
or 'absolute_error' are valid only when `sb_algorithm` is either `xgb`
or `nn`.

## Examples

``` r
display_valid_custom_objectives()
#> Valid Heuristic SB Metrics List
#> 
#> ## Required Metrics for sb_algorithm 'sw' or 'markowitz'
#> These metrics are needed when the `sb_algorithm` slot in the `sb_backtest_config` class is set to either 'sw' or 'markowitz'.
#> 
#> ### Active Performance Metrics
#> The following metrics are considered active performance metrics and are used when the `active_returns` slot of the `ss_backtest_config` object is TRUE. These metrics include any metrics containing 'act_', 'info_ratio', or 'track_err'.
#> 
#> 1. Active Arithmetic Mean Return   - act_arith_mean_ret
#> 2. Active Geometric Mean Return   - act_geom_mean_ret
#> 3. Active Annualized Return   - act_ann_ret
#> 4. Tracking Error   - track_err
#> 5. Annualized Tracking Error   - ann_track_err
#> 6. Active Semi Deviation   - act_semi_dev
#> 7. Active Downside Deviation   - act_down_dev
#> 8. Active Drawdown Deviation   - act_dd_dev
#> 9. Active Downside Frequency   - act_down_freq
#> 10. Active Expected Shortfall   - act_exp_short
#> 11. Active Pain   - act_pain
#> 12. Active Ulcer   - act_ulcer
#> 13. Active Maximum Drawdown   - act_max_dd
#> 14. Active Skewness   - act_skew
#> 15. Active Kurtosis   - act_kurt
#> 16. Information Ratio   - info_ratio
#> 17. Annualized Information Ratio   - ann_info_ratio
#> 18. Information Ratio (Semi Deviation)   - info_ratio_semi_dev
#> 19. Active Sortino Ratio   - act_sortino_ratio
#> 20. Active Annualized Burke Ratio   - act_ann_burke_ratio
#> 21. Active Inverted D-Ratio   - act_inv_d_ratio
#> 22. Information Ratio (Expected Shortfall)   - info_ratio_exp_short
#> 23. Active Annualized Pain Ratio   - act_ann_pain_ratio
#> 24. Active Annualized Martin Ratio   - act_ann_martin_ratio
#> 25. Active Annualized Calmar Ratio   - act_ann_calmar_ratio
#> 26. Active Omega Ratio   - act_omega
#> 27. Active Rachev Ratio   - act_rachev_ratio
#> 28. Active Average Drawdown Recovery   - act_avg_dd_rec
#> 29. Active Average Drawdown Length   - act_avg_dd_length
#> 30. Active Hurst Index   - act_hurst
#> 31. Active Minimum Track Record   - act_min_track_record
#> 32. Probabilistic Info Ratio   - prob_info_ratio
#> 33. Active Modigliani Ratio   - act_modigliani
#> 34. Active Annualized Modigliani Ratio   - act_ann_modigliani
#> 
#> ### Raw Performance Metrics
#> The following metrics are used when the `active_returns` slot of the `ss_backtest_config` object is FALSE.
#> 
#> 1. Arithmetic Mean Return   - arith_mean_ret
#> 2. Geometric Mean Return   - geom_mean_ret
#> 3. Annualized Return   - ann_ret
#> 4. Standard Deviation   - std_dev
#> 5. Annualized Standard Deviation   - ann_std_dev
#> 6. Semi Deviation   - semi_dev
#> 7. Downside Deviation   - down_dev
#> 8. Drawdown Deviation   - dd_dev
#> 9. Downside Frequency   - down_freq
#> 10. Expected Shortfall   - exp_short
#> 11. Pain   - pain
#> 12. Ulcer   - ulcer
#> 13. Maximum Drawdown   - max_dd
#> 14. Skewness   - skew
#> 15. Kurtosis   - kurt
#> 16. Sharpe Ratio   - sharpe_ratio
#> 17. Annualized Sharpe Ratio   - ann_sharpe_ratio
#> 18. Sortino Ratio   - sortino_ratio
#> 19. Annualized Burke Ratio   - ann_burke_ratio
#> 20. Inverted D-Ratio   - inv_d_ratio
#> 21. Omega Ratio   - omega
#> 22. Rachev Ratio   - rachev_ratio
#> 23. Average Drawdown Recovery   - avg_dd_rec
#> 24. Average Drawdown Length   - avg_dd_length
#> 25. Hurst Index   - hurst
#> 26. Minimum Track Record   - min_track_record
#> 27. Modigliani Ratio   - modigliani
#> 28. Annualized Modigliani Ratio   - ann_modigliani
#> 
#> ### Frequentist CAPM Metrics
#> The following metrics are used when the `p_correction_method` slot of the `ss_backtest_config` object is NOT `bayesian`.
#>     Therefore, these metrics include the Bayesian metrics without the 'posterior' prefix.
#>     When the 'model_stucture' in ss_backtest_config is 'partial_pooled', one can choose between 'theme_alpha', 'individual_alpha',
#>     'theme_beta' and 'individual_beta'. When the 'model_stucture' in ss_backtest_config is 'no_pooled', one can only choose between
#>     'alpha' and 'beta'.
#> 
#> 1. Theme Alpha   - theme_alpha
#> 2. Individual Alpha   - individual_alpha
#> 3. Alpha Standard Error   - alpha_se
#> 4. Theme Beta   - theme_beta
#> 5. Individual Beta   - individual_beta
#> 6. Specific Risk   - specific_risk
#> 7. Alpha T-Statistic   - alpha_t_stat
#> 8. Treynor Ratio   - treynor_ratio
#> 9. Appraisal Ratio   - appraisal_ratio
#> 
#> ### For no_pooled model_structures, the theme and individual alpha and beta are just 'alpha' and 'beta'. The remaining metrics stay the same: 
#> 10. Alpha   - alpha
#> 11. Beta   - beta
#> 
#> ### Posterior Metrics (Bayesian Correction Method)
#> The following metrics are used when the `p_correction_method` slot of the `sb_backtest_config` is set to `bayesian`.
#> 
#> 1. Posterior Theme Alpha   - posterior_theme_alpha
#> 2. Posterior Individual Alpha   - posterior_individual_alpha
#> 3. Posterior Alpha Standard Error   - posterior_alpha_se
#> 4. Posterior Theme Beta   - posterior_theme_beta
#> 5. Posterior Individual Beta   - posterior_individual_beta
#> 6. Posterior Specific Risk   - posterior_specific_risk
#> 7. Posterior Alpha T-Statistic   - posterior_alpha_t_stat
#> 8. Posterior Treynor Ratio   - posterior_treynor_ratio
#> 9. Posterior Appraisal Ratio   - posterior_appraisal_ratio
#> 
#> ### Error Metrics for XGBoost or Neural Networks (sb_algorithm = 'xgb' or 'nn')
#> These error metrics are valid only when the `sb_algorithm` is set to 'xgb' or 'nn'.
#> 
#> 1. Squared Error - squared_error
#> 2. Pseudo-Huber Error - pseudo_huber_error
#> 3. Absolute Error - absolute_error
#> ## Additionally, user-defined metrics can also be considered, if they have been passed to run_ss_backtest through a custom_signal_universe_metrics_m_df object
#> 
```
