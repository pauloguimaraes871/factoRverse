# Package index

## Data structures & meta objects

The self-documenting S4 containers at the heart of the package.
`meta_dataframe` (long panels) and `meta_xts` (wide time series) carry a
workflow log, metadata, and structural validation, and specialise into
the objects each workflow consumes and produces.

- [`create_meta_dataframe()`](https://pauloguimaraes871.github.io/factoRverse/reference/create_meta_dataframe.md)
  : Create a meta_dataframe

- [`create_meta_xts()`](https://pauloguimaraes871.github.io/factoRverse/reference/create_meta_xts.md)
  :

  Create a `meta_xts` Object (`returns_meta_xts` or `metrics_meta_xts`)

- [`update_meta_dataframe()`](https://pauloguimaraes871.github.io/factoRverse/reference/update_meta_dataframe.md)
  : Update meta_dataframe by appending a batch

- [`is_meta_dataframe()`](https://pauloguimaraes871.github.io/factoRverse/reference/is_meta_dataframe.md)
  : Check if an object is a meta_dataframe

- [`is_coercible_to_meta_dataframe()`](https://pauloguimaraes871.github.io/factoRverse/reference/is_coercible_to_meta_dataframe.md)
  : Check if an object is coercible to meta_dataframe

- [`get_data()`](https://pauloguimaraes871.github.io/factoRverse/reference/get_data.md)
  : Accessor for Data Slot

- [`get_workflow()`](https://pauloguimaraes871.github.io/factoRverse/reference/get_workflow.md)
  : Accessor for Workflow Slot

- [`get_dates()`](https://pauloguimaraes871.github.io/factoRverse/reference/get_dates.md)
  : Accessor for Dates

- [`get_tickers()`](https://pauloguimaraes871.github.io/factoRverse/reference/get_tickers.md)
  : Accessor for Tickers

- [`meta_dataframe-class`](https://pauloguimaraes871.github.io/factoRverse/reference/meta_dataframe-class.md)
  [`meta_dataframe`](https://pauloguimaraes871.github.io/factoRverse/reference/meta_dataframe-class.md)
  : meta_dataframe-class

- [`meta_xts-class`](https://pauloguimaraes871.github.io/factoRverse/reference/meta_xts-class.md)
  : An S4 Class Storing a Time Series Plus Metadata

- [`returns_meta_xts-class`](https://pauloguimaraes871.github.io/factoRverse/reference/returns_meta_xts-class.md)
  :

  An S4 Subclass of `meta_xts` for Return Series (No Holes Allowed)

- [`metrics_meta_xts-class`](https://pauloguimaraes871.github.io/factoRverse/reference/metrics_meta_xts-class.md)
  :

  An S4 Subclass of `meta_xts` for Metric Series (Holes Allowed)

- [`raw_features_m_df-class`](https://pauloguimaraes871.github.io/factoRverse/reference/raw_features_m_df-class.md)
  [`raw_features_m_df`](https://pauloguimaraes871.github.io/factoRverse/reference/raw_features_m_df-class.md)
  : raw_features_m_df-class

- [`signals_m_df-class`](https://pauloguimaraes871.github.io/factoRverse/reference/signals_m_df-class.md)
  [`signals_m_df`](https://pauloguimaraes871.github.io/factoRverse/reference/signals_m_df-class.md)
  : signals_m_df-class

- [`groups_m_df-class`](https://pauloguimaraes871.github.io/factoRverse/reference/groups_m_df-class.md)
  [`groups_m_df`](https://pauloguimaraes871.github.io/factoRverse/reference/groups_m_df-class.md)
  : groups_m_df-class

- [`weights_m_df-class`](https://pauloguimaraes871.github.io/factoRverse/reference/weights_m_df-class.md)
  [`weights_m_df`](https://pauloguimaraes871.github.io/factoRverse/reference/weights_m_df-class.md)
  : weights_m_df-class

- [`stock_universe_m_df-class`](https://pauloguimaraes871.github.io/factoRverse/reference/stock_universe_m_df-class.md)
  [`stock_universe_m_df`](https://pauloguimaraes871.github.io/factoRverse/reference/stock_universe_m_df-class.md)
  : stock_universe_m_df-class

- [`oos_sb_outputs_m_df-class`](https://pauloguimaraes871.github.io/factoRverse/reference/oos_sb_outputs_m_df-class.md)
  [`oos_sb_outputs_m_df`](https://pauloguimaraes871.github.io/factoRverse/reference/oos_sb_outputs_m_df-class.md)
  : oos_sb_outputs_m_df-class

- [`feature_importance_m_df-class`](https://pauloguimaraes871.github.io/factoRverse/reference/feature_importance_m_df-class.md)
  [`feature_importance_m_df`](https://pauloguimaraes871.github.io/factoRverse/reference/feature_importance_m_df-class.md)
  : feature_importance_m_df-class

## Tickers catalog & survivorship

Track ticker lifecycle events (listings, delistings, renames) with
stable permanent identifiers, and build a survivorship-aware panel.

- [`create_tickers_catalog()`](https://pauloguimaraes871.github.io/factoRverse/reference/create_tickers_catalog.md)
  : Create a tickers_catalog Object

- [`update_tickers_catalog()`](https://pauloguimaraes871.github.io/factoRverse/reference/update_tickers_catalog.md)
  :

  Update a `tickers_catalog` Object

- [`read_tickers_catalog()`](https://pauloguimaraes871.github.io/factoRverse/reference/read_tickers_catalog.md)
  : Apply tickers_catalog Transformations

- [`lookup_catalog()`](https://pauloguimaraes871.github.io/factoRverse/reference/lookup_catalog.md)
  : Lookup method for tickers_catalog

- [`tickers_catalog-class`](https://pauloguimaraes871.github.io/factoRverse/reference/tickers_catalog-class.md)
  [`tickers_catalog`](https://pauloguimaraes871.github.io/factoRverse/reference/tickers_catalog-class.md)
  : tickers_catalog-class

## Feature engineering & targets

Construct point-in-time characteristics from the factor-zoo literature
(rolling and seasonal windows, formulas, composite scores, sector-wise
metrics, cross-sectional / time-series combinations, sector-specific
mappings), and build forward, risk-adjusted prediction targets.

- [`compute_window()`](https://pauloguimaraes871.github.io/factoRverse/reference/compute_window.md)
  : Compute Rolling or Seasonal Calculations for a Given Metric in
  meta_dataframe or meta_xts
- [`compute_formula()`](https://pauloguimaraes871.github.io/factoRverse/reference/compute_formula.md)
  : Compute Formula-Based Signal Calculation
- [`compute_score()`](https://pauloguimaraes871.github.io/factoRverse/reference/compute_score.md)
  : Compute Score Based on Conditions
- [`compute_across()`](https://pauloguimaraes871.github.io/factoRverse/reference/compute_across.md)
  : Compute Across: Apply a Calculation Between meta_dataframe and
  meta_xts
- [`compute_sector_wise()`](https://pauloguimaraes871.github.io/factoRverse/reference/compute_sector_wise.md)
  : Compute Sector-Wise Calculation for a Given Signal in a
  meta_dataframe
- [`compute_sector_map()`](https://pauloguimaraes871.github.io/factoRverse/reference/compute_sector_map.md)
  : Compute Sector-Based Column Transformations
- [`compute_sector_map_across()`](https://pauloguimaraes871.github.io/factoRverse/reference/compute_sector_map_across.md)
  : Compute Sector-Based Mapped Values Across a Meta XTS
- [`create_target_m_df()`](https://pauloguimaraes871.github.io/factoRverse/reference/create_target_m_df.md)
  : Build target_m_df for forward horizons
- [`target_m_df-class`](https://pauloguimaraes871.github.io/factoRverse/reference/target_m_df-class.md)
  [`target_m_df`](https://pauloguimaraes871.github.io/factoRverse/reference/target_m_df-class.md)
  : target_m_df-class

## Signal & statistic primitives

The low-level building blocks used by the `compute_*` functions and the
portfolio engine: benchmark regressions, momentum / volatility
statistics, and signal transformation.

- [`alpha_bench()`](https://pauloguimaraes871.github.io/factoRverse/reference/alpha_bench.md)
  : Calculate Alpha Relative to a Benchmark
- [`alpha_tstat_bench()`](https://pauloguimaraes871.github.io/factoRverse/reference/alpha_tstat_bench.md)
  : Compute the t-Statistic of Alpha Relative to a Benchmark
- [`beta_bench()`](https://pauloguimaraes871.github.io/factoRverse/reference/beta_bench.md)
  : Calculate Beta Relative to a Benchmark
- [`correlation_bench()`](https://pauloguimaraes871.github.io/factoRverse/reference/correlation_bench.md)
  : Calculate Correlation with a Benchmark
- [`res_mom()`](https://pauloguimaraes871.github.io/factoRverse/reference/res_mom.md)
  : Calculate Residual Momentum Score
- [`idio_vol()`](https://pauloguimaraes871.github.io/factoRverse/reference/idio_vol.md)
  : Calculate Idiosyncratic Volatility
- [`skew()`](https://pauloguimaraes871.github.io/factoRverse/reference/skew.md)
  : Calculate Skewness of a Numeric Vector
- [`sur()`](https://pauloguimaraes871.github.io/factoRverse/reference/sur.md)
  : Calculate Standardized Unexpected Realization (SUR)
- [`cagr()`](https://pauloguimaraes871.github.io/factoRverse/reference/cagr.md)
  : Calculate Compound Annual Growth Rate (CAGR)
- [`geometric_mean_return()`](https://pauloguimaraes871.github.io/factoRverse/reference/geometric_mean_return.md)
  : Geometric Mean Return (percentage-point scale)
- [`count_if()`](https://pauloguimaraes871.github.io/factoRverse/reference/count_if.md)
  : Count elements that satisfy a condition
- [`signal_transform()`](https://pauloguimaraes871.github.io/factoRverse/reference/signal_transform.md)
  : Signal Transformation Function
- [`relative_risk_contribution()`](https://pauloguimaraes871.github.io/factoRverse/reference/relative_risk_contribution.md)
  : Calculate Relative Risk Contribution

## Point-in-time preprocessing

Apply a `recipes` preprocessing pipeline one date at a time so no future
information leaks into the past, plus custom recipe steps for
winsorization and sector-mean imputation.

- [`map_recipe_timewise()`](https://pauloguimaraes871.github.io/factoRverse/reference/map_recipe_timewise.md)
  : Map a Recipe to Sequential Dates
- [`step_winsorize()`](https://pauloguimaraes871.github.io/factoRverse/reference/step_winsorize.md)
  : Step for Winsorization
- [`step_impute_sector()`](https://pauloguimaraes871.github.io/factoRverse/reference/step_impute_sector.md)
  : Add step_impute_sector to a Recipe
- [`step_winsorize_new()`](https://pauloguimaraes871.github.io/factoRverse/reference/step_winsorize_new.md)
  : Constructor for step_winsorize
- [`step_impute_sector_new()`](https://pauloguimaraes871.github.io/factoRverse/reference/step_impute_sector_new.md)
  : Constructor for step_impute_sector
- [`required_pkgs.step_winsorize()`](https://pauloguimaraes871.github.io/factoRverse/reference/required_pkgs.step_winsorize.md)
  : Required Packages for step_winsorize
- [`required_pkgs.step_impute_sector()`](https://pauloguimaraes871.github.io/factoRverse/reference/required_pkgs.step_impute_sector.md)
  : Required Packages for step_impute_sector
- [`bake(`*`<step_winsorize>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/bake.step_winsorize.md)
  : Apply winsorization during baking
- [`bake(`*`<step_impute_sector>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/bake.step_impute_sector.md)
  : Apply imputation during baking
- [`prep(`*`<step_winsorize>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/prep.step_winsorize.md)
  : Prepare Method for step_winsorize
- [`prep(`*`<step_impute_sector>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/prep.step_impute_sector.md)
  : Prepare Method for step_impute_sector
- [`tidy(`*`<step_winsorize>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/tidy.step_winsorize.md)
  : Tidy method for step_winsorize
- [`tidy(`*`<step_impute_sector>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/tidy.step_impute_sector.md)
  : Tidy Method for step_impute_sector
- [`print(`*`<step_winsorize>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/print.step_winsorize.md)
  : Print Method for step_winsorize
- [`print(`*`<step_impute_sector>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/print.step_impute_sector.md)
  : Print Method for step_impute_sector

## Screening

Filter the investable universe by feature values, logical conditions, or
liquidity.

- [`screen_by_feature()`](https://pauloguimaraes871.github.io/factoRverse/reference/screen_by_feature.md)
  : Screen (select) features from a meta_dataframe
- [`screen_by_feature(`*`<meta_dataframe>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/screen_by_feature-meta_dataframe-method.md)
  : Method for selecting features from a meta_dataframe
- [`screen_by_conditions()`](https://pauloguimaraes871.github.io/factoRverse/reference/screen_by_conditions.md)
  : Screen (filter) a meta_dataframe by row-wise conditions
- [`screen_by_conditions(`*`<meta_dataframe>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/screen_by_conditions-meta_dataframe-method.md)
  : Method for screening a meta_dataframe based on conditions
- [`screen_by_liquidity()`](https://pauloguimaraes871.github.io/factoRverse/reference/screen_by_liquidity.md)
  : Generic function for screening a meta_dataframe based on liquidity
  classification, given a liquidity_floor_rule.

## Workflow 1: Characteristic-portfolio backtesting

Rank stocks on a characteristic, invest in the top quantile, and
backtest the resulting long-only portfolio. Aggregate many backtests
into a cohort to compare them as a group.

- [`create_port_backtest_config()`](https://pauloguimaraes871.github.io/factoRverse/reference/create_port_backtest_config.md)
  : Create port_backtest_config Object

- [`run_port_backtest()`](https://pauloguimaraes871.github.io/factoRverse/reference/run_port_backtest.md)
  : Run Portfolio Backtest

- [`update_port_backtest()`](https://pauloguimaraes871.github.io/factoRverse/reference/update_port_backtest.md)
  :

  Update Portfolio Backtest The `update_port_backtest` function will
  take an existing `port_backtest_results` object and update it with new
  dates. This function is useful when you want to add new dates to an
  existing backtest without having to re-run the entire backtest.

- [`create_port_backtest_cohort()`](https://pauloguimaraes871.github.io/factoRverse/reference/create_port_backtest_cohort.md)
  : Create Portfolio Backtest Cohort

- [`port_backtest_config-class`](https://pauloguimaraes871.github.io/factoRverse/reference/port_backtest_config-class.md)
  : Class for Port Backtest Config

- [`port_backtest_results-class`](https://pauloguimaraes871.github.io/factoRverse/reference/port_backtest_results-class.md)
  : S4 Class for Portfolio Backtest Results

- [`port_backtest_cohort-class`](https://pauloguimaraes871.github.io/factoRverse/reference/port_backtest_cohort-class.md)
  : S4 Class for Portfolio Backtest Cohort

## Constraints, liquidity & transaction costs

Composable policy builders attached to a config with `add_*()`:
liquidity floors and caps, turnover buffers, concentration limits,
covariance estimation, and a BARRA-style transaction-cost model.

- [`add_liquidity_floor_cutoffs()`](https://pauloguimaraes871.github.io/factoRverse/reference/add_liquidity_floor_cutoffs.md)
  : Add Liquidity Floor Cutoffs

- [`create_liquidity_floor_cutoffs()`](https://pauloguimaraes871.github.io/factoRverse/reference/create_liquidity_floor_cutoffs.md)
  : Create Liquidity Floor Cutoffs

- [`get_liquidity_floor_cutoffs()`](https://pauloguimaraes871.github.io/factoRverse/reference/get_liquidity_floor_cutoffs.md)
  : Accessor for Liquidity Floor Cutoffs

- [`add_liquidity_constraint_policy()`](https://pauloguimaraes871.github.io/factoRverse/reference/add_liquidity_constraint_policy.md)
  : Add Liquidity Constraint Policy

- [`create_liquidity_constraint_policy()`](https://pauloguimaraes871.github.io/factoRverse/reference/create_liquidity_constraint_policy.md)
  : Create Liquidity Constraint Policy

- [`get_liquidity_constraint_policy()`](https://pauloguimaraes871.github.io/factoRverse/reference/get_liquidity_constraint_policy.md)
  : Accessor for Liquidity Constraint Policy

- [`add_turnover_constraint_policy()`](https://pauloguimaraes871.github.io/factoRverse/reference/add_turnover_constraint_policy.md)
  : Add Turnover Constraint Policy

- [`create_turnover_constraint_policy()`](https://pauloguimaraes871.github.io/factoRverse/reference/create_turnover_constraint_policy.md)
  : Create Turnover Constraint Policy

- [`get_turnover_constraint_policy()`](https://pauloguimaraes871.github.io/factoRverse/reference/get_turnover_constraint_policy.md)
  : Accessor for Turnover Constraint Policy

- [`add_concentration_constraint_policy()`](https://pauloguimaraes871.github.io/factoRverse/reference/add_concentration_constraint_policy.md)
  : Add Concentration Constraint Policy

- [`create_concentration_constraint_policy()`](https://pauloguimaraes871.github.io/factoRverse/reference/create_concentration_constraint_policy.md)
  : Create Concentration Constraint Policy

- [`get_concentration_constraint_policy()`](https://pauloguimaraes871.github.io/factoRverse/reference/get_concentration_constraint_policy.md)
  : Get the Concentration Constraint Policy

- [`add_transaction_costs_parameters()`](https://pauloguimaraes871.github.io/factoRverse/reference/add_transaction_costs_parameters.md)
  : Add Transaction Cost Parameters to a Portfolio Backtest
  Configuration

- [`create_transaction_costs_parameters()`](https://pauloguimaraes871.github.io/factoRverse/reference/create_transaction_costs_parameters.md)
  : Create a New Transaction Cost Parameters Object

- [`get_transaction_costs_parameters()`](https://pauloguimaraes871.github.io/factoRverse/reference/get_transaction_costs_parameters.md)
  : Accessor for Transaction Cost Parameters

- [`validate_transaction_costs_parameters()`](https://pauloguimaraes871.github.io/factoRverse/reference/validate_transaction_costs_parameters.md)
  : Validate Transaction Cost Parameters

- [`add_cov_est_method()`](https://pauloguimaraes871.github.io/factoRverse/reference/add_cov_est_method.md)
  : Add covariance estimation method to a backtest configuration

- [`create_cov_est_method()`](https://pauloguimaraes871.github.io/factoRverse/reference/create_cov_est_method.md)
  : Create Covariance Estimation Method

- [`is_portfolio_policies()`](https://pauloguimaraes871.github.io/factoRverse/reference/is_portfolio_policies.md)
  : Define the is portfolio_policies function

- [`liquidity_constraint_policy-class`](https://pauloguimaraes871.github.io/factoRverse/reference/liquidity_constraint_policy-class.md)
  : Liquidity Constraint Policy

- [`turnover_constraint_policy-class`](https://pauloguimaraes871.github.io/factoRverse/reference/turnover_constraint_policy-class.md)
  : Turnover Constraint Policy

- [`concentration_constraint_policy-class`](https://pauloguimaraes871.github.io/factoRverse/reference/concentration_constraint_policy-class.md)
  : Concentration Constraint Policy

- [`transaction_costs_parameters-class`](https://pauloguimaraes871.github.io/factoRverse/reference/transaction_costs_parameters-class.md)
  : Transaction Cost Parameters S4 Class

- [`cov_est_method-class`](https://pauloguimaraes871.github.io/factoRverse/reference/cov_est_method-class.md)
  :

  Define the `cov_est_method` S4 Class

- [`transactions_log-class`](https://pauloguimaraes871.github.io/factoRverse/reference/transactions_log-class.md)
  : Define the transactions_log S4 Class

## Portfolio construction methods

Weighting schemes from equal-weight to risk parity, hierarchical risk
parity, mean-variance optimization (with resampling), and the
micro-macro allocation framework, plus universe classification and trade
generation.

- [`set_portfolio_weights()`](https://pauloguimaraes871.github.io/factoRverse/reference/set_portfolio_weights.md)
  : Set Portfolio Weights

- [`create_hrp_portfolio()`](https://pauloguimaraes871.github.io/factoRverse/reference/create_hrp_portfolio.md)
  : Create a Hierarchical Risk Parity (HRP) portfolio

- [`create_mvo_portfolio()`](https://pauloguimaraes871.github.io/factoRverse/reference/create_mvo_portfolio.md)
  : Create a MVO portfolio

- [`create_resampled_mvo_portfolio()`](https://pauloguimaraes871.github.io/factoRverse/reference/create_resampled_mvo_portfolio.md)
  : Create a Resampled MVO portfolio

- [`create_mmaf_portfolio()`](https://pauloguimaraes871.github.io/factoRverse/reference/create_mmaf_portfolio.md)
  : Create a Micro–Macro Allocation Framework (MMAF) portfolio

- [`classify_investment_universe()`](https://pauloguimaraes871.github.io/factoRverse/reference/classify_investment_universe.md)
  : Classify the universe based on signals and other custom and
  user-defined rules.

- [`classify_stock_liquidity()`](https://pauloguimaraes871.github.io/factoRverse/reference/classify_stock_liquidity.md)
  : Classify stocks based on their liquidity

- [`derive_stock_universe_m_d_ref()`](https://pauloguimaraes871.github.io/factoRverse/reference/derive_stock_universe_m_d_ref.md)
  : Build Stock Universe with Expected Return Score

- [`calculate_trade_orders()`](https://pauloguimaraes871.github.io/factoRverse/reference/calculate_trade_orders.md)
  : Calculate Trade Orders

- [`validate_exp_ret_score_tilt()`](https://pauloguimaraes871.github.io/factoRverse/reference/validate_exp_ret_score_tilt.md)
  : Valida exp_ret_score_tilt

- [`add_rp_parameters()`](https://pauloguimaraes871.github.io/factoRverse/reference/add_rp_parameters.md)
  : Add rp_parameters to a backtest config

- [`create_rp_parameters()`](https://pauloguimaraes871.github.io/factoRverse/reference/create_rp_parameters.md)
  : Create RP (Risk Parity) Parameters

- [`add_hrp_parameters()`](https://pauloguimaraes871.github.io/factoRverse/reference/add_hrp_parameters.md)
  : Add hrp_parameters to a backtest config

- [`create_hrp_parameters()`](https://pauloguimaraes871.github.io/factoRverse/reference/create_hrp_parameters.md)
  : Create HRP (Hierarchical Risk Parity) Parameters

- [`add_mvo_parameters()`](https://pauloguimaraes871.github.io/factoRverse/reference/add_mvo_parameters.md)
  : Add mvo_parameters to a backtest config

- [`create_mvo_parameters()`](https://pauloguimaraes871.github.io/factoRverse/reference/create_mvo_parameters.md)
  : Create MVO Parameters

- [`add_mmaf_parameters()`](https://pauloguimaraes871.github.io/factoRverse/reference/add_mmaf_parameters.md)
  : Add mmaf_parameters to a backtest config

- [`create_mmaf_parameters()`](https://pauloguimaraes871.github.io/factoRverse/reference/create_mmaf_parameters.md)
  : Create MMAF (Micro Macro Allocation Framework) Parameters

- [`port-class`](https://pauloguimaraes871.github.io/factoRverse/reference/port-class.md)
  [`signal_port-class`](https://pauloguimaraes871.github.io/factoRverse/reference/port-class.md)
  [`stock_port-class`](https://pauloguimaraes871.github.io/factoRverse/reference/port-class.md)
  : Portfolio classes for backtesting portfolios

- [`rp_parameters-class`](https://pauloguimaraes871.github.io/factoRverse/reference/rp_parameters-class.md)
  :

  Define the `rp_parameters` S4 Class

- [`hrp_parameters-class`](https://pauloguimaraes871.github.io/factoRverse/reference/hrp_parameters-class.md)
  :

  Define the `hrp_parameters` S4 Class

- [`mvo_parameters-class`](https://pauloguimaraes871.github.io/factoRverse/reference/mvo_parameters-class.md)
  :

  Define the `mvo_parameters` S4 Class

- [`mmaf_parameters-class`](https://pauloguimaraes871.github.io/factoRverse/reference/mmaf_parameters-class.md)
  :

  Define the `mmaf_parameters` S4 Class

- [`mmaf_sub_port_config-class`](https://pauloguimaraes871.github.io/factoRverse/reference/mmaf_sub_port_config-class.md)
  : MMAF Sub Portfolio Configuration

## Workflow 2: Signal selection (the factor zoo)

Walk-forward inference on CAPM alpha with multiple-testing control
(frequentist FWER / FDR) or hierarchical Bayesian partial pooling within
themes, to decide which signals enter the portfolio.

- [`create_ss_backtest_config()`](https://pauloguimaraes871.github.io/factoRverse/reference/create_ss_backtest_config.md)
  : Create an ss_backtest_config Object

- [`run_ss_backtest()`](https://pauloguimaraes871.github.io/factoRverse/reference/run_ss_backtest.md)
  : Run Signal Selection Backtest

- [`update_ss_backtest()`](https://pauloguimaraes871.github.io/factoRverse/reference/update_ss_backtest.md)
  :

  Update Signal Selection Backtest The `update_ss_backtest` function
  will take an existing `port_backtest_results` object and update it
  with new dates. This function is useful when you want to add new dates
  to an existing backtest without having to re-run the entire backtest.

- [`add_alpha_test_strategy()`](https://pauloguimaraes871.github.io/factoRverse/reference/add_alpha_test_strategy.md)
  : Add Alpha Test Strategy to ss_backtest_config

- [`create_alpha_test_strategy()`](https://pauloguimaraes871.github.io/factoRverse/reference/create_alpha_test_strategy.md)
  : Create an alpha_test_strategy object

- [`get_alpha_test_strategy()`](https://pauloguimaraes871.github.io/factoRverse/reference/get_alpha_test_strategy.md)
  : Get Alpha Test Strategy

- [`add_bayesian_model_parameters()`](https://pauloguimaraes871.github.io/factoRverse/reference/add_bayesian_model_parameters.md)
  : Add Bayesian Model Parameters

- [`create_bayesian_model_parameters()`](https://pauloguimaraes871.github.io/factoRverse/reference/create_bayesian_model_parameters.md)
  : Create Bayesian Model Parameters

- [`get_bayesian_model_parameters()`](https://pauloguimaraes871.github.io/factoRverse/reference/get_bayesian_model_parameters.md)
  : Get Bayesian Model Parameters

- [`add_brms_prior()`](https://pauloguimaraes871.github.io/factoRverse/reference/add_brms_prior.md)
  : Add Prior to Bayesian Alpha Test Strategy

- [`bayesian_adjustment()`](https://pauloguimaraes871.github.io/factoRverse/reference/bayesian_adjustment.md)
  : Bayesian Adjustment Function

- [`create_se_benchmarks()`](https://pauloguimaraes871.github.io/factoRverse/reference/create_se_benchmarks.md)
  : Create Signal Engineering Benchmarks

- [`create_performance_m_df()`](https://pauloguimaraes871.github.io/factoRverse/reference/create_performance_m_df.md)
  : Create a Performance Metrics Data Frame

- [`ss_backtest_config-class`](https://pauloguimaraes871.github.io/factoRverse/reference/ss_backtest_config-class.md)
  : ss_backtest_config Class

- [`ss_backtest_results-class`](https://pauloguimaraes871.github.io/factoRverse/reference/ss_backtest_results-class.md)
  : S4 Class for Signal Selection Backtest Results

- [`alpha_test_strategy-class`](https://pauloguimaraes871.github.io/factoRverse/reference/alpha_test_strategy-class.md)
  : alpha_test_strategy Class

- [`frequentist_alpha_test_strategy-class`](https://pauloguimaraes871.github.io/factoRverse/reference/frequentist_alpha_test_strategy-class.md)
  : frequentist_alpha_test_strategy Class

- [`bayesian_alpha_test_strategy-class`](https://pauloguimaraes871.github.io/factoRverse/reference/bayesian_alpha_test_strategy-class.md)
  : bayesian_alpha_test_strategy Class

- [`bayesian_model_parameters-class`](https://pauloguimaraes871.github.io/factoRverse/reference/bayesian_model_parameters-class.md)
  : bayesian_model_parameters Class

- [`signal_universe_m_df-class`](https://pauloguimaraes871.github.io/factoRverse/reference/signal_universe_m_df-class.md)
  [`signal_universe_m_df`](https://pauloguimaraes871.github.io/factoRverse/reference/signal_universe_m_df-class.md)
  : signal_universe_m_df-class

- [`priors_m_df-class`](https://pauloguimaraes871.github.io/factoRverse/reference/priors_m_df-class.md)
  [`priors_m_df`](https://pauloguimaraes871.github.io/factoRverse/reference/priors_m_df-class.md)
  : priors_m_df-class

## Workflow 3: Signal blending & machine learning

Combine the selected signals into one score per stock via heuristics
(equal / signal-weighted, risk parity, HRP, MVO) or ML (`glmnet`,
`ranger`, `xgboost`, `keras`) behind a single tuning and walk-forward
interface, then stack blenders into meta-ensembles and interpret
predictions.

- [`create_sb_backtest_config()`](https://pauloguimaraes871.github.io/factoRverse/reference/create_sb_backtest_config.md)
  : Create sb_backtest_config Object

- [`run_sb_backtest()`](https://pauloguimaraes871.github.io/factoRverse/reference/run_sb_backtest.md)
  : Run Signal Blending Backtest

- [`update_sb_backtest()`](https://pauloguimaraes871.github.io/factoRverse/reference/update_sb_backtest.md)
  : Update Signal Blending Backtest

- [`create_sb_metabacktest_config()`](https://pauloguimaraes871.github.io/factoRverse/reference/create_sb_metabacktest_config.md)
  : Create SB Meta Backtest Configuration

- [`add_tuning_strategy()`](https://pauloguimaraes871.github.io/factoRverse/reference/add_tuning_strategy.md)
  :

  Add a `tuning_strategy` to an Existing `sb_backtest_config`

- [`create_tuning_strategy()`](https://pauloguimaraes871.github.io/factoRverse/reference/create_tuning_strategy.md)
  : Hyperparameter Tuning Strategy Constructor

- [`get_tuning_strategy()`](https://pauloguimaraes871.github.io/factoRverse/reference/get_tuning_strategy.md)
  : Get Hyperparameter Tuning Strategy

- [`is_tuning_strategy()`](https://pauloguimaraes871.github.io/factoRverse/reference/is_tuning_strategy.md)
  : Define the is_hyperparameter_tuning_strategy function

- [`add_hyperparameter()`](https://pauloguimaraes871.github.io/factoRverse/reference/add_hyperparameter.md)
  :

  Add a Hyperparameter to a `hyper_grid_domain`, whether inside a
  `sb_backtest_config`, a `tuning_strategy` or on its own.

- [`add_hyper_grid_domain()`](https://pauloguimaraes871.github.io/factoRverse/reference/add_hyper_grid_domain.md)
  :

  Add a `hyper_grid_domain` Object

- [`is_hyper_grid_domain()`](https://pauloguimaraes871.github.io/factoRverse/reference/is_hyper_grid_domain.md)
  : Check if an object is of class hyper_grid_domain

- [`hyperparameters()`](https://pauloguimaraes871.github.io/factoRverse/reference/hyperparameters.md)
  : Get Expected Hyperparameters for a Machine Learning Algorithm or
  Configuration

- [`add_keras_architecture()`](https://pauloguimaraes871.github.io/factoRverse/reference/add_keras_architecture.md)
  : Add Keras Architecture

- [`add_keras_layer()`](https://pauloguimaraes871.github.io/factoRverse/reference/add_keras_layer.md)
  : Add Layer to Keras Architecture

- [`create_keras_architecture()`](https://pauloguimaraes871.github.io/factoRverse/reference/create_keras_architecture.md)
  : Create Keras Architecture

- [`is_keras_architecture_parameters()`](https://pauloguimaraes871.github.io/factoRverse/reference/is_keras_architecture_parameters.md)
  : Define the is keras_architecture_parameters function

- [`hyper_tune()`](https://pauloguimaraes871.github.io/factoRverse/reference/hyper_tune.md)
  : Perform Hyperparameter Tuning for Machine Learning Models

- [`set_eval_function()`](https://pauloguimaraes871.github.io/factoRverse/reference/set_eval_function.md)
  : Build the Evaluation Function for Hyperparameter Tuning

- [`calculate_eval_metrics()`](https://pauloguimaraes871.github.io/factoRverse/reference/calculate_eval_metrics.md)
  : Calculate Out-of-Sample Evaluation Metrics

- [`fit_keras_model()`](https://pauloguimaraes871.github.io/factoRverse/reference/fit_keras_model.md)
  : Fit a Keras Neural Network Model

- [`get_sb_algorithm()`](https://pauloguimaraes871.github.io/factoRverse/reference/sb_model_accessors.md)
  [`get_best_hyperparameters()`](https://pauloguimaraes871.github.io/factoRverse/reference/sb_model_accessors.md)
  [`get_model()`](https://pauloguimaraes871.github.io/factoRverse/reference/sb_model_accessors.md)
  : Accessor Methods for sb_model

- [`select_and_correct_signals()`](https://pauloguimaraes871.github.io/factoRverse/reference/select_and_correct_signals.md)
  : Select and Correct Signal Positions

- [`convert_oos_list_to_m_df()`](https://pauloguimaraes871.github.io/factoRverse/reference/convert_oos_list_to_m_df.md)
  : Convert OOS Lists to Meta Data Frame

- [`explain_prediction()`](https://pauloguimaraes871.github.io/factoRverse/reference/explain_prediction.md)
  : Explain Prediction

- [`sb_backtest_config-class`](https://pauloguimaraes871.github.io/factoRverse/reference/sb_backtest_config-class.md)
  : sb_backtest_config Class

- [`sb_backtest_results-class`](https://pauloguimaraes871.github.io/factoRverse/reference/sb_backtest_results-class.md)
  : S4 Class for Time Series Walk-Forward Validation Results of
  Signal-Blending Models

- [`sb_metabacktest_config-class`](https://pauloguimaraes871.github.io/factoRverse/reference/sb_metabacktest_config-class.md)
  : sb_metabacktest_config Class

- [`sb_metabacktest_results-class`](https://pauloguimaraes871.github.io/factoRverse/reference/sb_metabacktest_results-class.md)
  : sb_metabacktest_results Class

- [`sb_model-class`](https://pauloguimaraes871.github.io/factoRverse/reference/sb_model-class.md)
  :

  Define the `sb_model` S4 Class

- [`tuning_strategy-class`](https://pauloguimaraes871.github.io/factoRverse/reference/tuning_strategy-class.md)
  : Base Class for Hyperparameter Tuning Strategies

- [`grid_search_strategy-class`](https://pauloguimaraes871.github.io/factoRverse/reference/grid_search_strategy-class.md)
  : Grid Search Tuning Strategy

- [`random_search_strategy-class`](https://pauloguimaraes871.github.io/factoRverse/reference/random_search_strategy-class.md)
  : Random Search Tuning Strategy

- [`bayesian_opt_strategy-class`](https://pauloguimaraes871.github.io/factoRverse/reference/bayesian_opt_strategy-class.md)
  : Bayesian Optimization Tuning Strategy

- [`hyper_grid_domain-class`](https://pauloguimaraes871.github.io/factoRverse/reference/hyper_grid_domain-class.md)
  :

  Define the `hyper_grid_domain` S4 Class

- [`keras_architecture_parameters-class`](https://pauloguimaraes871.github.io/factoRverse/reference/keras_architecture_parameters-class.md)
  : Keras Architecture Parameters

- [`signal_port_parameters-class`](https://pauloguimaraes871.github.io/factoRverse/reference/signal_port_parameters-class.md)
  : Signal Portfolio Parameters

## Introspection & helpers

Utilities for discovering valid options and validating inputs.

- [`display_valid_custom_objectives()`](https://pauloguimaraes871.github.io/factoRverse/reference/display_valid_custom_objectives.md)
  : Display Valid Custom Objectives
- [`display_eligibility_criteria()`](https://pauloguimaraes871.github.io/factoRverse/reference/display_eligibility_criteria.md)
  : Display Eligibility Criteria with Colors
- [`check_consistent_dates()`](https://pauloguimaraes871.github.io/factoRverse/reference/check_consistent_dates.md)
  : Check for Consistent Dates in a Vector (Allowing NULLs)
- [`check_inputs_sb_backtest()`](https://pauloguimaraes871.github.io/factoRverse/reference/check_inputs_sb_backtest.md)
  : Perform validation checks on inputs for SB workflow

## S4 object methods

`show`, `summary`, `plot`, `predict`, and coercion methods defined for
factoRverse objects.

- [`show(`*`<alpha_test_strategy>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/show-alpha_test_strategy-method.md)
  : Show Alpha Test Strategy

- [`show(`*`<bayesian_alpha_test_strategy>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/show-bayesian_alpha_test_strategy-method.md)
  : Show Bayesian Alpha Test Strategy

- [`show(`*`<bayesian_model_parameters>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/show-bayesian_model_parameters-method.md)
  : Show Bayesian Model Parameters

- [`show(`*`<bayesian_opt_strategy>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/show-bayesian_opt_strategy-method.md)
  :

  Show Method for `bayesian_opt_strategy`

- [`show(`*`<concentration_constraint_policy>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/show-concentration_constraint_policy-method.md)
  : Show Concentration Constraint Policy

- [`show(`*`<cov_est_method>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/show-cov_est_method-method.md)
  : Show Covariance Estimation Method

- [`show(`*`<frequentist_alpha_test_strategy>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/show-frequentist_alpha_test_strategy-method.md)
  : Show Frequentist Alpha Test Strategy

- [`show(`*`<grid_search_strategy>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/show-grid_search_strategy-method.md)
  :

  Show Method for `grid_search_strategy`

- [`show(`*`<groups_m_df>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/show-groups_m_df-method.md)
  : Show Method for groups_m_df Class

- [`show(`*`<hrp_parameters>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/show-hrp_parameters-method.md)
  : Show HRP Parameters

- [`show(`*`<hyper_grid_domain>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/show-hyper_grid_domain-method.md)
  : Print method for hyper_grid_domain

- [`show(`*`<keras_architecture_parameters>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/show-keras_architecture_parameters-method.md)
  : Print keras_architecture_parameters

- [`show(`*`<liquidity_constraint_policy>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/show-liquidity_constraint_policy-method.md)
  : Show Liquidity Constraint Policy

- [`show(`*`<meta_dataframe>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/show-meta_dataframe-method.md)
  : Show Method for meta_dataframe Class

- [`show(`*`<meta_xts>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/show-meta_xts-method.md)
  : Show method for meta_xts

- [`show(`*`<metrics_meta_xts>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/show-metrics_meta_xts-method.md)
  : Show method for metrics_meta_xts

- [`show(`*`<mmaf_parameters>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/show-mmaf_parameters-method.md)
  : Show MMAF Parameters

- [`show(`*`<mvo_parameters>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/show-mvo_parameters-method.md)
  : Show MVO Parameters

- [`show(`*`<port>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/show-port-method.md)
  :

  Show a `port` object

- [`show(`*`<port_backtest_cohort>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/show-port_backtest_cohort-method.md)
  : Show Method for port_backtest_cohort Class

- [`show(`*`<port_backtest_config>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/show-port_backtest_config-method.md)
  : Show Port Backtest Config

- [`show(`*`<port_backtest_results>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/show-port_backtest_results-method.md)
  : Show Port Backtest Results

- [`show(`*`<random_search_strategy>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/show-random_search_strategy-method.md)
  :

  Show Method for `random_search_strategy`

- [`show(`*`<returns_meta_xts>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/show-returns_meta_xts-method.md)
  : Show method for returns_meta_xts

- [`show(`*`<rp_parameters>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/show-rp_parameters-method.md)
  : Show Risk-Parity Parameters

- [`show(`*`<sb_backtest_config>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/show-sb_backtest_config-method.md)
  : Show SB Backtest Config

- [`show(`*`<sb_backtest_results>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/show-sb_backtest_results-method.md)
  : Show Method for sb_backtest_results Class

- [`show(`*`<sb_metabacktest_config>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/show-sb_metabacktest_config-method.md)
  : Show Method for sb_metabacktest_config Class

- [`show(`*`<sb_metabacktest_results>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/show-sb_metabacktest_results-method.md)
  : Show Method for sb_metabacktest_results Class

- [`show(`*`<sb_model>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/show-sb_model-method.md)
  : Show Method for sb_model Class

- [`show(`*`<signal_universe_m_df>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/show-signal_universe_m_df-method.md)
  : Show Method for signal_universe_m_df Class

- [`show(`*`<ss_backtest_config>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/show-ss_backtest_config-method.md)
  : Show Signal Selection Backtest Config

- [`show(`*`<ss_backtest_results>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/show-ss_backtest_results-method.md)
  : Show Method for ss_backtest_results Class

- [`show(`*`<tickers_catalog>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/show-tickers_catalog-method.md)
  : Print method for tickers_catalog

- [`show(`*`<transaction_costs_parameters>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/show-transaction_costs_parameters-method.md)
  : Show Transaction Cost Parameters

- [`show(`*`<tuning_strategy>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/show-tuning_strategy-method.md)
  :

  Show Method for `tuning_strategy`

- [`show(`*`<turnover_constraint_policy>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/show-turnover_constraint_policy-method.md)
  : Show Turnover Constraint Policy

- [`summary(`*`<meta_dataframe>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/summary-meta_dataframe.md)
  : Summary for meta_dataframe

- [`summary(`*`<meta_xts>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/summary-meta_xts-method.md)
  :

  Summary Method for `meta_xts` Objects

- [`summary(`*`<port_backtest_cohort>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/summary-port_backtest_cohort-method.md)
  : Summary Method for port_backtest_cohort Class

- [`summary(`*`<port_backtest_results>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/summary-port_backtest_results-method.md)
  : Summary Method for port_backtest_results Class

- [`summary(`*`<sb_backtest_results>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/summary-sb_backtest_results-method.md)
  : Summary Method for sb_backtest_results Class

- [`summary(`*`<sb_metabacktest_config>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/summary-sb_metabacktest_config-method.md)
  : Summary Method for sb_metabacktest_config Class

- [`summary(`*`<sb_metabacktest_results>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/summary-sb_metabacktest_results-method.md)
  : Summary Method for sb_metabacktest_results Class

- [`summary(`*`<ss_backtest_results>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/summary-ss_backtest_results-method.md)
  : Summary Method for ss_backtest_results Class

- [`summary(`*`<tickers_catalog>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/summary-tickers_catalog-method.md)
  : Summary Method for tickers_catalog Class

- [`plot(`*`<bayesian_alpha_test_strategy>`*`,`*`<ANY>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/plot-bayesian_alpha_test_strategy-ANY-method.md)
  : Plot Bayesian Alpha Test Strategy Priors

- [`plot(`*`<bayesian_opt_strategy>`*`,`*`<missing>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/plot-bayesian_opt_strategy-missing-method.md)
  :

  Plot Method for `bayesian_opt_strategy`

- [`plot(`*`<grid_search_strategy>`*`,`*`<missing>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/plot-grid_search_strategy-missing-method.md)
  :

  Plot Method for `grid_search_strategy`

- [`plot(`*`<meta_dataframe>`*`,`*`<missing>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/plot-meta_dataframe.md)
  : Plot meta_dataframe

- [`plot(`*`<port>`*`,`*`<missing>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/plot-port-missing-method.md)
  : Plot Method for 'port' Objects

- [`plot(`*`<port_backtest_cohort>`*`,`*`<ANY>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/plot-port_backtest_cohort-ANY-method.md)
  : Plot Method for port_backtest_cohort Class

- [`plot(`*`<port_backtest_config>`*`,`*`<missing>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/plot-port_backtest_config-missing-method.md)
  : Plot Method for port_backtest_config: Faceted Liquidity Floor
  Cutoffs (Ordered Within Facets)

- [`plot(`*`<port_backtest_results>`*`,`*`<ANY>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/plot-port_backtest_results-ANY-method.md)
  : Plot Method for port_backtest_results Class

- [`plot(`*`<random_search_strategy>`*`,`*`<missing>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/plot-random_search_strategy-missing-method.md)
  :

  Plot Method for `random_search_strategy`

- [`plot(`*`<sb_backtest_config>`*`,`*`<missing>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/plot-sb_backtest_config-missing-method.md)
  :

  Plot Method for `sb_backtest_config`

- [`plot(`*`<sb_backtest_results>`*`,`*`<ANY>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/plot-sb_backtest_results-ANY-method.md)
  : Plot Signal Blending Walk-Forward Validation Results

- [`plot(`*`<sb_metabacktest_results>`*`,`*`<ANY>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/plot-sb_metabacktest_results-ANY-method.md)
  : Plot Method for sb_metabacktest_results Class

- [`plot(`*`<sb_model>`*`,`*`<missing>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/plot-sb_model-missing-method.md)
  : Plot Method for 'sb_model' Objects

- [`plot(`*`<ss_backtest_results>`*`,`*`<ANY>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/plot-ss_backtest_results-ANY-method.md)
  : Plot Method for ss_backtest_results Class

- [`plot(`*`<meta_xts>`*`,`*`<missing>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/plot_meta_xts.md)
  : Plot Method for meta_xts

- [`plot(`*`<ss_backtest_config>`*`,`*`<ANY>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/plot_ss_backtest_config.md)
  : Plot Priors from ss_backtest_config

- [`predict(`*`<sb_backtest_results>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/predict-sb_backtest_results-method.md)
  : Predict Method for sb_backtest_results Class

- [`predict(`*`<sb_model>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/predict-sb_model-method.md)
  : Predict Method for sb_model Class and meta_dataframe

- [`predict(`*`<signal_port>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/predict-signal_port-method.md)
  : Predict method for signal_port class

- [`as.data.frame(`*`<meta_dataframe>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/as.data.frame-meta_dataframe-method.md)
  : Coerce a meta_dataframe Object to Data Frame

- [`as.data.frame(`*`<meta_xts>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/as.data.frame-meta_xts-method.md)
  : Coerce a meta_xts Object to Data Frame

- [`as.list(`*`<concentration_constraint_policy>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/as.list-concentration_constraint_policy-method.md)
  : Turn a concentration_constraint_policy into a list

- [`as.list(`*`<keras_architecture_parameters>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/as.list-keras_architecture_parameters-method.md)
  : Convert Keras Architecture Parameters to List

- [`as.list(`*`<liquidity_constraint_policy>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/as.list-liquidity_constraint_policy-method.md)
  : as.list Method for liquidity_constraint_policy S4 Class

- [`as.list(`*`<transaction_costs_parameters>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/as.list-transaction_costs_parameters-method.md)
  : as.list Method for transaction_costs_parameters S4 Class

- [`as.list(`*`<turnover_constraint_policy>`*`)`](https://pauloguimaraes871.github.io/factoRverse/reference/as.list-turnover_constraint_policy-method.md)
  : as.list Method for turnover_constraint_policy S4 Class

## Operators

- [`` `%>%` ``](https://pauloguimaraes871.github.io/factoRverse/reference/pipe.md)
  : Pipe operator
- [`` `%dofuture%` ``](https://pauloguimaraes871.github.io/factoRverse/reference/dofuture.md)
  : dofuture operator
