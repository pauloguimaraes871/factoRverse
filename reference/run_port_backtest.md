# Run Portfolio Backtest

Run Portfolio Backtest

## Usage

``` r
run_port_backtest(
  signals_m_df,
  fwd_return_m_df,
  liquidity_m_df,
  volatility_m_df,
  config,
  ...
)

# S4 method for class 'meta_dataframe,meta_dataframe,meta_dataframe,meta_dataframe,port_backtest_config'
run_port_backtest(
  signals_m_df,
  fwd_return_m_df,
  liquidity_m_df,
  volatility_m_df,
  config,
  sb_backtest_results = NULL,
  scaler_m_df = NULL,
  stock_groups_m_df = NULL,
  benchmark_weights_m_df = NULL,
  daily_stock_returns_m_xts = NULL,
  daily_bench_returns_m_xts = NULL,
  benchmark_returns_m_xts = NULL,
  target_port_m_df = NULL,
  custom_stock_weights_m_df = NULL,
  custom_stock_metrics_m_df = NULL,
  user_defined_OR_rules_m_df = NULL,
  user_defined_AND_rules_m_df = NULL,
  winsorization_probs = c(0.025, 0.975),
  verbose = TRUE,
  parallel = TRUE,
  .test_seed = NULL,
  .update = FALSE,
  .old_backtest_port_weights_m_d_ref = NULL,
  .old_backtest_port_returns_m_xts = NULL,
  .old_backtest_port_costs_d_ref = NULL,
  .old_backtest_covered_dates = NULL
)
```

## Arguments

- signals_m_df:

  A `meta_dataframe` containing alpha signals. Must include columns
  `id`, `tickers`, and `dates`.

- fwd_return_m_df:

  A `meta_dataframe` of forward returns (e.g., 1M-ahead).

- liquidity_m_df:

  A `meta_dataframe` with liquidity metrics.

- volatility_m_df:

  A `meta_dataframe` with volatility metrics (e.g., 1M historical vol).

- config:

  A `port_backtest_config` object defining portfolio construction logic
  and constraints.

- ...:

  Additional arguments passed to class-specific methods (e.g., cohort or
  single backtests).

- sb_backtest_results:

  (Optional) An `sb_backtest_results` or `sb_metabacktest_results`
  object. If provided, its predictions are used in place of signals.

- scaler_m_df:

  A meta_dataframe containing information to scale exp_ret_score

- stock_groups_m_df:

  (Optional) Sector or group data for use in group constraints.

- benchmark_weights_m_df:

  (Optional) Benchmark stock weights.

- daily_stock_returns_m_xts:

  (Optional) Daily stock returns for covariance estimation.

- daily_bench_returns_m_xts:

  (Optional) Daily benchmark returns (only if active returns are used).

- benchmark_returns_m_xts:

  (Optional) Monthly benchmark returns, used to compute active returns
  and benchmark-relative metrics.

- target_port_m_df:

  (Optional) Target portfolio weights for shrinkage.

- custom_stock_weights_m_df:

  (Optional) User-defined portfolio weights (used only with
  `port_construction_method = "custom_weights"`).

- custom_stock_metrics_m_df:

  (Optional) Additional metrics to be aggregated in the portfolio.

- user_defined_OR_rules_m_df:

  (Optional) Rules that override stock eligibility if any OR condition
  is met.

- user_defined_AND_rules_m_df:

  (Optional) Rules that override stock eligibility only if all
  conditions are met.

- winsorization_probs:

  Numeric vector of length 2 (default = c(0.025, 0.975)). Determines
  quantiles used to winsorize signals.

- verbose:

  Logical. If `TRUE`, prints progress logs and diagnostic information.
  Default is `TRUE`.

- parallel:

  Logical. If `TRUE`, runs computation in parallel. Default is `TRUE`.

- .test_seed:

  (Internal) Seed used during testing to ensure reproducibility. Default
  is `NULL`.

- .update:

  (Internal) Logical; whether this is an update to an existing backtest.
  Default is `FALSE`.

- .old_backtest_port_weights_m_d_ref:

  (Internal) Previously computed portfolio weights (used when
  `.update = TRUE`).

- .old_backtest_port_returns_m_xts:

  (Internal) Previously computed return series (used when
  `.update = TRUE`).

- .old_backtest_port_costs_d_ref:

  (Internal) Previously computed cost series (used when
  `.update = TRUE`).

- .old_backtest_covered_dates:

  (Internal) Dates already covered in the previous backtest (used when
  `.update = TRUE`).

## Value

An object of class `port_backtest_results`, containing:

- `port_weights_m_df`: Portfolio weights by stock and date.

- `transactions_log`: Transaction log with costs and weights.

- `port_costs_m_xts`: Time series of cost metrics (turnover, market
  impact, etc.).

- `port_returns_m_xts`: Net and raw portfolio returns (and
  benchmark-relative returns if applicable).

- `port_metrics_m_xts`: Aggregated portfolio metrics (if
  `custom_stock_metrics_m_df` is supplied).

- `stock_universe_m_df`: Data frame with signal scores, eligibility
  flags, and classification for each stock.

- `port_stats_m_df`: Time series of portfolio (and, when applicable,
  group and active) analytics per rebalance date.

- `final_stock_port`: The `stock_port` object for the last rebalance
  date (final weights, covariance, RRC, macro/benchmark sub-portfolios).

- `port_backtest_workflow`: A list tracking workflow metadata, inputs,
  and date coverage.

- `backtest_identifier`: A character identifier of the form
  `"c__<config>_s__<signals>_f__<fwd_return>"`.

## Details

Main entry point for portfolio simulations in the `factoRverse`
ecosystem. This generic function defines the interface and documentation
for `run_port_backtest()`.

This is the stock-level portfolio construction and backtesting engine of
the `factoRverse` ecosystem. It is used at two points of a quantitative
workflow: (a) when designing backtests for an individual
signal/characteristic, and (b) when building a final portfolio out of a
blended signal. Accordingly, the expected-return score for each stock
comes from one of two mutually exclusive sources:

- a single characteristic column of `signals_m_df`, selected via
  `config@chosen_score_metric_and_position` (a named vector such as
  `c(book_yield = "long")`); or

- the `pred` column of an
  `sb_backtest_results`/`sb_metabacktest_results` object passed to
  `sb_backtest_results`, i.e. the out-of-sample output of
  [`run_sb_backtest()`](https://pauloguimaraes871.github.io/factoRverse/reference/run_sb_backtest.md).
  Only `id`, `tickers`, `dates`, and `pred` are extracted (the `target`
  column is dropped) to guard against look-ahead leakage.

The engine then walks forward date by date, at each rebalance deriving
the stock universe (`derive_stock_universe_m_d_ref`), classifying
eligibility (`classify_investment_universe`), assigning weights
(`set_portfolio_weights`), allocating with trade orders and costs
(`allocate_port`), computing metrics, and rolling weights and returns to
the next period (`roll_port`).

The function integrates multiple components:

### 1. **Stock Classification**

- If `asset_object = "stocks"`: Each asset is scored based on an
  `exp_ret_score`, and only those inside a defined quantile range (e.g.,
  top 20%) are considered "pre-eligible". If too few assets are
  selected, a fallback expands the quantile range iteratively until a
  minimum number of assets is included or a maximum range width is
  reached.

### 2. **Eligibility Filtering**

A stock is promoted to the final investment universe
(`filtered_universe`) if it satisfies at least one of several
eligibility criteria:

#### Regular Eligibility:

- **Quantile Rule**: The asset is within the top quantile of
  `exp_ret_score`.

- **Liquidity Floor Rule**: The asset meets a minimum liquidity
  threshold based on predefined liquidity classifications (e.g.,
  micro_caps).

#### Policy-Based Eligibility:

- **Turnover Policy**: Assets in buffer zones from the previous
  portfolio can be retained even if they fall outside the quantile rule.

- **Active Weights Constraint**: Assets with benchmark weights above the
  active weight threshold are automatically included.

- **Group Representativeness**: If no asset from a required group (e.g.,
  sector, theme) is eligible, the top-scoring asset from that group is
  forcibly promoted to preserve group balance.

#### Custom Rules:

- **user_defined_OR_rules**: Stocks matching *any* custom inclusion rule
  are always promoted.

- **user_defined_AND_rules**: Stocks that fail *any* custom exclusion
  rule are removed regardless of other criteria.

#### Rule Hierarchy:

- **Active Weights Constraint** overrides all other filters.

- **Turnover Policy** takes precedence over liquidity rules.

- **OR rules** force inclusion, **AND rules** force exclusion.

- **Group representativeness** is a fallback to ensure all relevant
  themes or sectors are represented.

### 3. **Benchmark Construction**

The user may supply benchmark weights explicitly.

### 4. **Portfolio Construction**

After defining the filtered investment universe:

- The portfolio is constructed based on signal strength (via
  `exp_ret_score`), user-specified optimization policies, or equal
  weighting.

- Constraints (e.g., turnover, group weight limits) are applied at this
  stage, potentially using box or group constraints via
  [`generate_box_constraints()`](https://pauloguimaraes871.github.io/factoRverse/reference/generate_box_constraints.md)
  and
  [`generate_group_constraints()`](https://pauloguimaraes871.github.io/factoRverse/reference/generate_group_constraints.md).

### 5. **Parallel and Verbose Execution**

The function supports verbose output and parallel execution to improve
transparency and computational efficiency when using heavy port
construction methods, such as MVO.

### 6. **Portfolio Analytics, Transaction Costs and Delisting Realism**

At every rebalance the engine computes a rich set of portfolio and
benchmark analytics (via `calculate_port_stats`) at both the micro
(per-asset) and macro (group/sector) levels: concentration (HHI,
effective N, entropy, Gini, top-k), diversification ratio,
weighted-average pairwise correlation, gross/net exposure, and relative
risk-contribution measures (RRC HHI, effective N, distance-to-ERC) —
plus active (`act_`, information-ratio) variants whenever a benchmark is
supplied. Trading is modelled realistically: `allocate_port` sizes trade
orders and applies a market-impact cost model
(`calculate_transaction_costs`, with static or `"dynamic"` lambda) on
top of direct costs, and keeps a full transaction log. Delisted and
newly listed (IPO) names are handled explicitly when rolling and
rebalancing — delisted holdings are unwound (zero target weight, imputed
liquidity/volatility), IPOs enter with zero weight, and surviving names
are carried forward by `roll_port`.

## Methods (by class)

- `run_port_backtest( signals_m_df = meta_dataframe, fwd_return_m_df = meta_dataframe, liquidity_m_df = meta_dataframe, volatility_m_df = meta_dataframe, config = port_backtest_config )`:
  Run Portfolio Backtest with Signal Filtering and Eligibility Rules

  Executes a full pipeline portfolio backtest, from asset/signal
  classification and eligibility filtering to benchmark construction and
  weight optimization, based on expected return scores (`exp_ret_score`)
  or signal performance. This function is designed to incorporate
  realistic portfolio constraints and rules—such as turnover, liquidity,
  group representativeness, and user-defined filters—while also
  supporting fallback logic to ensure a viable investment universe is
  selected in each rebalance date.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Single-signal (book-yield) equal-weighted backtest against the ibov benchmark.
  config <- create_port_backtest_config(
    chosen_score_metric_and_position = c(book_yield = "long"),
    eligibility_quantile_range = c(0.8, 1.0),
    initial_buffer_period = 12,
    rebalancing_months = c(6, 12),
    selected_benchmark = "ibov",
    main_liquidity_metric = "mean_volfin_3m",
    port_construction_method = "ew"
  )

  results <- run_port_backtest(
    signals_m_df           = signals_m_df,
    fwd_return_m_df        = fwd_return_m_df,
    liquidity_m_df         = liquidity_m_df,
    volatility_m_df        = volatility_m_df,
    config                 = config,
    benchmark_weights_m_df = benchmark_weights_m_df,
    benchmark_returns_m_xts = benchmark_returns_m_xts,
    verbose = TRUE, parallel = FALSE
  )

  # Alternatively, drive the backtest from a blended signal (run_sb_backtest output):
  # results <- run_port_backtest(signals_m_df, fwd_return_m_df, liquidity_m_df,
  #                              volatility_m_df, config, sb_backtest_results = sb_results)
} # }
```
