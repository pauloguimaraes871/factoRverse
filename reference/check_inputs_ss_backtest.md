# Perform Validation Checks on Inputs for Signal Selection (SS) Workflow

This function validates and checks all inputs required for running the
[`run_ss_backtest()`](https://pauloguimaraes871.github.io/factoRverse/reference/run_ss_backtest.md)
function, ensuring structural consistency, completeness, and proper
formatting for backtest returns, benchmark data, signal metadata, and
model configuration.

## Usage

``` r
check_inputs_ss_backtest(
  initial_sample_size,
  rebalancing_months,
  split_method,
  signals_m_df,
  chosen_signals_and_positions,
  forced_signals,
  custom_signal_universe_metrics_m_df,
  backtest_returns_m_xts,
  benchmark_returns_m_xts,
  market_factor_proxy,
  active_returns,
  p_correction_method,
  signal_significance_threshold,
  enable_theme_representativeness,
  model_structure,
  theme_level_intercept,
  theme_level_slope,
  lmer_control,
  priors_m_df,
  user_priors,
  brms_control,
  prior_derivation_control,
  signal_themes_m_df,
  lower_quantile_winsorization,
  upper_quantile_winsorization
)
```

## Arguments

- initial_sample_size:

  Numeric. The minimum number of observations required before the first
  backtest rebalancing can be executed.

- rebalancing_months:

  Numeric. Vector of months (1–12) when signal selection should be
  implemented.

- split_method:

  Character. Method used to split sample data in the backtest (e.g.,
  expanding or rolling).

- signals_m_df:

  A (meta) data frame with columns `id`, `tickers`, `dates`, and
  additional numeric columns representing signals. Should not contain
  NAs or columns prefixed with `"low_"`.

- chosen_signals_and_positions:

  A named character vector specifying selected signals and their
  positions. Example: `c(book_to_market = "long", vol_12m = "short")`.

- forced_signals:

  A named character vector of signals to be forcibly included. Values
  must be `"force"`.

- custom_signal_universe_metrics_m_df:

  A meta-data frame of custom metrics (optional). Must contain rows
  equal to the number of signals times number of dates, and should not
  include columns from the default performance metric output.

- backtest_returns_m_xts:

  An `xts` object with signal-specific backtested returns. Column names
  must correspond to the signals used.

- benchmark_returns_m_xts:

  An `xts` object containing benchmark returns, aligned with the dates
  of `backtest_returns_m_xts`.

- market_factor_proxy:

  Character string. Name of the column in `benchmark_returns_m_xts` to
  be used as the market factor.

- active_returns:

  Logical. If `TRUE`, returns are adjusted by subtracting benchmark
  returns before performance evaluation.

- p_correction_method:

  Character. Specifies the method for p-value correction. Options
  include:

  - `"none"`

  - `"bayesian"`

  - Frequentist FWER: `"bonferroni"`, `"holm"`, `"hochberg"`, `"hommel"`

  - Frequentist FDR: `"BH"`, `"fdr"`, `"BY"`

- signal_significance_threshold:

  Numeric. Threshold for selecting signals based on CAPM alpha
  significance (e.g., 0.05). Use 1 to bypass selection threshold.

- enable_theme_representativeness:

  Logical. If `TRUE`, ensures at least one signal per theme is selected,
  based on the highest t-statistic.

- model_structure:

  Character. Either `"partial_pooled"` or `"no_pooled"` to control
  hierarchical modeling strategy.

- theme_level_intercept:

  Character. Specifies how intercepts are modeled at the theme level
  (used if `model_structure == "partial_pooled"`).

- theme_level_slope:

  Character. Specifies how slopes are modeled at the theme level (used
  if `model_structure == "partial_pooled"`).

- lmer_control:

  A list with options passed to
  [`lme4::lmer`](https://rdrr.io/pkg/lme4/man/lmer.html). Accepted
  elements:

  - `lmer_optimizer`: Optimization method. One of `"nloptwrap"`,
    `"bobyqa"`, `"Nelder_Mead"`, `"nlminbwrap"`.

  - `lmer_optimization_objective`: Either `"REML"` or `"likelihood"`.

  - `hierarchical_p_value_method`: P-value method. One of
    `"Satterthwaite"`, `"Kenward-Roger"`, `"lme4"`.

- priors_m_df:

  A (meta) data frame with columns `id`, `tickers`, `dates`, `return`,
  `market_factor_proxy`, and `theme`. Used to derive informative priors.

- user_priors:

  A `data.frame` of `brmsprior` objects. Must match expected structure
  according to `model_spec_theme_level`.

- brms_control:

  A list of parameters passed to
  [`brms::brm`](https://paulbuerkner.com/brms/reference/brm.html):

  - `chains`: Integer, number of MCMC chains (default 4).

  - `iter`: Integer, iterations per chain (default 2000).

  - `warmup`: Integer, warm-up iterations (default `floor(iter / 2)`).

  - `thin`: Integer, thinning interval (default 1).

  - `seed`: Integer or `NA`, for reproducibility (default `NA`).

  - `adapt_delta`: Numeric between 0 and 1 (default 0.99).

- prior_derivation_control:

  A list of controls for frequentist prior derivation:

  - `half_t_df`: Degrees of freedom for half-t priors on scale
    parameters.

- signal_themes_m_df:

  A (meta) data frame with columns `id`, `tickers`, `dates`, and
  `theme`. Defines signal group structure for hierarchical modeling.

- lower_quantile_winsorization:

  Numeric between 0 and 1. Lower winsorization threshold for signal
  metrics.

- upper_quantile_winsorization:

  Numeric between 0 and 1. Upper winsorization threshold for signal
  metrics.
