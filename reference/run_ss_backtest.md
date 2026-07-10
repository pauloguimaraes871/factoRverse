# Run Signal Selection Backtest

Performs out-of-sample testing of alpha signal selection across time
using statistical methods (frequentist or Bayesian). It filters and
ranks signals based on their predictive power, typically using
regression-based techniques.

## Usage

``` r
run_ss_backtest(
  config,
  signals_m_df,
  backtest_returns_m_xts,
  port_backtest_cohort,
  benchmark_returns_m_xts,
  signal_themes_m_df,
  ...
)

# S4 method for class 'ss_backtest_config,meta_dataframe,missing,port_backtest_cohort,meta_xts,meta_dataframe'
run_ss_backtest(
  config,
  signals_m_df,
  port_backtest_cohort,
  benchmark_returns_m_xts,
  signal_themes_m_df,
  priors_m_df = NULL,
  custom_signal_universe_metrics_m_df = NULL,
  verbose = TRUE,
  parallel = TRUE,
  winsorization_probs = c(0.025, 0.975)
)

# S4 method for class 'ss_backtest_config,meta_dataframe,meta_xts,missing,meta_xts,meta_dataframe'
run_ss_backtest(
  config,
  signals_m_df,
  backtest_returns_m_xts,
  benchmark_returns_m_xts,
  signal_themes_m_df,
  priors_m_df = NULL,
  custom_signal_universe_metrics_m_df = NULL,
  verbose = TRUE,
  parallel = TRUE,
  winsorization_probs = c(0.025, 0.975),
  .update = FALSE
)
```

## Arguments

- config:

  An object of class `ss_backtest_config`, specifying the parameters of
  the signal selection backtest, including methodology and thresholds.

- signals_m_df:

  A `meta_dataframe` containing alpha signals (columns), identified by
  `tickers`, with `id` and `dates` columns.

- backtest_returns_m_xts:

  A `meta_xts` containing signal backtest returns (xts format). Must
  align with `signals_m_df` and include one column per signal.

- port_backtest_cohort:

  A `port_backtest_cohort` object. Used to extract historical return
  data via
  [`extract_returns_m_xts()`](https://pauloguimaraes871.github.io/factoRverse/reference/extract_returns_m_xts.md).

- benchmark_returns_m_xts:

  A `meta_xts` object with benchmark returns. Used to compute active
  returns for each signal.

- signal_themes_m_df:

  A `meta_dataframe` that maps each signal to a theme. Includes columns:
  `id`, `tickers`, `dates`, and `theme`.

- ...:

  Additional arguments passed to the function.

- priors_m_df:

  Optional. A `meta_dataframe` of priors used for Bayesian model
  specification. Required if `p_correction_method = "bayesian"` and
  `user_priors` are not supplied.

- custom_signal_universe_metrics_m_df:

  Optional. A `meta_dataframe` with additional signal-level metrics,
  used to influence eligibility.

- verbose:

  A boolean indicating whether to print messages.

- parallel:

  A boolean indicating whether to use parallel processing.

- winsorization_probs:

  Numeric vector of length 2. Defines lower and upper quantiles for
  winsorizing signal values. Default is `c(0.025, 0.975)`.

- .update:

  (Internal) Logical. If `TRUE`, updates a previous backtest
  incrementally. Default is `FALSE`.

## Value

An object of class `ss_backtest_results`, with slots:

- `ss_backtest_config`: the `ss_backtest_config` used to run the
  backtest.

- `signal_universe_m_df`: a `signal_universe_m_df` stacking the signal
  eligibility results at every rebalancing period.

- `final_signal_universe_m_d_ref`: a `signal_universe_m_df` with the
  last (most recent) signal universe snapshot, i.e. the eligible signals
  handed to
  [`run_sb_backtest()`](https://pauloguimaraes871.github.io/factoRverse/reference/run_sb_backtest.md).

- `selected_market_factor_proxy_m_xts`: the `meta_xts`
  market-factor-proxy series used to compute alphas.

- `frequentist_results`, `bayesian_results`: per-rebalancing lists of
  the fitted frequentist (`lm`/`lmer`) and Bayesian (`brms`) model
  outputs (whichever applies).

- `p_correction_method`: the multiple-testing correction method applied.

- `ss_backtest_workflow`: a list of per-batch execution metadata (object
  names, dates, and config).

- `backtest_identifier`: a character identifier for the backtest.

- `update`: logical flag indicating whether the object was produced by
  an incremental update.

## Details

This function performs iterative signal selection based on frequentist
or Bayesian methods, which are applied to portfolio returns backtests to
identify which signals can be considered significant in stock-level
return prediction.

To determine whether a signal matters in cross-sectional predictability,
the literature typically runs regressions of signal portfolios against a
benchmark factor model (e.g., CAPM), computing alphas and corresponding
t-stats. Due to the large number of signals identified in the literature
(the "factor zoo"), methods to control for multiple testing are often
advocated.

The backtest proceeds walk-forward, and at each rebalancing date it: (a)
takes the characteristic-portfolio backtest returns in
`backtest_returns_m_xts` (built, e.g., from a book-yield portfolio); (b)
tests the zero-alpha null against the market-factor proxy under the
chosen inference method (frequentist `no_pooled` OLS or `partial_pooled`
hierarchical [`lme4::lmer`](https://rdrr.io/pkg/lme4/man/lmer.html), or
Bayesian `brms`), controlling for multiple testing (FWER:
bonferroni/holm/hochberg/hommel; FDR: BH/fdr/BY; or Bayesian shrinkage);
and (c) classifies signals into an eligible universe. The eligible
signals are then passed to
[`run_sb_backtest()`](https://pauloguimaraes871.github.io/factoRverse/reference/run_sb_backtest.md),
which blends them into a final signal.

## Functions

- `run_ss_backtest( config = ss_backtest_config, signals_m_df = meta_dataframe, backtest_returns_m_xts = missing, port_backtest_cohort = port_backtest_cohort, benchmark_returns_m_xts = meta_xts, signal_themes_m_df = meta_dataframe )`:
  Wrapper method that internally derives `backtest_returns_m_xts` from a
  `port_backtest_cohort` object.

- `run_ss_backtest( config = ss_backtest_config, signals_m_df = meta_dataframe, backtest_returns_m_xts = meta_xts, port_backtest_cohort = missing, benchmark_returns_m_xts = meta_xts, signal_themes_m_df = meta_dataframe )`:
  Main method. Runs a full signal selection backtest using either
  Bayesian or frequentist statistical methods.

## Bayesian Hierarchical Model

One way of introducing shrinkage to alpha estimates from signal
portfolios is by using bayesian statistics, which might be specially
useful in the context of small samples of strategy returns. Bayesian
statistics allows for the incorporation of prior information about the
parameters of interest (alpha and beta), which can be particularly
useful in multiple testing.

A Bayesian model depends on parameters that are themselves random
variables. For instance, one can assume a normal distribution for the
CAPM alpha parameter, with a mean of 0 (the strategy is not profitable)
and a given standard deviation, effectively shrinking posterior
estimates towards the prior mean, with an intensity that depends on the
prior standard deviation and the sample distribution. Alternatively, one
can utilize priors derived from signal strategies from other countries
or asset classes than the ones being studied. Either way, the researcher
will incorporate the reasoning process people usually have, as we
usually have prior beliefs about a subject and then will update our
beliefs based on new information. If another research possesses other
priors, he or she can incorporate those into Bayes rule and derive other
posterior distribution for parameters of interest, which is kind of more
transparent than the frequentist approach, who will inherently give
ultimate importance to associational evidence from (possibly overfit)
data. Most of the time, in a frequentist setting, the researcher usually
does not disclose the reasoning process behind the model. Therefore,
bayesian statistics are being often recommended as a method to deal with
multiple testing problems.

Suppose one has return observations for multiple signal portfolios
across time and wants to estimate the parameters of a single-factor
model (CAPM) that describes the relationship between these signals
portfolios and the market factor, being interested in the significance
of the alpha parameter, while accounting for its exposure to systematic
market risk. A complete pooled model will treat all observations as
being independent and treating as irrelevant any information about
individual strategies, which might be misleading, because it does not
account for the hierarchical structure of the data. Observations of a
given signal portfolio are possible correlated. On the other hand, a no
pooling model will estimate a separate model for each signal portfolio,
fitting specific parameters for each individual strategy, which might be
inefficient, as it does not borrow information across signals. Signals
belonging to a given theme tend to be similar. Signals in the value
theme, for instance, are usually valuation multiples (eg book yield,
earnings yield, fcf yield, sales yield etc) and so are very similar,
thus it would be unfortunate to ignore information from other value
strategies when analyzing, for example, book yield alpha. By fitting a
hierarchical model, alpha estimates are first shrunk towards theme mean
and then towards priors. In this setting, there is top layer represented
by the population of all signal strategies under a theme (eg. the value
theme alpha) and a second layer for the backtested strategies that fall
under that theme, for which he have repeated observations.

By using a hierarchical model, one can study both within-signal
variability, examining how consistent the signal is across time, and
between-signal variability, examining how performance patterns vary
across different signals in a theme. Total variance is given by sum of
within-signal variance and between-signal variance.

Threfore, there are three layers for R_s,t (t-th observation of return
of signal s):

- Signal-specific layer:R_s,t ~ N(alpha_s + beta_s \* R_m,t, sigma_s^2).
  This represents how returns vary within strategy s

- Theme-specific layer:alpha_s ~ N(alpha_theme, sigma_a_theme^2); beta_s
  ~ N(beta_theme, sigma_b_theme^2). This represents how the typical
  alpha/beta vary across strategies in a theme

- Priors:alpha_theme, sigma_a_theme, beta_theme, sigma_b_theme, sigma_s.
  Global parameters shared between strategies.

More specifically, signal-specific mean parameters are treated as
deviations from global parameters (u_1, u_2 and u_sigma for alpha, beta
and sigma respectively, with corresponding mean and standard deviations
mu_u1, tau_u1, mu_u2, tau_u2, mu_u_sigma and tau_u_sigma). If the user
provide a `priors_m_df`, the function will, for each theme in respective
column (that should match possible options in `signal_themes_m_df`):

- For each date, calculate the average and standard deviation of
  individual signals alphas/betas/sigmas. Based on these average values,
  a prior for overall alpha parameters will be chosen based on maximum
  likelihood

- For each date, calculate the differential of individual signals
  alpha/betas/sigmas from overall counterparts. For alpha, this means
  getting mu_u1 and tau_u1, mean and standard deviation of differentials
  of individual signals to overall mean. In particular, tau_u1 measures
  alpha variability between signals, the dispersion of differentials of
  individual signals from theme alpha.

- Use maximum likelihood to derive priors for each tau and also for
  correlation, according to `priors_type`

For location parameters, priors are chosen between normal and t
distributions, given the option that minimizes BIC. For scale
parameters, other candidate distributions are cauchy, inverse-gamma and
log-normal. For correlation, the LKJ distribution is used.

By considering the hierarchical structure of the data, it is possible to
borrow information across signals and, thus, hierarchical models are
better at balancing bias and variance than estimates from complete
pooling (high bias) or no pooling models (high variance).

To speed up computation, Bayesian models are fitted in parallel using
the `future` framework.

## Signal Engineering Benchmarks

The process of generating a final signal (also known as Signal
Engineering) incorporates two steps:

- **Signal Selection**: Selecting signals deemed significant based on a
  hypothesis testing zero-alpha null-hypothesis rejection criteria
  applied to associated signal portfolios returns in
  `backtest_returns_m_xts`.

- **Signal Blending**: Blending selected signals into a final signal
  used to generate the final portfolio at the stock level.

The Signal Engineering Benchmarks (SE Benchmarks) evaluate the
performance of both steps:

- **Signal Selection Benchmark**: Built using the universe of all
  signals in `chosen_signals`. It evaluates the performance of the
  signal selection process.

- **Signal Blending Benchmark**: Built using only signals derived from
  the signal selection process. It evaluates the performance of the
  signal blending process.

Comparing predictive and return performance of the SS and SB Benchmarks
provides insights into the effectiveness of the signal selection
process. Additionally, comparing performance between the SB Benchmark
and the final portfolio evaluates the performance of the signal blending
process. SE Benchmarks are built based on themes; weights are first
equally distributed among themes and then equally distributed among
signals within each theme.

## Examples

``` r
if (FALSE) { # \dontrun{
# Frequentist, hierarchical (partial-pooled) alpha test with FDR (BH) control
alpha_strategy <- create_alpha_test_strategy(
  model_structure = "partial_pooled",
  theme_level_intercept = "theme_specific", theme_level_slope = "fixed",
  p_correction_method = "BH", signal_significance_threshold = 0.05,
  market_factor_proxy = "IBOV"
)
ss_config <- create_ss_backtest_config(
  initial_sample_size = 36, rebalancing_months = 1:12,
  alpha_test_strategy = alpha_strategy
)
ss_results <- run_ss_backtest(
  config = ss_config, signals_m_df = signals_m_df,
  backtest_returns_m_xts = backtest_returns_m_xts,
  benchmark_returns_m_xts = benchmark_returns_m_xts,
  signal_themes_m_df = signal_themes_m_df
)
} # }
```
