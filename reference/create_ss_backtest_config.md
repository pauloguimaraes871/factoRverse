# Create an ss_backtest_config Object

This function constructs an object of class `ss_backtest_config`,
ensuring the proper initialization and validation of its slots.

## Usage

``` r
create_ss_backtest_config(
  initial_sample_size,
  rebalancing_months,
  active_returns = TRUE,
  split_method = "expanding",
  alpha_test_strategy = NULL,
  config_name = "not_identified",
  chosen_signals_and_positions = "all"
)
```

## Arguments

- initial_sample_size:

  A numeric indicating the minimum number of observations required to
  begin the backtest.

- rebalancing_months:

  A numeric vector of calendar months (each in 1–12) at which signal
  selection is executed during the walk-forward backtest.

- active_returns:

  Logical, whether to calculate active returns when calculating
  performance metrics, except for CAPM (default is TRUE).

- split_method:

  A character string specifying the splitting method, either "expanding"
  (default) or "rolling".

- alpha_test_strategy:

  An `alpha_test_strategy` object — a `frequentist_alpha_test_strategy`
  or `bayesian_alpha_test_strategy` built with
  [`create_alpha_test_strategy()`](https://pauloguimaraes871.github.io/factoRverse/reference/create_alpha_test_strategy.md)
  — defining the alpha test configuration. May be `NULL`.

- config_name:

  A character string naming the configuration.

- chosen_signals_and_positions:

  A character vector specifying the chosen signals and positions. If set
  to "all", all signals in `signals_m_df` will be used, and a long
  position will be assumed for all.

## Value

An object of class `ss_backtest_config`.

## Examples

``` r
if (FALSE) { # \dontrun{
alpha_strategy <- create_alpha_test_strategy(
  model_structure = "no_pooled", p_correction_method = "holm",
  market_factor_proxy = "IBOV"
)
ss_config <- create_ss_backtest_config(
  initial_sample_size = 36, rebalancing_months = 1:12,
  alpha_test_strategy = alpha_strategy, config_name = "ss_holm_nopool"
)
} # }
```
