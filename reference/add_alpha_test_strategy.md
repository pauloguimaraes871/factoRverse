# Add Alpha Test Strategy to ss_backtest_config

This method allows you to add an `alpha_test_strategy` object to an
`ss_backtest_config` object.

## Usage

``` r
add_alpha_test_strategy(object, alpha_test_strategy, ...)

# S4 method for class 'ss_backtest_config,alpha_test_strategy'
add_alpha_test_strategy(object, alpha_test_strategy)

# S4 method for class 'ss_backtest_config,missing'
add_alpha_test_strategy(
  object,
  signal_significance_threshold = 0.05,
  p_correction_method = "none",
  market_factor_proxy,
  model_structure = "partial_pooled",
  theme_level_intercept = NULL,
  theme_level_slope = NULL,
  enable_theme_representativeness = TRUE,
  bayesian_model_parameters = NULL,
  lmer_control = NULL
)
```

## Arguments

- object:

  An `ss_backtest_config` object.

- alpha_test_strategy:

  An `alpha_test_strategy` object to be added.

- ...:

  Additional arguments to be passed to the method.

- signal_significance_threshold:

  A decimal indicating the hypothesis testing negative-alpha
  null-hypothesis rejection criteria. If one wants to select all
  chosen_signals, provide 1. In any case, a signal being selected
  demands a significant CAPM alpha.

- p_correction_method:

  The method for p-value correction. Possible options are:

  - "none": No correction.

  - "bayesian": When bayesian is set, a hierarchical mixed-effects
    bayesian linear model is fitted to the data, using the `brms`
    package, which is an interface to the `Stan` probabilistic
    programming language. The user can also choose one of the following
    frequentist methods, which will control Family-Wise Error Rate
    (FWER) or the False Discovery Rate (FDR). FDR is less stringent than
    FWER. For FWER, possible options are:

  - "bonferroni": Bonferroni correction, which is dominated by Holm's
    method.

  - "holm": Holm's (1979) method.

  - "hochberg": Hochberg's (1988) method, valid when hypothesis tests
    are independent or non-negatively associated. Less powerful than
    Hommel's (1988) method, but faster to compute.

  - "hommel": Hommel's (1988) method, also valid when hypothesis tests
    are independent or non-negatively associated, but is more powerful
    than Hochberg (1988). For FDR, possible options are:

  - "BH" or "fdr": Benjamini-Hochberg (1995) procedure.

  - "BY": Benjamini-Yekutieli (2001) procedure.

- market_factor_proxy:

  A character string indicating the market factor proxy to be used in
  the CAPM model. Should correspond to one of the columns in
  `benchmark_returns_df`.

- model_structure:

  A character describing the model structure.

- theme_level_intercept:

  A character indicating the specification for theme level intercept

- theme_level_slope:

  A character indicating the specification for theme level slope

- enable_theme_representativeness:

  A logical indicating whether, if a given theme in `signal_themes_m_df`
  does not have any eligible signal, the signal with highest alpha
  t-stat should be elected.

- bayesian_model_parameters:

  An object of class `bayesian_model_parameters`, containing the
  parameters needed to build the hierarhicical bayesian model and
  specify its priors.

- lmer_control:

  A list containing control parameters for the `lmer` function.

## Value

The updated `ss_backtest_config` object with the specified
`alpha_test_strategy` added.
