# Plot Priors from ss_backtest_config

Plots the distribution curves (normal, student_t, cauchy, lognormal,
beta, or exponential) for each user prior in
`x@alpha_test_strategy@bayesian_model_parameters@user_priors`. `"lkj"`
correlation priors and unparseable/unsupported prior strings are skipped
with a message. Requires a `bayesian_alpha_test_strategy` with non-empty
`user_priors`; otherwise prints a message and returns invisibly.

## Usage

``` r
# S4 method for class 'ss_backtest_config,ANY'
plot(x, palette = "cyberpunk", ...)
```

## Arguments

- x:

  An object of class `ss_backtest_config` with a
  `bayesian_alpha_test_strategy` alpha test strategy.

- palette:

  A character string specifying the color palette. `"cyberpunk"`
  produces a dark theme; any other value produces a light theme. Default
  is `"cyberpunk"`.

- ...:

  Additional arguments (currently unused).
