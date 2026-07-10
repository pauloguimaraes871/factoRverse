# Show Method for ss_backtest_results Class

This method displays a detailed summary of the most recent batch in the
`ss_backtest_results` object's `ss_backtest_workflow`, including:
backtest configuration (config name, alpha test strategy parameters,
lmer control), p-value correction method (with Bayesian model
parameters, if applicable), date/rebalancing information, signals
information, signal themes and priors information (if applicable),
winsorization parameters, and execution performance (elapsed time and
timestamps).

## Usage

``` r
# S4 method for class 'ss_backtest_results'
show(object)
```

## Arguments

- object:

  An instance of the `ss_backtest_results` class.

## Value

The method returns the object invisibly.
