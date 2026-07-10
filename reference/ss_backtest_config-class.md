# ss_backtest_config Class

The ss_backtest_config class is designed to define an end-to-end signal
selection experiment based on backtest returns of associated strategies.
The class includes parameters for manipulating the backtest returns
object and conducting hypothesis tests regarding CAPM alpha under a
multiple testing framework, with frequentist and bayesian approaches. In
the latter, a hierarhical model is fit, with informative priors set
according to an exogeneous dataset or by the user, or default
uninformative priors.

## Slots

- `chosen_signals_and_positions`:

  A character indicating to which signals ss_backtest should be applied
  and their positions (long and short). For example,
  chosen_signals_and_positions = c(book_yield = "long", vol_36m =
  "short").

- `initial_sample_size`:

  A numeric indicating the minimum number of observations required to
  begin the backtest.

- `rebalancing_months`:

  A numeric indicating the number of months between rebalancing periods.

- `active_returns`:

  A logical indicating whether to use active returns (TRUE) or total
  returns (FALSE) for the backtest.

- `split_method`:

  The method used for splitting the data, either "expanding" or
  "rolling" (default is "expanding").

- `alpha_test_strategy`:

  An `alpha_test_strategy` object with the configuration for the alpha
  test.

- `config_name`:

  A character string representing the name of the configuration.
