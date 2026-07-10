# Define the `rp_parameters` S4 Class

S4 class to represent a set of configurations for risk-parity
portfolios.

## Value

An S4 object of class `rp_parameters`.

## Slots

- `rp_method`:

  A character indicating the method to compute the risk-parity vanilla
  solution. It is passed to riskParityPortfolio::riskParityPortfolio
  function as method_init. Default is "cyclical-spinu"

- `exp_ret_score_tilt`:

  A character indicating when to apply the expected return score tilt.
  Possible options are 'none', 'inner' or 'final'. 'none' means no tilt
  is applied, 'inner' means the tilt is applied within the risk parity
  algorithm and 'final' means the tilt is applied after the risk parity
  solution is computed.

- `exp_ret_score_tilt_eta`:

  A numeric value representing the intensity of the expected return
  score tilt. Must be a positive value. Only needed when
  exp_ret_score_tilt is 'inner' or 'final'.
