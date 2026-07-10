# Define the `hrp_parameters` S4 Class

S4 class to represent a set of configurations for hierarchical
risk-parity portfolios.

## Value

An S4 object of class `rp_parameters`.

## Slots

- `linkage`:

  A character indicating the linkage method to be used in the
  hierarchical clustering algorithm. Must be one of 'single',
  'complete', 'average', 'ward.D', 'ward.D2', 'mcquitty', 'median' or
  'centroid'.

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
