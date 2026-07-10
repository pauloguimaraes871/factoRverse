# Valida exp_ret_score_tilt

Internal function to validate the exp_ret_score_tilt and
exp_ret_score_tilt_eta parameters that belong to rp and hrp port
constructions methods.

## Usage

``` r
validate_exp_ret_score_tilt(exp_ret_score_tilt, exp_ret_score_tilt_eta)
```

## Arguments

- exp_ret_score_tilt:

  A character indicating when to apply the expected return score tilt.
  Possible options are 'none', 'inner' or 'final'. 'none' means no tilt
  is applied, 'inner' means the tilt is applied within the risk parity
  algorithm and 'final' means the tilt is applied after the risk parity
  solution is computed.

- exp_ret_score_tilt_eta:

  A numeric value representing the intensity of the expected return
  score tilt. Must be a positive value. Only needed when
  exp_ret_score_tilt is 'inner' or 'final'.

## Value

Invisibly returns TRUE if the parameters are valid.
