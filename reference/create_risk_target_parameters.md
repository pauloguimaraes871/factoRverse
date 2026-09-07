# Create risk_target_parameters

Builds the parameters for the `risk_targeted` meta-portfolio path, which
scales a risky sleeve against a residual sleeve so the combination
targets a stated level of risk. See
[risk_target_parameters](https://pauloguimaraes871.github.io/factoRverse/reference/risk_target_parameters-class.md)
for how the residual and the target metric have to match, and for what
each `vol_source` measures.

## Usage

``` r
create_risk_target_parameters(
  residual_ticker,
  target,
  target_metric = c("tracking_error", "volatility"),
  p = 1,
  vol_source = c("ex_ante", "realized_rolling", "supplied"),
  vol_cov_est_method = NULL,
  vol_window = 6,
  exposure_method = c("none", "trend", "ts_adjusted", "as_is"),
  exposure_window = NULL,
  exposure_center = 1,
  exposure_sensitivity = NULL,
  exposure_bounds = c(0, 1),
  min_weight = 0,
  max_weight = 1
)
```

## Arguments

- residual_ticker:

  Character naming the residual sleeve. It must be a row of the data
  objects the backtest runs on, carrying its own return, liquidity and
  volatility.

- target:

  Numeric, in the target metric's own units.

- target_metric:

  `"tracking_error"` (default) or `"volatility"`. Must match what the
  residual is: an index-tracking residual for the former, a riskless one
  for the latter.

- p:

  Numeric exponent, 1 for risk targeting and 2 for the inverse-variance
  response.

- vol_source:

  `"ex_ante"` (default), `"realized_rolling"` or `"supplied"`.

- vol_cov_est_method:

  A `cov_est_method` for `"ex_ante"`. Defaults to EWMA over 60 daily
  observations with `active_returns = FALSE`, since a tracking-error
  target is expressed through active weights rather than by subtracting
  the benchmark from the returns.

- vol_window:

  Numeric months for `"realized_rolling"`. Default 6.

- exposure_method:

  How the exposure multiplier \\s\\ is derived from a metric on the
  sleeve. `"none"` (default) fixes it at one, so the weight is the risk
  ratio alone. `"trend"` reads only the sign of the metric,
  `"ts_adjusted"` scores it against its own history over
  `exposure_window`, and `"as_is"` passes it through as the multiplier.

- exposure_window:

  Numeric months of history for `"ts_adjusted"`. Ignored by the other
  methods. Default `NULL`.

- exposure_center:

  Numeric, the multiplier when the metric says nothing either way.
  Default 1.

- exposure_sensitivity:

  Numeric, how far the multiplier moves from `exposure_center`. Its sign
  sets the direction, so a negative value leans away from a high metric.
  Required by `"trend"` and `"ts_adjusted"`, which have no safe default
  for it. Default `NULL`.

- exposure_bounds:

  Numeric of length two, the box the multiplier is clipped to before the
  risk ratio scales it. Default `c(0, 1)`.

  The signal is read at the rebalance date and only from data available
  then, and it is kept apart from the risk ratio on purpose: a constant
  \\s\\ folds into the target by rescaling it to \\target \times
  s^{1/p}\\, so only a time-varying signal adds anything at all.

- min_weight, max_weight:

  Numeric bounds on the risky sleeve. Default 0 and 1.

## Value

An object of class `risk_target_parameters`.

## See also

[risk_target_parameters](https://pauloguimaraes871.github.io/factoRverse/reference/risk_target_parameters-class.md),
[`add_risk_target_parameters()`](https://pauloguimaraes871.github.io/factoRverse/reference/add_risk_target_parameters.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  # Hold at least half the risky sleeve, targeting 4 percent annualised tracking error
  risk_target_params <- create_risk_target_parameters(
    residual_ticker = "BOVA11", target = 4, target_metric = "tracking_error",
    p = 1, min_weight = 0.5
  )
} # }
```
