# Define the `risk_target_parameters` S4 Class

Parameters for the `risk_targeted` meta-portfolio path: scaling a single
risky sleeve against a residual sleeve so that the combination targets a
stated level of risk. The weight on the risky sleeve is \$\$w_t =
\min\left(\max\left(\left(\frac{target}{risk_t}\right)^{p},\\
min\\weight\right),\\ max\\weight\right)\$\$ and the residual sleeve
takes whatever is left. At \\p = 1\\ this is ordinary risk targeting; at
\\p = 2\\ it is the inverse-variance response of a volatility-managed
portfolio.

## Slots

- `residual_ticker`:

  Character naming the residual sleeve, which must be a row of the data
  objects the backtest runs on.

- `target`:

  Numeric, in the target metric's own units, typically annualised
  percentage points.

- `target_metric`:

  Either `"tracking_error"` or `"volatility"`.

- `p`:

  Numeric exponent. 1 for risk targeting, 2 for the inverse-variance
  response.

- `vol_source`:

  One of `"ex_ante"`, `"realized_rolling"` or `"supplied"`.

- `vol_cov_est_method`:

  A `cov_est_method` used when `vol_source` is `"ex_ante"`.

- `vol_window`:

  Numeric months, used when `vol_source` is `"realized_rolling"`.

- `exposure_method`:

  How the exposure multiplier \\s\\ is derived from a metric on the
  sleeve: `"none"`, `"trend"`, `"ts_adjusted"` or `"as_is"`.

- `exposure_window`:

  Numeric months of history for `"ts_adjusted"`, `NULL` otherwise.

- `exposure_center`:

  Numeric, the multiplier when the metric says nothing either way.

- `exposure_sensitivity`:

  Numeric, how far the multiplier moves from the centre. Its sign sets
  the direction. Required by `"trend"` and `"ts_adjusted"`.

- `exposure_bounds`:

  Numeric of length two, the box the multiplier is clipped to before the
  risk ratio scales it.

- `min_weight,max_weight`:

  Numeric bounds on the risky sleeve, so the residual is capped at
  `1 - min_weight`.

## The residual must match the target metric

This is the easiest thing to get wrong here, and it is silent when
wrong.

- A residual that **tracks the benchmark**, such as an index ETF, makes
  tracking error scale linearly in the weight: halving the risky sleeve
  halves the tracking error, and a fully residual portfolio has none.
  `target_metric = "tracking_error"` is then valid.

- A residual that is **riskless**, such as a cash line, makes total
  volatility scale linearly instead. `target_metric = "volatility"` is
  then valid.

- Crossing them does not error, it just stops working. Blending toward
  cash while targeting tracking error *raises* tracking error past a
  point, since a fully cash portfolio is maximally far from the index,
  so the rule chases a level it can never reach.

Validation cannot read a ticker's mind, so it checks the residual's own
realised tracking error against the benchmark and warns when a
tracking-error target is paired with a residual that does not track.

## Estimating current risk

`vol_source` chooses where \\risk_t\\ comes from.

- `"ex_ante"`:

  Re-estimates a covariance matrix from daily stock returns over a short
  window and applies the sleeve's current weights. This is the default
  because it describes the portfolio held now: a rolling window of past
  monthly portfolio returns describes a chain of past compositions
  instead, and inheriting the risk figure from the base backtest's
  `port_stats` would use a long window that is also stale between
  rebalances. Requires `daily_stock_returns_m_xts`.

- `"realized_rolling"`:

  Standard deviation of the sleeve's own past monthly returns over
  `vol_window` months. Fewer observations and a slower response.

- `"supplied"`:

  A series the caller computes, passed to the backtest as data.

For a tracking-error target the covariance is applied to *active*
weights, portfolio minus benchmark, which is how `calculate_port_stats`
derives `act_risk`. The estimator itself therefore works on raw returns,
so `active_returns` in `vol_cov_est_method` should stay `FALSE` to avoid
subtracting the benchmark twice.

## See also

[`create_risk_target_parameters()`](https://pauloguimaraes871.github.io/factoRverse/reference/create_risk_target_parameters.md),
[`add_risk_target_parameters()`](https://pauloguimaraes871.github.io/factoRverse/reference/add_risk_target_parameters.md),
[`port_metabacktest_config-class`](https://pauloguimaraes871.github.io/factoRverse/reference/port_metabacktest_config-class.md)
