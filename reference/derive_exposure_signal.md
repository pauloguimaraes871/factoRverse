# Derive an Exposure Signal From a Metric

Maps a per-date metric onto the exposure multiplier \\s_t\\ of a
risk-targeted allocation, whose weight on the risky sleeve is \$\$w_t =
s_t \times \left(\frac{target}{risk_t}\right)^{p}.\$\$

## Usage

``` r
derive_exposure_signal(
  metric_m_df,
  metric = NULL,
  expected_risky_ticker = NULL,
  method = c("trend", "ts_adjusted", "as_is"),
  window = NULL,
  center = 1,
  sensitivity = NULL,
  min_exposure = 0,
  max_exposure = 1,
  verbose = TRUE
)
```

## Arguments

- metric_m_df:

  A `meta_dataframe` or `data.frame` with `id`, `tickers` and `dates`
  plus the metric column, carrying exactly one ticker: the risky sleeve.

- metric:

  Character naming the metric column. Defaults to the only non-key
  column when there is exactly one.

- expected_risky_ticker:

  Optional character naming the risky sleeve. When given, the metric
  must describe exactly that asset. Carrying one ticker is not the same
  as carrying the right one, and a metric for something else would drive
  the multiplier unnoticed.

- method:

  One of `"trend"`, `"ts_adjusted"` or `"as_is"`.

- window:

  Positive whole number, required for `"ts_adjusted"`. Length of the
  trailing window in observations. Dates without a full window get no
  exposure.

- center:

  Numeric, default 1. The multiplier a neutral signal maps to.

- sensitivity:

  Numeric, signed, required for `"trend"` and `"ts_adjusted"`.

- min_exposure, max_exposure:

  Numeric bounds on \\s_t\\ itself, default 0 and 1. These bound the
  lean before the risk ratio scales it; the final weight is bounded
  separately by `risk_target_parameters`.

- verbose:

  Logical, default `TRUE`.

## Value

A `data.frame` with columns `dates` and `exposure`, covering the dates
for which the mapping could be computed.

## Why the two terms are separate

They answer different questions and neither substitutes for the other.
\\s_t\\ says which way and how strongly to lean, from a trend or a
valuation signal. The risk ratio says how large that lean should be
given what the sleeve currently risks. Multiplying them is the standard
construction for running trend and volatility management together:
time-series momentum in the sense of Moskowitz, Ooi and Pedersen is
exactly \\sign(r\_{t-12,t})\\ times an inverse-volatility scaling, which
is this expression at `method = "trend"` and \\p = 1\\.

A *constant* \\s\\ would be redundant, since it could be folded into
`target` by rescaling it to \\target \times s^{1/p}\\. A time-varying
\\s_t\\ cannot, which is what makes it a real degree of freedom rather
than a second name for the target.

## The mappings

- `"trend"`:

  \\s_t = center + sensitivity \times sign(metric_t)\\. Exposure depends
  on the direction of a trailing return, not its size, which is the
  time-series momentum rule.

- `"ts_adjusted"`:

  The metric is z-scored against its own trailing window and \\s_t =
  center + sensitivity \times z_t\\. This is the valuation and
  signal-strength family: a sleeve expensive relative to its own
  history, or carrying an unusually strong expected return score.
  Comparing a metric to its own past rather than to a cross-section is
  what makes it a timing rule.

- `"as_is"`:

  The metric is already an exposure multiplier and passes through, for a
  caller who computed one elsewhere.

There is deliberately no inverse-of-risk mapping here. That is what the
\\(target/risk)^p\\ term does, and offering it in both places would let
the volatility scaling be applied twice without it being visible in the
output.

## Sensitivity carries the direction

`sensitivity` is signed and has no default, because its sign is the
entire economic claim. A sleeve expensive against its own history argues
for less exposure, so a valuation metric takes a negative sensitivity; a
strong expected-return score argues for more, so it takes a positive
one. Defaulting it would let the direction be chosen by accident.

## Look-ahead

The metric on date `t` sets the exposure for `t`, and the trailing
window of `"ts_adjusted"` ends at `t` inclusive. Whether the metric
itself was knowable at `t` is the caller's responsibility.

## See also

[`risk_target_parameters-class`](https://pauloguimaraes871.github.io/factoRverse/reference/risk_target_parameters-class.md),
[`estimate_sleeve_risk`](https://pauloguimaraes871.github.io/factoRverse/reference/estimate_sleeve_risk.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  # Time-series momentum: full exposure while the trailing return is positive, half when not
  exposure <- derive_exposure_signal(
    trailing_return_m_df, metric = "return_12m",
    method = "trend", center = 0.75, sensitivity = 0.25
  )
} # }
```
