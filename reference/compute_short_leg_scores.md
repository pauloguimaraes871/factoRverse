# Compute Short-Leg Scores for the SLSAF Portfolio

Builds the score that drives the underweight (short) leg of a Simulated
Long-Short Allocation Framework (`slsaf`) portfolio. The short leg is
applied to benchmark constituents that failed regular eligibility, and
its score answers a different question from `exp_ret_score`: not "how
much do I want to own this", but "how much conviction do I have to hold
less of it than the benchmark does".

## Usage

``` r
compute_short_leg_scores(
  exp_ret_score_raw,
  bench_weights,
  badness_tilt_eta = 1,
  bench_weight_tilt_eta = 1
)
```

## Arguments

- exp_ret_score_raw:

  Numeric vector of unscaled expected return scores (the
  `exp_ret_score_raw` column, never the scaled `exp_ret_score`). Must be
  strictly positive and finite, which
  [`signal_transform`](https://pauloguimaraes871.github.io/factoRverse/reference/signal_transform.md)
  guarantees by construction.

- bench_weights:

  Numeric vector of benchmark weights for the same assets, in the same
  order. Must be strictly positive and finite: every asset in the short
  block is by definition a benchmark constituent.

- badness_tilt_eta:

  Numeric scalar exponent applied to the badness score. Defaults to 1.
  Values above 1 concentrate underweight on the worst names at the cost
  of budget; 0 removes the conviction tilt entirely.

- bench_weight_tilt_eta:

  Numeric scalar exponent applied to the benchmark weight. Defaults to
  1, the benchmark-proportional anchor. 0 removes the benchmark term
  (\\b^0 = 1\\), leaving a pure badness score.

## Value

A named numeric vector of strictly positive short-leg scores, with the
same length and names as `exp_ret_score_raw`. The scores are not
normalized: the downstream signal-weighted call normalizes them to sum
to 1.

## Details

The score is the product of two strictly positive terms, each with its
own exponent:

\$\$short\\score_i = b_i ^ {bench\\weight\\tilt\\eta} \times badness_i ^
{badness\\tilt\\eta}\$\$

- `badness_i = 1 / exp_ret_score_raw_i`:

  The reciprocal is the canonical inversion in this package rather than
  an ad hoc choice:
  [`signal_transform()`](https://pauloguimaraes871.github.io/factoRverse/reference/signal_transform.md)
  maps \\z \> 0\\ to \\1 + z\\ and \\z \< 0\\ to \\1 / (1 - z)\\, so
  \\f(-z) = 1 / f(z)\\ identically. The reciprocal of a score is
  therefore exactly the score the same signal would produce under the
  opposite position.

- `b_i`:

  The raw benchmark weight, which anchors the short leg on the position
  actually held. The raw weight is used rather than a cross-sectional
  transform of it because only the raw weight makes the
  budget-maximizing point exactly representable, see below.

**The scaler is deliberately absent.** The long leg uses
`exp_ret_score = exp_ret_score_raw * scaler`, but the short leg is
reconstructed from `exp_ret_score_raw` only. Inverting a scaler would
invert its economic meaning: a scaler such as `1 / idio_vol` would make
the short leg underweight low-volatility stocks the most, silently
shorting a documented return premium as a side effect of a plumbing
decision. Any scaler that is itself return-predictive must not reach the
short leg.

**What each exponent actually does.** Downstream,
[`compute_short_leg_underweights`](https://pauloguimaraes871.github.io/factoRverse/reference/compute_short_leg_underweights.md)
spends a budget \\T = \sum_i b_i\\ subject to \\u_i \le b_i\\. Because
\\\sum_i T s_i = T = \sum_i b_i\\, the budget is fully spent if and only
if the normalized short weights are benchmark-proportional (\\s \propto
b\\), and any departure from proportionality strictly loses budget. That
gives both exponents a clean reading:

- `bench_weight_tilt_eta` sets the *basis*. At 1 the benchmark term
  alone is exactly proportional to \\b\\, which is the budget-maximizing
  anchor. At 0 the benchmark is ignored entirely and the score is pure
  badness.

- `badness_tilt_eta` then walks the *budget-versus-grading frontier*. At
  0 (with `bench_weight_tilt_eta = 1`) the whole budget is spent, which
  is also the ungraded case where every ineligible constituent is sold
  in full. Raising it concentrates underweight on the worst names and
  monotonically gives up budget in exchange, which is the graded
  behaviour this construction exists to produce.

Maximum budget and graded underweights are therefore mutually exclusive
by construction, and `badness_tilt_eta` is the parameter that prices the
trade.

## See also

[`compute_short_leg_underweights`](https://pauloguimaraes871.github.io/factoRverse/reference/compute_short_leg_underweights.md),
[`signal_transform`](https://pauloguimaraes871.github.io/factoRverse/reference/signal_transform.md)
