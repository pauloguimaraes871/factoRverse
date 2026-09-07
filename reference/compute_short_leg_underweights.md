# Compute Short-Leg Underweights for the SLSAF Portfolio

Converts short-leg scores into the actual underweights of a Simulated
Long-Short Allocation Framework (`slsaf`) portfolio, and reports the
active budget those underweights release for the long leg.

## Usage

``` r
compute_short_leg_underweights(
  short_scores,
  bench_weights,
  max_short_budget = NULL
)
```

## Arguments

- short_scores:

  Numeric vector of strictly positive short-leg scores, typically
  produced by
  [`compute_short_leg_scores`](https://pauloguimaraes871.github.io/factoRverse/reference/compute_short_leg_scores.md).
  Not required to be normalized.

- bench_weights:

  Numeric vector of benchmark weights for the same assets, in the same
  order. Must be strictly positive and finite.

- max_short_budget:

  Optional numeric scalar in (0, 1\]. Hard ceiling on the realized
  active budget. `NULL` (default) leaves the budget fully endogenous.

## Value

A named list with:

- `underweights`:

  Named numeric vector of underweights \\u_i\\, each in \\\[0, b_i\]\\,
  in the input order.

- `short_budget`:

  The available budget \\T\\, the benchmark mass of the short block.

- `active_budget`:

  The realized budget \\U = \sum u_i\\, which is what the long leg
  receives. Always at most `short_budget`.

- `n_zeroed`:

  Number of assets driven to a zero portfolio weight, i.e. whose
  underweight reached their full benchmark weight.

## Details

The short budget is endogenous: it is the benchmark mass sitting outside
the eligible set, \\T = \sum\_{i \in S} b_i\\. Desired underweights
distribute that budget according to the scores, and each one is then
capped by the position actually held in the benchmark:

\$\$u_i = \min(T \times s_i, b_i), \quad s_i = short\\score_i / \sum_j
short\\score_j\$\$

\$\$U = \sum_i u_i\$\$

**The cap is the mechanism, not a defect.** When \\T s_i\\ exceeds
\\b_i\\ the excess is discarded rather than redistributed to other
short-block names. That is deliberate: full redistribution has a fixed
point where every ineligible constituent goes to zero weight, which is
precisely the ungraded behaviour this construction exists to avoid. The
binding cap is what produces a graded outcome, where mega caps stay
close to benchmark weight and small disliked names are eliminated.

**Only uncapped names convert budget into underweight.** Since capped
names contribute a fixed \\b_i\\ regardless of their score, \\U\\ rises
only when score mass moves toward names whose cap is not binding. This
is why `bench_weight_tilt_eta` in
[`compute_short_leg_scores`](https://pauloguimaraes871.github.io/factoRverse/reference/compute_short_leg_scores.md)
raises the realized budget while `badness_tilt_eta` does not.

**The ceiling rescales proportionally.** When `max_short_budget` is
supplied and \\U\\ exceeds it, every underweight is scaled by the same
factor. Scaling down can never violate \\u_i \le b_i\\, so the result
stays feasible and \\U\\ lands exactly on the ceiling in one step, with
no iteration. The reduction is spread evenly across all names rather
than concentrated on the uncapped ones.

## See also

[`compute_short_leg_scores`](https://pauloguimaraes871.github.io/factoRverse/reference/compute_short_leg_scores.md)
