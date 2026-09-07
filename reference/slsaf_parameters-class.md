# Define the `slsaf_parameters` S4 Class

S4 class holding the configuration of the Simulated Long-Short
Allocation Framework, a long-only benchmark-relative method that
expresses conviction as an active overlay: benchmark constituents
outside the eligible set are underweighted according to conviction,
capped by the position actually held, and the budget released is spent
on the eligible names.

## Value

An S4 object of class `slsaf_parameters`.

## Details

The short leg is fully specified by the class and has no method of its
own to choose: it is always signal weighted on the score built by
[`compute_short_leg_scores`](https://pauloguimaraes871.github.io/factoRverse/reference/compute_short_leg_scores.md),
since a risk-based method would grant the largest underweight to the
name contributing least to active risk. Only the long leg is
configurable.

The two exponents divide as follows. `bench_weight_tilt_eta` sets the
basis: at 1 the benchmark term is exactly proportional to benchmark
weight, which is the point where the whole available budget converts
into realized underweight, and which is also the ungraded case where
every ineligible constituent is sold in full. `badness_tilt_eta` then
walks the budget-versus-grading frontier away from that anchor,
concentrating underweight on the worst names and monotonically giving up
budget in exchange. Maximum budget and graded underweights are mutually
exclusive by construction.

## Slots

- `long_port_config`:

  An object of class `sub_port_config` describing how the long leg is
  built. Any non-layered method is allowed.

- `bench_weight_tilt_eta`:

  A numeric exponent applied to the benchmark weight in the short-leg
  score. Defaults to 1.

- `badness_tilt_eta`:

  A numeric exponent applied to the badness score in the short leg. Must
  be non-negative; defaults to 1.

- `max_short_budget`:

  An optional numeric in (0, 1\] capping the realized active budget, or
  NULL to leave it endogenous.
