# Create SLSAF (Simulated Long-Short Allocation Framework) Parameters

Constructor for an `slsaf_parameters` object, the configuration of a
long-only benchmark-relative portfolio built as a benchmark position
plus a self-financing active overlay.

## Usage

``` r
create_slsaf_parameters(
  long_port_construction_method = "sw",
  long_port_config = NULL,
  bench_weight_tilt_eta = 1,
  badness_tilt_eta = 1,
  max_short_budget = NULL
)
```

## Arguments

- long_port_construction_method:

  A character string with the method used to build the long leg. Must be
  one of 'ew', 'sw', 'cw', 'cs', 'rp', 'hrp' or 'mvo'. Ignored if
  `long_port_config` is supplied.

- long_port_config:

  An object of class `sub_port_config`. If missing, one is created from
  `long_port_construction_method`.

- bench_weight_tilt_eta:

  A numeric exponent applied to the benchmark weight in the short-leg
  score. Defaults to 1.

- badness_tilt_eta:

  A non-negative numeric exponent applied to the badness score. Defaults
  to 1.

- max_short_budget:

  An optional numeric in (0, 1\] capping the realized active budget.
  Defaults to NULL, leaving it endogenous.

## Value

An S4 object of class `slsaf_parameters`.

## Details

Only the long leg is configurable. The short leg is always signal
weighted on the badness score from
[`compute_short_leg_scores`](https://pauloguimaraes871.github.io/factoRverse/reference/compute_short_leg_scores.md),
because a risk-based method would grant the largest underweight to the
name contributing least to active risk.

On the exponents: `bench_weight_tilt_eta = 1` is the
benchmark-proportional anchor, where the whole available budget converts
into realized underweight and every ineligible constituent is sold in
full. `badness_tilt_eta` moves away from that anchor, concentrating
underweight on the worst names and giving up budget in exchange.
Tracking error is otherwise governed by `eligibility_quantile_range`
(which sets how much benchmark mass falls outside the eligible set) and
by how concentrated the long leg is.
