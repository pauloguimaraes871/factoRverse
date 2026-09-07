# Create a Simulated Long-Short Allocation Framework (SLSAF) portfolio

Builds a long-only, benchmark-relative portfolio as a benchmark position
plus a self-financing active overlay. The universe is split into two
conviction blocks by
[`classify_investment_universe`](https://pauloguimaraes871.github.io/factoRverse/reference/classify_investment_universe.md):
a long block (what the eligibility cascade is willing to buy) and a
short block (benchmark constituents the cascade rejected, which may only
be underweighted). The overlay underweights the short block according to
conviction, and spends exactly the budget it releases on the long block.

## Usage

``` r
create_slsaf_portfolio(
  universe_m_d_ref,
  selected_benchmark,
  long_port_config,
  bench_weight_tilt_eta = 1,
  badness_tilt_eta = 1,
  max_short_budget = NULL,
  covariance_matrix = NULL,
  eligible_returns_m_xts_upd_ref = NULL,
  selected_benchmark_m_xts_upd_ref = NULL,
  active_returns = if (is.null(selected_benchmark_m_xts_upd_ref)) FALSE else TRUE,
  cov_estimation_method = "sample",
  cov_matrix_sample_size = if (is.null(eligible_returns_m_xts_upd_ref)) NULL else
    nrow(eligible_returns_m_xts_upd_ref),
  groups_m_d_ref = NULL,
  liquidity_m_d_ref = NULL,
  cap_weighting_metric = NULL,
  lower_quantile_winsorization = 0.025,
  upper_quantile_winsorization = 0.975,
  parallel = FALSE,
  verbose = TRUE
)
```

## Arguments

- universe_m_d_ref:

  A single-date data frame carrying `is_eligible`, `is_long_candidate`,
  `is_short_candidate`, `exp_ret_score`, the benchmark weight column,
  and ideally `exp_ret_score_raw`. Produced by
  [`classify_investment_universe`](https://pauloguimaraes871.github.io/factoRverse/reference/classify_investment_universe.md)
  with `include_benchmark_in_universe = TRUE`.

- selected_benchmark:

  Character scalar naming the benchmark. The universe must carry a
  `<selected_benchmark>_bench_weights` column.

- long_port_config:

  A `sub_port_config` describing how to build the long leg.

- bench_weight_tilt_eta, badness_tilt_eta:

  Numeric exponents of the short-leg score, passed to
  [`compute_short_leg_scores`](https://pauloguimaraes871.github.io/factoRverse/reference/compute_short_leg_scores.md).

- max_short_budget:

  Optional numeric in (0, 1\]. Ceiling on the realized active budget,
  passed to
  [`compute_short_leg_underweights`](https://pauloguimaraes871.github.io/factoRverse/reference/compute_short_leg_underweights.md).

- covariance_matrix:

  Optional covariance matrix of the eligible assets, in the eligible
  universe order. Sub-portfolios receive the relevant submatrix rather
  than re-estimating, so both legs share the parent risk model.

- eligible_returns_m_xts_upd_ref, selected_benchmark_m_xts_upd_ref,
  active_returns, cov_estimation_method, cov_matrix_sample_size:

  Covariance estimation inputs, forwarded to the long leg when it needs
  them.

- groups_m_d_ref, liquidity_m_d_ref, cap_weighting_metric:

  Optional data forwarded to the long leg.

- lower_quantile_winsorization, upper_quantile_winsorization:

  Numerics in (0, 1), forwarded to the long leg.

- parallel:

  Logical, forwarded to the long leg.

- verbose:

  Logical, print progress and timing via `tictoc`.

## Value

A list with:

- universe_m_d_ref:

  Input universe joined with final `weights`.

- weights:

  Final portfolio weights.

- underweights:

  Named numeric vector of short-block underweights.

- short_budget:

  The available budget \\T\\.

- active_budget:

  The realized budget \\U\\.

- n_long,n_short,n_zeroed:

  Block sizes and the number of fully sold constituents.

- micro:

  `list(long = <port or NULL>, short = <port or NULL>)`.

## Details

For benchmark weights \\b_i\\, long block \\L\\ and short block \\S\\:

1.  **Short budget.** \\T = \sum\_{i \in S} b_i\\, the benchmark mass
    sitting outside the eligible set. It is endogenous by design: when
    the index heavyweights score well they are eligible, \\T\\ shrinks
    and the portfolio hugs the benchmark.

2.  **Short leg.** A signal-weighted sub-portfolio over \\S\\ on the
    score from
    [`compute_short_leg_scores`](https://pauloguimaraes871.github.io/factoRverse/reference/compute_short_leg_scores.md),
    giving \\s\\.

3.  **Underweights.** \\u_i = \min(T s_i, b_i)\\, optionally rescaled to
    a ceiling, via
    [`compute_short_leg_underweights`](https://pauloguimaraes871.github.io/factoRverse/reference/compute_short_leg_underweights.md).
    The realized active budget is \\U = \sum_i u_i\\.

4.  **Long leg.** A sub-portfolio over \\L\\ built with
    `long_port_config`, giving \\l\\.

5.  **Combine.** \\w_i = b_i - u_i\\ on \\S\\, \\w_i = b_i + U l_i\\ on
    \\L\\, and 0 elsewhere.

The construction guarantees, and asserts before returning:

- active weights sum to zero exactly, so weights sum to 1 with no
  renormalization;

- \\0 \le w_i \le b_i\\ on the short block, so a disliked constituent is
  never overweighted and never shorted;

- \\w_i \ge b_i\\ on the long block, so an eligible name is never
  structurally underweighted, which is the problem this method exists to
  solve;

- long-only throughout.

Note that the long leg weights \\l\\ are *active* weights, not total
weights. The sub-portfolios are therefore built with
`selected_benchmark = NULL`: a benchmark-relative statistic computed on
an active-weight vector would be misleading. The parent portfolio still
reports full active statistics against the real benchmark.

## See also

[`compute_short_leg_scores`](https://pauloguimaraes871.github.io/factoRverse/reference/compute_short_leg_scores.md),
[`compute_short_leg_underweights`](https://pauloguimaraes871.github.io/factoRverse/reference/compute_short_leg_underweights.md),
[`set_portfolio_weights`](https://pauloguimaraes871.github.io/factoRverse/reference/set_portfolio_weights.md)
