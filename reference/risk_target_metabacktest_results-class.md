# S4 Class for Risk-Targeted Meta Backtest Results

The result of the `risk_targeted` path: a single risky sleeve scaled
against a residual sleeve so the combination targets a stated level of
risk. Extends
[`port_metabacktest_results-class`](https://pauloguimaraes871.github.io/factoRverse/reference/port_metabacktest_results-class.md)
with the diagnostics that only make sense for a targeting rule, and
carries its own plot method for the capital-market-line view.

## Slots

- `residual_ticker`:

  Character naming the residual sleeve.

- `risk_target_parameters`:

  The `risk_target_parameters` used.

## Reading meta_port_stats_m_df on this path

Where the multi-portfolio path reports cross-sectional portfolio
analytics, this one reports the targeting rule at work: `sleeve_risk` is
the risky sleeve's estimated risk at each rebalance date, `risky_weight`
the weight the rule set from it, `target` the level asked for, and
`implied_risk` the product \\sleeve\\risk \times risky\\weight\\.

`implied_risk` equals `target` exactly whenever the weight is unclipped,
so a departure from it says a bound was binding, not that the targeting
failed. Whether the rule actually worked is a question about realised
returns, which exist only after the stock-level run: compare the
realised tracking error of
`meta_port_backtest_results@port_returns_m_xts` against `target`. A
realised figure persistently above the target means the risk estimator
is too slow to catch risk as it rises.

## See also

[`port_metabacktest_results-class`](https://pauloguimaraes871.github.io/factoRverse/reference/port_metabacktest_results-class.md),
[`risk_target_parameters-class`](https://pauloguimaraes871.github.io/factoRverse/reference/risk_target_parameters-class.md)
