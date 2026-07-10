# factoRverse: Portfolio Construction with ML-Based and Characteristics-Based Signals

An end-to-end toolkit for factor-investing research and deployment. It
provides point-in-time construction of firm-level characteristics and
anomalies, look-ahead-safe preprocessing, and a consistent, auditable
interface across four workflows: characteristic-sorted portfolio
backtesting; signal selection under multiple-testing control
(family-wise error rate and false discovery rate procedures, and
hierarchical Bayesian partial pooling); signal blending with heuristics
and machine-learning models (elastic net, random forests, gradient
boosting and neural networks) under a unified tuning and walk-forward
scheme; and risk-based portfolio construction (equal weight, risk
parity, hierarchical risk parity, mean-variance optimization) with
liquidity, turnover and concentration constraints and transaction costs.

## Workflows

factoRverse organises the factor-investing pipeline into four consistent
`config + data objects -> run_*() -> *_results` workflows:

1.  **Characteristic portfolios**: rank stocks on a signal and backtest
    top-quantile, long-only portfolios with
    [`run_port_backtest()`](https://pauloguimaraes871.github.io/factoRverse/reference/run_port_backtest.md).

2.  **Signal selection**: control the factor zoo with multiple-testing
    and hierarchical Bayesian methods via
    [`run_ss_backtest()`](https://pauloguimaraes871.github.io/factoRverse/reference/run_ss_backtest.md).

3.  **Signal blending**: combine selected signals with heuristics or
    machine learning via
    [`run_sb_backtest()`](https://pauloguimaraes871.github.io/factoRverse/reference/run_sb_backtest.md).

4.  **Deployment**: feed the blended score back into
    [`run_port_backtest()`](https://pauloguimaraes871.github.io/factoRverse/reference/run_port_backtest.md)
    to build the final, constraint- and cost-aware book.

Data enters through
[`create_meta_dataframe()`](https://pauloguimaraes871.github.io/factoRverse/reference/create_meta_dataframe.md)
/
[`create_meta_xts()`](https://pauloguimaraes871.github.io/factoRverse/reference/create_meta_xts.md)
and is engineered with the `compute_*()` family and
[`map_recipe_timewise()`](https://pauloguimaraes871.github.io/factoRverse/reference/map_recipe_timewise.md).

## See also

Useful links:

- Package website: <https://pauloguimaraes871.github.io/factoRverse/>

- Source and bug reports:
  <https://github.com/pauloguimaraes871/factoRverse>

## Author

**Maintainer**: Paulo Guimaraes <paulo.guimaraes871@gmail.com>
([ORCID](https://orcid.org/0009-0002-2719-7731))
