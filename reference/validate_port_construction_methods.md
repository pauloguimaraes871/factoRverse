# Validate portfolio construction methods and parameters

Validates consistency and allowed values for **MVO**, **RP/HRP**, and
**MMAF** (including macro/micro variants), plus optional **ridge
penalties** with a target portfolio. Errors on invalid configurations;
otherwise returns `TRUE` invisibly.

## Usage

``` r
validate_port_construction_methods(
  port_construction_method,
  micro_port_construction_method = NULL,
  macro_port_construction_method = NULL,
  opt_method = NULL,
  random_ports_method = NULL,
  n_random_ports = NULL,
  opt_objective = NULL,
  n_resamples = NULL,
  exp_ret_score_jitter = NULL,
  cov_eigval_jitter = NULL,
  macro_opt_method = NULL,
  macro_random_ports_method = NULL,
  macro_n_random_ports = NULL,
  macro_opt_objective = NULL,
  macro_n_resamples = NULL,
  macro_exp_ret_score_jitter = NULL,
  macro_cov_eigval_jitter = NULL,
  exp_ret_score_tilt = NULL,
  exp_ret_score_tilt_eta = NULL,
  macro_exp_ret_score_tilt = NULL,
  macro_exp_ret_score_tilt_eta = NULL,
  mmaf_group_col = NULL,
  stock_groups_m_df = NULL,
  mmaf_method = NULL,
  top_down_proxy_port_method = NULL,
  ridge_pen = NULL,
  macro_ridge_pen = NULL,
  target_port_m_df = NULL,
  signals_m_df = NULL,
  dates_m_vector = NULL,
  initial_buffer_period = NULL
)
```

## Arguments

- port_construction_method:

  Character. One of `"ew","sw","cw","cs","rp","hrp","mvo","mmaf"`.

- micro_port_construction_method:

  Character or `NULL`. Micro-level method (when
  `port_construction_method == "mmaf"`).

- macro_port_construction_method:

  Character or `NULL`. Macro-level method (when
  `port_construction_method == "mmaf"`).

- opt_method, random_ports_method, n_random_ports, opt_objective,
  n_resamples, :

  exp_ret_score_jitter,cov_eigval_jitter MVO (micro) controls; see
  function body for constraints.

- macro_opt_method, macro_random_ports_method, macro_n_random_ports,
  macro_opt_objective, :

  macro_n_resamples,macro_exp_ret_score_jitter,macro_cov_eigval_jitter
  MVO (macro) controls; see constraints.

- exp_ret_score_tilt, exp_ret_score_tilt_eta:

  RP/HRP (micro) tilt config; see constraints.

- macro_exp_ret_score_tilt, macro_exp_ret_score_tilt_eta:

  RP/HRP (macro) tilt config; see constraints.

- mmaf_group_col:

  Character or `NULL`. Grouping column name for MMAF.

- stock_groups_m_df:

  `data.frame` or `NULL`. Must contain `mmaf_group_col` for MMAF.

- mmaf_method:

  Character or `NULL`. One of `"bottom_up","top_down"` for MMAF.

- top_down_proxy_port_method:

  Character or `NULL`. If `mmaf_method == "top_down"`, one of
  `"ew","sw","cs","rp","hrp"`.

- ridge_pen, macro_ridge_pen:

  Numeric or `NULL`. Non-negative ridge penalties (micro/macro). Require
  MVO and a valid `target_port_m_df`.

- target_port_m_df:

  `data.frame`/`meta_dataframe` or `NULL`. Target weights for ridge
  penalty (no NAs; column 4 must be `target_weights`).

- signals_m_df:

  `data.frame`. Used to verify ID coverage vs `target_port_m_df`.

- dates_m_vector:

  `Date` vector. Used with `initial_buffer_period` to filter
  `signals_m_df`.

- initial_buffer_period:

  Integer index into `dates_m_vector`.

## Value

Invisibly returns `TRUE` if all checks pass.
