# Estimate a Portfolio Sleeve's Current Risk

Measures how risky a portfolio is as of a given date, which is the
denominator of the risk-targeting rule in the `risk_targeted`
meta-portfolio path. Returns an annualised figure in percentage points,
matching the units a `risk_target_parameters` target is stated in.

## Usage

``` r
estimate_sleeve_risk(
  current_date,
  risk_target_params,
  risky_port_backtest_results,
  daily_stock_returns_m_xts = NULL,
  selected_benchmark = NULL,
  stock_groups_m_d_ref = NULL,
  vol_m_df = NULL,
  expected_risky_ticker = NULL,
  return_basis = "net"
)
```

## Arguments

- current_date:

  The date to measure at. Only data up to and including it is used.

- risk_target_params:

  A `risk_target_parameters` object.

- risky_port_backtest_results:

  The `port_backtest_results` for the sleeve being scaled.

- daily_stock_returns_m_xts:

  Daily stock returns, required for `"ex_ante"`.

- selected_benchmark:

  Character naming the benchmark, required for a tracking-error target.

- stock_groups_m_d_ref:

  Optional groups for the date, passed through to
  [`estimate_covariance_matrix()`](https://pauloguimaraes871.github.io/factoRverse/reference/estimate_covariance_matrix.md).
  It is needed whenever the daily return sample contains missing values,
  since those are filled from group medians; without it the estimator
  fails on any series with gaps.

- vol_m_df:

  Optional `data.frame` with `tickers`, `dates` and a risk column,
  required for `"supplied"`.

- return_basis:

  `"net"` or `"raw"`, for `"realized_rolling"`.

## Value

A single annualised risk figure in percentage points, or `NA_real_` when
there is not enough history to estimate one.

## Which risk

`target_metric` decides what is being measured, and the weights follow
from it.

- `"volatility"` uses the sleeve's own weights, giving total volatility.

- `"tracking_error"` uses *active* weights, the sleeve's weights minus
  the benchmark's, against a raw covariance matrix. That is how
  [`calculate_port_stats()`](https://pauloguimaraes871.github.io/factoRverse/reference/calculate_port_stats.md)
  derives `act_risk`, so the convention is inherited rather than
  invented. Estimating the covariance on active returns as well would
  subtract the benchmark twice, which `risk_target_parameters` refuses.

## Where the number comes from

- `"ex_ante"`:

  A covariance matrix estimated from daily stock returns over a short
  window ending at `current_date`, applied to the weights held then.
  This describes the portfolio as it stands. The alternatives do not: a
  window of past monthly portfolio returns describes a chain of past
  compositions, and the `act_risk` already sitting in the base
  backtest's `port_stats` was estimated over a long window and only
  refreshed on that backtest's own rebalance dates.

- `"realized_rolling"`:

  Standard deviation of the sleeve's own past monthly returns, active
  returns for a tracking-error target.

- `"supplied"`:

  Read from a series the caller computed, taken as already annualised.

## Units

Returns are in percentage points throughout the package, so a standard
deviation of them is too. A daily covariance gives a daily standard
deviation, annualised here by \\\sqrt{252}\\; a monthly one by
\\\sqrt{12}\\. A supplied series is assumed to be annualised already,
since the caller computed it and only they know its frequency.

## See also

[`risk_target_parameters-class`](https://pauloguimaraes871.github.io/factoRverse/reference/risk_target_parameters-class.md),
[`estimate_covariance_matrix`](https://pauloguimaraes871.github.io/factoRverse/reference/estimate_covariance_matrix.md)
