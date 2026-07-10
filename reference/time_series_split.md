# Time-Series Split for Walk-Forward Validation

Splits panel data into training, (optional) validation, and refit
samples for one `current_date` of a walk-forward, expanding- or
rolling-window backtest. The cut points embed a `target_fwd`-period
embargo so no sample uses target information unobservable at decision
time.

## Usage

``` r
time_series_split(
  current_date,
  features_m_df,
  target_m_df,
  dates_m_vector,
  training_sample_size,
  validation_sample_size = 0,
  target_fwd,
  target_fwd_name,
  split_method = "expanding"
)
```

## Arguments

- current_date:

  A single `Date` (`"%Y-%m-%d"`) for the rebalancing point.

- features_m_df:

  Data frame/matrix with `id`, `tickers`, `dates` columns.

- target_m_df:

  Data frame/matrix of targets aligned row-for-row with `features_m_df`.

- dates_m_vector:

  Ascending vector of unique panel dates (`"%Y-%m-%d"`).

- training_sample_size:

  Integer, number of dates in the training window.

- validation_sample_size:

  Integer, dates in the validation window (`0` disables it; default
  `0`).

- target_fwd:

  Integer, forecast horizon in periods (the embargo length).

- target_fwd_name:

  Character, name of the target column to extract.

- split_method:

  Character, `"expanding"` (default) or `"rolling"`.

## Value

A named list with `training`, `refit`, and — when
`validation_sample_size > 0` — `validation`. `training`/`refit` each
hold raw features, the target vector, and a cleaned `full_data_*_clean`
frame (target + features, no id columns); `validation` holds validation
features and target.

## Details

Because the target is a `target_fwd`-months-forward return, the last
usable training/validation date is shifted back by `target_fwd` periods
relative to `current_date`. The first rebalancing
(`d == training_sample_size + validation_sample_size`) is a special
case; later rebalances slide the windows forward.
