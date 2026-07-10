# Create Signal Engineering Benchmarks

Creates Signal-Selection and Signal-Blending benchmark weights:

- Signal-Selection Benchmark: A benchmark that is built using the
  universe of all signals in `chosen_signals`. It is used to evaluate
  the performance of the signal selection process.

- Signal-Blending Benchmark: A benchmark that is built using only
  signals derived from signal selection process (those with
  pre_eligible_assets assigned as 1). It is used to evaluate the
  performance of the signal blending process.

## Usage

``` r
create_se_benchmarks(signal_universe_m_d_ref, selected_signal_themes_m_d_ref)
```

## Arguments

- signal_universe_m_d_ref:

  A data frame containing the signal universe. Must include columns:

  - `id`

  - `tickers`

  - `dates`

  - `pre_eligible_assets` (indicating which signals are statistically
    significant)

- selected_signal_themes_m_d_ref:

  An optional data frame with signal themes. Must include columns:

  - `tickers`

  - `theme` (the classification theme for each signal) This parameter is
    mandatory if one wants to calculate theme-weighted benchmarks.

## Value

A list with two data frames, each with benchmark weights following
Signal-Selection and Signal-Blending methods. The columns of each
data.frame include:

- `id`

- `tickers`

- `dates`

- `individual` (weight assigned to each signal)

- `theme` (weight assigned to each signal given their theme, if
  `selected_signal_themes_m_d_ref` is provided)

## Details

The function calculates weights for signals based on their statistical
significance. If `selected_signal_themes_m_d_ref` is provided, it also
calculates theme-based weights and merges them back into the main data
frame.
