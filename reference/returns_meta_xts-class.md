# An S4 Subclass of `meta_xts` for Return Series (No Holes Allowed)

Stores asset (or portfolio) return time series. Extends
[`meta_xts-class`](https://pauloguimaraes871.github.io/factoRverse/reference/meta_xts-class.md)
with asset identity metadata and enforces the invariants that make a
return series usable in backtests: consecutive dates and, for
portfolios, no missing values.

## Slots

- `asset_type`:

  Character. Type of asset (e.g. `"stock"`, `"bond"`, `"ports"` for
  portfolios).

- `assets`:

  Character. Column names of `data` (one per asset).

- `n_assets`:

  Numeric. Number of asset columns.

## Return convention

Values are expected on a **percent scale** (e.g. `2.0` for 2\\ `0.02`).
If the median absolute value in `data` is below 1, validity emits a
[`warning()`](https://rdrr.io/r/base/warning.html) flagging a probable
decimal-scale input, since downstream performance functions assume
percent scale.

## Validity

- Inherits all checks from
  [`meta_xts-class`](https://pauloguimaraes871.github.io/factoRverse/reference/meta_xts-class.md).

- Dates should be (approximately) consecutive for the detected
  frequency; a large proportion of gaps raises an error.

- `n_assets` must equal `ncol(data)`, and `assets` must match
  `colnames(data)`.

- Missing values trigger a
  [`warning()`](https://rdrr.io/r/base/warning.html) for individual
  assets, but are a hard [`stop()`](https://rdrr.io/r/base/stop.html)
  when `asset_type == "ports"` – a portfolio return series is not
  allowed to have gaps.

- Emits a [`message()`](https://rdrr.io/r/base/message.html) reporting
  the detected frequency, useful because detection can be unreliable on
  short or irregular series.

## See also

[`meta_xts-class`](https://pauloguimaraes871.github.io/factoRverse/reference/meta_xts-class.md),
[`metrics_meta_xts-class`](https://pauloguimaraes871.github.io/factoRverse/reference/metrics_meta_xts-class.md),
[`create_meta_xts`](https://pauloguimaraes871.github.io/factoRverse/reference/create_meta_xts.md)
