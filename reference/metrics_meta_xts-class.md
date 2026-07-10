# An S4 Subclass of `meta_xts` for Metric Series (Holes Allowed)

Stores arbitrary metric time series (e.g. factor exposures, signals)
that are not required to be gap-free, unlike
[`returns_meta_xts-class`](https://pauloguimaraes871.github.io/factoRverse/reference/returns_meta_xts-class.md).

## Slots

- `series`:

  Character. Column names of `data` (one per metric).

- `n_series`:

  Numeric. Number of metric columns.

## Validity

- Inherits all checks from
  [`meta_xts-class`](https://pauloguimaraes871.github.io/factoRverse/reference/meta_xts-class.md).

- `n_series` must equal `ncol(data)`, and `series` must match
  `colnames(data)`.

- No check on date consecutiveness – holes (e.g. from illiquid tickers
  or metrics undefined on every date) are expected and allowed.

## See also

[`meta_xts-class`](https://pauloguimaraes871.github.io/factoRverse/reference/meta_xts-class.md),
[`returns_meta_xts-class`](https://pauloguimaraes871.github.io/factoRverse/reference/returns_meta_xts-class.md),
[`create_meta_xts`](https://pauloguimaraes871.github.io/factoRverse/reference/create_meta_xts.md)
