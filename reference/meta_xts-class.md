# An S4 Class Storing a Time Series Plus Metadata

Base container class for an `xts` time series together with the metadata
needed to track its provenance and shape. Not normally instantiated
directly; use
[`create_meta_xts()`](https://pauloguimaraes871.github.io/factoRverse/reference/create_meta_xts.md),
which builds the appropriate subclass
([`returns_meta_xts-class`](https://pauloguimaraes871.github.io/factoRverse/reference/returns_meta_xts-class.md)
or
[`metrics_meta_xts-class`](https://pauloguimaraes871.github.io/factoRverse/reference/metrics_meta_xts-class.md))
for you.

## Slots

- `data`:

  An `xts` object containing the time series.

- `meta_xts_name`:

  Character. A label or ID for the series.

- `metric_name`:

  Character. Name of the metric/return series being stored.

- `workflow`:

  ANY. A placeholder for user-defined workflow/pipeline objects (e.g. a
  log of the processing steps that produced `data`).

- `n_dates`:

  Numeric. Number of rows in `data`.

- `source`:

  Character. Origin of each column, one entry per column of `data`.

- `frequency`:

  Character. Detected time frequency (e.g. `"daily"`, `"weekly"`,
  `"monthly"`, `"yearly"`).

- `current_date`:

  Date. The most recent date in `data`.

## Validity

- Dates in `data` must be strictly increasing (no duplicated
  timestamps).

- `n_dates` must equal `nrow(data)`.

- `source` must have one entry per column of `data`.

- Column names in `data` must not be duplicated.

## See also

[`create_meta_xts`](https://pauloguimaraes871.github.io/factoRverse/reference/create_meta_xts.md),
[`returns_meta_xts-class`](https://pauloguimaraes871.github.io/factoRverse/reference/returns_meta_xts-class.md),
[`metrics_meta_xts-class`](https://pauloguimaraes871.github.io/factoRverse/reference/metrics_meta_xts-class.md),
[`meta_dataframe-class`](https://pauloguimaraes871.github.io/factoRverse/reference/meta_dataframe-class.md)
