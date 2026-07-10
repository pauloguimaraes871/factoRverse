# Create a `meta_xts` Object (`returns_meta_xts` or `metrics_meta_xts`)

Constructs a
[`returns_meta_xts-class`](https://pauloguimaraes871.github.io/factoRverse/reference/returns_meta_xts-class.md)
or
[`metrics_meta_xts-class`](https://pauloguimaraes871.github.io/factoRverse/reference/metrics_meta_xts-class.md)
object from an `xts` object, a `data.frame` (wide or long format), or a
`meta_dataframe`, auto-detecting frequency and filling in metadata
slots.

## Usage

``` r
create_meta_xts(
  data,
  type = c("returns", "metrics"),
  asset_type = "not_identified",
  meta_xts_name = "not_identified",
  metric_name = NULL,
  workflow = NULL,
  source = NULL,
  ...
)

# S4 method for class 'xts'
create_meta_xts(
  data,
  type = c("returns", "metrics"),
  asset_type = "not_identified",
  meta_xts_name = "not_identified",
  metric_name = NULL,
  workflow = NULL,
  source = NULL
)

# S4 method for class 'data.frame'
create_meta_xts(
  data,
  type = c("returns", "metrics"),
  asset_type = "not_identified",
  meta_xts_name = "not_identified",
  metric_name = NULL,
  workflow = NULL,
  source = NULL,
  data_format = c("wide", "long"),
  dates = NULL
)

# S4 method for class 'meta_dataframe'
create_meta_xts(
  data,
  type = c("returns", "metrics"),
  asset_type = "not_identified",
  meta_xts_name = "not_identified",
  metric_name = NULL,
  source = NULL
)
```

## Arguments

- data:

  An `xts` object, a `data.frame`, or a `meta_dataframe` containing the
  time series data.

- type:

  Character. Either `"returns"` (built as `returns_meta_xts`, no holes
  allowed) or `"metrics"` (built as `metrics_meta_xts`, holes allowed).
  Defaults to `"returns"` (the first value of `c("returns", "metrics")`,
  resolved via [`match.arg()`](https://rdrr.io/r/base/match.arg.html)).

- asset_type:

  Character. Type of asset for `returns_meta_xts` objects (e.g.
  `"stock"`, `"ports"`). Defaults to `"not_identified"`, which emits a
  [`message()`](https://rdrr.io/r/base/message.html) when
  `type = "returns"`.

- meta_xts_name:

  Character. A label for the resulting object(s). Defaults to
  `"not_identified"`.

- metric_name:

  Character. Name of the metric/return series. If `NULL`, defaults to
  `"returns"`/`"metrics"` for `xts`/wide-`data.frame` input, or to the
  feature column name(s) for long-`data.frame` input. A vector supplied
  for long input must match the number of feature columns in length.

- workflow:

  An ANY object recording processing history. Defaults to `NULL`. For
  `meta_dataframe` input, the source object's own `workflow` is carried
  over with a coercion entry appended.

- source:

  A character vector indicating data origin for each column. If `NULL`,
  defaults to `"not_identified"` repeated for each column.

- ...:

  Additional arguments passed to the underlying method.

- data_format:

  Character. `data.frame` input only: `"wide"` (columns are already
  assets/metrics) or `"long"` (requires `tickers` and `dates` columns;
  each non-id column becomes a separate result). Defaults to `"wide"`.

- dates:

  An optional vector of dates, sorted ascending. For wide input it
  supplies the time index directly; for long input it overrides the
  pivoted `dates` column. Unsorted input is an error rather than being
  silently re-sorted, so data the caller believes is already aligned is
  never reordered without their knowledge.

## Value

A single `returns_meta_xts` or `metrics_meta_xts` object, **except**
when `data` is a `data.frame` in long format with more than one feature
column: then a named `list` of such objects is returned, one per feature
column, named after the column.

## See also

[`meta_xts-class`](https://pauloguimaraes871.github.io/factoRverse/reference/meta_xts-class.md),
[`returns_meta_xts-class`](https://pauloguimaraes871.github.io/factoRverse/reference/returns_meta_xts-class.md),
[`metrics_meta_xts-class`](https://pauloguimaraes871.github.io/factoRverse/reference/metrics_meta_xts-class.md),
[`create_meta_dataframe`](https://pauloguimaraes871.github.io/factoRverse/reference/create_meta_dataframe.md)
