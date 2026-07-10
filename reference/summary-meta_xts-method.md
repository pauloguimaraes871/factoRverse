# Summary Method for `meta_xts` Objects

Interactively (or via `summary_id`) prints a styled
[`DT::datatable`](https://rdrr.io/pkg/DT/man/datatable.html) summary for
a `meta_xts` object: a numeric summary, a by-year numeric summary, a
series frequency table, and, for `returns_meta_xts` objects, a
performance metrics table.

## Usage

``` r
# S4 method for class 'meta_xts'
summary(
  object,
  summary_id = NULL,
  benchmark_returns_m_xts = NULL,
  active_returns = FALSE,
  ...
)
```

## Arguments

- object:

  An S4 object of class `meta_xts`, possibly a subclass like
  `returns_meta_xts`.

- summary_id:

  Numeric (`1`-`3`, or `1`-`4` for `returns_meta_xts`) or the matching
  character label: `"Numeric Summary Table"`,
  `"Numeric Summary Table by Year"`, `"Series Frequency Table"`, or
  (only for `returns_meta_xts`) `"Performance Metrics Table"`. If
  `NULL`, an interactive menu is shown via
  [`readline()`](https://rdrr.io/r/base/readline.html).

- benchmark_returns_m_xts:

  A single-column `returns_meta_xts` object with benchmark returns,
  required to compute active performance metrics.

- active_returns:

  Logical. Requested default for computing active returns relative to
  `benchmark_returns_m_xts`. Note: when `summary_id` is supplied as the
  numeric `4`, this value is overridden by an interactive
  [`readline()`](https://rdrr.io/r/base/readline.html) prompt regardless
  of what was passed; supplying `summary_id` as the string
  `"Performance Metrics Table"` skips that prompt and honors this
  argument as-is.

- ...:

  Further arguments (not used).

## Value

Invisibly returns `object`; called for the side effect of printing a
[`DT::datatable`](https://rdrr.io/pkg/DT/man/datatable.html) (via
`htmlwidgets`/`htmltools`) to the Viewer.

## Details

Requires the suggested packages `DT`, `htmltools`, and `htmlwidgets`; if
any are missing, an informative
[`stop()`](https://rdrr.io/r/base/stop.html) is raised.
