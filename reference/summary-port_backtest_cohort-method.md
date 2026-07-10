# Summary Method for port_backtest_cohort Class

Provides a detailed summary of a `port_backtest_cohort` object as an
interactive, styled table (built with the `DT`, `htmltools`, and
`htmlwidgets` packages, which must be installed). The table is selected
via `summary_id` (a table name or its numeric index); if omitted, an
interactive menu lists the options.

## Usage

``` r
# S4 method for class 'port_backtest_cohort'
summary(object, summary_id = NULL)
```

## Arguments

- object:

  An object of class `port_backtest_cohort`.

- summary_id:

  A character string (table name) or numeric index selecting the table
  to display. One of `"Raw Returns Summary"`, `"Net Returns Summary"`,
  `"Raw Active Returns Summary"`, `"Net Active Returns Summary"`,
  `"Direct Cost Summary"`, `"Market Impact Cost Summary"`,
  `"Total Cost Summary"`, `"Turnover Summary"`, `"Weights Summary"`,
  `"Metrics Summary"`, or `"Port Stats Summary"`. If `NULL`, an
  interactive menu is shown.

## Value

Invisibly returns the input `object`.
