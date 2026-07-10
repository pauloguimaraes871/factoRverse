# Summary Method for port_backtest_results Class

Provides a detailed summary of a `port_backtest_results` object as an
interactive, styled table (built with the `DT`, `htmltools`, and
`htmlwidgets` packages, which must be installed). The table is selected
via `summary_id` (a table name or its numeric index); if omitted, an
interactive menu lists the options.

## Usage

``` r
# S4 method for class 'port_backtest_results'
summary(object, summary_id = NULL)
```

## Arguments

- object:

  An object of class `port_backtest_results`.

- summary_id:

  A character string (table name) or numeric index selecting the table
  to display. One of `"Returns Summary"`, `"Costs Summary"`,
  `"Metrics Summary"`, `"Stats Summary"`, `"Stock Universe Summary"`,
  `"Final Port Summary"`, `"Final Stock Universe Summary"`, or
  `"Transactions Log"`. If `NULL`, an interactive menu is shown.

## Value

Invisibly returns the input `object`.
