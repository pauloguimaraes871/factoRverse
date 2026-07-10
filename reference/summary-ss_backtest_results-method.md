# Summary Method for ss_backtest_results Class

Provides a detailed summary of an `ss_backtest_results` object. Users
can select which summary table to display by specifying the `summary_id`
parameter (numeric index or exact name); if omitted, an interactive menu
is shown. The summary includes interactive tables styled using the `DT`
package. Requires the `DT`, `htmltools`, and `htmlwidgets` packages.

## Usage

``` r
# S4 method for class 'ss_backtest_results'
summary(object, summary_id = NULL)
```

## Arguments

- object:

  An object of class `ss_backtest_results`.

- summary_id:

  A character string or numeric value specifying which table to display.
  One of: `"Eligibility_Count"`, `"Theme_Eligibility_Proportion"`,
  `"Eligibility_Over_Time"`, `"Metric_Rate_of_Change"`,
  `"Metrics_By_Theme"`, `"Metrics_By_Eligibility"`, `"Top_Signals"`,
  `"Top_Themes"`.

## Value

Invisibly returns the input `object`.
