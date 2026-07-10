# Summary Method for sb_backtest_results Class

Provides a detailed summary of an `sb_backtest_results` object. Users
can select which summary table to display by specifying the `summary_id`
parameter, either by name or by number. The summary includes interactive
tables styled using the `DT` package and leverages the summary method of
the `meta_dataframe` class for the `oos_sb_outputs_m_df` component.

## Usage

``` r
# S4 method for class 'sb_backtest_results'
summary(object, summary_id = NULL)
```

## Arguments

- object:

  An object of class `sb_backtest_results`.

- summary_id:

  A character string or numeric value specifying which table to display.

  - By name: Options are:

    - `"OOS_SB_Outputs_Summary"` (Delegates to `meta_dataframe` summary)

    - `"OOS_Testing_Eval_Metrics"`

    - `"Chosen_Eval_Metric_Validation"`

    - `"Feature_Importance"`

  - By number: Provide a number corresponding to the table (as listed
    when `summary_id` is `NULL`). If `NULL` (default), the method lists
    available tables.

## Value

Invisibly returns the input `object`.
