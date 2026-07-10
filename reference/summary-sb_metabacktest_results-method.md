# Summary Method for sb_metabacktest_results Class

Provides a detailed summary of an `sb_metabacktest_results` object.
Users can select which summary table to display by specifying the
`summary_id` parameter, either by name or by number. The summary
includes interactive tables styled using the `DT` package.

## Usage

``` r
# S4 method for class 'sb_metabacktest_results'
summary(object, summary_id = NULL, which_backtest_results = NULL)
```

## Arguments

- object:

  An object of class `sb_metabacktest_results`.

- summary_id:

  A character string or numeric value specifying which table to display.

  - By name: Options are:

    - `"Combined_OOS_Testing_Metrics"`

    - `"Mean_Validation_Metrics"`

    - `"Time_Series_OOS_Testing_Metrics"`

    - `"Time_Series_Validation_Metrics"`

    - `"Base_Learners_OOS_Predictions"`

  - By number: Provide a number corresponding to the table (as listed
    when `summary_id` is `NULL`). If `NULL` (default), the method lists
    available tables.

- which_backtest_results:

  Character/numeric selecting which nested results to summarize: the
  meta-backtest itself, the meta learner
  (`meta_learner_sb_backtest_results`), or a base learner
  (`base_learners_sb_backtest_results`). If `NULL`, the method prompts
  interactively.

## Value

Invisibly returns the input `object`.
