# Validate Updated Backtest Objects

Checks that each named `_m_df` or `_m_xts` object in `new_objects_list`
satisfies the following conditions:

- Its internal `meta_dataframe_name` or `meta_xts_name` matches the
  expected name in `old_objects_names_list[[arg_name]]`.

- It provides new dates beyond those in `dates_covered`.

- Unless it is a daily returns object (named
  `"daily_stock_returns_m_xts"` or `"daily_bench_returns_m_xts"`), it
  starts exactly one month after the last date in `dates_covered`.

- The total number of new dates matches the required `n_update` (taken
  from `config@n_update` inside the function).

## Usage

``` r
check_update_backtest_objects(
  new_objects_list,
  old_objects_names_list,
  old_objects_dates_covered_list,
  n_update
)
```

## Arguments

- new_objects_list:

  A named list of `_m_df` or `_m_xts` objects. Each object must have:

  - A `meta_dataframe_name` or `meta_xts_name` slot (depending on
    whether it’s an `_m_df` or `_m_xts` object).

  - A `@data` slot containing the underlying data (a `data.frame` in the
    case of an `_m_df`, or an `xts` object for an `_m_xts`).

  - A `@current_date` slot (optional for your checks, but used in some
    places).

- old_objects_names_list:

  A named list or vector that maps each `arg_name` in `new_objects_list`
  to the “official” or expected name. For example:
  `old_objects_names_list[["signals_m_df"]]` might be `"my_signals"`.

- old_objects_dates_covered_list:

  A named list of dates that have already been covered by previous
  backtest data. The function checks that the new objects contain dates
  strictly after these.

- n_update:

  The number of new dates that should be present in the updated objects.

## Value

Returns `TRUE` invisibly if all checks pass. Otherwise, it raises an
error via [`stop()`](https://rdrr.io/r/base/stop.html).
