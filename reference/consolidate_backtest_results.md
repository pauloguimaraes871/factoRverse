# Consolidate Newly Produced Backtest Results

This function takes a named list of newly produced backtest results (the
"new" objects), and merges each with the corresponding old object from
an explicitly provided list (`old_backtest_outputs_list`). The old
object is treated as the "main" data source, and the new object as the
"supplemental" data to be appended.

Specifically:

- For objects whose name ends with `"_m_df"`, the function uses
  [`consolidate_generic_meta_dataframes`](https://pauloguimaraes871.github.io/factoRverse/reference/consolidate_generic_meta_dataframes.md).

- For objects whose name ends with `"_m_xts"`, the function uses
  [`consolidate_generic_meta_xts`](https://pauloguimaraes871.github.io/factoRverse/reference/consolidate_generic_meta_xts.md),
  with an operation of `"bind_rows"`.

## Usage

``` r
consolidate_backtest_results(new_backtest_outputs_list, old_backtest_results)
```

## Arguments

- new_backtest_outputs_list:

  A named list of **new** backtest objects (S4). Each element's name
  (e.g. `"port_weights_m_df"`) must match the slot name used in the old
  backtest. These objects are the "additional" data.

- old_backtest_results:

  An **old** port_backtest_results objects (S4). These objects are
  considered the "main" data source.

## Value

A named list of **consolidated** S4 objects. The returned list has the
same names as `new_backtest_outputs_list`, but each item now contains
**both** old and new data merged together in `new_obj@data`.

## Details

1.  Looks up an old object in `old_backtest_outputs_list` by the same
    key.

2.  If the name ends with `"_m_df"`, calls
    `consolidate_generic_meta_dataframes(main_generic_m_df = old_obj, supplemental_generic_m_df = new_obj, ...)`.

3.  If the name ends with `"_m_xts"`, calls
    `consolidate_generic_meta_xts(main_generic_m_xts = old_obj, supplemental_generic_m_xts = new_obj, ...)`.

4.  Updates `new_obj@data` slot with the merged data.

5.  Returns the updated `new_obj` in the output list.

An error is thrown if no matching old object is found, or if the slot
name does not match the `"_m_df"` or `"_m_xts"` pattern.

## See also

[`consolidate_generic_meta_dataframes`](https://pauloguimaraes871.github.io/factoRverse/reference/consolidate_generic_meta_dataframes.md),
[`consolidate_generic_meta_xts`](https://pauloguimaraes871.github.io/factoRverse/reference/consolidate_generic_meta_xts.md)

## Examples

``` r
if (FALSE) { # \dontrun{
  # Suppose you have:
  #   - old_results (port_backtest_results) with existing data
  #   - a new list 'new_list' with updated S4 objects

  # Construct the 'old_backtest_outputs_list':
  old_list <- list(
    port_weights_m_df   = old_results@port_weights_m_df,
    stock_universe_m_df = old_results@stock_universe_m_df,
    port_returns_m_xts  = old_results@port_returns_m_xts,
    port_costs_m_xts    = old_results@port_costs_m_xts
    # add more as needed...
  )

  # Then call:
  updated_list <- consolidate_backtest_results(
    new_backtest_outputs_list = new_list,
    old_backtest_outputs_list = old_list
  )
} # }
```
