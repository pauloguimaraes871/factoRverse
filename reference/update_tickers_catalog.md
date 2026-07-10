# Update a `tickers_catalog` Object

Updates a `tickers_catalog` object with a newer `tickers_catalog`,
reconciling ticker classifications (`listed`, `delisted`, `untraded`,
`old`) and resolving ticker changes so that renamed tickers keep a
single `perm_id` across time.

## Usage

``` r
update_tickers_catalog(old_tickers_catalog, new_tickers_catalog, ...)

# S4 method for class 'tickers_catalog,tickers_catalog'
update_tickers_catalog(
  old_tickers_catalog,
  new_tickers_catalog,
  ticker_changes = NULL
)
```

## Arguments

- old_tickers_catalog:

  A `tickers_catalog` S4 object representing the previously stored
  catalog (the state as of the last update).

- new_tickers_catalog:

  A `tickers_catalog` S4 object containing the latest batch of stock
  data. Its `current_date` must equal `old_tickers_catalog@current_date`
  plus one month, and its maximum `tickers_last_quote` must be strictly
  greater than the old catalog's.

- ...:

  Additional arguments (currently unused; reserved for future methods).

- ticker_changes:

  A `data.frame` mapping tickers renamed between `old_tickers_catalog`
  and `new_tickers_catalog`, with columns:

  `new_tickers`

  :   Character. The new ticker symbol observed in
      `new_tickers_catalog`.

  `old_tickers`

  :   Character. The corresponding previous ticker symbol in
      `old_tickers_catalog`.

  `change_date`

  :   Date. The date on which the ticker change occurred.

  Must contain no `NA`s and no duplicated `new_tickers`/`old_tickers`.
  Defaults to `NULL`, treated as an empty (zero-row) `data.frame`, i.e.
  no ticker changes between updates. Any ticker newly observed in
  `new_tickers_catalog` and not listed here is treated as a new IPO; any
  ticker missing from `old_tickers_catalog`'s successor without a
  matching entry here triggers an error.

## Value

A new `tickers_catalog` S4 object combining `old_tickers_catalog` and
`new_tickers_catalog`:

- Renamed tickers keep the `perm_id` of their pre-change ticker (looked
  up through the full ticker-change history, not just the current call's
  `ticker_changes`).

- Genuinely new tickers keep the hash-based `perm_id` assigned by
  [`create_tickers_catalog()`](https://pauloguimaraes871.github.io/factoRverse/reference/create_tickers_catalog.md).

- At a rename boundary, `tickers_last_quote` of the old ticker and
  `tickers_first_quote` of the new ticker are both set to `change_date`.

- Pre-rename ticker rows are retained with `old = TRUE` and
  `listed = delisted = untraded = FALSE`, so history is never dropped.

- `current_date` is taken from `new_tickers_catalog`;
  `meta_dataframe_name` and `n_days_tolerance` are taken from
  `old_tickers_catalog`.

- `ticker_change_history` accumulates `ticker_changes` across calls.

## Details

The update proceeds in three stages:

1.  **Validation** of `ticker_changes` structure/types/uniqueness, of
    the partition of newly added tickers into IPOs vs. renames, of
    classification transitions, and of date consistency between the two
    catalogs.

2.  **`perm_id` reassignment**, inheriting the predecessor's `perm_id`
    for renamed tickers.

3.  **Recombination**, re-appending old (pre-rename) rows and merging
    `ticker_change_history`.

Validation failures ([`stop()`](https://rdrr.io/r/base/stop.html))
include: malformed `ticker_changes` (missing columns, wrong types,
`NA`s, duplicates); newly added tickers that cannot be decomposed into
IPOs and renamed old tickers; a `delisted` ticker being renamed, or
becoming `listed`/`untraded`; an `untraded` ticker becoming `listed` (or
vice versa), with or without a rename; a `tickers_first_quote`/
`tickers_last_quote` mismatch for tickers common to both catalogs
(delisted tickers' `tickers_last_quote` must not change); an IPO's
`tickers_first_quote` earlier than `old_tickers_catalog@current_date`;
`new_tickers_catalog`'s maximum `tickers_last_quote` not exceeding the
old catalog's; and `new_tickers_catalog@current_date` not exactly one
month after `old_tickers_catalog@current_date`. A mismatched
`n_days_tolerance` between catalogs produces a
[`warning()`](https://rdrr.io/r/base/warning.html) rather than a
[`stop()`](https://rdrr.io/r/base/stop.html).

## See also

[`create_tickers_catalog`](https://pauloguimaraes871.github.io/factoRverse/reference/create_tickers_catalog.md),
[`read_tickers_catalog`](https://pauloguimaraes871.github.io/factoRverse/reference/read_tickers_catalog.md),
[`tickers_catalog-class`](https://pauloguimaraes871.github.io/factoRverse/reference/tickers_catalog-class.md)

## Examples

``` r
if (FALSE) { # \dontrun{
updated_catalog <- update_tickers_catalog(
  old_tickers_catalog = old_catalog,
  new_tickers_catalog = new_catalog,
  ticker_changes = data.frame(
    new_tickers = "BRAV3",
    old_tickers = "RRRP3",
    change_date = as.Date("2001-06-02")
  )
)
} # }
```
