# Create a meta_dataframe

Generic constructor that creates an S4 `meta_dataframe` or an
appropriate subclass from supplied input. Dispatches on the class of
`data` and implements specialized behavior for `data.frame` and
structured wide-panel `list` inputs.

## Usage

``` r
create_meta_dataframe(data, meta_dataframe_name = "not_identified", ...)

# S4 method for class 'data.frame'
create_meta_dataframe(
  data,
  meta_dataframe_name = "not_identified",
  workflow = NULL,
  ss_backtest_workflow = NULL,
  sb_backtest_workflow = NULL,
  port_backtest_workflow = NULL,
  type = "generic",
  ...
)

# S4 method for class 'list'
create_meta_dataframe(
  data,
  tickers,
  dates,
  features_names,
  meta_dataframe_name = "not_identified",
  data_format = "wide",
  tickers_on = "rows"
)
```

## Arguments

- data:

  A `list` where each element is a matrix, `data.frame` or tibble
  representing a feature. All elements must have identical dimensions
  (rows = entities, columns = time points).

- meta_dataframe_name:

  Character. Name for the resulting object (default:
  `"not_identified"`).

- ...:

  Additional arguments (ignored by many branches).

- workflow:

  Optional list. Workflow entries stored in the `workflow` slot.

- ss_backtest_workflow:

  Optional list. Required when `type = "signal_universe"`.

- sb_backtest_workflow:

  Optional list. Required when `type = "oos_sb_outputs"`.

- port_backtest_workflow:

  Optional list. Required when `type = "stock_universe"`.

- type:

  Character. Determines which subclass is instantiated (see generic
  `type` values). For `"generic"` the method warns about missing monthly
  dates but still constructs the object; specialized `type` values
  enforce additional required workflow args.

- tickers:

  Character vector. Row identifiers for the elements of `data`; must be
  unique and length equal to nrow of each element.

- dates:

  Date vector. Column identifiers for the elements of `data`; must be
  `Date`, unique, have the same day-of-month, and be consecutive by
  month (monthly panel).

- features_names:

  Character vector. Names assigned to each element of `data`; length
  must equal `length(data)`.

- data_format:

  Character. Currently only `"wide"` is supported; the method errors
  otherwise.

- tickers_on:

  Character. Currently only `"rows"` is supported; the method errors
  otherwise.

## Value

An S4 object of class `meta_dataframe` or an appropriate subclass.
Populated slots: `data`, `signals`, `unique_dates`, `unique_tickers`,
`n_obs`, `current_date`, `meta_dataframe_name`, and optional
workflow/backtest slots.

An object of class `meta_dataframe` or one of its subclasses with
metadata: `unique_dates`, `unique_tickers`, `n_obs`, `current_date`,
`signals`, `meta_dataframe_name`.

A `raw_features_m_df` (subclass of `meta_dataframe`) containing a
long-format `data.frame` with `id`, `tickers`, `dates` and one column
per feature (named by `features_names`), plus metadata slots.

## Details

Create a meta_dataframe Object

Prefer this high-level constructor to calling `new(...)` directly
because it performs structural validation and populates metadata
consistently. The generic only dispatches — see the method-specific
documentation for exact validation rules and error conditions (a number
of tests assert these validations).

— Validation performed by this method (tests assert these behaviours)

- Calls
  [`is_coercible_to_meta_dataframe()`](https://pauloguimaraes871.github.io/factoRverse/reference/is_coercible_to_meta_dataframe.md)
  which checks: required columns, `dates` class, `id` format, ordering,
  uniqueness and NA presence.

- If `type == "generic"` a warning is emitted for missing months in the
  sequence (monthly cadence).

- For specialized types the method requires the corresponding workflow
  argument: `ss_backtest_workflow` for `signal_universe`,
  `sb_backtest_workflow` for `oos_sb_outputs`, `port_backtest_workflow`
  for `stock_universe`.

— Validation performed by this method (tests assert these behaviours)

- `data` must be a `list`; each element must be a matrix, `data.frame`,
  or tibble.

- All elements must have identical numbers of rows and columns;
  otherwise an error is thrown (tests expect messages such as "All
  elements in the list must have the same number of rows/columns.").

- `features_names` length must equal `length(data)`.

- No element may consist entirely of `NA`; otherwise an error ("One or
  more datasets contain only NA values.").

- `tickers` must be a unique `character` vector whose length equals nrow
  of elements; errors on non-character or duplicated tickers.

- `dates` must be class `Date`, unique, all share the same day-of-month,
  and be consecutive by month; failures raise informative errors used in
  tests.

- Elements must not already contain columns named `tickers` or `dates`;
  elements must not contain values matching provided `tickers` or
  `dates` (these conditions raise errors asserted by tests).

- Only `data_format = "wide"` and `tickers_on = "rows"` are accepted.

## Functions

- `create_meta_dataframe(data.frame)`: Create a `meta_dataframe` (or
  subclass) from a long-format `data.frame`.

- `create_meta_dataframe(list)`: Create a `meta_dataframe` from a
  structured `list` of wide feature matrices/tibbles.

## See also

[`meta_dataframe-class`](https://pauloguimaraes871.github.io/factoRverse/reference/meta_dataframe-class.md),
[`create_meta_xts`](https://pauloguimaraes871.github.io/factoRverse/reference/create_meta_xts.md),
[`create_tickers_catalog`](https://pauloguimaraes871.github.io/factoRverse/reference/create_tickers_catalog.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# See the data.frame and list method examples below.
} # }

if (FALSE) { # \dontrun{
df <- data.frame(
  id = c("A-2026-01-31","B-2026-01-31"),
  tickers = c("A","B"),
  dates = as.Date(c("2026-01-31","2026-01-31")),
  momentum = c(0.1, -0.2),
  value = c(0.05, 0.03),
  stringsAsFactors = FALSE
)
mdf <- create_meta_dataframe(df, meta_dataframe_name = "features_example", type = "generic")
# For signal_universe:
su <- create_meta_dataframe(df, meta_dataframe_name = "signal_universe_example",
                            type = "signal_universe",
                            ss_backtest_workflow = list(params = "example"))
} # }

if (FALSE) { # \dontrun{
tickers <- c("Stock A","Stock B")
dates <- as.Date(c("2001-03-15","2001-04-15"))
feat1 <- matrix(c(0,1,2,3), nrow = 2, byrow = TRUE)
feat2 <- matrix(c(4,5,6,7), nrow = 2, byrow = TRUE)
feat3 <- matrix(c(8,9,10,11), nrow = 2, byrow = TRUE)
features_list <- list(Alpha = feat1, Beta = feat2, Gamma = feat3)

raw_mdf <- create_meta_dataframe(features_list,
                                 tickers = tickers,
                                 dates = dates,
                                 features_names = c("Alpha","Beta","Gamma"),
                                 meta_dataframe_name = "raw_features_example")
} # }
```
