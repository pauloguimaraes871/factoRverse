# Create a tickers_catalog Object

Constructs a `tickers_catalog` object by integrating stock metadata from
multiple data sources. Ensures data consistency, generates unique
identifiers, and classifies stocks based on listing status.

## Usage

``` r
create_tickers_catalog(
  raw_features_m_df,
  date_first_quote,
  date_last_quote,
  ...
)

# S4 method for class 'raw_features_m_df,data.frame,data.frame'
create_tickers_catalog(
  raw_features_m_df,
  date_first_quote,
  date_last_quote,
  n_days_tolerance = 10
)
```

## Arguments

- raw_features_m_df:

  A `raw_features_m_df` (source panel).

- date_first_quote:

  A `data.frame` with columns `tickers` and `date_first_quote`
  (coercible to `Date`).

- date_last_quote:

  A `data.frame` with columns `tickers` and `date_last_quote` (coercible
  to `Date`).

- ...:

  Additional arguments (currently unused).

- n_days_tolerance:

  Numeric scalar (in days). Window used to decide whether a last-quote
  date indicates delisting relative to `raw_features_m_df@current_date`.
  Default: `10`.

## Value

An object of class `tickers_catalog`.

An object of class `tickers_catalog` with slots: `catalog`, `tickers`,
`perm_id` (named by tickers), `tickers_first_quote`,
`tickers_last_quote`, `untraded`, `delisted`, `listed`, `old`,
`current_date`, `meta_dataframe_name`, `n_days_tolerance`, and
`ticker_change_history`.

## Details

This generic performs these responsibilities (method implementations
perform checks described below):

1.  Validate input types and required columns in `date_first_quote` and
    `date_last_quote`.

2.  Verify `tickers` are identical across `raw_features_m_df`,
    `date_first_quote` and `date_last_quote`.

3.  Enforce that `date_last_quote >= date_first_quote` when both are
    present (NAs allowed).

4.  Generate a deterministic `perm_id` per ticker from
    `ticker + date_first_quote`.

5.  Extract `current_date` as the most recent date in
    `raw_features_m_df@data$dates`.

6.  Classify tickers as `untraded` (both dates NA), `delisted`
    (last_quote \< current_date - tolerance), or `listed` (last_quote
    \>= current_date - tolerance).

7.  Emit informative errors or warnings for duplicate tickers,
    mismatched ticker sets, inconsistent NA patterns, invalid date
    ordering, or if no tradable stocks are found.

Method behavior and validations (errors/warnings mirror unit tests):

- Requires the date tables to contain the columns `tickers` +
  `date_first_quote` / `date_last_quote`; otherwise errors with
  "date_first_quote must have columns 'tickers' and 'date_first_quote',
  and date_last_quote must have 'tickers' and 'date_last_quote'."

- Converts date columns to `Date` and errors on duplicate tickers
  ("Duplicate tickers found in date_first_quote or date_last_quote.").

- Errors with "No tradable stocks identified" when all dates are NA.

- Requires the ticker sets to match across inputs; otherwise errors
  "Mismatch in tickers between raw_features_m_df, date_first_quote, and
  date_last_quote."

- Enforces pairwise NA-ness (both NA or neither) and errors
  "date_first_quote and date_last_quote must both be NA or neither."

- Warns when the modal `date_last_quote` is older than
  `current_date - n_days_tolerance` with: "Most common date in
  date_last_quote is not the last date in raw_features_m_df -
  n_days_tolerance. Consider increasing n_days_tolerance"

- Errors if any `date_last_quote < date_first_quote` with:
  "date_last_quote must be greater than or equal to date_first_quote for
  all tickers."

- Warns if any `date_last_quote` is \> `current_date`: "Some
  date_last_quote values are greater than the current_date. This may
  indicate future dates or errors in the data."

- Generates deterministic `perm_id` values using an MD5-derived short
  hash of `ticker + first_quote` (NA first-quote uses "NA"), sorts the
  internal `catalog` by `perm_id`, and classifies tickers into
  `untraded`, `delisted`, `listed`, `old`.

## Functions

- `create_tickers_catalog( raw_features_m_df = raw_features_m_df, date_first_quote = data.frame, date_last_quote = data.frame )`:
  Method that builds a `tickers_catalog` from a `raw_features_m_df` and
  two `data.frame`s containing per-ticker first- and last-quote dates.

## See also

[`tickers_catalog-class`](https://pauloguimaraes871.github.io/factoRverse/reference/tickers_catalog-class.md),
[`update_tickers_catalog`](https://pauloguimaraes871.github.io/factoRverse/reference/update_tickers_catalog.md),
[`read_tickers_catalog`](https://pauloguimaraes871.github.io/factoRverse/reference/read_tickers_catalog.md),
[`create_meta_dataframe`](https://pauloguimaraes871.github.io/factoRverse/reference/create_meta_dataframe.md)

## Examples

``` r
if (FALSE) { # \dontrun{
df_first <- data.frame(tickers = c("A","B"),
                       date_first_quote = as.Date(c("1995-01-01","1996-01-01")))
df_last  <- data.frame(tickers = c("A","B"),
                       date_last_quote  = as.Date(c(Sys.Date()-1, Sys.Date()-20)))
tc <- create_tickers_catalog(raw_features_m_df = raw_mdf,
                             date_first_quote = df_first,
                             date_last_quote  = df_last,
                             n_days_tolerance = 10)
} # }
```
