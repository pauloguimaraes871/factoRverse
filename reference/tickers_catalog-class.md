# tickers_catalog-class

An S4 class to store stock metadata and bookkeeping for ticker lifecycle
events (listings, delistings, ticker changes).

## Details

The class centralizes per-ticker metadata used by universe construction
and backtests. `perm_id` is intended to be stable across ticker renames
(constructed from ticker + first-quote date). The validity method
enforces mutual exclusivity of classification vectors (a ticker cannot
be simultaneously `listed`, `delisted`, `untraded` or `old`), consistent
lengths, a single `current_date`, and that `catalog` is ordered by
`perm_id` with `perm_id` names matching `tickers`.

## Slots

- `catalog`:

  A `data.frame` with one row per ticker and columns at least:
  `tickers`, `tickers_first_quote` (Date), `tickers_last_quote` (Date),
  and `perm_id`.

- `tickers`:

  Character vector of tickers.

- `perm_id`:

  Character vector of stable unique identifiers (named by `tickers`).

- `tickers_first_quote`:

  Date vector (first observed trading date per ticker).

- `tickers_last_quote`:

  Date vector (last observed trading date per ticker).

- `untraded`:

  Character vector of tickers with no trading dates (both dates NA).

- `delisted`:

  Character vector of delisted tickers (last quote older than tolerance
  window).

- `listed`:

  Character vector of currently listed tickers.

- `old`:

  Character vector of tickers that were renamed/changed.

- `current_date`:

  A single `Date` representing the catalogue's current reference date.

- `meta_dataframe_name`:

  Character: name of the source meta_dataframe (usually the raw features
  input).

- `n_days_tolerance`:

  Numeric: number of days tolerance used to decide delisting vs listed
  status.

- `ticker_change_history`:

  Any: free-form record of ticker-change operations (may be `NULL`).

## Common validity errors

- "A stock can't be untraded and delisted." — overlapping
  classifications found.

- "Length of listed + delisted + untraded + old does not equal length of
  tickers." — partitioning mismatch.

- "perm_id must be named according to tickers." — naming mismatch
  between `perm_id` and `tickers`.

## See also

[`meta_dataframe-class`](https://pauloguimaraes871.github.io/factoRverse/reference/meta_dataframe-class.md)

## Examples

``` r
if (FALSE) { # \dontrun{
df_first <- data.frame(tickers = c("A", "B"),
                       date_first_quote = as.Date(c("2020-01-01","2020-06-01")))
df_last  <- data.frame(tickers = c("A", "B"),
                       date_last_quote  = as.Date(c(NA, "2026-06-30")))
tickers_catalog_obj <- create_tickers_catalog(raw_features_m_df = raw_mdf,
                                              date_first_quote = df_first,
                                              date_last_quote  = df_last,
                                              n_days_tolerance = 10)
} # }
```
