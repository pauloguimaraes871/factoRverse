# Ex-ante and realized counterparts among portfolio statistics

Lookup table pairing the portfolio statistics that exist in two
flavours. A base `port_stats_m_df` stores both side by side under names
that do not advertise the difference: figures such as `track_err` and
`info_ratio` are computed from a realized return series, while
`act_risk` and `IR` are computed from the portfolio's positions and its
covariance matrix at the formation date.

## Usage

``` r
meta_score_basis_table()
```

## Value

A `data.frame` with columns `stat`, `basis` (`"ex-ante"` or
`"realized"`), `counterpart` and `note`.
