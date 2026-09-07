# Announce whether a chosen meta score is ex-ante or realized

Emits a message naming the basis of a statistic selected as a
meta-portfolio score, together with its counterpart of the other basis.
Mixing the two silently changes what a meta allocation optimizes: an
ex-ante information ratio expresses the conviction embedded in current
positions, while a realized one measures what the portfolio actually
earned.

## Usage

``` r
message_meta_score_basis(stat_name, verbose = TRUE)
```

## Arguments

- stat_name:

  Character. Name of the statistic chosen as the meta score.

- verbose:

  Logical, default `TRUE`. When `FALSE`, nothing is emitted.

## Value

Invisibly, the basis (`"ex-ante"` or `"realized"`), or `NULL` when the
statistic has no counterpart of the other basis.
