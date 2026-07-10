# Decompose Feature Importance from Meta-Learner to Base Features

Internal utility to propagate feature importance values from a
meta-model to its constituent base learners. The function adjusts and
redistributes meta-level feature importance into the features used by
each base learner, weighting them proportionally by their within-learner
relative importance.

## Usage

``` r
decompose_feature_importance(
  most_recent_meta_date,
  meta_feature_importance,
  base_identifiers,
  base_feature_importance_filtered
)
```

## Arguments

- most_recent_meta_date:

  Date. A single date used to tag all decomposed importance values. This
  is typically the date of the most recent meta-model estimation.

- meta_feature_importance:

  A `data.frame` containing feature importance values from the
  meta-learner. Must include at least:

  - `tickers`: Identifiers for the base learners used in the meta-model.

  - `importance`: The estimated importance of each base learner.

- base_identifiers:

  A character vector of base learner identifiers. These should match the
  `tickers` in `meta_feature_importance`.

- base_feature_importance_filtered:

  A list of `data.frame`s, each containing feature importance values for
  a base learner. Each element must correspond in order to
  `base_identifiers`, and each `data.frame` must include:

  - `tickers`: Feature identifiers used in the base learner.

  - `importance`: Their respective importance values.

## Value

A `data.frame` (in `meta_dataframe` format) with the decomposed feature
importance. Columns include:

- `id`: Unique identifier combining feature and date.

- `dates`: Set to `most_recent_meta_date`.

- `tickers`: Feature names (or base learner features).

- `importance`: Decomposed and consolidated importance value.

## Details

For each base learner:

1.  The meta importance is multiplied by each feature’s relative
    importance (within the base model).

2.  The adjusted importance is assigned to the respective feature.

3.  The base learner row is removed from the meta table and replaced by
    its decomposed features.

After looping through all base learners, duplicated features (from
different learners) are consolidated by summing their importances.
