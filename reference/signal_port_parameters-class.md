# Signal Portfolio Parameters

Class to encapsulate parameters for constructing signal portfolios
(portfolio-blending). Only needed when sb_algorithm is 'rp' or 'mvo'.

## Slots

- `cov_est_method`:

  An object of class `cov_est_method` representing the covariance
  estimation method and relevant parameters. Current methods are:
  'sample', 'ewma', 'cc' (constant correlation), 'pca1', 'pca2',
  'shrink_id' (shrinkage to identity matrix), 'shrink_cc' (shrinkage to
  constant correlation). This is only relevant for 'rp' and 'mvo'.

- `mvo_parameters`:

  An object of class `mvo_parameters` representing the parameters for
  mean-variance optimization. This is only relevant for 'mvo'.

- `rp_parameters`:

  An object of class `rp_parameters` representing the parameters for
  risk parity. This is only relevant for 'rp'.

- `hrp_parameters`:

  An object of class `hrp_parameters` representing the parameters for
  hierarchical risk parity. This is only relevant for 'hrp'.

- `mmaf_parameters`:

  An object of class `mmaf_parameters` representing the parameters for
  the MMAF portfolio construction method. This is only relevant for
  'mmaf'.

- `concentration_constraint_policy`:

  The policy to handle concentration constraints. It contains up to to
  three elements:

  - `benchmark`: A character vector describing the benchmark to be used
    to apply constraint. For signal portfolios, possible options are
    theme_ss or theme_sb. For stock portfolios, there must be a
    correspondence in `benchmark_weights_m_df`

  - `max_abs_active_individual_weight`: The maximum absolute individual
    active weights.

  - `max_abs_active_group_weight`: The maximum absolute theme active
    weight used for creating group constraints.
