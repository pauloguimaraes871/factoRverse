# Create expanded hyperparameter grid list based on tuning method

This function generates an expanded hyperparameter grid list based on
the specified tuning method ('grid_search' or 'random_search') and the
corresponding hyperparameter domain list.

## Usage

``` r
create_expanded_hyper_grid_list(
  hyper_grid_domain_list,
  tuning_method,
  n_iter,
  ml_algorithm
)
```

## Arguments

- hyper_grid_domain_list:

  List containing hyperparameter domains for tuning. Each element should
  include 'distribution_choice', 'pars', and 'value' for
  'random_search'.

- tuning_method:

  Character, method of hyperparameter tuning ("grid_search" or
  "random_search").

- n_iter:

  Integer, number of iterations for random search (ignored for grid
  search).

## Value

List of expanded hyperparameter combinations ready for grid or random
search.

## Details

Depending on the specified tuning method:

- For 'grid_search', it expands the grid of hyperparameter values.

- For 'random_search', it generates random samples based on specified
  distributions.

  - 'constant': Returns a constant value.

  - 'normal': Samples from a normal distribution.

  - 'uniform': Samples from a uniform distribution.

  - 'lognormal': Samples from a lognormal distribution. All generated
    values are rounded to integers if specified in
    'hyper_grid_domain_list'. Duplicates are removed to prevent repeated
    inputs.

## See also

Use with functions that require hyperparameter tuning such as 'train' or
'caret'.
