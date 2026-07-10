# Show Bayesian Model Parameters

Prints the contents of a `bayesian_model_parameters` object: prior
derivation control (e.g. `half_t_df`), `brms` control parameters (e.g.
`chains`, `iter`, `warmup`, `thin`, `seed`, `adapt_delta`), and user
priors (if set) — each section shown, or reported as not set,
individually.

## Usage

``` r
# S4 method for class 'bayesian_model_parameters'
show(object)
```

## Arguments

- object:

  A `bayesian_model_parameters` object to be displayed.
