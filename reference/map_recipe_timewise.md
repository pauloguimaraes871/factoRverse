# Map a Recipe to Sequential Dates

This method applies a recipes preprocessing pipeline in a time-wise
(point-in-time) manner to a `meta_dataframe` object. For each date, the
recipe is
[`recipes::prep()`](https://recipes.tidymodels.org/reference/prep.html)-ed
(estimating step parameters, e.g. imputation means, winsorization
limits, dummy levels) and
[`recipes::bake()`](https://recipes.tidymodels.org/reference/bake.html)-d
on **only** the cross-section of data available at that date. Because
parameters are re-estimated per date rather than pooled across the whole
panel, no future information leaks into preprocessing - the key
difference from applying a recipe once to the entire dataset. This is
what advances a `meta_dataframe` along the medallion path before it is
handed to the `run_*` functions.

## Usage

``` r
map_recipe_timewise(
  meta_dataframe,
  recipe,
  verbose = TRUE,
  parallel = TRUE,
  type = "signals"
)

# S4 method for class 'meta_dataframe,recipe'
map_recipe_timewise(
  meta_dataframe,
  recipe,
  verbose = TRUE,
  parallel = TRUE,
  type = "signals"
)
```

## Arguments

- meta_dataframe:

  A `meta_dataframe` object.

- recipe:

  A `recipe` object that contains a recipe in its `recipe` slot. In
  special, make sure that:

  - The required meta columns (id, tickers, dates) are assigned the role
    `"id_vars"`.

  - All columns present in the `meta_dataframe` object have an assigned
    role in the recipe.

  - Target variables are preprocessed separately (build a dedicated
    meta_dataframe with the appropriate `type`).

- verbose:

  A logical indicating whether to print messages during the process.
  Default is `TRUE`.

- parallel:

  A logical indicating whether to use parallel processing. Default is
  `TRUE`.

- type:

  A character string indicating the type of data to be preprocessed, to
  be passed to meta_dataframe. Default is `"signals"`.

## Value

A time-wise preprocessed `meta_dataframe`.

## Details

Parallel processing is achieved using
[`furrr::future_map`](https://furrr.futureverse.org/reference/future_map.html);
ensure that an appropriate future plan is set (e.g.,
`future::plan(future::multisession)`).

## Examples

``` r
if (FALSE) { # \dontrun{
# Build a per-date recipe: identifier columns as id_vars, then impute + winsorize predictors
rec <- recipes::recipe(panel@data) %>%
  recipes::update_role(id, tickers, dates, new_role = "id_vars") %>%
  recipes::update_role(recipes::all_numeric(), new_role = "predictor") %>%
  recipes::step_impute_median(recipes::all_numeric_predictors()) %>%
  step_winsorize(recipes::all_numeric_predictors(), probs = c(0.05, 0.95))

silver <- map_recipe_timewise(panel, rec, parallel = FALSE, type = "generic")
} # }
```
