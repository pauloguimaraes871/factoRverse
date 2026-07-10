# Add step_impute_sector to a Recipe

Adds a `step_impute_sector` step to a recipe. This step imputes missing
values group-wise, replacing NAs in the selected columns with the
sector-level mean or median. The imputation values are estimated during
`prep()` (per sector) and applied during `bake()`.

## Usage

``` r
step_impute_sector(
  recipe,
  ...,
  sector,
  method = "mean",
  role = NA,
  trained = FALSE,
  impute_values = NULL,
  skip = FALSE,
  id = recipes::rand_id("impute_sector")
)
```

## Arguments

- recipe:

  A recipe object.

- ...:

  Additional arguments passed to methods (not used).

- sector:

  A character string specifying the name of the sector variable.

- method:

  A character string specifying the imputation method. Either "mean" or
  "median".

- role:

  The role to assign to the new variables created by this step. Default
  is `NA`.

- trained:

  A logical indicating if the step has been trained. Default is `FALSE`.

- impute_values:

  A list of computed imputation values for each variable.

- skip:

  A logical indicating if the step should be skipped during baking.
  Default is `FALSE`.

- id:

  A character string for the step ID. Default is a random ID.

## Value

A `step_impute_sector` object with computed `impute_values`.

## See also

[`map_recipe_timewise`](https://pauloguimaraes871.github.io/factoRverse/reference/map_recipe_timewise.md);
[`recipes::step_impute_mean`](https://recipes.tidymodels.org/reference/step_impute_mean.html)
for the ungrouped analogue.
