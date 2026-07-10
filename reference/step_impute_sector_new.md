# Constructor for step_impute_sector

Low-level constructor function for creating a new `step_impute_sector`
step in a recipe. This step is designed to impute missing values by
group (e.g., sectors), using group-specific means or medians. The
constructor is typically used internally and should not be called
directly by users.

## Usage

``` r
step_impute_sector_new(
  terms,
  sector,
  method,
  role,
  trained,
  impute_values,
  skip,
  id
)
```

## Arguments

- terms:

  A `quosure` with the columns to be imputed, as selected via
  [`recipes::terms_select()`](https://recipes.tidymodels.org/reference/terms_select.html).

- sector:

  A `character` string specifying the name of the column in the data
  used to group observations (e.g., "sector").

- method:

  A `character` string specifying the imputation method to use within
  each group. Typically "mean" or "median".

- role:

  A character string describing the role of the created variable
  (usually `"predictor"`). This is required by the `recipes` API.

- trained:

  A logical indicating whether the preprocessing step has been trained.

- impute_values:

  A list of named vectors storing the imputed values computed during
  training (one vector per group).

- skip:

  A logical. Should the step be skipped when the recipe is baked by
  [`recipes::bake()`](https://recipes.tidymodels.org/reference/bake.html)?
  Default is `FALSE`.

- id:

  A character string used to uniquely identify the step. Automatically
  generated if `NULL`.

## Value

A `step` object of class `step_impute_sector`.
