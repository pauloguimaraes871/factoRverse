#' Map a Recipe to Sequential Dates
#'
#' This method maps recipes in a time-wise manner to a
#' \code{meta_dataframe} object using the \code{recipe} supplied.
#' For each date, the recipe is prepped and baked on the
#' subset of data available at the specific date. This ensures that no future
#' information is used during preprocessing.
#'
#' Parallel processing is achieved using \code{furrr::future_map}; ensure that an appropriate
#' future plan is set (e.g., \code{future::plan(future::multisession)}).
#'
#' @param meta_dataframe A \code{meta_dataframe} object.
#' @param recipe A \code{recipe} object that contains a recipe in its \code{recipe} slot.
#' In special, make sure that:
#' \itemize{
#'   \item The required meta columns (id, tickers, dates) are assigned the role \code{"id_vars"}.
#'   \item All columns present in the \code{meta_dataframe} object have an assigned role in the recipe.
#'   \item Target variables are preproessed separetely
#' }
#'
#' @param verbose A logical indicating whether to print messages during the process. Default is \code{FALSE}.
#'
#' @return A time-wise preprocessed \code{meta_dataframe}.
#'
#' @export
setGeneric("map_recipe_timewise", function(meta_dataframe, recipe, verbose = TRUE, parallel = TRUE, type = "signals") {
  standardGeneric("map_recipe_timewise")
})


setMethod("map_recipe_timewise",
          signature(meta_dataframe = "meta_dataframe", recipe = "recipe"),
          function(meta_dataframe, recipe, verbose = TRUE, parallel = TRUE, type = "signals") {

            #Initial steps
            #################
            ##Extract objs
            meta_dataframe_current_date <- meta_dataframe@current_date
            meta_dataframe_workflow <- meta_dataframe@workflow
            meta_dataframe_name <- meta_dataframe@meta_dataframe_name
            pre_silver_features_m_df <- meta_dataframe@data

            ##Check that id, tickers, and dates are present with role "id_vars"
            var_info <- recipe$term_info
            required_id_vars <- c("id", "tickers", "dates")
            for (var in required_id_vars) {
               if (!(var %in% var_info$variable))
                 stop(paste("Required variable", var, "is not present in the recipe."))
               role <- var_info$role[var_info$variable == var]
               if (!("id_vars" %in% role))
                 stop(paste("Variable", var, "must have the role 'id_vars'."))
             }

             ##Check that all columns in meta_dataframe have a role assigned in recipe
             missing_roles <- var_info %>% dplyr::filter(is.na(role)) %>% dplyr::pull(variable)
             if (length(missing_roles) > 0)
               stop(paste("The following columns do not have an assigned role in the recipe:",
                            paste(missing_roles, collapse = ", ")))

             ##Check that var_info$role contains either only outcome or no outcome
             has_all_outcome <- all(sapply(var_info$role, function(x) "outcome" %in% x)) #This is ok
             has_outcome <- any(sapply(var_info$role, function(x) "outcome" %in% x))
             #It can be has_all_outcome and has_outcome, but not only has_outcome
             if (has_outcome && !has_all_outcome){
               stop("Please create a specific meta_dataframe with appropriate type to manage targets separately.")
             }

            #################

            #Process each date in parallel.
            #################
            dates_m_vector <- pre_silver_features_m_df %>% dplyr::pull(dates) %>% unique() %>% sort()

            ##Preprocess using furrr:: or purrr::

              ###Define a function to process time-wise
              process_date <- function(current_date) {

                pre_silver_features_m_d_ref <- pre_silver_features_m_df %>% dplyr::filter(dates == current_date)

                if (nrow(pre_silver_features_m_d_ref) < 2) {
                  warning("Not enough data to prep the recipe for date: ", current_date)
                  return(NULL)
                }

                # Prep and bake the recipe
                rec_prepped <- recipes::prep(recipe, training = pre_silver_features_m_d_ref,
                                             retain = TRUE, verbose = verbose)

                baked_data <- recipes::bake(rec_prepped, new_data = pre_silver_features_m_d_ref)

                return(baked_data)
              }

              ###Apply preprocessing using parallel or sequential approach
              if (parallel) {
                preprocessed_pre_silver_features_m_d_ref_list <-
                  furrr::future_map(dates_m_vector, process_date, .options = furrr::furrr_options(seed = TRUE))
              } else {
                preprocessed_pre_silver_features_m_d_ref_list <-
                  purrr::map(dates_m_vector, process_date)
              }

            #################

            #Combine the processed rows into a single meta_dataframe
            #################

              ##Remove NULL results
              preprocessed_pre_silver_features_m_d_ref_list <-
                preprocessed_pre_silver_features_m_d_ref_list[!sapply(preprocessed_pre_silver_features_m_d_ref_list, is.null)]

              ##If all dates failed, return an empty dataframe
              if (length(preprocessed_pre_silver_features_m_d_ref_list) == 0) {
                stop("All preprocessing steps failed due to insufficient data.")
              }

              ##Combine and sort
              preprocessed_features_m_df <- dplyr::bind_rows(preprocessed_pre_silver_features_m_d_ref_list) %>%
                dplyr::arrange(id) %>% as.data.frame()

              ###Ensure consistent columns across processed datasets by introducing missing factors as 0
                ###Identify columns created by step_dummy for handling different factor levels across time
                dummy_steps <- purrr::keep(recipe$steps, ~ inherits(.x, "step_dummy"))
                ###Extract the factor cols
                factor_columns <- unique(unlist(purrr::map(dummy_steps, function(step) {
                  step_tidy <- recipes::tidy(step)
                  step_tidy$terms  # Extracts the actual names of the dummy variables
                })))
                ####Get dummy_columns_to_fill
                all_columns <- unique(unlist(lapply(preprocessed_pre_silver_features_m_d_ref_list, colnames)))
                dummy_columns_to_fill <- all_columns[stringr::str_detect(all_columns, paste0("^", factor_columns, "_"))]
                if (length(dummy_columns_to_fill) > 0) {
                  ####Identify dates where NA exists in dummy columns and replace with 0
                  preprocessed_features_m_df <- preprocessed_features_m_df %>%
                    dplyr::mutate(dplyr::across(dplyr::all_of(dummy_columns_to_fill), ~ ifelse(is.na(.), 0, .)))
                }


            #################

            #Return the preprocessed meta_dataframe
            #################
                ##Finalize with the workflow
                new_workflow <- list(
                  list(current_date = meta_dataframe_current_date,  # Current date
                       timestamp = Sys.time(),        # Timestamp
                       recipe = recipe,
                       call = match.call()
                  )
                )

                ##Recreate
                silver_features_m_df <- create_meta_dataframe(preprocessed_features_m_df,
                                                              meta_dataframe_name = meta_dataframe_name,
                                                              workflow = c(meta_dataframe_workflow, new_workflow),
                                                              type = type)


            ##Rename
            names(silver_features_m_df@workflow)[length(silver_features_m_df@workflow)] <-
              paste0("preprocessing_recipe", "_", silver_features_m_df@current_date)

            return(silver_features_m_df)


          })


# step_winsorize------------------------------------
#' @title Prepare Step: step_winsorize
#' @description
#' Computes the lower and upper winsorization limits based on specified quantiles.
#' Called internally by `prep()` on a recipe with `step_winsorize()`.
#'
#' @param x A `step_winsorize` object.
#' @param training A data frame used to estimate the quantile thresholds.
#' @param info An optional `term_info` object, passed by the recipe infrastructure (not used here).
#' @param ... Additional arguments passed to methods (not used).
#'
#' @return A trained `step_winsorize` object with calculated winsorization limits stored.
#' @export
step_winsorize <- function(recipe, ..., role = NA, trained = FALSE,
                           probs = c(0.05, 0.95), skip = FALSE,
                           id = recipes::rand_id("winsorize")) {

  terms <- rlang::enquos(...)

  # If no columns are selected, apply to all numeric variables
  if (rlang::is_empty(terms)) {
    terms <- rlang::exprs(dplyr::everything())
  }

  recipes::add_step(
    recipe,
    step_winsorize_new(
      terms = terms,
      role = role,
      trained = trained,
      probs = probs,
      winsor_limits = NULL,
      skip = skip,
      id = id
    )
  )
}

#' Constructor for step_winsorize
step_winsorize_new <- function(terms, role, trained, probs, winsor_limits, skip, id) {
    recipes::step(
      subclass = "winsorize",
      terms = terms,
      role = role,
      trained = trained,
      probs = probs,
      winsor_limits = winsor_limits,
      skip = skip,
      id = id
    )
}

#' @title Prepare Method for step_winsorize
#' @description Estimates lower and upper bounds for winsorization based on quantiles for each selected column.
#'
#' @param x A `step_winsorize` object.
#' @param training A data frame of training data used to estimate winsorization thresholds.
#' @param info An optional `term_info` object (not used here).
#' @param ... Additional arguments passed to methods (currently not used).
#'
#' @importFrom recipes prep
#' @return An updated `step_winsorize` object with estimated bounds stored in `winsor_limits` and `trained = TRUE`.
#' @export
prep.step_winsorize <- function(x, training, info = NULL, ...) {

  # Get colnames
  col_names <- recipes::recipes_eval_select(x$terms, training, info)

  # Ensure selected columns are numeric
  recipes::check_type(training[, col_names], types = c("double", "integer"))

  # Extract probs to ensure it is properly referenced
  probs <- x$probs %>% sort()

  # Compute winsorization thresholds separately for each column
  winsor_limits <- purrr::map_dfr(
    col_names,
    ~ {
      lower <- stats::quantile(training[[.x]], probs = probs[1], na.rm = TRUE)
      upper <- stats::quantile(training[[.x]], probs = probs[2], na.rm = TRUE)

      warning_msg <- NULL  # Initialize warning message

      # Check for Inf values
      if (is.infinite(lower) || is.infinite(upper)) {
        if (is.infinite(lower)) {
          new_lower <- min(training[[.x]][is.finite(training[[.x]])], na.rm = TRUE)
          warning_msg <- paste0(
            "Winsorization for column '", .x, "' resulted in an infinite lower bound. ",
            "Using min of finite values instead (", new_lower, "). Consider adjusting probs = c(",
            probs[1], ", ", probs[2], ")."
          )
          lower <- new_lower
        }

        if (is.infinite(upper)) {
          new_upper <- max(training[[.x]][is.finite(training[[.x]])], na.rm = TRUE)
          warning_msg <- paste0(
            warning_msg, if (!is.null(warning_msg)) "\n" else "",
            "Winsorization for column '", .x, "' resulted in an infinite upper bound. ",
            "Using max of finite values instead (", new_upper, "). Consider adjusting probs = c(",
            probs[1], ", ", probs[2], ")."
          )
          upper <- new_upper
        }

        warning(warning_msg)  # Issue warning if needed
      }

      tibble::tibble(
        column = .x,
        lower = lower,
        upper = upper
      )
    }
  )

  ## Use the constructor function to return the updated object.
  step_winsorize_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    probs = probs,
    winsor_limits = winsor_limits,
    skip = x$skip,
    id = x$id
  )


}

#' Apply winsorization during baking
#' @importFrom recipes bake
#' @export
bake.step_winsorize <- function(object, new_data, ...) {
  if (!object$trained) {
    rlang::abort("step_winsorize was not trained")
  }

  col_names <- object$winsor_limits$column
  recipes::check_new_data(col_names, object, new_data)

  winsor_limits <- object$winsor_limits

  for (col in col_names) {
    limits <- dplyr::filter(winsor_limits, column == col)
    if (nrow(limits) > 0) {
      #Ensure winsorized values respect finite bounds
      new_data[[col]] <- base::pmax(limits$lower, base::pmin(new_data[[col]], limits$upper))
    }
  }

  new_data
}


#' @title Print Method for step_winsorize
#' @description Print method for a `step_winsorize` object.
#' @param x A `step_winsorize` object.
#' @param width Width of the output. Default adjusts to console width.
#' @param ... Additional arguments (not used).
#' @return The original `step_winsorize` object, invisibly.
#' @export
print.step_winsorize <- function(x, width = max(20, options()$width - 30), ...) {
  selected_columns <- if (x$trained) {
    x$winsor_limits$column  # Extract actual column names after training
  } else {
    purrr::map_chr(x$terms, rlang::as_label)  # Convert quosures to character names before prep()
  }

  cat("Winsorization step on: ", paste(selected_columns, collapse = ", "), "\n", sep = "")
  invisible(x)
}


#' Tidy method for step_winsorize
#' @importFrom recipes tidy
#' @export
tidy.step_winsorize <- function(x, ...) {
  if (recipes::is_trained(x)) {
    res <- x$winsor_limits
  } else {
    res <- tibble::tibble(
      column = recipes::sel2char(x$terms),
      lower = rlang::na_dbl,
      upper = rlang::na_dbl
    )
  }
  res$id <- x$id
  return(res)
}

#' @title Required Packages for step_winsorize
#' @description Declares the required packages for using the `step_winsorize` step inside a recipe.
#'
#' @param x A `step_winsorize` object.
#' @param ... Additional arguments (not used).
#'
#' @return A character vector of package names required for the step.
#' @export

required_pkgs.step_winsorize <- function(x, ...) {
  c("dplyr", "tidyr", "recipes")
}




# step_impute_sector-----------------------------------
#' @title Prepare Step: step_impute_sector
#' @description Prepares the `step_impute_sector` by computing mean or median values by sector for imputation.
#' This function is called automatically during `prep()` on a recipe.
#'
#' @param x A `step_impute_sector` object.
#' @param training A data frame of training data that will be used to estimate the imputation values.
#' @param info An optional `term_info` object. Not used here but required by the S3 method signature.
#' @param ... Additional arguments passed to methods (not used).
#'
#' @return A `step_impute_sector` object with computed `impute_values`.
#' @export
step_impute_sector <- function(recipe, ..., sector, method = "mean",
                               role = NA, trained = FALSE,
                               impute_values = NULL, skip = FALSE,
                               id = recipes::rand_id("impute_sector")) {

  terms <- rlang::enquos(...)

  # Validate method argument
  if (!method %in% c("mean", "median")) {
    rlang::abort("`method` must be either 'mean' or 'median'.")
  }

  recipes::add_step(
    recipe,
    step_impute_sector_new(
      terms = terms,
      sector = sector,
      method = method,
      role = role,
      trained = trained,
      impute_values = impute_values,
      skip = skip,
      id = id
    )
  )
}

#' Constructor for step_impute_sector
step_impute_sector_new <- function(terms, sector, method, role, trained, impute_values, skip, id) {
  recipes::step(
    subclass = "impute_sector",
    terms = terms,
    sector = sector,
    method = method,
    role = role,
    trained = trained,
    impute_values = impute_values,
    skip = skip,
    id = id
  )
}

#' @title Prepare Method for step_impute_sector
#' @description Computes sector-wise means or medians for selected columns, storing them in the step object.
#'
#' @param x A `step_impute_sector` object.
#' @param training A data frame of training data used to compute imputation values.
#' @param info A `term_info` object passed by `prep()`, not used here.
#' @param ... Additional arguments passed to methods (not used).
#'
#' @importFrom recipes prep
#' @return An updated `step_impute_sector` object with `trained = TRUE` and imputation values stored.
#' @export
prep.step_impute_sector <- function(x, training, info = NULL, ...) {

  # Extract selected column names
  col_names <- recipes::recipes_eval_select(x$terms, training, info)

  # Ensure selected columns are numeric
  recipes::check_type(training[, col_names], types = c("double", "integer"))

  # Ensure sector column exists and is categorical
  if (!x$sector %in% colnames(training)) {
    rlang::abort(paste("Sector column", x$sector, "is not in the training data."))
  }
  if (!is.character(training[[x$sector]]) && !is.factor(training[[x$sector]])) {
    rlang::abort(paste("Sector column", x$sector, "must be a character or factor variable."))
  }

  # Store method separately to avoid referencing x inside across()
  method <- x$method

  # Compute imputation values (mean or median within each sector)
  impute_values <- training %>%
    dplyr::group_by(.data[[x$sector]]) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(col_names),
        ~ {
          if (identical(method, "mean")) {
            mean(.x, na.rm = TRUE)
          } else {
            median(.x, na.rm = TRUE)
          }
        },
        .names = "{.col}"
      ),
      .groups = "drop"
    )

  # Return updated step
  step_impute_sector_new(
    terms = x$terms,
    sector = x$sector,
    method = x$method,
    role = x$role,
    trained = TRUE,
    impute_values = impute_values,
    skip = x$skip,
    id = x$id
  )
}

#' Apply imputation during baking
#' @importFrom recipes bake
#' @export
bake.step_impute_sector <- function(object, new_data, ...) {
  if (!object$trained) {
    rlang::abort("step_impute_sector was not trained")
  }

  col_names <- colnames(object$impute_values)[-1]  # Extract imputed columns
  sector_column <- object$sector

  # Ensure new data has the required columns
  recipes::check_new_data(c(sector_column, col_names), object, new_data)

  # Perform sector-based imputation
  new_data <- new_data %>%
    dplyr::left_join(object$impute_values, by = sector_column, suffix = c("", "_impute")) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(col_names),
        ~ ifelse(is.na(.), get(paste0(dplyr::cur_column(), "_impute")), .)
      )
    ) %>%
    dplyr::select(-dplyr::ends_with("_impute"))

  return(new_data)
}

#' Tidy method for step_impute_sector
#' @importFrom recipes tidy
#' @export
tidy.step_impute_sector <- function(x, ...) {
  if (recipes::is_trained(x)) {
    res <- x$impute_values %>%
      tidyr::pivot_longer(cols = -1, names_to = "column", values_to = "imputed_value") %>%
      dplyr::mutate(method = x$method, id = x$id)
  } else {
    res <- tibble::tibble(
      column = recipes::sel2char(x$terms),
      sector = x$sector,
      method = x$method,
      imputed_value = NA_real_,
      id = x$id
    )
  }
  return(res)
}

#' @title Print Method for step_impute_sector
#' @description
#' Prints a concise summary of the `step_impute_sector` step, including selected columns, the sector column, and the imputation method.
#'
#' @param x A `step_impute_sector` object.
#' @param width An integer specifying the desired display width. Defaults to a dynamic value based on console width.
#' @param ... Additional arguments (not used).
#'
#' @return The original `step_impute_sector` object, invisibly.
#' @export
print.step_impute_sector <- function(x, width = max(20, options()$width - 30), ...) {
  selected_columns <- if (x$trained) {
    colnames(x$impute_values)[-1]  # Extract imputed columns after training
  } else {
    purrr::map_chr(x$terms, rlang::as_label)  # Extract column names before training
  }

  cat("Sector-based imputation step on: ", paste(selected_columns, collapse = ", "), "\n",
      "Sector column: ", x$sector, "\n",
      "Imputation method: ", x$method, "\n",
      if (x$trained) paste("Trained with", nrow(x$impute_values), "sector groups.\n") else "",
      sep = "")

  invisible(x)
}

#' @title Required Packages for step_impute_sector
#' @description Declares the required packages for using the `step_impute_sector` step inside a recipe.
#'
#' @param x A `step_impute_sector` object.
#' @param ... Additional arguments (not used).
#'
#' @return A character vector of package names required for the step.
#' @export
required_pkgs.step_impute_sector <- function(x, ...) {
  c("dplyr", "tidyr", "recipes")
}









