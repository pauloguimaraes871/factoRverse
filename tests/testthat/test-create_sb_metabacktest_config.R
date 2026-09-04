test_that("allow_heterogeneous_base_features is refused unless nothing is passed through", {

  ## The pairing between allow_heterogeneous_base_features and features_passthrough is a
  ## property of the configuration, so it is enforced by the class validity function
  ## rather than at run time. That is the stronger place for it: a config that cannot be
  ## constructed cannot be stored on a pins board, reloaded a year later, and only then
  ## discovered to describe a design that never made sense.
  ##
  ## The relaxation is sound only when features_passthrough is "none", because only then
  ## does the meta learner ignore features_m_df entirely and build its design matrix from
  ## the base learners' predictions joined on id. With anything passed through, the meta
  ## learner must select columns out of one supplied features_m_df, and for a pool whose
  ## learners saw different feature sets "which learner's features?" has no answer.

  meta_learner_config <- create_sb_backtest_config(
    sb_algorithm = "ew", training_sample_size = 4, target_fwd_name = "fwd_premium_3m",
    rebalancing_months = 6, config_name = "meta")


  #Constructible with "none"
  ##################################
  expect_no_error(
    cfg_none <- create_sb_metabacktest_config(
      meta_sb_backtest_config = meta_learner_config,
      features_passthrough = "none", config_name = "het_ok",
      allow_heterogeneous_base_features = TRUE)
  )

  expect_true(cfg_none@allow_heterogeneous_base_features)


  #Refused with "all", and with a named subset
  ##################################
  ### Both non-"none" shapes are covered because features_passthrough is a character
  ### vector: the scalar sentinel and a multi-element selection take different branches
  ### of the length test, and a condition written without the length guard would warn
  ### or silently take the first element on the second one.
  expect_error(
    create_sb_metabacktest_config(
      meta_sb_backtest_config = meta_learner_config,
      features_passthrough = "all", config_name = "het_bad_all",
      allow_heterogeneous_base_features = TRUE),
    "requires features_passthrough = 'none'"
  )

  expect_error(
    create_sb_metabacktest_config(
      meta_sb_backtest_config = meta_learner_config,
      features_passthrough = c("roe_3m", "sharpe_6m"), config_name = "het_bad_subset",
      allow_heterogeneous_base_features = TRUE),
    "requires features_passthrough = 'none'"
  )


  #Off by default, and inert against every features_passthrough
  ##################################
  ### The default must not constrain ordinary configurations in any way, since it is the
  ### one every existing caller gets without asking.
  for (fp in list("none", "all", c("roe_3m", "sharpe_6m"))) {
    cfg <- create_sb_metabacktest_config(
      meta_sb_backtest_config = meta_learner_config,
      features_passthrough = fp, config_name = "default_off")
    expect_false(cfg@allow_heterogeneous_base_features)
  }


  #The flag itself must be a single non-missing logical
  ##################################
  ### NA is the case worth naming: it is a logical, so the slot's type check accepts it,
  ### and `if (NA)` is an error rather than a falsy value. Caught at construction instead
  ### of half way through a backtest.
  expect_error(
    create_sb_metabacktest_config(
      meta_sb_backtest_config = meta_learner_config,
      features_passthrough = "none", config_name = "het_na",
      allow_heterogeneous_base_features = NA),
    "single non-missing logical"
  )

  expect_error(
    create_sb_metabacktest_config(
      meta_sb_backtest_config = meta_learner_config,
      features_passthrough = "none", config_name = "het_vec",
      allow_heterogeneous_base_features = c(TRUE, TRUE)),
    "single non-missing logical"
  )

})


test_that("a config serialized before the slot existed still runs as homogeneous", {

  ## factoRverse 0.9.0 added allow_heterogeneous_base_features to sb_metabacktest_config.
  ## R does not backfill a prototype into an object that was written before the slot
  ## existed: both slot access and validObject() error on it. Every read of the slot in
  ## package code therefore goes through a defensive accessor, and FALSE is the right
  ## answer for such an object, because a pool built before the relaxation existed was
  ## necessarily homogeneous.
  ##
  ## The stale object is simulated by removing the slot from a freshly built config,
  ## which is what readRDS() of a pre-0.9.0 pin produces.

  meta_learner_config <- create_sb_backtest_config(
    sb_algorithm = "ew", training_sample_size = 4, target_fwd_name = "fwd_premium_3m",
    rebalancing_months = 6, config_name = "meta")

  cfg <- create_sb_metabacktest_config(
    meta_sb_backtest_config = meta_learner_config,
    features_passthrough = "none", config_name = "stale")

  stale_cfg <- cfg
  attributes(stale_cfg)$allow_heterogeneous_base_features <- NULL

  ### The premise: reading the slot directly is an error, not a default
  expect_error(stale_cfg@allow_heterogeneous_base_features)

  ### The accessor pattern used throughout the package resolves it to FALSE
  expect_false(
    isTRUE(tryCatch(stale_cfg@allow_heterogeneous_base_features,
                    error = function(e) FALSE))
  )

  ### ...and printing such a config must not error either
  expect_output(show(stale_cfg), "SB Metabacktest Configuration")


  #It must actually RUN, which is the promise the rest of this only implies
  ##################################
  ### Slot access, the accessor and show() are the three reads in package code, but
  ### checking them one by one is not the same as checking that a stored configuration
  ### still drives a backtest. This is the backward-compatibility promise itself, so it
  ### is exercised end to end rather than inferred: a full meta run on the stale config,
  ### then a monthly update of the result it produced.
  load(paste(test_path(),"/testdata/","toy_preprocessed_features_and_targets.RData", sep =""))

  build <- function(cutoff){
    list(
      features = create_meta_dataframe(
        toy_preprocessed_features %>%
          dplyr::select(dplyr::all_of(c("id", "tickers", "dates", "book_yield", "eps_yield"))) %>%
          dplyr::filter(dates <= as.Date(cutoff)),
        type = "features", meta_dataframe_name = "signals"),
      target = create_meta_dataframe(
        toy_preprocessed_targets %>% dplyr::filter(dates <= as.Date(cutoff)) %>%
          dplyr::mutate(
            fwd_return_1m  = dplyr::if_else(dates == as.Date(cutoff), NA_real_, fwd_return_1m),
            fwd_premium_1m = dplyr::if_else(dates == as.Date(cutoff), NA_real_, fwd_premium_1m)),
        meta_dataframe_name = "target", type = "target")
    )
  }

  base_cfg <- function(nm) create_sb_backtest_config(
    sb_algorithm = "ols", target_fwd_name = "fwd_premium_1m", training_sample_size = 6,
    rebalancing_months = 11, config_name = nm,
    chosen_signals_and_positions = c(book_yield = "long", eps_yield = "long"))

  in_1 <- build("2023-04-15")

  suppressWarnings(suppressMessages({
    a_1 <- run_sb_backtest(features_m_df = in_1$features, target_m_df = in_1$target,
                           config = base_cfg("ols_a"), parallel = FALSE, verbose = FALSE)
    b_1 <- run_sb_backtest(features_m_df = in_1$features, target_m_df = in_1$target,
                           config = base_cfg("ols_b"), parallel = FALSE, verbose = FALSE)
  }))

  run_cfg <- create_sb_metabacktest_config(
    meta_sb_backtest_config = create_sb_backtest_config(
      sb_algorithm = "ew", training_sample_size = 2, target_fwd_name = "fwd_premium_1m",
      rebalancing_months = 6, config_name = "meta"),
    features_passthrough = "none", config_name = "stale_run")

  stale_run_cfg <- run_cfg
  attributes(stale_run_cfg)$allow_heterogeneous_base_features <- NULL

  suppressWarnings(suppressMessages(
    res_stale <- run_sb_backtest(features_m_df = in_1$features, target_m_df = in_1$target,
                                 config = stale_run_cfg,
                                 base_sb_backtest_results_list = list(a_1, b_1),
                                 parallel = FALSE, verbose = FALSE)
  ))

  expect_s4_class(res_stale, "sb_metabacktest_results")

  ### It ran as homogeneous, which is the only correct reading of a config that predates
  ### the relaxation
  meta_wf <- res_stale@meta_sb_backtest_results@sb_backtest_workflow
  expect_false(meta_wf[[length(meta_wf)]]$heterogeneous_base_features)

  ### ...and the result it produced can still be rolled forward a month
  in_2 <- build("2023-05-15")

  suppressWarnings(suppressMessages({
    a_2 <- update_sb_backtest(features_m_df = in_2$features, target_m_df = in_2$target,
                              old_results = a_1, verbose = FALSE)
    b_2 <- update_sb_backtest(features_m_df = in_2$features, target_m_df = in_2$target,
                              old_results = b_1, verbose = FALSE)
    upd_stale <- update_sb_backtest(features_m_df = in_2$features, target_m_df = in_2$target,
                                    updated_base_sb_backtest_results = list(a_2, b_2),
                                    old_results = res_stale, parallel = FALSE, verbose = FALSE)
  }))

  expect_s4_class(upd_stale, "sb_metabacktest_results")

  upd_wf <- upd_stale@meta_sb_backtest_results@sb_backtest_workflow
  expect_false(upd_wf[[length(upd_wf)]]$heterogeneous_base_features)

})
