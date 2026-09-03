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

})
