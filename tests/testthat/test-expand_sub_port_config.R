#Expansion per method
test_that("expand_sub_port_config expands only the parameters of the configured method", {

  #Methods that carry no parameters expand to the method alone
  for (method in c("ew", "sw", "cw", "cs")){
    expanded <- expand_sub_port_config(create_sub_port_config(method))
    expect_equal(expanded, list(port_construction_method = method))
  }
})

test_that("expand_sub_port_config expands risk parity parameters", {

  config <- create_sub_port_config(
    port_construction_method = "rp",
    rp_parameters = create_rp_parameters(rp_method = "newton",
                                         exp_ret_score_tilt = "inner",
                                         exp_ret_score_tilt_eta = 0.7)
  )

  expanded <- expand_sub_port_config(config)

  expect_equal(expanded$port_construction_method, "rp")
  expect_equal(expanded$rp_method, "newton")
  expect_equal(expanded$exp_ret_score_tilt, "inner")
  expect_equal(expanded$exp_ret_score_tilt_eta, 0.7)

  #No parameters of other methods leak in
  expect_null(expanded$linkage)
  expect_null(expanded$opt_objective)
})

test_that("expand_sub_port_config expands hierarchical risk parity parameters", {

  config <- create_sub_port_config(
    port_construction_method = "hrp",
    hrp_parameters = create_hrp_parameters(linkage = "ward.D2",
                                           exp_ret_score_tilt = "final",
                                           exp_ret_score_tilt_eta = 1.5)
  )

  expanded <- expand_sub_port_config(config)

  expect_equal(expanded$port_construction_method, "hrp")
  expect_equal(expanded$linkage, "ward.D2")
  expect_equal(expanded$exp_ret_score_tilt, "final")
  expect_equal(expanded$exp_ret_score_tilt_eta, 1.5)
  expect_null(expanded$rp_method)
})

test_that("expand_sub_port_config expands mean-variance parameters", {

  config <- create_sub_port_config(
    port_construction_method = "mvo",
    mvo_parameters = create_mvo_parameters(opt_method = "random",
                                           random_ports_method = "simplex",
                                           n_random_ports = 250,
                                           opt_objective = "risk",
                                           ridge_pen = 0.3,
                                           n_resamples = 2,
                                           exp_ret_score_jitter = 0.1,
                                           cov_eigval_jitter = 0.2)
  )

  expanded <- expand_sub_port_config(config)

  expect_equal(expanded$port_construction_method, "mvo")
  expect_equal(expanded$random_ports_method, "simplex")
  expect_equal(expanded$n_random_ports, 250)
  expect_equal(expanded$opt_objective, "risk")
  expect_equal(expanded$ridge_pen, 0.3)
  expect_equal(expanded$n_resamples, 2)
  expect_equal(expanded$exp_ret_score_jitter, 0.1)
  expect_equal(expanded$cov_eigval_jitter, 0.2)
  expect_null(expanded$rp_method)
})

#Defaults and isolation
test_that("expand_sub_port_config fills defaults when the parameter object is absent", {

  #Bypass the constructor, which would have filled the defaults already
  bare_config <- methods::new("sub_port_config", port_construction_method = "rp")
  expanded <- expand_sub_port_config(bare_config)

  expect_equal(expanded$rp_method, create_rp_parameters()@rp_method)
  expect_equal(expanded$exp_ret_score_tilt, create_rp_parameters()@exp_ret_score_tilt)
})

test_that("expand_sub_port_config ignores parameters that do not match the method", {

  #A stray parameter object must not reach the inner call
  config <- methods::new("sub_port_config",
                         port_construction_method = "ew",
                         rp_parameters = create_rp_parameters(rp_method = "newton"))

  expect_equal(expand_sub_port_config(config),
               list(port_construction_method = "ew"))
})

test_that("expand_sub_port_config accepts a subclass configuration", {

  config <- create_sub_port_config("rp", class = "mmaf_sub_port_config")
  expect_equal(expand_sub_port_config(config)$port_construction_method, "rp")
})

#Contract with set_portfolio_weights
test_that("every expanded name is a formal argument of set_portfolio_weights", {

  formal_names <- names(formals(set_portfolio_weights))

  for (method in c("ew", "sw", "cw", "cs", "rp", "hrp", "mvo")){
    expanded <- expand_sub_port_config(create_sub_port_config(method))
    expect_true(all(names(expanded) %in% formal_names),
                info = paste("method:", method))
  }
})

#Validation
test_that("expand_sub_port_config rejects anything that is not a sub_port_config", {

  expect_error(expand_sub_port_config(list(port_construction_method = "ew")),
               "must be an object of class 'sub_port_config'")
  expect_error(expand_sub_port_config("rp"),
               "must be an object of class 'sub_port_config'")

  #An object modified after construction is revalidated rather than trusted
  tampered <- create_sub_port_config("rp")
  tampered@port_construction_method <- "not_a_method"
  expect_error(expand_sub_port_config(tampered),
               "port_construction_method must be one of")
})
