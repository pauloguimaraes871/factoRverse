#Construction
test_that("create_sub_port_config builds a valid object for every allowed method", {

  allowed_methods <- c("ew", "sw", "cw", "cs", "rp", "hrp", "mvo")

  for (method in allowed_methods){

    config <- create_sub_port_config(port_construction_method = method)

    expect_s4_class(config, "sub_port_config")
    expect_identical(config@port_construction_method, method)
    expect_true(methods::validObject(config))
  }
})

test_that("create_sub_port_config fills defaults only for the chosen method", {

  #Methods without parameter objects keep every parameter slot NULL
  ew_config <- create_sub_port_config(port_construction_method = "ew")
  expect_null(ew_config@mvo_parameters)
  expect_null(ew_config@rp_parameters)
  expect_null(ew_config@hrp_parameters)

  #Each covariance-based method gets its own default and nothing else
  rp_config <- create_sub_port_config(port_construction_method = "rp")
  expect_s4_class(rp_config@rp_parameters, "rp_parameters")
  expect_null(rp_config@mvo_parameters)
  expect_null(rp_config@hrp_parameters)

  hrp_config <- create_sub_port_config(port_construction_method = "hrp")
  expect_s4_class(hrp_config@hrp_parameters, "hrp_parameters")
  expect_null(hrp_config@rp_parameters)
  expect_null(hrp_config@mvo_parameters)

  mvo_config <- create_sub_port_config(port_construction_method = "mvo")
  expect_s4_class(mvo_config@mvo_parameters, "mvo_parameters")
  expect_null(mvo_config@rp_parameters)
  expect_null(mvo_config@hrp_parameters)
})

test_that("create_sub_port_config honours a supplied parameter object", {

  rp_params <- create_rp_parameters(rp_method = "newton",
                                    exp_ret_score_tilt = "inner",
                                    exp_ret_score_tilt_eta = 0.5)

  config <- create_sub_port_config(port_construction_method = "rp",
                                   rp_parameters = rp_params)

  expect_identical(config@rp_parameters@rp_method, "newton")
  expect_identical(config@rp_parameters@exp_ret_score_tilt, "inner")
  expect_equal(config@rp_parameters@exp_ret_score_tilt_eta, 0.5)
})

test_that("create_sub_port_config keeps mismatched parameter objects inert but valid", {

  #Parameters that do not match the method are ignored, not rejected
  config <- create_sub_port_config(port_construction_method = "ew",
                                   rp_parameters = create_rp_parameters())

  expect_true(methods::validObject(config))
  expect_identical(config@port_construction_method, "ew")
  expect_s4_class(config@rp_parameters, "rp_parameters")
})

#Subclassing
test_that("create_sub_port_config can build a subclass and rejects unrelated classes", {

  mmaf_config <- create_sub_port_config(port_construction_method = "rp",
                                        class = "mmaf_sub_port_config")

  expect_s4_class(mmaf_config, "mmaf_sub_port_config")
  expect_s4_class(mmaf_config, "sub_port_config")

  expect_error(
    create_sub_port_config(port_construction_method = "ew", class = "rp_parameters"),
    "extending it"
  )
  expect_error(
    create_sub_port_config(port_construction_method = "ew", class = c("a", "b")),
    "single non-NA character"
  )
})

test_that("mmaf_sub_port_config inherits the sub_port_config contract", {

  #The MMAF configuration must remain constructible exactly as before
  legacy_config <- methods::new("mmaf_sub_port_config",
                                port_construction_method = "mvo",
                                mvo_parameters = NULL,
                                rp_parameters = NULL,
                                hrp_parameters = NULL)

  expect_s4_class(legacy_config, "mmaf_sub_port_config")
  expect_true(methods::extends("mmaf_sub_port_config", "sub_port_config"))
  expect_identical(methods::slotNames("mmaf_sub_port_config"),
                   methods::slotNames("sub_port_config"))

  #And it must inherit the shared validity rules
  expect_error(
    methods::new("mmaf_sub_port_config", port_construction_method = "not_a_method"),
    "port_construction_method must be one of"
  )
})

#Validation
test_that("validate_sub_port_config rejects malformed methods", {

  expect_error(
    create_sub_port_config(port_construction_method = "custom_weights"),
    "port_construction_method must be one of"
  )
  #Layered methods may not be nested inside a sub-portfolio
  expect_error(
    create_sub_port_config(port_construction_method = "mmaf"),
    "port_construction_method must be one of"
  )
  expect_error(
    methods::new("sub_port_config", port_construction_method = character(0)),
    "single non-NA character"
  )
  expect_error(
    methods::new("sub_port_config", port_construction_method = NA_character_),
    "single non-NA character"
  )
  expect_error(
    methods::new("sub_port_config", port_construction_method = c("ew", "rp")),
    "single non-NA character"
  )
})

test_that("validate_sub_port_config rejects a wrongly classed parameter object", {

  expect_error(
    methods::new("sub_port_config",
                 port_construction_method = "rp",
                 rp_parameters = create_hrp_parameters()),
    "rp_parameters must be of class 'rp_parameters'"
  )
  expect_error(
    methods::new("sub_port_config",
                 port_construction_method = "mvo",
                 mvo_parameters = list(opt_method = "random")),
    "mvo_parameters must be of class 'mvo_parameters'"
  )
  expect_error(
    methods::new("sub_port_config",
                 port_construction_method = "hrp",
                 hrp_parameters = create_rp_parameters()),
    "hrp_parameters must be of class 'hrp_parameters'"
  )
})
