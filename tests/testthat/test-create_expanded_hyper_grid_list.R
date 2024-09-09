#Define test
test_that("create_expanded_hyper_grid_list generates adequate hyperparameters list for grid_search", {

  #RF
  expect_equal(
    create_expanded_hyper_grid_list(
    hyper_grid_domain_list = list(mtry = c(0, 1), num.trees = c(200, 500),
                                  max.depth = c(2), min.bucket = c(1, 10,15)) ,
    n_iter = NULL,
    tuning_method = "grid_search",
    ml_algorithm = "rf"

  ),  list(mtry = expand.grid(list(mtry = c(0, 1), num.trees = c(200, 500),
                  max.depth = c(2), min.bucket = c(1, 10, 15)))$mtry,

           num.trees = expand.grid(list(mtry = c(0, 1), num.trees = c(200, 500),
                                   max.depth = c(2), min.bucket = c(1, 10, 15)))$num.trees,

           max.depth = expand.grid(list(mtry = c(0, 1), num.trees = c(200, 500),
                                        max.depth = c(2), min.bucket = c(1, 10, 15)))$max.depth,

           min.bucket = expand.grid(list(mtry = c(0, 1), num.trees = c(200, 500),
                                        max.depth = c(2), min.bucket = c(1, 10, 15)))$min.bucket
  )

  )


  #XGBOOST
  expect_equal(
    create_expanded_hyper_grid_list(
      hyper_grid_domain_list = list(min_child_weight = c(1), max_depth = c(200, 500),
                                    subsample = c(0.2, 0.6), colsample_bytree = c(0.1, 0.9), eta = c(0.25),
                                    alpha = c(1,2,3), gamma = c(0.25), nrounds = c(100,200,300)),
      n_iter = NULL,
      tuning_method = "grid_search",
      ml_algorithm = "xgb"
    ),  list(min_child_weight = expand.grid(list(min_child_weight = c(1), max_depth = c(200, 500),
                                                 subsample = c(0.2, 0.6), colsample_bytree = c(0.1, 0.9), eta = c(0.25),
                                                 alpha = c(1,2,3), gamma = c(0.25), nrounds = c(100,200,300)))$min_child_weight,

             max_depth =  expand.grid(list(min_child_weight = c(1), max_depth = c(200, 500),
                                           subsample = c(0.2, 0.6), colsample_bytree = c(0.1, 0.9), eta = c(0.25),
                                           alpha = c(1,2,3), gamma = c(0.25), nrounds = c(100,200,300)))$max_depth,

             subsample = expand.grid(list(min_child_weight = c(1), max_depth = c(200, 500),
                                          subsample = c(0.2, 0.6), colsample_bytree = c(0.1, 0.9), eta = c(0.25),
                                          alpha = c(1,2,3), gamma = c(0.25), nrounds = c(100,200,300)))$subsample,

             colsample_bytree =  expand.grid(list(min_child_weight = c(1), max_depth = c(200, 500),
                                                  subsample = c(0.2, 0.6), colsample_bytree = c(0.1, 0.9), eta = c(0.25),
                                                  alpha = c(1,2,3), gamma = c(0.25), nrounds = c(100,200,300)))$colsample_bytree,

             eta = expand.grid(list(min_child_weight = c(1), max_depth = c(200, 500),
                                    subsample = c(0.2, 0.6), colsample_bytree = c(0.1, 0.9), eta = c(0.25),
                                    alpha = c(1,2,3), gamma = c(0.25), nrounds = c(100,200,300)))$eta,

             alpha = expand.grid(list(min_child_weight = c(1), max_depth = c(200, 500),
                                      subsample = c(0.2, 0.6), colsample_bytree = c(0.1, 0.9), eta = c(0.25),
                                      alpha = c(1,2,3), gamma = c(0.25), nrounds = c(100,200,300)))$alpha,

             gamma =  expand.grid(list(min_child_weight = c(1), max_depth = c(200, 500),
                                       subsample = c(0.2, 0.6), colsample_bytree = c(0.1, 0.9), eta = c(0.25),
                                       alpha = c(1,2,3), gamma = c(0.25), nrounds = c(100,200,300)))$gamma,

             nrounds = expand.grid(list(min_child_weight = c(1), max_depth = c(200, 500),
                                        subsample = c(0.2, 0.6), colsample_bytree = c(0.1, 0.9), eta = c(0.25),
                                        alpha = c(1,2,3), gamma = c(0.25), nrounds = c(100,200,300)))$nrounds


    )

  )

  #NN
  expect_equal(
    create_expanded_hyper_grid_list(
      hyper_grid_domain_list = list(regularizer_l1 = c(0, 1), regularizer_l2 = c(200, 500),
                                    droprate = c(0.5), lr = c(1, 10,15), size_of_batch = 512, number_of_epochs = 10),
      n_iter = NULL,
      tuning_method = "grid_search",
      ml_algorithm = "nn"

    ),  list(regularizer_l1 = expand.grid(list(regularizer_l1 = c(0, 1), regularizer_l2 = c(200, 500),
                                     droprate = c(0.5), lr = c(1, 10,15), size_of_batch = 512, number_of_epochs = 10))$regularizer_l1,

             regularizer_l2 = expand.grid(list(regularizer_l1 = c(0, 1), regularizer_l2 = c(200, 500),
                                          droprate = c(0.5), lr = c(1, 10,15), size_of_batch = 512, number_of_epochs = 10))$regularizer_l2,

             droprate = expand.grid(list(regularizer_l1 = c(0, 1), regularizer_l2 = c(200, 500),
                                          droprate = c(0.5), lr = c(1, 10,15), size_of_batch = 512, number_of_epochs = 10))$droprate,

             lr = expand.grid(list(regularizer_l1 = c(0, 1), regularizer_l2 = c(200, 500),
                                           droprate = c(0.5), lr = c(1, 10,15), size_of_batch = 512, number_of_epochs = 10))$lr,


             size_of_batch = expand.grid(list(regularizer_l1 = c(0, 1), regularizer_l2 = c(200, 500),
                                   droprate = c(0.5), lr = c(1, 10,15), size_of_batch = 512, number_of_epochs = 10))$size_of_batch,


             number_of_epochs = expand.grid(list(regularizer_l1 = c(0, 1), regularizer_l2 = c(200, 500),
                                              droprate = c(0.5), lr = c(1, 10,15), size_of_batch = 512, number_of_epochs = 10))$number_of_epochs



    )

  )



})

#Define test
test_that("create_expanded_hyper_grid_list exhausts hyperparameters for grid_search", {

  #RF
  expect_true(
    all(
      unique(unlist(create_expanded_hyper_grid_list(
      hyper_grid_domain_list = list(mtry = c(0, 1), num.trees = c(200, 500),
                                    max.depth = c(6), min.bucket = c(1, 10)),
      n_iter = NULL,
      tuning_method = "grid_search",
      ml_algorithm = "rf"
    ))) %in% unlist(list(mtry = c(0, 1), num.trees = c(200, 500),
                        max.depth = c(6), min.bucket = c(1, 10)))
    ))



  #XGBOOST
  expect_true(
    all(
      unique(unlist(create_expanded_hyper_grid_list(
        hyper_grid_domain_list = list(min_child_weight = c(1), max_depth = c(200, 500),
                                      subsample = c(0.2, 0.6), colsample_bytree = c(0.1, 0.2), eta = c(0.25),
                                      alpha = c(1,2,3), gamma = c(0.25), nrounds = c(100,200,300)),
        n_iter = NULL,
        tuning_method = "grid_search",
        ml_algorithm = "xgb"
      ))) %in% unlist(list(min_child_weight = c(1), max_depth = c(200, 500),
                           subsample = c(0.2, 0.6), colsample_bytree = c(0.1, 0.2), eta = c(0.25),
                           alpha = c(1,2,3), gamma = c(0.25), nrounds = c(100,200,300)))
    ))


})

#Define test
test_that("create_expanded_hyper_grid_list generates adequate hyperparameters list for random_search", {

  #RF
  set.seed(123)
  mtry <- runif(2, 0.1, 1)
  num.trees <- rlnorm(2, 6, 1)
  max.depth <- runif(2, 2, 8)
  min.bucket <- runif(2, 1, 10)

  #RF
  set.seed(123)
  expect_equal(
    create_expanded_hyper_grid_list(
      hyper_grid_domain_list = list(mtry = list(distribution_choice = "uniform", pars = c(min = 0.1, max = 1)),
                                    num.trees = list(distribution_choice = "lognormal", pars = c(meanlog = 6L, sdlog = 1L)),
                                    max.depth = list(distribution_choice = "uniform", pars = c(min = 2L, max = 8L)),
                                    min.bucket = list(distribution_choice = "uniform", pars = c(min = 1, max = 10))),
      n_iter = 2,
      tuning_method = "random_search",
      ml_algorithm = "rf"
    ),  list(mtry = expand.grid(list(mtry = mtry, num.trees = num.trees,
                                     max.depth = max.depth, min.bucket = min.bucket))$mtry,

             num.trees =  round(expand.grid(list(mtry = mtry, num.trees = num.trees,
                                           max.depth = max.depth, min.bucket = min.bucket))$num.trees),

             max.depth =  round(expand.grid(list(mtry = mtry, num.trees = num.trees,
                                           max.depth = max.depth, min.bucket = min.bucket))$max.depth),

             min.bucket =  expand.grid(list(mtry = mtry, num.trees = num.trees,
                                            max.depth = max.depth, min.bucket = min.bucket))$min.bucket
    )

  )


  #XGBOOST
  set.seed(123)
  min_child_weight <- runif(3, 1, 2)
  max_depth <- c(6)
  subsample <- runif(3, 0.2, 0.8)
  colsample_bytree <- runif(3, 0.1, 1)
  eta <- runif(3, 0.01, 0.05)
  alpha <- runif(3, 1, 3)
  gamma <- runif(3, 1, 10)
  nrounds <- round(rlnorm(3, 1, 5),0)

  #XGBOOST
  set.seed(123)
  expect_equal(
    create_expanded_hyper_grid_list(
      hyper_grid_domain_list = list(min_child_weight = list(distribution_choice = "uniform", pars = c(min = 1, max = 2)),
                                    max_depth = list(distribution_choice = "constant", value = c(6)),
                                    subsample = list(distribution_choice = "uniform", pars = c(min = 0.2, max = 0.8)),
                                    colsample_bytree = list(distribution_choice = "uniform", pars = c(min = 0.1, max = 1)),
                                    eta = list(distribution_choice = "uniform", pars = c(min = 0.01, max = 0.05)),
                                    alpha = list(distribution_choice = "uniform", pars = c(min = 1, max = 3)),
                                    gamma = list(distribution_choice = "uniform", pars = c(min = 1, max = 10)),
                                    nrounds = list(distribution_choice = "lognormal", pars = c(meanlog = 1L, sdlog = 5L))
                                    ),
      n_iter = 3,
      tuning_method = "random_search",
      ml_algorithm = "xgb"
    ),  list(min_child_weight = expand.grid(list(min_child_weight = min_child_weight, max_depth = max_depth,
                                                 subsample = subsample, colsample_bytree = colsample_bytree, eta = eta,
                                                 alpha = alpha, gamma = gamma, nrounds = nrounds))$min_child_weight,

             max_depth = expand.grid(list(min_child_weight = min_child_weight, max_depth = max_depth,
                                          subsample = subsample, colsample_bytree = colsample_bytree, eta = eta,
                                          alpha = alpha, gamma = gamma, nrounds = nrounds))$max_depth,

             subsample = expand.grid(list(min_child_weight = min_child_weight, max_depth = max_depth,
                                          subsample = subsample, colsample_bytree = colsample_bytree, eta = eta,
                                          alpha = alpha, gamma = gamma, nrounds = nrounds))$subsample,

             colsample_bytree = expand.grid(list(min_child_weight = min_child_weight, max_depth = max_depth,
                                                 subsample = subsample, colsample_bytree = colsample_bytree, eta = eta,
                                                 alpha = alpha, gamma = gamma, nrounds = nrounds))$colsample_bytree,

             eta = expand.grid(list(min_child_weight = min_child_weight, max_depth = max_depth,
                                    subsample = subsample, colsample_bytree = colsample_bytree, eta = eta,
                                    alpha = alpha, gamma = gamma, nrounds = nrounds))$eta,

             alpha = expand.grid(list(min_child_weight = min_child_weight, max_depth = max_depth,
                                      subsample = subsample, colsample_bytree = colsample_bytree, eta = eta,
                                      alpha = alpha, gamma = gamma, nrounds = nrounds))$alpha,

             gamma = expand.grid(list(min_child_weight = min_child_weight, max_depth = max_depth,
                                      subsample = subsample, colsample_bytree = colsample_bytree, eta = eta,
                                      alpha = alpha, gamma = gamma, nrounds = nrounds))$gamma,

             nrounds = expand.grid(list(min_child_weight = min_child_weight, max_depth = max_depth,
                                         subsample = subsample, colsample_bytree = colsample_bytree, eta = eta,
                                         alpha = alpha, gamma = gamma, nrounds = nrounds))$nrounds


    )

  )


  #NN
  set.seed(123)
  regularizer_l1 <- runif(3, 1, 2)
  regularizer_l2 <- c(6)
  droprate <- runif(3, 0.2, 0.8)
  lr = runif(3, 0.1, 1)
  size_of_batch <- c(256, 512)
  number_of_epochs <- runif(3, 100, 300)
  batch_norm_option <- c(TRUE,FALSE)




  set.seed(123)
  expect_equal(
    create_expanded_hyper_grid_list(
      hyper_grid_domain_list = list(regularizer_l1 = list(distribution_choice = "uniform", pars = c(min = 1, max = 2)),
                                    regularizer_l2 = list(distribution_choice = "constant", value = c(6)),
                                    droprate = list(distribution_choice = "uniform", pars = c(min = 0.2, max = 0.8)),
                                    lr = list(distribution_choice = "uniform", pars = c(min = 0.1, max = 1)),
                                    size_of_batch = list(distribution_choice = "constant", value = c(256, 512)),
                                    number_of_epochs = list(distribution_choice = "uniform", pars = c(min = 100, max = 300))),
      n_iter = 3,
      tuning_method = "random_search",
      ml_algorithm = "NN"
    ),  list(regularizer_l1 = expand.grid(list(regularizer_l1 = regularizer_l1, regularizer_l2 = regularizer_l2,
                                               droprate = droprate, lr = lr, size_of_batch = size_of_batch,
                                               number_of_epochs = number_of_epochs))$regularizer_l1,

             regularizer_l2 = expand.grid(list(regularizer_l1 = regularizer_l1, regularizer_l2 = regularizer_l2,
                                               droprate = droprate, lr = lr, size_of_batch = size_of_batch,
                                               number_of_epochs = number_of_epochs))$regularizer_l2,

             droprate = expand.grid(list(regularizer_l1 = regularizer_l1, regularizer_l2 = regularizer_l2,
                                               droprate = droprate, lr = lr, size_of_batch = size_of_batch,
                                               number_of_epochs = number_of_epochs))$droprate,

             lr = expand.grid(list(regularizer_l1 = regularizer_l1, regularizer_l2 = regularizer_l2,
                                         droprate = droprate, lr = lr, size_of_batch = size_of_batch,
                                         number_of_epochs = number_of_epochs))$lr,

             size_of_batch = expand.grid(list(regularizer_l1 = regularizer_l1, regularizer_l2 = regularizer_l2,
                                   droprate = droprate, lr = lr, size_of_batch = size_of_batch,
                                   number_of_epochs = number_of_epochs))$size_of_batch,

             number_of_epochs = expand.grid(list(regularizer_l1 = regularizer_l1, regularizer_l2 = regularizer_l2,
                                              droprate = droprate, lr = lr, size_of_batch = size_of_batch,
                                              number_of_epochs = number_of_epochs))$number_of_epochs

    )

  )




})

#Define test
test_that("create_expanded_hyper_grid_list does not generate repeated hyperparameters", {

  #Check that there are no repeated hyperparameters (length is equal)
  expanded_hyper_grid_list <-  create_expanded_hyper_grid_list(
    hyper_grid_domain_list = list(regularizer_l1 = list(distribution_choice = "uniform", pars = c(min = 0.1, max = 1)),
                                  regularizer_l2 = list(distribution_choice = "lognormal", pars = c(meanlog = 6L, sdlog = 1L)),
                                  droprate = list(distribution_choice = "constant", value = c(0.5, 0.5, 0.9)), #Repeated droprate
                                  lr = list(distribution_choice = "uniform", pars = c(min = 1, max = 10)),
                                  size_of_batch = list(distribution_choice = "constant", value = c(256L)),
                                  number_of_epochs = list(distribution_choice = "uniform", pars = c(min = 100L, max = 200L))),

    n_iter = 2,
    tuning_method = "random_search",
    ml_algorithm = "nn"
  )

  expect_equal(
    length(which(expanded_hyper_grid_list$droprate == 0.5)),
    length(which(expanded_hyper_grid_list$droprate == 0.9))
  )


  #Check that there are no repeated hyperparameters (length is equal)
  expanded_hyper_grid_list <-  create_expanded_hyper_grid_list(
    hyper_grid_domain_list = list(regularizer_l1 = c(1,2,3),
                                  regularizer_l2 = c(2,5,6),
                                  droprate = c(0.5, 0.5, 0.9), #Repeated droprate
                                  lr = c(0.05),
                                  size_of_batch = 256,
                                  number_of_epochs = 100
                                  ),
    tuning_method = "grid_search",
    ml_algorithm = "nn"
  )

  expect_equal(
    length(which(expanded_hyper_grid_list$droprate == 0.5)),
    length(which(expanded_hyper_grid_list$droprate == 0.9))
  )


})

#Define test
test_that("create_expanded_hyper_grid_list throws errors when hyperparameters are not correctly set", {

  suppressWarnings(
   expect_error(
      create_expanded_hyper_grid_list(
        hyper_grid_domain_list = list(alpha = c(0, 1.01),
                                      lambda.min.ratio = c(6,5)),
        n_iter = 2,
        tuning_method = "grid_search",
        ml_algorithm = "glmnet"
      )
    )
  )


  suppressWarnings(
    expect_error(
      create_expanded_hyper_grid_list(
        hyper_grid_domain_list = list(alpha = c(0, 1.0),
                                      lambda.min.ratio = c(6)),
        n_iter = 2,
        tuning_method = "grid_search",
        ml_algorithm = "glmnet"
      )
    )
  )


  suppressWarnings(
    expect_error(
      create_expanded_hyper_grid_list(
        hyper_grid_domain_list = list(num.trees = c(100, 203.5)
                                      ),
        n_iter = 2,
        tuning_method = "grid_search",
        ml_algorithm = "rf"
      ), "num.trees should have no decimals"
    )
  )


  suppressWarnings(
    expect_error(
      create_expanded_hyper_grid_list(
        hyper_grid_domain_list = list(mtry = list(distribution_choice = "uniform",
                                                  pars = c(min = 100, max = 1000))),
        n_iter = 2,
        tuning_method = "random_search",
        ml_algorithm = "rf"
      ),
    )
  )

  suppressWarnings(
    expect_error(
      create_expanded_hyper_grid_list(
        hyper_grid_domain_list = list(max.depth = list(distribution_choice = "uniform",
                                                  pars = c(min = 100, max = 1000))),
        n_iter = 2,
        tuning_method = "random_search",
        ml_algorithm = "rf"
      ), "max.depth should be positive with no decimals"
    )
  )


  suppressWarnings(
    expect_no_error(
      create_expanded_hyper_grid_list(
        hyper_grid_domain_list = list(max.depth = list(distribution_choice = "uniform",
                                                       pars = c(min = 100L, max = 1000L))),
        n_iter = 2,
        tuning_method = "random_search",
        ml_algorithm = "rf"
      )
    )
  )

  suppressWarnings(
    expect_error(
      create_expanded_hyper_grid_list(
        hyper_grid_domain_list = list(max_depth = list(distribution_choice = "uniform",
                                                       pars = c(min = -10L, max = -1L))),
        n_iter = 2,
        tuning_method = "random_search",
        ml_algorithm = "xgb"
      ), "max_depth should be positive with no decimals"
    )
  )

  suppressWarnings(
    expect_error(
      create_expanded_hyper_grid_list(
        hyper_grid_domain_list = list(colsample_bytree = list(distribution_choice = "uniform",
                                                       pars = c(min = 100, max = 1000))),
        n_iter = 2,
        tuning_method = "random_search",
        ml_algorithm = "xgb"
      )
    )
  )


  suppressWarnings(
    expect_error(
      create_expanded_hyper_grid_list(
        hyper_grid_domain_list = list(subsample = list(distribution_choice = "uniform",
                                                              pars = c(min = 100, max = 1000))),
        n_iter = 2,
        tuning_method = "random_search",
        ml_algorithm = "xgb"
      )
    )
  )


  suppressWarnings(
    expect_error(
      create_expanded_hyper_grid_list(
        hyper_grid_domain_list = list(droprate = list(distribution_choice = "uniform",
                                                              pars = c(min = 100, max = 1000))),
        n_iter = 2,
        tuning_method = "random_search",
        ml_algorithm = "nn"
      )
    )
  )

  suppressWarnings(
    expect_error(
      create_expanded_hyper_grid_list(
        hyper_grid_domain_list = list(number_of_epochs = list(distribution_choice = "uniform",
                                                  pars = c(min = 100, max = 1000))),
        n_iter = 2,
        tuning_method = "random_search",
        ml_algorithm = "nn"
      )
    )
  )

  suppressWarnings(
    expect_error(
      create_expanded_hyper_grid_list(
        hyper_grid_domain_list = list(size_of_batch = list(distribution_choice = "uniform",
                                                              pars = c(min = 100, max = 1000))),
        n_iter = 2,
        tuning_method = "random_search",
        ml_algorithm = "nn"
      )
    )
  )

  suppressWarnings(
    expect_error(
      create_expanded_hyper_grid_list(
        hyper_grid_domain_list = list(size_of_batch = list(distribution_choice = "uniform",
                                                           pars = c(min = 100, max = 1000))),
        n_iter = 2,
        tuning_method = "random_search",
        ml_algorithm = "nn"
      )
    )
  )

  suppressWarnings(
    expect_warning(
      create_expanded_hyper_grid_list(
        hyper_grid_domain_list = list(size_of_batch = list(distribution_choice = "uniform",
                                                           pars = c(min = 100L, max = 500L))),
        n_iter = 2,
        tuning_method = "random_search",
        ml_algorithm = "nn"
      )
  )
  )





})
