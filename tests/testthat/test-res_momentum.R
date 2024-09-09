# Define your test
test_that("res_momentum is running correctly small matrices", {
  expect_equal(
    res_momentum(
      ret_assets_main = matrix(c(5,6,7,8), nrow=2, ncol=2), #Ret Assets Main
      ret_assets_complementary = matrix(c(1,2,3,4), nrow=2, ncol=2), #Ret Assets Complementary
      ret_bench_main = matrix(c(2,1), nrow=1, ncol=2), #Ret Bench Main
      ret_bench_complementary = matrix(c(1,0), nrow=1, ncol=2), #Ret Bench Complementary
      beta_bench = matrix(c(1,1.2,1,1), nrow=2, ncol=2), #Beta
      alpha_bench = matrix(c(0.5,0,1,1), nrow=2, ncol=2) #Alpha
      ),
    matrix(c(sum(c(5,3,1) - 0.5 - 1.0*(c(2,0,1))),
             sum(c(6,4,2) - 0 - 1.2*(c(2,0,1))),
             sum(c(7,5,3) - 1 - 1.0*(c(1,2,0))),
             sum(c(8,6,4) - 1 - 1.0*(c(1,2,0)))),
           nrow=2, ncol=2)
  )
})

# Define your test
test_that("res_momentum is running correctly with data frames", {
  expect_equal(
    res_momentum(
      ret_assets_main = data.frame(matrix(c(5,6,7,8), nrow=2, ncol=2)), #Ret Assets Main
      ret_assets_complementary = data.frame(matrix(c(1,2,3,4), nrow=2, ncol=2)), #Ret Assets Complementary
      ret_bench_main = data.frame(matrix(c(2,1), nrow=1, ncol=2)), #Ret Bench Main
      ret_bench_complementary = data.frame(matrix(c(1,0), nrow=1, ncol=2)), #Ret Bench Complementary
      beta_bench = data.frame(matrix(c(1,1.2,1,1), nrow=2, ncol=2)), #Beta
      alpha_bench = data.frame(matrix(c(0.5,0,1,1), nrow=2, ncol=2)) #Alpha
    ),
    matrix(c(sum(c(5,3,1) - 0.5 - 1.0*(c(2,0,1))),
             sum(c(6,4,2) - 0 - 1.2*(c(2,0,1))),
             sum(c(7,5,3) - 1 - 1.0*(c(1,2,0))),
             sum(c(8,6,4) - 1 - 1.0*(c(1,2,0)))),
           nrow=2, ncol=2)
  )
})


# Define your test
test_that("res_momentum is running correctly with big matrices", {
  expect_equal(
    res_momentum(
      ret_assets_main = matrix(c(5,6,-4,7,8,4,-9,10,5), nrow=3, ncol=3), #Ret Assets Main
      ret_assets_complementary = matrix(c(1,2,-1,3,4,-2), nrow=3, ncol=2), #Ret Assets Complementary
      ret_bench_main = matrix(c(2,1,-4), nrow=1, ncol=3), #Ret Bench Main
      ret_bench_complementary = matrix(c(1,0), nrow=1, ncol=2), #Ret Bench Complementary
      beta_bench = matrix(c(1,1.2,-1,1,1,0.2,-0.4,2,0.3), nrow=3, ncol=3), #Beta
      alpha_bench = matrix(c(0.5,0,-0.4,1,1,0,-0.3,1,-1.5), nrow=3, ncol=3) #Alpha
    ),
    matrix(c(sum(c(5,3,1) - 0.5 - 1.0*(c(2,0,1))),
             sum(c(6,4,2) - 0 - 1.2*(c(2,0,1))),
             sum(c(-4,-2,-1) - -0.4 - -1.0*(c(2,0,1))),
             
             sum(c(7,5,3) - 1 - 1.0*(c(1,2,0))),
             sum(c(8,6,4) - 1 - 1.0*(c(1,2,0))),
             sum(c(4,-4,-2) - 0 - 0.2*(c(1,2,0))),
             
             sum(c(-9,7,5) - -0.3 - -0.4*(c(-4,1,2))),
             sum(c(10,8,6) - 1 - 2*(c(-4,1,2))),
             sum(c(5,4,-4) - -1.5 -0.3*(c(-4,1,2)))),
             
           nrow=3, ncol=3)
  )
})


# Define your test
test_that("res_momentum is running correctly with non-square matrices", {
  expect_equal(
    res_momentum(
      ret_assets_main = matrix(c(5,6,-4,7,8,4), nrow=3, ncol=2), #Ret Assets Main
      ret_assets_complementary = matrix(c(1,2,-1,3,4,-2), nrow=3, ncol=2), #Ret Assets Complementary
      ret_bench_main = matrix(c(2,1), nrow=1, ncol=2), #Ret Bench Main
      ret_bench_complementary = matrix(c(1,0), nrow=1, ncol=2), #Ret Bench Complementary
      beta_bench = matrix(c(1,1.2,-1,1,1,0.2), nrow=3, ncol=2), #Beta
      alpha_bench = matrix(c(0.5,0,-0.4,1,1,0), nrow=3, ncol=2) #Alpha
    ),
    matrix(c(sum(c(5,3,1) - 0.5 - 1.0*(c(2,0,1))),
             sum(c(6,4,2) - 0 - 1.2*(c(2,0,1))),
             sum(c(-4,-2,-1) - -0.4 - -1.0*(c(2,0,1))),
             
             sum(c(7,5,3) - 1 - 1.0*(c(1,2,0))),
             sum(c(8,6,4) - 1 - 1.0*(c(1,2,0))),
             sum(c(4,-4,-2) - 0 - 0.2*(c(1,2,0)))),
             

           
           nrow=3, ncol=2)
  )
})


# Define your test
test_that("res_momentum is running correctly with NAs", {
  expect_equal(
    res_momentum(
      ret_assets_main = matrix(c(5,6,-4,7,NA,4,-9,10,5), nrow=3, ncol=3), #Ret Assets Main
      ret_assets_complementary = matrix(c(1,2,-1,3,4,NA), nrow=3, ncol=2), #Ret Assets Complementary
      ret_bench_main = matrix(c(2,1,-4), nrow=1, ncol=3), #Ret Bench Main
      ret_bench_complementary = matrix(c(1,0), nrow=1, ncol=2), #Ret Bench Complementary
      beta_bench = matrix(c(1,1.2,-1,1,1,NA,-0.4,2,0.3), nrow=3, ncol=3), #Beta
      alpha_bench = matrix(c(0.5,0,-0.4,1,1,0,-0.3,1,NA), nrow=3, ncol=3) #Alpha
    ),
    matrix(c(sum(c(5,3,1) - 0.5 - 1.0*(c(2,0,1))),
             sum(c(6,4,2) - 0 - 1.2*(c(2,0,1))),
             sum(c(-4,NA,-1) - -0.4 - -1.0*(c(2,0,1)), na.rm = TRUE),
             
             sum(c(7,5,3) - 1 - 1.0*(c(1,2,0))),
             sum(c(NA,6,4) - 1 - 1.0*(c(1,2,0)), na.rm = TRUE),
             NA,
             
             sum(c(-9,7,5) - -0.3 - -0.4*(c(-4,1,2))),
             sum(c(10,NA,6) - 1 - 2*(c(-4,1,2)), na.rm = TRUE),
             NA),
           
           nrow=3, ncol=3)
  )
})

# Define your test
test_that("res_momentum is running correctly with NAs (a row full of NAs)", {
  expect_equal(
    res_momentum(
      ret_assets_main = matrix(c(5,6,NA,7,NA,4,-9,10,5), nrow=3, ncol=3), #Ret Assets Main
      ret_assets_complementary = matrix(c(1,2,NA,3,4,NA), nrow=3, ncol=2), #Ret Assets Complementary
      ret_bench_main = matrix(c(2,1,-4), nrow=1, ncol=3), #Ret Bench Main
      ret_bench_complementary = matrix(c(1,0), nrow=1, ncol=2), #Ret Bench Complementary
      beta_bench = matrix(c(1,1.2,-1,1,1,NA,-0.4,2,0.3), nrow=3, ncol=3), #Beta
      alpha_bench = matrix(c(0.5,0,-0.4,1,1,0,-0.3,1,NA), nrow=3, ncol=3) #Alpha
    ),
    matrix(c(sum(c(5,3,1) - 0.5 - 1.0*(c(2,0,1))),
             sum(c(6,4,2) - 0 - 1.2*(c(2,0,1))),
             NA,
             
             sum(c(7,5,3) - 1 - 1.0*(c(1,2,0))),
             sum(c(NA,6,4) - 1 - 1.0*(c(1,2,0)), na.rm = TRUE),
             NA,
             
             sum(c(-9,7,5) - -0.3 - -0.4*(c(-4,1,2))),
             sum(c(10,NA,6) - 1 - 2*(c(-4,1,2)), na.rm = TRUE),
             NA),
           
           nrow=3, ncol=3)
  )
})

# Define your test
test_that("res_momentum is running correctly with NAs", {
  expect_equal(
    res_momentum(
      ret_assets_main = matrix(c(5,6,-4,7,NA,4,-9,10,5), nrow=3, ncol=3), #Ret Assets Main
      ret_assets_complementary = matrix(c(1,2,-1,3,4,NA), nrow=3, ncol=2), #Ret Assets Complementary
      ret_bench_main = matrix(c(2,1,-4), nrow=1, ncol=3), #Ret Bench Main
      ret_bench_complementary = matrix(c(1,0), nrow=1, ncol=2), #Ret Bench Complementary
      beta_bench = matrix(c(1,NA,-1,1,1,NA,-0.4,2,0.3), nrow=3, ncol=3), #Beta
      alpha_bench = matrix(c(NA,0,-0.4,1,1,0,-0.3,1,NA), nrow=3, ncol=3) #Alpha
    ),
    matrix(c(NA,
             NA,
             sum(c(-4,NA,-1) - -0.4 - -1.0*(c(2,0,1)), na.rm = TRUE),
             
             sum(c(7,5,3) - 1 - 1.0*(c(1,2,0))),
             sum(c(NA,6,4) - 1 - 1.0*(c(1,2,0)), na.rm = TRUE),
             NA,
             
             sum(c(-9,7,5) - -0.3 - -0.4*(c(-4,1,2))),
             sum(c(10,NA,6) - 1 - 2*(c(-4,1,2)), na.rm = TRUE),
             NA),
           
           nrow=3, ncol=3)
  )
})




# Define your test
test_that("res_momentum is running correctly with all NAs", {
  expect_equal(
    res_momentum(
      ret_assets_main = matrix(c(NA,NA,NA,NA,NA,NA,NA,NA,NA), nrow=3, ncol=3), #Ret Assets Main
      ret_assets_complementary = matrix(c(NA,NA,NA,NA,NA,NA), nrow=3, ncol=2), #Ret Assets Complementary
      ret_bench_main = matrix(c(2,1,-4), nrow=1, ncol=3), #Ret Bench Main
      ret_bench_complementary = matrix(c(1,0), nrow=1, ncol=2), #Ret Bench Complementary
      beta_bench = matrix(c(NA,NA,NA,NA,NA,NA,NA,NA,NA), nrow=3, ncol=3), #Beta
      alpha_bench = matrix(c(NA,0,NA,NA,NA,NA,NA,NA,NA), nrow=3, ncol=3) #Alpha
    ),
    matrix(c(NA,NA,NA,NA,NA,NA,NA,NA,NA
             ),
           
           nrow=3, ncol=3)
  )
})




# Define your test
test_that("res_momentum is running correctly with Infs", {
  expect_equal(
    res_momentum(
      ret_assets_main = matrix(c(5,6,-4,Inf,8,4,-9,10,5), nrow=3, ncol=3), #Ret Assets Main
      ret_assets_complementary = matrix(c(1,2,-1,3,4,Inf), nrow=3, ncol=2), #Ret Assets Complementary
      ret_bench_main = matrix(c(2,1,-4), nrow=1, ncol=3), #Ret Bench Main
      ret_bench_complementary = matrix(c(1,0), nrow=1, ncol=2), #Ret Bench Complementary
      beta_bench = matrix(c(1,1.2,-1,1,1,0.2,-0.4,2,0.3), nrow=3, ncol=3), #Beta
      alpha_bench = matrix(c(0.5,0,-0.4,1,1,0,-0.3,1,-Inf), nrow=3, ncol=3) #Alpha
    ),
    matrix(c(sum(c(5,3,1) - 0.5 - 1.0*(c(2,0,1))),
             sum(c(6,4,2) - 0 - 1.2*(c(2,0,1))),
             sum(c(-4,Inf,-1) - -0.4 - -1.0*(c(2,0,1))),
             
             sum(c(Inf,5,3) - 1 - 1.0*(c(1,2,0))),
             sum(c(8,6,4) - 1 - 1.0*(c(1,2,0))),
             sum(c(4,-4,Inf) - 0 - 0.2*(c(1,2,0))),
             
             sum(c(-9,Inf,5) - -0.3 - -0.4*(c(-4,1,2))),
             sum(c(10,8,6) - 1 - 2*(c(-4,1,2))),
             sum(c(5,4,-4) - -Inf -0.3*(c(-4,1,2)))),
           
           nrow=3, ncol=3)
  )
})



# Define your test
test_that("res_momentum throws an error when there are different number of rows/columns", {
  expect_error(
    res_momentum(
      ret_assets_main = matrix(c(5,6,7,8), nrow=2, ncol=2), #Ret Assets Main
      ret_assets_complementary = matrix(c(3,4), nrow=1, ncol=2), #Ret Assets Complementary
      ret_bench_main = matrix(c(2,1), nrow=1, ncol=2), #Ret Bench Main
      ret_bench_complementary = matrix(c(1,0), nrow=1, ncol=2), #Ret Bench Complementary
      beta_bench = matrix(c(1,1.2,1,1), nrow=2, ncol=2), #Beta
      alpha_bench = matrix(c(0.5,0,1,1), nrow=2, ncol=2) #Alpha
    ), "ret_assets_main and ret_assets_complementary should have same number of rows."
   )
})

test_that("res_momentum throws an error when there are different number of rows/columns", {
  expect_error(
    res_momentum(
      ret_assets_main = matrix(c(5,6,7,8), nrow=2, ncol=2), #Ret Assets Main
      ret_assets_complementary = matrix(c(1,2,3,4), nrow=2, ncol=2), #Ret Assets Complementary
      ret_bench_main = matrix(c(2,1,3,4), nrow=2, ncol=2), #Ret Bench Main
      ret_bench_complementary = matrix(c(1,0), nrow=1, ncol=2), #Ret Bench Complementary
      beta_bench = matrix(c(1,1.2,1,1), nrow=2, ncol=2), #Beta
      alpha_bench = matrix(c(0.5,0,1,1), nrow=2, ncol=2) #Alpha
    ), "ret_bench_main and ret_bench_complementary should have same number of rows."
 
  )
})


test_that("res_momentum throws an error when there are different number of rows/columns", {
  expect_error(
    res_momentum(
      ret_assets_main = matrix(c(5,6,7), nrow=3, ncol=1), #Ret Assets Main
      ret_assets_complementary = matrix(c(1,2,3,4,0,0), nrow=3, ncol=2), #Ret Assets Complementary
      ret_bench_main = matrix(c(2,1), nrow=1, ncol=2), #Ret Bench Main
      ret_bench_complementary = matrix(c(1,0), nrow=1, ncol=2), #Ret Bench Complementary
      beta_bench = matrix(c(1,1.2,1,1), nrow=2, ncol=2), #Beta
      alpha_bench = matrix(c(0.5,0,1,1), nrow=2, ncol=2) #Alpha
    ), "ret_bench_main and ret_assets_main should have same number of columns"
  )
})

test_that("res_momentum throws an error when there are different number of rows/columns", {  
  expect_error(
    res_momentum(
      ret_assets_main = matrix(c(5,6,7,8), nrow=2, ncol=2), #Ret Assets Main
      ret_assets_complementary = matrix(c(1,2,3,4,1,0), nrow=2, ncol=3), #Ret Assets Complementary
      ret_bench_main = matrix(c(2,1), nrow=1, ncol=2), #Ret Bench Main
      ret_bench_complementary = matrix(c(1,0), nrow=1, ncol=2), #Ret Bench Complementary
      beta_bench = matrix(c(1,1.2,1,1), nrow=2, ncol=2), #Beta
      alpha_bench = matrix(c(0.5,0,1,1), nrow=2, ncol=2) #Alpha
    ), "ret_assets_complementary and ret_bench_complementary should have same number of columns"

  )
})
  
test_that("res_momentum throws an error when there are different number of rows/columns", { 
  expect_error(
    res_momentum(
      ret_assets_main = matrix(c(5,6,7,8), nrow=2, ncol=2), #Ret Assets Main
      ret_assets_complementary = matrix(c(1,2,3,4), nrow=2, ncol=2), #Ret Assets Complementary
      ret_bench_main = matrix(c(2,1), nrow=1, ncol=2), #Ret Bench Main
      ret_bench_complementary = matrix(c(1,0), nrow=1, ncol=2), #Ret Bench Complementary
      beta_bench = matrix(c(1,1.2,1,1,0.4,0.6), nrow=3, ncol=2), #Beta
      alpha_bench = matrix(c(0.5,0,1,1), nrow=2, ncol=2) #Alpha
    ), "beta_bench and ret_assets_main should have same dimension"
  )
})
  
test_that("res_momentum throws an error when there are different number of rows/columns", { 
  expect_error(
      res_momentum(
        ret_assets_main = matrix(c(5,6,7,8), nrow=2, ncol=2), #Ret Assets Main
        ret_assets_complementary = matrix(c(1,2,3,4), nrow=2, ncol=2), #Ret Assets Complementary
        ret_bench_main = matrix(c(2,1), nrow=1, ncol=2), #Ret Bench Main
        ret_bench_complementary = matrix(c(1,0), nrow=1, ncol=2), #Ret Bench Complementary
        beta_bench = matrix(c(1,1.2,1,1), nrow=2, ncol=2), #Beta
        alpha_bench = matrix(c(0.5,0,1,1,4,NA), nrow=3, ncol=2) #Alpha
      ),"alpha_bench and ret_assets_main should have same dimension"

    )
  
})



# Define your test
test_that("res_momentum throws an error when there are NAs in bench vector", {
  expect_error(
    res_momentum(
      ret_assets_main = matrix(c(5,6,7,8), nrow=2, ncol=2), #Ret Assets Main
      ret_assets_complementary = matrix(c(1,2,3,4), nrow=2, ncol=2), #Ret Assets Complementary
      ret_bench_main = matrix(c(NA,1), nrow=1, ncol=2), #Ret Bench Main
      ret_bench_complementary = matrix(c(1,0), nrow=1, ncol=2), #Ret Bench Complementary
      beta_bench = matrix(c(1,1.2,1,1), nrow=2, ncol=2), #Beta
      alpha_bench = matrix(c(0.5,0,1,1), nrow=2, ncol=2) #Alpha
    ), "There should not be any NAs in Bench Vector"
  )
  
  expect_error(
    res_momentum(
      ret_assets_main = matrix(c(5,6,7,8), nrow=2, ncol=2), #Ret Assets Main
      ret_assets_complementary = matrix(c(1,2,3,4), nrow=2, ncol=2), #Ret Assets Complementary
      ret_bench_main = matrix(c(4,1), nrow=1, ncol=2), #Ret Bench Main
      ret_bench_complementary = matrix(c(1,NA), nrow=1, ncol=2), #Ret Bench Complementary
      beta_bench = matrix(c(1,1.2,1,1), nrow=2, ncol=2), #Beta
      alpha_bench = matrix(c(0.5,0,1,1), nrow=2, ncol=2) #Alpha
    ), "There should not be any NAs in Bench Vector"
  )

})


# Define your test
test_that("res_momentum handles extreme values", {
  expect_equal(
    res_momentum(
      ret_assets_main = matrix(c(1e20, 1e-20, -1e20, -1e-20), nrow=2, ncol=2),
      ret_assets_complementary = matrix(c(1e20, 1e-20, -1e20, -1e-20), nrow=2, ncol=2),
      ret_bench_main = matrix(c(1e20, 1e-20), nrow=1, ncol=2),
      ret_bench_complementary = matrix(c(1e20, 1e-20), nrow=1, ncol=2),
      beta_bench = matrix(c(1e20, 1e-20, -1e20, -1e-20), nrow=2, ncol=2),
      alpha_bench = matrix(c(1e20, 1e-20, -1e20, -1e-20), nrow=2, ncol=2)
    ),
    matrix(c(sum(c(1e20,-1e20,1e20) - 1e20 - 1e20*(c(1e20,1e-20,1e20))),
             sum(c(1e-20,-1e-20,1e-20) - 1e-20 - 1e-20*(c(1e20,1e-20,1e20))),
             sum(c(-1e20,1e20,-1e20) - -1e20 -  -1e20*(c(1e-20,1e20,1e-20))),
             sum(c(-1e-20,1e-20,-1e-20) - -1e-20 - -1e-20*(c(1e-20,1e20,1e-20)))),
           nrow=2, ncol=2)
  )
})




