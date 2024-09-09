# Define your test
test_that("Financial Cia Adj. is running correctly.", {
  expect_equal(
    financial_cia_adjust(
      matrix(c(1,2,3,4,5,6,7,8), nrow=4, ncol=2),
      matrix(c("A","Intermediários financeiros","A","Intermediários financeiros","A","Intermediários financeiros", "A", "Intermediários financeiros"), nrow=4, ncol=2),
      matrix(c(9,8,7,6,5,4,3,2), nrow=4, ncol=2),
      c("Intermediários financeiros", "Previdência e seguros")),
    matrix(c(1,8,3,6,5,4,7,2), nrow=4, ncol=2)
  )
  
  expect_equal(
    financial_cia_adjust(
      matrix(c(1,2,3,4,-5,6,7,8), nrow=4, ncol=2),
      matrix(c("A","Intermediários financeiros","A","Intermediários financeiros","A","Intermediários financeiros", "A", "Intermediários financeiros"), nrow=4, ncol=2),
      matrix(c(9,-8,7,6,5,4,3,0), nrow=4, ncol=2),
      c("Intermediários financeiros", "Previdência e seguros")),
    matrix(c(1,-8,3,6,-5,4,7,0), nrow=4, ncol=2)
  )
  
})

# Define your test
test_that("Financial Cia Adj. is running correctly.", {
  expect_equal(
    financial_cia_adjust(
      matrix(c(1,2,3,4,5,6,7,8), nrow=4, ncol=2),
      matrix(c("A","Previdência e seguros","A","Intermediários financeiros","A","Previdência e seguros", "A", "Intermediários financeiros"), nrow=4, ncol=2),
      matrix(c(9,8,7,6,5,4,3,2), nrow=4, ncol=2),
      c("Intermediários financeiros", "Previdência e seguros")),
    matrix(c(1,8,3,6,5,4,7,2), nrow=4, ncol=2)
  )
})


test_that("Financial Cia Adj. is running correctly Adj only banks.", {
  expect_equal(
    financial_cia_adjust(
      matrix(c(1,2,3,4,5,6,7,8), nrow=4, ncol=2),
      matrix(c("A","Previdência e seguros","A","Intermediários financeiros","A","Previdência e seguros", "A", "Intermediários financeiros"), nrow=4, ncol=2),
      matrix(c(9,8,7,6,5,4,3,2), nrow=4, ncol=2),
      c("Intermediários financeiros")),
    matrix(c(1,2,3,6,5,6,7,2), nrow=4, ncol=2)
  )
})


# Define your test
test_that("Financial Cia Adj. is running correctly - DFs.", {
  expect_equal(
    financial_cia_adjust(
      data.frame(matrix(c(1,2,3,4,5,6,7,8), nrow=4, ncol=2)),
      data.frame(matrix(c("A","Intermediários financeiros","A","Intermediários financeiros","A","Intermediários financeiros", "A", "Intermediários financeiros"), nrow=4, ncol=2)),
      data.frame(matrix(c(9,8,7,6,5,4,3,2), nrow=4, ncol=2)),
      c("Intermediários financeiros", "Previdência e seguros")),
    matrix(c(1,8,3,6,5,4,7,2), nrow=4, ncol=2)
  )
})

# Define your test
test_that("Financial Cia Adj. is running correctly - DFs.", {
  expect_equal(
    financial_cia_adjust(
      matrix(c(1,2,3,4,5,6,7,8), nrow=4, ncol=2),
      data.frame(matrix(c("A","Intermediários financeiros","A","Intermediários financeiros","A","Intermediários financeiros", "A", "Intermediários financeiros"), nrow=4, ncol=2)),
      data.frame(matrix(c(9,8,7,6,5,4,3,2), nrow=4, ncol=2)),
      c("Intermediários financeiros", "Previdência e seguros")),
    matrix(c(1,8,3,6,5,4,7,2), nrow=4, ncol=2)
  )
})


# Define your test
test_that("Financial Cia Adj.  is running correctly - DFs.", {
  expect_equal(
    financial_cia_adjust(
      data.frame(matrix(c(1,2,3,4,5,6,7,8), nrow=4, ncol=2)),
      data.frame(matrix(c("A","Intermediários financeiros","A","Intermediários financeiros","A","Intermediários financeiros", "A", "Previdência e seguros"), nrow=4, ncol=2)),
      data.frame(matrix(c(9,8,7,6,5,4,3,2), nrow=4, ncol=2)),
      c("Intermediários financeiros", "Previdência e seguros")),
    matrix(c(1,8,3,6,5,4,7,2), nrow=4, ncol=2)
  )
})
# Define your test
test_that("Financial Cia Adj.  is running correctly -  Only one Financial Cia Adj. Adj", {
  expect_equal(
    financial_cia_adjust(
      data.frame(matrix(c(1,2,3,4,5,6,7,8), nrow=4, ncol=2)),
      data.frame(matrix(c("A","Intermediários financeiros","A","C","A","Intermediários financeiros", "A", "C"), nrow=4, ncol=2)),
      data.frame(matrix(c(9,8,7,6,5,4,3,2), nrow=4, ncol=2)),
      c("Intermediários financeiros", "Previdência e seguros")),
    matrix(c(1,8,3,4,5,4,7,8), nrow=4, ncol=2)
  )
})

# Define your test
test_that("Financial Cia Adj.  is running correctly -  No sector to adjust", {
  expect_equal(
    financial_cia_adjust(
      data.frame(matrix(c(1,2,3,4,5,6,7,8), nrow=4, ncol=2)),
      data.frame(matrix(c("A","Intermediários financeiros","A","C","A","Intermediários financeiros", "A", "C"), nrow=4, ncol=2)),
      data.frame(matrix(c(9,8,7,6,5,4,3,2), nrow=4, ncol=2)),
      c("D")),
    matrix(c(1,2,3,4,5,6,7,8), nrow=4, ncol=2))
    
})



# Define your test
test_that("Financial Cia Adj. is running correctly with Infs.", {
  expect_equal(
    financial_cia_adjust(
      matrix(c(1,2,3,Inf,5,6,7,8), nrow=4, ncol=2),
      matrix(c("A","Intermediários financeiros","A","Intermediários financeiros","A","Intermediários financeiros", "A", "Intermediários financeiros"), nrow=4, ncol=2),
      matrix(c(9,8,7,-Inf,5,4,3,2), nrow=4, ncol=2),
      c("Intermediários financeiros", "Previdência e seguros")),
    matrix(c(1,8,3,-Inf,5,4,7,2), nrow=4, ncol=2)
  )
  

})

# Define your test
test_that("Financial Cia Adj.  is running correctly - NA.", {
  expect_equal(
    financial_cia_adjust(
      matrix(c(NA,2,3,NA,5,6,1,1), nrow=4, ncol=2),
      matrix(c("A","Intermediários financeiros","A","Intermediários financeiros","A","Intermediários financeiros", "A", "Intermediários financeiros"), nrow=4, ncol=2),
      matrix(c(1,2,3,3,2,1,2,NA), nrow=4, ncol=2),
      c("Intermediários financeiros", "Previdência e seguros")),
    matrix(c(NA,2,3,3,5,1,1,NA), nrow=4, ncol=2)
  )
})

# Define your test
test_that("Financial Cia Adj.  is running correctly - Sector NA.", {
  expect_equal(
    financial_cia_adjust(
      matrix(c(NA,2,3,NA,5,6,1,1), nrow=4, ncol=2),
      matrix(c("A","Intermediários financeiros","A",NA,"A","Intermediários financeiros", "A", NA), nrow=4, ncol=2),
      matrix(c(1,2,3,3,2,1,2,NA), nrow=4, ncol=2),
      c("Intermediários financeiros", "Previdência e seguros")),
    matrix(c(NA,2,3,NA,5,1,1,1), nrow=4, ncol=2)
  )
})


# Define your test
test_that("Financial Cia Adj. throws an error when diff dim. .", {
  expect_error(
    financial_cia_adjust(
      matrix(c(1,2,3,4,5,6), nrow=3, ncol=2),
      matrix(c("A","Previdência e seguros","A","Intermediários financeiros","A","Previdência e seguros", "A", "Intermediários financeiros"), nrow=4, ncol=2),
      matrix(c(9,8,7,6,5,4,3,2), nrow=4, ncol=2),
      c("Intermediários financeiros", "Previdência e seguros")),
    "Input matrices must have the same dimensions.")
    

  expect_error(
      financial_cia_adjust(
        matrix(c(1,2,3,4,5,6,7,8), nrow=4, ncol=2),
        matrix(c("A","Previdência e seguros","A","Intermediários financeiros","A","Previdência e seguros", "A", "Intermediários financeiros"), nrow=4, ncol=2),
        matrix(c(9,8,7,6), nrow=4, ncol=1),
        c("Intermediários financeiros", "Previdência e seguros")),
      "Input matrices must have the same dimensions.")
      

})


