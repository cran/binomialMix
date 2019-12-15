context("init_lambda")

test_that("lambda is correctly initialized", {

  K<-3
  expect_is(init_lambda(K),'numeric')
  expect_equal(length(init_lambda(K)),K)
  
})