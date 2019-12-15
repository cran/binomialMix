context("extract_target")

test_that("right extraction of the target from formula", {
  
  input_formula<-"survived~class+sex+age"
  output<-"survived"
  expect_equal(extract_target(input_formula),output)
  
  input_formula<-as.formula("survived~class+sex+age")
  expect_equal(extract_target(input_formula),output)
  
  input_formula<-2
  expect_error(extract_target(input_formula),"formula needs to be a character type or formula type")

})