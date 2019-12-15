context("extract_variables")

test_that("right extraction of the variables (except the target) from formula", {
  
  input_formula<-"survived~class+sex+age"
  output<-as.formula("~class+sex+age")
  expect_equal(extract_variables(input_formula),output)
  
  input_formula<-as.formula(input_formula)
  expect_equal(extract_variables(input_formula),output)
  
  input_formula<-3
  expect_error(extract_variables(input_formula),"formula needs to be a character type or formula type")
  
})