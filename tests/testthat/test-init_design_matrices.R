#' @importFrom dplyr select
context("init_design_matrices")

test_that("design matrices are correctly initialized", {
  
  df_input <- data.frame(prop  = c(0.125, 0,  0.667, 0.005,  0.9),
                         cases = c(8, 1,3,3,10),
                         x1     = c(11,12,15,16,18),
                         group = c("a","b","a","a","b"))
  input         <- rbind(df_input,df_input)
  input$success <- rep(c(1, 0), each=nrow(df_input))
  input$cases   <- round(input$cases*ifelse(input$success,input$prop,1-input$prop))
  rm(df_input)
  mod<-glm(success ~ x1, family = binomial, data = input, weights = cases)
  mod_form<-"success~1"
  
  expect_error(init_design_matrices(mod_form,input,"id"),'col_id does not exist in this dataframe')
  expect_error(init_design_matrices(mod_form,input,"group"),'col_id should only be with factor numeric values')
  
  input$group_num<-as.factor(as.numeric(input$group))
  expect_is(init_design_matrices(mod_form,input,"group_num"),'list')
  
  input<-data.frame()
  expect_error(init_design_matrices(mod_form,input,"group_num"),'dataframe is empty : 0 row, 0 column or both')
  input<-data.frame(prop  = c(0.125, 0,  0.667, 0.005,  0.9),
                    cases = c(8, 1,3,3,10),
                    x1     = c(11,12,15,16,18),
                    group = c("a","b","a","a","b")) %>% filter(prop>1)
  expect_error(init_design_matrices(mod_form,input,"group_num"),'dataframe is empty : 0 row, 0 column or both')
  input<-data.frame(prop  = c(0.125, 0,  0.667, 0.005,  0.9),
                    cases = c(8, 1,3,3,10),
                    x1     = c(11,12,15,16,18),
                    group = c("a","b","a","a","b")) %>% dplyr::select(-c(prop,cases,x1,group))
  expect_error(init_design_matrices(mod_form,input,"group_num"),'dataframe is empty : 0 row, 0 column or both')

  
})
