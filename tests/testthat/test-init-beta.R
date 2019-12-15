context("init_beta")

test_that("beta is correctly initialized", {
  
  df_input <- data.frame(prop  = c(0.125, 0,  0.667, 0.005,  0.9),
                     cases = c(8, 1,3,3,10),
                     x1     = c(11,12,15,16,18),
                     group = c("a","b","a","a","b"))
  input         <- rbind(df_input,df_input)
  input$success <- rep(c(1, 0), each=nrow(df_input))
  input$cases   <- round(input$cases*ifelse(input$success,input$prop,1-input$prop))
  rm(df_input)
  mod<-glm(success ~ x1, family = binomial, data = input, weights = cases)
  
  expect_error(init_subset(input,2,"id"),'col_id does not exist in this dataframe')
  expect_error(init_subset(input,4,"group"),'col_id should only be with factor numeric values')
  
  input$group_num<-as.numeric(input$group)
  
  expect_is(init_subset(input,3,"group_num"),'list')
  expect_length(init_subset(input,5,"group_num"),5)
  
  output<-init_subset(input,2,"group_num")

})