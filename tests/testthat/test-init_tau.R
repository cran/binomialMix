context("init_tau")

test_that("tau is correctly initialized", {
  
  input<-data.frame(cbind(c("3","6","1203","141414","3","3","3","1203","1203"),
                          sample(1:9),
                          c(rep("a",3),rep("b",4),rep("a",2))),
                    stringsAsFactors = TRUE)
  colnames(input)<-c("people","grade","group")
  output<-as.numeric(levels(input$people))
  
  expect_error(init_tau(input,2,"id"),'col_id does not exist in this dataframe')
  expect_error(init_tau(input,4,"group"),'col_id should only be with factor numeric values')
  
  input$group_num<-as.numeric(input$group)
  
  expect_is(init_tau(input,3,"group_num"),'matrix')
  expect_equal(nrow(init_tau(input,3,"group_num")),3)
  expect_equal(ncol(init_tau(input,3,"group_num")),nlevels(as.factor(input$group_num)))

})