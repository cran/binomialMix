context("extract_id")

test_that("extract distinct id works", {
  
  input<-data.frame(cbind(c("3","6","1203","141414","3","3","3","1203","1203"),
                          sample(1:9),
                          c(rep("a",3),rep("b",4),rep("a",2))))
  colnames(input)<-c("people","grade","group")
  output<-as.numeric(levels(input$people))

  expect_equal(extract_id(input,"people"),output)
  expect_error(extract_id(input,"id"),"col_id does not exist in this dataframe")
  
})
