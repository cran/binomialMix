context("log_density_binom")

test_that("Test the good input/output of the log density", {
  
  input <- data.frame(success  = c(1,0,2,0,1,0,0,1),
                      cases = c(8, 1,3,3,10,1,2,1),
                      x1     = c(11,12,15,16,18,1,10,10),
                      group = c("10","20","10","10","20","10","10","20"))
  input$prop   <- input$success/input$cases
  input_matrix<-list()
  input_group<-list()
  for (g in as.numeric(levels(input$group))) {
    input_group[[g]]<-input %>% filter(input[,"group"] == g)
    input_matrix[[g]]<-model.matrix(success ~ x1, data = input_group[[g]])
  }
  input_lambda<-c(0.80,0.20)
  input_beta<-matrix(cbind(glm(prop ~ x1, family = binomial, data = input_group[[10]], weights = cases)$coefficients,
                           glm(prop ~ x1, family = binomial, data = input_group[[20]], weights = cases)$coefficients),ncol=2)
  K<-2
  g<-as.numeric(levels(input$group))[1]
  k<-2

  expect_is(log_density_binom(input_group[[g]][, "prop"],
                              input_matrix[[g]],
                              input_beta,
                              1,
                              input_group[[g]][,"cases"]),
            "numeric")

  expect_error(log_density_binom(-input_group[[g]][, "prop"],
                                 input_matrix[[g]],
                                 input_beta,
                                 1,
                                 input_group[[g]][,"cases"]),
               'target variable must be > 0')
  
  expect_error(log_density_binom(-input_group[[g]][, "prop"],
                                 input_matrix[[g]],
                                 input_beta,
                                 1,
                                 input_group[[g]][,"cases"]),
               'target variable must be > 0')

  expect_error(log_density_binom(input_group[[g]][, "success"],
                                 input_matrix[[g]][-1,],
                                 input_beta,
                                 1,
                                 input_group[[g]][,"cases"]),
               'check for dimension of target variable and/or design matrices')
  
  g<-20
  expect_error(log_density_binom(input_group[[g]][, "success"],
                                 input_matrix[[g]],
                                 input_beta,
                                 1,
                                 c(1,0,1)),
               'Loglikelihood result is infinite : verify values of target and weights variables')
  
  
})