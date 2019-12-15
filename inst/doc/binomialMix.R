## ----eval=FALSE----------------------------------------------------------
#  # install.packages("devtools")
#  devtools::install_git("https://gitlab.com/tabmo/binomialmix")
#  devtools::install_gitlab("tabmo/binomialMix")

## ----eval=FALSE----------------------------------------------------------
#  devtools::install("/path/to/binomialMix/pkg") # edit the path

## ---- echo = TRUE--------------------------------------------------------
# our library for mixture modelling:
library(binomialMix)
# if not installed : 
#install.packages("pander", repos="http://cran.us.r-project.org")
#install.packages("ggplot2", repos="http://cran.us.r-project.org")
#library(pander)
library(qpdf)

## ---- echo = TRUE--------------------------------------------------------
data(adcampaign)

## ---- echo=FALSE---------------------------------------------------------
#pandoc.table(head(adcampaign),split.table=Inf)
head(adcampaign)

## ----eval=TRUE-----------------------------------------------------------
# The dataframe to cluster: 
df_tocluster<-adcampaign
# We choose two explainable variables:
model_formula<-"ctr~timeSlot+day"
# As we are in a case of binomial mixture model, we define the weighted variable
weighted_variable<-"impressions"
# We want to analyse results for K=3.
K<-3
# We define the individual to cluster:
col_id<-"id"
set.seed(1992)
# We run our EM algorithm developped for mixture of binomial and longitudinal dataset:
result_K3<-runEM(model_formula,
                weighted_variable,
                K,
                df_tocluster,
                col_id)

## ---- eval=FALSE,results = "asis"----------------------------------------
#  library(ggplot2)
#  qplot(seq_along(result_K3[[1]]), result_K3[[1]],
#        xlab="Number of EM iterations",
#        ylab="Loglikelihood")

## ----eval=FALSE,echo=TRUE------------------------------------------------
#  result_K3[[3]][[length(result_K3[[3]])]]

## ---- echo=FALSE---------------------------------------------------------
df_beta<-result_K3[[2]][[length(result_K3[[2]])]]
colnames(df_beta)<-paste0("k=",c(1:3))
#pandoc.table(head(df_beta),split.table=Inf,digits=4)
head(df_beta)

## ----eval=TRUE,echo=TRUE-------------------------------------------------
result_K3[[3]][[length(result_K3[[3]])]]

## ----echo=TRUE,eval=FALSE------------------------------------------------
#  # We only display the results for the first 10 campaigns (10 columns)
#  set.seed(1992)
#  result_K3[[4]][[length(result_K3[[4]])]][,1:10]

## ---- echo=FALSE---------------------------------------------------------
# We only display the results for the first 10 campaigns (10 columns)
df_proba<-as.data.frame(result_K3[[4]][[length(result_K3[[4]])]][,1:10])
colnames(df_proba)<-paste0("ID_",c(1:10))
rownames(df_proba)<-paste0("k=",c(1:3))
#pandoc.table(df_proba,split.table=Inf,digits=4)
head(df_proba)

## ----eval=FALSE,echo=TRUE------------------------------------------------
#  result_K3[[5]][[length(result_K3[[5]])]] # BIC value
#  result_K3[[6]][[length(result_K3[[6]])]] # ICL value

## ----eval=TRUE,echo=FALSE------------------------------------------------
paste0("BIC=",round(result_K3[[5]][[length(result_K3[[5]])]],2))
paste0("ICL=",round(result_K3[[6]][[length(result_K3[[6]])]],2))

## ----eval=FALSE,echo=TRUE------------------------------------------------
#  matrix(unlist(result_K3[[7]]),ncol=length(result_K3[[7]])-1)

## ---- echo=FALSE---------------------------------------------------------
#nrow is equal to the number of cluster
#ncol is equal to the number of iteration
df_fisher<-as.data.frame(matrix(unlist(result_K3[[7]]),ncol=length(result_K3[[7]])-1))
colnames(df_fisher)<-paste0("iter_",c(1:(length(result_K3[[7]])-1)))
rownames(df_fisher)<-paste0("k=",c(1:3))
#pandoc.table(df_fisher,split.table=Inf)
head(df_fisher)

