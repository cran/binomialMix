
------------------------------------------------------------------------

output: github\_document

------------------------------------------------------------------------

R-package binomialMix
=====================

![pipeline](https://gitlab.com/tabmo/binomialmix/badges/develop/pipeline.svg)

Copyright 2019 Faustine Bousquet (faustine.bousquet@tabmo.io or faustine.bousquet@umontpellier.fr) from TabMo and IMAG (Institut Montpelliérain Alexander Grothendieck, University of Montpellier). The binomialMix package is available under the Apache2 license.

Description
-----------

The **binomialMix** package provides a clustering method for longitudinal and non gaussian data. It uses an EM algorithm for GLM.

Instruction for users
---------------------

### Installation

You can install the `binomialMix` R package with the following R command:

``` r
# install.packages("devtools")
devtools::install_git("https://gitlab.com/tabmo/binomialmix")
devtools::install_gitlab("tabmo/binomialMix")
```

You can also directly use the git repository :

``` bash
git clone https://gitlab.com/tabmo/binomialMix
```

Once you cloned the git repository, you can run to install the `binomialMix` package:

``` r
devtools::install("/path/to/binomialMix/pkg") # edit the path
```

### Example of use

-   Import the library :

``` r
library(binomialMix)
```

-   Load the data :

``` r
data(adcampaign)
```

Of course, you can use your own data. The format you need to have is the following : 
- a dataframe is needed 
- a column with factor id representing the objects you want to cluster 
- a target value \* a weighted value variable as we are in case of binomial data 
- at least, one column as explicative variable

**Run the clustering algorithm**
Here, we want to cluster advertising campaigns. Each campaigns (column "id") is composed of n\_c observations from the whole dataset. We have repeated mesure for a same id level. The explicatives variables could be : day, timeSlot or app\_or\_site. We want to try with K=3 clusters.

``` r
model_formula<-"ctr~timeSlot+day"
weighted_variable<-"impressions"
nb_cluster<-3
df_tocluster<-adcampaign
col_id<-"id"
result_K3<-runEM(model_formula,
                  weighted_variable,
                  nb_cluster,
                  df_tocluster,
                  col_id)
```

-   Analysis of clustering obtained : The output of the runEM function provides the following values :
-   loglikelihood for each EM iteration
-   estimation of *β*, *λ*, *π* parameters
-   BIC/ICL value
-   Number of fisher iteration needed for each M-Step

**Plotting evolution of Loglikelihood over iteration**

``` r
# Plotting Loglikelihood :
install.packages("ggplot2")
library(ggplot2)
qplot(seq_along(result_K3[[1]]), result_K3[[1]])
```

**Matrix of beta estimated (values taken for last iteration) :**

``` r
head(result_K3[[2]][[length(result_K3[[2]])]])
```

    ##            [,1]       [,2]       [,3]
    ## [1,] -3.8126661 -5.2914380 -3.2418550
    ## [2,] -0.4134079  0.3794783  0.4115441
    ## [3,] -0.2975236  0.2407683  0.4076950
    ## [4,] -0.1948168  0.2122175  0.3753815
    ## [5,] -0.1590104  0.4028323  0.1885215
    ## [6,] -0.2160946  0.3545593  0.1872363

**Vector of proportion in each cluster (values taken for last iteration) :**

``` r
result_K3[[3]][[length(result_K3[[3]])]]
```

    ## [1] 0.1871000 0.7246125 0.0883000

**Matrix of proability for each campaign to belong to the different cluster (values taken for last iteration) :**

``` r
## Too large to print here
result_K3[[4]][[length(result_K3[[4]])]]
```

**BIC value as numeric :**

``` r
paste0("BIC=",result_K3[[5]][[length(result_K3[[5]])]])
```

    ## [1] "BIC=387914.537681485"

**ICL value as numeric :**

``` r
paste0("ICL value=",result_K3[[6]][[length(result_K3[[6]])]])
```

    ## [1] "ICL value=387919.96962191"

**Total number of EM iteration as numeric value :**

``` r
paste0("Number of EM iteration :",length(result_K3[[7]]))
```

    ## [1] "Number of EM iteration :10"

**Matrix of Fisher scoring number of iteration at each M step :**

``` r
matrix(unlist(result_K3[[7]]),ncol=length(result_K3[[7]])-1)
```

    ##      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9]
    ## [1,]    4    3    4    6    3    3    2    1    1
    ## [2,]    3    2    2    2    2    2    2    1    1
    ## [3,]    5    4    2    2    3    1    1    1    1

``` r
#nrow is equal to the number of cluster
#ncol is equal to the number of iteration
```
