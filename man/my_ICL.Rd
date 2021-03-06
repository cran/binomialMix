% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/my_ICL.R
\name{my_ICL}
\alias{my_ICL}
\title{Calculate the Integrated Complete Likelihood (ICL)}
\usage{
my_ICL(data, col_id, nb_cluster, nb_param, logl, val_tau, nb_obs)
}
\arguments{
\item{data}{A dataframe}

\item{col_id}{A character value corresponding to id column name}

\item{nb_cluster}{A numeric value of number of clusters chosen for the mixture}

\item{nb_param}{The number of paramaters estimated by the EM}

\item{logl}{A numeric value which is the maximum value from Incomplete Loglikelihood}

\item{val_tau}{A matrix of probability which rows number is the K clusters, columns number is the number of distinct id levels}

\item{nb_obs}{A numeric value corresponding to the rows number of the whole dataframe}
}
\value{
ICL The numeric value of the ICL
}
\description{
Calculate the Integrated Complete Likelihood (ICL)
}
