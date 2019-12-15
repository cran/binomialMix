#' Calculate the Integrated Complete Likelihood (ICL)
#'
#' @param data A dataframe  
#' @param col_id A character value corresponding to id column name
#' @param nb_cluster A numeric value of number of clusters chosen for the mixture
#' @param nb_param The number of paramaters estimated by the EM
#' @param logl A numeric value which is the maximum value from Incomplete Loglikelihood
#' @param val_tau A matrix of probability which rows number is the K clusters, columns number is the number of distinct id levels 
#' @param nb_obs A numeric value corresponding to the rows number of the whole dataframe
#' @return ICL The numeric value of the ICL
#' @export
my_ICL <-
function(data, col_id, nb_cluster, nb_param, logl, val_tau, nb_obs) {
  aux <- 0
  for (c in 1:length(extract_id(data, col_id))) {
    for (k in 1:nb_cluster) {
      if (val_tau[k, c] != 0) {
        aux <- aux + val_tau[k, c] * log(val_tau[k, c])
      }
    }
  }
  ICL <- my_BIC(nb_param, logl, nb_obs) - 2 * aux
  return(ICL)
}
