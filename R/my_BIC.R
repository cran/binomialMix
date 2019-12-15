#' Calculate the Bayesian Information Criterion (BIC)
#' 
#' @param nb_param The number of paramaters estimated by the EM
#' @param logl A numeric value which is the maximum value from Incomplete Loglikelihood
#' @param nb_obs A numeric value corresponding to the rows number of the whole dataframe
#' @return BIC The numeric value of the BIC
#' @export
my_BIC <-
function(nb_param, logl, nb_obs) {
  BIC <- -2 * max(logl, na.rm = TRUE) + nb_param * log(nb_obs)
  return(BIC)
}
