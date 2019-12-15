#' Calculate de log density of a binomial
#'
#' @param y A dataframe corresponding to a specific id levels from col_id
#' @param matrix_id A design matrix corresponding to a specific id levels from col_id
#' @param b_hk A matrix of estimated beta
#' @param k A numeric value to select the beta from a specific cluster
#' @param var_weights A character value corresponding to the weights variable
#' @return res A numeric value
#' @export
log_density_binom <-
function(y, matrix_id, b_hk, k, var_weights) {
  if (any(y<0)) stop('target variable must be > 0')
  theta <- matrix_id %*% b_hk[, k]
  if (length(y) != nrow(theta)) stop('check for dimension of target variable and/or design matrices')
  vec <- y * theta - var_weights * log(1 + exp(theta)) + log(gmp::chooseZ(var_weights, y))
  res <- (sum(vec, na.rm = TRUE))
  if( is.infinite(res)) stop('Loglikelihood result is infinite : verify values of target and weights variables')
  return(res)
}
