#' Initialize the vector lambda of mixture proportion
#' @param K A numeric value corresponding to the number of cluster
#' @return result A numeric vector of length K 
#' @examples 
#' init_lambda(K=3)
#' @export 
init_lambda <-
function(K) {
  result<-rep(1/K, K)
  return(result)
}
