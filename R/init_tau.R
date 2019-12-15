#' Initialize the matrix probability of each levels id to be in the clusters
#' 
#' @param df A dataframe
#' @param col_id A character value corresponding to id column name
#' @param K The number of dataframe to obtain depending on the number of cluster chosen for the mixture
#' @return result_matrix A matrix of dimension : rows number is the number of cluster K, columns number is the number of distinct levels from id column
#' @examples
#' init_tau(adcampaign,3,"id")
#' @export 
init_tau <-
function(df, K, col_id = "id") {
  if( !(col_id %in% colnames(df)) ) stop('col_id does not exist in this dataframe')
  if( any(str_detect(df[,col_id], "\\d")==FALSE) ) stop('col_id should only be with factor numeric values')
  result_matrix<-matrix(0, nrow = K, ncol = length(levels(as.factor(df[, col_id]))))
  return(result_matrix)
}
