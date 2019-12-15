#' Calculate the incomplete loglikelihood from mixture of binomial
#'
#' @param df A dataframe
#' @param col_id A character value corresponding to id column name
#' @param target A character value corresponding to the target variable
#' @param var_weights A character value corresponding to the weights variable 
#' @param df_id A list of dataframe filter by id levels
#' @param matrix_id A list of design matrices filter by id levels
#' @param lamb A numeric vector of proportion into the different clusters
#' @param b_hk A matrix of estimated beta 
#' @param K A numeric value of number of clusters chosen for the mixture
#' @return result A numeric value of incomplete loglikelihood
#' @export
Incomplete_Loglikelihood_binomiale <-
function(df, col_id = "id", target, var_weights, df_id, matrix_id, lamb, b_hk, K) {
  
  if( !(col_id %in% colnames(df)) ) stop('col_id does not exist in this dataframe')
  if( any(str_detect(df[,col_id], "\\d")==FALSE) ) stop('col_id should only be with factor numeric values')
  if( !(var_weights %in% colnames(df)) ) stop('weights variable does not exist in this dataframe')
  
  result <- 0
  for (c in extract_id(df, col_id)) {
    aux <- 0
    for (k in 1:K) {
      aux <- aux + exp(Rmpfr::mpfr(log(lamb[k]) + log_density_binom(df_id[[c]][, target] * df_id[[c]][, var_weights], matrix_id[[c]], b_hk, k, df_id[[c]][, var_weights]), 
                                   50))
    }
    result <- result + log(aux)
  }
  result<-as.numeric(result)
  return(result)
}
