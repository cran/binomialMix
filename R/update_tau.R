#' E-step : update of tau 
#' 
#' @param df A dataframe
#' @param K The numeric value of the total number of clusters
#' @param col_id A character value corresponding to id column name
#' @param beta_hk A matrix of estimated beta
#' @param lambda A numeric vector of proportion into the different clusters
#' @param m A numeric iterative value
#' @param df_id A list of dataframe filter by id levels
#' @param n_c A numeric vector containing the number of rows for each distinct id levels
#' @param matrix_id A list of design matrices filter by id levels
#' @param var_weights A character value corresponding to the weights variable
#' @param target A character value corresponding to the target variable
#' @return result_pi Estimated probabilities of tau matrix
#' @export
update_tau <-
function(df, K, col_id = "id", beta_hk, lambda, m, df_id, n_c, matrix_id, var_weights, target) {
  
  pi <- list()
  log_num_tau <- sapply(extract_id(df, col_id), function(c) {
    sapply(1:K, function(k) {
      log(lambda[[m]][k]) + log_density_binom(df_id[[c]][, target] * df_id[[c]][, var_weights], matrix_id[[c]], beta_hk[[m]], k, df_id[[c]][, var_weights])
    })
  })
  
  num_tau <- exp(Rmpfr::mpfr(log_num_tau, 40))
  
  if (K == 1) {
    denom_tau <- sum(num_tau)
  } else {
    denom_tau <- apply(num_tau, 2, sum)
  }
  
  # Calcul de la probilité que la campagne c soit dans la classe l sachant df_id :
  if (K == 1) {
    
    tau[[m]] <- round(sapply(1:length(extract_id(df, col_id)), function(c) {
      as.numeric(num_tau[c]/denom_tau)
    }), 3)
    
  } else {
    
    pi[[m]] <- round(sapply(1:length(extract_id(df, col_id)), function(c) {
      as.numeric(num_tau[, c]/denom_tau[[c]])
    }), 3)
    
  }
  
  result_pi<-pi[[m]]
  return(result_pi)
}
