#' M-step : Update the diagonal matrix W from beta iterative equation
#' 
#' @param df A dataframe
#' @param col_id A character value corresponding to id column name
#' @param var_weights A character value corresponding to the weights variable
#' @param beta_up A matrix of estimated beta in a specific cluster k
#' @param df_id A list of dataframe filter by id levels
#' @param matrix_id A list of design matrices filter by id levels
#' @return omega_inv An up-to-date diagonal matrix W
#' @export
update_w <-
function(df, col_id = "id", var_weights, beta_up, df_id, matrix_id) {
  omega_inv <- sapply(extract_id(df, col_id), function(c) {
    diag(df_id[[c]][, var_weights] * exp(as.numeric(matrix_id[[c]] %*% beta_up))/(1 + exp(as.numeric(matrix_id[[c]] %*% beta_up)))^2, ncol = dim(matrix_id[[c]])[1], 
         nrow = dim(matrix_id[[c]])[1])
  })
  return(omega_inv)
}
