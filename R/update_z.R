#' M-step : Update the matrix of working variables Z from beta iterative equation
#'
#' @param df A dataframe
#' @param col_id A character value corresponding to id column name
#' @param target A character value corresponding to the target variable
#' @param beta_up A matrix of estimated beta in a specific cluster k
#' @param df_id A list of dataframe filter by id levels
#' @param matrix_id A list of design matrices filter by id levels
#' @return work_z An up-to-date matrix of working variables Z
#' @export 
update_z <-
function(df, col_id = "id", target, beta_up, df_id, matrix_id) {
  work_z <- sapply(extract_id(df, col_id), function(c) {
    matrix_id[[c]] %*% beta_up + diag(((1 + exp(as.numeric(matrix_id[[c]] %*% beta_up)))^2)/exp(as.numeric(matrix_id[[c]] %*% beta_up)), ncol = dim(matrix_id[[c]])[1], 
                                      nrow = dim(matrix_id[[c]])[1]) %*% (df_id[[c]][, target] - exp(matrix_id[[c]] %*% beta_up)/(1 + exp(matrix_id[[c]] %*% beta_up)))
  })
  return(work_z)
}
