#' M-step : update of beta parameters
#'
#' @param formula A character formula with target variable and predictor variables
#' @param df A dataframe
#' @param k The numeric value of the specific cluster to be updated
#' @param col_id A character value corresponding to id column name
#' @param tau A matrix of dimension : rows number is the number of cluster K, columns number is the number of distinct levels id
#' @param m A numeric iterative value
#' @param w_inv An inverse matrix representing the W matrix in the beta equation for the M step
#' @param z Working data in the EM algorithm
#' @param matrix_id A list of design matrices filter by id levels
#' @return result_beta Estimated beta for cluster k
#'
#' @importFrom stats as.formula
#' @importFrom MASS ginv
#' @export
update_beta <-
function(formula, df, k, col_id, tau, m, w_inv, z, matrix_id) {
  
  if (formula == as.formula("ctr~1")) {
    num_beta <- Reduce("+", sapply(1:length(extract_id(df, col_id)), function(c) {
      tau[[m]][k, c] * (t(matrix_id[!sapply(matrix_id, is.null)][[c]]) %*% w_inv[[c]] %*% z[[c]])
    }))
  } else {
    num_beta <- apply(sapply(1:length(as.numeric(levels(df$id))), function(c) {
      tau[[m]][k, c] * (t(matrix_id[!sapply(matrix_id, is.null)][[c]]) %*% w_inv[[c]] %*% z[[c]])
    }
    ), 1, sum)
  }
  aux <- Reduce("+", lapply(c(1:length(extract_id(df, col_id))), function(c) {
    tau[[m]][k, c] * (t(matrix_id[!sapply(matrix_id, is.null)][[c]]) %*% w_inv[[c]] %*% (matrix_id[!sapply(matrix_id, is.null)][[c]]))
  }))
  result_beta<-(ginv(aux) %*% num_beta)
  return(result_beta)
}
