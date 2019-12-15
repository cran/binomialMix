#' Initialize design matrices from dataframe to cluster
#'
#' @importFrom stats model.matrix
#' @param formula A character formula with target variable and predictor variables
#' @param df A dataframe to cluster
#' @param col_id A character value corresponding to name of id column in the dataframe df
#' @return result_list A list containing the df filter by id levels, the design matrices filter by id levels, the number of rows for df filter by id levels
#' @examples
#' init_design_matrices("ctr~timeSlot",adcampaign,"id")
#' @export
init_design_matrices <-
function(formula, df, col_id = "id") {
  
  if (nrow(df)==0 | ncol(df)==0) stop('dataframe is empty : 0 row, 0 column or both')
  if( !(col_id %in% colnames(df)) ) stop('col_id does not exist in this dataframe')
  if( any(str_detect(df[,col_id], "\\d")==FALSE) ) stop('col_id should only be with factor numeric values')
  form <- extract_variables(formula)
  init_y_c <- list()
  init_n_c <- NULL
  init_matrix_hc <- list()
  for (c in extract_id(df, col_id)) {
    init_y_c[[c]] <- df %>% filter(df[, col_id] == c)
    init_n_c[c] <- nrow(init_y_c[[c]])
    init_matrix_hc[[c]] <- model.matrix(form, data = init_y_c[[c]])
    
  }
  result_list<-list()
  result_list<-list(init_y_c, init_n_c, init_matrix_hc)
  return(result_list)
}
