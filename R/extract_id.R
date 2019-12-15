#' Extract levels as numeric from id column of the dataset
#'
#' @param df A dataframe
#' @param col_id A character value corresponding to id column name
#' @return dist_id The numeric levels of id column from df
#' @export
#' @examples
#' extract_id(adcampaign,"id")
extract_id <-
function(df, col_id) {
  if( !(col_id %in% colnames(df)) ) stop('col_id does not exist in this dataframe')
  dist_id<-as.numeric(levels(df[, col_id]))
  return(dist_id)
}
