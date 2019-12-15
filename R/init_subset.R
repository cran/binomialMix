#' Initialize the estimation of beta
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr distinct
#' @importFrom dplyr sample_frac
#' @importFrom dplyr pull
#' @importFrom stringr str_detect
#' @importFrom dplyr select
#' 
#' @param df A dataframe
#' @param col_id A character value corresponding to id column name
#' @param K The number of dataframe to obtain depending on the number of cluster chosen for the mixture
#' @return subset_df A list of K subset of dataframe
#' @examples
#' init_subset(adcampaign,3,"id")
#' @export
init_subset <-
function(df,K,col_id="id"){
  
  if( !(col_id %in% colnames(df)) ) stop('col_id does not exist in this dataframe')
  if( any(str_detect(df[,col_id], "\\d")==F) ) stop('col_id should only be with factor numeric values')
  subset_df<-list()
  for (i in 1:K){
    sample_id<-as.character(df %>%
                              dplyr::select(col_id) %>% distinct() %>%
                              sample_frac(1/K) %>%
                              pull((!!col_id)))
    subset_df[[i]]<-df %>% filter(df[,col_id] %in% sample_id) %>% droplevels()
  }
  return(subset_df)
}
