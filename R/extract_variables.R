#' Extract variables from GLM model
#' 
#' @param formula A character formula with target variable and predictor variables 
#' @return formula_var The predictor variables from formula in formula type
#' @export
#' @examples 
#' extract_variables("ctr~timeSlot")
extract_variables <-
function(formula) {
  
  if ( inherits(formula,"formula")==FALSE & inherits(formula,"character")==FALSE ) stop('formula needs to be a character type or formula type')
  if ( inherits(formula,"formula") == TRUE){
    formula_var <- as.formula(paste0("~",unlist(strsplit(as.character(formula), "~"))[3]))
  } else
    formula_var <- as.formula(paste0("~",unlist(strsplit(as.character(formula), "~"))[2]))
  return(formula_var)
}
