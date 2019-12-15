#' Extract target value of GLM
#'
#' @param formula A character formula with target variable and predictor variables
#' @return y The target variable from formula in character type
#' @export
#' @examples
#' extract_target("ctr~timeSlot")
extract_target <-
function(formula) {
  
  if ( inherits(formula,"formula")==FALSE & inherits(formula,"character")==F ) stop('formula needs to be a character type or formula type')
  if ( inherits(formula,"formula") == TRUE){
    y <- unlist(strsplit(as.character(formula),"~"))[2]
  } else
  y <- unlist(strsplit(as.character(formula), "~"))[1]
  return(y)
}
