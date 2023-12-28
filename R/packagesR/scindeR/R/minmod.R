#' minmod
#' fonction retournant la modalité avec le moins d'occurence.
#'
#' @param data un dataframe
#' @param variable  une variable qualitative de ce dataframe
#'
#' @return
#' @export
#'
#' @examples
minmod<- function(data, variable){
  if(is.data.frame(data) == FALSE)
    print("data doit être un dataframe")
  else if (!variable %in% names(data))
    print("il doit s'agir d'un nom de variable présent dans la dataframe")
  else if (is.numeric(data[,variable]))
    print("la variable doit être qualitative")
  else{
    counts <- table(data[,variable])
    names(counts)[which.min(counts)]
  }

}
