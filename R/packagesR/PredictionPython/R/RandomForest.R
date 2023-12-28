
Int <- function(x){
  as.integer(x)
}
#' Title
#'
#' @param indiv_prediction Vecteur comprennant l’individu à prédire
#' @param n Nombre d'estimateur dans l’algorithme de la Random Forest
#'
#' @importFrom reticulate use_virtualenv
#' @importFrom reticulate source_python
#'
#' @return
#' @export
#'
#' @examples
RandomForest_Python <- function(indiv_prediction,n = Int(2)){
  use_virtualenv("../Python/venv")
  source_python("../Python/main.py")
  RandomForest_result = Tree_Process(data,indiv_prediction,Int(n))
  return(list(prediction = RandomForest_result[[1]], proba_f = RandomForest_result[[2]],proba_s = RandomForest_result[[3]],temps = RandomForest_result[[4]]))
}
