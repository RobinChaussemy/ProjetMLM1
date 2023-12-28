
Int <- function(x){
  as.integer(x)
}

#' Title
#'
#' @param indiv_prediction Vecteur comprennant l’individu à prédire
#' @param n Permet de sélectionner le nombre de voisin
#' @param p Défini le p dans la norme pour la p-norme
#'
#' @return La fonction renvoie un vecteur contenant :
#' $prediction : Renvoie une valeur boolean sur la succes de l’expédition
#' $proba : Renvoie le vecteur contenant la probabilité d’appartenir à chaque classe
#' $temps : Renvoie le temps mis pour l'encodage et la prédiction
#'
#' @importFrom reticulate use_virtualenv
#' @importFrom reticulate source_python
#'
#' @export
#'
#' @examples KNN_Python(c("Ama Dablam","Autumn","France","Climber",2025,1,1,1,1,1,1,1), n = 11 , p = 3)

KNN_Python <- function(indiv_prediction,n = Int(11), p = Int(3)){
  use_virtualenv("../Python/venv")
  source_python("../Python/main.py")
  KNN_result = KNN_Process(data,indiv_prediction,target ,Int(n), Int(p))
  return(list(prediction = KNN_result[[1]], proba_f = KNN_result[[2]],proba_s = KNN_result[[3]],temps = KNN_result[[4]] ))
}
