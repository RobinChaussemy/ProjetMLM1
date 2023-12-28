#' scinde une data
#' fonction permettant de recuperer une subdata avec seulement le type de variables specifie
#'
#' @param data un dataframe
#' @param type une chaine de caractere specifiant la partie retourne.
#' Si null,renvoie une liste avec les noms des variables pour chaque type.
#'
#' @return
#' @export
#'
#' @examples
scinde <- function(data,type=NULL) {
  names_data_quanti <- c()
  names_data_quali <- c()
  names_data_binaire <- c()
  names_data<-names(data)
  for(i in 1:length(names_data)){
    var <- names_data[i]
    if(is.numeric(data[,var])){
      names_data_quanti <- append(names_data_quanti,var)
    }
    else {
      names_data_quali <- append(names_data_quali,var)
      if(length(levels(as.factor(data[,var])))== 2){
        names_data_binaire <- append(names_data_binaire,var)
      }
    }
  }
  res <- list(quanti = names_data_quanti,quali = names_data_quali,binaire = names_data_binaire)
  if(is.null(type))
    return(res)
  else if(type == "quanti")
    data[,res$quanti]
  else if (type == "quali")
    data[,res$quali]
  else if (type == "binaire")
    data[,res$binaire]
  else
    print('le type doit etre:"quanti","quali" ou "binaire')
}
