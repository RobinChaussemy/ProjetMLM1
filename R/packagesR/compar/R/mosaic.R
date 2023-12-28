#' Mosaicplot
#' Permet de comparer un mosaicplot classique a un ggplot/plotly.
#'
#' @param data un dataframe
#' @param var1 une variable qualitative
#' @param var2 une 2eme variable qualitative
#' @param color la couleur du graphique
#' @param type le type de graphique: ('classique','ggplot' ou 'plotly').
#' @import rlang
#' @return un mosaicplot
#' @export
#'
#' @examples
mosaique <- function(data,var1,var2,color="grey",type="classique"){
  if(!is.data.frame(data)){
    stop("data doit etre un dataframe")
  }
  else if(is.numeric(data[,var1])){
    stop("la variable var1 doit etre de type qualitatif")
  }
  else if(is.numeric(data[,var2])){
    stop("la variable var2 doit etre de type qualitatif")
  }
  else if(type!= "classique" && type != "ggplot" && type!= "plotly"){
    stop("type doit etre egal a 'classique'/'ggplot'/'plotly")
  }
  else if(type == "classique")
    mosaicplot(table(data[, var1], data[, var2]),col = color, xlab = var1,ylab = var2,main = paste("Mosaicplot de ",var1,"en fonction de ",var2),las = 2)
  else if (type == "ggplot"){
    ggplot(data = data) +
    geom_mosaic(aes(x = product(!!rlang::sym(var1),!!rlang::sym(var2)), fill = !!rlang::sym(var1)))
  }
}
