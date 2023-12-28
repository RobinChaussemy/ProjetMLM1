#' permet de faire un plot qualitatif
#'
#' @param plot_type le type de plot ('histogramme','Density','boxplot','scatterplot').
#' @param data un dataframe
#' @param var1 une variable qualitative
#' @param var2 une 2eme variable qualitative
#' @param tri tri un barplot si "oui".
#' @param color couleur du graphique.
#' @param type le type de graphe utilise ('classique','ggplot','plotly')
#'
#' @return
#' @export
#'
#' @examples
plot_quali <- function(plot_type="barplot",data,var1,var2,tri="non",color="grey",type="classique"){
  ##barplot##
  if(plot_type == "barplot"){
    diagbatons(data,var1,ifelse(tri=="non",FALSE,TRUE),color = color,type = type)
  }
  ##mosaicplot##
  else if(plot_type == "mosaicplot"){
    mosaique(data,var1,var2,col = color, type = type)
  }
}
