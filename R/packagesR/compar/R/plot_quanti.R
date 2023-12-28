#' permet de faire un plot quantitatif
#'
#' @param plot_type le type de plot ('histogramme','Density','boxplot','scatterplot')
#' @param data un dataframe
#' @param variable_quanti1 une variable quantitative.
#' @param variable_quanti2 une variable quantitative.
#' @param variable_binaire une variable indicatrice.
#' @param modalites choix de la modalite a discriminer.
#' @param breaksHist le nombre de tranches pour un histogramme
#' @param breaksBox le nombre de tranches pour un boxplot
#' @param ajust ajustement de la taille des box si 'oui'.
#' @param discr discrimination par une modalite si 'oui'.
#' @param type le type de graphe utilise ('classique','ggplot','plotly')
#'
#' @return
#' @export
#'
#' @examples
plot_quanti<-function(plot_type="Histogram",data,variable_quanti1,variable_quanti2,variable_binaire,modalites="Aucune",breaksHist=10,breaksBox=1,ajust='non',discr='non',type = "classique"){

  if(plot_type == "Histogram"){
    histogramme(data,variable_quanti1,breaksHist,"red",type)
  }
  #######Densite######
  else if (plot_type == "Density"){
    modalite <- NULL
    if(modalites!="Aucune")
      modalite <- modalites
    densite(data = data,variable = variable_quanti1,modalite=modalite ,type = type)
  }
  ######boxplot######
  else if (plot_type == "boxplot"){
    width <- ifelse(ajust == 'oui',TRUE,FALSE)
    boitemoustache(data = data,variable = variable_quanti1,nbClasses = breaksBox,varwidth = width,type = type)#palette_couleurs[1:breaksBox])#gestion des couleurs
  }
  ##scatterplot ##
  else {
    modalite <- NULL
    if(discr == "oui")
      modalite <- variable_binaire
    scatterplot(data,variable_quanti1,variable_quanti2,type,modalite = modalite)
  }
}
