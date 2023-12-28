#'
#' Histogramme d'une variable quantitative selon le type de plot specifie
#'
#' @param data un dataframe
#' @param variable la variable
#' @param nbClasses le nombre de classes souhaite pour decouper la variable
#' @param color la couleur du graphique
#' @param type le type de graphique souhaite ('classique','ggplot','plotly')
#' @import ggplot2
#' @import plotly
#' @return
#' @export
#'
#' @examples
#' histogramme(data = iris,variable = "Sepal.Length",nbClasses = 8,color = "red",type = "classique")
#' histogramme(data = iris,variable = "Sepal.Width",nbClasses = 12,color = "blue",type = "ggplot")
histogramme <- function(data,variable,nbClasses=8,color="grey",type="classique"){
  if(!is.data.frame(data)){
    stop("data doit etre un dataframe")
  }
  else if(!is.numeric(data[,variable])){
    stop("variable doit etre de type quantitatif")
  }
  else if(nbClasses <1){
    stop("le nombre de classes ne peut pas etre inferieur a 1")
  }
  else if(type != "classique" && type!= "ggplot" && type != "plotly"){
    stop("type doit etre egal a 'classique' ou 'ggplot'")
  }
  else{
    x1 <- data[,variable]
    bins <- seq(min(x1), max(x1), length.out = nbClasses + 1)
    labels <- paste0("(", round(bins[-length(bins)], 0), ",", round(bins[-1], 0), "]")
    if(type == "classique"){
      hist(x1,breaks = bins,main="histogramme",col=color, xlab = variable)
    }
    else if(type == "ggplot"){
      ggplot(data, aes(x = cut(x1, breaks = nbClasses,labels=labels))) +
        geom_bar(color = color)+
        labs( #la legende
          x = variable
        ) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    }
    else if (type == "plotly"){
      p <- plot_ly(data, x = ~x1, type = "histogram", xbins = list(size = (max(x1) - min(x1)) / nbClasses),marker=list(color = color))
      layout(p, xaxis = list(title = variable))

    }
  }
}
