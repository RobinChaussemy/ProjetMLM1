#' Boxplot
#' Permet de comparer des boxplot classique a ceux de ggplot/plotly
#'
#' @param data un dataframe.
#' @param variable une variable quanti du data.
#' @param nbClasses le nombre de classes pour cut la variable quanti (si =1 , pas de classes).
#' @param type le type du graphique (ggplot,classique,ou plotly).
#' @param color la couleur des boxplot.
#' @param varwidth TRUE pour ajuster la taille des box selon la quantite de donnees.
#' @param modalite variable qualitative a discriminer.
#' @import forcats
#' @import ggplot2
#' @import plotly
#' @return
#' @export
#'
#' @examples
boitemoustache <- function(data,variable,nbClasses=1,varwidth = FALSE,type = "classique",color="grey",modalite=NULL){
  if(!is.data.frame(data)){
    stop("data doit etre un dataframe")
  }
  #else if(!is.numeric(data[,variable])){
   # stop("variable doit etre de type quantitatif")
  #}
  else if(nbClasses <1){
    stop("le nombre de classes ne peut pas etre inferieur a 1")
  }
  else if(varwidth!= TRUE && varwidth != FALSE){
    stop("varwidth doit etre egal a TRUE/FALSE")
  }
  else if(type != "classique" && type!= "ggplot" && type != "plotly"){
    stop("type doit etre egal a 'classique','ggplot' ou 'plotly'")
  }
  else{
    x1 <- data[,variable]
    if(!is.null(modalite)){
      y <- data[,modalite]
      if(type == "classique"){
        means <- tapply(x1,y,mean)
        boxplot(x1~y,col = color,main = paste("boxplot de la variable ",variable," en fonction de ",modalite), ylab = variable,xlab = "",las = 2)
        points(x = 1:length(means), y = means, pch = 19, col = "black") #ajout de la moyenne
      }
      else if (type == "ggplot"){
        ggplot(data, aes(x = fct_reorder(y,x1, median),y=x1, color = y)) +
          geom_boxplot() +
          stat_summary(
            fun = "mean",
            geom = "point",
            shape = 18,
            size = 1
          ) +
          labs( #la legende
            x = modalite,
            y = variable,
            color = modalite
          ) +
          theme(axis.text.x = element_text(angle = 90, hjust = 1))
      }
      else if (type == "plotly"){
        plot <- plot_ly(data, y = ~x1, type = "box",color = ~y)
        layout(plot, yaxis = list(title = variable))
      }
    }
    else{
      if(type == "classique"){
        if(nbClasses>1){
          bins <- seq(min(x1), max(x1), length.out = nbClasses + 1)
          labels <- paste0("(", round(bins[-length(bins)], 0), ",", round(bins[-1], 0), "]")
          varcut <- cut(x1, breaks = nbClasses,labels = labels)
          boxplot(x1 ~ varcut, varwidth = varwidth, ylab = variable,xlab = "",main="",las=2,col=color)
        }
        else {
          boxplot(x1,xlab = variable,main = "",col=color)
        }
      }
      else if(type == "ggplot"){
        ggplot(data, aes(y = x1)) +
        geom_boxplot(aes(group = cut_interval(x1, nbClasses)),varwidth = varwidth,color=color) +
        labs(
          y = variable
        )
      }
      else if (type == "plotly"){
        if(nbClasses>1){
          x1cut <- cut(x1,nbClasses)
          plot <- plot_ly(y = ~x1, type = "box", x = x1cut, width = 0.6,boxpoints="outliers",line = list(colors = color))
          layout(plot,xaxis = list(title = x1cut),yaxis = list(title = variable))
        }
        else{
          plot <- plot_ly(y = ~x1, type = "box",line = list(color = color))
          layout(plot,xaxis = list(title = ""),yaxis = list(title = variable))
        }
      }
    }
  }
}
