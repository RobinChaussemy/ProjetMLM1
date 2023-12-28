#' Density
#' Densite d'une variable quantitative selon le type de plot specifie
#'
#' @param data un dataframe
#' @param variable la variable
#' @param color la couleur du graphique
#' @param modalite variable a discriminer (Null par defaut)
#' @param type le type de graphique souhaite (classique,ggplot)
#' @import ggplot2
#' @import ggridges
#' @import plotly
#' @return
#' @export
#' @examples
densite <- function(data,variable,modalite=NULL,type="classique",color="black"){
  x1 <- data[,variable]
  if(type == "classique"){
    if(is.null(modalite))
      plot(density(x1),main = paste("densite de la variable ",variable),col=color)
    else{
      moda <- levels(as.factor(data[,modalite]))
      nb <- length(moda)
      colors <- 1:(nb + 1)
      for(i in 1:nb){
        mod<- moda[i]
        datasub <- data[data[,modalite]== mod,]
        x1 <- datasub[,variable]
        if(i == 1)
          plot(density(x1),main=paste("Densite de ",variable," en fonction de ",modalite),col=colors[i])
        else
          lines(density(x1),col=colors[i])
      }
      legend("topright",legend=moda,col=colors,lty=1,cex=0.6)

    }

  }
  else if(type == "ggplot"){
    if(is.null(modalite))
      ggplot(data,aes(x = x1)) + geom_density(color = color) +
      labs(
        x = variable
      )
    else{
      y <- data[,modalite]
      ggplot(data, aes(x = x1, y = y, fill = y, color = y)) +
      geom_density_ridges(alpha = 0.5, show.legend = TRUE) +
      labs(
        x = variable,
        y = NULL,
        color = modalite,
        fill = modalite
      )
    }
  }
  else if (type == "plotly"){
    if(is.null(modalite)){
      densite <- density(x1)
      p <- plot_ly(x = densite$x, y = densite$y, type = "scatter", mode = "lines", line = list(shape = "linear",color=color))
    }

    else{
      p <- plot_ly(data, x = ~x1, type = "histogram", histnorm = "probability", color = data[,modalite])
    }
    layout(p,xaxis = list(title = variable))

  }
}
