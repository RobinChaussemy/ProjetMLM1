#' Scatterplot
#' Permet de comparer un scatterplot classique a un de ggplot
#'
#' @param data un dataframe.
#' @param variable1 une variable quanti.
#' @param variable2 une variable quanti.
#' @param type le type du graphique (ggplot,classique ou plotly).
#' @param color la couleur des individus.
#' @param pch le caractère du point.
#' @param reg la courbe de regression lisse.
#' @param modalite la variable a dicriminer.
#'
#' @import ggplot2
#' @import plotly
#' @return
#' @export
#'
#' @examples
scatterplot <- function(data, variable1,variable2,type="classique",color="black",pch=19,modalite=NULL,reg=FALSE){
  if(!is.data.frame(data)){
    stop("data doit etre un dataframe")
  }
  else if(!is.numeric(data[,variable1]) || !is.numeric(data[,variable2])){
    stop("les variables doivent etre de type quantitatif")
  }
  else if(!is.numeric(pch) || round(pch,0)!= pch || pch>20 || pch <0){
    stop("pch doit etre un entier entre 0 et 20")
  }
  else if(reg!=FALSE && reg!=TRUE){
    stop("reg doit etre egal a TRUE/FALSE")
  }

  else if(type != "classique" && type!= "ggplot" && type!= "plotly"){
    stop("type doit etre egal a 'classique' ou 'ggplot'")
  }
  else if(!is.null(modalite) && !is.character(modalite)){
    stop("modalite doit etre une variable qualitative")
  }
  else {
    x1 <- data[,variable1]
    x2 <- data[,variable2]
    if (is.null(modalite)){#toute la population
      if(type == "classique")
        plot(x1,x2,xlab = variable1, ylab = variable2,col = color, pch = pch)
      else if(type == "ggplot"){
        ggplot(data, aes(x = x1, y = x2)) +
        geom_bin2d() +
        labs(
          x = variable1,
          y = variable2,
        )
      }
      else if(type == "plotly"){
        plot <- plot_ly(data, x = ~x1, y = ~x2, type = "histogram2d")
        layout(plot,xaxis = list(title = variable1),yaxis = list(title = variable2))
      }
    }
    else{#on discrimine par rapport a une modalite
      y <- data[,modalite]
      if(type == "classique"){
        moda <- levels(as.factor(y))
        pchs <-  ifelse(y == moda[1],pch,pch+1)
        couleurs <- ifelse(y == moda[1], "blue","deeppink")
        plot(x1,x2, xlab = variable1, ylab = variable2,col = couleurs,pch = pchs)
        legend("topright", legend = moda, pch = c(pch,pch+1), col = c("blue","deeppink"), cex = 0.8)
      }
      else if (type == "ggplot"){ ##ggplot version
        p1 <- ggplot(data, aes(x = x1, y = x2)) +
              geom_point(aes(color = y)) +
              labs( #la legende
                x = variable1,
                y = variable2,
                color = modalite,
                title = paste(variable1," en fonction de ",variable2)
              )
        if(reg == TRUE){
          p1 <- p1 + geom_smooth(se = FALSE)
        }
        p1
      }
      else if(type == "plotly"){
        plot <- plot_ly(data, x = ~x1, y = ~x2, type = "scatter",color = ~y,mode="markers")
        layout(plot,xaxis = list(title = variable1),yaxis = list(title = variable2))
      }
    }
  }
}
