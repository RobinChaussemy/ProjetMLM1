#' diagbatons
#' Permet de comparer un barplot classique a un ggplot/plotly.
#'
#' @param data un dataframe.
#' @param modalite une variable qualitative.
#' @param variable une variable quelconque.
#' @param tri si TRUE, trie les barres.
#' @param color la couleur.
#' @param type le type de graphique: ('classique','ggplot' ou 'plotly').
#' @import ggplot2
#' @import plotly
#' @return un barplot
#' @export
#'
#' @examples
diagbatons<- function(data,modalite,tri=FALSE,color="grey",type="classique",variable=NULL){
  if(!is.data.frame(data)){
    stop("data doit etre un dataframe")
  }
  else if(is.numeric(data[,modalite])){
    stop("la variable modalite doit etre de type qualitatif")
  }
  else if(tri!= TRUE && tri != FALSE){
    stop("tri doit etre egal a TRUE/FALSE")
  }
  else if(type!= "classique" && type != "ggplot" && type!= "plotly"){
    stop("type doit etre egal a 'classique'/'ggplot'/'plotly")
  }
  else{
    y <- data[,modalite]
    if(is.null(variable)){
      if(type == "classique"){
        if(!tri){
          barplot(table(y), col = color, las = 2)
        }
        else {
          barplot(sort(table(y)), col = color, las = 2)
        }
      }
      else if(type == "ggplot"){
        if(!tri){
          ggplot(data, aes(x = y, fill = y)) +
            geom_bar() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
        }
        else {
          data |> barsTriees(y)
        }
      }
      else if(type == "plotly"){
        if(!tri){
          plot <- plot_ly(data, x = ~y, type = "histogram",color=~y)
          layout(plot,xaxis = list(title = modalite))
        }
        else{
          category_counts <- table(y)
          sorted_categories <- names(sort(category_counts))
          sort_mod <- factor(y, levels = sorted_categories)
          plot <- plot_ly(x=sort_mod, type = "histogram", color = sort_mod)
          layout(plot,xaxis = list(title = modalite))
        }
      }
    }
    else{#une quanti en fct d'une quali
      x <- data[,variable]
      if(type=="classique"){
        cont <- table(y,x)
        barplot(cont,beside = FALSE, col = color,legend.text = TRUE, main = paste("Distribution de ",variable," par ",modalite),
                xlab = " ",ylab = "Frequence",las = 2)
      }
      else if(type == "ggplot"){##ggplot
        ggplot(data, aes(x = x, fill = y)) +
        geom_bar() +
        labs(
            x = variable,
            y = modalite,
            fill = modalite
        ) +
          theme(axis.text.x = element_text(angle = 90, hjust = 1))
      }
      else if(type == "plotly"){
        plot <- plot_ly(x = ~x, type = "histogram",color=~y)
        plot %>% layout(barmode= "stack",xaxis=list(title=variable))
      }

    }
  }
}
