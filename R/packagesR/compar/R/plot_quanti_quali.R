#' Permet de faire des plots quanti vs quali
#'
#' @param plot_type le type de plot ('boxplot','barplot','scatteplot').
#' @param data un dataframe.
#' @param var_quanti1 une variable quantitative.
#' @param var_quanti2 une variable quantitative (up: pas obligatoire).
#' @param var_quali une variable qualitative.
#' @param modalites choix de la modalite a discriminer.
#' @param color couleur du graphique.
#' @param type le type de graphique ("classique','ggplot','plotly')
#'
#' @return
#' @export
#'
#' @examples
plot_quanti_quali <- function(plot_type="boxplot",data,var_quanti1,var_quanti2,var_quali,modalites="toute la population",color="grey",type="classique"){

  x <- data[,var_quanti1]
  y <- data[,var_quali]

  ##boxplot##
  if(plot_type == "boxplot"){
    boitemoustache(data,var_quanti1,type = type,color = color,modalite = var_quali)
  }
  ##barplot##
  else if (plot_type == "barplot") {
    diagbatons(data,var_quali,color = color,type = type,variable = var_quanti1)
  }
  ##scatterplot##
  else{
    if(type=="classique"){
      if(modalites == "toute la population"){
        mod <- levels(as.factor(y))
        taille_grille <- floor(sqrt(length(mod)))+1
        par(mfrow = c(taille_grille,taille_grille))
        for(i in 1:length(mod)){
          data_filtre <- data[y == mod[i],]
          plot(data_filtre[,var_quanti1],data_filtre[,var_quanti2],xlab = var_quanti1, ylab = var_quanti2,pch=20,main=mod[i])
        }
      }
      else{#on regarde un sous ensemble
        data_filtre <- data[y == modalites,]
        plot(data_filtre[,var_quanti1],data_filtre[,var_quanti2],xlab = var_quanti1, ylab = var_quanti2,pch=20,main=modalites)
      }
    }
    else{##cas ggplot
      if(modalites == "toute la population"){
        ggplot(data, aes(x = x, y = data[,var_quanti2])) +
          geom_point() +
          facet_wrap(~data[,var_quali]) +
          labs(
            x = var_quanti1,
            y = var_quanti2
          )
      }
      else {#on regarde un sous ensemble
        data_filtre <- data[y == modalites,]
        ggplot(data_filtre,aes(x = data_filtre[,var_quanti1], y = data_filtre[,var_quanti2])) +
          geom_point() +
          labs(
            x = var_quanti1,
            y = var_quanti2,
            title = var_quali,
            caption = "Data from Himalayan Expeditions"
          )
      }
    }
  }
}
