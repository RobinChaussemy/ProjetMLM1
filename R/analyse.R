library(shiny)
library(shinythemes)
library(RColorBrewer)
library(reactable)
library(tidyverse)
library(scales)
library(ggrepel)
library(patchwork)
library(ggridges)
library(separ)#permet de scinder ma data en quanti, quali et binaire.
library(compar)
palette_couleurs <- brewer.pal(12, "Set3")
data <- read.csv("https://www.dropbox.com/scl/fi/d3v41yp6x9cxlqvueoet3/membersClean.csv?rlkey=v9xfdgu6oyubjur9rlu6k9eow&dl=1")
names_data_quali <- scinde(data,"quali")
names_data_quanti <- scinde(data,"quanti")
names_data_binaire <- scinde(data,"binaire")
data_quanti <- data[,names_data_quanti]
data_quali <- data[,names_data_quali]
data_binaire <- data[,names_data_binaire]

ui <- fluidPage(
    navbarPage(theme = shinytheme("flatly"),
      "M&N",
      tabPanel("DATA",
               #sidebarPanel(),
               mainPanel(
                            h1("Expeditions_Himalayan"),
                            reactableOutput('dtFinal_data'), 
                            downloadButton('save_data','save to csv')
               ) # mainPanel    
      ),
      navbarMenu("Analyse",
                #tabpanel 1
                tabPanel("Résumés statistiques",
                        sidebarPanel("Informations requises",
                                    selectInput("var1", " Choisissez une variable", choices = names(data)),
                                    conditionalPanel(
                                      condition = "input.var1 == 'age' || input.var1 == 'year'",
                                      radioButtons("bool1","Souhaitez vous regarder une partie de la population?",choices=c('non','oui')),
                                      conditionalPanel(
                                        condition = "input.bool1 == 'oui'",
                                        selectInput("var_quali","Variable à discriminer:",choices = names(data_quali)),
                                        selectInput("cat1","Quel partie de la population souhaitez vous regarder?",choices=NULL)
                                      )
                                    ),
                                    actionButton("run","run")
                        ),
                        mainPanel(h1("Résumé statistiques"),
                                  verbatimTextOutput("summary")
                        )
                        
                ),
       

                #tabpanel2
                tabPanel("Graphique quantitatifs",
                        sidebarPanel("Informations requises",
          radioButtons("type_graph","Choix du style de graphiques:",choices = c("classique","ggplot")),

          selectInput("var2", " Choisissez une variable:", choices = names(data_quanti)),
          selectInput('plot_type', 'Choisissez le type de graphique:', choices = c('Histogram', 'scatterplot','Density','boxplot')),
          conditionalPanel(
            condition = "input.plot_type == 'Histogram'",
            sliderInput(inputId = "Classes", label = "Nombre de classes:", min = 2, max = 50, value = 8, step = 2)
          ),
          conditionalPanel(
            condition = "input.plot_type == 'boxplot'",
            sliderInput(inputId = "Classes2", label = "Nombre de classes:", min = 1, max = 10, value = 2, step = 1),
            radioButtons("bool4","Voulez vous que la taille des box dépende de l'effectif?",choices = c("oui","non"))
          ),
          conditionalPanel(
            condition = "input.plot_type == 'scatterplot'",
            selectInput("var3", "Choisissez la seconde variable", choices = names(data_quanti)),
            radioButtons("bool2", "Souhaitez vous regarder une partie de la population?", choices = c('non', 'oui')),
            conditionalPanel(
              condition = "input.bool2 == 'oui'",
              selectInput("var_binaire", "Variable à discriminer:", choices = names(data_binaire))
            )
          ),
          conditionalPanel(
            condition = "input.plot_type == 'Density'",
            selectInput("var_quali2","Variable à discriminer:",choices = c("Aucune",names(data_quali)))
          ),
          actionButton("run2", "run")
        ),
        mainPanel(
          plotOutput("plot_quanti")
        )
                  ),
                  #tabpanel3
                  tabPanel("Graphique qualitatifs",
                          sidebarPanel("Informations requises",
                         radioButtons("type_graph","Choix du style de graphiques:",choices = c("classique","ggplot")),
          selectInput("var4", " Choisissez une variable", choices = names(data_quali)),
          selectInput('plot_type_quali', 'Choisissez le type de graphique:', choices = c('barplot', 'mosaicplot')),
          conditionalPanel(
            condition = "input.plot_type_quali == 'mosaicplot'",
            selectInput("var5", "Choisissez la 2ème variable", choices = names(data_quali))
          ),
          conditionalPanel(
            condition = "input.plot_type_quali == 'barplot'",
            radioButtons("bool3","Voulez vous que les barres soient triées?",choices = c("non","oui"))
          ),
          actionButton("run3", "run")
        ),
        mainPanel(
          plotOutput("plot_quali")
        )
                  ),#fin du tabpanel

                  #tabpanel4
                  tabPanel("Graphique quanti vs quali",
                          sidebarPanel("Informations requises",
                          radioButtons("type_graph","Choix du style de graphiques:",choices = c("classique","ggplot")),

          selectInput("var6", "Choisissez une variable quantitative:", choices = names(data_quanti)),
          selectInput("var7", "Choisissez une variable qualitative:", choices = names(data_quali)),
          selectInput('plot_type_quali_quanti', 'Choisissez le type de graphique:', choices = c('barplot', 'boxplot', 'scatterplot')),
          conditionalPanel(
            condition = "input.plot_type_quali_quanti == 'scatterplot'",
            selectInput("var8", "Choisissez une deuxième variable quantitative:", choices = names(data_quanti)),
            selectInput("cat2", "Sur quel modalité voulez vous discriminer?", choices = NULL)
          ),
          actionButton("run4", "run")
        ),
        mainPanel(
          plotOutput("plot_quali_quanti")
        )
                  )#fin du tabpanel

      ), # navbarMenu
      navbarMenu("Predictions",
                  tabPanel("KNNPredictions")
      )
    )#NavBarPage
  ) # fluidPage



server <- function(input, output,session) {
  
  observe({
    var_qualitative <- input$var_quali
    var_qualitative2 <- input$var7
    modalites <- levels(as.factor(data_quali[,var_qualitative]))
    modalites2 <- c("toute la population",levels(as.factor(data_quali[,var_qualitative2])))
    # Mettre à jour les choix des selectInput
    updateSelectInput(session, "cat1", choices = modalites)
    updateSelectInput(session, "cat2", choices = modalites2)
    
    if(input$plot_type_quali_quanti == "barplot"){
        updateSelectInput(session,"var6",choices = names(data),label = "Choisissez une variable:")
    }
    else{
        updateSelectInput(session,"var6",choices = names(data_quanti),label = "Choisissez une variable quantitative:")
    }


  })

  #theme du jeu de donnée
    theme_dark <-reactableTheme(
      color = "hsl(233, 9%, 87%)",
      backgroundColor = "hsl(233, 9%, 19%)",
      borderColor = "hsl(233, 9%, 22%)",
      stripedColor = "hsl(233, 12%, 22%)",
      highlightColor = "hsl(233, 12%, 24%)",
      inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)"))


    df <- reactive({
      data
      })

    output$dtFinal_data <- renderReactable({
        reactable(df(),
        columns = list(success = colDef(align = "center", style = "color: red;", filterable = FALSE)),
        fullWidth = TRUE,defaultColDef = colDef(style = "font-style: italic;"),searchable = TRUE,
        filterable = TRUE,highlight = TRUE, showPageSizeOptions = TRUE,defaultPageSize = 10,pageSizeOptions = c(10, 50, 100),theme = theme_dark)
    })


        #################################Résumés statistiques###########################
        resume <- eventReactive(input$run, {
            v1 <- input$var1
            x1 <- data[,v1]
            if(typeof(x1) == "integer"){##cas ou c'est un variable quanti
              if(input$bool1 == "oui"){##on regarde une sous partie
                data_filtre <- data[data[,input$var_quali] == input$cat1, ]
                n_observations <- length(data_filtre[,v1])
                frequency <- n_observations / length(x1)#/73000 normalement
                pop <- round(frequency*100,2)
                # Création du résumé personnalisé
                custom_summary <- summary(data_filtre[,v1])
                custom_summary <- c(custom_summary, N_Observations = n_observations, Pourcentage_population = pop)
                round(custom_summary,2)
              }
              else #on regarde la variable
                 summary(data[,input$var1])
            }
            else{#cas d'une variable qualitative#
              effectifs <- table(x1)
              frequence <- round(effectifs/length(x1)*100,2)
              frequence_cumulés <- cumsum(frequence)
              table_data <- data.frame(Effectif = as.vector(effectifs), Frequence = as.vector(frequence), Frequence_Cumulees= frequence_cumulés)
              table_data
            }
        }) #ignoreNull=false, permet d'afficher sans cliquer sur run



    output$summary <- renderPrint({
        resume()
    })

    #################################PARTIE QUANTITATIF###########################

    plot_quanti_print <- eventReactive(input$run2,{
      #######Histogram######
      if(input$plot_type == "Histogram"){
         histogramme(data,input$var2,input$Classes,"deeppink",input$type_graph)
      }
      #######Densite######
      else if (input$plot_type == "Density"){
          modalite <- NULL
          if(input$var_quali2!="Aucune")
            modalite <- input$var_quali2
          densite(data = data,variable = input$var2,modalite=modalite ,type = input$type_graph)
      }          
      ######boxplot######
      else if (input$plot_type == "boxplot"){
          width <- ifelse(input$bool4 == 'oui',TRUE,FALSE)
          boitemoustache(data = data,variable = input$var2,nbClasses = input$Classes2,varwidth = width,type = input$type_graph,color = palette_couleurs[1:input$Classes2])#gestion des couleurs
      }
        ##scatterplot ##
      else {
            modalite <- NULL
            if(input$bool2 == "oui")
              modalite <- input$var_binaire
            scatterplot(data,input$var2,input$var3,input$type_graph,modalite = modalite)
            
              #moda <- minmod(data,input$var_binaire)    ##a revoir##
              #p2 <- ggplot(data, aes(x = x1, y = x2)) + 
               #     geom_point() + 
                #    geom_point(
                 #     data = data |> filter(sex == "F"), 
                  #    color = "red"
                   # ) +
                    #geom_point(
                     # data = data |> filter(sex == "F"), 
                      #shape = "circle open", size = 3, color = "red"
                    #) 
      }
      
    })


    output$plot_quanti <- renderPlot({
       plot_quanti_print()
    })

    
    



 #################################PARTIE QUALITATIF#############################
    plot_quali_print <- eventReactive(input$run3,{
      y <- data[, input$var4]
      ##barplot##
      if(input$plot_type_quali == "barplot"){
        diagbatons(data, input$var4,ifelse(input$bool3=="non",FALSE,TRUE),color = palette_couleurs,type = input$type_graph)
      }
          ##mosaicplot##
      else
        mosaicplot(table(data[, input$var4], data[, input$var5]),col = palette_couleurs, xlab = input$var4,ylab = input$var5,main = paste("Mosaicplot de ",input$var4,"en fonction de ",input$var5),las = 2)
    })
    output$plot_quali <- renderPlot({
       plot_quali_print()
    })


  
 #################################PARTIE QUANTITATIF vs QUALITATIF##########################
   

    plot_quali_quanti_print <- eventReactive(input$run4,{
        x <- data[,input$var6]
        y <- data_quali[,input$var7] 
        
           ##boxplot##
        if(input$plot_type_quali_quanti == "boxplot"){
          boitemoustache(data,input$var6,type = input$type_graph,color = palette_couleurs,modalite = input$var7)
        }
          ##barplot##
        else if (input$plot_type_quali_quanti == "barplot") {
            diagbatons(data,input$var7,color = palette_couleurs,type = input$type_graph,variable = input$var6)
        }
        ##scatterplot##
        else{
          if(input$type_graph=="classique"){
            if(input$cat2 == "toute la population"){
              mod <- levels(as.factor(y))
              taille_grille <- floor(sqrt(length(mod)))+1
              par(mfrow = c(taille_grille,taille_grille))
              for(i in 1:length(mod)){
                data_filtre <- data[y == mod[i],]
                plot(data_filtre[,input$var6],data_filtre[,input$var8],xlab = input$var6, ylab = input$var8,pch=20,main=mod[i])
              }
            }
            else{#on regarde un sous ensemble 
                data_filtre <- data[y == input$cat2,]
                plot(data_filtre[,input$var6],data_filtre[,input$var8],xlab = input$var6, ylab = input$var8,pch=20,main=input$cat2)
            }
          }
          else{##cas ggplot
            if(input$cat2 == "toute la population"){
              ggplot(data, aes(x = x, y = data[,input$var8])) + 
              geom_point() + 
              facet_wrap(~data[,input$var7])
            }
            else {#on regarde un sous ensemble
              data_filtre <- data[y == input$cat2,]
              ggplot(data_filtre,aes(x = data_filtre[,input$var6], y = data_filtre[,input$var8])) +
              geom_point() +
              labs(
                 x = input$var6,
                 y = input$var8,
                 title = input$var7,
                 caption = "Data from Himalayan Expeditions"
              )
            }
          }
        }
    })

    output$plot_quali_quanti <- renderPlot({
      plot_quali_quanti_print()
    })

   
 


    #sauvegarde de df au format csv
    output$save_data <- downloadHandler(
        filename <- function(){
            paste("data",Sys.Date(), ".csv", sep = ',')
        },
        content <- function(file){
            write.csv(data,file)
        }
    )
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)



