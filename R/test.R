####################################
# Data Professor                   #
# http://youtube.com/dataprofessor #
# http://github.com/dataprofessor  #
####################################

# Modified from Winston Chang, 
# https://shiny.rstudio.com/gallery/shiny-theme-selector.html

# Concepts about Reactive programming used by Shiny, 
# https://shiny.rstudio.com/articles/reactivity-overview.html

# Load R packages
library(shiny)
library(shinythemes)


  # Define UI
  ui <- fluidPage(theme = shinytheme("cerulean"),
    navbarPage(
      # theme = "cerulean",  # <--- To use a theme, uncomment this
      "M&N",
      tabPanel("IMC",
               sidebarPanel(
                 tags$h3("Informations requises:"),
                 textInput("txt1", "Nom:", ""),
                 textInput("txt2", "Prénom:", ""),
                 conditionalPanel(
                  condition = "input.txt1 != '' && input.txt2 != ''",
                  numericInput("var","Quel est ton âge?",value=20,min=1,max=100),
                  numericInput("var2","Quel est ta taille? (en cm)",value=150,min=0,max=200),
                  numericInput("var3", "Quel est votre poids actuel? (en kg)",value=60,min=0,max=150),
                  actionButton("run","run")
                 )
                 
                 

               ), # sidebarPanel
               mainPanel(
                            h1("Votre Poids idéal"),
                            
                            h4("IMC Formula"),
                            verbatimTextOutput("poids"),
                            conditionalPanel(
                              condition = "input.var3/(input.var2*input.var2/10000) > 30",
                              verbatimTextOutput("injuregratuite")
                            )

               ) # mainPanel
               
      ), # Navbar 1, tabPanel
      tabPanel("Navbar 2", "This panel is intentionally left blank"),
      tabPanel("Navbar 3", "This panel is intentionally left blank")
  
    ) # navbarPage
  ) # fluidPage

  
  # Define server function  
  server <- function(input, output) {
    
      poids_ideal <- reactive({ 
        round((input$var2-100+input$var/10)*0.9,1)
    })

      diff <- reactive({
        input$var3-poids_ideal()
      })
    
    imc <- reactive({
      round(input$var3/(input$var2*input$var2/10000),0)
    })
    
    
    output$poids <- renderText({
      input$run
      isolate({
        paste("Ton poids idéal est: ",poids_ideal(),"kg")
      })
    })

    output$injuregratuite <- renderText({
      if (input$run>0) { #permet de ne s'afficher que lorsque le bouton est cliqué
      isolate(paste("Tu es à ",diff(),"kg de votre poids idéal, sors toi les doigts du uc","\n",
        "IMC=",imc()))
      }
    })
  } # server
  

  # Create Shiny object
  shinyApp(ui = ui, server = server)

classes<-c(0,20,30,40,50,60,70,80,90)
agecut <- cut(age,classes,include.lowest = TRUE)
hist(agecut, pch = ifelse(sex == "M", 1, 2), col = ifelse(sex == "M", "blue", "red"),
     xlab = "Âge", ylab = "Sexe", main = "Scatter Plot Âge vs Sexe")

# Ajoutez une légende
legend("topright", legend = levels(sex), pch = 1:2, col = c("blue", "red"))
#regarder le tp de rachdi pour faire ça!!!

##ajout  visualisation quantitative selon une modalité quali: exemple l'age des français.
couleurs <- ifelse(sex == "M", "blue","red")
plot(year,age, 
     xlab = "Saison", ylab = "Ozone",
     main = "Ozone en fonction des saisons",
     col = couleurs)

legend("topright", legend = levels(as.factor(saison)), 
       pch = c(1, 2, 3), col = c("red", "green", "blue"),
       title = "Saison", cex = 0.8)



data_filtre <- data[data[,"expedition_role"] == "Leader", ]
n_observations <- length(data_filtre[,"age"])

# Calcul de la fréquence par rapport à la population totale 
frequency <- n_observations / length(data[,"age"])
pop <- round(frequency*100,2)
# Création du résumé personnalisé
custom_summary <- summary(data_filtre[,"age"])
custom_summary <- c(custom_summary, N_Observations = n_observations, Pourcentage_population = pop)
round(custom_summary,2)
