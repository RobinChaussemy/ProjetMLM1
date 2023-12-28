library(tidymodels)

data <- read.csv("DataSets/membersClean.csv")

##prediction##
##objectif: predire success avec differents modeles##
##les modeles: randomforest, naive bayes,



set.seed(4595) #creer de l'aléatoire
data_split <- initial_split(data, strata = "success", prop = 0.75)

data_train <- training(data_split)
data_test  <- testing(data_split)

###foret aléatoire###

rf_defaults <- rand_forest(mode = "regression")
rf_defaults
preds <- names(data)[-9] #ce qui va nous aider à predire , notre data sans la variable à predire
rf_xy_fit <- 
  rf_defaults %>%
  set_engine("ranger") %>%
  fit_xy(
    x = data_train[, preds],
    y = data_train$success
  )
