library(PredictionJulia)
library(PredictionPython)
library(jl4R)
setwd(paste0(getwd(),"/R"))
exemple_julia_knn <-function(){
  
  return(KNN_Julia(
    "Ama Dablam",
    "Autumn",
    "France",
    "Climber",
    2025,
    1,
    25,
    0,
    0,
    1,
    mort = 0,
    blesse = 0,
    nvoisin = 5,
    norme = 2
  ))
}

exemple_julia_forest <-function(){
  
  return(RandomForest_Julia(
    "Ama Dablam",
    "Autumn",
    "France",
    "Climber",
    2025,
    1,
    25,
    0,
    0,
    1,
    mort = 0,
    blesse = 0,
    ntree = 100
  ))
}

exemple_python_knn <- function(){
  return(KNN_Python(c("Ama Dablam","Autumn","France","Climber",2025,1,25,0,0,1,0,0), n = 1000 , p = 2))
}

exemple_python_forest <- function(){
  return(RandomForest_Python(c("Ama Dablam","Autumn","France","Climber",2025,1,25,0,0,1,0,0), n = 100))
}
