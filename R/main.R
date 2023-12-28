#install.packages("reticulate")
library(reticulate)

Int <- function(x){
  as.integer(x)
}

use_virtualenv("../Python/venv")
source_python("../Python/main.py")
my_indiv = c("Ama Dablam","Autumn","France","Climber",2025,1,1,1,1,1,1,1)

KNN_Process(data,my_indiv,target,n = Int(3), p = Int(1))
Tree_Process(data,my_indiv,Int(2))

pred = KNN_Python(my_indiv)

library(jl4R)
jl('using Pkg;Pkg.add(url = "https://github.com/RobinChaussemy/JuliaPrediction.jl.git")')
jl("using JuliaPrediction")
jl("using JuliaPrediction.get_indiv()")
ind <- jl("JuliaPrediction.KNN_Process(JuliaPrediction.get_data(),JuliaPrediction.get_indiv())")
