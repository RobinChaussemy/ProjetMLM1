using MLJ,DataFrames,Plots
iris = load_iris();
selectrows(iris, 1:3)  |> pretty
schema(iris)
iris = DataFrames.DataFrame(iris);
y, X = unpack(iris, ==(:target); rng=123);
first(X, 3) |> pretty
using DecisionTree
Tree = @load DecisionTreeClassifier pkg=DecisionTree
tree=Tree()
evaluate(tree, X, y,resampling=CV(shuffle=true),measures=[log_loss, accuracy],verbosity=0)
mach = machine(tree, X, y)
train, test = partition(eachindex(y), 0.7);
fit!(mach, rows=train);
yhat = predict(mach, X[test,:]);

import RDatasets
channing = RDatasets.dataset("boot", "channing")
schema(channing)
y, X =  unpack(channing, ==(:Exit),!=(:Time), rng=123);
schema(X)
X = coerce(X,:Entry=>Continuous, :Cens=>Multiclass)
y=coerce(y,Continuous)


#trouver le model dispo pour notre jeu de données
X, y = @load_boston
ms = models(matching(X, y)) # ce ne sera que des modèles compatible avec des variables quantitatives!
#pour voir les models non supervisés:
models(matching(X))
#avoir des infos sur un model qui m'intéresse
info("KNNRegressor") 
KNN=@load KNNRegressor
knn=KNN()
evaluate(knn, X, y,
         resampling=CV(nfolds=5),
         measure=[RootMeanSquaredError() , LPLoss(1)])

#avec de la cross validation
crabs = load_crabs() |> DataFrames.DataFrame
schema(crabs)
#on prend comme targer sp et comme x tout sauf index,sex
y, X = unpack(crabs, ==(:sp), !in([:index, :sex]); rng=123)
tree=Tree()
mach=machine(tree,X,y)
#cv avec 5 nfolds
evaluate!(mach, resampling=CV(nfolds=5, shuffle=true, rng=1234),
          measure=[LogLoss(), Accuracy()])
#on change les hyperparametres pour voir 
tree.max_depth=5
s=evaluate!(mach, resampling=CV(nfolds=5, shuffle=true, rng=1234),
          measure=[LogLoss(), Accuracy()])
acc=s.measurement[2]
#on boucle sur une dizaine de max pour voir ce qui est le mieux
n=0
acc=0
for i in 3:2:11
    tree.max_depth=i
    s=evaluate!(mach, resampling=CV(nfolds=5, shuffle=true, rng=1234),
          measure=[LogLoss(), Accuracy()])
    if(s.measurement[2]>acc)
        acc=s.measurement[2]
        n=i
    end
end
n#n a prend pour max.depth
acc #accuracy pour ce n

#optimisation avec les gridsearch
forest = EnsembleModel(model=tree, n=300)
r1 = range(forest, :(model.n_subfeatures), lower=1, upper=3)
r2 = range(forest, :(model.max_depth),lower=3,upper=9) 
tuned_forest = TunedModel(model=forest,
                          tuning=Grid(resolution=12),
                          resampling=CV(nfolds=6),
                          ranges=[r1, r2],
                          measure=Accuracy())        
                          
mach = machine(tuned_forest, X, y) |> fit!
F = fitted_params(mach)
best_model=F.best_model

#graphique de représentation des meilleurs paramètres
plot(mach)
r = range(forest, :n, lower=1, upper=100, scale=:log10)

curve = learning_curve(mach,
                       range=r,
                       resampling=Holdout(),
                       resolution=50,
                       measure=Accuracy(),
                       verbosity=0)

                      
plot(curve.parameter_values, curve.measurements, xlab=curve.parameter_name, xscale=curve.parameter_scale)



        
