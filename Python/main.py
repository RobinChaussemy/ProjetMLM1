import pandas as pd
import numpy as np
from sklearn.neighbors import KNeighborsClassifier
from sklearn.preprocessing import OneHotEncoder
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestClassifier
import time as tm
import warnings
warnings.filterwarnings("ignore")

data = pd.read_csv("../DataSets/membersClean.csv")
target = data.success
print("Load success")

def EncodeData(data,indiv):
    boolean = ["hired", "solo", "oxygen_used", "injured", "success", "died"]
    data[boolean] = data[boolean].replace(False, 0)
    data[boolean] = data[boolean].replace(True, 1)
    data["sex"] = data["sex"].replace("F", 1).replace("M", 0)
    y_dict = {
        "peak_name" : [indiv[0]],
        "season" : [indiv[1]],
        "citizenship" : [indiv[2]],
        "expedition_role" : [indiv[3]],
        "year": [indiv[4]],
        "sex": [indiv[5]],
        "age": [indiv[6]],
        "hired": [indiv[7]],
        "solo": [indiv[8]],
        "oxygen_used": [indiv[9]],
        "died": [indiv[10]],
        "injured": [indiv[11]]
    }
    ydf = pd.DataFrame.from_dict(y_dict)
    ydf[boolean] = data[boolean].replace("false", 0)
    ydf[boolean] = data[boolean].replace("true", 1)
    ydf["sex"] = data["sex"].replace("F", 1).replace("M", 0)
    y = data.success
    Y_encoded = []
    categorical_variables = ["peak_name", "season", "citizenship", "expedition_role"]
    quantitative_variables = ["year","sex", "age", "hired","solo", "oxygen_used", "died","injured"]

    labels = ["year","sex", "age", "hired", "solo", "oxygen_used", "injured", "died"]

    enc = OneHotEncoder()
    X_cat_hot = enc.fit_transform(data[categorical_variables])

    Y_cat_hot = enc.transform(ydf[categorical_variables])

    X_encoded = np.concatenate([X_cat_hot.toarray(), data[quantitative_variables]], axis=1)
    Y_encoded = np.concatenate([Y_cat_hot.toarray(), ydf[quantitative_variables]], axis=1)

    return [X_encoded,Y_encoded]

def PredictKNN(indiv,dataencode,y,p = 3,n = 11):
    #X_train, X_test, y_train, y_test = train_test_split(dataencode, y, test_size=0.3, random_state=0)
    KNN = KNeighborsClassifier(n_neighbors = n, p = p)
    KNN.fit(dataencode,y)
    res_predi = KNN.predict(indiv)
    prob_predi = KNN.predict_proba(indiv)
    return [res_predi[0],prob_predi[0][0],prob_predi[0][1]]
  
def PredictForest(data,X,indiv,n_arbre = 100):
    random_forest = RandomForestClassifier(criterion = "gini",n_estimators = n_arbre)
    target = data.success
    X_train, X_test, y_train, y_test = train_test_split(X, target, test_size=0.7)
    random_forest.fit(X_train,y_train)
    res_predi = random_forest.predict(indiv)
    prob_predi = random_forest.predict_proba(indiv)
    return [res_predi[0],prob_predi[0][0],prob_predi[0][1]]

def KNN_Process(data,indiv,y,p=3,n=11):
    t0 = tm.time()
    process = EncodeData(data, indiv)
    res_predict = PredictKNN(process[1], process[0], y,p,n)
    res_predict.append(tm.time()-t0)
    return res_predict

def Tree_Process(data,indiv,n=2):
    t0 = tm.time()
    process = EncodeData(data, indiv)
    res_predict = PredictForest(data,process[0], process[1],n)
    res_predict.append(tm.time() - t0)
    return res_predict
