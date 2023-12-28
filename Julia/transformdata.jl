using StatsPlots, Interact,DataFrames,CSV
using Blink
#w = Window()

using Downloads
dataMort=CSV.File(Downloads.download(https://www.kaggle.com/datasets/raskoshik/himalayan-expeditions?select=deaths.csv))
#body!(w, dataviewer(data))
#=
for i in 1:length(data.cause_of_death) #je remplace toute les modalités par leurs 3 premieres lettres
    data.cause_of_death[i]=SubString(data.cause_of_death[i],1:3)
end
replace!(data.cause_of_death, "Fal" => "Chute")
replace!(data.cause_of_death, "Ava" => "Avalanche")
replace!(data.cause_of_death, "AMS" => "MaldesMontagnes")

for i in 1:length(data.cause_of_death)
    if(data.cause_of_death[i] ∉ ["Chute","Avalanche","MaldesMontagnes"])
        data.cause_of_death[i]="Autres"
    end
end
=# 

print(subset(data, :age => a -> a .> 70)) #faire des subsets
using StatsBase
dicAge=(StatsBase.countmap(data.age))
ks=keys(dicAge)
0 ∈ ks

ksM=keys(dicMort)

#methode pour remplacer toutes les modalités de cause_of_death par 4
map_to_Int(s) = SubString(s,1:3) == "AMS" ? "MAM" : SubString(s,1:3) == "Fal" ? "Chute" : SubString(s,1:3) == "Ava" ? "Avalanche" : SubString(s,1:3) == "Exh" ? "Epuisement" 
: SubString(s,1:3) == "Dis" ? "Disparition" : "Autres"
transform!(data, :cause_of_death => ByRow(map_to_Int) => :cause_of_death)


dicMort=(StatsBase.countmap(data.cause_of_death))
data=CSV.read("./ProjetsLogSpe/DataSets/expeditions.csv",DataFrame)
dropmissing!(data,:members)


occursin.("(D)",data[:,"members"])
#aller lire doc regex
vecMembers = split.(data.members,',')
members = vcat(vecMembers...)
morts = members[endswith.(members,"(D)")] #liste des morts 
dataCleanExp=data[!,[2,3,5,6,20,21,26,27,29,30,36,38,46,49,50,52,57]]
dropmissing!(dataCleanExp,:members)
CSV.write("ProjetsLogSpe/DataSets/expeditionsClean.csv",dataCleanExp)

###########Changement de jeu de données################

using CSV,Downloads,DataFrames
dt=CSV.File(Downloads.download("https://www.dropbox.com/scl/fi/cpvcr9svg72vg921xc4ih/members.csv?rlkey=kf76mywll5rfp860rhrmass93&dl=0")) |> DataFrame
using Statistics
#j'enlève les valeurs manquantes dans age
dt=dt[dt.age.!="NA",:]
#je transofrme la variable age en vecteur d'entier
dt.age=parse.(Int64, dt.age)

map_to_String(s) = length(s) == 2 ? "Autres" : SubString(s,1:3) == "Ama" ? "Ama Dablam" : SubString(s,1:3) == "Sha" ? "Sharphu" : SubString(s,1:3) == "Ann" ? "Annapurna" : SubString(s,1:3) == "Dha" ? "Dhaulagiri" : SubString(s,1:3) == "Mak" ? "Makalu" : SubString(s,1:3) == "Gan" ? "Ganesh" : SubString(s,1:3) == "Eve" ? "Everest" : SubString(s,1:3) == "Kan" ? "Kangchen" : SubString(s,1:3) == "Pum" ? "Pumori" : SubString(s,1:3) == "Bar" ? "Baruntse" : SubString(s,1:3) == "Lho" ? "Lhotse" : SubString(s,1:3) == "Man" ? "Manaslu" : SubString(s,1:3) == "Cho" ? "Cho" : "Autres"
transform!(dt, :peak_name => ByRow(map_to_String) => :peak_name)
dtClean=dt[!,[4,5,6,7,8,9,10,11,13,14,15,16,19]]




function drop!(dt::DataFrame,g::Function) #permet de drop toute les lignes qui ne satisfassent pas g
    #selectionne les colonnes type string
    taille=1:length(dt[1,:])
    for j=taille
        if dt[1,j] isa AbstractString
            #verifier la condition sur chaque ligne de cette colonne pour drop avec la fonction g
            filter!(row -> !g(row[j]),dt)
        end
    end
end
#v2
g(x)= x isa AbstractString && x=="NA"
filter(r -> all(!g,r),dtClean) #je filtre par ligne r , et je garde tout celles qui ne sastifassent pas la condition
dtClean=filter(r -> all(!g,r),dtClean) 
###expedition_role###
##on visualise la table de contingence
tab=combine(groupby(dt, :expedition_role), nrow=> :Freq)
##on regarde les 10 premieres modalités avec le plus d'occurences
sort(tab[!,[2,1]],rev=true)[1:10,:]
## on ne garde que les modalites les plus pertinentes et on range les autres dans Autres.
map_to_String(s) = length(s) == 2 ? "Autres" :  (occursin("Leader",s) || occursin("leader",s)) ? "Leader" : SubString(s,1:3) == "Cli" ? "Climber" : SubString(s,1:2) == "BC" ? "BaseCampStaff" : (occursin("Tv",s) || occursin("Film",s)) ? "Movie/TV_team" : "Autres"

dtFinal=transform(dtClean, :expedition_role => ByRow(map_to_String) => :expedition_role)

#reste à choisir pour "citizenship" si on garde les doubles nationalités ou non


tab=combine(groupby(dtFinal, :citizenship), nrow=> :Freq)
CSV.write("DataSets/membersClean.csv",dtFinal)
tab=sort(tab[!,[2,1]])
tab[tab[!,1].>1000,1]

#nettoyyage de la variable citizenshipp###
#on split pour enlever les doubles nationalités
vec=split.(dtFinal.citizenship,'/')
for i in 1:length(vec)
    if length(vec[i])>1
        vec[i]=[vec[i][1]]
        end
end
vecF=[]
#on met dans un vecteur toute les nationalités
for i in 1:length(vec)
    push!(vecF,vec[i][1])
end

using CategoricalArrays,FreqTables

#on regroupe tout les allemands ensemble (on est plus en 50)

vecF[occursin.("Germany",vecF)] .= "Germany"

dtFinal.citizenship = vecF
nom=names(tab[tab.<=300])
nom=nom[1]
#on regroupe toute les nationalités présentes moins de 300 fois dans une variable autre
for i in 1: length(nom)
    vecF[occursin.(nom[i],vecF)].= "Autres"
end

tab=sort(freqtable(vecF))

#on met notre vecteur dans notre variable citizenship
dtFinal.citizenship = vecF

#on recupère notre jeu de donnée nettoyé
CSV.write("DataSets/membersClean.csv",dtFinal)
