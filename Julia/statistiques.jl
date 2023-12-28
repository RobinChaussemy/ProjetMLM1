using CSV,Downloads,DataFrames
dtFinal=CSV.File(Downloads.download("https://www.dropbox.com/scl/fi/d3v41yp6x9cxlqvueoet3/membersClean.csv?rlkey=v9xfdgu6oyubjur9rlu6k9eow&dl=0"),normalizenames = true) |> DataFrame



#partie stat
counts = combine(groupby(dtFinal, :peak_name), nrow)  #permet d'avoir le nombre de valeurs prises pour chaque modalités
counts=sort(counts,[order(:nrow)]) #je l'odronne pour que ce soit plus lisible
using Plots
bar(counts.peak_name, counts.nrow, xlabel="Modalité", ylabel="Nombre de valeurs", legend=true,color=3)



using StatsPlots
using DataFrames

df = copy(dtFinal)
@df df plot(:peak_name,:age)
@df df scatter(:age,:peak_name,title="My DataFrame Scatter Plot!")

#variable age
boxplot(df.age)
using StatsBase #pour utiliser les tables de frequence
countsAge = combine(groupby(dtFinal, :age), nrow)  #permet d'avoir le nombre de valeurs prises pour chaque modalités
classe_age = [0, 20,30,40,50,60,70,80,90,100]
using CategoricalArrays,FreqTables

df.age=cut(df.age,classe_age)
countsAge = combine(groupby(df, :age), nrow=> :Freq)#tab des frequences en dataframe
freqAge=freqtable(df,:age)#ou en vecteur
propp=round.(freqAge/length(df.age)*100,digits=2)
# Créez un graphique à barres des proportions
proportions = collect(propp)
bar(classe_age, proportions, xlabel="Catégorie d'âge", ylabel="% Proportion", legend=false)




using StatsBase #pour utiliser les tables de frequence
countsAge = combine(groupby(dtFinal, :age), nrow)  #permet d'avoir le nombre de valeurs prises pour chaque modalités
classe_age = [0, 20,30,40,50,60,70,80,90]
using CategoricalArrays,FreqTables

df.age=cut(df.age,classe_age)
countsAge = combine(groupby(df, :age), nrow=> :Freq)#tab des frequences en dataframe
freqAge=freqtable(df,:age)#ou en vecteur
propp=round.(freqAge/length(df.age)*100,digits=2)
# Créez un graphique à barres des proportions
proportions = collect(propp)
bar(classe_age, proportions, xlabel="Catégorie d'âge", ylabel="% Proportion", legend=false)

#variable year
countsYear = combine(groupby(df, :year), nrow=> :Freq)#tab des frequences en dataframe
scatter(countsYear.year,countsYear.Freq,label="Year")

xlabel!("annee")
ylabel!("count")


gr()
@df df scatter(
    :age,
    :year,
    group = :died,
    m = (0.5, [:+ :h :star7], 12),
    bg = RGB(0.2, 0.2, 0.2)
)


freqtable(df,:year,:died)
# aevc des classes pour year
df.year=cut(df.year,1900:20:2020)
freqtable(df,:year)
freqtable(df,:year,:died)


#visualiser des frequences plus facilement
histogram(dtFinal.age,bg_inside = "black",
title = "Frequency of age groups",label = "Age",xlabel = "age")

histogram(dtFinal.year, bg_inside = "black",title = "Frequency of year",label = "Age", xlabel = "age")
#on voit une explosion du nb d'exploration à partir des années 80 surement liés aux avancées technologiques




##avec PlotlyJS
using CSV,Downloads,DataFrames
dtFinal=CSV.File(Downloads.download("https://www.dropbox.com/scl/fi/d3v41yp6x9cxlqvueoet3/membersClean.csv?rlkey=v9xfdgu6oyubjur9rlu6k9eow&dl=0"),normalizenames = true) |> DataFrame

using PlotlyJS


#####histogram#####
plot(dtFinal,x=:age,kind="histogram")

#####barplot#####
tab(data ::DataFrame,variable ::Symbol)=combine(groupby(data, variable), nrow)
x= :peak_name
y= :sex
count =tab(dtFinal,x)
plot(dtFinal,x=count.peak_name,y= count.nrow,kind="bar",Layout(barmode="stack"))


#####scatterplot#####
##classique##
plot(dtFinal,x=:age,y=:year,kind="scatter",mode="markers")

##selon une variable qualitative##
plot(dtFinal,x=:age,y=:year,color=:sex,kind="scatter",mode="markers")

#sous population#
plot(
    dtFinal, x=:age, y=:year, facet_col=:season,
    kind="scatter", mode="markers",
    Layout(
        width=800, height=800,
        margin=attr(l=20,r=20,t=20,b=20),
        paper_bgcolor="LightSteelBlue"
    )
)




