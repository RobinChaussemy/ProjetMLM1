
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Projet Master 1 SSD UE Logiciels spécialisés R

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Vous trouverez sur cette page l'ensemble du projet consacré aux deux UE dirigés par Monsieur Remi Drouilhet.
En plus de l'application AnalyseR, vous trouverez les packages crées permettant la réalisation de l'interface  et le machine learning (`R/packagesR` et `Julia/data.jld2`) ainsi que l'ensemble des fichiers R et Julia dans des dossiers respctifs qui ont permis de construire ces différents packages, de réaliser le traitement de notre jeu de donnée ainsi que son analyse.  
Vpus trouverez également le rapport et la présentation dédiés au projet au format html (resp. `rapport.html`et `presentation_projet.html`) avec leurs fichiers respectifs. Le rapport étant dans un format spécifique, le dossier `_extensions`est également nécessaire pour son bon fonctionnement (format personnalisé depuis un templates déja existant: SOURCE https://github.com/juba/bookup-html).  
Pour récupérer l'ensemble du projet, cela nécessite de copier sur votre terminal la commande suivante:
``` {source, engine='bash'}
git clone https://github.com/yanismicha/ProjetMLM1.git 
```

# Application shiny AnalyseR
Pour lancer l'application, il vous faudra vous placer dans la page principale du projet (~ProjetMLM1) et executer les commandes suivante:
``` r
if(!require(shiny))
  install.packages("shiny")
require(shiny)
runApp("R/app.R)
```
Il se peut que les différents packages crées par nos soins ne s'installe pas correctement, si tel est le cas, ils sont disponible en format tar.gz dans le dossier **R/packagesR**.

# Machine Learning
Pour la partie de Machine Learning, elle fonctionne très bien de manière autonome mais n'est pas encore opérationnelle au sein de l'interface.
Pour vous permettre d' utiliser la partie python, il vous faut réaliser quelques opérations au sein du projet.

## Création d'un environnement virtuel
```{source, engine='bash'}
brew install python@3.11 # uniquement sous linux et mac
python3 -m venv Python/venv
source Python/venv/bin/activate
pip install -r Python/requirements.txt
```

## Installation du package Julia

]add https://github.com/RobinChaussemy/JuliaPredict.jl.git
