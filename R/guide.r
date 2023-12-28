library(cicerone)
guide <- Cicerone$
  new(allow_close = FALSE)$
  step(
    "title",
    "titre du jeu de données utilisé"
  )$
  step(
    "dtFinal_data",
    "Le jeu de donnée",
    "Vous pouvez faire des recherches, trié selon une variable en particulier."
  )$
  step(
    "save_data",
    "Sauvegarder le jeu de données",
    "Cliquez ici pour sauvegarder le jeu de données en format csv."
  )
  
  guide1 <-Cicerone$
  new(allow_close = FALSE)$
   step(
    "run",
    "Bouton d'execution:",
    "Cliquez ici pour lancer le résumé de la variable choisi."
  )$
  step(
    "summary",
    "Resumé statistique",
    "Vous trouverez ici le résumé statistique."
  )

   guide2 <-Cicerone$
  new(allow_close = FALSE)$
   step(
    "run2",
    "Bouton d'execution:",
    "Cliquez ici pour lancer le graphique selon vos choix."
  )$
  step(
    "type_graph",
    "Style graphique",
    "Ici vous pouvez choisir le style du graphique."
  )$
  step(
    "plot_quanti",
    "Graphique",
    "Affichage du graphique."
  )
 

 guide3 <-Cicerone$
  new(allow_close = FALSE)$
   step(
    "run3",
    "Bouton d'execution:",
    "Cliquez ici pour lancer le graphique selon vos choix."
  )$
  step(
    "type_graph",
    "Style graphique",
    "Ici vous pouvez choisir le style du graphique."
  )$
 step(
    "plot_quali",
    "Graphique",
    "Affichage du graphique."
  )

  guide4 <-Cicerone$
  new(allow_close = FALSE)$
   step(
    "run4",
    "Bouton d'execution:",
    "Cliquez ici pour lancer le graphique selon vos choix."
  )$
  step(
    "type_graph",
    "Style graphique",
    "Ici vous pouvez choisir le style du graphique."
  )$
 step(
    "plot_quali_quanti",
    "Graphique",
    "Affichage du graphique."
  )

  guide5 <-Cicerone$
  new(allow_close = FALSE)$
    step(
      "title3",
      "Informations sur vous:",
      "Renseignez vos informations personnelles permettant de prédire votre succès."
  )$
   step(
    "run5",
    "Bouton d'execution:",
    "Cliquez ici pour lancer la prédiction."
  )$
  step(
    "predknn",
    "Prédiction Knn",
    "Ici se trouve la prédiction faite par le modèle knn."
  )