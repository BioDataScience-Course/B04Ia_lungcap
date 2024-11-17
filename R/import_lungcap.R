# Jeu de données `lungcap`
#
# description et métadonnées :
#   https://rdrr.io/cran/GLMsData/man/lungcap.html
# données :
#   Les données proviennent du package {GLMsData} installé par défaut dans la
#   SciViews Box 2024. Cependant, l'importation ayant déjà été réalisée pour
#   vous, vous ne devez pas exécuter ce script et vous n'avez rien à faire pour
#   avoir accès aux données remaniées qui sont dans le fichier
#   `data/lungcap.rds`.

# Configuration de l'environnement
SciViews::R("model", lang = "fr")

# Étape 1 : importation des données ---------------------------------------

lungcap <- read("lungcap", package = "GLMsData")
attr(lungcap, "spec") <- NULL
attr(lungcap, "problems") <- NULL
# Aide en ligne
#?GLMsData::lungcap


# Étape 2 : prétraitement des données -------------------------------------

# Transformation de Smoke en variable facteur
lungcap$Smoke <- factor(lungcap$Smoke,
  levels = c(0, 1), labels = c("non", "oui"))
# Idem pour Gender
lungcap$Gender <- factor(lungcap$Gender,
  levels = c("F", "M"), labels = c("F", "H"))
# Ht est en pouces -> convertissons en cm
lungcap$Ht <- lungcap$Ht * 2.54


# Étape 3 : ajout des labels et des unités --------------------------------

lungcap <- labelise(lungcap,
  label = list(
    Age    = "Âge",
    FEV    = "Volume respiratoire",
    Ht     = "Taille",
    Gender = "Genre",
    Smoke  = "Fume"
  ), units = list(
    Age    = "années",
    FEV    = "L",
    Ht     = "cm"
  )
)


# Étape 4 : sauvegarde des données ----------------------------------------

# Sauvegarder la version finale du jeu de données et nettoyer l'environnement
write$rds(lungcap, "data/lungcap.rds")
rm(lungcap)

