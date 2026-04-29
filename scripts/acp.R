# Chargement des librairies
library(FactoMineR)
library(factoextra)
library(ggplot2)


# Importation des données normalisées
donnees_norm <- read.csv("donnees_normalisees.csv")


# Vérification rapide
summary(donnees_norm)
dim(donnees_norm)


# Réalisation de l’ACP
# Les données sont déjà centrées-réduites, donc scale = FALSE
res_acp <- prcomp(donnees_norm, scale = FALSE)

# Résumé des résultats
summary(res_acp)

# Extraction des valeurs propres (variances expliquées)
eig <- res_acp$sdev^2
eig
barplot(eig,
        main = "Valeurs propres (Variance expliquée par chaque axe)",
        col = "skyblue",
        xlab = "Composantes principales",
        ylab = "Variance")

# Ligne de tendance
lines(eig, type = "b", col = "red")

# Pourcentage de variance cumulée
var_expliquee <- eig / sum(eig) * 100
var_expliquee
cumsum(var_expliquee)

# Cercle des corrélations
fviz_pca_var(res_acp,
             col.var = "contrib",
             gradient.cols = c("blue", "orange", "red"),
             repel = TRUE,
             title = "Cercle des corrélations - ACP Verre")


# Contribution des variables à chaque axe
round(res_acp$rotation, 3)

# Projection des individus (plan factoriel)
# Chargement du fichier original pour récupérer les types
glass <- read.csv("data/glass.data", header = FALSE)
colnames(glass) <- c("Id","RI","Na","Mg","Al","Si","K","Ca","Ba","Fe","Type")
glass$Type <- as.factor(glass$Type)

fviz_pca_ind(res_acp,
             geom = "point",
             habillage = glass$Type,
             palette = "Dark2",
             addEllipses = TRUE,
             title = "Plan factoriel des individus (coloré par Type de verre)")

# Contributions cumulées des variables
fviz_contrib(res_acp, choice = "var", axes = 1, top = 10)
fviz_contrib(res_acp, choice = "var", axes = 2, top = 10)

# Export des coordonnées des individus (pour K-means)
coord_ind <- as.data.frame(res_acp$x)
write.csv(coord_ind, "coord_individus_acp.csv", row.names = FALSE)

# Export des coordonnées des variables (pour interprétation)
coord_var <- as.data.frame(res_acp$rotation)
write.csv(coord_var, "coord_variables_acp.csv", row.names = TRUE)

