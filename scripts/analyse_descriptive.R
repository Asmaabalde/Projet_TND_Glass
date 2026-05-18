# Chargement du fichier
glass <- read.csv("data/glass.data", header=FALSE)

# Nommage des colonnes selon la doc technique
colnames(glass) <- c("Id", "RI", "Na", "Mg", "Al", "Si", "K", "Ca", "Ba", "Fe", "Type")

# Transformation du Type en "Facteur" (pour l'analyse qualitative)
glass$Type <- as.factor(glass$Type)

# Vérification immédiate 
print(paste("Nombre de valeurs manquantes :", sum(is.na(glass))))

# Résumé complet
summary(glass)

# Calcul des écart-types (mesure de dispersion)
sd_values <- sapply(glass[, 2:10], sd)
print(sd_values)

# Calcul de la matrice de corrélation
mat_corr <- cor(glass[, 2:10])
round(mat_corr, 2)

# Visualisation de la matrice de corrélation
library(corrplot)
corrplot(mat_corr, method = "circle", type = "upper", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black")

# Mettre les 3 graphiques sur une seule ligne
par(mfrow=c(1,3)) 

# Les 3 boxplots essentiels
boxplot(Mg ~ Type, data=glass, col="orange", main="1. Tri Principal (Mg)")
boxplot(Al ~ Type, data=glass, col="lightblue", main="2. Affinement (Al)")
boxplot(Ba ~ Type, data=glass, col="lightgreen", main="3. Signature Phares (Ba)")

# données brutes => données normalisées
donnees_normalisees <- scale(glass[, 2:10])

# Exportation des données normalisées en fichier CSV 
write.csv(donnees_normalisees, "donnees_normalisees.csv", row.names = FALSE)