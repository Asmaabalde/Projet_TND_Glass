
# Chargement des librairies nécessaires
# Décommentez la ligne suivante si le package mclust n'est pas installé
# install.packages("mclust") 
library(mclust) # Nécessaire pour calculer l'Indice de Rand facilement

# 1. Préparation des données et des "vrais" labels
# On récupère le fichier d'origine pour isoler la colonne 11 (les vrais types)
glass <- read.csv("data/glass.data", header=FALSE)
colnames(glass) <- c("Id", "RI", "Na", "Mg", "Al", "Si", "K", "Ca", "Ba", "Fe", "Type")
vrais_labels <- as.factor(glass$Type)

# On importe les données quantitatives qui ont déjà été normalisées par la Personne A
# (L'ACP n'est pas obligatoire pour les k-means, on peut utiliser les données centrées-réduites directes)
donnees_norm <- read.csv("donnees_normalisees.csv")

# ==========================================
# MÉTHODE 1 : K-MEANS
# ==========================================

# 2. Lancement de l'algorithme K-Means
# On cherche 6 groupes car le Type 4 est absent du dataset
set.seed(42) # Fixe l'aléatoire pour que les résultats soient reproductibles
res_kmeans <- kmeans(donnees_norm, centers = 6, nstart = 25)

# On extrait les groupes prédits
clusters_kmeans <- res_kmeans$cluster

# 3. Comparaison et Mesure de Performance (K-means)
cat("\n--- RÉSULTATS K-MEANS ---\n")

# Matrice de confusion (Croisement entre vrais types et clusters trouvés)
table_croisee_km <- table(Vrai_Type = vrais_labels, Cluster_Trouve = clusters_kmeans)
print("Matrice de confusion (Vrais Types vs Clusters) :")
print(table_croisee_km)

# Calcul de l'Indice de Rand Ajusté (Adjusted Rand Index)
rand_index_km <- adjustedRandIndex(vrais_labels, clusters_kmeans)
cat("Indice de Rand Ajusté (K-means) :", round(rand_index_km, 4), "\n")

# Calcul de la Pureté
# Formule : Somme des éléments majoritaires de chaque cluster / Nombre total d'éléments
purete_km <- sum(apply(table_croisee_km, 2, max)) / nrow(donnees_norm)
cat("Pureté du clustering (K-means) :", round(purete_km * 100, 2), "%\n")


# ==========================================
# MÉTHODE 2 : CAH (Classification Ascendante Hiérarchique)
# ==========================================

# 1. Calcul de la matrice des distances euclidiennes
matrice_distances <- dist(donnees_norm, method = "euclidean")

# 2. Algorithme CAH avec la méthode de Ward
res_cah <- hclust(matrice_distances, method = "ward.D2")

# Affichage du dendrogramme
# Affichage propre sans le "pâté" de texte en bas
plot(res_cah, 
     main = "Dendrogramme de la CAH (Verres)", 
     xlab = "Individus (n=214)", 
     ylab = "Hauteur", 
     sub = "", 
     labels = FALSE,
     hang = -1) # C'est cette commande qui efface les textes du bas

abline(h = 0, col = "black", lwd = 1)

rect.hclust(res_cah, k = 6, border = "red") # Entoure les 6 groupes sur le graphique

# 3. Découpage de l'arbre en 6 groupes
clusters_cah <- cutree(res_cah, k = 6)

# 4. Comparaison et Mesure de Performance (CAH)
cat("\n--- RÉSULTATS CAH ---\n")

table_croisee_cah <- table(Vrai_Type = vrais_labels, Cluster_Trouve = clusters_cah)
rand_index_cah <- adjustedRandIndex(vrais_labels, clusters_cah)
purete_cah <- sum(apply(table_croisee_cah, 2, max)) / nrow(donnees_norm)

cat("Indice de Rand Ajusté (CAH) :", round(rand_index_cah, 4), "\n")
cat("Pureté (CAH) :", round(purete_cah * 100, 2), "%\n")

