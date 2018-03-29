## Travail Pratique 3
## Par Diana El-Masri et William Flageol

library('RCurl')
library('Matrix')

# Obtentions et organisation des données
u.data <- read.csv(text=getURL('http://www.groupes.polymtl.ca/log6308/Tp/20173/u.data.csv', userpwd='20113:20113'), sep='|', header=T)
u.sparse <- sparseMatrix(i = u.data$user.id, j = u.data$item.id, x = u.data$rating)
u.sparse.na <- u.sparse
u.sparse.na[u.sparse.na == 0] <- NA

# Fonctions pour calculer les erreurs.
f.mae <- function(m1, m2) mean(as.matrix(abs(m1 - m2)), na.rm=T)
f.rmse <- function(m1, m2) mean(as.matrix((m1 - m2)^2), na.rm=T)


## Question 1

# Comme algorithme de prédiction de base, nous allons prendre la moyenne de chaque item comme
# prédiction.
u.sparse.colMeans <- matrix(colMeans(u.sparse.na, na.rm=T), nrow(u.sparse), ncol(u.sparse), byrow=T)

# Calculs des erreurs.
f.mae(u.sparse.colMeans, u.sparse.na)
f.rmse(u.sparse.colMeans, u.sparse.na)

# Environ 0.8 et 1.0. Un bon algorithme devrait donc avoir de meilleurs résultats que ça.


## Question 2

# Fonction qui divise une matrice creuse en résultats de svd (UDV).
f.svd <- function(m.sparse) {
  # On transforme la matrice creuse en matrice dense en utilisant la moyenne des items.
  m.dense <- apply(m.sparse, 2, function (c)
    replace(c, is.na(c), mean(c, na.rm=T)))
  
  # Dans le cas où un item n'aurait pas de valeurs (e.g. durant les validations croisées),
  # on utilisera la moyenne totale de la matrice.
  m.dense[is.na(m.dense)] <- mean(m.dense, na.rm=T)
  
  # On normalise la matrice dense en soustrayant la moyenne de chaque utilisateur.
  m.dense.normalized <- m.dense - rowMeans(m.sparse, na.rm=T)
  
  # Application de svd.
  svd(m.dense.normalized)
}

# On utilise la fonction pour exécuter svd et obtenir les trois matrices U, D et V.
u.svd <- f.svd(u.sparse.na)


## Question 3

# Fonction qui utilise les résultats d'un svd pour prédire les valeurs d'une matrice.
# Nécessite la matrice de base comme paramètre d'entrée pour dénormaliser les résultats.
f.svd.predict <- function(m.sparse, svd, dim) {
  # On crée la diagonale à partir de la matrice D en gardant un nombre de dimension dim.
  svd.d.dim <- diag(c(svd$d[1:dim], rep(0, length(svd$d) - dim)))
  
  # On reconstruit la matrice initiale à partir de la diagonale.
  predict <- svd$u %*% svd.d.dim %*% t(svd$v)
  
  # Dénormalisation.
  predict + rowMeans(m.sparse, na.rm=T)
}

# Exécution de la prédiction pour toute la matrice, avec 10 dimensions gardées.
u.svd.predict <- f.svd.predict(u.sparse.na, u.svd, 10)


## Question 4

# Calculs des erreurs de la prédiction.
f.mae(u.svd.predict, u.sparse.na)
f.rmse(u.svd.predict, u.sparse.na)

# Environ 0.7 et 0.79, meilleur que l'algorithme de base de la question 1.


## Question 5

# Calcul de l'erreur absolue moyenne pour svd de 1 à 15 dimensions.
# Attention, un peu long à exécuter.
r.mae <- sapply(c(1:15), function(i) f.mae(f.svd.predict(u.sparse.na, u.svd, i), u.sparse.na))

# Calcul de l'erreur quadratique pour 1 à 15 dimensions.
# Commenté parce que long à exécuter et résultats semblables à MAE.
#r.rmse <- sapply(c(1:15), function(i) f.rmse(f.svd.predict(u.sparse.na, u.svd, i), u.sparse.na))

# Graphique de l'erreur absolue moyenne.
plot(r.mae)

# Graphique de l'erreur quadratique.
#plot(r.rmse)


# Difficile de déterminer un nombre de dimension optimal. Sans validation croisée,
# la précision continue d'augmenter plus on ajoute des dimensions.

# Néanmoins, il semble y avoir une asymptote à 6-7 dimensions?


## Questions 6

# Fonction pour diviser un vecteur en plusieurs vecteurs de taille régulière.
f.split.chunks <- function(v, size.chunk)
  split(v, ceiling(seq_along(v) / size.chunk))

# Fonction pour exécuter les validations croisées sur une matrice creuse,
# en spécifiant un ratio pour chaque pli et les dimensions de svd à utiliser.
f.cross <- function(m.sparse, ratio, dims) {
  # On veut d'abord tous les votes.
  id.ratings <- which(!is.na(m.sparse))
  
  # On divise les votes en plusieurs vecteurs basé sur ratio.
  m.id.test <- f.split.chunks(sample(id.ratings), floor(length(id.ratings) * ratio))
  
  # Calcul de l'erreur absolue moyenne pour chaque vecteur de test.
  results <- sapply(m.id.test, function (id.test) {
    # Création de la matrice d'entrainement.
    training <- m.sparse
    training[id.test] <- NA
    
    # Exécution de svd en utilisant la matrice d'entrainement.
    svd <- f.svd(training)
    
    # Création de la matrice de test.
    test <- m.sparse
    test[-id.test] <- NA
    
    # Calcul de MAE sur chaque vote de test par rapport au vote prédit.
    sapply(dims, function(i) f.mae(f.svd.predict(m.sparse, svd, i), test))
  })

  if(is.null(nrow(results)))
    # Si un seul vecteur, on prend la moyenne totale du vecteur.
    mean(results)
  else
    # Si plusieurs vecteurs, on retourne la moyenne de chacun dans un vecteur.
    rowMeans(results)
}

# Validations croisées avec 5 plis (ratio 0.2) pour des dimensions de
# 2, 5 à 21, 25, 50 et 100 (comme l'article).
# Attention, très long à exécuter (quelques minutes).
r.cross <- f.cross(u.sparse.na, 0.2, c(2, 5:21, 25, 50, 100))
plot(r.cross)

# On prend le nombre de dimensions avec la meilleure erreur.
order(r.cross)[1]

# Environ index 11, donc

# Index: 1 2 3 4 5 6 7  8  9  10 11
# Dim:   2 5 6 7 8 9 10 11 12 13 14
 
# ~ 14 dimensions, même chose que l'article.

f.cross(u.sparse.na, 0.2, 14)

# 14 dimensions pour un MAE de 0.77, toujours mieux que l'algorithme de Q1,
# et cette fois avec des validations croisées (ce que Q1 n'a pas fait).


## Question 7

# Récupération de quelques fonctions du TP1
f.min.nindex <- function(m, n=5) {
  i <- order(m)
  return(i[1:n])
}

# Cosinus entre un vecteur v et chaque colonne de la matrice m
f.cosinus.vm <- function(v,m) { n <- sqrt(colSums(m^2)); (v %*% m)/(n * sqrt(sum(v^2))) }

# Fonction qui exécute des validations croisées utilisant une approche item-item.
f.item.item.cross <- function(m.sparse, ratio, max.folds = 0) {
  # Vecteurs des votes.
  id.ratings <- which(!is.na(m.sparse))
  # On sépare les votes en vecteurs égaux.
  m.id.test <- f.split.chunks(sample(id.ratings), floor(length(id.ratings) * ratio))
  
  # Par soucis de performance, on peut limiter le nombre de plis.
  if(max.folds > 0)
    m.id.test = m.id.test[1:min(max.folds, length(m.id.test))]
  
  # Calcul de MAE pour tous les votes de test.
  results <- sapply(m.id.test, function (id.test) {
    # Matrice d'entrainement.
    training <- m.sparse
    training[id.test] <- NA
    
    # On veut une matrice d'entrainement avec des 0 à la place des NA pour le calcul du cosinus.
    training.zero <- training
    training.zero[is.na(training)] <- 0 
    
    # On centre la matrice d'entrainement.
    training.center <- training.zero
    training.center <- scale(training.center, center = T, scale = F)
    
    # Utilisation de l'approche item-item pour prédire chaque test du vecteur.
    training[id.test] <- sapply(id.test, function(id) f.item.item(training, training.center, id))
    
    # Matrice de test.
    test <- m.sparse
    test[-id.test] <- NA
    
    # Calcul de MAE entre les valeurs prédites et les votes de tests.
    f.mae(training, test)
  })
  
  # On retourne la moyenne des résultats.
  mean(results)
}

f.item.item <- function(m.sparse.na, m.sparse.center, id){
  # l'index du film et de l'utilisateur dans la matrice "training" pour lesquelle on predit le vote
  k<-arrayInd(id, dim(m.sparse.center))
  item.id <- k[2]
  user.id <- k[1]
  
  ## Ici, on commence par aller chercher les 20 voisins rapprochés (films)
  n.voisins <- 20 + 1
  
  ## On calcul la distance Euclédienne entre chaque film et id
  distance.id <- sqrt(colSums((m.sparse.center[,item.id] - m.sparse.center)^2))
  
  ## On obtient les 21 voisins les plus proches de id
  min.distance.id <- f.min.nindex(distance.id, n.voisins)[-1]
  
  ## Ensuite, on calcule le cosinus entre chaque item et id.
  sim.cosinus <- f.cosinus.vm(m.sparse.center[,item.id], m.sparse.center[ ,min.distance.id])
  
  ## On calcule la moyenne des votes de l'item.
  mean <- mean(m.sparse.na[,item.id], na.rm=T)
  
  # On vérifie si l'utilisateur a un vote dans la liste.
  has.data <- !is.na(m.sparse.na[user.id, min.distance.id])
  if(any(has.data)) {
    # Si oui, on effectue le calcul d'estimation avec le cosinus comme poids.
    calc.num <- sum((m.sparse.center[user.id,min.distance.id] * sim.cosinus))
    calc.denum <- sum(abs(sim.cosinus))
    mean + calc.num / calc.denum
  }
  else {
    # Si non, impossible d'estimer.
    NA
  }
}


# Seulement un pli effectué pour sauver du temps.
# prend quelques minutes.
f.item.item.cross(u.sparse.na, 0.1, 1)

## Résultat semblable, mais en plus de temps (tous les plis):
#f.item.item.cross(u.sparse.na, 0.1, 10)
#0.8923129

# Environ 0.9 MAE pour l'apprcohe item-item. 

### On constate donc que SVD avec 14 dimensions est significativement meilleur et à une precision plus élevée.(MAE 0.77)