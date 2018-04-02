library('RCurl')
library('Matrix')
library('bitops')
library('ggplot2')
library('knitr')



###########################################Initialization #################################################################
u.user <- read.csv(text=getURL('http://www.groupes.polymtl.ca/log6308/Tp/20173/u.user.csv', userpwd='20113:20113'), sep='|', header=T)
u.item <- read.csv(text=getURL('http://www.groupes.polymtl.ca/log6308/Tp/20173/u.item.csv', userpwd='20113:20113'), sep='|', header=T)
u.data <- read.csv(text=getURL('http://www.groupes.polymtl.ca/log6308/Tp/20173/u.data.csv', userpwd='20113:20113'), sep='|', header=T)


u.sparse <- sparseMatrix(i = u.data$user.id, j = u.data$item.id, x = u.data$rating)
u.sparse.na <- u.sparse
u.sparse.na[u.sparse.na == 0] <- NA

u.sparse.center <- u.sparse.na - rowMeans(u.sparse.na, na.rm = T) 
u.sparse.center0 <- u.sparse.center                   # créons une matrice avec des 0 au lieu des NA
u.sparse.center0[is.na(u.sparse.center)] <- 0

id.ratings <- which(!is.na(u.sparse.na))

# On divise les votes en plusieurs vecteurs basé sur ratio. 80% training 20% test
id.test <- sample(id.ratings, floor(length(id.ratings) * 0.2))
id.test <- as.vector(id.test)
                     

# Création de la matrice d'entrainement.
training <- u.sparse.na
training[id.test] <- NA

training.zero <- training
training.zero[is.na(training)] <- 0 

# On centre la matrice d'entrainement.
training.center <- training - rowMeans(training, na.rm = T) 
training.center0 <- training.center                   # créons une matrice avec des 0 au lieu des NA
training.center0[is.na(training.center)] <- 0

# Création de la matrice de test.
test <- u.sparse.na
test[-id.test] <- NA  

#################################################################


users.sample <- sample(c(1:943), 50)

#users.sample
#[1] 661 937 250 288 545 867 159 775 270 824 830 415 540 703 181 314 544 127 641 898 165 298 291 943 158 264 439  80 815
#[30] 519  34  96 305 464 795 123 684  97 376 211 933 427 255 371 761 259 800 431 871 429

sample.users_50 <- c(661, 937, 250, 288, 545, 867, 159, 775, 270, 824, 830, 415, 540, 703, 181, 314, 544, 127, 641, 898, 165, 298, 291, 943, 158, 264, 439,  80, 815,
519,  34,  96, 305, 464, 795, 123, 684,  97, 376, 211, 933, 427, 255, 371, 761, 259, 800, 431, 871, 429)

##############################################
m.explore.donnee <- sparseMatrix(i = u.data$user.id, j = u.data$item.id, x = u.data$rating)
v <-as.vector(m.explore.donnee)


Real.sparse.na <- as(  as(u.sparse.na, "matrix")   , "realRatingMatrix")
user.stats <-  as.data.frame(cbind('mean'=rowMeans(Real.sparse.na, na.rm=T),'number'=rowCounts(Real.sparse.na)))
item.stats <- as.data.frame(cbind('mean'=colMeans(Real.sparse.na, na.rm=T), 'number'=colCounts(Real.sparse.na)))
item.stats <-as.data.frame(sapply(item.stats, function(x) as.numeric(as.character(x))))


#  fonctions
f.mae <- function(m1, m2) mean(as.matrix(abs(m1 - m2)), na.rm=T)
f.rmse <- function(m1, m2) mean(as.matrix((m1 - m2)^2), na.rm=T)

f.min.nindex <- function(m, n=5) {
  i <- order(m)
  return(i[1:n])
}

# Trouve les indexes des premières 'n' valeurs maximales d'une matrice
f.max.nindex <- function(m, n=5) {
  i <- order(m, decreasing=TRUE)
  return(i[1:n])
}

# Fonction pour diviser un vecteur en plusieurs vecteurs de taille régulière.
f.split.chunks <- function(v, size.chunk) split(v, ceiling(seq_along(v) / size.chunk))


# Cosinus entre un vecteur v et chaque colonne de la matrice m
f.cosinus.vm <- function(v,m) { n <- sqrt(colSums(m^2)); (v %*% m)/(n * sqrt(sum(v^2))) }


###########################################Primitive Model topN ##########################################
f.Primitive.Model.Top_50 <- function(){
  # Compute the movie IDs with the largest number of users.
  number.users.voted.for.item <- colSums(u.sparse != 0)
  number.users.voted.for.item <- as.matrix(number.users.voted.for.item)
  rownames(number.users.voted.for.item) <- paste(1:ncol(u.sparse), sep='')
  number.users.voted.for.item <- number.users.voted.for.item [order(-number.users.voted.for.item[,1]), , drop = FALSE]
  
  
  # Compute the movie IDs with the hihest average ratings
  mean.votes.item <- colMeans(u.sparse.na, na.rm=T)
  mean.votes.item <- as.matrix(mean.votes.item)
  rownames(mean.votes.item) <- paste(1:ncol(u.sparse), sep='')
  mean.votes.item <- mean.votes.item [order(-mean.votes.item[,1]), , drop = FALSE]
  
  PM.list <- unique(c(rbind(rownames(number.users.voted.for.item),rownames(mean.votes.item))))
  PM.list.top.50 <- PM.list[1:50]
  as.numeric(PM.list.top.50)
}

###########################################Serendepity Metric######################################
f.serendepity <- function(RS.top_20){

  PM.list.top.50 <- f.Primitive.Model.Top_50() 
  
  Serendepity.total <- c()
  UNEXP_Usfull <- c()
  serendipity.list <- c()
  
  for ( j in 1:length(sample.users_50)){
  #for ( j in 1:1){
    #k = sample.users_50[j]

    #RS_List.to.test <- RS.top_20[,j]
    RS_List.to.test <- RS.top_20[,j]
  
    UNEXP_Usfull <- c()
    
    serendipity.list <- c()
    
    
    for( i in 1:length(RS_List.to.test)){
      if (!(RS_List.to.test[i] %in% PM.list.top.50)){ 
        if (u.sparse[j,RS_List.to.test[i]] > 2){
          UNEXP_Usfull <- c(UNEXP_Usfull , RS_List.to.test[i])
        }
      }
    }
    
    serendepity.liste <- length(UNEXP_Usfull)/length(RS_List.to.test)
    
    Serendepity.total <- c(Serendepity.total,serendepity.liste)

  }
  seren.results <- sum(Serendepity.total)/ length(sample.users_50)
  print(seren.results)
}

##########################################Recal Metrics################################# 
f.recall <- function(RS_top_20){
  
  Recall.total <- c()
  relevant <- 0

  for ( j in 1:length(sample.users_50)){
    
    RS_List.to.test <- RS_top_20[,j]
    
    total.number.of.relevant <- which(u.sparse[sample.users_50[j],] > 2)
    
    relevant <- 0

        for( i in 1:length(RS_List.to.test)){
      
      if (u.sparse[sample.users_50[j],RS_List.to.test[i]] > 2){
         relevant <- relevant + 1
      }
        }
    
    recall.list <-  relevant / length(total.number.of.relevant)   
    Recall.total <- c(Recall.total , recall.list)
    
  }

  Recall.total <- sum(Recall.total)/ length(sample.users_50)
  print(Recall.total)
}    

################################################Precision metric##########################################
f.precision <- function(RS_top_20){
  
  Precision.total <- c()
  relevant <- 0

  for ( j in 1:length(sample.users_50)){
    
    RS_List.to.test <- RS_top_20[,1]
    
    relevant <- 0
    
    for( i in 1:length(RS_List.to.test)){
      if (u.sparse[sample.users_50[j],RS_List.to.test[i]] > 2){
        relevant <- relevant + 1
      }
    }
    
    total.number.of.relevant <- which(u.sparse[sample.users_50[j],] > 2)
    
    Precision.list <-  relevant / length(RS_List.to.test)   
    
    Precision.total <- c(Precision.total , Precision.list)
    
  }
  
  Precision.total <- sum(Precision.total)/ length(sample.users_50)
  print(Precision.total)
  
}     


########################################Prediction par la moyenne de votes par film#####


f.RS.top_20.colmeans <- function(){
  
  predict.colMeans <- matrix(colMeans(u.sparse.na, na.rm=T), nrow(u.sparse), ncol(u.sparse), byrow=T)
  
  sapply(sample.users_50, function(i){
    id.filmes.nouveaux <- which(is.na(u.sparse.na[i,]))
    f.max.nindex(predict.colMeans[i,id.filmes.nouveaux],20)
    }
    )
}

#############################################SVD########################################
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


# Fonction qui utilise les résultats d'un svd pour prédire les valeurs d'une matrice.
# Nécessite la matrice de base comme paramètre d'entrée pour dénormaliser les résultats.
f.s <- function(m.sparse, svd, dim = 14) {
  # On crée la diagonale à partir de la matrice D en gardant un nombre de dimension dim.
  svd.d.dim <- diag(c(svd$d[1:dim], rep(0, length(svd$d) - dim)))
  
  # On reconstruit la matrice initiale à partir de la diagonale.
  predict <- svd$u %*% svd.d.dim %*% t(svd$v)
  
  # Dénormalisation.
  predict + rowMeans(m.sparse, na.rm=T)
}


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
    sapply(dims, function(i) f.mae(f.s(m.sparse, svd, i), test))
   
  })

  if(is.null(nrow(results)))
    # Si un seul vecteur, on prend la moyenne totale du vecteur.
    mean(results)
  else
    # Si plusieurs vecteurs, on retourne la moyenne de chacun dans un vecteur.
    rowMeans(results)
}

f.RS.top_20.svd <- function(){
  # 14 dimensions 
  metric.svd <- f.cross(u.sparse.na, 0.2, 14)
  p <- f.s(u.sparse.na, f.svd(u.sparse.na))
  sapply(sample.users_50, function(i) f.max.nindex(p[i,],20))
}


##############################################################################################################################
##################################################UBCF##############################################################################


# Fonction qui exécute des validations croisées utilisant une approche user-user.
f.user.user.cross <- function(m.sparse, ratio, max.folds = 1) {
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
    
    training.center <- t(scale(t(training), center = T, scale = F))
    training.center0 <- training.center                   # créons une matrice avec des 0 au lieu des NA
    training.center0[is.na(training.center)] <- 0
   
    # Utilisation de l'approche user-user pour prédire chaque test du vecteur.
    training[id.test] <- sapply(id.test, function(id) f.user.user(training, training.zero, training.center0, id))
    
    # Matrice de test.
    test <- m.sparse
    test[-id.test] <- NA
    
    # Calcul de MAE entre les valeurs prédites et les votes de tests.
    f.mae(training, test)
  
  })
  
 
  # On retourne la moyenne des résultats.
  mean(results)

}

f.user.user <- function(m.sparse.na, m.sparse, m.sparse.center0, id){
  # l'index du film et de l'utilisateur dans la matrice "training" pour lesquelle on predit le vote
  k<-arrayInd(id, dim(m.sparse.na))
  item.id <- k[2]
  user.id <- k[1]
  
  print(user.id)
 
  ## Ici, on commence par aller chercher les 20 voisins rapprochés (user)
  n.voisins <- 20 + 1
  
  ## On calcul la distance Euclédienne entre chaque user et id
  distance.id <- sqrt(colSums((m.sparse[user.id,] - t(m.sparse))^2))
   
  ## On obtient les 21 voisins les plus proches de id
  min.distance.id <- f.min.nindex(distance.id, n.voisins)[-1]
  
  ## Ensuite, on calcule le cosinus entre chaque users proche et id.
  sim.cosinus <- f.cosinus.vm(m.sparse.center0[user.id,], t(m.sparse.center0[min.distance.id,]))
  
  ## On calcule la moyenne des votes de l'usager.
  mean <- mean(m.sparse.na[user.id, ], na.rm=T)
  
  # On vérifie si l'utilisateur a un vote dans la liste.
  has.data <- !is.na(m.sparse.na[min.distance.id, item.id])
  if(any(has.data)) {
    # Si oui, on effectue le calcul d'estimation avec le cosinus comme poids.
    calc.num <- sum((m.sparse.center0[min.distance.id, item.id] * sim.cosinus))
    calc.denum <- sum(abs(sim.cosinus))
    mean + calc.num / calc.denum
  }
  else {
    # Si non, impossible d'estimer.
    NA
  }
}

############################recommander 20 films aux 50 usager choisis au hazard

f.ubcf <- function(id){

  user.id <- id
  print(id)
  ## On dresse une liste des utilisateurs n'ayant pas de vote pour Star Trek V.

  ## Ici, on commence par aller chercher les 20 voisins rapprochés (user)
  n.voisins <- 20 + 1
  
  ## On calcul la distance Euclédienne entre chaque user et id
  distance.id <- sqrt(colSums((training.zero[user.id,] - t(training.zero))^2))
   
  ## On obtient les 21 voisins les plus proches de id
  min.distance.id <- f.min.nindex(distance.id, n.voisins)[-1]
  
  ## Ensuite, on calcule le cosinus entre chaque users proche et id.
  sim.cosinus <- f.cosinus.vm(training.center0[user.id,], t(training.center0[min.distance.id,]))
  
  ## On calcule la moyenne des votes de l'usager.
  mean <- mean(training[user.id, ], na.rm=T)

  
  ## On applique la fonction à chaque filmes sans vote.
  #sparse.avec.votes <- sapply(filmes.sans.vote, function(id) f.estimate(mean, sim.cosinus, min.distance.id, user.id, id))

   sapply(c(1:1682), function(id) f.estimate(mean, sim.cosinus, min.distance.id, user.id, id))
  
}

f.estimate <- function(mean, sim.cosinus, min.distance.id, user.id,item.id) {
  

  
  # On vérifie si l'utilisateur a un vote dans la liste.
  has.data <- !is.na(training[min.distance.id, item.id])
  if(any(has.data)) { 
    # Si oui, on effectue le calcul d'estimation avec le cosinus comme poids.
    print("h")
    calc.num <- sum((training.center0[min.distance.id, item.id] * sim.cosinus))
    calc.denum <- sum(abs(sim.cosinus))
    mean + calc.num / calc.denum
  }
  else {
   
    # Si non, impossible d'estimer.
   NA
  }
  
}

# Seulement un pli effectué pour sauver du temps (très lent sinon).
# Même à 1 pli, prend quelques minutes.
metric.ubcf <- f.user.user.cross(u.sparse.na, 0.1, 1)
# 0.7725346

f.RS.top_20.ubcf<- function(){
  m <- sapply(sample.users_50, f.ubcf )
  RS.top_20.ubcf <- sapply(sample.users_50, function(i) f.max.nindex(m[i,],20))
}


#############################################################################################
#############################################################################################
######################################Top_N recommendation CB#######################################################

f.cb.norm.movie.genre <- function(m.item.genre){
  
  ########################normalization to sum to one of item/genre (feature) matrix
  
  # normalize la matrice pour que si jamais un vector a un seulement 3 genre nos donne nous soit pas 
  # biasie
  total.atrributes <- rowSums(m.item.genre,na.rm = T, dims = 1)
  
  x <- matrix(rep(total.atrributes,19),1682)
  
  norm.movie.genre <- m.item.genre / x
  
}

##############TF-IDF Frequency (DF) and Inverse Document Frequency (IDF) of featur:genre

f.cb.Tf_IDF <- function(m.item.genre){  
  
  binary_TF <- colSums(m.item.genre,na.rm = T, dims = 1)
  N <- 1682
  
  tf <- binary_TF / sum(binary_TF)
  
  idf <-log(N/binary_TF)
  tfidf <- tf * idf

  matrix(tfidf,1)
}

f.IUf <- function(m.item.genre){
  u.rating <- u.sparse
  u.rating[u.rating < 3] <- 0
  u.rating[u.rating > 0] <- 1
  u <- u.rating %*% m.item.genre
  
  u[u != 0] <- 1
  Uf = colSums(u,na.rm = T, dims = 1)
  log(943/Uf)
  
}
###########################################################################################################
###########################################################################################################
########## CB Feature weighted user 

f.RS.top_20.cb_ubcf <- function(){
  
  ################ Matrice genre de film. row: les films. col: les genres
  u.genre <- u.item[-(1:5)]
  m.item.genre<- as.matrix(u.genre)

  #m.item.genre <- m.item.genre [,-1]
  
    ########################normailse somme = 1 la matrcie item/genre
  movie.genre.norm <- f.cb.norm.movie.genre(m.item.genre)
  
  #m.tfidf <- f.cb.Tf_IDF(m.item.genre)
  
  
  IUf <- f.IUf(m.item.genre )
  
###################################################################  
  #u.rating <- u.sparse.center0
  #u.rating[u.rating < 0] <- 0
  #u.rating[u.rating > 0] <- 1
  
  #u.rating <- u.sparse
  #u.rating[u.rating < 3] <- 0
  #u.rating[u.rating > 0] <- 1
  
  #u.rating <- u.sparse
  
  u.rating <- u.sparse.center0
  

 #user.genre <- u.rating %*% m.item.genre
 user.genre <- u.rating %*% movie.genre.norm

   ###################poids vote#########################################
  #user.genre.weight <-t(t(user.genre) * as.vector(m.tfidf))
  user.genre.weight <- t(t(user.genre) * IUf)
  
  
  ###############################
  
  RS.top_20.cb_ubcf <- sapply(sample.users_50, function(i){
  
    user.id <- i
    
    n.voisins <- 21
    
      
    ## On obtient les 21 voisins les plus proches de id
    ##distance.id <- sqrt(colSums((u.sparse.center0[user.id,] - t(u.sparse.center0))^2))
    distance.id <- sqrt(colSums((user.genre.weight[user.id,] - t(user.genre.weight))^2))
    min.distance.id <- f.min.nindex(distance.id, n.voisins)[-1]
      
    film.ds.voisinage <- (colSums( u.sparse[min.distance.id,], na.rm=T) != 0)
    film.ds.voisinage <- which(film.ds.voisinage)
      
    frequence.genre.ds.voisinage <- colSums(user.genre.weight[min.distance.id,] != 0)
  # frequence.genre.ds.voisinage <- colSums(weight[min.distance.id,] != 0)
      
   # frequence.genre.ds.voisinage <- colSums(m.item.genre[film.ds.voisinage,] != 0)
    
    poid.genre<- m.item.genre[film.ds.voisinage,] * frequence.genre.ds.voisinage
      
    poid.film <- rowSums(poid.genre)
    
    
      
    f.max.nindex(poid.film[film.ds.voisinage],20)
  })
}

#####################################CB base
f.RS.top_20.cb_base <- function(){

  ################ Matrice genre de film. row: les films. col: les genres
  u.genre <- u.item[-(1:5)]
  m.item.genre<- as.matrix(u.genre)
  
  ########################normailse somme = 1 la matrcie item/genre
  movie.genre.norm <- f.cb.norm.movie.genre(m.item.genre)
  
  m.tfidf <- f.cb.Tf_IDF(m.item.genre)
  IUf <- f.IUf(m.item.genre )
  
  user.genre <- u.sparse.center0 %*% movie.genre.norm
  

  ###################poids vote#########################################

  user.genre.weight <-t(t(user.genre) * as.vector(m.tfidf))
  #user.genre.weight <- t(t(user.genre) * IUf)
  

  # les profile negatif sont mis a 0. les profile positifs sont mis a 1
  user.genre.binaire <- user.genre.weight
  
  user.genre.binaire[user.genre.binaire > 0 ] <- 1
  user.genre.binaire[ user.genre.binaire < 0 ] <- 0
  
 
  
  RS.top_20.cb_base <- sapply(sample.users_50,function(i){
    distance.id <- sqrt(colSums((user.genre.binaire[i,] -t(m.item.genre))^2))
    min.distance.id <- f.min.nindex(distance.id, 20)
  })

}




qplot(v) + ggtitle("Fréquence de votes") + labs(x = "Vote")
ggplot(user.stats,aes(number)) + geom_histogram(binwidth = 0.8,fill="orchid3") + labs(x = "Nombre de films evalués", y = "Nombre d'utilisateurs", title = "Nombre de films évalués par chaque utilisateur")
ggplot(item.stats,aes(number)) + geom_histogram(binwidth = 0.8,fill="blue") + labs(x = "Nombre de vote par film", y = "Nombre de films", title = "Fréquence de votes pour chaque film")


RS.top_20.colmeans <- f.RS.top_20.colmeans()
serendepity.colmeans <- f.serendepity(RS.top_20.colmeans)
precision.colmeans <- f.precision(RS.top_20.colmeans)
recall.colmeans <- f.recall(RS.top_20.colmeans)

RS.top_20.svd <- f.RS.top_20.svd()  
serendepity.svd <- f.serendepity(RS.top_20.svd)
precision.svd <- f.precision(RS.top_20.svd)
recall.svd <- f.recall(RS.top_20.svd)

RS.top_20.ubcf <- f.RS.top_20.ubcf()  # prend quelques minutes
serendepity.ubcf <- f.serendepity(RS.top_20.ubcf)
precision.ubcf <- f.precision(RS.top_20.ubcf)
recall.ubcf <- f.recall(RS.top_20.ubcf)

RS.top_20.cb_ubcf <- f.RS.top_20.cb_ubcf()
serendepity.cb_ubcf <- f.serendepity(RS.top_20.cb_ubcf)
precision.cb_ubcf <- f.precision(RS.top_20.cb_ubcf)
recall.cb_ubcf <- f.recall(RS.top_20.cb_ubcf)

RS.top_20.cb_base <- f.RS.top_20.cb_base()
serendepity.cb_base <- f.serendepity(RS.top_20.cb_base)
precision.cb_base <- f.precision(RS.top_20.cb_base)
recall.cb_base <- f.recall(RS.top_20.cb_base)


#Serendepity <- rbind(serendepity.colmeans,serendepity.svd,serendepity.ubcf,serendepity.cb_ubcf,serendepity.cb_base)
#Precision <- rbind(precision.colmeans,precision.svd,precision.ubcf,precision.cb_ubcf,precision.cb_base)
#Recall <- rbind(recall.colmeans,recall.svd,recall.ubcf,recall.cb_ubcf,recall.cb_base)

Serendepity <- rbind(serendepity.svd,serendepity.ubcf,serendepity.cb_ubcf,serendepity.cb_base)
Precision <- rbind(precision.svd,precision.ubcf,precision.cb_ubcf,precision.cb_base)
Recall <- rbind(recall.svd,recall.ubcf,recall.cb_ubcf,recall.cb_base)


Serendepity <- as.data.frame(Serendepity)
#Serendepity$Model <- c('ColMeans','SVD','UBCF','UBCF_CB', 'CB')
Serendepity$Model <- c('SVD','UBCF','UBCF_CB', 'CB')
(Serendepity)

#                      V1      model
#-----------------------------------
#serendepity.colmeans 0.024 ColMeans
#serendepity.svd      0.007      SVD
#serendepity.ubcf     0.112     UBCF
#serendepity.cb_ubcf  0.123  UBCF_CB
#serendepity.cb_base  0.092       CB

ggplot(Serendepity, aes(Model, Serendepity$V1))+ geom_col(width = 0.25, fill = "turquoise2") + labs(title = "Sérendipité")


Precision <- as.data.frame(Precision)
Precision$Model <- c('SVD','UBCF','UBCF_CB', 'CB')
(Precision)
#                    V1    model
#----------------------------------
#precision.colmeans 0.042 ColMeans
#precision.svd      0.148      SVD
#precision.ubcf     0.163     UBCF
#precision.cb_ubcf  0.119  UBCF_CB
#precision.cb_base  0.087       CB

ggplot(Precision, aes(Model, Precision$V1))+ geom_col(width = 0.25, fill = "violetred2") + labs(title = "Précision")

Recall <- as.data.frame(Recall)
Recall$Model <- c('SVD','UBCF','UBCF_CB', 'CB')
(Recall)
#                     V1    model
#-----------------------------------
#recall.colmeans 0.007643603 ColMeans
#recall.svd      0.032135491      SVD
#recall.ubcf     0.037053899     UBCF
#recall.cb_ubcf  0.02643686   UBCF_CB
#recall.cb_base  0.048201161       CB

ggplot(Recall, aes(Model, Recall$V1))+ geom_col(width = 0.25, fill = "seagreen3")+ labs(title = "Recall")
