

require(rpart)

#changer l'achitecture, on a besoin de tous nos objets rpart donc il faudrait que get predictionrpart prenne en argument un objet rpart

#remarque : selon intro to boosting le calcul de l'erreur a l'air de d�pendre de la distrib

#de manière générale il y a des problemes lorsque erreur nulle

#avec le dataset sonar
sonar <- read.csv("~/GIT/adaboostR/sonar.all-data", header=FALSE)

adaboostM1(V61~.,sonar,nIter=5,maxDepth=5)

#avec le dataset plrx de kaggle
plrx <- read.delim("~/GIT/adaboostR/plrx.txt", header=FALSE)
plrx=plrx[,-14]
plrx$V13[which(plrx$V13=="1")]="0"
plrx$V13[which(plrx$V13=="2")]="1"

adaboostM1(V13~.,plrx,nIter=5,maxDepth=1)

#données aléatoires
data=matrix(runif(1000),ncol=10)
data[,1]=rbinom(100,1,0.5)
data=data.frame(data)
data$X1=as.factor(data$X1)
data$X1 <- ifelse(data$X1 == "1", 1, -1)


adaboostM1(X1~.,data,5)

#iris biclasse
data(iris)
iris=iris[iris$Species!="virginica",]
iris$Species=factor(iris$Species)

# Recodage en +1 -1
iris$Species <- ifelse(iris$Species == "setosa", 1, -1)

adaboostM1(Species~.,iris,3)

#mettre quelque part une vérification pour que le nombre de classes a prédire ne pose pas problème
getPredictionRPart <- function(arbre, data){
  m <- nrow(data)
  pred <- predict(arbre, newdata = data)
  classes <- colnames(pred)
  prediction <- vector(length = m)
  for(i in 1:m){
    prediction[i] <- classes[which.max(pred[i,])]
  }
  return(prediction)
}

#trouver un meilleur nom : cette fonction renvoie l'erreur mais aussi les indices des bons classements
getError <- function(obs, pred, distrib){
  stopifnot(length(obs) == length(pred))
  indTrue = which(obs == pred)
  #ce qui suit est sale
  # res <- NA
  # res$indTrue <- indTrue
  # res$error <- 1 - (length(indTrue)/length(obs))
  res <- list(indTrue = indTrue,
              error = sum(distrib[-indTrue])/sum(distrib)) # On somme les poids des observations mal prédites
  #on divise aussi par la somme de distrib, celle-ci ne somme visiblement pas toujours a 1
  return(res)
}

adaboostM1 <- function(formula, data, nIter = 10, maxDepth = 1){
  m <- nrow(data)
  nColY <- which(colnames(data) == all.vars(formula)[1]) #numero de colonne où se trouve Y dans data
  distrib <- matrix(0, nrow = m, ncol = nIter)
  distrib[,1] <- 1/m
  for (i in 1:nIter){
    prediction <- getPredictionRPart(formule = formula, data = data, poids = distrib[,i], maxDepth = maxDepth)
    erreur <- getError(data[,nColY], prediction)
    if(erreur$error>0.5){
      nIter <- i-1
      return("pas cool man")
    }
    beta <- erreur$error/(1-erreur$error)
    if(i < nIter){ # pas besoin de la distrib nIter+1
      Z <- 2*sqrt(beta) # facteur de normalisation trouvé sur wiki
      distrib[,(i+1)] <- distrib[,i]/Z
      distrib[erreur$indTrue,(i+1)] <- distrib[erreur$indTrue,(i+1)]*beta
    }
  }
  #à finir
  return(distrib)
}

adaboostBin <- function(formula, data, nIter = 10, maxDepth = 1){
  m <- nrow(data)
  nColY <- which(colnames(data) == all.vars(formula)[1]) #numero de colonne où se trouve Y dans data
  data[,nColY] <- as.factor(data[,nColY])
  
  distrib <- matrix(0, nrow = m, ncol = nIter)
  distrib[,1] <- 1/m
  
  listeArbres <- list()
  alphas <- numeric(nIter)
  
  environment(formula) <- environment()
  
  for(i in 1:nIter){
    modele <- rpart(formula = formula, data = data, weights = distrib[,i], control = rpart.control(maxdepth = maxDepth))
    listeArbres[[i]] <- modele
    
    prediction <- getPredictionRPart(arbre = modele, data = data)
    
    # Conversion des prédiction en -1 et +1
    prediction <- as.integer(prediction)
    
    erreur <- getError(data[,nColY], prediction, distrib[,i])
    
    sign <- rep(-1, m)
    sign[erreur$indTrue] <-  1
    
    alphas[i] <- 1/2*log((1 - erreur$error)/erreur$error)
    
    Z <- 2 * sqrt(alphas[i]) # facteur de normalisation trouvé sur wiki
    
    if(i<nIter){
      distrib[,i+1] <- distrib[,i] / Z * exp(-sign * alphas[i]) #probl�matique, distrib ne somme pas toujours � 1????
    }
    
  }
  return(list(classifiers = listeArbres, alphas = alphas))
}

predictAdaboost <- function(adaBooster,newdata){
  nbClassif <- length(adaBooster$alphas)
  predictions <- matrix(0,nrow=nrow(newdata),ncol=nbClassif)
  for (i in 1:nbClassif){
    predictions[,i] <- getPredictionRPart(adaBooster$classifiers[[i]],newdata)
  }
  predictions <- ifelse(predictions == "1",1,-1)
  res <- sign(predictions%*%adaBooster$alphas)
  return(res)
}

calculErrMoyAlea <- function(nEch){
  res <- numeric(nEch)
  for (i in 1:nEch){
    data=matrix(runif(1000),ncol=10)
    data[,1]=rbinom(100,1,0.5)
    data=data.frame(data)
    data$X1=as.factor(data$X1)
    data$X1 <- ifelse(data$X1 == "1", 1, -1)
    
    a=adaboostBin(X1~.,data)
    res[i]=sum(data$X1!=predictAdaboost(a,data))
  }
  return(mean(res)/100)
}
#apr�s simulation de 100 jeux de donn�es alatoires on a 26,5% de taux d'erreur moyen