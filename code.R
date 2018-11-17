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
  res <- list(indTrue = indTrue,
              error = sum(distrib[-indTrue]))#/sum(distrib)) # On somme les poids des observations mal prédites
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
  
  distrib <- matrix(NA, nrow = m, ncol = nIter)
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
    
    #Z <- 2 * sqrt(alphas[i]) # facteur de normalisation trouvé sur wiki
    Z <- 2*sqrt(erreur$error*(1- erreur$error)) # Z du MIT
    
    if(i < nIter){
      distrib[,i+1] <- distrib[,i] / Z * exp(-sign * alphas[i]) #probl?matique, distrib ne somme pas toujours ? 1????
    }
    
  }
  return(list(classifiers = listeArbres, alphas = alphas))
}

predictAdaboost <- function(adaBooster,newdata){
  nbClassif <- length(adaBooster$alphas)
  predictions <- matrix(nrow = nrow(newdata), ncol = nbClassif)
  for (i in 1:nbClassif){
    predictions[,i] <- getPredictionRPart(adaBooster$classifiers[[i]],newdata)
  }
  predictions <- ifelse(predictions == "1", 1, -1)
  res <- sign(predictions%*%adaBooster$alphas)
  return(res)
}