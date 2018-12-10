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

adaboostM1 <- function(formula, data, nIter = 10, maxDepth = 5, bootstrap = T){
  
  environment(formula) <- environment()
  
  m <- nrow(data)
  nColY <- which(colnames(data) == all.vars(formula)[1]) #numero de colonne où se trouve Y dans data
  
  distrib <- matrix(0, nrow = m, ncol = nIter)
  distrib[,1] <- 1/m
  
  listeArbres <- list()
  betas <- numeric(nIter)
  
  for (i in 1:nIter){
    
    if(bootstrap){
      ech <- sample(1:nrow(data), nrow(data), replace = T, prob = distrib[,i])
      ech <- data[ech,]
      modele <- rpart(formula = formula,
                      data = ech,
                      control = rpart.control(maxdepth = maxDepth),
                      method = "class")
      
    }else{
      modele <- rpart(formula = formula,
                      data = data,
                      weights = distrib[,i],
                      control = rpart.control(maxdepth = maxDepth),
                      method = "class") 
    }
    
    prediction <- getPredictionRPart(modele, data)
    erreur <- getError(data[,nColY], prediction, distrib[,i])
    
    # Si on dépasse 50% d'erreur sur la dernière itération on l'ignore et on s'arrête
    if(erreur$error>0.5){
      i <- i - 1
      betas <- betas[1:i]
      
      return(list(classifiers = listeArbres,
                  factors = betas,
                  modalities = unique(data[, nColY])))
    }
    
    listeArbres[[i]] <- modele
    betas[i] <- erreur$error/(1-erreur$error)
    if(i < nIter){ # pas besoin de la distrib nIter+1
      # Z <- 2*sqrt(betas[i]) # facteur de normalisation trouvé sur wiki

      distrib[,(i+1)] <- distrib[,i]
      distrib[erreur$indTrue,(i+1)] <- distrib[erreur$indTrue,(i+1)]*betas[i]
      Z <- sum(distrib[,(i+1)])
      distrib[,(i+1)] <- distrib[,(i+1)]/Z
    }
  }
  return(list(classifiers = listeArbres,
              factors = betas,
              modalities = unique(data[, nColY])))
}

adaboostBin <- function(formula, data, nIter = 10, maxDepth = 5, bootstrap = T){
  
  environment(formula) <- environment()
  
  m <- nrow(data)
  
  # Identification de la colonne à prédire
  nColY <- which(colnames(data) == all.vars(formula)[1])
  
  # SOLUTION TEMPORAIRE
  data[,nColY] <- as.character(data[,nColY])
  
  # correspondance entre les modalités de la colonne à prédire et -1 ; +1
  modalities <- unique(data[,nColY])
  data[,nColY] <- ifelse(data[,nColY] == modalities[1], -1, 1)
  
  
  # Initialisation de la matrice des poids
  distrib <- matrix(NA, nrow = m, ncol = nIter)
  distrib[,1] <- 1/m
  
  listeArbres <- list()
  alphas <- numeric(nIter)
  
  
  for(i in 1:nIter){
    if(bootstrap){
      ech <- sample(1:nrow(data), nrow(data), replace = T, prob = distrib[,i])
      ech <- data[ech,]
      modele <- rpart(formula = formula,
                      data = ech,
                      control = rpart.control(maxdepth = maxDepth),
                      method = "class")
      
    }else{
      modele <- rpart(formula = formula,
                      data = data,
                      weights = distrib[,i],
                      control = rpart.control(maxdepth = maxDepth),
                      method = "class") 
    }
    listeArbres[[i]] <- modele
    
    prediction <- getPredictionRPart(arbre = modele, data = data)
    
    # Conversion des prédiction en -1 et +1
    prediction <- as.integer(prediction)
    
    erreur <- getError(data[,nColY], prediction, distrib[,i])
    
    sign <- rep(-1, m)
    sign[erreur$indTrue] <-  1
    
    alphas[i] <- 1/2*log((1 - erreur$error)/erreur$error)
    
    # Facteur de normalisation
    Z <- 2*sqrt(erreur$error*(1- erreur$error))
    
    if(i < nIter){
      distrib[,i+1] <- distrib[,i] / Z * exp(-sign * alphas[i])
    }
    
  }
  return(list(classifiers = listeArbres,
              factors = alphas,
              modalities = modalities))
}

predictAdaboost <- function(adaBooster, newdata){
  
  # Pour obtenir le label des modalité quand la colonne fournie est en factor
  if(is.factor(adaBooster$modalities)){
    adaBooster$modalities <- levels(adaBooster$modalities)
  }
  
  nbClassif <- length(adaBooster$factors)
  
  # Initialisation de la matrice des prédictions
  predictions <- matrix(nrow = nrow(newdata), ncol = nbClassif)
  
  for (i in 1:nbClassif){
    predictions[,i] <- getPredictionRPart(adaBooster$classifiers[[i]], newdata)
  }
  
  # Classification binaire
  if(length(adaBooster$modalities) == 2){
    # Convertit les "0" et "1" en -1 et 1
    predictions <- ifelse(predictions == "1", 1, -1)
    # Prédiction grâce au signe
    res <- sign(predictions%*%adaBooster$factors)
    # Recodage avec les modalités de départ
    res <- ifelse(res == -1, adaBooster$modalities[1], adaBooster$modalities[2])
  
  # Classification multinomiale
  }else{
    res <- c()
    for(ligne in 1:nrow(newdata)){
      sums <- c()
      for(mod in adaBooster$modalities){
        ind <- which(predictions[ligne,] == mod)
        sums <- c(sums, sum(log(1/adaBooster$factors[ind])))
      }
      res <- c(res, adaBooster$modalities[which.max(sums)])
    }
  }
  return(res)
}

