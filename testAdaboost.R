require(rpart)

#de mani�re g�n�rale il y a des problemes lorsque erreur nulle

#avec le dataset sonar
sonar <- read.csv("~/GIT/adaboostR/sonar.all-data", header=FALSE)

adaboostM1(V61~.,sonar,nIter = 5,maxDepth=5)

#avec le dataset plrx de kaggle
plrx <- read.delim("~/GIT/adaboostR/plrx.txt", header=FALSE)
plrx=plrx[,-14]
plrx$V13[which(plrx$V13=="1")]="0"
plrx$V13[which(plrx$V13=="2")]="1"

adaboostM1(V13~.,plrx,nIter=5,maxDepth=1)

# donn�es al�atoires
data=matrix(runif(1000),ncol=10)
data[,1]=rbinom(100,1,0.5)
data=data.frame(data)
data$X1=as.factor(data$X1)

adaboostM1(X1~.,data,5)

#iris biclasse
data(iris)
iris=iris[iris$Species!="virginica",]
iris$Species=factor(iris$Species)

adaboostM1(Species~.,iris,3)

#mettre quelque part une v�rification pour que le nombre de classes a pr�dire ne pose pas probl�me

getPredictionRPart=function(formule,data,poids,maxDepth){
  m=nrow(data)
  environment(formule) <- environment() #pour eviter le probleme des poids nons trouv�s
  modele=rpart(formula=formule,data=data,weights=poids,control = rpart.control(maxdepth = maxDepth))
  pred=predict(modele,newdata=data)
  classes=colnames(pred)
  prediction=vector(length=m)
  for (i in 1:m){
    prediction[i]=classes[which.max(pred[i,])]
  }
  return(prediction)
}

#trouver un meilleur nom : cette fonction renvoie l'erreur mais aussi les indices des bons classements
getError=function(data1,data2){
  stopifnot(length(data1)==length(data2))
  indTrue=which(data1==data2)
  #ce qui suit est sale
  res=NA
  res$indTrue=indTrue
  res$error=1-(length(indTrue)/length(data1))
  return(res)
}

adaboostM1=function(formula,data,nIter=10,maxDepth=1){
  m=nrow(data)
  nColY=which(colnames(data)==all.vars(formula)[1]) #numero de colonne o� se trouve Y dans data
  distrib=matrix(0,nrow=m,ncol=nIter)
  distrib[,1]=1/m
  for (i in 1:nIter){
    prediction=getPredictionRPart(formule=formula,data=data,poids=distrib[,i],maxDepth=maxDepth)
    erreur=getError(data[,nColY],prediction)
    if(erreur$error>0.5){
      nIter=i-1
      return("pas cool man")
    }
    beta=erreur$error/(1-erreur$error)
    if(i<nIter){ #pas besoin de la distrib nIter+1
      Z=2*sqrt(beta) #facteur de normalisation trouv� sur wiki
      distrib[,(i+1)]=distrib[,i]/Z
      distrib[erreur$indTrue,(i+1)]=distrib[erreur$indTrue,(i+1)]*beta
    }
  }
  #� finir
  return(distrib)
}


getPredictionRPart(X1~.,data,runif(100),2)
