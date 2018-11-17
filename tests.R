

require(rpart)

#changer l'achitecture, on a besoin de tous nos objets rpart donc il faudrait que get predictionrpart prenne en argument un objet rpart

#remarque : selon intro to boosting le calcul de l'erreur a l'air de d?pendre de la distrib

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
#apr?s simulation de 100 jeux de donn?es alatoires on a 26,5% de taux d'erreur moyen



###### Code pour tester ######

formula <- X61~.
arb <- rpart(formula, app)
test2 <- test
test2$arb <- sign(predict(arb, test))

ada <- adaboostBin(formula, app, maxDepth = 5)
test2$ada <- predictAdaboost(ada, test)

table(test2$X61, test2$arb)
table(test2$X61, test2$ada)
