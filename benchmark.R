library(dplyr)
library(ggplot2)
library(rpart)
library(e1071)
library(randomForest)
source("D:/Repo_git/adaboostR/adaboost.R")

# Fonction qui découpe des données en échantillons de test et d'apprentissage
makeTT <- function(data, ratioTest = 1/3){
  n <- round(nrow(data)*ratioTest, 0)
  test <- sample(1:nrow(data), n, replace = F)
  
  return(list(train = data[-test,],
              test = data[test,]))
}

# importation des données
data("iris")
setwd("D:/Dropbox/M2/Advanced Supervised Learning/Projet/data")
glass <- read.table("glass.data", header = F, sep = ",")[,-1]
letters <- read.table("letters.data", header = F, sep = ",")
sonar <- read.table("sonar.data", header = F, sep = ",")
abalone <- read.table("abalone.data", header = F, sep = ",")
ionosphere <- read.table("ionosphere.data", header = F, sep = ",")[,-2]

# Renommage des colonnes à prédire
iris <- rename(iris, class = Species)
glass <- rename(glass, class = V11) %>%
  mutate(class = as.factor(class))
letters <- rename(letters, class = V1)
sonar <- rename(sonar, class = V61)
abalone <- rename(abalone, class = V1)
ionosphere <- rename(ionosphere, class = V35)

f <- class~.


##############################################
################# BENCHMARK ##################
##############################################



data <- sonar
bin <- length(unique(data$class)) == 2
TT <- makeTT(data)
n <- 5

###### CART ######
error_cart <- vector("numeric", n)
for(i in 1:n){
  tc <- trainControl("cv", 10)
  rpart.grid <- expand.grid(.cp = 0.2)
  train.rpart <- train(f, data = data, method = "rpart", trControl = tc, tuneGrid = rpart.grid)
  error_cart[i] <- as.numeric(1-train.rpart$results[2])
}
cat("Erreur de CART :", round(mean(error_cart)*100, 2), "%")

###### SVM ######
error_svm <- vector("numeric", n)
for(i in 1:n){
  svm <- svm(f, data, cross = 10)
  error_svm[i] <- 100 - svm$tot.accuracy
}
cat("Erreur des SVM :", round(mean(error_svm), 2), "%")


###### RF ######
error_rf <- vector("numeric", n)

for(i in 1:n){
  rf <- randomForest(f, data)
  error_rf[i] <- rf$err.rate[500,1]
}
cat("Erreur de la RF :", round(mean(error_rf)*100, 2), "%")


###### ADABOOST ######
# err_ada <- cv.adaboost(f, data, bin = T, nIter = 100, maxDepth = 30, nTimes = n, bootstrap = F)
# cat("Erreur d'Adaboost :", round(err_ada*100, 2), "%")

TT <- makeTT(data)
ada <- adaboostM1(f, TT$train, nIter = 10, maxDepth = 1)
pred <- predictAdaboost(ada, TT$test)
t <- table(real = TT$test$class, pred = pred)
t
1-sum(diag(t))/sum(t)



############# GRAPHS ###############
## nIter varie
depth <- 30
iterations <- seq(10, 150, 10)
df <- data.frame(nIter = iterations)

error <- c()
for(i in iterations){
  error <- c(error, cv.adaboost(f, data, bin, nIter = i, maxDepth = depth))
}
df$error <- error

ggplot(df) +
  geom_point(aes(x = nIter, y = error)) +
  geom_path(aes(x = nIter, y = error)) +
  theme_bw() + 

## maxDepth varie
depths <- 1:30
iteration <- 100
df <- data.frame(maxDepth = depths)

error <- c()
for(d in depths){
  error <- c(error, cv.adaboost(f, data, bin, nIter = iteration, maxDepth = d))
}
df$error <- error
