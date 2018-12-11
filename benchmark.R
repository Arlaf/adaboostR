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
setwd("D:/Dropbox/M2/Advanced Supervised Learning/Projet/data")
glass <- read.table("glass.data", header = F, sep = ",")[,-1]
letters <- read.table("letters.data", header = F, sep = ",")
sonar <- read.table("sonar.data", header = F, sep = ",")
abalone <- read.table("abalone.data", header = F, sep = ",")
ionosphere <- read.table("ionosphere.data", header = F, sep = ",")[,-2]

# Renommage des colonnes à prédire
glass <- rename(glass, class = V11) %>%
  mutate(class = as.factor(class))
letters <- rename(letters, class = V1)
sonar <- rename(sonar, class = V61)
abalone <- rename(abalone, class = V1)
ionosphere <- rename(ionosphere, class = V35)
datasets <- list(glass, letters, sonar, abalone, ionosphere)
names <- c("Glass", "Letters", "Sonar", "Abalone", "Ionosphere")

f <- class~.

df <- data.frame(model = rep(c("CART", "SVM", "RF", "Adaboost"), 5),
                 data = c(rep("Glass", 4), rep("Letters", 4), rep("Sonar", 4), rep("Abalone", 4), rep("Ionosphere", 4)),
                 error = NA)

##############################################
data <- ionosphere
bin <- length(unique(data$class)) == 2
cv.adaboost(f, data, bin, nIter = 50, maxDepth = 10, nTimes = 1)
# glass ; 100 ; 30 : 0.2333141
# glass ; 50 ; 30 : 0.2285455
# glass ; 100 ; 15 : 0.2048707
# glass ; 100 ; 10 : 0.2095577
# glass ; 50 ; 15 : 0.2232035
# glass ; 75 ; 10 : 0.2033896
# glass ; 25 ; 10 : 0.2410292


################# BENCHMARK ##################
##############################################
n <- 5

for(j in 1:length(names)){
  name <- names[j]
  print(name)
  data <- datasets[[j]]
  
  ###### CART ######
  print("CART")
  error_cart <- vector("numeric", n)
  for(i in 1:n){
    tc <- trainControl("cv", 10)
    rpart.grid <- expand.grid(.cp = 0.2)
    train.rpart <- train(f, data = data, method = "rpart", trControl = tc, tuneGrid = rpart.grid)
    error_cart[i] <- as.numeric(1-train.rpart$results[2])
  }
  df$error[df$model == "CART" & df$data == name] <- round(mean(error_cart)*100, 2)
  # cat("Erreur de CART :", df$error[df$model == "CART" & df$data == name], "%")
  
  ###### SVM ######
  print("SVM")
  error_svm <- vector("numeric", n)
  for(i in 1:n){
    svm <- svm(f, data, cross = 10)
    error_svm[i] <- 100 - svm$tot.accuracy
  }
  df$error[df$model == "SVM" & df$data == name] <- round(mean(error_svm, 2))
  # cat("Erreur des SVM :", df$error[df$model == "SVM" & df$data == name], "%")
  
  ###### RF ######
  print("RF")
  error_rf <- vector("numeric", n)
  for(i in 1:n){
    rf <- randomForest(f, data)
    error_rf[i] <- rf$err.rate[500,1]
  }
  df$error[df$model == "RF" & df$data == name] <- round(mean(error_rf)*100, 2)
  # cat("Erreur de la RF :", df$error[df$model == "RF" & df$data == name], "%")
}

###### ADABOOST ######
for(j in 1:length(names)){
  name <- names[j]
  if(name != "Letters"){
    print(name)
    data <- datasets[[j]]
    bin <- length(unique(data$class)) == 2
    df$error[df$model == "Adaboost" & df$data == name] <- round(100*cv.adaboost(f, data, bin, nIter = 50, maxDepth = 30, nTimes = n), 2)
    # cat("Erreur d'Adaboost :", df$error[df$model == "Adaboost" & df$data == name], "%")
  }
}

############# GRAPHS ###############
## Diagramme en batons
ggplot(df) +
  geom_bar(aes(x = data, y = error, fill = model), stat = "identity", position = "dodge") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  scale_fill_manual(values = c("#1D74B2", "#0F9BFF", "#CCCC21", "#FF2C29")) +
  labs(y = "Erreur (%)",
       x = "",
       fill = "Modèle",
       title = "Taux d'erreur des différents modèles")

## nIter varie
tictoc::tic()
data <- sonar
depth <- 30
iterations <- seq(10, 120, 10)
df2 <- data.frame(nIter = iterations)

error2 <- c()
for(i in iterations){
  error2 <- c(error2, cv.adaboost(f, data, bin, nIter = i, maxDepth = depth))
}
df2$error <- error2

# ggplot(df2) +
#   geom_point(aes(x = nIter, y = error)) +
#   geom_path(aes(x = nIter, y = error)) +
#   theme_bw()

  
## maxDepth varie
depths <- 1:30
iteration <- 100
df3 <- data.frame(maxDepth = depths)

error3 <- c()
for(d in depths){
  error3 <- c(error3, cv.adaboost(f, data, bin, nIter = iteration, maxDepth = d))
}
df3$error <- error3
tictoc::toc()

write.csv2(df3,"df3.csv")
