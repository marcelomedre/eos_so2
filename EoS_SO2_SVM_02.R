setwd("C:/Users/Marcelo/Documents/Marcelo/Pós-Doc/Dados/EoS SO2")

rm(list = ls())
# Function
rmse <- function(error)
{
  sqrt(mean(error^2))
}

# Loadind data sets
library(data.table)
library(dplyr)
library(ggplot2)
train <- read.table("data_train.txt", sep = " ", header = F, stringsAsFactors = F)
train <- train[,1:3]
names(train) <- c("temp", "pressao", "densid")

test <- read.table("data_test.txt", sep = " ", header = F, stringsAsFactors = F)
test <- test[,1:3]
names(test) <- c("temp", "pressao", "densid")

# train_norm <- as.data.frame(scale(train)) também funciona
# train_scaled <- train %>%
#  mutate_each_(funs(scale(.) %>% as.vector),
#               vars = c("temp", "pressao", "densid"))

#test_scaled <- test %>%
#  mutate_each_(funs(scale(.) %>% as.vector),
#               vars = c("temp", "pressao", "densid"))

library(caTools)
set.seed(123)
# Split trains into trains and val
split = sample.split(train$temp, SplitRatio = 0.85)

# Separating train and test datasets
train_nn <- subset(train, split == TRUE)
val_nn <- subset(train, split == FALSE)

library(e1071)

model_svm <- svm(densid ~ . , data = train_nn, scale = TRUE, kernel = 'radial',
                 cachesize = 400, cross = 5, epsilon = 0.2)

svm_val <- predict(model_svm, val_nn[1:2])

error <- val_nn$densid - svm_val
RMSE <- rmse(error)
RMSE

# tunning model
tuneResult <- tune(svm, densid ~ . , data = train_nn,scale = TRUE, kernel = 'radial',
                   cachesize = 400, ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9)))

print(tuneResult)
plot(tuneResult)

# Refining model
tuneResult <- tune(svm, densid ~ . , data = train_nn,scale = TRUE, kernel = 'radial',
                   cachesize = 400, ranges = list(epsilon = seq(0,0.4,0.01), cost = 2^(2:9)))

print(tuneResult)
plot(tuneResult)

tuneModel <- tuneResult$best.model
summary(tuneModel)

tuneModelY <- predict(tuneModel, val_nn[1:2])

errorTunedModel <- val_nn$densid - tuneModelY
tunedModelRMSE <-rmse(errorTunedModel)
tunedModelRMSE

results_val_tuned_Model <- as.data.frame(cbind(val_nn, tuneModelY))
names(results_val_tuned_Model) <- c("temp", "pressao", "densid", "test.svm")

ggplot(results_val_tuned_Model, aes(x = 1:nrow(val_nn), y = densid))+
  geom_point()+
  geom_point(aes(y = test.svm), col = "red")+
  xlab("Amostras Validação")+
  ylab("Densidade")

# Predicting density in the test data set
tunedModel_test <- predict(tuneModel, test[1:2])

error_test <- test$densid - tunedModel_test
error_testRMSE <-rmse(error_test)
error_testRMSE 

results_test <- as.data.frame(cbind(test, tunedModel_test))
names(results_test) <- c("temp", "pressao", "densid", "test.svm")

ggplot(results_test, aes(x = 1:nrow(test), y = densid))+
  geom_point()+
  geom_point(aes(y = test.svm), col = "red")+
  xlab("Amostras Teste")+
  ylab("Densidade / kg.m-3")

ggplot(results_test, aes(x = temp, y = densid))+
  geom_point()+
  geom_point(aes(y = test.svm), col = "red")+
  xlab("Temperatura / K")+
  ylab("Densidade / kg.m-3")

ggplot(results_test, aes(x = pressao, y = densid))+
  geom_point()+
  geom_point(aes(y = test.svm), col = "red")+
  xlab("Pressão / MPa")+
  ylab("Densidade / kg.m-3")

# tuning SVM trough parallelism

pkgs <-c("foreach", "doParallel")
lapply(pkgs, require, character.only = T)
registerDoParallel(cores = 4)
### PREPARE FOR THE DATA ###
df2 <- train
x <- paste("temp + pressao")
fml <- as.formula(paste("as.factor(densid) ~ ", x))
library(caret)
library(nloptr)
### SPLIT DATA INTO K FOLDS ###
set.seed(2016)
df2$fold <- caret::createFolds(1:nrow(df2), k = 4, list = FALSE)
### PARAMETER LIST ###
cost <- c(10, 100)
gamma <- c(1, 2)
parms <- expand.grid(cost = cost, gamma = gamma)
### LOOP THROUGH PARAMETER VALUES ###
result <- foreach(i = 1:nrow(parms), .combine = rbind) %do% {
  c <- parms[i, ]$cost
  g <- parms[i, ]$gamma
  ### K-FOLD VALIDATION ###
  out <- foreach(j = 1:max(df2$fold), .combine = rbind, .inorder = FALSE) %dopar% {
    deve <- df2[df2$fold != j, ]
    test <- df2[df2$fold == j, ]
    mdl <- e1071::svm(fml, data = deve, type = "eps-regression", kernel = "radial",
                      cost = c, gamma = g)
    pred <- predict(mdl, test, decision.values = TRUE, probability = TRUE)
    data.frame(y = test$DEFAULT, prob = attributes(pred)$probabilities[, 2])
  }
  ### CALCULATE SVM PERFORMANCE ###
  roc <- pROC::roc(as.factor(out$y), out$prob) 
  data.frame(parms[i, ], roc = roc$auc[1])
}

