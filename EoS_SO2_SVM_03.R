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

# fulldata

fulldata <- rbind(train, test)

# train_norm <- as.data.frame(scale(train)) também funciona
# train_scaled <- train %>%
#  mutate_each_(funs(scale(.) %>% as.vector),
#               vars = c("temp", "pressao", "densid"))

#test_scaled <- test %>%
#  mutate_each_(funs(scale(.) %>% as.vector),
#               vars = c("temp", "pressao", "densid"))

library(caTools)
set.seed(123)
# Split fulldata into train and test
split = sample.split(fulldata$temp, SplitRatio = 0.75)
train <- subset(fulldata, split == TRUE)
test <- subset(fulldata, split == FALSE)

# Separating train and test datasets
split2 = sample.split(train$temp, SplitRatio = 0.85)
train_nn <- subset(train, split2 == TRUE)
val_nn <- subset(train, split2 == FALSE)

library(e1071)

system.time(model_svm <- svm(densid ~ . , data = train_nn, scale = TRUE, kernel = 'radial',
                 cachesize = 400, cross = 5, epsilon = 0.2))

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

# tuning SVM

costs <- seq(from = 1, to = 500, by = 1)
pseudor2 <- double(length(costs))
gamma <- seq(from = 1, to = 2, by = 1)
for (c in 1:length(cost)){
  tuned_svm <- svm(densid ~ . , data = train_nn, scale = TRUE, kernel = 'radial',
                    cachesize = 800, cost = costs[c], epsilon = 0.3, gamma = 0.5,
                   shirinking = TRUE, tolerance = 0.001, fitted = TRUE)
  #prediction
  pred.tuned_svm <- predict(tuned_svm, val_nn[1:2])
  errorTunedModel <- sum((val_nn$densid - pred.tuned_svm)^2)
  RMSE <-rmse(errorTunedModel)
  pseudor2[c] <- RMSE
}

plot(costs, pseudor2, type = "l")
