setwd("C:/Users/Marcelo/Desktop/EoS SO2/")

rm(list = ls())

# Loadind data sets
library(data.table)
library(dplyr)
train <- read.table("data_train.txt", sep = " ", header = F, stringsAsFactors = F)
train <- train[,1:3]
names(train) <- c("temp", "pressao", "densid")

test <- read.table("data_test.txt", sep = " ", header = F, stringsAsFactors = F)
test <- test[,1:3]
names(test) <- c("temp", "pressao", "densid")

# train_norm <- as.data.frame(scale(train)) também funciona
train_scaled <- train %>%
  mutate_each_(funs(scale(.) %>% as.vector),
               vars = c("temp", "pressao", "densid"))

library(caTools)
set.seed(123)
# Split trains into trains and val
split = sample.split(train_scaled$temp, SplitRatio = 0.85)

# Separating train and test datasets
train_nn <- subset(train_scaled, split == TRUE)
val_nn <- subset(train_scaled, split == FALSE)

# The neuralnetwork() function won't accept the typical decimal R format
# for a formula involving all features (e.g. y ~.)

# simple script to create the expanded formula and save us some typing:

feats <- names(train_scaled)
# concatenated strings
f <- paste(feats, collapse = " + ")
f <- as.formula(paste("densid ~", paste(feats[!feats %in% c("densid")], collapse = " + ")))

f

library(neuralnet)

nn <- neuralnet(f, train_nn, hidden = 5, linear.output = T)

plot(nn)


# Compute Predictions off Test Set
predict.nn.values <- compute(nn, val_nn[1:2])
print(head(predict.nn.values$net.result))

predict.nn <- predict.nn.values$net.result
densid_val <- as.data.frame(val_nn[,3])

# MSE val
MSE.nn <- sum((densid_val - predict.nn)^2)/nrow(val_nn)
MSE.nn

results_val <- as.data.frame(cbind(val_nn, predict.nn))

library(ggplot2)
png('ANN_validation_vs_Densidade_exp.png')
ggplot(results_val, aes(x = 1:42, y = densid))+
  geom_point()+
  geom_point(aes(y = predict.nn), col = "red")
dev.off()

###############################################################################
# testing MLP with several hidden neurons
# create progress bar

total <- 10
# create progress bar
pb <- txtProgressBar(min = 0, max = total, style = 3)
for(i in 1:total){
  Sys.sleep(0.1)
  nn <- neuralnet(f, data = train_nn, hidden = i, linear.output = TRUE)
  plot(nn)
  # predicting using NN and val_nn data set
  pr.nn.val <- compute(nn, val_nn[,1:2])
  # backnormalizing SalePrice variable
  pr.nn.r <- pr.nn.val$net.result
  
  # MSE NN model
  MSE.nn <- sum((densid_val - pr.nn.r)^2)/nrow(val_nn)
  print(c(i, MSE.nn))
  
  # Visual plot NN computed values vs val dataset
  ggplot(results_val, aes(x = 1:42, y = densid_val))+
    geom_point()+
    geom_point(aes(y = pr.nn.r), col = "red")
  
  # update progress bar
  setTxtProgressBar(pb, i)
}
close(pb)

###############################################################################

best_nn <- neuralnet(f, train_nn, hidden = 8, linear.output = T)

plot(best_nn)

# Compute Predictions Val Set
predict.nn.values <- compute(best_nn, val_nn[,1:2])
predict.nn <- predict.nn.values$net.result

# MSE val
MSE.nn <- sum((densid_val - predict.nn)^2)/nrow(val_nn)
MSE.nn

results_val <- as.data.frame(cbind(val_nn, predict.nn))

library(ggplot2)
png('ANN_validation_vs_Densidade_exp.png')
ggplot(results_val, aes(x = 1:42, y = densid))+
  geom_point()+
  geom_point(aes(y = predict.nn), col = "red")
dev.off()

#############################
# Testing best_nn test data set

test_scaled <- test %>%
  mutate_each_(funs(scale(.) %>% as.vector),
               vars = c("temp", "pressao", "densid"))

# Compute Predictions test Set
test.nn.values <- compute(best_nn, test_scaled[,1:2])
test.nn <- test.nn.values$net.result

# MSE test
densid_test <- as.data.frame(test_scaled[,3])
MSE.nn.te <- sum((densid_test - test.nn)^2)/nrow(densid_test)
MSE.nn.te

results_test <- as.data.frame(cbind(densid_test, test.nn))
names(results_test) <- c("densid", "test.nn")

library(ggplot2)
png('ANN_Teste_vs_Densidade_exp.png')
ggplot(results_test, aes(x = 1:nrow(densid_test), y = densid))+
  geom_point()+
  geom_point(aes(y = test.nn), col = "red")
dev.off()
