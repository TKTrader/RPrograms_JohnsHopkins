## Machine Learning Course Project
data.train <- read.csv("pml-training.csv", stringsAsFactors = FALSE, header=TRUE, na.strings=c("NA","#DIV/0!",""))  ## import csv
data.test <- read.csv("pml-testing.csv", stringsAsFactors = FALSE, header=TRUE, na.strings=c("NA","#DIV/0!",""))  ## import csv
library(caret)
library(dplyr)
library(rpart)   ## for classification tree model
library(rattle)  ## for classification tree graphics
library(randomForest) ## for random forest model
## remove first 7 columns
data.train <- select(data.train, -(1:7))
data.test <- select(data.test, -(1:7))
## remove zero covariates
nzv <- nearZeroVar(data.train)
data.train <- data.train[, -nzv]
## eliminate columns with all NA values
var.comp <- names(data.train[,colSums(is.na(data.train)) == 0])
data.train <- data.train[, var.comp]
p <- data.test[, -nzv]

## separate data into subsets for decision tree
set.seed(412)
sub <- createDataPartition(data.train$classe, p = .7,list=FALSE) 
data.train.1 <- data.train[sub,]
data.train.2 <- data.train[-sub,]

## classification tree model
model.tree <- train(classe ~ .,method="rpart",data=data.train.1)
fancyRpartPlot(model.tree$finalModel)
tree.predict <- predict(model.tree,newdata=data.train.2)
confusionMatrix(tree.predict, data.train.2$classe)
## 49.8 % accuracy no good, eliminate tree model

## GBM model
model.gbm <- train(classe ~ .,method="gbm",data=data.train.1,verbose=FALSE)
gbm.predict2 <- predict(model.gbm,newdata=data.train.2,n.trees=150,interaction.depth = 3, shrinkage = 0.1, n.minobsinnode = 10)
gbm.predict <- predict(model.gbm,newdata=data.train.2)
confusionMatrix(gbm.predict, data.train.2$classe)

## random forest model, use all data with 3 fold cross validation
model.rf <- train(classe ~ .,method="rf",ntree=200,data=data.train,trControl=trainControl(method="cv",number=3))
model.rf
model.rf$finalModel
#predict on test set
rf.predict <- predict(model.rf,data.test)
## OOB estimate of error = .43%;  99.3% accuracy

