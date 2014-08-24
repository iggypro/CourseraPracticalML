# PracticalML - Course Project
#### Igor Protsenko | Aug 18, 2014
==================================



## Executive Summary
Random Forest algorithm has shown 99% accuracy, when splitting training set into two subsets for cross validation. Based on this accuracy check Random Forest has been selected as a model for Test set prediction. Near zero variables and variables with high NA rates have been excluded from prediction model.

## Data Processing

```r
# download data from URLs
train <- read.csv(textConnection(getURL(trainURL)))
test <- read.csv(textConnection(getURL(testURL)))
# remove near zero variables
test <- test[,!nearZeroVar(train,saveMetrics=T)$nzv]
train <- train[,!nearZeroVar(train,saveMetrics=T)$nzv]
# remove variables with high percentage of NA's
naRate <- sapply(colnames(train), function(x) sum(is.na(train[, x]))/nrow(train))
unique(naRate)
```

```
## [1] 0.0000 0.9793
```

```r
# as we have either 0 or 0.98 rate values, we need to remove all variables with naRate > 0
test <- test[,which(naRate==0)]
train <- train[,which(naRate==0)]
```

## Cross-Validation and Out-of-Sample error estimation

```r
# in order to estimate model accuracy and out-of-sample error, let's split the training set
samp <- sample(as.numeric(row.names(train)),length(row.names(train))*0.4)
# 60% of training set will be used for model training
crossTrain <- train[-samp,]
# 40% of training set will be used for cross validation and out-of-sample error estimation
crossTest <- train[samp,]
# let's cross-validate random forest model
model <- randomForest(classe~.,data = crossTrain,importance=TRUE,keep.forest=TRUE)
prediction <- predict(model,newdata=crossTest)
cm <- confusionMatrix(prediction,crossTest$classe)
cm$table
```

```
##           Reference
## Prediction    A    B    C    D    E
##          A 2257    0    0    0    0
##          B    0 1524    0    0    0
##          C    0    0 1318    0    0
##          D    0    0    0 1308    0
##          E    0    0    0    0 1441
```

Cross Validation demonstrated **100%** accuracy for Random Forest model, so we will proceed with this model on the 100% training set.

## Random Forest prediction based on 100% of Training set

```r
model <- randomForest(classe~.,data = train,importance=TRUE,keep.forest=TRUE)
prediction <- predict(model,newdata=crossTest)
```
