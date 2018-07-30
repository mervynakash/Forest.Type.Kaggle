set.seed(123)
setwd("E:/Kaggle/ForestCoverType/")

library(data.table)
library(dplyr)
library(randomForest)
library(class)
library(BBmisc)
library(rpart)
library(e1071)
library(earth)
library(glmnet)
library(caret)
library(VGAM)
library(MASS)

forest.train = fread("train.csv")
forest.test = fread("test.csv")

dim(forest.train)
dim(forest.test)

#=================== Data Partition =============================#
inTrain = createDataPartition(forest.train$Id, p = 0.75, list = F)
forest.new.train = forest.train[inTrain,]
forest.new.test = forest.train[-inTrain,]


#=================== Decision Tree ==============================#

model_dt = rpart(Cover_Type~., data=forest.new.train, control = rpart.control(cp = 0),method = "anova")
printcp(model_dt)

cpval = model_dt$cptable[names(which.min(model_dt$cptable[,"xerror"])),"CP"]

model_dt = prune(model_dt, cp = cpval)

dtpred = predict(model_dt, forest.new.test)
dtpred = round(dtpred)

mean(dtpred == forest.new.test$Cover_Type)


#==================== Linear Discriminant Analysis ==============#

model_lda = lda(as.factor(Cover_Type)~., data = forest.new.train)
