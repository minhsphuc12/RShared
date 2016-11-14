steven=read.csv('stevens.csv')
library(caTools)
set.seed(3000)
split=sample.split(steven$Reverse,SplitRatio=0.7)
train=subset(steven,split==TRUE)
test=subset(steven,split==FALSE)

install.packages('rpart')
library(rpart)
install.packages('rpart.plot')

library(rpart.plot)
stevenTree=rpart(Reverse~Circuit+Issue+Petitioner + Respondent+LowerCourt+Unconst,data=train, method = 'class',control=rpart.control(minbucket=100))
prp(stevenTree)
predictCART=predict(stevenTree,newdata=test,type='class')
table(test$Reverse,predictCART)
#accuracy =0.658
library(ROCR)
PredictROC=predict(stevenTree,newdata=test)
PredictROC
pred=prediction(PredictROC[,2],test$Reverse)
perf=performance(pred,'tpr','fpr')
plot(perf)

auc=as.numeric(performance(pred,'auc')@y.values)

library(randomForest)
train$Reverse=as.factor(train$Reverse)
test$Reverse=as.factor(test$Reverse)
#convert variable to factor, to compatible with random forest
stevenForest=randomForest(Reverse~Circuit+Issue+Petitioner + Respondent+LowerCourt+Unconst,data=train, nodeize=25,ntree=200)

predictForest=predict(stevenForest,newdata=test)
table(test$Reverse,predictForest)
#accuracy 67%, higher than 65.9% of CART model
#(34+80)/(34+80+43+13)
install.packages('caret')
install.packages('e1071')
library(caret)
library(e1071)

fitControl=trainControl(method='cv',number=10)
#method 'cv' means cross validation, number 10 folds
cartGrid=expand.grid(.cp=(1:50)*0.01)
train(Reverse~Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst,data=train,method='rpart',trControl=fitControl,tuneGrid=cartGrid)

steventTreeCV=rpart(Reverse~Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst,method='class',data=train,control=rpart.control(cp=0.18))
predictCV=predict(steventTreeCV,newdata=test,type='class')
table(test$Reverse,predictCV)
#this one give accuracy 72.35%, better than 65.9% of minbucket value 25

prp(steventTreeCV)


