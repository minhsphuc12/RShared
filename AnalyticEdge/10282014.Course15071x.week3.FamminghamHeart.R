framingham = read.csv('framingham.csv')
library(caTools)
set.seed(1000)
split=sample.split(framingham$TenYearCHD,SplitRatio = 0.65)
train=subset(framingham,split==TRUE)
test=subset(framingham,split==FALSE)

framinghamLog=glm(TenYearCHD~ .,data=train,family=binomial)
summary(framinghamLog)

predictTest=predict(framinghamLog,type='response',newdat=test)
table(test$TenYearCHD,predictTest>0.5)
'''
    FALSE TRUE
  0  1069    6
  1   187   11
'''
library(ROCR)
ROCRPred=prediction(predictTest,test$TenYearCHD)
AUC=as.numeric(performance(ROCRPred,'auc')@y.values)
#74% --> pretty good