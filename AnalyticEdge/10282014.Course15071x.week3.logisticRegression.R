quality=read.csv('quality.csv')

install.packages('caTools')
library(caTools)

set.seed(88)
split=sample.split(quality$PoorCare,SplitRatio=0.75)
summary(split)
table(quality$PoorCare)
split
qualityTrain=subset(quality,split==TRUE)
qualityTest=subset(quality,split==FALSE)
nrow(qualityTrain)

qualityLog=glm(PoorCare~OfficeVisits+Narcotics,data=qualityTrain,family=binomial)
summary(qualityLog)
#AIC is the similar metrics with R-square, but only
#used between model of same dataset
#prefered model is the one with minimum AIC

predictionTrain=predict(qualityLog,type='response')
summary(predictionTrain)

tapply(predictionTrain,qualityTrain$PoorCare,mean)
#prediction gives 18.94% 0, which is good care

table(qualityTest$PoorCare)

qualityLog2 = glm(PoorCare ~ OfficeVisits + StartedOnCombination, data=qualityTrain, family=binomial)
summary(qualityLog2)
'''
A model with a higher threshold will 
have a lower sensitivity
and a higher specificity.
A model with a lower threshold will 
have a higher sensitivity
and a lower specificity.
'''

table(qualityTrain$PoorCare,predictionTrain>0.5)
'''
FALSE TRUE
0    70-TN    4-FP
1    15-FN   10 TP
sensitivity = TP / (TP + FN)= 0.4
specificity = TN / (TN + FP)= ~0.95

'''
#different threshold
table(qualityTrain$PoorCare,predictionTrain>0.7)
'''
    FALSE TRUE
  0    73    1
  1    17    8
sensitivity = TP / (TP + FN)= 0.32
specificity = TN / (TN + FP)= ~0.99

'''
#inverse case when lower threshold


#ROC Curve
install.packages('ROCR')
library(ROCR)
ROCRPred=prediction(predictionTrain,qualityTrain$PoorCare)
ROCRperf=performance(ROCRPred,'tpr','fpr')
plot(ROCRperf,colorize=TRUE,print.cutoffs.at=seq(0,1,0.2),text.adj=c(-0.2,1.7))
plot(ROCRperf,colorize=TRUE,print.cutoffs.at=seq(0,1,0.2),text.adj=c(-0.2,1))

predictTest = predict(qualityLog, type="response", newdata=qualityTest)
ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc

plot(performance(ROCRpredTest, "auc"))
