polling=read.csv('PollingData.csv')

install.packages('mice')
library(mice)

simple_poll=polling[c('Rasmussen','SurveyUSA', 'PropR',
                      'DiffCount')]
#reduce variables amount

set.seed(144)
imputed=complete(mice(simple_poll))
#replace NA or missing values#replace NA and b
polling$Rasmussen=imputed$Rasmussen
polling$SurveyUSA=imputed$SurveyUSA
#put back new data column to original dataframe

polling
train=subset(polling,Year==2004|Year==2008)
test=subset(polling,Year==2012)

sign(20)
table(sign(train$Rasmussen))
cor(train[c('Rasmussen','SurveyUSA','PropR','Republican')])
'''
           Rasmussen SurveyUSA     PropR Republican
Rasmussen  1.0000000 0.9194508 0.8404803  0.8021191
SurveyUSA  0.9194508 1.0000000 0.8756581  0.8205806
PropR      0.8404803 0.8756581 1.0000000  0.9484204
Republican 0.8021191 0.8205806 0.9484204  1.0000000

This result show correlation between pairs of variable
We pick PropR, with highest correlation, to build model to predict Republican
'''

model1 = glm(Republican~PropR,data=train,family='binomial')
summary(model1)

predict1=predict(model1,type='response')
table(train$Republican,predict1 >= 0.5)

model2 = glm(Republican~SurveyUSA+DiffCount,data=train,family='binomial')
summary(model2)

predict2=predict(model2,type='response')
table(train$Republican,predict2 >= 0.5)
#check accuracy of model 2 with simple baseline model

table(test$Republican,sign(test$Rasmussen))
'''
    -1  0  1
  0 18  2  4
  1  0  0 21
18 predict Democrat and Democrat win
21 predict Republican and Republican win
2 in conlusive
4 errors
'''
testPredict=predict(model2,newdata=test,type='response')
table(test$Republican,testPredict>=0.5)

subset(test,testPredict>=0.5 & Republican == 0)
