dataframe1=data.frame(c(0,1,1),c(2,2,8))
plot(dataframe1)
colnames(dataframe1)=c('x','y')
res=lm(y~x,dataframe1)
abline(res)
summary(res)
anova(res)

wine = read.csv('wine.csv')
str(wine)
model_linear=lm(Price~AGST,data=wine)
summary(model_linear)
SSE1=sum(model_linear$residuals^2)

plot(Price~AGST,data=wine)
abline(model_linear)
class(SSE1)
help(type)

model_linear2=lm(Price ~AGST +HarvestRain,data=wine)
plot(Price~AGST+HarvestRain,data=wine)
#this is not work :v
summary(model_linear2)
abline(model_linear2)
SSE2=sum(model_linear2$residuals^2)
SSE2

model_linear3=lm(Price~AGST+HarvestRain++WinterRain+Age+FrancePop,data=wine)
summary(model_linear3)
SSE3=sum(model_linear3$residuals^2)

model_linear4=lm(Price~HarvestRain+WinterRain,data=wine)
summary(model_linear4)
SSE4=sum(model_linear4$residuals^2)

#correlation
cor(wine, use="complete.obs", method="kendall") 
cor(wine$HarvestRain,wine$WinterRain)
#two of those above result differently

#prediction
wineTest=read.csv('wine_test.csv')
str(wineTest)
predictTest=predict(model_linear,newdata=wineTest)
predictTest
SSE=sum((wineTest$Price-predictTest)^2)
SST=sum((wineTest$Price-mean(wine$Price))^2)
1-SSE/SST
#this is R^2 of prediction model

