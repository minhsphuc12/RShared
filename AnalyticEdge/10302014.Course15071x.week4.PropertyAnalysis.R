boston=read.csv('boston.csv')
plot(boston$LON,boston$LAT)
points(boston$LON[boston$CHAS==1],boston$LAT[boston$CHAS==1],col='blue',pch=15)

summary(boston$NOX)

points(boston$LON[boston$NOX>=0.55],boston$LAT[boston$NOX>=0.55],col='yellow',pch=15)
points(boston$LON[boston$TRACT==3531],boston$LAT[boston$TRACT==3531],col='red',pch=15)

plot(boston$LON,boston$LAT)
points(boston$LON[boston$MEDV>=21.21],boston$LAT[boston$MEDV>=21.21],col='red',pch=1)
help(points)

latlonlm=lm(MEDV~LAT+LON,data=boston)
summary(latlonlm)

points(boston$LON[latlonlm$fitted.values>21.2],boston$LAT[latlonlm$fitted.values>21.2],col='blue',pch = '$' )
library(rpart)
library(rpart.plot)
latlontree=rpart(MEDV~LAT+LON,data=boston)
summary(latlontree)
prp(latlontree)

plot(boston$LON,boston$LAT)
points(boston$LON[boston$MEDV>=21.21],boston$LAT[boston$MEDV>=21.21],col='red',pch=1)
fittedvalues=predict(latlontree)
points(boston$LON[fittedvalues>=21.2],boston$LAT[fittedvalues>=21.2],col='blue',pch='-')

latlontree2=rpart(MEDV~LAT+LON,data=boston,minbucket=50)
prp(latlontree2)
text(latlontree2)
plot(boston$LON,boston$LAT)
abline(v=-71.07)
abline(h=42.21)
abline(h=42.17)
points(boston$LON[boston$MEDV>=21.21],boston$LAT[boston$MEDV>=21.21],col='red',pch=1)

#in here we split the map based on information of tree


points(boston$LON[boston$MEDV>=21.21],boston$LAT[boston$MEDV>=21.21],col='red',pch=1)
fittedvalues2=predict(latlontree2)
points(boston$LON[fittedvalue2s>=21.2],boston$LAT[fittedvalues2>=21.2],col='blue',pch='-')

#THIS SECTION IS ABOUT PREDICTIVE ANALYSIS USING TREE REGRESSION

library(caTools)
set.seed(123)
split=sample.split(boston$MEDV,SplitRatio=0.7)
train=subset(boston,split==TRUE)
test=subset(boston,split==FALSE)
linreg=lm(MEDV~LAT+LON+CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+DIS+RAD+TAX+PTRATIO,data=train)
summary(linreg)

linreg.pred=predict(linreg,newdata=test)
linreg.sse=sum((linreg.pred - test$MEDV)^2)
linreg.sse
tree=rpart(MEDV~LAT+LON+CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+DIS+RAD+TAX+PTRATIO,data=train)
prp(tree)
tree.pred=predict(tree,newdata=test)
tree.sse=sum((tree.pred-test$MEDV)^2)
tree.sse
#4328 of tree larger than 3037 of linear reg


#USE CROSS VALIDATION TO OPTIMIZE PARAMETER
library(caret)
library(e1071)
train_control=trainControl(method='cv',number=10)
cp.grid=expand.grid(.cp=(0:10)*0.001)
tr=train(MEDV~LAT+LON+CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+DIS+RAD+TAX+PTRATIO,data=train,method='rpart',trControl=train_control,tuneGrid=cp.grid) 

tr
#using tr$finalModel is to 
#pick best one with lowest RMSE on tr list
#which with cp 0.001 (close to 0, mean tree will be big)

best.tree=tr$finalModel
prp(best.tree)
best.tree.pred=predict(best.tree,newdata=test)
best.tree.sse=sum((best.tree.pred-test$MEDV)^2)
best.tree.sse
#3655, not very good yet, but better than tree

