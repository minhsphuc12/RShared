nba = read.csv('NBA_train.csv')
View(nba)
table(nba$W,nba$Playoffs)
nba$PTSdiff=nba$PTS-nba$oppPTS
plot(nba$PTSdiff~nba$W)
WinsReg=lm(W~PTSdiff,data=nba)
summary(WinsReg)

PointsReg=lm(PTS~X2PA+X3PA+FTA+AST+ORB+DRB+TOV+STL+BLK,data=nba)
summary(PointsReg)

SSE=sum(PointsReg$residuals^2)
#result 28394313, not very useful

RMSE = sqrt(SSE/nrow(nba))
#184.4
mean(nba$PTS)
#compare with mean, this root mean deviation is not very bad

PointsReg2=lm(PTS~X2PA+X3PA+FTA+AST+ORB+DRB+STL+BLK,data=nba)
summary(PointsReg2)
#remove TOV

PointsReg3=lm(PTS~X2PA+X3PA+FTA+AST+ORB+STL+BLK,data=nba)
#remove defense rebount, DRB
summary(PointsReg3)

PointsReg4=lm(PTS~X2PA+X3PA+FTA+AST+ORB+STL,data=nba)
#remove BLK, block
summary(PointsReg4)
SSE4=sum(PointsReg4$residuals^2)
RMSE4=sqrt(SSE4/nrow(nba))

nba_test=read.csv('NBA_test.csv')
PointsPredictions=predict(PointsReg4,newdata=nba_test)
SSEz=sum((PointsPredictions-nba_test$PTS)^2)
SSTz=sum((mean(nba$PTS)-nba_test$PTS)^2)
R2=1-SSEz/SSTz
RMSE2=sqrt(SSEz/nrow(nba_test))
#196, a bit higher than in-train model

summary(PointsReg4)$coefficients[,4]
#get p value of all independent variables
summary(PointsReg4)$r.squared
#get the R-squared








