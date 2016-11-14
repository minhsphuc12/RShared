library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(y=spam$type,
                               p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training)

set.seed(32343)
modelFit <- train(type ~.,data=training, method="glm")
modelFit

modelFit$finalModel

#modelFit2=train(type~our|address,data=training,method='glm')
#modelFit2

predictions <- predict(modelFit,newdata=testing)
predictions

set.seed(32323)
folds <- createFolds(y=spam$type,k=10,
                     list=TRUE,returnTrain=TRUE)
sapply(folds,length)


a=c(1,3,4,6,7,4,3,6,8,4)
b=createFolds(y=a,k=3)
b=createFolds(y=a,k=3,list=TRUE,returnTrain=TRUE)

set.seed(32323)
folds <- createResample(y=spam$type,times=10,
                        list=TRUE)
sapply(folds,length)

set.seed(32323)
tme <- 1:1000
folds <- createTimeSlices(y=tme,initialWindow=20,
                          horizon=10)
names(folds)


library(ISLR); library(ggplot2); library(caret);
data(Wage)
summary(Wage)

#Feature plot (caret package)
featurePlot(x=training[,c("age","education","jobclass")],
            y = training$wage,
            plot="pairs")

#Qplot with color (ggplot2 package)
qplot(age,wage,data=training)
qplot(age,wage,colour=jobclass,data=training)

#Add regression smoothers (ggplot2 package)
qq <- qplot(age,wage,colour=education,data=training)
qq +  geom_smooth(method='lm',formula=y~x)


#cut2, making factors (Hmisc package)
library(Hmisc)
cutWage <- cut2(training$wage,g=3)
table(cutWage)

#Boxplots with cut2
p1 <- qplot(cutWage,age, data=training,fill=cutWage,
            geom=c("boxplot"))
p1

#Boxplots with points overlayed
p2 <- qplot(cutWage,age, data=training,fill=cutWage,
            geom=c("boxplot","jitter"))
grid.arrange(p1,p2,ncol=2)

#Tables
t1 <- table(cutWage,training$jobclass)
t1
prop.table(t1,1)

#Density plots
qplot(wage,colour=education,data=training,geom="density")


#machine learning with regression
library(caret);data()







