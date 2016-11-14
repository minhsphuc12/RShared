# Example 3: Loan Acceptance
library(car) ## needed to recode variables
set.seed(1)

loan <- read.csv("C:/DataMining/Data/UniversalBank.csv")
loan[1:3,]

## familiarize yourself with the data
hist(loan$Age)
hist(loan$Experience)
hist(loan$Income)
hist(loan$Family) ## below we treat loan$Family as categorical
hist(loan$CCAvg) 
hist(loan$Mortgage)
hist(loan$SecuritiesAccount)
hist(loan$CDAccount)
hist(loan$Online)
hist(loan$CreditCard)
hist(loan$Education) ## below we treat loan$Education as categorical
response=loan$PersonalLoan
hist(response)
MeanRes=mean(response) 
MeanRes

## creating indicator variables for loan$Family and loan$Education
v1=rep(1,dim(loan)[1])
v2=rep(0,dim(loan)[1])
## creating indicator variables for family size (4 groups: 1, 2, 3, 4)
loan$FamSize2=ifelse(loan$Family==2,v1,v2)
loan$FamSize3=ifelse(loan$Family==3,v1,v2)
loan$FamSize4=ifelse(loan$Family==4,v1,v2)
## creating indicator variables for education level (3 groups: 1, 2, 3)
loan$Educ2=ifelse(loan$Education==2,v1,v2)
loan$Educ3=ifelse(loan$Education==3,v1,v2)

xx=cbind(response,Age=loan$Age,Exp=loan$Experience,Inc=loan$Income,Fam2=loan$FamSize2,Fam3=loan$FamSize3,Fam4=loan$FamSize4,CCAve=loan$CCAvg,Mort=loan$Mortgage,SecAcc=loan$SecuritiesAccount,CD=loan$CDAccount,Online=loan$Online,CreditCard=loan$CreditCard,Educ2=loan$Educ2,Educ3=loan$Educ3)
xx[1:3,]

## split the data set into training and test (evaluation) set
n=dim(loan)[1]
n
n1=floor(n*(0.6))
n1
n2=n-n1
n2
train=sample(1:n,n1)

## model fitted on all data 
m1=glm(response~.,family=binomial,data=data.frame(xx))
summary(m1)

xx=xx[,-1]
xtrain <- xx[train,]
xnew <- xx[-train,]
ytrain <- response[train]
ynew <- response[-train]

## model fitted on the training data set
m2=glm(response~.,family=binomial,data=data.frame(response=ytrain,xtrain))
summary(m2)

## create predictions for the test (evaluation) data set
ptest=predict(m2,newdata=data.frame(xnew),type="response")  
## predicted probabilities
hist(ptest)
plot(ynew~ptest)

## coding as 1 if probability 0.5 or larger
gg1=floor(ptest+0.5)
ttt=table(ynew,gg1)
ttt
error=(ttt[1,2]+ttt[2,1])/n2
error

## coding as 1 if probability 0.3 or larger
gg2=floor(ptest+0.7)
ttt=table(ynew,gg2)
ttt
error=(ttt[1,2]+ttt[2,1])/n2
error

bb=cbind(ptest,ynew)
bb
bb1=bb[order(ptest,decreasing=TRUE),]
bb1
## order cases in test set according to their success prob
## actual outcome shown next to it

## overall success probability in evaluation (test) data set
xbar=mean(ynew)
xbar

## calculating the lift
## cumulative 1's sorted by predicted values
## cumulative 1's using the average success prob from evaluation set
axis=dim(n2)
ax=dim(n2)
ay=dim(n2)
axis[1]=1
ax[1]=xbar
ay[1]=bb1[1,2]
for (i in 2:n2) {
  axis[i]=i
  ax[i]=xbar*i
  ay[i]=ay[i-1]+bb1[i,2]
}

aaa=cbind(bb1[,1],bb1[,2],ay,ax)
aaa[1:20,]

plot(axis,ay,xlab="number of cases",ylab="number of successes",main="Lift: Cum successes sorted by pred val/success prob")
points(axis,ax,type="l")
