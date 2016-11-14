## Example 1
## We specify a seed to make the results reproducible. Omitting the 
## set.seed statement would lead to a different set of random numbers 
## and the results would vary somewhat
set.seed(10)
alpha=0.10
m=100
p=dim(m)
index=dim(m)
for (i in 1:5) {
  x=rnorm(25,1,1)
  t=-abs(mean(x)/(sd(x)/sqrt(25)))
  p[i]=2*pt(t,24)
  index[i]=i
}
for (i in 6:m) {
  x=rnorm(25)
  t=-abs(mean(x)/(sd(x)/sqrt(25)))
  p[i]=2*pt(t,24)
  index[i]=i
}
count=p<=0.05
table(count)
ps=sort(p)
logps=log(ps)
logindex=log(index)
y=log(index*alpha/m)
plot(logps~logindex,xlab="log(j)",ylab="log(ProbValue(j))",main="False Discovery Rate")
points(y~logindex,type="l")
ps
ps[6]




Example 2

## Example 2
set.seed(10)
alpha=0.20
m=500
p=dim(m)
index=dim(m)
for (i in 1:5) {
  x=rnorm(25,1,1)
  t=-abs(mean(x)/(sd(x)/sqrt(25)))
  p[i]=2*pt(t,24)
  index[i]=i
}
for (i in 6:m) {
  x=rnorm(25)
  t=-abs(mean(x)/(sd(x)/sqrt(25)))
  p[i]=2*pt(t,24)
  index[i]=i
}
count=p<=0.05
table(count)
ps=sort(p)
logps=log(ps)
logindex=log(index)
y=log(index*alpha/m)
plot(logps~logindex,xlab="log(j)",ylab="log(ProbValue(j))",main="False Discovery Rate")
points(y~logindex,type="l")
ps
ps[7]






CHAPTER 6: PENALTY-BASED VARIABLE SELECTION IN REGRESSION MODELS WITH MANY PARAMETERS (LASSO)

Example 1: Prostate Cancer 

prostate <- read.csv("C:/DataMining/Data/prostate.csv")
prostate[1:3,]
m1=lm(lcavol~.,data=prostate)
summary(m1)
## the model.matrix statement defines the model to be fitted 
x <- model.matrix(lcavol~age+lbph+lcp+gleason+lpsa,data=prostate)
x=x[,-1]  
## stripping off the column of 1s as LASSO includes the intercept
## automatically
library(lars)  
## lasso on all data
lasso <- lars(x=x,y=prostate$lcavol,trace=TRUE)
## trace of lasso (standardized) coefficients for varying penalty
plot(lasso)
lasso
coef(lasso,s=c(.25,.50,0.75,1.0),mode="fraction")
## cross-validation using 10 folds
cv.lars(x=x,y=prostate$lcavol,K=10)
## another way to evaluate lasso's out-of-sample prediction performance
MSElasso25=dim(10) 
MSElasso50=dim(10) 
MSElasso75=dim(10) 
MSElasso100=dim(10) 
set.seed(1)
for(i in 1:10){
  train <- sample(1:nrow(prostate),80)
  lasso <- lars(x=x[train,],y=prostate$lcavol[train])
  MSElasso25[i]= 
    mean((predict(lasso,x[-train,],s=.25,mode="fraction")$fit-prostate$lcavol[-train])^2)
  MSElasso50[i]=  
    mean((predict(lasso,x[-train,],s=.50,mode="fraction")$fit-prostate$lcavol[-train])^2)
  MSElasso75[i]=
    mean((predict(lasso,x[-train,],s=.75,mode="fraction")$fit-prostate$lcavol[-train])^2)
  MSElasso100[i]=
    mean((predict(lasso,x[-train,],s=1.00,mode="fraction")$fit-prostate$lcavol[-train])^2)
}
mean(MSElasso25)
mean(MSElasso50)
mean(MSElasso75)
mean(MSElasso100)
boxplot(MSElasso25,MSElasso50,MSElasso75,MSElasso100,ylab="MSE", sub="LASSO model",xlab="s=0.25            s=0.50             s=0.75          s=1.0(LS)")




Example 2: Orange Juice 

oj <- read.csv("C:/DataMining/Data/oj.csv")
oj$store <- factor(oj$store)
oj[1:2,]
x <- model.matrix(logmove ~ log(price)*(feat + brand 
                                        + AGE60 + EDUC + ETHNIC + INCOME + HHLARGE + WORKWOM 
                                        + HVAL150 + SSTRDIST + SSTRVOL + CPDIST5 + CPWVOL5)^2, data=oj)
dim(x)

## First column of x consists of ones (the intercept)
## We strip the column of ones as intercept is included automatically
x=x[,-1]
## We normalize the covariates as they are of very different magnitudes
## Each normalized covariate has mean 0 and standard deviation 1
for (j in 1:209) {
  x[,j]=(x[,j]-mean(x[,j]))/sd(x[,j])
}

## One could consider the standard regression model
reg <- lm(oj$logmove~x)
summary(reg)
p0=predict(reg)

## Or, one could consider LASSO 
library(lars) 
lasso <- lars(x=x, y=oj$logmove, trace=TRUE)
coef(lasso, s=c(.25,.50,0.75,1.00), mode="fraction")
## creates LASSO estimates as function of lambda 
## gives you the estimates for four shrinkage coef 

## Check that predictions in regression and lars (s=1) are the same
p1=predict(lasso,x,s=1,mode="fraction")
p1$fit  	
pdiff=p1$fit-p0
pdiff  ## zero differences

## out of sample prediction; estimate model on 20,000 rows
MSElasso10=dim(10) 
MSElasso50=dim(10) 
MSElasso90=dim(10) 
MSElasso100=dim(10) 
set.seed(1)	## fixes seed to make random draws reproducible
for(i in 1:10){
  train <- sample(1:nrow(oj), 20000)
  lasso <- lars(x=x[train,], y=oj$logmove[train])
  MSElasso10[i]= 
    mean((predict(lasso,x[-train,], s=.10, mode="fraction")
          $fit - oj$logmove[-train])^2)
  MSElasso50[i]=  
    mean((predict(lasso,x[-train,], s=.50, mode="fraction")
          $fit - oj$logmove[-train])^2)
  MSElasso90[i]=
    mean((predict(lasso,x[-train,], s=.90, mode="fraction")
          $fit - oj$logmove[-train])^2)
  MSElasso100[i]=
    mean((predict(lasso,x[-train,], s=1.0, mode="fraction")
          $fit - oj$logmove[-train])^2)
}
mean(MSElasso10)
mean(MSElasso50)
mean(MSElasso90)
mean(MSElasso100)
boxplot(MSElasso10,MSElasso50,MSElasso90,MSElasso100,ylab="MSE", sub="LASSO model",xlab="s=0.10            s=0.50             s=0.9          s=1.0(LS)")

## out of sample prediction; estimate model on 1,000 rows
set.seed(1)	## fixes seed to make random draws reproducible
for(i in 1:10){
  train <- sample(1:nrow(oj), 1000)
  lasso <- lars(x=x[train,], y=oj$logmove[train])  
  MSElasso10[i]= 
    mean((predict(lasso,x[-train,], s=.10, mode="fraction")
          $fit - oj$logmove[-train])^2)
  MSElasso50[i]=  
    mean((predict(lasso,x[-train,], s=.50, mode="fraction")
          $fit - oj$logmove[-train])^2)
  MSElasso90[i]=
    mean((predict(lasso,x[-train,], s=.90, mode="fraction")
          $fit - oj$logmove[-train])^2)
  MSElasso100[i]=
    mean((predict(lasso,x[-train,], s=1.0, mode="fraction")
          $fit - oj$logmove[-train])^2)
}
mean(MSElasso10)
mean(MSElasso50)
mean(MSElasso90)
mean(MSElasso100)
boxplot(MSElasso10,MSElasso50,MSElasso90,MSElasso100,ylab="MSE", sub="LASSO model",xlab="s=0.10            s=0.50             s=0.9          s=1.0(LS)")






CHAPTER 7: LOGISTIC REGRESSION	

Example 1: Death Penalty Data

## analyzing individual observations
dpen <- read.csv("C:/DataMining/Data/DeathPenalty.csv")
dpen[1:4,]
dpen[359:362,]
m1=glm(Death~VRace+Agg,family=binomial,data=dpen)
m1
summary(m1)
## calculating logits
exp(m1$coef[2])
exp(m1$coef[3])
## plotting probability of getting death penalty as a function of aggravation
## separately for black (in black) and white (in red) victim
fitBlack=dim(501)
fitWhite=dim(501)
ag=dim(501)
for (i in 1:501) {
  ag[i]=(99+i)/100
  fitBlack[i]=exp(m1$coef[1]+ag[i]*m1$coef[3])/(1+exp(m1$coef[1]+ag[i]*m1$coef[3]))
  fitWhite[i]=exp(m1$coef[1]+m1$coef[2]+ag[i]*m1$coef[3])/(1+exp(m1$coef[1]+m1$coef[2]+ag[i]*m1$coef[3]))
}
plot(fitBlack~ag,type="l",col="black",ylab="Prob[Death]",xlab="Aggravation",ylim=c(0,1),main="red line for white victim; black line for black victim")
points(fitWhite~ag,type="l",col="red")


## analyzing summarized data
dpenother <- read.csv("C:/DataMining/Data/DeathPenaltyOther.csv")
dpenother
m1=glm(Death~VRace+Agg,family=binomial,weights=Freq,data=dpenother)
m1
summary(m1)
exp(m1$coef[2])
exp(m1$coef[3])
