# Example 3: European Protein Consumption Revisited (Mixture Model)
install.packages('mixtools')
library(mixtools)

## for a brief description of mvnormalmixEM
## mvnormalmixEM(x, lambda = NULL, mu = NULL, sigma = NULL, k = 2,
##              arbmean = TRUE, arbvar = TRUE, epsilon = 1e-08, 
##              maxit = 10000, verb = FALSE)
## arbvar=FALSE  		same cov matrices
## arbvar=TRUE (default)	different cov matrices
## arbmean=TRUE (default)	different means
## k	number of groups
s=file.path(getwd(),'protein.csv')
food <- read.csv(s,header=TRUE)
## Consider just Red and White meat clusters
food[1:3,]
X=cbind(food[,2],food[,3])
X[1:3,]

set.seed(1) 
## here we use an iterative procedure and the results in repeated runs may 
## not be exactly the same
## set.seed(1) is used to obtain reproducible results

## mixtures of two normal distributions on the first 2 features
## we consider different variances
out2<-mvnormalmixEM(X,arbvar=TRUE,k=2,epsilon=1e-02)
out2
prob1=round(out2$posterior[,1],digits=3)
prob2=round(out2$posterior[,2],digits=3)
prob=round(out2$posterior[,1])
o=order(prob)
data.frame(food$Country[o],prob1[o],prob2[o],prob[o])
plot(food$Red, food$White, type="n",xlab="Red Meat", ylab="White Meat")
text(x=food$Red,y=food$White,labels=food$Country,col=prob+1)

## mixtures of two normal distributions on all 9 features
## we consider equal variances
X1=cbind(food[,2],food[,3],food[,4],food[,5],food[,6],food[,7], food[,8],food[,9],food[,10])
X1[1:3,]
set.seed(1)
out2all<-mvnormalmixEM(X1,arbvar=FALSE,k=2,epsilon=1e-02)
out2all
prob1=round(out2all$posterior[,1],digits=3)
prob2=round(out2all$posterior[,2],digits=3)
prob=round(out2all$posterior[,1])
data.frame(food$Country,prob1,prob2,prob)
o=order(prob)
data.frame(food$Country[o],prob[o])



#HIERACHICAL CLUSTERING
#R program to create Figure 15.1

library(cluster)
dis=matrix(nrow=5,ncol=5)
dis[1,1]=0
dis[2,2]=0
dis[3,3]=0
dis[4,4]=0
dis[5,5]=0
dis[2,1]=9 
dis[3,1]=3
dis[4,1]=6
dis[5,1]=11
dis[3,2]=7
dis[4,2]=5
dis[5,2]=10
dis[4,3]=9 
dis[5,3]=2
dis[5,4]=8
dis[1,2]=dis[2,1]
dis[1,3]=dis[3,1]
dis[1,4]=dis[4,1]
dis[1,5]=dis[5,1]
dis[2,3]=dis[3,2]
dis[2,4]=dis[4,2]
dis[2,5]=dis[5,2]
dis[3,4]=dis[4,3] 
dis[3,5]=dis[5,3]
dis[4,5]=dis[5,4]
plot(agnes(x=dis,diss=TRUE,metric="eucledian",method="single"))
plot(agnes(x=dis,diss=TRUE,metric="eucledian",method="complete"))
