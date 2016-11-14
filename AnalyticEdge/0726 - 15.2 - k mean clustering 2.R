# Example 2: Monthly US Unemployment Rates 

## read the data; series are stored column-wise with labels in first row
s=file.path(getwd(),'unempstates.csv')
raw <- read.csv(s,header=TRUE)
raw[1:3,]

## time sequence plots of three series

plot(raw[,5],type="l",ylim=c(0,15),xlab="month",ylab="unemployment rate")
## CA
points(raw[,32],type="l", cex = .5, col = "dark red")   
## New York
points(raw[,15],type="l", cex = .5, col = "dark green") 
## Iowa

## transpose the data
## then we have 50 rows (states) and 416 columns (time periods)
rawt=matrix(nrow=50,ncol=416)
rawt=t(raw) #function t is transpose
rawt[1:3,]

## k-means clustering in 416 dimensions 
set.seed(1)
grpunemp2 <- kmeans(rawt, centers=2, nstart=10)
sort(grpunemp2$cluster)
grpunemp3 <- kmeans(rawt, centers=3, nstart=10)
sort(grpunemp3$cluster)
grpunemp4 <- kmeans(rawt, centers=4, nstart=10)
sort(grpunemp4$cluster)
grpunemp5 <- kmeans(rawt, centers=5, nstart=10)
sort(grpunemp5$cluster)

k=data.frame(rawt)
states= data.frame(colnames(raw))
k2=cbind(states,k)
colnames(k2)[1]='states'

o=order(grpunemp5$cluster)
data.frame(k2$states[o],grpunemp5$cluster[o])
data.frame(grpunemp5$cluster[o])
plot(k2$X1, k2$X2, type="n", 
     xlab="X1", ylab="X2")
text(x=k2$X1, y=k2$X2, labels=k2$states, col=grpunemp5$cluster+1)
## another analysis
## data set unemp.csv with means and standard deviations for each state
## k-means clustering on 2 dimensions (mean, stddev) 
s2=s=file.path(getwd(),'unemp.csv')
unemp <- read.csv(s2)
unemp[1:3,]

set.seed(1)
grpunemp <- kmeans(unemp[,c("mean","stddev")], centers=3, nstart=10)
## list of cluster assignments
o=order(grpunemp$cluster)
data.frame(unemp$state[o],grpunemp$cluster[o])
plot(unemp$mean,unemp$stddev,type="n",xlab="mean", ylab="stddev")
text(x=unemp$mean,y=unemp$stddev,labels=unemp$state, col=grpunemp$cluster+1)

