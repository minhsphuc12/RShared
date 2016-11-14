flower=read.csv('flower.csv',header=FALSE)
flowerMatrix = as.matrix(flower)
flowerVector = as.vector(flowerMatrix)

distance = dist(flowerVector,method='euclidean')
clusterIntensity = hclust(distance, method='ward.D')
plot(clusterIntensity)
rect.hclust(clusterIntensity,k=3,border='red' )

flowerCluster=cutree(clusterIntensity,k=3)

tapply(flowerVector,flowerCluster,mean)
#check percentage of each cluster in original vector

dim(flowerCluster) = c(50,50)
#turn vector of clustered vector to matrix

image(flowerCluster,axes=FALSE)
#visualize the clustered matrix
image(flowerMatrix,axes=FALSE,col=grey(seq(0,1,length=256)))
#visualize the original data, not so different
cluster1= subset(flowerCluster, flowerCluster==1)
