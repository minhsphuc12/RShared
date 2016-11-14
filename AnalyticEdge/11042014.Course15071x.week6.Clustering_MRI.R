healthy=read.csv('healthy.csv',header=FALSE)
healthyMatrix=as.matrix(healthy)
healthyVector=as.vector(healthyMatrix)
distance=dist(healthyVector,method='euclidean')
#this will give error, because the memory required is huge, matrix is too big

image(healthyMatrix,axes=FALSE,col=grey(seq(0,1,length=256)))
#visualize original data

str(healthyVector)
n=365636

k=5
set.seed(1)
KMC=kmeans(healthyVector,k,iter.max=1000)
str(KMC)

healthyClusters=KMC$cluster
healthyClusters
centroid2=KMC$centers[2]
cluster2size=KMC$size[2]

dim(healthyClusters)=c(nrow(healthyMatrix),ncol(healthyMatrix))
image(healthyClusters,axes=FALSE,col=rainbow(k))

#DETECT A TUMOR
tumor=read.csv('tumor.csv',header=FALSE)
tumorMatrix=as.matrix(tumor)
tumorVector=as.vector(tumorMatrix)

install.packages('flexclust')
library(flexclust)
KMC.kcca=as.kcca(KMC,healthyVector)
#~buidling model. object
tumorCluster=predict(KMC.kcca,newdata=tumorVector)
dim(tumorCluster)=c(nrow(tumorMatrix),ncol(tumorMatrix))

image(tumorCluster,axes=FALSE,col=rainbow(k))
