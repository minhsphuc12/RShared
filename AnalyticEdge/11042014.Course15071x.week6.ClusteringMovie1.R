movies=read.table('movie.txt',header=FALSE,sep='|',quote="\"")

colnames(movies)=c('ID','Title','ReleaseDate','VideoReleaseDate','IMDB',
                   'Unknown','Action','Adventure','Animation',"Chilrend's",'Comedy','Crime',"Documentary", "Drama", "Fantasy", "FilmNoir"
                   ,"Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller"
                   ,"War","Western")

movies$ReleaseDate=NULL
movies$VideoReleaseDate=NULL
movies$IMDB=NULL

table(movies$Comedy)
table(movies$Romance,movies$Drama)
table(movies$Romance,movies$Drama)
movies[is.na(movies)]=0
#compute all distance of genres
distances= dist(movies[2:20],method='euclidean')
clusterMovies=hclust(distances,method='ward.D')
plot(clusterMovies)

clusterGroups=cutree(clusterMovies,k=10)
tapply(movies$Action,clusterGroups,mean)
tapply(movies$Drama,clusterGroups,mean)

clusterGroups[movies[grep("Men in Black",movies$Title),][[1]]]
#find cluster of movie with title includes 'Men in Black'

cluster2=subset(movies,clusterGroups==2)
cluster2$Title[1:20]

clusterGroups2=cutree(clusterMovies,k=2)
cluster22=subset(movies,clusterGroups2==2)
cluster22[1:100,]

tapply(movies$.,clusterGroups2,mean)
lapply(cluster2[-c(1,2,3)],mean)
#find the mean value of cluster 2 on each genre
#this will be CENTROID of cluster 2


