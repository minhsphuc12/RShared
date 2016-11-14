
# OBJECTIVE
# 
# the objective is to provide the recommendation of movie items to various users based on various criteria.
# 
# METHOD 
# 1. Descriptive Analysis
# First all data will be described by some features: mean, median, mode, range, ANOVA. 
# There will be graphical plot for notable variables, using ggplot2 package.
# (suggestions............)
# 
# 2. Exploratory 
# Each variable pair,triple,... in original data, user, item will be cross-analyzed to see any patterns and correlation possible.
# (suggestions............)
# 
# 3. Predictive Analysis
# Several method will be implemented, analyzed and benchmarked
# a.Collaborative Filtering (user-based): based on similarity of ratings, user information. 
# 
# b.Content Filtering (item-based): based on item information (basically genre).
# One suggestion is use clustering, k-nearest neighbor and other classification algorithms (random forest, trees)
# (suggestions............)
# 
# Final recommendation will be picked or composed based on the performace analysis


#View README.txt to read introuction, file description and structure

data=read.csv('u.data',sep='',header=FALSE)
#read data with parameter sep='', without it there will be only 1 column
colnames(data)=c("UserID","ItemID","Rating","TimeStamp")
#rename proper variables

#read and rename all training & test datasets
train1=read.csv('u1.base',sep='',header=FALSE)
test1=read.csv('u1.test',sep='',header=FALSE)
train2=read.csv('u2.base',sep='',header=FALSE)
test2=read.csv('u2.test',sep='',header=FALSE)
train3=read.csv('u3.base',sep='',header=FALSE)
test3=read.csv('u3.test',sep='',header=FALSE)
train4=read.csv('u4.base',sep='',header=FALSE)
test4=read.csv('u4.test',sep='',header=FALSE)
train5=read.csv('u5.base',sep='',header=FALSE)
test5=read.csv('u5.test',sep='',header=FALSE)

traina=read.csv('ua.base',sep='',header=FALSE)
testa=read.csv('ua.test',sep='',header=FALSE)
trainb=read.csv('ub.base',sep='',header=FALSE)
testb=read.csv('ub.test',sep='',header=FALSE)

colnames(train1)=c("UserID","ItemID","Rating","TimeStamp")
colnames(test1)=c("UserID","ItemID","Rating","TimeStamp")
colnames(train2)=c("UserID","ItemID","Rating","TimeStamp")
colnames(test2)=c("UserID","ItemID","Rating","TimeStamp")
colnames(train3)=c("UserID","ItemID","Rating","TimeStamp")
colnames(test3)=c("UserID","ItemID","Rating","TimeStamp")
colnames(train4)=c("UserID","ItemID","Rating","TimeStamp")
colnames(test4)=c("UserID","ItemID","Rating","TimeStamp")
colnames(train5)=c("UserID","ItemID","Rating","TimeStamp")
colnames(test5)=c("UserID","ItemID","Rating","TimeStamp")
colnames(traina)=c("UserID","ItemID","Rating","TimeStamp")
colnames(testa)=c("UserID","ItemID","Rating","TimeStamp")
colnames(trainb)=c("UserID","ItemID","Rating","TimeStamp")
colnames(testb)=c("UserID","ItemID","Rating","TimeStamp")

#read genre, item info, user info, occupation
#rename variables to be more descriptive
genre = read.csv('u.genre',sep='|',header=FALSE)
colnames(genre)=c('Genre','GenreID')

item = read.csv('u.item',sep='|',header=FALSE)
colnames(item)=c('ID','Title','ReleaseDate','VideoReleaseDate','IMDB',
                   'Unknown','Action','Adventure','Animation',"Chilrend's",'Comedy','Crime',"Documentary", "Drama", "Fantasy", "FilmNoir"
                   ,"Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller"
                   ,"War","Western")


user = read.csv('u.user',sep='|',header=FALSE)
colnames(user)=c('UserID','Age','Gender','Occupation','ZipCode')

occupation = read.csv('u.occupation',sep='|',header=FALSE)
colnames(occupation)=c('Occupation')

summary(user$Age)
aov(Age~Age,data=user)
table(user$Gender)

### DESCRIPTIVE ANALYSIS


### EXPOLATORY ANALYSIS


### PREDICTIVE ANALYSIS


#subset user rating data from dataset
#convert particular user rating to named character vector

userRating=function(User,set){
  dataFrame=subset(set,UserID==User & Rating > 0)
  rating = dataFrame$Rating
  names(rating) = dataFrame$ItemID
  return (rating)
}

2>1 & 5>4

#define minkowski distance
minkowskiDistance= function (UserID1, UserID2,set,n){
  distance = 0
  rating1 = userRating(UserID1,set)
  rating2 = userRating(UserID2,set)
  for (item in names(rating1)) {
    if (item %in% names(rating2) ==TRUE) {
      distance = distance + (abs(rating1[[item]]-rating2[[item]]))**n
    }
  }
  return (distance**(1/n))
}


minkowskiDistance(196,207,testb,2)

## COLLABORATIVE ANALYSIS

#extract x% with most 
similarRating = function ()

# implement simple case mechanics of distance
# Compute nearest neighbors
# creates a sorted list of users based on their distance to username
#the following instance will use n=2, equivalent of Euclidean distance



nearestNeighbors = function(user, set){
  distanceList = c()
  
  for (i in unique(subset(set, UserID!=user & Rating > 0)$UserID)){
    distanceList=append(distanceList,minkowskiDistance(user,i,set,2))
    
  }
  names(distanceList)=unique(subset(set, UserID!=user)$UserID)
  distanceList=sort(distanceList)
  return (distanceList)
  
}

nearestNeighbors(145,testb)


#Better algorithm: instead of using Makowski distance, using similarity point is much better.
# Why: case 2 person have no commmon rating, distance will be 0, which is absurd, because 
# 2 person with exactly same rating will also have distance 0
# similarity point will better account the similarity between 2 user rating, event different one
# only challenge is how to turn the difference to similarity point appropriately. 

# same rating: similarity +1
# different rating: similarity +1/(difference+1)**k
# This method place quite high significance on same ratings with k>=2


#define similarity points
similarPoint = function(UserID1,UserID2,set,k=1){
  point=0
  rating1 = userRating(UserID1,set)
  rating2 = userRating(UserID2,set)
  for (item in names(rating1)) {
    if (item %in% names(rating2) ==TRUE) {
      point = point + 1/(abs(rating1[[item]]-rating2[[item]])+1)**k
    }
  }
  return (point)
}

highestSimilar = function(user, set){
  similarList = c()
  for (i in unique(subset(set, UserID!=user & Rating > 0)$UserID)){
    similarList = append(similarList,similarPoint(user,i,set))
  }
  names(similarList)=unique(subset(set, UserID!=user & Rating > 0)$UserID)
  similarList=sort(similarList,decreasing=TRUE)
  return (similarList)
}

#give recommendation list for an user in a data set

recommend = function(user, set,x=10){
  recommendList = c()
  #get the list of users ordred by similarity 
  similarList = highestSimilar(user,set)
  
  #get the rating for that user
  userRatingList=userRating(user,set)
  
  #determine the total similarity point
  totalSimilarPoint = 0
  for (i in similarList){
    totalSimilarPoint = totalSimilarPoint + i[[1]]
  }
  
  #accumulate the rating by ilterate through k highest rated
  for (i in 1:length(similarList)){
    simi = similarList[i]
    #compute rating similarity significance
    weight = simi[[1]]/totalSimilarPoint
    
    #get the ID of the neighbor
    name = as.numeric(names(simi))
    
    #get all rating of the neighbor
    neighborRating=userRating(name,set)
    
    #now find neighbor rated that original user didn't
    for (movieID in names(neighborRating)){
      if (!(movieID %in% names(userRatingList))) {
        
        #append movie rating into the list, or add with current rating
        if(!(movieID %in% names(recommendList))) {
          #add value first, then named that value
          recommendList = c(recommendList,neighborRating[[movieID]]*weight)
          names(recommendList)=c(names(recommendList)[-length(recommendList)],movieID)
        }
        else {
          recommendList[[movieID]]= recommendList[[movieID]] + neighborRating[[movieID]]*weight
        }
      }
    }
  }
  
  #sort the recommendation list
  recommendList = sort(recommendList,decreasing=TRUE)
  
  #return first x% item, meaning top x% highest recommended rating
  percentile = round(x*length(recommendList)/100)
  top = recommendList[1:percentile]
  topNamesOnly = names(top)
  return (top)
}

