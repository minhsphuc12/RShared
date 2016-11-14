
# recommend(16,test1) [1:10]
# str(recommend(16,test1))
# 
# j = c('anna'=1,'benny'=2)
# for (i in 1:length(j)) {
#   z= j[i]
#   print(z)
# }
# j[['anna']]=j[['anna']]+1
# beam='cesar'
# j
# somenum=4
# 
# j= c(j,5)
# names(j) = c(unlist(names(j)),'cesar')
# names(j[3])='cesar'
# j=c(j,eval(parse(text = paste0("c(",beam," = 3)"))))
# j=c(j,eval(parse(text = paste0("c(",beam," = (somenum))"))))
# 
# 
# k=c(1,2,3,5,8,9)
# k[-length(k)]
# l=c(3,5,7,5,7,9,64)
# h=c(1,2)
# s=0
# for (i in k) {
#   if (!(i %in% l)) {
#     print(i)
#     h=append(h,i)
#   }
# }
# for (i in k) {
#   h=c(h,i)
# }
# 
# help(append)
# append(k,15)
# percentile = round(60*length(a)/100)
# top = a[1:percentile]
# recommend(16,test1)
# 
# exrecommendList = c()
# #get the list of users ordred by similarity 
# exsimilarList = highestSimilar(16,test1)
# 
# #get the rating for that user
# exuserRatingList=userRating(16,test1)
# 
# #determine the total similarity point
# extotalSimilarPoint=0
# for (i in exsimilarList){
#   extotalSimilarPoint = extotalSimilarPoint + i[[1]]
# }
# 
# #accumulate the rating by ilterate through k highest rated
# for (i in 1:length(exsimilarList)){
#   simi= exsimilarList[i]
# #   compute rating similarity significance
#   weight = simi[[1]]/extotalSimilarPoint
# #   #get the ID of the neighbor
#   name = as.numeric(names(simi))
# #   #get all rating of the neighbor
#   neighborRating = userRating(name,test1)
#   #now find neighbor rated that original user didn't
#   for (movieID in names(neighborRating)){
#     if (!(movieID %in% names(exuserRatingList))) {
#       if(!(movieID %in% names(exrecommendList))) {
#         exrecommendList = c(exrecommendList,neighborRating[[movieID]]*weight)
#         names(exrecommendList)=c(names(exrecommendList)[-length(exrecommendList)],movieID)
#       }
#       else {
#         exrecommendList[[movieID]]= exrecommendList[[movieID]] + neighborRating[[movieID]]*weight
#       }
#     }
#   }
# }
# 
# exsimilarList[1]
# 
# a=exsimilarList[1]
# exweight = a[[1]]/extotalSimilarPoint
# exname = as.numeric(names(a))
# exneighborRating = userRating(exname,test1)
# 
# 
# for (movieID in names(exneighborRating)){
#   if (!(movieID %in% names(exuserRatingList))) {
#     if(!(movieID %in% names(exrecommendList))) {
#       exrecommendList = c(exrecommendList,exneighborRating[[movieID]]*exweight)
#       names(exrecommendList)=c(names(exrecommendList)[-length(exrecommendList)],movieID)
#     }
#     else {
#       exrecommendList[[movieID]]= exrecommendList[[movieID]] + exneighborRating[[movieID]]*exweight
#     }
#   }
# }
# 
# b=exsimilarList[2]
# exweight2=b[[1]]/extotalSimilarPoint
# exname2=as.numeric(names(b))
# exneighborRating2=userRating(exname2,test1)
# for (movieID in names(exneighborRating2)){
#   if (!(movieID %in% names(exuserRatingList))) {
#     if(!(movieID %in% names(exrecommendList))) {
#       #       movie=movieID
#       #       exrecommendList = c(exrecommendList,eval(parse(text=paste0("c(",movieID," = exneighborRating[[movie]]*exweight)"))))
#       # #       j=                c(j              ,eval(parse(text=paste0("c(",beam,   " =            3                       )"))))
#       exrecommendList = c(exrecommendList,exneighborRating2[[movieID]]*exweight2)
#       names(exrecommendList)=c(names(exrecommendList)[-length(exrecommendList)],movieID)
#       
#     }
#     else {
#       exrecommendList[[movieID]]= exrecommendList[[movieID]] + exneighborRating2[[movieID]]*exweight2
#     }
#   }
# }
# 
# c=exsimilarList[3]
# exweight3=b[[1]]/extotalSimilarPoint
# exname3=as.numeric(names(c))
# exneighborRating3=userRating(exname3,test1)
# for (movieID in names(exneighborRating3)){
#   if (!(movieID %in% names(exuserRatingList))) {
#     if(!(movieID %in% names(exrecommendList))) {
#       #       movie=movieID
#       #       exrecommendList = c(exrecommendList,eval(parse(text=paste0("c(",movieID," = exneighborRating[[movie]]*exweight)"))))
#       # #       j=                c(j              ,eval(parse(text=paste0("c(",beam,   " =            3                       )"))))
#       exrecommendList = c(exrecommendList,exneighborRating3[[movieID]]*exweight3)
#       names(exrecommendList)=c(names(exrecommendList)[-length(exrecommendList)],movieID)
#       
#     }
#     else {
#       exrecommendList[[movieID]]= exrecommendList[[movieID]] + exneighborRating3[[movieID]]*exweight3
#     }
#   }
# }
# sort(exrecommendList,decreasing=TRUE)[1:5]
# exrecommendList[1:10]
# names(exuserRatingList[1:5])
# for (i in names(exneighborRating[1:5])){ print(i)}
# exneighborRating[['8']]
# exsimilarList[1:5]
# exrecommendList = c()
# 
# #sort the recommendation list
# exrecommendList = sort(exrecommendList,decreasing=TRUE)
# length(test1)
# #return first x% item, meaning top x% highest recommended rating
# expercentile = round(1*length(exrecommendList)/100)
# extop = exrecommendList[1:expercentile]
# extopNamesOnly = names(extop)
# extopNamesOnly
# ###
