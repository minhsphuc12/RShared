data=read.csv('tweets.csv',stringsAsFactors=FALSE)
str(data)
data$Negative=as.factor(data$Avg<=-1)
table(data$Negative)
install.packages('tm')
install.packages('SnowballC')

library(tm)
library(SnowballC)

corpus = Corpus(VectorSource(data$Tweet))
corpus[[1]]

#EACH LINE HERE WILL USE 1 TYPE OF PREPROCESSING
corpus1 = tm_map(corpus,tolower)
corpus1[[1]]

corpus2 = tm_map(corpus,removePunctuation)
corpus2[[1]]

corpus3 = tm_map(corpus,removeWords,c('apple',stopwords('english')))
corpus3[[1]]

corpus4 = tm_map(corpus, stemDocument)
corpus4[[1]]

#COMBINE PREPROCESSING STEP
corpus = tm_map(corpus,tolower)
#this one will cause problem with DocumentTermMatrix later, leave it there.
corpus = tm_map(corpus,removePunctuation)
corpus = tm_map(corpus,removeWords,c('apple',stopwords('english')))
corpus = tm_map(corpus, stemDocument)
corpus[[1]]
table(sapply(corpus, class))
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
corpus = tm_map(corpus, stripWhitespace)
frequencies = DocumentTermMatrix(corpus)
frequencies

inspect(frequencies[1000:1005,505:515])
findFreqTerms(frequencies,lowfreq=100)
#get all term appear at least 20 times
sparse=removeSparseTerms(frequencies,0.99)
sparse
tweetSparse=as.data.frame(as.matrix(sparse))
colnames(tweetSparse)=make.names(colnames(tweetSparse))
#convert variable begin with number to proper name

tweetSparse$Negative = data$Negative
#add dependent variable to this data, from original dataset, for future analysis
library(caTools)
set.seed(123)
split=sample.split(tweetSparse$Negative,SplitRatio=0.7)
train = subset(tweetSparse,split==TRUE)
test = subset(tweetSparse,split==FALSE)

tweetSparse$Negative
#tweetSparse consist of 1181 rows, corresponding with 1181 tweets, col is variable (here is 133)


## PREDICTIVE ANALYSIS USING TREE ##
library(rpart)
library(rpart.plot)
library(ROCR)
tweetCART = rpart(Negative ~ . , data=train,method='class')
prp(tweetCART)

predictCART = predict(tweetCART,newdata=test,type='class')

table(test$Negative,predictCART)
#accuracy = 87.8%
table(test$Negative)
#baseline model 84.4%

predictionCART=prediction(predictCART,test$Negative)
perf=performance(predictionCART,'tpr','fpr')
plot(perf)

as.numeric(performance(predictionCART,'auc')@y.values)
#0.6465152

## USING RANDOM FOREST ##
set.seed(123)
library(randomForest)
tweetRandomForest=randomForest(Negative~., data=train)
predictRF=predict(tweetRandomForest,newdata=test)
table(test$Negative,predictRF)
#accuracy 88.4%, a bit better than logistic
(296+18)/(296+18+4+37)

## USING LOGISTIC LINEAR REGRESSION
tweetLOG=glm(Negative~., data=train,family=binomial)
predictions = predict(tweetLOG, newdata=test, type="response")
table(test$Negative,predictions>0.5)
#accuracy 82.5%
