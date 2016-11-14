email=read.csv('energy_bids.csv',stringsAsFactors=FALSE)
table(email$responsive)
strwrap(email$email[2])

#load to corpus
library(tm)
corpus=Corpus(VectorSource(email$email))
strwrap(corpus[[1]])

#preprocess
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c(stopwords('english')))
corpus = tm_map(corpus, stemDocument)

#build bag of words
dtm = DocumentTermMatrix(corpus)
dtm
dtm = removeSparseTerms(dtm,0.97)
dtm
labeledTerms = as.data.frame(as.matrix(dtm))
colnames(labeledTerms)=make.names(colnames(labeledTerms))
labeledTerms$responsive = email$responsive
str(labeledTerms)

#BUILDING MODEL

#load packages
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ROCR)

#construction train and test set
split =sample.split(labeledTerms$responsive,SplitRatio=0.7)
train = subset(labeledTerms,split==TRUE)
test = subset(labeledTerms,split==FALSE)

#tree analysis
emailsTree = rpart(responsive~., data= train, method = 'class')
predictTree = predict (emailsTree,newdata = test)
table(test$responsive,predictTree[,2]>0.5)
prp(emailsTree)
#accuracy 87.5%, a bit better than baseline model
#(207+18)/(207+18+8+24)

table(test$responsive)
#baseline model accuracy is 83.6% 215/(215+42)

library(ROCR)
predictionTree=prediction(predictTree[,2],test$responsive)
#ERROR!: format of predictions is invalid
performance=performance(predictionTree,'tpr','fpr')
plot(performance,colorize=TRUE)

performance(predictionTree,'auc')@y.values
#79.36%

#random forest analysis
emailsForest = randomForest(responsive~.,data = train,method='class')
predictRF = predict(emailsForest,newdata=test,type='class')
table(test$responsive,predictRF)
#prediction unclear??

#logistics linear regression analysis
emailLOG = glm (responsive~.,data=train, family = 'binomial')
predictLOG = predict (emailLOG,newdata=test,type='response')
table(test$responsive,predictLOG>0.5)
#accuracy 51.3%, TERRIBLE!!!!
#(116+16)/(116+16+99+26)


