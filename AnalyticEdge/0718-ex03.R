## Install packages from CRAN; use any USA mirror 
library(lattice)  
address02=file.path(getwd(),"oj.csv")
oj <- read.csv(address02,header=TRUE)

#convert variable 'store' to factor
oj$store <- factor(oj$store)
oj[1:2,]

#get mean logmove based on brand w/o missing value
t1=tapply(oj$logmove,oj$brand,FUN=mean,na.rm=TRUE)
t1
#get mean logmove based on brand and week
t2=tapply(oj$logmove,INDEX=list(oj$brand,oj$week),FUN=mean,na.rm=TRUE)
t2

#dominicks, minutemaid and tropicana
plot(t2[1,],type= "l",xlab="week",ylab="dominicks",ylim=c(7,12))
plot(t2[2,],type= "l",xlab="week",ylab="minute.maid",ylim=c(7,12))
plot(t2[3,],type= "l",xlab="week",ylab="tropicana",ylim=c(7,12))

#get all logmove value
logmove=c(t2[1,],t2[2,],t2[3,])
week1=c(40:160)
week=c(week1,week1,week1)
brand1=rep(1,121)
brand2=rep(2,121)
brand3=rep(3,121)
brand=c(brand1,brand2,brand3)
#get plot logmove against week, compare 3 brands, 3 graphs side vertical
xyplot(logmove~week|factor(brand),type= "l",layout=c(1,3),col="black")

#boxplot show dominic has a lot of high and few low outliers, minute mail got A LOT of high outlier
# tropicana show less deviated distribution
boxplot(logmove~brand,data=oj)
#same thing
histogram(~logmove|brand,data=oj,layout=c(1,3))
#just same thing again
densityplot(~logmove|brand,data=oj,layout=c(1,3),plot.points=FALSE)
#put 'em all in one
densityplot(~logmove,groups=brand,data=oj,plot.points=FALSE) 

#plot logmove against week all brands
xyplot(logmove~week,data=oj,col="black")
#put 'em separately
xyplot(logmove~week|brand,data=oj,layout=c(1,3),col="black")
#plot logemove with price all brands
xyplot(logmove~price,data=oj,col="black")
#separate 'em, look at that data, we got that price tropicana > maid > dominicks. 
#Who's the high end ^^
xyplot(logmove~price|brand,data=oj,layout=c(1,3),col="black")
#smootthhhyyy!
smoothScatter(oj$price,oj$logmove)
#smoothy by brands
smoothScatter(oj$price,oj$logmove|oj$brand)

#plot density of logmove by feat 0 and 1, feat
densityplot(~logmove,groups=feat, data=oj, lty=c(1,1),
            plot.points=FALSE)
            #legend(2004,9.5, c("1","2")))
#now that is way better, we now violet is... what again??
xyplot(logmove~price,groups=feat, data=oj)

#select store 5 data to analyze
oj1=oj[oj$store == 5,]
xyplot(logmove~week|brand,data=oj1,type="l",layout=c(1,3),col="black")
xyplot(logmove~price,data=oj1,col="black")
#seems like tropicana go way higher price than maid, and then dominicks
xyplot(logmove~price|brand,data=oj1,layout=c(1,3),col="black")
#just another density to get it clear, but hey, maid and 
#tropicana get very low deviation of logmove distribution
densityplot(~logmove|brand,groups=feat,data=oj1,plot.points=FALSE)
#plot show 'em all
xyplot(logmove~price|brand,groups=feat,data=oj1)

#get mean income based against store
t21=tapply(oj$INCOME,oj$store,FUN=mean,na.rm=TRUE)
t21
t21[t21==max(t21)]
t21[t21==min(t21)]
#quite scattered mind say, have to got another plotting to show
plot(oj$INCOME~oj$store)

oj1=oj[oj$store == 62,]
oj2=oj[oj$store == 75,]
oj3=rbind(oj1,oj2)
#compare logmove against price at two store 62 and 75
xyplot(logmove~price|store,data=oj3)
#same, but with feat identification
xyplot(logmove~price|store,groups=feat,data=oj3)
## store in the wealthiest neighborhood
mhigh=lm(logmove~price,data=oj1)
summary(mhigh)
plot(logmove~price,data=oj1,xlim=c(0,4),ylim=c(0,13))
abline(mhigh)
## store in the poorest neighborhood
mlow=lm(logmove~price,data=oj2)
summary(mlow)
plot(logmove~price,data=oj2,xlim=c(0,4),ylim=c(0,13))
abline(mlow)
#charts show that log move for high segment not change significan't by price, 
#but logmove of poor segement got slight tendency to fall off when price increases.