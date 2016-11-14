library(ggplot2)
library(lattice)
data=read.csv('activity.csv',header=TRUE)
#histogram(~sum(steps!=NA)|date,data=data)

#Make a histogram of the total number of steps taken each day
datadate=list((data2$date))
a=aggregate(x=data2$steps,by=datadate,FUN=sum)
colnames(a)=c('date','steps')
#make barplot
barplot(a$steps,width=0.1,names.arg=a$date)

#Calculate and report the mean and median 
#total number of steps taken per day
a1=mean(a$steps)
print(paste('The mean of total no. of steps per day is',a1))
a2=median(a$steps)
print(paste('The median of total no. of steps per day is',a2))


#Make a time series plot (i.e. type = "l") of the 5-minute 
#interval (x-axis) and the average number of steps taken, 
#averaged across all days (y-axis)
datainterval=list(factor((data2$interval)))
b=aggregate(x=data2$steps,by=datainterval,FUN='mean')
colnames(b)=c('interval','stepmean')
#make barplot
barplot(b$stepmean,width=0.1,names.arg=b$interval)

#Which 5-minute interval, on average across all the days in the dataset, 
#contains the maximum number of steps?
b2=as.character(b[order(b$stepmean,decreasing=TRUE)[1],1])
print(paste('The interval with maximum no. of steps is',b2))

#Calculate and report the total number of missing values in the dataset
miss=sum(is.na(data$steps))
print(paste('The numer of missing data is',miss))

#fill missing values with median of all numbers of step in same interval
#put filled values in new dataset 
data3=data
m=as.numeric(data$steps)
s=which(m %in% c(NA))
data3$interval = as.numeric(data3$interval)
b$interval=as.numeric(b$interval)
for (i in s) {
    data3[i,1]=b[which(b$interval %in% c(data3[i,2])),2]
}






