mvt = read.csv('mvt.csv',stringsAsFactors=FALSE)
str(mvt)

mvt$Date = strptime(mvt$Date,format = '%m/%d/%y %H:%M')
#convert time stamp to POSIXlt format by strptime fucntion
mvt$WeekDay=weekdays(mvt$Date)
mvt$Hour=mvt$Date$hour
#extract weekday and hour to independent variables

weekdaycount=as.data.frame(table(mvt$WeekDay))
#aggregate crime count on week days
#convert to data frame
str(weekdaycount)

library(ggplot2)
ggplot(weekdaycount,aes(Var1,Freq))+geom_line(aes(group=1),alpha=0.3)+xlab('Day of the Week')+ylab('Total Motor Vehicle Thefts')

'''
plot crime rate based on weekday
!issues!: 
original weekday get ordered by alphabet order
then the plot is not very natural
fix by 2 options: 
1) rearrange the data frame, and redraw (THIS IS NOT CONFIRMED TO BE POSSIBLE)
2) factorize the variables, as below
'''
weekdaycount$Var1 = factor(weekdaycount$Var1, ordered=TRUE,
                           levels=c('Sunday','Monday','Tuesday',
                                    "Wednesday", "Thursday", 
                                    "Friday", "Saturday"))
dayhourcount=as.data.frame(table(mvt$WeekDay,mvt$Hour))
dayhourcount$Hour=as.numeric(as.character(dayhourcount$Var2))

#CREATING PLOT FOR CRIME BASED ON HOUR, EACH LINE FOR A DAY
ggplot(dayhourcount,aes(Freq,Hour))+ geom_line(aes(group=Var1,color=Var1,size=0.5))

#CREATING HEAT MAP
#the weekday order is not problem in line graph
#but it is big issue in heat map
#so again, have to factorize 'em

dayhourcount$Var1 = factor(dayhourcount$Var1, ordered=TRUE,
                           levels=c('Sunday','Monday','Tuesday',
                                    "Wednesday", "Thursday", 
                                    "Friday", "Saturday"))

ggplot(dayhourcount,aes(Hour,Var1))+ geom_tile(aes(fill=Freq))+scale_fill_gradient(low="green", high="red",name='Total MV Thefts')+theme(axis.title.y =element_blank())
#geom_tile put each data point into 1 tile, with parameter 'fill'
#scale_fill_gradient: 2 parameter 'low','high' will do coloring nicely
#theme(axis.title.y =element_blank()) will remove y axis lable
#NOTICE: the x and y axes is hour and weekdays, not hour and freq anymore
#let's play other cases
ggplot(dayhourcount,aes(Hour,Freq))+ geom_tile(aes(fill=Var1))
#this one make no sense

#CREATING MAP BY LOCATION
install.packages('maps')
install.packages('ggmap')
library(maps,ggmap)
chicago=get_map(location='chicago',zoom=11)
ggmap(chicago)
#****************this get_map and ggmap function is VERY IMPORTANT********************

ggmap(chicago) +geom_point(data=mvt[1:100,],aes(Longitude,Latitude))
#see location of first 100 incident on data

latloncount=as.data.frame(table(round(mvt$Longitude,2),round(mvt$Latitude,2)))
str(latloncount)
#make table of crime frequency based on longitude and latitude

latloncount$LAT=as.numeric(as.character(latloncount$Var2))
latloncount$LON=as.numeric(as.character(latloncount$Var1))
#convert longitude and latitude to numeric value

ggmap(chicago) + geom_jitter(data=latloncount,aes(x=LON,y=LAT,size=Freq,color=Freq))+scale_color_gradient(low="yellow",
                                                                                                          high="red")
#use _jitter instead of _point for more accuracy visualization
#scale_color_gradient works, not scale_fill_gradient as above
#???
#solved: color gradient works with point, while 
#fill gradient works with tiles and bars

ggmap(chicago)+geom_tile(data=latloncount,aes(LON,LAT,alpha=Freq),fill='red')

latloncount2=subset(latloncount,Freq>0)
ggmap(chicago)+geom_tile(data=latloncount2,aes(LON,LAT,alpha=Freq),fill='red')

