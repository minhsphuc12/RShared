library(ggplot2)
library(lattice)

#read data
nei <- readRDS("summarySCC_PM25.rds")
scc <- readRDS("Source_Classification_Code.rds")

#1
#Have total emissions from PM2.5 
#decreased in the United States from 1999 to 2008
year=list(factor(nei$year))
emis=aggregate(x=nei$Emissions,by=year,FUN=sum)
colnames(emis)=c('year','emission')
#construct plot and save png
png('plot1.png')
barplot(emis$emission,names.arg=emis$year,
        main='Emission Summary')
dev.off()
#answer
print('The emission total amount from 1999 to 2008 was decreasing')


#2
#Have total emissions from PM2.5 decreased in the Baltimore 
#City, Maryland (fips == "24510") from 1999 to 2008?
nei2=subset(nei,fips == "24510")
year2=list(factor(nei2$year))
emis2=aggregate(x=nei2$Emissions,by=year2,FUN=sum)
colnames(emis2)=c('year','emission')
#construct plot and save png
png('plot2.png')
barplot(emis2$emission,names.arg=emis2$year,
        main='Emission Summary in Baltimore City')
dev.off()
#answer
print('The emission total amount in Baltimore City from 1999 to 2008 was decreasing, but not continuous every year')

#3
#Of the four types of sources indicated by the 
#type (point, nonpoint, onroad, nonroad) variable, for Baltimore City 
#which have seen decreases in emissions from 1999-2008? 
#Which have seen increases in emissions from 1999-2008?
emis3=aggregate(Emissions ~ type + year,data=nei2,FUN=sum)

#construct plot and save png
png('plot3.png')
qplot(year,Emissions,data=emis3,facets=  .~type)
dev.off()
#answer
Print('NONPOINT, NON-ROAD, ON-ROAD have decreasing emission each year, but type POINT is not')

#4
#Across the United States, how have emissions 
#from coal combustion-related sources changed from 1999-2008?
scc2=scc[grep('Coal',as.character(scc$Short.Name), fixed=TRUE),1]
nei3=subset(nei,SCC %in% scc2)
year3=list(factor(nei3$year))
emis4=aggregate(x=nei3$Emissions,by=year3,FUN=sum)
colnames(emis4)=c('year','emission')
png('plot4.png')
barplot(emis4$emission,names.arg=emis4$year,
        main='Emission Summary Coal Combustion')
dev.off()
#answer
Print('Decreasing')

#5
#How have emissions from motor vehicle sources 
#changed from 1999-2008 in Baltimore City?
scc3=scc[grep('Motor',as.character(scc$Short.Name), fixed=TRUE),1]
nei4=subset(nei,SCC %in% scc3)
year4=list(factor(nei4$year))
emis5=aggregate(x=nei4$Emissions,by=year4,FUN=sum)
colnames(emis5)=c('year','emission')
png('plot5.png')
barplot(emis5$emission,names.arg=emis5$year,
        main='Emission Summary Motor Vehicle')
dev.off()

#6
#Compare emissions from motor vehicle sources in 
#Baltimore City with emissions 
#from motor vehicle sources in Los Angeles County
nei5=subset(nei,fips %in% c('06037','24510'))
year5=list(factor(nei5$year))
emis6=aggregate(Emissions ~ fips+year,data=nei5,FUN=sum)

png('plot6.png')
qplot(year,Emissions,data=emis6,facets=  .~fips)
dev.off()
Print('Los Angeles has much higher emission to Baltimore')
Print('Los Angeles has varying and more substantial changes than Baltimore')